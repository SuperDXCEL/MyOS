[org 0x7c00] ; Set the origin where BIOS loads the bootloader
[bits 16]    ; 16 bit mode

jmp short main
nop

; FAT12 BOOT RECORD HEADER
bdb_oem: 		                    db "MSWIN4.1" ; 8 bytes
bdb_bytes_per_sector: 		      dw 512
bdb_sectors_per_cluster: 		    db 1
bdb_reserved_sectors: 		      dw 1
bdb_fat_count:         		      db 2
bdb_dir_entries_count:     	    dw 0E0h
bdb_total_sectors: 	            dw 2880
bdb_media_descriptor_type: 	    db 0F0h
bdb_sectors_per_fat: 		        dw 9
bdb_sectors_per_track: 		      dw 18
bdb_heads: 		                  dw 2
bdb_hidden_sectors: 		        dd 0
bdb_large_sectors: 		          dd 0

; FAT12 EXTENDED BOOT RECORD
ebr_drive_number: 		          db 0
		 		                        db 0
ebr_signature: 			            db 29h
ebr_volume_id:			            db 12h,34h,56h,78h
ebr_volume_label:		            db 'elevenbytes'
ebr_system_id:			            db 'FAT12   '


main:
  mov ax, 0
  mov ds, ax                        ; memory address of data segment
  mov es, ax                        ; memory address of extra segment 

  mov ss, ax                        ; stack grows downwards from where we are loaded in memory
  mov sp, 0x7C00                    ; that's why the stack pointer begins at the OS beginning, to avoid overwritting

  ; Following stanislavs.org/helppc/int_13-2.html
  ; INT 13, 2 - Read Disk Sectors
	; AH = 02
	; AL = number of sectors to read	(1-128 dec.)
	; CH = track/cylinder number  (0-1023 dec., see below)
	; CL = sector number  (1-17 dec.)
	; DH = head number  (0-15 dec.)
	; DL = drive number (0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
	; ES:BX = pointer to buffer

  ; mov [ebr_drive_number], dl
  ; mov ax, 1
  ; mov cl, 1                         ; number of sectors to read
  ; mov bx, 0x7E00                    ; pointer to buffer
  ; call disk_read
  
  mov si, msg_hello_world
  call printString

  ; 4 segments
  ; reserved segment: 1 sector
  ; FileAllocationTables sectors: 9 sectors per FAT * 2 FATs
  ; Root directory:
  ; Data

  ; -----------------------------------------------------------
  ; Calculate the Logical Block Address (LBA) of the Root Directory
  ; -----------------------------------------------------------

  ; Load the number of sectors per FAT from the BIOS Parameter Block (BPB) into AX
  mov ax, [bdb_sectors_per_fat]  

  ; Load the number of FAT tables into BL (BL = FAT count)
  mov bl, [bdb_fat_count]       

  ; Clear BH to make BX a valid 16-bit register (BX = FAT count)
  xor bh, bh                  

  ; Multiply AX by BX (AX = sectors per FAT * FAT count)
  mul bx                     

  ; Add the number of reserved sectors (from BPB) to AX
  ; This gives the starting LBA of the first FAT.
  add ax, [bdb_reserved_sectors] 

  ; Push this LBA onto the stack for later use
  push ax                     

  ; -----------------------------------------------------------
  ; Calculate the Number of Sectors for the Root Directory
  ; -----------------------------------------------------------

  ; Load the number of root directory entries from BPB into AX
  mov ax, [bdb_dir_entries_count] 

  ; Multiply AX by 32 (each directory entry is 32 bytes)
  ; This gives the total size of the root directory in bytes.
  shl ax, 5                    

  ; Clear DX, preparing for the division
  xor dx, dx                 

  ; Divide AX by the number of bytes per sector (from BPB)
  ; This gives the number of sectors needed to store the root directory.
  div word [bdb_bytes_per_sector]

  ; -----------------------------------------------------------
  ; Check for Partial Sector and Adjust Sector Count if Needed
  ; -----------------------------------------------------------

  ; Test DX to see if there is a remainder (partial sector)
  test dx, dx                 

  ; If no remainder (DX == 0), jump to rootDirAfter
  jz rootDirAfter             

  ; If there was a remainder, increment AX to account for an additional sector
  inc ax                      

  ; -----------------------------------------------------------
  ; Prepare for Disk Read Operation
  ; -----------------------------------------------------------

rootDirAfter:
  ; Move the number of sectors to read (from AX) into CL
  mov cl, al                   

  ; Pop the LBA of the root directory start sector from the stack into AX
  pop ax                      

  ; Load the drive number into DL (from the Extended BIOS Data Area or passed from bootloader)
  mov dl, [ebr_drive_number]  

  ; Load the address of the buffer into BX (where data will be read into)
  mov bx, buffer              

  ; Call the disk_read subroutine to read the calculated number of sectors
  call disk_read              

  ; -----------------------------------------------------------
  ; Prepare to Process the Root Directory Entries
  ; -----------------------------------------------------------

  ; Clear BX, likely preparing it to iterate over directory entries or similar
  xor bx, bx                 

  ; Load the buffer address into DI, setting up for processing the directory entries
  mov di, buffer              

searchKernel:
  mov si, file_kernel_bin
  mov cx, 11                        ; size of file_kernel_bin
  push di                           ; preserving the buffer
  repe cmpsb                        ; repeat comparison of bytes between 'si' and 'di' eleven times
  pop di
  je foundKernel

  add di, 32                        ; next directory entry
  inc bx
  cmp bx, [bdb_dir_entries_count]   ; have we reached all the directories that exist?
  jl searchKernel

  jmp kernelNotFound

kernelNotFound:
  mov si, msg_kernel_not_found
  call printString

  hlt
  jmp halt

foundKernel:
  mov ax, [di + 26]                 ; 'di' holds the kernel address, 26 is the offset needed to find the first logical cluster field
  mov [kernel_cluster], ax

  mov ax, [bdb_reserved_sectors]
  mov bx, buffer
  mov cl, [bdb_sectors_per_fat]
  mov dl, [ebr_drive_number]

  call disk_read

  mov bx, kernel_load_segment
  mov es, bx
  mov bx, kernel_load_offset

loadKernelLoop:
  mov ax, [kernel_cluster]
  add ax, 31                        ; Offset to read the cluster
  mov cl, 1
  mov dl, [ebr_drive_number]

  call disk_read

  add bx, [bdb_bytes_per_sector]

  mov ax, [kernel_cluster]          ; (kernel cluster * 3) / 2
  mov cx, 3
  mul cx
  mov cx, 2
  div cx

  mov si, buffer
  add si, ax
  mov ax, [ds:si]

  or dx, dx
  jz even

odd:
  shr ax, 4
  jmp nextClusterAfter

even:
  and ax, 0x0FFF

nextClusterAfter:
  cmp ax, 0x0FF8
  JAE readFinish

  mov [kernel_cluster], ax
  jmp loadKernelLoop

readFinish:
  mov dl, [ebr_drive_number]
  mov ax, [kernel_load_segment]
  mov ds, ax
  mov es, ax

  jmp kernel_load_segment:kernel_load_offset

  hlt

printString:
  mov ah, 0x0e                      ; BIOS function to print a character
  mov al, [si]                      ; al is the register for printing
  jmp printStringLoop

printStringLoop:
  int 0x10                          ; CPU interrupt to print the character (syscall)
  inc si                                              
  mov al, [si]
  or al, al
  jnz printStringLoop

  ret

halt:
  jmp halt

; Convert LBA direction to CHS:
;   Input: 
;     LBA index in ax
;   Output:
;     cx [bits 0-5]: sector number
;     cx: [bits 6-15]: cylinder
;     dh: head
;
lba_to_chs:
  push ax
  push dx

  xor dx, dx                        ; turn dx to 0
  div word [bdb_sectors_per_track] 	; ax = LBA / SectorsPerTrack
	  			 	                        ; dx = LBA % SectorsPerTrack
  inc dx 				                    ; dx = (LBA % SectorsPerTrack + 1) = sector
  mov cx, dx				                ; cx = sector

  xor dx, dx				                ; dx = 0
  div word [bdb_heads] 	            ; ax = (LBA / sectors per track) / heads = cylinder
    					                      ; dx = (LBA / sectors per track) % heads = head

  mov dh, dl  				              ; dh = head
  mov ch, al                        ; move the lower 8 bits of ax into ch
  shl ah, 6                         ; shift the lower 6 bits of ah
  or cl, ah                         ; copy the values into cl, since cl is just 0

  ; We now have dh = head, cx [0-5] = sector number, cx [6-15] = cylinder

   pop ax
   mov dl, al
   pop ax

   ret

disk_read:
  push ax
  push bx
  push cx
  push dx
  push di

  call lba_to_chs

  mov ah, 02h
  mov di, 3                          ; counter

retry:
  stc                                ; set the carry flag just in case
  int 13h                            ; interrupt
  jnc done_read                      ; jump no carry

  call disk_reset

  dec di
  test di, di
  jnz retry

fail_disk_read:
  mov si, msg_read_failed
  call printString
  hlt
  jmp halt

disk_reset:
  pusha
  mov ah, 0
  int 13h
  JC fail_disk_read                   ; if carry is set, disk reading failed again

done_read:
  pop di
  pop dx
  pop cx
  pop bx
  pop ax

  ret

msg_read_failed: db 'Read from disk failed', 0x0D, 0x0A, 0
msg_hello_world: db 'Welcome to your Nightmare', 0x0D, 0x0A, 0
file_kernel_bin db 'KERNEL  BIN'
msg_kernel_not_found db 'KERNEL.BIN not found!'
kernel_cluster dw 0

kernel_load_segment equ 0x2000
kernel_load_offset equ 0

times 510-($-$$) db 0 ; Fill the rest of the boot sector with zeroes
dw 0xaa55 ; Boot sector signature 

buffer: