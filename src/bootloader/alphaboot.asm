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

  ; Following stanislav.org/helppc/int_13-2.html
  ; INT 13, 2 - Read Disk Sectors
	; AH = 02
	; AL = number of sectors to read	(1-128 dec.)
	; CH = track/cylinder number  (0-1023 dec., see below)
	; CL = sector number  (1-17 dec.)
	; DH = head number  (0-15 dec.)
	; DL = drive number (0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
	; ES:BX = pointer to buffer

  mov [ebr_drive_number], dl
  mov ax, 1
  mov cl, 1                         ; number of sectors to read
  mov bx, 0x7E00                    ; pointer to buffer
  call disk_read
  
  mov si, msg_hello_world
  call printString

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

times 510-($-$$) db 0 ; Fill the rest of the boot sector with zeroes
dw 0xaa55 ; Boot sector signature 
