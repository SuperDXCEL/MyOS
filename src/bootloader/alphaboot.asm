[org 0x7c00] ; Set the origin where BIOS loads the bootloader
[bits 16]

; FAT12 BOOT RECORD HEADER
jmp short start
nop


bdb_FATspecification: 		db "MSWIN4.1", 0 ; 8 bytes
bdb_sectorSize: 		dw 512
bdb_sectorsByCluster: 		db 1
bdb_reservedSectors: 		dw 2
bdb_storageMediaFATS: 		db 2
bdb_rootDirectoryEntries: 	dw 0E0h
bdb_totalLogicalVolumeSectors: 	dw 2880
bdb_mediaDescriptorType: 	db 0F0h
bdb_sectorsPerFAT: 		dw 9
bdb_sectorsPerTrack: 		dw 18
bdb_sidesOnStorage: 		dw 2
bdb_hiddenSectors: 		dw 0
bdb_largeSectors: 		dw 0

# extended boot record
ebr_DriveNumber: 		db 0
		 		db 0
ebr_signature: 			db 29h
ebr_volume_id:			db
start:
  mov ah, 0x0e ; BIOS funciton to print a character
  mov al, 66
  int 0x10

  jmp loop

loop:
  cmp al, 92
  je break_loop

  inc al
  int 0x10

  jmp loop

break_loop:
  times 510-($-$$) db 0 ; Fill the rest of the boot sector with zeroes
  dw 0xaa55 ; Boot sector signature 
