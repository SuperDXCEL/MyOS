org 0x0
bits 16

start:
  mov si, msg
  call print
  jmp halt

halt:
  jmp halt

print:
  push si
  push ax
  push bx

printLoop:
  LODSB
  or al, al
  jz printDone

  mov ah, 0x0E
  mov bh, 0
  int 0x10

  jmp printLoop

printDone:
  pop bx
  pop ax
  pop si
  ret

msg: db "NIGHTMARE OS HAS BEEN BOOTED", 0x0D, 0x0A, 0