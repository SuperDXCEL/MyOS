org 0x7C00
bits 16

start:
  jmp main

puts:
  ; Save registers we will modify
  push si
  push ax

.loop:
  lodsb            
  or al, al 
  jz .done         

  mov ah, 0x0e     
  int 0x10 
  jmp short .loop

.done:
  pop ax 
  pop si
  ret 

main:
  mov ax, 0
  mov ds, ax      
  mov es, ax       

  mov ss, ax     
  mov sp, 0x7C00

  mov si, msg_hello 
  call puts        

  hlt           

.halt:
  jmp .halt        

msg_hello db 'Hello, World!', 0 

times 510 - ($ - $$) db 0 
db 0xaa55                ; Boot sector signature (must be the last two bytes)
