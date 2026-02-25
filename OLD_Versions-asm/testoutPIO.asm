; Test out PIO
;port A; 0 Blue
;port A; 1 red
;port A; 2 Yellow
;port A; 3 Green
;  --
 ; Copy a block of memory from one location to another.
 ;
 ; Entry registers
 ;      BC - Number of bytes to copy
 ;      DE - Address of source data block
 ;      HL - Address of target data block
 ;
 ; Return registers
 ;      BC - Zero
 ; testoutPIO.asm
#target bin

portA_Contr:    equ $02
portB_Contr:    equ $03
portA_Data:     equ $00
portB_Data:     equ $01

#code   RAM_PAGE,0x0000,60      ; basic ram               ;RAM mem at E000h
            nop
            nop
            nop
            ld a, %00001111   ;mode 3 in/out
            out portA_Contr, a         ; set port A as output
            ld a, %11110010   ;4 input (7-4)and 4 output (0-3)
            ;;out portA_Contr, a         ; set port A as output
            ;ld a, %00000011   ;disable interrupt
            ;out portA_Contr, a         ; set port A as output
            
            ld a, %00001111   ;mode 3 in/out
            out (portB_Contr), a         ; set port A as output
            ;ld a, %11110000   ;4 input (7-4)and 4 output (0-3)
            ;out (portB_Contr), a         ; set port A as output
            ;ld a, %00000011   ;disable interrupt
            ;out (portB_Contr), a         ; set port A as output
            
loop2:      ld a, $01
            out (portA_Data), a
            out (portB_Data), a
            ld a, $03
            out (portA_Data), a 
            out (portB_Data), a 
            ld a, $06
            out (portA_Data), a 
            out (portB_Data), a        
            ld a, $0C
            out (portA_Data), a 
            out (portB_Data), a
            ld a, $18
            out (portA_Data), a 
            out (portB_Data), a  
            ld a, $30
            out (portA_Data), a 
            out (portB_Data), a    
            jr loop2       ;Repeat the loop

            ld      (IX+0), b   ;spara sista data $FF
            ld d,   #00
            djnz loop2
            halt



    .end


