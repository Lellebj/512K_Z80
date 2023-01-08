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
 ; test595IORQ.asm
#target bin

IORQ595_CE:    equ $20
IORQ595_RST:    equ $FF
E_SIGNAL:         equ $FF


#code   RAM_PAGE,$0000,*  ;     ;EEPROM mem at 0000h
        nop
        nop
        nop

        ld HL, $BE00
        ld SP,HL                    ; Set stackpointer $3000

        ld b,$3
init:   ld a, $30     ; init 3 times....
        ld c, $00
        call sendTo595
        djnz init

        ld hl, initbytes
        ld b, 5
initseq:ld a, (hl)    
        inc hl 
        ld c, $00
        call sendTo595

        djnz initseq
            

        ld b, 6
        ld hl, t_string

tout:   ld a, (HL)     ; Will there be an 'H'...
        inc hl
        ld c, %10000000
        call sendTo595 
        
        djnz tout

            ;ld a, $B0     ; Will there be an 'H'...
            ;ld c, %00000000
            ;call sendTo595      

slut:       jr slut
            
initbytes:   .byte $01, $38, $0E, $06, $B0
t_string:    .ascii "ABCEDFGDFS"

;********************************************************************
#code   SUB1,$0060,$50             ;          ;EEPROM mem at 0060h

sendTo595:  
        push AF
        push BC
        ld b, $5
        scf
        ccf                         ; resets the carry flag

next:   out (IORQ595_CE), a         ;only D0 get stored in 595 in
        sra c                         
        rra                         ;lsb of c in to CY then msb in a...   
        
        djnz  next
        out (IORQ595_RST), a        ; latch OE on 595 
        out (E_SIGNAL), a
            
        ld b, $05
delay1: 
        nop
        djnz delay1   
        out (E_SIGNAL), a   

        pop BC
        pop AF
        ret
;****************************************************************

.end



