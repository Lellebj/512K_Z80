; Test out PIO
;  --
 ;
 ;
 ; Return registers
 ;      BC - Zero
#target bin

portA_Contr:    equ $81
portB_Contr:    equ $83
portA_Data:     equ $80
portB_Data:     equ $82
command           = $30A0
setcursor         = $3070
write             = $30B0

HD44780_main_init = $3000
Interupt_vector   = $0160
PIO_B_value:  equ $BC00
SP_value:     equ $BF00
Result_NumToHex: equ $BF08

#code   EEPROM_Startup, 0, *    ;EEPROM mem at 0000h

        nop
        nop
        nop

        ld HL, $BE00
        ld SP,HL                    ; Set stackpointer $BE00

        EI
        IM 2
        ld a,$04

        call HD44780_main_init
        JP  RAMPAGE
        align 8            

#code   INT_IM1, $38, *    ;EEPROM mem at 0000h
        jp PIO_A_INT
        align 8
        defw $0400          ; NMI adress table    


#code RAMPAGE, $50, *            ; RAM area        
RAMPAGE:
;----------******************* PIO PORT A
        ld A, $0F                 ;mode 1 out
        out (portA_Contr), A         ; set port A as output
        ld A, Interupt_vector&0xFF                   ; low byte of INT table
        out (portA_Contr), A         ; PIO A interrupt vector
        ld A, $87
        out (portA_Contr), A         ; PIO A interrupt enable
        ld a,Interupt_vector>>8                   ; high byte of INT table
        ld I,A
        ei
;----------******************* PIO PORT B
        ld A, $0F                    ;mode 0 output 
        out (portB_Contr), A         ; set port A as output
        ld A, $03
        out (portB_Contr), A         ; PIO A interrupt disable
        ld a,0
        ld (PIO_B_value),a
        out (portB_Data), a



        ; Try to print something
        ld ix,t_string1
        ld b, $0B
	ld de, $0105	; row 1 column 5
	call setcursor	; runs also 'command'
nextchar1:
        ld a, (ix+0)
        inc ix
        call write
        djnz nextchar1
        out (portA_Data),a

        call ShowPC_HALT

        ; Try to print something 2
        ld ix,t_string2
        ld b, $09
	ld de, $0205	; row 2 column 5
	call setcursor	; runs also 'command'
nextchar2:
        ld a, (ix+0)
        inc ix
        call write
        djnz nextchar2
        out (portA_Data),a

        call ShowPC_HALT

        ; Try to print something 2
	ld ix,t_string3
        ld b, $07
	ld de, $0305	; row 2 column 5
	call setcursor	; runs also 'command'
nextchar3:
        ld a, (ix+0)
        inc ix
        call write
        djnz nextchar3
        out (portA_Data),a

        call ShowPC_HALT
        nop
        inc ix
        nop
        inc ix

slut:      
        rst 00h 

		align 8            
initbytes:   .byte $01, $38, $0E, $06, $B0
t_string1:   .ascii "ARZ80 micro"
t_string2:   .ascii "Waking up"
t_string3:   .ascii "Ready->"

#code INT_TABLE, Interupt_vector, $10
        ;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
        ;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
        ;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
        ;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
        .word PIO_A_INT,PIO_A_INT,PIO_A_INT,PIO_A_INT
        
        ;defw $0400          ; NMI adress table    


#code PIO_A_INT, $200, *            ; PIO A interrupt handler

        di
        
        out (portB_Data),a
        ;call inc_portB_value
        ;ld ix,t_intAstr
        LD BC, $4000
wait01: nop
        nop
        nop
        djnz wait01
        out (portA_Data),a

        ei
        reti




;************************************************************************
; ShowPC_HALT:
; Dump prog counter prior to HALT instr. value present in stack (pointed by SP).
; uses  IX (pointer to HEX chars)
;       IY (pointer at stack)
;       BC ( count)
;       HL (value for conversion to HEX)
;       DE (positon of display 2004A)
        align 8            
ShowPC_HALT:
        ld (SP_value), sp
        ld iy,(SP_value)
        ld L,(iy+0)
        ld H,(iy+1)
        
        call Num2Hex
	ld de, $0002	; row 0 column 2
        call setcursor	; runs also 'command'
        ld b, $04
        ld ix, Result_NumToHex    ; adress for ascii chars.
nxt4:
        ld a, (ix+0)
        inc ix
        call write
        djnz nxt4
        halt
        ret

	align 8            
t_intAstr:   .ascii "PIO A INT"
;************************************************************************
; Hexadecimal conversion operates directly on nibbles and takes advantage of nifty DAA trick.
;Input: HL = number to convert, IX = location of ASCII string
;Output: ASCII string at (IX) 
Num2Hex
        ld ix, Result_NumToHex
        ld	a,h
	call	Num1
	ld	a,h
	call	Num2
	ld	a,l
	call	Num1
	ld	a,l
	jr	Num2
        
Num1	rra
	rra
	rra
	rra
Num2	or	$F0
	daa
	add	a,$A0
	adc	a,$40

	ld	(ix+0),a
	inc	ix
	ret

inc_portB_value:
        ld a, (PIO_B_value)
        inc a
        ld (PIO_B_value), a
        out (portB_Data), a
        ret

.end



