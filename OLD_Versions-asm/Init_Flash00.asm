; Test out PIO
;  --
;
;
; Return registers
;      BC - Zero
#target bin


Interupt_vector		equ $FF10
PIO_B_value:		equ $FF20
SP_value:			equ $FF30
Stack:				equ $F000
Result_NumToHex:	equ $FF40

#code   EEPROM_Startup, 0, *    ;EEPROM mem at 0000h

		nop
		nop
		nop

		ld HL, Stack
		ld SP,HL    		; Set stackpointer  ($F000)

		EI
		IM 2
		ld a,$04

		halt
		
	if DOALIGN
		align 8
	endif
            

#code   INT_IM1, $38, *    ;EEPROM mem at 0000h
		halt
	if DOALIGN
		align 8
	endif

		defw $0400          ; NMI adress table    


.end