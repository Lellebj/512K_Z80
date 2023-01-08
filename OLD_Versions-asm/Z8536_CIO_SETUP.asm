	    ;TEST,CIO
        ;OBJ C   ODE M STMT SOURCE STATEMENT
        ;CIO TEST PROGRAM

        ;INITIAL CREATION
        ;THIS PROGRAM INITIALIZES THE THREE COUNTER
        ;TIMERS IN    THE Z8536 CIO TO GENERATE SQUARE WAVES. THEN LOOPS FOREVER,
;$4200 se bmZ8536_CIO_SETUP
#target bin
#include "CIO_Main_Control_Registers.asm"

CIOC:   EQU     $A0       		; CIO PORT C
CIOB:   EQU     CIOC+1  		; CIO PORT B
CIOA:   equ     CIOC+2  		; CIO PORT A
CIOCTL: equ     CIOC+3  		; CIO CTRL PORT
BAUD:   equ     9600    		;ASYNC BAUD RATE
RATE:   equ     BAUD/100
CIOCNT: equ     576/RATE		; ...6
CIO_Init_data:	equ $F100



;*** MAIN PROGRAM
#CODE CIO_SETUP, $4200,$7F


Z8536_Init:
CIOINI:
		push BC
		push de
		push hl

		ld hl, CLST
		ld de, CIO_Init_data
		ld bc, CEND-CLST
		LDIR					; make stack for CIO init data at $F100

		IN A,(CIOCTL)			; INSURE STATE 0
		LD A,0					; REG 0 OR RESET
		OUT (CIOCTL), A			;WRITE PTR OR CLEAR RESET
		IN	A,(CIOCTL)			; STATE 0
		LD	A,0					; REG 0
		OUT	(CIOCTL),A			; WRITE PTR
		LD	A,1					; WRITE RESET
		OUT	(CIOCTL),A	
		LD	A,0					; CLEAR RESET
		OUT	(CIOCTL),A
		LD	HL,CIO_Init_data	; INIT CIO
		LD	B,CEND-CLST
		LD	C,CIOCTL

		OTIR

		pop hl
		pop de
		pop BC

		RET


		align 8
CLST:			; start of init data		
		defb	_PMSB				; PORT B MODE	bitport
		defb	$00
		defb	_PMSA				; PORT B MODE	bitport
		defb	$00
		defb	_DDB					; PORT B DIRECTION 0=output
		defb	11100000B
		defb	_DDA					; PORT B DIRECTION 0=output
		defb	$00
		defb	_DDC					; PORT C DIRECTION
		defb	11111110B
;		defb	_CTMS1					; CT1 MODE;Counter/Timer 1’s Mode Specification
;		defb	_CS_N|_EOE|_SQUAREWAVE	; CT1 out pin (B)12
;		defb	_CTMS2					; CT2 MODE;Counter/Timer 2’s Mode Specification
;		defb	_CS_N|_EOE|_SQUAREWAVE	; CT2 out pin (B)8
		defb	_CTMS3					; CT3 MODE;Counter/Timer 3’s Mode Specification
		defb	_REB| _DCS1| _DCS0		; CT3 single loop, no output (C)19
;		defb	_CTTC1M					; CT1 TC MSB;Counter/Timer l’s Time Constant-MSBs
;		defb	$FF
;		defb	_CTTC1L					; LSB
;		defb	$FF; CIOCNT
;		defb	_CTTC2M					; CT2 TC MS8Counter/Timer 2’s Time Constant-MSBs
;		defb	0
;		defb	_CTTC2L					; LSB
;		defb	CIOCNT
		defb	_CTTC3M					; CT3 TC MSBCounter/Timer 3’s Time Constant-MSBs
		defb	$FF
		defb	_CTTC3L					; LSB
		defb	$FF						;CIOCNT
		defb	_MCC					; Master Config. REG.
		defb	_PAE|_PBE|_CT3E			;PORT A&B&CT3 ENABLE 
										;COUNTER/TIMER 1,2,3 ENABLE
;		defb	_CT1CS					; CT1 TRIGGERCounter/Timer 1’s Command and Status
;		defb	_GCB|_TCB				;GATE COMMAND BIT (GCB)|TRIGGER COMMAND BIT (TCB) (WRITE ONLY - READ RETURNS 0)
;		defb	_CT2CS					; CT2 TRIGGER  same as 1
;		defb	_GCB|_TCB
		;defb	_CT3CS					; CT3 TRIGGER  same as 1
		;defb	_GCB|_TCB
CEND: 	EQU		$



#code  Z8536_Set_Wait_T3, $4280, $7F

		; reg DE contains time constant.
		push AF
		push BC
		push DE
		push HL

		LD	HL,CBEG_Timer_data	; INIT CIO
		LD	B, CEND_Timer_data-CBEG_Timer_data
		LD	C,CIOCTL

		OTIR

		ld A, _CTTC3M
		out (CIOCTL),A
		ld A, D
		out (CIOCTL),A			; MSB of time constant
		ld A, _CTTC3L
		out (CIOCTL),A
		ld A, E
		out (CIOCTL),A			; LSB of time constant
		ld A, _CT3CS
		out (CIOCTL),A
		ld A, _GCB| _TCB
		out (CIOCTL),A			; Start Timer 3
		
		; check if timer 3 is count to 0
		; register A and flag Z/NZ contains result
		ld A, _CT3CS
		out (CIOCTL),A					; Timer 3 command & status

waitT3:	in A,(CIOCTL)					; read status

		bit 0, A						; test bit 0 (CIP)
		jr NZ, waitT3					
		

		pop HL
		pop DE
		pop BC
		pop AF
		RET

	align 8
CBEG_Timer_data:			; start of init data		
		defb	_CT3CS
		defb	$0						; stop prev. timer
		defb	_CTMS3					; CT3 MODE;Counter/Timer 3’s Mode Specification
		defb	_REB| _DCS1| _DCS0		; CT3 single loop, no output (C)19

		defb	_CTTC3M					; CT3 TC MSBCounter/Timer 3’s Time Constant-MSBs
		defb	$FF
		defb	_CTTC3L					; LSB
		defb	$FF						;CIOCNT
		defb	_MCC					; Master Config. REG.
		defb	_PAE|_PBE|_CT3E			;PORT A&B&CT3 ENABLE 
										;COUNTER/TIMER 1,2,3 ENABLE
		;defb	_CT3CS					; CT3 TRIGGER  same as 1
		;defb	_GCB|_TCB
CEND_Timer_data: 	EQU		$

