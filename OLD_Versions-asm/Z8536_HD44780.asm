; Test out PIO
;  --
;
;
; Return registers
;Z8536_HD44780.asm
; $0 se bmZ8536_HD44780
#target bin
include "HD_44780_defs.inc"


;Function addresses
setcursor		= $3100
writecommand	= $3180
writedata		= $3140
testBusyFlag    = $31B0
HD44780_main_init = $3000


; PIO addressess...
portA_Contr:	equ $81
portB_Contr:	equ $83
portA_Data:		equ $80
portB_Data:		equ $82

; Z8536 parametres
Z8536_SetTimer 		= $4300
Z8536_Init			= $4200

; Data tables  (upper EPROM/FLASH)
DataTables			= $7C00
Interupt_vector		= $7E00
; variables  	(upper ram)
PIO_B_value:		= $F1D0
SP_value:			= $F1E0
AF_value:			= $F1E2
Result_NumToHex:	= $F1F0

#data SPStack, $F200, $200
stack	= SPStack_end


#code   EEPROM_Startup, 0, *    ;EEPROM mem at 0000h

		nop
		nop
		nop

		ld HL, stack
		ld SP,HL                    ; Set stackpointer $BE00

		EI
		IM 2
		ld a,$04

		call PIO_Init
		call Z8536_Init				; display hardware port before:

		call HD44780_main_init		; display unit setup

		JP  RAMPAGE
		align 8            

#code   INT_IM1, $38, *    ;EEPROM mem at 0000h
		jp PIO_A_INT
		align 8
		defw $0400          ; NMI adress table    


#code PIO_A_INT, $50, *            ; PIO A interrupt handler

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



#code RAMPAGE, $100, *            ; RAM area        
RAMPAGE:

		; Try to print something
		ld ix,t_str1
		ld b, t_str2 - t_str1
		;ld de, $0102	; row 1 column 2
		;call setcursor	; runs also 'command'
		;
nextchar1:
		;ld a, (ix+0)
		;inc ix
		;call writedata
		;djnz nextchar1
		;out (portA_Data),a
		ld A,$AA
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		sla A
		call ShowPC_HALT

		; Try to print something 2
		ld ix,t_str2
		ld b, t_str3 - t_str2
		ld de, $0203	; row 2 column 3
		;call setcursor	; runs also 'command'
nextchar2:
		;ld a, (ix+0)
		;inc ix
		;call writedata
		;djnz nextchar2
		;out (portA_Data),a
		scf
		ld BC, $A987
		ld DE, $1234
		ld HL, $5678
		ld A,$30
		cp $40

		scf
		call ShowPC_HALT

		; Try to print something 2
		ld ix,t_str3
		ld b, t_string_E - t_str3
		ld de, $0304	; row 2 column 4
		call setcursor	; runs also 'command'
nextchar3:
		ld a, (ix+0)
		inc ix
		call writedata
		djnz nextchar3
		out (portA_Data),a

		call ShowPC_HALT
		nop
		inc ix
		nop
		inc ix

slut:      
		rst 00h 

PIO_Init:
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
	ret



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
		ld (SP_value), SP	; save contents of SP
		push AF
		push BC
		push DE
		push HL
		ld HL,(SP_value)
		push HL				; push the SP value on the stack...
		push IX
		Push IY

		
		; first print the labels: adr: t_str4 - 7
		;*****************************************
		ld de, $0000		; row 0, col 1
		ld IX, t_str4
nxt2:
		call setcursor		; set cur
		ld B, t_str5-t_str4	; all 4 rows has the same length
nxt3:
		ld a, (ix+0)
		inc ix
		call writedata
		djnz nxt3

		inc D				;Next row
		ld A,D
		cp $04				; all rows printed ?
		jr NZ, nxt2


		; set all values, first value (SP)
		;***************************************
		ld iy,(SP_value)		; top of stored stack
		ld IX,cur_pos			; table of cursor positions
		
		
nxt4:	ld L,(iy+0)
		ld H,(iy+1)
		dec IY
		dec IY					; next value in stack

		call Num4Hex			; convert value in HL
		;
		ld E,(IX+0)
		ld D,(IX+1)			; DE - cursor pos d-row e-col
		inc IX
		inc IX				; IX - next cursor position adr.
		call setcursor		; runs also 'command'
	;
		ld b, $04
		push IX
		ld IX, Result_NumToHex	; pointer to hex characters
nxt5:
		ld a, (IX+0)
		inc IX
		call writedata
		djnz nxt5
		pop IX
		ld a,(IX+0)				
		cp $FF					; check if end of cursor positions
		jr NZ, nxt4

		; print flags Z/NZ, C/NC, PO/PE, P/M
		;***************************************
		ld DE, $0011
		call setcursor
		ld IY,(SP_value)		; IY - top of stack
		ld A, (IY-2)
		ld (AF_value), A
		bit 6, (IY-2)					; test for Z flag
		jr NZ, nx_noNZ			
		ld A,'N'
		call writedata
nx_noNZ:
		ld A,'Z'
		call writedata
		inc D					; next row (E=$11), next flag (C)
		call setcursor
		;-----------------------------
		
		bit 0, (IY-2)					; test for C flag
		jr NZ, nx_noNC			
		ld A,'N'
		call writedata
nx_noNC:
		ld A,'C'
		call writedata
		inc D					; next row (E=$11), next flag (PE/PO)
		call setcursor
		;-----------------------------
		ld A,'P'
		call writedata
		bit 2,(IY-2)					; test for P/V flag
		jr Z, nx_PO			
		ld A,'E'				;parity even (PE)
		call writedata
		jr nx_sign
nx_PO:
		ld A,'O'					;parity even (PE)
		call writedata
nx_sign:		
		inc D					; next row (E=$11), next flag (sign)
		call setcursor
		;-----------------------------
		bit 7,(IY-2)					; test for S flag S=0 positive
		jr Z, nx_S			
		ld A,'M'				;sign negative (Minus)
		call writedata
		jr nx_hlt
nx_S:
		ld A,'P'					;sign positive (P)
		call writedata
nx_hlt:		
		;-----------------------------

		halt

		pop IY
		pop IX
		pop HL
		pop HL
		pop DE
		pop BC
		pop AF

		ret

	align 8            
t_intAstr:   .ascii "PIO A INT"
;************************************************************************
; Hexadecimal conversion operates directly on nibbles and takes advantage of nifty DAA trick.
;Input: HL = number to convert, IX = location of ASCII string
;Output: ASCII string at (IX) 
Num4Hex:  	; convert 2 bytes in HL
	push IX
	ld ix, Result_NumToHex
	ld	a,h
	call	Num1
	ld	a,h
	call	Num2
Num2Hex:	; converts 1 byte in L
	ld	a,l
	call	Num1
	ld	a,l
	call	Num2
	pop IX
	ret

Num1:
	rra
	rra
	rra
	rra
Num2:
	or	$F0
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


#code DAT_TABLE, DataTables,  $200


		align 8            
initbytes:   .byte $01, $38, $0E, $06, $B0
t_str1:		.ascii "Z80 micro and"
t_str2:		.ascii "HD44780 display"
t_str3:		.ascii "Z8536 assist->"
t_str4:		.ascii " PC:____ AF:____    "
t_str5:		.ascii " BC:____ DE:____    "
t_str6:		.ascii " HL:____ SP:____    "
t_str7:		.ascii " IX:____ IY:____    "
t_str8:		.ascii " "
t_str9:		.ascii " "
t_string_E: equ $
		align 2
cur_pos: equ $
		defw	$0004, $000C, $0104, $010C,$0204, $020C,$0304, $030C, $FFFF
		align 2
t_str_table: equ $
		defw	t_str1, t_str2, t_str3, t_str4, t_str5, t_str6, t_str7, t_str8


#code INT_TABLE, Interupt_vector, $10
		;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
		;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
		.word PIO_A_INT,PIO_A_INT,PIO_A_INT,PIO_A_INT
		
		;defw $0400          ; NMI adress table    
.end



