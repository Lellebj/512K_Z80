;Z80_PLD_PCB_.asm


		section  EEPROM_Startup    ;EEPROM mem at 0000h
EPS1:

		include 	"Z80_Params_.inc"
		xref	RAM_Start,PLD_PCB_Start, SC5B,SC4C,SC8B, WriteLineCRNL, WriteLine, ReadLine, CRLF,DumpRegisters

		xref	stacktop

		ld		sp,stacktop
		jp		setBanks
		
		section RST08
		jp	WriteLineCRNL	
		section RST10	
		jp 	WriteLine
		section RST18	
		jp	ReadLine
		section RST20	
		jp	CRLF
		section RST28	
		db 0,0,0
		section RST30	
		db 0,0,0
		section RST38	
		jp	DumpRegisters


;********************************************************
		section  INT_IM1     ;EEPROM mem at 0066h
;********************************************************

		LD C,04		; jp PIO_A_INT
		LD C,04		; jp PIO_A_INT
		LD C,04		; jp PIO_A_INT
		LD D,04		; jp PIO_A_INT
		retn		; jp PIO_A_INT
		defw $0400          ; NMI adress table    


;********************************************************		
		section EEtestprog			; main program in sram
;********************************************************	

		; xdef	RDATA_END,RDATA,TB_length

; RTestprog:
; 		;--------------------------------------------------
; 		; ld A,5
; 		; out (_CE_RST_BANK),A
; 		; ld 	A,$00	
; 		; out (_Z80_BankCS),A		// set bank register number 	
; 		; ld 	A,$01
; 		; out (_CE_RST_BANK),A 		// set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH

; 		out (_8Bitsout),A

; 		ld A, $0F                 ;mode 1 out
; 		out (portA_Contr), A         ; set port A as output
; 		ld A,$81

; tll:	
; 		ld (40000),A
; 		ld A,0
; 		ld A,(40000)

; 		out (portA_Data),A		; Data to PIO port A
; 		out (_8Bitsout),A
; 		;--------------------------------------------------
; 		ld	DE,$8100
; 		ld	HL,RDATA
; 		ld 	BC,RDATA_END-RDATA
; 		ldir
; 	if DOALIGN
; 		align 4
; 	endif

; RDATA:
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; 		defw	$1122, $2233, $3344, $5566, $ABCD, $FEDC, $DCBA, $AEAE
; RDATA_END:
; TB_length	equ 	RDATA_END-RDATA


setBanks:
		; ld 		A,$80					; set bit 7 - SRAM64 set
		; ld 		(memBankID),A			; clear memory banks
		
		call 	EEPIO_Init
		ld 		A,$55
		out 	(gpio_out),A

		xor 	A
		ld 		(memBankID),A			; set memory banks #0
		call 	EEsetFLASHBank				; FLASH bank #0
		xor 	A
		call 	EEsetSRAMBank				; ram bank #0
		ld 		A,$77
		out 	(gpio_out),A

		call 	EEenableFLASH			; start from FLASH

		call 	EEenableIC620_OE 			; enable the outputs.
		; jp		PLD_PCB_Start

;********************************************************************************************
;********************************************************************************************	
		; ******   Copy data from flash $1000 to $2FF0 to SRAM $D000
		; Code in $D002-D005 = '0000' - 'AAAA': copy from flash
		; Code in $D002-D005 = 'CCCC': code uploaded from xmodem/or DMA. Do not copy from flash

		ld 		HL,$D002
		ld  	A,'C'
		ld 		BC,04

.nxt:	cpi 	
		jr 		NZ,doCopy
		jp 		PE,.nxt
		;JP PE means "branch if BC has not been decremented to 0."

		; the code 'CCCC' is found in $D002-D005, do not copy from flashmem.
		jp		PLD_PCB_Start

doCopy:
		ld 		HL,$1000				; source
		ld 		DE,$D000	 			; destination
		ld 		BC,$1FF0				; 

		ldir

		jp		PLD_PCB_Start


;********************************************************************************************
;********************************************************************************************	
EEsetSRAMBank:
		; ***	set the SRAM bank ID; Bank ID in A
		push 	HL
		push 	BC
		ld 		HL,memBankID
		and 	$0F 				; clear all bits but 0-3

		ld 		B,A
		ld 		A,(HL)				; get the actl. mem Bank ID
		and 	$F0  				; zero bits 0-3
		or 		B					; put new SRAM bank ID in A...
		ld 		(HL),A				; store new value
		jr 		putBank

;********************************************************************************************
;********************************************************************************************	

EEsetFLASHBank:
		; ***	set the EEPROM bank ID; Bank ID in A
		push 	HL
		push 	BC
		ld 		HL,memBankID
		and 	$07 				; clear all bits but 0-2
		rlca
		rlca
		rlca
		rlca						; bank ID = bits 4-6

		ld 		B,A
		ld 		A,(HL)				; get the actl. mem Bank ID
		and 	$8F  				; zero bits 4-6
		or 		B					; put new EEP bank ID in A...
		ld 		(HL),A				; store new value
putBank:
		ld 		A,(HL)	
		out 	(_Z80_BankCS),A		; set bank register number 0 and 64K_SRAM=1	
		pop 	BC
		pop 	HL
		ret 

;********************************************************************************************
;********************************************************************************************	
EEenableFLASH:
		; ***	activate FLASH MEM, leave bank ID unchanged; 
				; if '64K_SRAM' 1  ($80) no FLASH memory is selected
				; if '64K_SRAM' 0  ($00) FLASH memory is lower 32k and SRAM upper 32k
		push 	HL
		push 	BC
		ld 		HL,memBankID
		res 	7,(HL)
		jr 		putBank
		
;********************************************************************************************
;********************************************************************************************	
EEdisableFLASH:
		; ***	disconnect FLASH MEM, leave bank ID unchanged; 
				; if '64K_SRAM' 1  ($80) no FLASH memory is selected
				; if '64K_SRAM' 0  ($00) FLASH memory is lower 32k and SRAM upper 32k
		push 	HL
		push 	BC
		ld 		HL,memBankID
		set 	7,(HL)
		jr 		putBank

;********************************************************************************************
;********************************************************************************************	


EEdisableIC620_OE:
		; ***	Set IC620 pin 1 high
		ld A,0
		out (_CE_RST_BANK),A			;IC620 (HC374) goes to high impedance.. all signals = GND
		; ld 	A,$00					; FLASH memory is lower 32k and SRAM upper 32k
		; out (_Z80_BankCS),A			; set bank register number 0 and 64K_SRAM=0	
		; ld 	A,$01
		; out (_CE_RST_BANK),A		; set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH
		ret


;********************************************************************************************
;********************************************************************************************	

EEenableIC620_OE: 
		; ***	Set IC620 pin 1 low
		ld A,1
		out (_CE_RST_BANK),A			;IC620 (HC374) goes to high impedance.. all signals = GND
		; ld 	A,$00					; FLASH memory is lower 32k and SRAM upper 32k
		; out (_Z80_BankCS),A			; set bank register number 0 and 64K_SRAM=0	
		; ld 	A,$01
		; out (_CE_RST_BANK),A		; set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH
		ret


;********************************************************************************************
;********************************************************************************************	

		; out (_8Bitsout),A
		

; 
EEPIO_Init:
; ;----------******************* PIO PORT A
		ld A, $0F                 ;mode 1 out
		out (portA_Contr), A         ; set port A as output
; 		ld A, Interupt_vector&0xFF                   ; low byte of INT table
; 		out (portA_Contr), A         ; PIO A interrupt vector
		ld A, $03
		out (portA_Contr), A         ; PIO A interrupt disable
; 		ld a,Interupt_vector>>8                   ; high byte of INT table
; 		ld I,A
; 		ei
; ;----------******************* PIO PORT B
; 		ld A, $0F                    ;mode 0 output 
; 		out (portB_Contr), A         ; set port A as output
; 		ld A, $03
; 		out (portB_Contr), A         ; PIO A interrupt disable
; 		ld a,0
; 		ld (PIO_B_value),a
; 		out (portB_Data), a
	ret
; 

;************************************************************************
; ShowPC_HALT:
; Dump prog counter prior to HALT instr. value present in stack (pointed by SP).
; uses  IX (pointer to HEX chars)
;       IY (pointer at stack)
;       BC ( count)
; ;       HL (value for conversion to HEX)
; ;       DE (positon of display 2004A)
	if DOALIGN
		align 8
	endif
            
; ShowPC_HALT:
; 		ld (SP_value), SP	; save contents of SP
; 		push AF
; 		push BC
; 		push DE
; 		push HL
; 		ld HL,(SP_value)
; 		push HL				; push the SP value on the stack...
; 		push IX
; 		Push IY

		
; 		; first print the labels: adr: t_str4 - 7
; 		;*****************************************
; 		ld de, $0000		; row 0, col 1
; 		ld IX, t_str4
; nxt2:
; 		call setcursor		; set cur
; 		ld B, t_str5-t_str4	; all 4 rows has the same length
; nxt3:
; 		ld a, (ix+0)
; 		inc ix
; 		call writedata
; 		djnz nxt3

; 		inc D				;Next row
; 		ld A,D
; 		cp $04				; all rows printed ?
; 		jr NZ, nxt2


; 		; set all values, first value (SP)
; 		;***************************************
; 		ld iy,(SP_value)		; top of stored stack
; 		ld IX,cur_pos			; table of cursor positions
		
		
; nxt4:	ld L,(iy+0)
; 		ld H,(iy+1)
; 		dec IY
; 		dec IY					; next value in stack

; 		call Num4Hex			; convert value in HL
; 		;
; 		ld E,(IX+0)
; 		ld D,(IX+1)			; DE - cursor pos d-row e-col
; 		inc IX
; 		inc IX				; IX - next cursor position adr.
; 		call setcursor		; runs also 'command'
; 	;
; 		ld b, $04
; 		push IX
; 		ld IX, Result_NumToHex	; pointer to hex characters
; nxt5:
; 		ld a, (IX+0)
; 		inc IX
; 		call writedata
; 		djnz nxt5
; 		pop IX
; 		ld a,(IX+0)				
; 		cp $FF					; check if end of cursor positions
; 		jr NZ, nxt4

; 		; print flags Z/NZ, C/NC, PO/PE, P/M
; 		;***************************************
; 		ld DE, $0011
; 		call setcursor
; 		ld IY,(SP_value)		; IY - top of stack
; 		ld A, (IY-2)
; 		ld (AF_value), A
; 		bit 6, (IY-2)					; test for Z flag
; 		jr NZ, nx_noNZ			
; 		ld A,'N'
; 		call writedata
; nx_noNZ:
; 		ld A,'Z'
; 		call writedata
; 		inc D					; next row (E=$11), next flag (C)
; 		call setcursor
; 		;-----------------------------
		
; 		bit 0, (IY-2)					; test for C flag
; 		jr NZ, nx_noNC			
; 		ld A,'N'
; 		call writedata
; nx_noNC:
; 		ld A,'C'
; 		call writedata
; 		inc D					; next row (E=$11), next flag (PE/PO)
; 		call setcursor
; 		;-----------------------------
; 		ld A,'P'
; 		call writedata
; 		bit 2,(IY-2)					; test for P/V flag
; 		jr Z, nx_PO			
; 		ld A,'E'				;parity even (PE)
; 		call writedata
; 		jr nx_sign
; nx_PO:
; 		ld A,'O'					;parity even (PE)
; 		call writedata
; nx_sign:		
; 		inc D					; next row (E=$11), next flag (sign)
; 		call setcursor
; 		;-----------------------------
; 		bit 7,(IY-2)					; test for S flag S=0 positive
; 		jr Z, nx_S			
; 		ld A,'M'				;sign negative (Minus)
; 		call writedata
; 		jr nx_hlt
; nx_S:
; 		ld A,'P'					;sign positive (P)
; 		call writedata
; nx_hlt:		
; 		;-----------------------------

; 		halt

; 		pop IY
; 		pop IX
; 		pop HL
; 		pop HL
; 		pop DE
; 		pop BC
; 		pop AF

; 		ret

; 	align 8        
; */

;
;t_intAstr:   .ascii "PIO A INT"
;************************************************************************
; Hexadecimal conversion operates directly on nibbles and takes advantage of nifty DAA trick.
;Input: HL = number to convert, IX = location of ASCII string
;Output: ASCII string at (IX) 
; Num4Hex:  	; convert 2 bytes in HL
; 	push IX
; 	ld ix, Result_NumToHex
; 	ld	a,h
; 	call	Num1
; 	ld	a,h
; 	call	Num2
; Num2Hex:	; converts 1 byte in L
; 	ld	a,l
; 	call	Num1
; 	ld	a,l
; 	call	Num2
; 	pop IX
; 	ret

; Num1:
; 	rra
; 	rra
; 	rra
; 	rra
; Num2:
; 	or	$F0
; 	daa
; 	add	a,$A0
; 	adc	a,$40

; 	ld	(ix+0),a
; 	inc	ix
; 	ret

; inc_portB_value:
; 		ld a, (PIO_B_value)
; 		inc a
; 		ld (PIO_B_value), a
; 		out (portB_Data), a
; 		ret


; #code DAT_TABLE, DataTables,  $200


	if DOALIGN
		align 8
	endif
            
; initbytes:   .byte $01, $38, $0E, $06, $B0
; t_str1:		.ascii "Z80 micro and"
; t_str2:		.ascii "HD44780 display"
; t_str3:		.ascii "Z8536 assist->"
; t_str4:		.ascii " PC:____ AF:____    "
; t_str5:		.ascii " BC:____ DE:____    "
; t_str6:		.ascii " HL:____ SP:____    "
; t_str7:		.ascii " IX:____ IY:____    "
; t_str8:		.ascii " "
; t_str9:		.ascii " "
; t_string_E: equ $
	if DOALIGN
		align 4
	endif

; cur_pos: equ $
; 		defw	$0004, $000C, $0104, $010C,$0204, $020C,$0304, $030C, $FFFF
	if DOALIGN
		align 4
	endif

; t_str_table: equ $
; 		defw	t_str1, t_str2, t_str3, t_str4, t_str5, t_str6, t_str7, t_str8


; #code INT_TABLE, Interupt_vector, $10
; 		;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
; 		;.byte $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04, $00, $04
; 		.word PIO_A_INT,PIO_A_INT,PIO_A_INT,PIO_A_INT
		
; 		;defw $0400          ; NMI adress table    



.end
