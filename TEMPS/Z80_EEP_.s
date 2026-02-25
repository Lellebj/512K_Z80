;Z80_PLD_PCB_.asm

GPIODEBUG EQU 1

		section  FLASH_Startup   	 ;FLASH mem at 0000h
EPS1:

		include 	"Z80_Params_.inc"
		xref	RAM_Start, SC5B,SC4C,SC8B, WriteLineCRNL, WriteLine, ReadLine, CRLF,DumpRegisters

		xref	stacktop
		global 	setFLASHBank, setSRAMBank, enableFLASH, disableFLASH, setFLASHBank

		ld		sp,stacktop
		
		jp		setBanks
		align 3
		; section RST08
		jp	WriteLineCRNL	
		align 3
		; section RST10	
		jp 	WriteLine
		align 3
		; section RST18	
		jp	ReadLine
		align 3
		; section RST20	
		jp	CRLF
		align 3
		; section RST28	
		db 0,0,0
		align 3
		; section RST30	
		db 0,0,0
		align 3
		; section RST38	
		jp	DumpRegisters


;********************************************************
		section  INT_IM1     ;FLASH mem at 0066h
;********************************************************

		LD C,04		; jp PIO_A_INT
		LD C,04		; jp PIO_A_INT
		LD C,04		; jp PIO_A_INT
		LD D,04		; jp PIO_A_INT
		retn		; jp PIO_A_INT
		defw $0400          ; NMI adress table    



;************************************************************************************************
;************************************************************************************************
;***		FLASH startup sequence (starts at $0080)
;************************************************************************************************
;************************************************************************************************
;********************************************************
		section  INIT_BODY     ;FLASH mem at 0080h
;********************************************************

		align 4
setBanks:
		; ld 		A,$80					; set bit 7 - SRAM64 set
		; ld 		(memBankID),A			; clear memory banks
		
		call 	EEPIO_Init
		
	ifd 	GPIODEBUG
		ld 		A,$55
		out 	(gpio_out),A
	endif

		xor 	A					; A=0
		ld 		(memBankID),A		; set memory banks #0
	ifd NOFLASH
		call 	disableFLASH		; NO FLASH 
	else	
		call 	enableFLASH			; start from FLASH
	endif

		xor 	A
		call 	setSRAMBank			; ram bank #0

	ifd 	GPIODEBUG
		ld 		A,$77
		out 	(gpio_out),A
	endif


		call 	enableIC620_OE 		; enable the outputs.

		jp		SD_USB_startup
	
;********************************************************************************************
;********************************************************************************************	
		; ******   Copy data from flash $400 to $2000 to SRAM $D000
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
		; jp		MONITOR_Start

doCopy:
		ld 		HL,$400				; source
		ld 		DE,$D000	 			; destination
		ld 		BC,$1FF0				; 

		ldir

		; jp		MONITOR_Start


;********************************************************************************************
;********************************************************************************************	
setSRAMBank:
		; ***	set the SRAM bank ID; Bank ID in A

		push 	HL
		push 	BC
		ld 		HL,memBankID
		and 	$0F 				; clear all bits but 0-3 in A

		ld 		B,A
		ld 		A,(HL)				; get the actl. mem Bank ID
		and 	$F0  				; zero bits 0-3
		jr 		putBank

;********************************************************************************************
;********************************************************************************************	

setFLASHBank:
		; ***	set the FLASH bank ID; Bank ID in A

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
putBank:
		or 		B					; put new EEP bank ID in A...
		ld 		(HL),A				; store new value
		out 	(_Z80_BankCS),A		; set bank register number 0 and 64K_SRAM=1	
		pop 	BC
		pop 	HL
		ret 

;********************************************************************************************
;********************************************************************************************	
enableFLASH:
		; ***	activate FLASH MEM, leave bank ID unchanged; 
				; if '64K_SRAM' 1  ($08) no FLASH memory is selected
				; if '64K_SRAM' 0  ($00) FLASH memory is lower 32k and SRAM upper 32k
		push 	HL
		push 	BC
		ld 		HL,rstBankID
		res 	3,(HL)				; clear bit 3 -> enable FLASH
		res 	2,(HL)				; temp enable reset of IC622
putBankF:
		ld 		A,(HL)
		out 	(_CE_RST_BANK),A		; set bank register number 0 and 64K_SRAM=1	
		pop 	BC
		pop 	HL
		ret 
		
;********************************************************************************************
;********************************************************************************************	
disableFLASH:
		; ***	disconnect FLASH MEM, leave bank ID unchanged; 
				; if '64K_SRAM' 1  ($08) no FLASH memory is selected
				; if '64K_SRAM' 0  ($00) FLASH memory is lower 32k and SRAM upper 32k
		push 	HL
		push 	BC
		ld 		HL,rstBankID
		set 	2,(HL) 			; temp disable reset of IC622
		set 	3,(HL)			; set bit 3 -> disable FLASH
		jr 		putBankF

;********************************************************************************************
;********************************************************************************************	


disableIC620_OE:
		; ***	Set IC620 pin 1 high
		push 	HL
		push 	BC
		ld 		HL,rstBankID
		res 	0,(HL)
		jr 		putBankF


;********************************************************************************************
;********************************************************************************************	

enableIC620_OE: 
		; ***	Set IC620 pin 1 low
		push 	HL
		push 	BC
		ld 		HL,rstBankID
		set 	0,(HL)
		jr 		putBankF


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
 		ld A, $0F                    ;mode 0 output 
 		out (portB_Contr), A         ; set port B as output
 		ld A, $03
 		out (portB_Contr), A         ; PIO B interrupt disable
 		ld a,0
 		ld (PIO_B_value),a
 		out (portB_Data), a
	ret
 

;************************************************************************************************
;************************************************************************************************
;***		SDcard/USB startup sequence
;************************************************************************************************
;************************************************************************************************
		; section SD_USB_Start


		; jp 		MONITOR_Start0 		; jump to MONITOR_Start if hard call to $D000

SD_USB_startup:

	ifdef  	GPIODEBUG
	ld 		A,$33
	out 	(gpio_out),A

	; call	Init_RAM_HEAP			; put zero values to addr $F000 - $FFF0

	ld 		(SP_value),SP

	ld 		A,$AA
	out 	(gpio_out),A
	
	CALL 	InitBuffers			;INITIALIZE in/Out buffers,	;INITIALIZE SIO_0. INTERRUPT SYSTEM
			; initialize buffer counters and pointers.
	ld 		A,$BB
	out 	(gpio_out),A

		call	PIO_Init
	ld 		A,$CC
	out 	(gpio_out),A
		call 	CTC_Init
	ld 		A,$DD
	out 	(gpio_out),A
		call 	SIO_Init			; LEV_Sect11_IO_Interrupts.s
	ld 		A,$DF
	out 	(gpio_out),A
		call	S_head_tail			; save input heads and tails
	ld 		A,$81
	out 	(gpio_out),A
	
	else
	
		CALL 	InitBuffers			;INITIALIZE in/Out buffers,	;INITIALIZE SIO_0. INTERRUPT SYSTEM
			; initialize buffer counters and pointers.
		call	PIO_Init
		call 	CTC_Init
		call 	SIO_Init			; LEV_Sect11_IO_Interrupts.s
		call	S_head_tail			; save input heads and tails
	endif


		; call	sh_test
		; call 	Flash_WR_Test
		; ld	HL,$2010
		; call	Flash_SE_Erase

		; check  $D008-$D00B for $33333333 -> Startup code is preloaded from Arduino 
		; check  $D008-$D00B for $CCCCCCCC -> start from Flash 

		ld 		HL,BootCodeAdr
		ld 		B,04
		ld 		A,'3'
.checkBootCode:
		cp 		(HL)	
		inc 	HL
		jp 		NZ,.SDstart
		djnz 	.checkBootCode


		call	CRLF
		call 	writeSTRBelow
		defb   	"\r\n"
		defb	"+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-+-=-\r\n"
		defb	"Start from Arduino preloaded monitor\r\n"
		defb	"    git: @@GIT_VERSION@@\r\n"
		defb	"    build: @@DATE@@\r\n"
		defb	"    FLASH->SRAM 0xD000.\r\n"
		defb	"\0"

		; call 	waitForFinishedPrintout
		jp 		_RAMSTART			; monitor start $D000 MONITOR_Start:
		

.SDstart:
		
		call	CRLF
		call 	writeSTRBelow
		defb   	"\r\n"
		defb	"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\r\n"
		defb	"Start from SD/USB\r\n"
		; defb	"    git: @@GIT_VERSION@@\r\n"
		; defb	"    build: @@DATE@@\r\n"
		; defb	"    FLASH->SRAM 0xD000.\r\n"
		defb	"\0"

		 call 	waitForFinishedPrintout

	ifd 	GPIODEBUG
	ld 		A,$83
	out 	(gpio_out),A
	endif
		call	CRLF

;*****	Setup Boot load from SD card.
;***************************************
		ld 		DE,commStr1					; save filename in commStr1
		ld 		HL,rfile_name
.nxtchr:
		ldi									; (DE) <- (HL) 
		ld 		A,(HL)
		or 		A 							; = 0 ?
		jr  	NZ,.nxtchr
		ld 		(DE),A						; save '0'
		ld 		HL,S1x						; result in S1x
		ld 		(commAdr1),HL

		call 	p_C_Read_SD


;***	correct $0A to $00 $00 in S1x (check for ascii lower than $20)
		ld 		HL,S1x
		ld 		A,$20
		ld 		DE,commStr1

.find0A:
		ldi						; (DE) <- (HL) 
		cp 		(HL)			; char lower than ' '  $20 - (HL)
		jp 		M,.find0A		; char > ' '...

		ld 		A,00
		ld 		(de),A
		inc 	de
		ld 		(de),A			; strip eventually $0A, $0D, ...
		inc 	de
		ld 		(de),A			; Boot file name present in commStr1
		ld 		HL,_RAMSTART
		ld 		(commAdr1),HL 	; place adress for boot file...
		call 	p_C_Read_SD		; read and place boot file.


		call 	writeSTRBelow
		defb   "\r\nUSE RAM bank #0, Copy FLASH Boot seq\r\n"
		defb   "To RAM bank #1 ($0-$2000) \r\n"
		defb	"Jump to MONITOR_Start! ($D000)\r\n",0,0,0

		jp 		_RAMSTART			; monitor start $D000 MONITOR_Start:

.loopINF:
	ifd 	GPIODEBUG	
	ld 		A,$99
	out 	(gpio_out),A
	endif
		jr 		.loopINF
rfile_name:
	 db "BOOTFILE.TXT",0,0,0,0
	; db "PROVIDE.txt",0,0,0,0

	
;************************************************************************************************
;************************************************************************************************
p_C_Read_SD:

		;call 	checkArgsTAL				; check necessary args
		;jp		NZ,argumentsError			; show argument error and return
	
		ld 		DE,CTC_delay_INT_handler
		ld 		(CTC_CH1_I_Vector),DE
	ifd 	GPIODEBUG
	xor A
	out (gpio_out),A
	endif
		; call  	SIO_A_DI					; disable text output
	ifd 	GPIODEBUG
	ld a,4
	out (gpio_out),A
	ld a,0
	out (gpio_out),A
	endif

		ld a,e	

		call 	purgeRXB					; XMODEM_CRC_SUB.s
		call 	initSIOBInterrupt			; turn on interrupt on SIO B (CH376S) LEV_Sect11_IO_Interrupts.s
		call 	HC376S_ResetAll
		call 	HC376S_CheckConnection
		; ld 		A,(commParseTable)
		; cp 		15							; 15 read SD; 17-read USB
		; jr 		Z,.doSD
		; cp 		21							; 21 read SD enumerate, 22 read USB enumerate
		; jr 		Z,.doSD
		; call 	HC376S_setUSBMode
		; call 	HC376S_diskConnectionStatus		; dont use with SD card
		; jr 		.cont
.doSD:
		call 	HC376S_setSDMode
		
.cont:
		call 	HC376S_USBdiskMount				; ret with NZ  on failure
		jr 		NZ,SDabort


		call 	HC376S_setFileName
		call 	HC376S_fileOpen
		jr 		NZ,SDabort
		call 	waitForFinishedPrintout

		call 	HC376S_getFileSize
		call 	HC376S_fileRead
		call 	HC376S_fileClose
SDabort:

		; ***	reset the interrupt handler for CTC
		; call 	SIO_A_EI					; enable text output
		call 	HC376S_ResetAll
		call 	CTC1_INT_OFF
		ld		HL,CTC_CH1_Interrupt_Handler
		ld		(CTC_CH1_I_Vector),HL		;STORE CTC channel 1 VECTOR
		ret

MONITOR_Start0:	

;***********************************************************************



		
;********************************************************		
		section EEtestprog			; main program in sram
;********************************************************	

		; xdef	RDATA_END,RDATA,TB_length
hit:

		call	EEPIO_Init
		di
hit3:
		LD 		A,$81
		; out 	(_CE_RST_BANK),A
		out 	(gpio_out),A


		LD 		A,$7E
		; out 	(_Z80_BankCS),A
		out 	(gpio_out),A

		ld 		ix,$8000

		ld 		bc,$0F00
		ld 		A,$3F
		ld 		(IX+0),A
		inc 	IX
.nxt1:
		ld 		(IX+0),A
		out 	(gpio_out),A
		inc 	IX
		inc 	A
		djnz 	.nxt1			

;***********************************
	 	ld 		A,$1B
		out 	(gpio_out),A
		out 	(gpio_out),A
	 	ld 		A,$E3
		out 	(gpio_out),A
		ld 		A,$0C
		out 	(gpio_out),A
		
		ld 		A,$0F
		out 	(gpio_out),A

		CALL 	InitBuffers			;INITIALIZE in/Out buffers,	;INITIALIZE SIO_0. INTERRUPT SYSTEM
			; initialize buffer counters and pointers.


		call 	CTC_Init


		ld 		A,$1D
		out 	(gpio_out),A
		ld 		A,$00
		out 	(gpio_out),A

		call 	SIO_Init
		; ld      HL,SIO_0INT		;BASE ADDRESS OF INITIALIZATION ARRAY
		; call    InitSIO_0Ports			; INITIALIZE SIO_0


		ld 		A,$1E
		out 	(gpio_out),A

		; ld 		A,'#'
		; out		(SIO_A_D),A			;output data
		; call 	TX_EMP

		; ld 		A,$18
		; out 	(gpio_out),A

		; ld 		A,'A'
		; call  	WriteChar

		; ld 		A,'B'
		; call  	WriteChar

		; ld 		A,'C'
		; call  	WriteChar
		; call 	CRLF

		call 	writeSTRBelow
		defb   	"\0\r\n"
		defb	"##########################################################\r\n"
		defb	"-*-*/-*/-*/-*/-*//-*/-*/-*/-*/-*/-*/**-/-*/-*/-*/-*/-*/-*/\r\n"
		defb	"-+-+-+-+-+-+-+---++--++--++--++--++--+++-+-+-+-+-+-+-+-+-+\r\n"
		defb	"The Z80 Board Awakened 2025\r\n"
		defb	"    FLASH->SRAM 0xD000.\r\n"
		defb	"    2026-02-07 .\r\n"
		defb	"\0\0"

		halt


;***************************
		halt
;**************************
		ld  	HL,$0402

		call 	EEPIO_Init
		LD 		A,$30

		ld 		B,$40
hit2:		
		ld 		(HL),a
		inc 	a
		INC 	HL
		out 	(gpio_in),A

	
		djnz 	hit2
	
		LD 		(hl),a
		inc 	a
		INC 	HL
		out 	(gpio_in),A
		LD 		(hl),a
		inc 	a
		INC 	HL
		out 	(gpio_in),A
		LD 		(hl),a
		inc 	a
		INC 	HL
		out 	(gpio_in),A
		LD 		(hl),a

		LD 		A,$0C
		out 	(_CE_RST_BANK),A

		

TX_EMP:	sub a
		inc a
		out (SIO_A_C),A
		in 	a,(SIO_A_C)
		bit 0,A
		jr  z,TX_EMP
		ret




.end


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


