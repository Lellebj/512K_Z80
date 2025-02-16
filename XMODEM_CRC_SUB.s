;XMODEM_SUB.s

		include 	"Z80_Params_.inc"
 
		xdef 	RAM_Start,SetupXMODEM_TXandRX,purgeRXA,purgeRXB,TX_EMP,TX_NAK,TX_ACK,SIO_A_DI,SIO_A_EI,SIO_A_RESET
		xdef 	SIO_A_RTS_OFF,SIO_A_RTS_ON,SIO_A_TXRX_INToff,SIO_A_TXon,SIO_A_RXon,SIO_A_TXRX_INTon,TX_C,TX_X
		xdef 	doImportXMODEM,CRC16
;********************************************************		
;		Routines in order to read data via XMODEM on SIO_0 chA
;********************************************************		

		section Xmodems
RAM_Start:
		; jp 		MONITOR_Start

;********************************************************************************************
;********************************************************************************************
;********************************************************************************************


RX_CR:
		;do something on carriage return reception here
		jp		EO_CH_AV

RX_BS:
		;do something on backspace reception here
		jp		EO_CH_AV
EO_CH_AV:
		ei						;see comments below
		call	SIO_A_RTS_ON		;see comments below
		pop		AF				;restore AF
		Reti
	

SPEC_RX_CONDITON:
		jp		0000h

;**************************************************************************
;**				SetupXMODEM_TX and RX:									**
;**************************************************************************



doImportXMODEM: 
		call 	writeSTRBelow
		DB 		0,"Wait for XMODEM start... ",CR,LF,00
		xor 	A
		ld 		(TempVar1),A				; reset badblock counter
		;------------INIT CTC (2 sec timing for 'C'/NAK process----------------
		;init CH 0 and 1
		ld 	 	A,_Counter|_Rising|_TC_Follow|_Reset|_CW
		out		(CH0),A 		; CH0 is on hold now
		ld		A,$42			; time constant (prescaler; $42;$DA; 14390,625 khz -> 2, sec peroid;  
		out		(CH0),A			; and loaded into channel 0
		
		ld		A,_INT_EN|_Counter|_Rising|_TC_Follow|_Reset|_CW	
		out		(CH1),A			; CH1 counter
		ld		A,$DA			; time constant 
		out		(CH1),A			; and loaded into channel 2

		;------------INIT SIO----------------------------------------

		ld		HL,receiveBlockIn       	; ON INTERRUPT SIO_0 channel A
		ld		(SIO_Int_Read_Vec),HL		;STORE READ VECTOR

		ld 		A,_Reset_STAT_INT|_Reset_STAT_INT	
		ld		A,_EN_INT_Nx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;wait active, interrupt on first RX character
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition

		call  	purgeRXA

		ld 		HL,$B010
		ld 		C,1					; block number



;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

nextC:		
		ei

nextBlock:
		ld		A,_EN_INT_Nx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;wait active, interrupt on first RX character
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition
		ei


		call 	SIO_A_RTS_ON

		halt						;await first rx char

		call 	SIO_A_RTS_OFF

		; ***	wait function inactive
		ld		a,WR1			;write into WR0: select WR1
		out		(SIO_A_C),A
		ld		a,_WAIT_READY_R_T|_Rx_INT_First_Char		;wait function inactive
		out		(SIO_A_C),A

		;check return code of block reception (e holds return code)
		ld 		A,E
		out 	(portB_Data),A
		ld 		($B000),A
		cp		CTCtimeout					; timeout error ; no file transfer started
		jp		Z,timeOutErr		

		cp 		CTCpulse 					; ret from CTC
		jr 		Z,nextC 					; one more 'C' -> goto .nextC

		cp		NUL							;block finished, no error
		jp		Z,blockFinished

		cp		EOT_FOUND					;eot found (end of transmission)
		jp		Z,exitRecBlock

		cp		_err01_						;Byte 1 not recognized (08)
		jp		Z,blockErrors1_3

		cp		_err02_						;wrong block number (09)
		jp		Z,blockErrors1_3

		cp		_err03_						;wrong complement block number  (0C)
		jp		Z,blockErrors1_3

		cp		_err04_						;chk sum error  (0D)
		jp		Z,checkSumErr

		jp		blockErrors1_3
		

blockFinished:
		call	TX_ACK					;when no error
		inc		C						;prepare next block to receive
		sub		A
		ld		(TempVar2),A			;clear bad block counter
		jr 		nextC

;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*


receiveBlockIn:

		; CH1 counter not send any interrupts
		call 	CTC1_INT_OFF			; CH1 counter not send any interrupts

		ld		A,WR1								;write into WR0 cmd4 and select WR1 
		out		(SIO_A_C),A
		ld		a,_WAIT_READY_EN|_WAIT_READY_R_T	;wait active, 
		out		(SIO_A_C),A						;buffer overrun is a spec RX condition
		; ld		A,_EN_INT_Nx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
		; out		(SIO_A_C),A
		; ; ld		A,_Rx_INT_First_Char			;wait active, interrupt on first RX character
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		; out		(SIO_A_C),A					;buffer overrun is a spec RX condition

		ld 		(XBAddr),HL						; save actual block start address 

		in		A,(SIO_A_D)			;read RX byte into A
		ld 		($B008),A
checkByte01:
		cp		SOH					;check for SOH
		jp		z,checkBlockNum
		cp		EOT					;check for EOT
		jp		nz,Er01_
		ld		e,EOT_FOUND			;eot found (end of transmission)
		reti
		
		
		; jr		.nextC

		; jr		.nextC


Er01_:	; Byte 1 not recognized
		ld		E,_err01_
		RETI

Er02_:	; wrong block number
		ld		E,_err02_
		RETI

Er03_:	; wrong complement block number
		ld		E,_err03_
		RETI

Er04_:
		ld		E,_err04_
		RETI

		;check block number
checkBlockNum:
		in		A,(SIO_A_D)		;read RX byte into A	
		ld 		($B009),A
		cp		C					;check for match of block nr
		jp		nz,Er02_			; wrong block number (09)

		;get complement of block number
		ld		A,C					;copy block nr to expect into A
		CPL							;and cpl A	
		ld		E,A					;E holds cpl of block nr to expect

checkComplBlockNum:
		in		A,(SIO_A_D)		;read RX byte into A
		ld 		($B00A),A
		cp		E					;check for cpl of block nr
		jp		nz,Er03_			; wrong complement block number

		;get data block
		ld		D,0h				;start value of checksum
		ld		B,80h				;defines block size 128byte

getBlockData:
		in		A,(SIO_A_D)		;read RX byte into A
		ld		(HL),A				;update
		add		A,D
		ld		D,A					;checksum in D
		inc		HL					;dest address +1
		ld 		A,B
		ld 		($B002),A
		djnz	getBlockData		;loop until block finished


checkBlockChecksum:

		in		A,(SIO_A_D)		;read RX hi byte into A
		ld 		D,A
		in		A,(SIO_A_D)		;read RX low byte into A
		ld 		E,A					; DE = checksum in file
		ld 		(XMChkSum),DE
		; ***	Calculate CRC16

		push 	BC
		push 	HL
		ld 		HL,(XBAddr)			; get the block start address
		ld 		BC,$80

		call	CRC16				; result CRC in DE
		ld 		($F1AC),DE
		ld 		HL,(XMChkSum)		; get the file checksum in HL
		or 		A 					; clear carry
		sbc 	HL,DE				; calc the differnce
		; ***	if checksum OK the Z is set
		pop 	HL					
		pop  	BC					; restore  HL and BC


		jr		z,retBlockComplete
		ld		e,_err04_
		reti						;return with checksum error
retBlockComplete: 
		ld		E,0h
		reti						;return when block received completely

restoreSIO_0IO:
		di
		ld		A,_Counter|_Rising|_Reset|_CW	
		out		(CH1),A				; CH1 counter - disable interrupt

		CALL 	InitBuffers			;INITIALIZE in/Out buffers,	;INITIALIZE SIO_0. INTERRUPT SYSTEM

		call 	SIO_A_TXRX_INTon
		call 	SIO_A_RTS_ON
		ei
		ret

exitRecBlock:
		; ***	File transfer OK
		call 	TX_ACK
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"XMODEM file transfer OK !",00
		ret


timeOutErr:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"A timout on XMODEM occured !",00
		ret

blockErrors1_3:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"File transfer error: Format, Block ID, ... !",00
		ret

retry9Err:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"XMODEM: Block retry 9 times... !",00
		ret



;************

checkSumErr: 
		call 	TX_NAK 			;on chk sum error
		scf
		ccf						;clear carry flag
		ld		DE,0080h		;subtract 80h
		sbc 	HL,DE 			;from HL, so HL is reset to block start address

		ld		A,(TempVar2)		;count bad blocks in TempVar2
		inc		A
		ld		(TempVar2),A
		cp		09h
		jp		z,retry9Err		;abort download after 9 attempts to transfer a block
		jp 		nextBlock		;repeat block reception


; Calculating XMODEM CRC-16 in Z80
; ================================

; Calculate an XMODEM 16-bit CRC from data in memory. This code is as
; tight and as fast as it can be, moving as much code out of inner
; loops as possible. Can be made shorter, but slower, by replacing
; JP with JR.

; On entry, crc..crc+1   =  incoming CRC
;           addr..addr+1 => start address of data
;           num..num+1   =  number of bytes
; On exit,  crc..crc+1   =  updated CRC
;           addr..addr+1 => undefined
;           num..num+1   =  undefined

; Multiple passes over data in memory can be made to update the CRC.
; For XMODEM, initial CRC must be 0. Result in DE..

CRC16:
		ld 		DE,$00					; Incoming CRC
		; Enter here with HL=>data, BC=count, DE=incoming CRC
bytelp:
		push 	BC					; Save count
		ld 		A,(HL)				; Fetch byte from memory
;		 The following code updates the CRC with the byte in A -----+
		xor 	D					; XOR byte into CRC top byte		|
		ld 		B,8					; Prepare to rotate 8 bits			|
rotlp:                    ;											|
		sla 	E
		adc 	A,A					; Rotate CRC						|
		jp 		NC,clear			; b15 was zero						|
		ld 		D,A					; Put CRC high byte back into D		|
		ld 		A,E
		xor 	$21
		ld 		E,A					; CRC=CRC XOR $1021, XMODEM polynomic	|
		ld 		A,D
		xor 	$10					; And get CRC top byte back into A	|
clear:                    ;											|
		dec 	B	
		jp 		NZ,rotlp				; Loop for 8 bits					|
		ld 		D,A					; Put CRC top byte back into D		|
;		 -----------------------------------------------------------+

		inc		HL					; Step to next byte
		pop 	BC
		dec 	BC					; num=num-1
		ld 		A,B
		or 		C
		jp 		NZ,bytelp			; Loop until num=0

		ret



	;***************************************************************
	;SAMPLE EXECUTION:
	;***************************************************************



