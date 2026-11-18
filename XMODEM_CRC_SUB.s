;XMODEM_SUB.s

		include 	"Z80_Params_.inc"
 
		xdef 	RAM_Start,SetupXMODEM_TXandRX,purgeRXA,purgeRXB,TX_EMP,TX_NAK,TX_ACK,SIO_A_DI,SIO_A_EI,SIO_A_RESET
		xdef 	SIO_A_RTS_OFF,SIO_A_RTS_ON,SIO_A_TXRX_INToff,SIO_A_TXon,SIO_A_RXon,SIO_A_TXRX_INTon,TX_C,TX_X
		xdef 	doImportXMODEM, Start_WaitRDY, Stop_WaitRDY, CRC16
		GLOBAL 	WaitForNextChar, restoreSIO_0IO
;********************************************************		
;		Routines in order to read data via KERMIT on SIO_0 chA
;********************************************************		

		section Xmodems

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

		;***	stäng av wait funktion
		;***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
Stop_WaitRDY:
		push 	af
		ld		A,WR1			;Skriv SIO skrivreg  WR0: ange skrivregister 1 WR1
		out		(SIO_A_C),A
		ld		A,_WAIT_READY_R_T|_Rx_INT_First_Char		;stäng av wait funktion från nu
		out		(SIO_A_C),A
		pop 	af
		ret	
		;***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	

		;***	starta av wait funktion
		;***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
Start_WaitRDY:
		push 	af
		ld		A,_EN_INT_Nxt_Rx_Char|WR1				;skrivreg 0 ange WR1 ( sätt igång  INT när nästa tecken finns)
		out		(SIO_A_C),A
		; ld		A,_Rx_INT_First_Char			;'wait' aktiverat , interrupt vid nästa RX tecken 
		ld		a,_WAIT_READY_EN|_WAIT_READY_R_T		;'wait' aktiverat , ej interrupt  
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition
		pop 	af
		ret
		;***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	

; SPEC_RX_CONDITON:
; 		jp		0000h



doImportXMODEM: 

		call 	writeSTRBelow
		DB 		0,"Invänta XMODEM start... ",CR,LF,00
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

		ld		HL,receiveBlockIn       	; Avbrottsrutin för SIO A

; doKERMITentry:		; ***  Kermit initiering hoppar hit

		ld		(SIO_Int_Read_Vec),HL		;STORE READ VECTOR

		; ld 		A,_Reset_STAT_INT|_Reset_STAT_INT	
		; out		(SIO_A_C),A
		ld		A,_EN_INT_Nxt_Rx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;wait active, interrupt on first RX character
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition

		call  	purgeRXA

		ld 		HL,(commAdr1)
		ld 		C,1					; block number


;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

XnextC:		
		ei

XnextBlock:
		
		call 	WaitForNextChar					; initiera avbrott (receiveBlockIn), vänta på nästa tecken...

		; ***	stäng av wait funktion
		ld		A,WR1			;Skriv SIO skrivreg  WR0: ange skrivregister 1 WR1
		out		(SIO_A_C),A
		ld		A,_WAIT_READY_R_T|_Rx_INT_First_Char		;stäng av wait funktion från nu
		out		(SIO_A_C),A

		ld 		A,E

		cp		CTCtimeout					; timeout ; ingen fil vald..(16 st 'C'), CTC avbrptt 'CTC_CH0_Interrupt_Handler'
		jp		Z,timeOutErr		

		cp 		CTCpulse 					; retur från CTC 'CTC_CH0_Interrupt_Handler'
		jp 		Z,XnextC 					; skriv ut 'C' -> forts till .nextC

		cp		NUL							;block finished, no error
		jp		Z,XblockFinished

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
		

XblockFinished:
		call	TX_ACK					;when no error
		inc		C						;prepare next block to receive
		sub		A
		ld		(TempVar2),A			;clear bad block counter
		jr 		XnextC



;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
		
WaitForNextChar:	
		push 	BC 						; spara blocknummer (C)
		ld		A,_EN_INT_Nxt_Rx_Char|WR1			;skriv till WR1  ( enable INT on next char)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;Ej wait (Wait EN(bit7) = 0) interrupt on first RX character
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition
		ei


		call 	SIO_A_RTS_ON			; 8 bitars teckenlängd (6&5), break (4), TX enable, RTS enable

		halt						;vänta på första tecken SIO A (receiveBlockIn) eller CTC puls... (CTC_CH0_Interrupt_Handler)

		;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
		;		Retur till denna pos efter avbrottsrutin. Ev felkoder i E (=0 är OK)
		;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

		call 	SIO_A_RTS_OFF			; 8 bitars teckenlängd (6&5), ingen break (4), TX enable
		pop  	BC 						; Återställ blocknummer (C)
		ret 



;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
; 		Avbrottshantering vid XMODEM
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
receiveBlockIn:
		
		out  	($70),A					; start pulseview

		; CH1 counter not send any interrupts
		call 	CTC1_INT_OFF			; CH1 counter not send any interrupts

		call 	Start_WaitRDY			; aktivera wait - RDY för SIO-CPU ej SIO interrupt

		ld 		(XBAddr),HL						; startadress där block sparas (commAdr1)

		in		A,(SIO_A_D)			;read RX byte into A
checkByte01:
		cp		SOH					;hittat 128 byte block (SOH)
		jp		z,checkBlockNum
		cp		STX					;hittat 1024 byte block (STX)
		JP 		Z,checkBlockNum
		cp		EOT					;check for EOT
		jp		nz,Er01_
		ld		e,EOT_FOUND			;EOT (4) hittad (end of transmission)
		reti					; avbrottsretur med kod (=2) i E  EOT

;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
; 		kolla status på SIO och läs in data till A när data finns.  (pulling)	
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
InChar: 					; SIO läser in till A !:
		in a, (SIO_A_C)
		bit 0, a        ; Finns nytt tecken?
		jr z, InChar					; SIO läser in till A !
		in a, (SIO_A_D) ; Läs datat
		ret
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

		


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


		;kolla blocknummer. hoppar hit om tecken A är SOH(01)
checkBlockNum:
		ld 		(PktLen),A			; lagra '1',SOH eller '2' (STX) i PktLen
		out  	($60),A					; start pulseview
		call 	InChar					; SIO läser in till A !				;läs nästa byte till A (blocknummer)
		cp		C					;kolla om rätt block nr
		jp		nz,Er02_			; wrong block number (09)

		;kolla inverterat block number
		ld		A,C					;förväntat blocknummer till A
		CPL							;invertera	
		ld		E,A					;förväntat inverterat blocknummer till A

checkComplBlockNum:		
		call 	InChar					; SIO läser in till A !			;läs nästa byte till A (inverterat blocknummer)
		cp		E					;jämför med förväntat inverterat blocknummer(A)
		jp		nz,Er03_			; felmeddelande fel blocknummer
	
		; ld		DE,0000					;startvärde checksumma CRC16
		; ld 	 	(TempVar0),DE
		ld 		BC,$400 				; anta block med 1024 bytes
		ld      A,(PktLen)			; SOH(1) eller (STX)2
		cp 		STX
		jr 		Z,getBlockData

		;hämta aktuellt block med data
		ld 		BC,$80 				; ändra till block 128 bytes
getBlockData:
		call 	InChar					; SIO läser in till A !				; läs in byte till A om ny byte finns
		; in		A,(SIO_A_D)	
		ld		(HL),A				;spara ny byte
		; add		A,D
		; ld		D,A					;uppdaterad checksumma i D
		inc		HL					;address +1
		dec 	BC 					; byteräknare


		ld 		A,B
		or 		C					; BC = 0 ?
		jr  	NZ,getBlockData

checkBlockChecksum:

		call 	InChar					; SIO läser in till A !
		ld 		D,A
		call 	InChar					; SIO läser in till A !
		ld 		E,A					; DE = inlästa blockets checksumma 
		ld 		(XMChkSum),DE

		; ***	Beräkna  CRC16
		push 	BC
		push 	HL
		ld 		HL,(XBAddr)			; låt HL peka på startadress för datapaket
		ld 		BC,$400				; anta 1k datablock (STX)
		ld      A,(PktLen)			; SOH(1) eller (STX)2
		cp 		STX
		jr 		Z,.bigBlock
		ld 		BC,$80				; ladda räknare för små (128 byte) datablock
.bigBlock:
		call	CRC16				; resulterande CRC i DE
		ld 		HL,(XMChkSum)		; HL = blockets checksumma
		or 		A 					; släck carry
		sbc 	HL,DE				; differens  (borde vara 0)
		; ***	if checksum OK the Z is set
		pop 	HL					
		pop  	BC					; restore  HL and BC


		jr		z,retBlockComplete	; Z flagga -> aktuellt block är OK
		ld		e,_err04_			; annars skriv ut fel
		reti						; avbrottsretur med felkok i E

;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

retBlockComplete: 
		ld		E,0h
		reti						;avbrottsretur med felkok 0 i E (status OK)

;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

restoreSIO_0IO:
		di
		ld		A,_Counter|_Rising|_Reset|_CW	
		out		(CH1),A				; CH1 counter - disable interrupt

		CALL 	InitBuffers			;Nollställ in/ut buffers, SIO A. INTERRUPT SYSTEM

		call 	SIO_A_TXRX_INTon
		call 	SIO_A_RTS_ON
		ei
		ret

exitRecBlock:
		push HL

	; ***	File transfer OK
		call 	TX_ACK
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"Filen inläst OK !",00

		pop HL

		ld DE,(commAdr1)		; get the start address of memory to write to
		ccf
		sbc  HL,DE
		; call DumpRegisters

		ret

		
timeOutErr:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"Timeout för XMODEM inträffade!",00
		ret

blockErrors1_3:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"Fil överföringsfel : Format, Block ID, ... !",00
		ret

retry9Err:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"XMODEM: 9 försök att läsa block... !",00
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
		jp 		MainLoop		;repeat block reception


; Calculating XMODEM CRC-16 in Z80
; ================================


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
		jr 		NC,clear			; b15 was zero						|
		ld 		D,A					; Put CRC high byte back into D		|
		ld 		A,E
		xor 	$21
		ld 		E,A					; CRC=CRC XOR $1021, XMODEM polynomic	|
		ld 		A,D
		xor 	$10					; And get CRC top byte back into A	|
clear:                    ;											|
		dec 	B	
		jr 		NZ,rotlp				; Loop for 8 bits					|

		ld 		D,A					; Put CRC top byte back into D		|
;		 -----------------------------------------------------------+

		inc		HL					; Step to next byte
		pop 	BC
		dec 	BC					; num=num-1
		ld 		A,B
		or 		C
		jr 		NZ,bytelp			; Loop until num=0

		ret



	;***************************************************************
	;SAMPLE EXECUTION:
	;***************************************************************



