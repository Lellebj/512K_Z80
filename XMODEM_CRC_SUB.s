;XMODEM_SUB.s

		include 	"Z80_Params_.inc"
 
		xdef 	RAM_Start,SetupXMODEM_TXandRX,purgeRXA,purgeRXB,TX_EMP,TX_NAK,TX_ACK,SIO_A_DI,SIO_A_EI,SIO_A_RESET
		xdef 	SIO_A_RTS_OFF,SIO_A_RTS_ON,SIO_A_TXRX_INToff,SIO_A_TXon,SIO_A_RXon,SIO_A_TXRX_INTon,TX_C,TX_X
		xdef 	doImportXMODEM, doImportKERMIT, CRC16
;********************************************************		
;		Routines in order to read data via XMODEM on SIO_0 chA
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
	

SPEC_RX_CONDITON:
		jp		0000h

;**************************************************************************
;**				SetupXMODEM_TX and RX:									**
;**************************************************************************

doImportKERMIT:

		call 	writeSTRBelow
		DB 		0,"Väntar på minicom wermit... ",CR,LF,00
		xor 	A
		;		*** 	läs in tecken till dess ^A [01] hittas.


		;------------INIT SIO för KERMIT----------------------------------------

		; ld 		HL,(commAdr1)

		ld		HL,recMinicomTecken      	; Avbrott för SIO A, när tecken kommer från minicom
		ld		(SIO_Int_Read_Vec),HL		; spara avbrottsvektor (recMinicomTecken) i SIO_Int_Read_Vec
		ld 		A,_Reset_STAT_INT|_Reset_STAT_INT	
		out		(SIO_A_C),A
		ld		A,_EN_INT_Nxt_Rx_Char|WR1			;skrivreg 0 ange WR1 ( sätt igång  INT när nästa tecken finns)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;'wait' aktiverat , interrupt vid nästa RX tecken 
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition

		call  	purgeRXA

		ld 		HL,(commAdr1)
		ld 		C,1					; block number
		xor  	A
nextC:		
		ei

MainLoop:
		out  	($60),A					; start pulseview

		call 		WaitForNextChar					; vänta på avbrott från SIO/CTC  avbrott rutin 'recMinicomTecken', resultat A/E

		; ld		A,_EN_INT_Nxt_Rx_Char|WR1				;skrivreg 0 ange WR1 ( sätt igång  INT när nästa tecken finns)
		; out		(SIO_A_C),A
		; ld		A,_Rx_INT_First_Char			;'wait' aktiverat , interrupt vid nästa RX tecken 
		; ; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char			;'wait' aktiverat , interrupt vid nästa RX tecken 
		; out		(SIO_A_C),A		;buffer overrun is a spec RX condition
		; ei


		; call 	SIO_A_RTS_ON			; 8 bitars teckenlängd (6&5), break (4), TX enable, RTS enable

		; halt						;vänta på första tecken SIO A
		; 							; avbrott: recMinicomTecken, resultat i A och E. HL-> (XBaddr)

		; call 	SIO_A_RTS_OFF			; 8 bitars teckenlängd (6&5), ingen break (4), TX enable

		; ***	stäng av wait funktion
		; ***	************************
		ld		A,WR1			;Skriv SIO skrivreg  WR0: ange skrivregister 1 WR1
		out		(SIO_A_C),A
		ld		A,_WAIT_READY_R_T|_Rx_INT_First_Char		;stäng av wait funktion från nu
		out		(SIO_A_C),A
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	

		ld  	A,E
		cp		SOH					;Sök 'MARK' /SOH  ^A
		jp		Z,getLength		; om inte; sök nytt tecken.
		call 	TX_EMP				; skriv ut tecken till SIO
		jr  	MainLoop

		; ***	Paket längd (kodad)
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
getLength:
	
        call InChar
            ld b, a         ; Spara rå-längd för checksumma
            add a, c
            ld c, a         ; Uppdatera checksumma
            ld a, b
		sub CHAR_OFFSET 
		ld (PktLen), a				; Avkodad längd i A sparas i PktLen
		ld b, a        				 ; B = Loopräknare för resten av paketet (exkl. MARK & LEN)

		; ***	Sekvensnummer
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
        call InChar
            ld d, a         ; Spara rå-sekvens
            add a, c
            ld c, a         ; Uppdatera checksumma
            ld a, d
		sub CHAR_OFFSET
		ld (PktSeq), a				; Avkodad sekvensnummer i A sparas i PktSeq
		dec b

		; ***	Läs pakettyp
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
        call InChar
		ld (PktType), a				; Avkodad sekvensnummer i A sparas i PktType
            add a, c
            ld c, a         ; Uppdatera checksumma
		dec b

		; ***	Läs Datafält
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	

        ld hl, PktData
RecvDataLoop:
            ld a, b   			 ; A/B = Loopräknare för resten av paketet (exkl. MARK & LEN)
            cp 1            ; Sista byten är Checksumman
            jr z, VerifyCheck
            
            push bc
            push hl
            call InChar
            pop hl
            pop bc
            
            ld (hl), a
            add a, c
            ld c, a         ; Uppdatera checksumma
            inc hl
            dec b
            jr RecvDataLoop

VerifyCheck:
            ; 6. Kontrollera checksumma
            call InChar   			  ; Läs inkommande checksumma
            ld d, a        			 ; D = Mottagen checksumma

            ; Beräkna Kermit-stil checksumma: ((c + (c & 0xC0)/0x40) & 0x3F) + 0x20
            ld a, c
            and 0xC0
            rlca
            rlca
            add a, c
            and 0x3F
            add a, CHAR_OFFSET ; A = Beräknad kodad checksumma

            cp d
            jp nz, BadCheck


            ; Analysera pakattyp
            ld a, (PktType)
            cp 'S'          ; Send-Init packet?
            jr z, HandleS
            cp 'F'          ; File Header packet?
            jr z, HandleF
            cp 'D'          ; Data packet?
            jr z, HandleD
            cp 'Z'          ; End of File (EOF)?
            jr z, HandleZ
            cp 'B'          ; Break Transmission (EOT)?
            jr z, HandleB
            jp MainLoop

            or a            ; Clear Carry = OK paket checksum
            ret

HandleS:
            ; Svara med ACK på Send-Init
            call KermitACK
            jp MainLoop

HandleF:
            ; Här kan du extrahera filnamn om du har ett filsystem (t.ex. CP/M)
            call KermitACK
            jp MainLoop

HandleD:
            ; Skriv ut eller lagra rådata (avkodad)
            ; call ProcessData
            call KermitACK
            jp MainLoop

HandleZ:
            call KermitACK
            jp MainLoop

HandleB:
            call KermitACK
            ; Överföring klar!
            ; halt

            scf             ; Set Carry = Fel checksumma paket
            ret



Error:
            ; Vid fel, skicka NAK för att begära omsändning
            call KermitNAK
            jp MainLoop



; sparaC:
; 		ld  	A,E
; 		ld   	(HL),A
; 		inc  	HL
; 		jr  	nextC

; 		; ld		E,EOT_FOUND			;eot found (end of transmission)
; startcap:
; 		;***	starta lagring av paket
; 		ld 		A,($A804)
; 		inc   	A  			; flagga Z ej aktiv (NZ)
; 		ld  	($A804),A
; 		jr 		sparaC

; endcap:	
; 		;***	stoppa lagring av paket
; 		ld 		A,($A804)
; 		cp  	1
; 		jr  	z,endcap2
; 		jr 	 	nextC

; endcap2:		
; 		inc   	A  			; flagga Z ej aktiv (NZ)
; 		ld  	($A804),A

; 		CALL 	InitBuffers			;Nollställ in/ut buffers, SIO A. INTERRUPT SYSTEM
; 		call 	SIO_A_TXRX_INTon
; 		call 	SIO_A_RTS_ON

; 		ld  	IY,ACKstring
; 		call 	WriteLine





BadCheck:
		call 	restoreSIO_0IO 			; get normal keyboard/screen function
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"Checksummafel för wermit inträffade!",00
		ret


		; ***	VerifyCheck
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	

.n_tecken:
; 		inc  	IY
; 		ld  	A,(IY)
; 		out		(SIO_A_D),A			; skicka tecken till SIO A (utan avbrott)
; 		call	TX_EMP

; 		ld  	A,(IY)
; 		cp 		CR					; vagnretur 0Dh ?
		; ; jr 	    NZ,.n_tecken
		; jr 	    .n_tecken


		; halt 
		; halt 
		; halt 


;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;		Avbrottshantering vid  KERMIT
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

recMinicomTecken:
				;***	läs tecken från minicom till dess [01] ^A
		out  	($70),A					; start pulseview
		ld		A,WR1								;skriv i SIO WR0 cmd4 och välj WR1 
		out		(SIO_A_C),A
		ld		a,_WAIT_READY_EN|_WAIT_READY_R_T	;aktivera WAIT
		out		(SIO_A_C),A						;'buffer overrun' blir special RX condition
		; ld		A,_EN_INT_Nxt_Rx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
		; out		(SIO_A_C),A
		; ; ld		A,_Rx_INT_First_Char			;wait active, interrupt on first RX character
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		; out		(SIO_A_C),A					;buffer overrun is a spec RX condition

		ld 		(XBAddr),HL						; spara aktuell block adress värde HL i XBAddr

		in		A,(SIO_A_D)			;läs in aktuell byte till A
		ld    	E,A

		reti



;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*



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

doKERMITentry:		; ***  Kermit initiering hoppar hit

		ld		(SIO_Int_Read_Vec),HL		;STORE READ VECTOR

		ld 		A,_Reset_STAT_INT|_Reset_STAT_INT	
		out		(SIO_A_C),A
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
		ld		A,_EN_INT_Nxt_Rx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;wait active, interrupt on first RX character
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

		ld		A,WR1								;write into WR0 cmd4 and select WR1 
		out		(SIO_A_C),A
		ld		a,_WAIT_READY_EN|_WAIT_READY_R_T	;wait active, 
		out		(SIO_A_C),A						;buffer overrun is a spec RX condition

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
; 		kolla status på SIO och läs in data när data finns.  (pulling)	
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
 InChar:
		in a, (SIO_A_C)
		bit 0, a        ; Finns nytt tecken?
		jr z, InChar
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
		call 	InChar				;läs nästa byte till A (blocknummer)
		cp		C					;kolla om rätt block nr
		jp		nz,Er02_			; wrong block number (09)

		;kolla inverterat block number
		ld		A,C					;förväntat blocknummer till A
		CPL							;invertera	
		ld		E,A					;förväntat inverterat blocknummer till A

checkComplBlockNum:		
		call 	InChar			;läs nästa byte till A (inverterat blocknummer)
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
		call 	InChar				; läs in byte till A om ny byte finns
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

		call 	InChar
		ld 		D,A
		call 	InChar
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



