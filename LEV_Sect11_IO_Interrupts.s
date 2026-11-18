
		INCLUDE "Z80_Params_.inc"

			
		Section InOutInterrupt


		GLOBAL 	InitBuffers,ReadChar,WriteChar, WriteLine, WriteLine_CRLF, ReadLine, CRLF, puts_crlf,cleanInBuffer,cleanOutBuffer
		GLOBAL	S_head_tail, inBufferEnd, inBuffer, writeSTRBelow, writeSTRBelow_CRLF
		GLOBAL  BOOTclear_p_C_read
		GLOBAL  addStrBelowToPrintQueue,addStrBelowToPrintQueue_IY, waitForKey,RetInpStatus
		GLOBAL	PIO_Init,CTC_Init,SIO_Init,InitInterruptVectors,CTC1_INT_OFF,initSIOBInterrupt
		GLOBAL 	SIO_0INT,InitSIO_0Ports,ReadUSBHandler, purgeRXB, waitForFinishedPrintout, purgeRXA, purgeTXA, TX_NAK,TX_ACK,TX_C,TX_X, TX_EMP,P_TX_EMP
		GLOBAL 	SIO_B_EI,SIO_B_RX_INTon,SIO_B_TXRX_INToff,SIO_B_EI,SIO_B_DI
		GLOBAL 	SIO_A_EI,SIO_A_TXRX_INTon,SIO_A_TXRX_INToff,SIO_A_TX_INTon,SIO_A_RX_INTon,SIO_A_RTS_OFF,SIO_A_RTS_ON,SIO_A_DI

			;ARBITRARY SIO_0 PORT ADDRESSES
								; INTERRUPT VECTOR
			;READ   CHARACTER
		;*************************************
		; ROUTINE: ReadLine
		; PURPOSE: Read a line up to CR and store in (HL)
		; ENTRY: HL = POINTER
		;EXIT: HL = POINTER 
		;      A = length of input string (Textbuf)
		;REGISTERS USED: AF.DE.HL
		;***************************************

waitForKey:
		ld 		A,FF
		ld 		(inbufferDeactivate),A

		halt		; wait for key interrupt

		ld 		A,00
		ld 		(inbufferDeactivate),A

		ret


ReadLine:

		; ***	Vid inläsning av ny rad, börja med att nollställa pekare till (inBufferPtr)
		; ***	 och nollställ flagga för nytt tecken (inBufferVal)  (HL pekar på Textbuf)

		ld		(inBufferPtr), HL		; lagra  pekaradress till 'Textbuf'
		xor 	A						; nollställ A
		ld		(inBufferOffset), A		; lagra  offset för pekaradress '0'
		ld 		(inBufferVal),A				; nollställ flagga för nytt tecken (inBufferVal)	
		ld 		(inBufCount),a			; nollställ buffetlängd
		ld 		IX,ESCsequence
		ld 		(IX),A

Read_LOOP:
		
		; ***	Kolla om nytt tecken finns
		ld 	 	HL,inBufferVal
		xor 	A					; A=0
		cp 		(HL)	
		jr 		Z,Read_LOOP				; vänta på nytt tecken

		; *** 	nytt tecken finns i inbuffer, hämta det och skriv ut det på skärmen	
		; *** 	A är 0 och HL pekar på 'inBufferVal'
		ld  	(HL),A 					; nollställ flagga för nytt tecken... (A=0)

		; cp 		BS
		; jr		Z,Read_LOOP				;återvänd ny loop om inte backspace

		ld 		A,(IY)					; hämta senaste tecken (IY pekar på detta)
		push 	AF
		call 	WriteChar
		POP 	AF

		cp		CR					;vagnretur ?
		jr		NZ,Read_LOOP				;återvänd ny loop om inte vagnretur
		; ***	sätt in '0' efter CR kan vara mitt i strängen om backsteg anänts
		xor		A
		ld 		(IY+1),A			; IY pekar på sist inmatade tecken, vilket kan vara CR 	
		ld 		(IY+2),A			; IY pekar på sist inmatade tecken, vilket kan vara CR 	

		call	CRNL
		ret
		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+




	;defb 	"text!!!",SOH,"öasdöa",STX,"poicpp",ETX,"wqembnrqeb",EOT,ENQ
	;					1			2			  3				  4	  5	
	; 					byte		bytes						bytes
;##############################################################
;	Skriv ut en sträng som slutar med '0' och finns 
;	direkt efter anropet till denna subrutin (call writeSTRBelow)
; 	använder  AF, C
;##############################################################
writeSTRBelow:
        ex      (sp),iy                 ; iy = adress för strängen som skrivs ut.
		call	WriteLine
        inc     iy                      ; pekar förbi slutet ('0') i strängen
        ex      (sp),iy
        ret

writeSTRBelow_CRLF:
		ex		(sp),iy                 ; iy = adress för strängen som skrivs ut.
		call	WriteLine_CRLF
		inc		iy                      ; pekar förbi slutet ('0') i strängen
        ex      (sp),iy
		ret

;##############################################################
;	Läs in en sträng från subrutinanropet. IX pekar på innehåll
;	Formatera en utskriftsträng i Textbuf av text och angivna 
; 	parametrar omvandlade till ascii text. parametrar finns i 
; 	stack och (paramStack) är pekare.
;##############################################################
addStrBelowToPrintQueue_IY:		; lägg till ny info till utskriftssträng och starta parameter stack från topp.
		ld 		IY,(SP_valueP)			; välj pekare till början av angivna parmetrar i stack

addStrBelowToPrintQueue:						; lägg till ny info till utskriftssträng...
		;  		IY uppdateras på nästa parameter i stacken
		; p_2H: värde i paramStack konverteras till 1byte text (2 tecken)
		; p_4H: värde i paramStack konverteras till 2byte text (4 tecken)
		; P_1C: värde i paramStack konverteras till 1byte text (1 tecken) 
		; [EOT: värde i paramStack konverteras till 4byte text (8 tecken) ] för senare bruk


        ex      (sp),IX                 ; IX = adress för strängen i anropet.
		inc 	IX					; hoppa förbi inledande 0'a (filstorlek)

strloop:
		ld 		A,(IX)
		or 		a						; kolla om strängavslut (=0)
		jr 		z,endproc

		cp 		p_H2					; parameter för enkel byte (2 tecken) ?
		jr 		nz,chkpD5
		call 	convParamString			; nästa parameter i stack -> DE.
		call 	Bin2Hex8				; bin i DE -> fyll på med tecken i (HL)
		jr 		p_nextchar
		
chkpD5:
		cp 		p_D5					; parameter för enkel byte (2 tecken) ?
		jr 		nz,chkpH4
		call 	convParamString			; nästa parameter i stack -> DE.
		call 	Num2Dec					; bin i DE -> fyll på med tecken i (HL)
		jr 		p_nextchar

chkpH4:
		cp 		p_H4					; parameter för enkel byte (4 tecken) ?
		jr 		nz,chkchar
		call 	convParamString			; nästa parameter i stack -> DE.
		call 	Bin2Hex16					; bin i DE -> fyll på med tecken i (HL)
		jr 		p_nextchar

chkchar:
		cp 		p_C1					; parameter för enkelt tecken (ascii) (1 tecken) ?
		jr 		nz,.noEvalue

		call 	convParamString			; nästa parameter i stack -> DE.
		ld  	A,E 					; hämta värde från ack E.
 		call 	isChar_A				; kollar om (HL) är skrivbart tecken ? Carry->  A = '.'

.noEvalue:					;  skicka aktuell char (ix) till sträng för utskrift (ingen omvandling)
		ld  	(HL),A				; fyll på med tecken från (IX)=A  i (HL)
		inc 	HL
		ld  	(HL),0

p_nextchar:
		inc 	IX				; peka på nästa sträng.
		jr 		strloop
endproc:
		inc  	IX
		ex 		(SP),IX			; ta tillbaka gamla värdet på SP 

		ret

;##############################################################

convParamString:
		ld 		IY,(paramStack)			; pekare för angivna parmetrar i stack
		ld 		D,(IY-1)
		ld 		E,(IY-2)				; DE innehåller aktuell parameter från stacken
		dec 	iy
		dec 	iy
		ld 		(paramStack),IY			; visar på nästa parameter
								; gör en sträng med 2 tecken
		ret
;##############################################################
;##############################################################
; Skriv ut CR och LF. 
; Använder  AF, C
;##############################################################
puts_crlf:
        call    writeSTRBelow
        defb    '\0\r\n\0'
        ret


;##############################################################
; skriv ut strängen som pekas ut av IY. strängslut ='0'
; använder  AF, C
;##############################################################
WriteLine:
		; ld 		b,(iy)		; get length
		inc		iy			; First pos point to str length, Dont check length, skip first len byte 22.05.01
nxtchr:
		ld 		a,(iy)
		or		A			; = 0 ??
		ret 	z
		push	hl
		call	WriteChar
		pop 	hl
		inc		iy
		jr 		nxtchr

		ret					; return on maxlength
			; WriteLine from address (iy) (until char = 00)and add CRLF 
WriteLine_CRLF:
		call	WriteLine
			; Entry for excl. output CRNL
CRNL:
CRLF:
		call	puts_crlf
		ret

			;Write character
WriteChar:
		push 	BC
		push 	DE
		push 	HL				; save the reg  for hexdump...
		PUSH	AF				;SAVE CHARACTER TO OUTPUT
			;wait for output buffer not full, then store next character
WaitOutBuff:
		call	GetOutStatus			; get output status. Carry=1 if buffer full, 0 if not
		jr		C,WaitOutBuff			; wait if output buffer is full
		di                     			; disable interrupts while looking at
										; buffer, interrupt status
		ld		HL,OutBufCount
		inc		(HL)					; increase output buffer count by 1
		ld		HL, (outTailAdr)		; point to next empty byte in buffer
		pop		AF						; get character
		ld		(HL),A					; store character at tail of buffer
		call	incOutPointer			; move tail pointer up 1
		ld		(outTailAdr),HL
		ld		A,(OutINTExpect)		; test output interrupt expected flag
		or		A
		call	Z,CharToSIO_0			; output character immediately if
										; output interrupt not expected
		pop 	HL		
		pop 	DE	
		pop 	BC			
		ei						;reenable interrupts
		ret
			;output status (carry=1 if buffer is full)
GetOutStatus:
		ld		A, (OutBufCount)		; get current output buffer count
		cp		outBufferSize			; compare to maximum
		ccf						; complement carry
		ret						; carry = 1 if buffer full, 0 if not
			; INITIALIZE SIO_0, Interrupt system
InitBuffers:
			; initialize buffer counters and pointers.
		sub		A					; zero A
		ld		(OutINTExpect),A	; indicate no output interruptS
		ld		(inBufCount),A		; buffer counters = 0
		ld		(OutBufCount),A
		; call	cleanInBuffer
		call	cleanOutBuffer
		call 	InitInterruptVectors		; init interrupt vectors
		ret

; cleanInBuffer:
; 		sub		A					; A=0
; 		ld		(inBufCount),A		; läsbuffert  = 0
; 		ld 		(inbufferDeactivate),A  ; Nollställ flagga för deaktivering av läsbuffert (inaktivering > 0)...
; 		ld		HL,inBuffer			; HL pekar på läsbuffert start
; 		ld		(inHeadAdr),HL		; 
; 		ld		(inTailAdr),HL		; läsbuffert start,slut = början på buffert
; 		ret

cleanOutBuffer:
		sub		A					; zero A
		ld		(OutBufCount),A
		ld		HL,outBuffer
		ld		(outHeadAdr),HL
		ld		(outTailAdr),HL
		ret

waitForFinishedPrintout:
		;  wait for print to finish
.waitmore:
		ld		A, (OutBufCount)	;get output buffer counter
		or 		A 					;=0 ?
		jr 		nz,.waitmore
		ret

;******************************************************************************
InitInterruptVectors:
;******************************************************************************
			;INITIALIZE INTERRUPT VECTORS (SIO_0)
			; initialize . interrupt flag
		di
		ld 		HL,SIO_PIO_CTC_stack
		ld		A,H       		;mest sig byte i adress för avbrottsvektorer   (F800 >> 8 = F8)
		ld		I,A             ;SET INTERRUPT VECTOR IN zao
		im		2               ; INTERRUPT MODE 2 - VECTORS IN TABLE
		ld		HL,ReadINTHandler      	 ; ON INTERRUPT PAGE
		ld		(SIO_Int_Read_Vec),HL		;STORE READ VECTOR
		ld		HL,WriteINTHandler
		ld		(SIO_Int_WR_Vec),HL		;STORE WRITE VECTOR
		ld		HL,ExternINTHandler
		ld		(SIO_Int_EXT_Vec),HL		;STORE EXTERNAL/STATUS VECTOR
		ld		HL,SpecINTHandler
		ld		(SIO_Int_Spec_Vec),HL		;STORE SPECIAL RECEIVE VECTOR
		ld		HL,ReadUSBHandler      	 ; ON INTERRUPT PAGE
		ld		(SIO_USB_Read_Vec),HL		;STORE READ VECTOR

		ld 		HL,Write_USB_Handler
		ld		(SIO_USB_WR_Vec),HL		;STORE READ VECTOR
		ld 		HL,Extern_B_USB_Handler
		ld 		(SIO_USB_EXT_Vec),HL
		ld 		HL,SpecINT_B_USB_Handler
		ld 		(SIO_USB_Spec_Vec),HL
		
				; INT Vectors  for the CTC 
		ld		HL,CTC_CH0_Interrupt_Handler
		ld		(CTC_CH0_I_Vector),HL		;STORE CTC channel 0 VECTOR
		ld		HL,CTC_CH1_Interrupt_Handler
		ld		(CTC_CH1_I_Vector),HL		;STORE CTC channel 1 VECTOR
		ld		HL,CTC_CH2_Interrupt_Handler
		ld		(CTC_CH2_I_Vector),HL		;STORE CTC channel 2 VECTOR
		ld		HL,CTC_CH3_Interrupt_Handler
		ld		(CTC_CH3_I_Vector),HL		;STORE CTC channel 3 VECTOR
		ld 		A,$7F
		out 	(gpio_out),A

		ret
		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


SIO_Init:		
		;INITIALIZE I/O PORTS
		ld      HL,SIO_0INT		;BASE ADDRESS OF INITIALIZATION ARRAY
		call    InitSIO_0Ports			; INITIALIZE SIO_0
		ei						; ENABLE INTERRUPTS
		ret
		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+




;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;******************************************************************************
				;SIO kanal A: Avbrottshanterare för teckeninmatning 
;******************************************************************************
;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-





ReadINTHandler:

		push	AF				;SAVE REGISTERS
		push	BC
		push	DE
		push   	HL
		in		A,(SIO_A_D)			; läs data från  SIO_A
		ld		C,A					; save data in register c

		; 		kolla om escape sekvens
		cp 		ESC
		jp 		Z,FixESCSEQ_Init		; hantera escapesekvens vid första $1B tecknet
		; ***	----------------------------------------------------

		ld 		HL,ESCsequence
		xor  	A						; A=0	
		cp 		(HL)					; kolla om (ESCsequence) är > 0 - pågående ESCsekvens
		jp 		NZ,FixESCSEQ			; om ESC flagga: fortsätt läsa in ESC sekvens
		; ***	----------------------------------------------------

		; ld 		A,(inbufferDeactivate)
		; cp 		$00 					; = 0 		
		; jr 		NZ,exitRHandler
		; ***	----------------------------------------------------

checkBS:		
		; ***	öka markör pos offset med 1 om inte offset = inBufCount
		ld 		A,BS
		cp 		C 					; kolla om 'backspace'
		jp 		NZ,kollaCR
		; ***	BS fungerar även om offset är 0. Offset är oförändrat, och inBufCount minskar med 1
		; ***	radera tecken vid markör, flytta tecknen t o m strängslut ett steg vänster

		ld 		A,(inBufferOffset)
		ld 		HL,inBufCount
		cp 		(HL)					; jämför buffertlängd och offset om 0; avbryt.
		jp 		P,hanteraslut 			; kan ej ta bort om offset = 0

		xor 	A 						; nollställ  A för att visa BS
		call 	skifta_BS_DEL 			; skifta tecken i insträng

		
		call 	writeSTRBelow
		defb   	0,"\e[D\e[P",0			; radera tecken vid markör och flytta vänster
		jp 		exitRHandler		
		; ***	----------------------------------------------------

kollaCR:
		ld 		HL,inBufferVal
		inc 	(HL)				; signalera att nytt tecken finns ( >0 )	
		; ***	----------------------------------------------------


		; ***	kolla om vagnretur
		ld 		A,CR
		cp 		C 					; kolla om vagnretur
		jr 		NZ,kollabuflen
		; ***	skriv in CR,0 i slutet av inskriven rad (inBufferPtr)
		ld 		IY,(inBufferPtr)
		inc 	IY
		ld 		(IY),A			; skriv in avslutande 'CR'
		xor 	A
		ld 		(IY+1),A				; skriv in avslutande '0'
		jp 		exitRHandler
		; ***	----------------------------------------------------
kollabuflen:
		ld		HL,inBufCount			; Är inbuffert full?
		ld		A, (HL)
		cp		bufferSize
		jp		NC,exitRHandler			; retur (NC) om ingen plats finns kvar
		inc		(HL)				; öka värde på bufferstorlek 
		; ***	----------------------------------------------------

		; ***	för in aktuellt tecken i sträng m h t markörpos, DEL, BS, ---
		ld		HL, (inBufferPtr)		; HL pekar på sista tecknet (strängslut)
		inc 	HL
		push 	HL
		pop 	IY				; IY pekar på sträng slut
		ld  	A, (inBufferOffset)	; räknare för att skifta n st bytes till höger
		ld 		B,A
		; ***  om offset är 0 - skriv till strängslut.
.nstskift:
		xor 	A		
		or  	B
		jr  	Z,.skiftklar		; om allt är skiftat - fortsätt

		dec 	B
		ld 		A,(IY-1)
		ld 		(IY),A			; skifta byte till höger
		dec 	IY					; skifta vänster
		jr 		.nstskift

.skiftklar:
		ld		(IY),C				; inläst värde (A=C) lagras i (inBufferPtr - offset) 
		ld		(inBufferPtr), HL		; lagra ny pekaradress för strängslut (IY) pekar på insatt tecken
		; inc 	HL
		; xor 	A
		; ld 	 	(HL),A				; Nollställ tecknet efter strängslut = '0'
		jr	 	exitRHandler
		; ***	----------------------------------------------------


exitRHandler:
		; ld 		A,00
		; ld 		(CTCdelayFlag),A 	; reset timeout flag indicate interrupt by SIOA read.

		pop		HL				;restore registers
		pop		DE
		pop		BC
		pop		AF
		ei						;reenable interrupts
		reti
		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
		
skifta_BS_DEL:
		; ***	Gemensam rutin för Backspace (BS) eller DEL.
		; ***	Om A är 0 avses BS, annars DEL
			; ***	rutinstart ....
		ld 		(TempVar1),A 			; spara A (är 0 avses BS, annars DEL)

		ld 		A,(inBufferOffset)
		ld 		B,A					; räknare = offset
		; dec 	A

		ld  	HL,(inBufferPtr)
		dec 	HL
		ld  	(inBufferPtr),HL	; spara inBufferPtr minskat med 1

		push 	HL
		ld 		HL,TempVar1			; HL pekar på värde på A vid anrop till 'skifta_BS_DEL'
		bit 	0,(HL)				; kolla om A är 0(BS) eller ej 0(DEL)
							; Z flaggan ställd om (HL)0=0		
		pop 	HL					; återställ HL innan kontroll av Z
		
		jr 		Z,.hoppaBS
		; *** 	hoppa över dessa rader om BS, men kör om DEL	
		dec 	A
		ld 		(inBufferOffset),A		; spara offset minskat med 1
.hoppaBS:
		inc 	HL 					;justera HL	
		ld 		D,0
		ld 		E,A
		scf
		ccf
		sbc 	HL,DE 				; HL = HL - offset

		push 	HL
		pop 	IY				; sätt IY = inBufferPtr - offset
.nskift:
		ld	 	A,(IY+1)
		ld 		(IY),A
		inc 	IY
		djnz 	.nskift

		ld 		HL,inBufCount
		dec 	(HL)					; minska bufferlängd med 1
		; ***	rutinslut  ....
		ret		


;##############################################################
;	Ta hand om ESC sekvens...
; 	
; 	använder  AF, C
;##############################################################
FixESCSEQ_Init:

		ld 		DE,S1x+2			; använd Heap area 'S1x'
		ld 		(S1x),DE
		ld 		A,$10		; indikera start inläsning av n st tecken i ESC sekvens. >0
							; låga halvan av 'ESCsequence' används som räknare
		ld 		IX,ESCsequence
		ld 		(IX),A

FixESCSEQ:
		; lagra escape sekvens i T_BUFFER; 
		ld 		DE,(S1x)
		ld 		A,C
		ld 		(DE),A			; spara 'ESC'
		inc 	DE				; plats för nästa tecken i ESC sekvens

		ld 		(S1x),DE
		inc 	(IX)			; öka antalet tecken i ESC sekvens med 1


		; om antal tecken är 3, 4, 5, 5, 7 st
		; INS			   1B 5B 32 7E 
		ld 		A,(IX)
		and 	$0F				; behåll lägre halvan		
		cp 		3
		jp 		Z,hantera3t
		cp 		4
		jp 		Z,hantera4t
		cp 		5
		jp 		Z,hantera5t
		cp 		6
		jp 		Z,hantera6t
		cp 		7
		jp 		Z,hantera7t
		
		jp 		exitRHandler	; fortsätt läsa tecken

hantera3t:
		; ***	3'e tecknet i sekvens är mellan $41 och $53
		ld	 	A,C			; ladda A med värde av sista tecknet (påverkar ej CCR)
		sub 	$41
		jp 		M,exitRHandler 		; mindre än $41
		cp 		$54						
		jp 		P,exitRHandler		; större än $53
			; up, down, left, rigt, F1, F2, F3, F4 mellan $41 och $53 -> OK

		ld 		HL,inBufferOffset		; hämta offset för markörposition 
		ld 		A,(inBufCount)		; A antal tecken i buffert
		ld  	B,A					; spara buffertlängd -B

		ld 		A,C				; A är 3'e tecknet
		CP 		$44				; kolla vänster pil
		jr 		NZ,.kh
	
		; ***	öka markör pos offset med 1 om inte offset = inBufCount
		ld 		A,B 				; A är buffertlängd
		ld 		B,(HL)				; B är offset
		sub 	A,B
		jp 		Z,hanteraslut		;inBufCount = inBufferOffset; vänster ändläge. (Sign minus)
		inc 	(HL)					; öka offset med 1
		call 	writeSTRBelow
		defb   	0,"\e[D",0			; flytta markör vänster

		jp 		hanteraslut			; mellan $41 och $53 -> OK
.kh:
		CP 		$43					; kolla höger pil
		jr 		NZ,.ku

		xor 	A
		cp 		(HL)				; är offset 0 ?
		jr 		Z,hanteraslut		;inBufCount = inBufferOffset; vänster ändläge. (Sign minus)
		dec 	(HL)					; minska offset med 1

		call 	writeSTRBelow
		defb   	0,"\e[C",0			; flytta markör höger
.ku:	jp 		hanteraslut			; mellan $41 och $53 -> OK

hantera4t:
		dec 	de
		dec 	de
		ld 		A,(DE)			; ladda A med värde av 3'e tecknet i sekvens (påverkar ej CCR)		

		CP 		$32				; kolla 'INS'
		jp 		NZ,checkDEL
		; ***	ändra värde på 'InsertMode' med CPL
		; ld 		A,(InsertMode)
		; cpl
		; ld 		(InsertMode),A
		; or 		a						; ställ Z om A=0
		; jr 		NZ,.INS_1
		; call 	writeSTRBelow
		; defb   	0,"\e[4l",0				; cursor överskrivning!
		; 		; call 	writeSTRBelow
		; 		; defb   	0,"\e[2 q",0				; markör useende!
		; jp 		hanteraslut
		
; .INS_1	call 	writeSTRBelow
; 		defb   	0,"\e[4h",0				; cursor infoga !
; 				; call 	writeSTRBelow
; 				; defb   	0,"\e[6 q",0				; markör useende!
		jp 		hanteraslut

checkDEL:
		CP 		$33				; kolla 'DEL'
		jp 		NZ,exitRHandler
		; ***	DEL fungerar om offset > 0. Offset och inBufCount minskar med 1
		; ***	radera tecken vid markör, flytta tecken till höger ett steg vänster

		ld 		A,(inBufferOffset)
		or  	A
		jp 		Z,hanteraslut 			; kan ej ta bort om offset = 0

		ld  	A,1 					; ställ bit 0 för att visa DEL (kollas med BIT)
		call 	skifta_BS_DEL 			; skifta tecken i insträng

		call 	writeSTRBelow
		defb   	0,"\e[P",0			; radera tecken vid markör 

		jp 		hanteraslut

hantera5t:
		; ***	4'e tecknet i sekvens är $7E, alt 5'e tecknet i sekvens är $7E
		ld	 	A,C			; ladda A med värde av sista tecknet (påverkar ej CCR)
		cp 		$7E
		jp 		NZ,exitRHandler		; om inte $7E -> fortsätt läsa in sekvens (fler än 4 tecken)

		jp 		hanteraslut			; värde $7E -> OK

hantera6t:
hantera7t:
		; ***	4'e tecknet av 6 mottagna i sekvens är $3B '['; alt 5'e tecknet av 7 mottagna i sekvens är $3B '['
		dec 	de
		dec 	de
		dec 	de
		ld 		A,(DE)			; ladda A med värde av 4'e tecknet i sekvens (påverkar ej CCR)		
		cp 		$3B
		jP 		NZ,exitRHandler		; om inte $7E -> fortsätt läsa in sekvens (fler än 4 tecken)

		jp 		hanteraslut			; 4'e värde $3B -> OK

hanteraslut:
		xor 	A				; nollställ A
		ld 		(IX),A
		jp 		exitRHandler	; fortsätt läsa tecken nollställ flagga för ESC sekvens.



;******************************************************************************
			;SIO kanal A: avbrottshanterare för utskrift 
;******************************************************************************
WriteINTHandler:
		push	AF					;save registers
		push	BC
		push	DE
		push	HL
		ld		A, (OutBufCount)	;hämta räknare för utbuffert
		or		A					;är buffert tom ?
		jr		Z,nodata			;hoppa till 'nodata' om buffert är tom
		call	CharToSIO_0			;skriv ut tecken till SIO
		jr		wrdone
			; if an output interrupt occurs when no data is available.
			; we must disable output interrupts to avoid an endless loop.
			; when the next character is ready, it must be sent immediately
			; since no interrupt will occur. this state in which an output
			; interrupt has occurred but has not been serviced is indicated
			; by clearing ole (output interrupt expected flag).
nodata:
		sub		A
		ld		(OutINTExpect),a				;00 not expect an interrupt
		out		(SIO_A_C),a			;select register 0
		ld		a,00101000b			;reset transmitter interrupt
		out		(SIO_A_C),a
wrdone:
		ld 		A,00
		ld 		(CTCdelayFlag),A 	; reset timeout flag indicate interrupt by SIOA read.
		pop		HL					;restore registers
		pop		DE
		pop		BC
		pop		AF
		ei
		reti
;******************************************************************************



;******************************************************************************
		; *** 	avbrottshanterare för läsning SD/USB via HC376S (SIO B)
;******************************************************************************
ReadUSBHandler:
		in  	A,(sio_bd)		  		;read char from SIO B
		ld 		E,A
	; ifd 	GPIODEBUG	
	;	ld a,1
	;	out (gpio_out),A
	;	ld a,0
	;	out (gpio_out),A
	; endif
	
		ld a,e
		; call 	purgeRXB

		; in  	A,(CH1)
		; ld 		(TempVar8),A
		; in  	A,(CH0)
		; ld 		(TempVar7),A
; 		cp 		USB_INT_CONNECT
; 		jr  	NZ,.p2
; 		call 	writeSTRBelow_CRLF
; 		db		0,">USB_INT_CONNECT",0,0
; 		jr 		.p3
; .p2:
; 		cp 		USB_INT_DISCONNECT
; 		jr 		NZ,.p3
; 		call 	writeSTRBelow_CRLF
; 		db		0,">USB_INT_DISCONNECT",0,0
.p3:
		

		ei
		reti

;********************************************************************************************     

;******************************************************************************
		; *** 	avbrottshanterare för SD/USB skriv via HC376S (SIO B)
;******************************************************************************
Write_USB_Handler:

	; ifd 	GPIODEBUG	
	; 	ld a,20
	; 	out (gpio_out),A
	; 	ld a,0
	; 	out (gpio_out),A
	; endif

		ld a,e
		ei
		reti
			;external/status changed interrupt handler
Extern_B_USB_Handler:
	ifd 	GPIODEBUG
		ld a,21
		out (gpio_out),A
		ld a,0
		out (gpio_out),A
	endif
		ld a,e
		ei							; dcd or cts line changed state. or a
		reti						; break was detected
									; service here if necessary
			;special receive error interrupt
SpecINT_B_USB_Handler:
	; ifd 	GPIODEBUG
		ld a,30
		out (gpio_out),A
		ld a,0
		out (gpio_out),A
	; endif
	
		ld a,e
		ei							;framing error or overrun error occurred
		reti						; service here if necessary

;********************************************************************************************     

; SIO_A_EI:
; 		;enable SIO_0 channel A RX

; 		; ld		a,003h			;write into WR0: select WR3
; 		; out		(SIO_A_C),A
; 		; ld		a,_Rx_Enable|_RX_8_bits|_Auto_Enable 			;RX 8bit, auto enable on, RX on
; 		; out		(SIO_A_C),A	Channel A RX active

; 		ld		a,WR5			;write into WR0: select WR5
; 		out		(SIO_A_C),A
; 		ld		a,_RTS_Enable|_Tx_Enable|_Tx_8bits_char			
; 		out		(SIO_A_C),A		;Channel A RX active

; 		; ld		a,001h			;write into WR0: select WR3
; 		; out		(SIO_A_C),A
; 		; ld		a,_Tx_INT_EN|_Int_All_Rx_Char_NP		
; 		; out		(SIO_A_C),A	Channel A RX active



; 		RET
	
; SIO_A_DI:
; 		;disable SIO_0 channel A RX
; 		; ld		a,WR3			;write into WR0: select WR3
; 		; out		(SIO_A_C),A
; 		; ld		a,_RX_8_bits|_Rx_Disable			;RX 8bit, auto enable off, RX off
; 		; out		(SIO_A_C),A
; 		ld		a,WR5			;write into WR0: select WR3
; 		out		(SIO_A_C),A
; 		ld		a,_Tx_8bits_char			
; 		out		(SIO_A_C),A		;Channel A RX active


; 		;Channel A RX inactive
; 		ret


SIO_A_TXRX_INTon:
		;enable SIO_0 channel A RX
		ld		A,WR1							;write into WR0: select WR1
		out		(SIO_A_C),A
		ld		A,_Tx_INT_EN|_Int_All_Rx_Char_NP		 			;RX and TX interrupt on
		out		(SIO_A_C),A						; Channel A RX active
		RET



; SIO_A_TXRX_INToff:
; 		;enable SIO_0 channel A RX
; 		ld		A,WR1			;write into WR0: select WR1
; 		out		(SIO_A_C),A
; 		ld		A,00h			;RX and TX interrupt off
; 		out		(SIO_A_C),A		;	Channel A RX 
; 		RET

; SIO_A_TX_INTon:
; 		;enable SIO_0 channel A RX
; 		ld		A,WR1							;write into WR0: select WR1
; 		out		(SIO_A_C),A
; 		ld		A,_Tx_INT_EN		 			;TX interrupt on
; 		out		(SIO_A_C),A						; Channel A TX active
; 		RET

; SIO_A_RX_INTon:
; 		;enable SIO_0 channel A RX
; 		ld		A,WR1							;write into WR0: select WR1
; 		out		(SIO_A_C),A
; 		ld		A,_Int_All_Rx_Char_NP		 	;RX interrupt on
; 		out		(SIO_A_C),A						;Channel A RX active
; 		RET

SIO_A_RTS_OFF:
		;signaling the host go or nogo for reception
		ld		a,WR5			;write into WR0: select WR5
		out		(SIO_A_C),A
		ld		a,_Tx_8bits_char|_Tx_Enable 				; 8 bitars teckenlängd (6&5), ingen break (4), TX enable
		ld		a,0E8h			
		out		(SIO_A_C),A 
		ret 
		
		
SIO_A_RTS_ON:
		; signaling the host go or nogo for reception
		ld		a,WR5			;write into WR0: select WR5
		out		(SIO_A_C),A
		; ld		a,_Tx_8bits_char|_Tx_Enable|_RTS_Enable 		;TX 8bit, BREAK off, TX on, RTS active
		ld		a,0EAh					
		out		(SIO_A_C),A 
		ret 
		
	


; SIO_B_RX_INTon:
; 		;enable SIO_0 channel A RX
; 		ld		A,WR1							;write into WR0: select WR1
; 		out		(sio_bc),A
; 		ld 		A,_Int_All_Rx_Char_NP|_Status_Vector  		;RX interrupt on
; 		out		(sio_bc),A						;Channel A RX active
; 		RET


; SIO_B_TXRX_INToff:
; 		;enable SIO_0 channel B RX
; 		ld		A,WR1			; write into WR0: select WR1
; 		out		(sio_bc),A
; 		ld		A,00h			; RX and TX interrupt off
; 		out		(sio_bc),A		; Channel B RX 
; 		RET

; SIO_B_EI:
; 		;enable SIO_0 channel B RX
; 		ld		a,003h			;write into WR0: select WR3
; 		out		(sio_bc),A
; 		ld		a,0C1h			;RX 8bit, auto enable off, RX on
; 		out		(sio_bc),A		; Channel A RX active
; 		RET
	
	
; SIO_B_DI:
; 		;disable SIO_0 channel B RX
; 		ld		a,WR3			;write into WR0: select WR3
; 		out		(sio_bc),A
; 		ld		a,_RX_8_bits|_Rx_Disable			;RX 8bit, auto enable off, RX off
; 		out		(sio_bc),A
; 		;Channel A RX inactive
; 		ret


purgeRXA:
		; flushing the receive buffer
		;check for RX buffer empty
		;modifies A
		sub		a				;clear a, write into WR0: select RR0
		out		(SIO_A_C),A
		in		A,(SIO_A_C)		;read RRx
		bit		0,A
		ret		z				;if any rx char left in rx buffer

		in		A,(SIO_A_D)		;read that char
		jp		purgeRXA		


; purgeTXA:
; 		; flushing the transmit buffer
; 		;check for TX buffer empty
; 		;modifies A
; 		sub		a				;clear a, write into WR0: select RR0
; 		out		(SIO_A_C),A
; 		in		A,(SIO_A_C)		;read RRx
; 		bit		2,A
; 		ret		z				;if any tx char left in tx buffer

; 		in		A,(SIO_A_D)		;read that char
; 		jp		purgeTXA		


purgeRXB:
		; flushing the receive buffer, check for RX(B) buffer empty
		;modifies A
		sub		a				;clear a, write into WR0: select RR0
		out		(sio_bc),A
		in		A,(sio_bc)		;read RRx
		bit		0,A
		ret		z				;if any rx char left in rx buffer

		in		A,(sio_bd)		;read that char
		jp		purgeRXA		

TX_NAK:
		ld 		a,NAK				;send NAK 15h to host
		out		(SIO_A_D),A
		call	TX_EMP
		RET



TX_ACK:
		ld		 A,ACK				;send AK to host
		out		(SIO_A_D),A
		call	TX_EMP
		ret


TX_C:
		ld		 A,'C'				;send 'C' to host
		out		(SIO_A_D),A
		call	TX_EMP
		RET

TX_X:
		ld		 a,'X'				;send 'X' to host
		out		(SIO_A_D),A
		call	TX_EMP
		RET

		;###############################################
		;		skriv ut tecken och vänta på SIO
P_TX_EMP:			
		out		(SIO_A_D),A

TX_EMP:
		; sänder tecken till värddator 
		; kontroll att transmit buffer är tom.
		ld 		a,01				;välj läsregister  RR1
		out		(SIO_A_C),A
		in		A,(SIO_A_C)		; läs sändbuffer TRx, 'all sent'
		bit		0,A				; kontrollera bit 0
		jr		z,TX_EMP
		ret
		


				; return with A=0, Z set
CTC1_INT_OFF:
		ld		A,_Counter|_Rising|_Reset|_CW	
		out		(CH1),A			; reset and turn off interrupt CH1
		xor 	A 				; clear A
		ld 		(CTCdelayFlag),A ; reset timeout flag
		ret							; return with A=0, Z set
;******************************************************************************


			;external/status changed interrupt handler
ExternINTHandler:
		push	AF
		ld		A,00010000b			;reset status interrupt

		out		(SIO_A_C),a
		pop		AF
		ei							; dcd or cts line changed state. or a
		reti						; break was detected
									; service here if necessary
			;special receive error interrupt
SpecINTHandler:
		push	AF
		ld		A,00110000b			;reset receive error interrupt
		out		(SIO_A_C),a
		pop		AF
		ei							;framing error or overrun error occurred
		reti						; service here if necessary

		;*************************************
		; 	routine: chartoSIO_0
		; 	purpose: send character to SIO_0
		; 	entry: none
		;	exit: none
		;	registers used: af.de.hl
		;***************************************
CharToSIO_0:
		ld		HL,(outHeadAdr)
		ld		A, (HL)					;get data from head of output buffer
		out		(SIO_A_D),A			;output data


		call	incOutPointer			; increment head pointer
		ld		(outHeadAdr),HL
		ld		HL,OutBufCount			;decrement output buffer count
		dec		(HL)
		ld		a,0ffh
		ld		(OutINTExpect),a		;expect an output interrupt
		ret

		
		;*************************************
		; routine: incoutpointer
		; purpose: increment pointer into output
		;			buffer with wraparound
		; entry: HL =  pointer
		;exit: HL   =  pointer incremented with wraparound
		;registers used: AF.DE.HL
		;***************************************
incOutPointer:
		inc		HL					; increment pointer
		ld		DE,endOutBuffer			;compare pointer. end of buffer
		ld		A,L
		cp		E
		ret		NZ
		ld		A,H
		cp		D
		ret		NZ
		ld		HL,outBuffer				;if pointer at end of buffer.
		ret							; set it back to base address


		;**************************************
		; routine: initSIO_0ports
		; purpose: initialize i/o ports
		; entry: hl = base address of initialization array
		;exit: data output to ports
		;registers used: af.bc.hl
		;************************************
InitSIO_0Ports:
		;get number of data bytes to send to current port
		;exit if number of bytes is o. indicating terminator
		ld		A,(HL)			;get number of bytes
		or		A				;test for zero (terminator)
		ret		Z				;return if number of bytes = 0
		ld		B,A
		inc		HL				;point to port address (next byte)

			;c = port address
			;hl   base address of output data
		ld		C,(HL)			;get port address
		inc		HL				;point to first data value (next byte)
			;output data and continue to next port
		otir					;send data values to port
		jr      InitSIO_0Ports			;continue to next port entry
		
		;SIO_0 initialization data

		; sio_ac
		; sio_ad
		; sio_bc
		; sio_bd
initSIOBInterrupt:
		ld 		A,WR1
		out 	(sio_bc),A
		ld 		A,_Int_All_Rx_Char_NP|_Status_Vector
		out 	(sio_bc),A					; interrupt on HC376S read
		ret
	
SIO_0INT:
		; Reset channel a
		db	1					;output 1 byte
		db	sio_ac			;to channel a command/status
		db	_Ch_Reset			;select write register 0
								;bits 2.1.0    0 (write register 0)
								;bits 5,4,3 = 011 (channel reset)
								;bits 7,6 = 0 (do not care)
		db	1					;output 1 byte
		db	sio_bc			;to channel a command/status
		db	_Ch_Reset			;select write register 0


		;sET INTERRUPT VECTOR AND ALLOW STATUS TO AFFECT IT
		db	4					;OUTPUT 4 BYTES
		db	SIO_B_C			;DESTINATION IS COMMAND REGISTER B
		db	WR2					;SELECT WRITE REGISTER 2
		db	SIO_Int_Vec&0FFH	;SET INTERRUPT VECTOR FOR SIO_0
		db	WR1					;SELECT WRITE REGISTER 1
		db	_Status_Vector		;TURN ON STATUS AFFECTS VECTOR

		; INITIALIZE CHANNEL A
		db	8					;OUTPUT 8 BYTES
		db	SIO_A_C			;DESTINATION IS COMMAND REGISTER A

		;iNITIALIZE BAUD RATE CONTROL
		db	_Reset_STAT_INT|4	;SELECT WRITE REGISTER 4 & RESET EXTERNAL/STATUS INTERRUPT
		db	_Stop_1_bit|_X16_Clock_mode
								;BIT 0 = 0 (NO PARITY)
								;BIT 1 = 0 (DON'T CARE)
								;BITS 3,2 = 01 (1 1/2 STOP BITS)
								; BITS 5.4 = 00 (DON-'T CARE)
								;BITS 7.6 = 10 (32 TIMES CLOCK)
	
		; INITIALIZE RECEIVE CONTROL
		db	WR3		;SELECT WRITE REGISTER 3
		db	_Rx_Enable|_RX_8_bits|_Auto_Enable  (0xC1)
								;BIT 0 = 1 (RECEIVE ENABLE)
								; BITS 4,3,2,1 = 0 (DON-'T CARE)
								;BIT 5 = 0 (NO AUTO ENABLE)
								;BIT 7.6 = 11 (RECEIVE 8 BITS/CHAR)
		;iNITIALIZE TRANSMIT CONTROL
		db	WR5					;SELECT WRITE REGISTER 5
		db	_RTS_Enable|_Tx_Enable|_Tx_8bits_char   (0x6A)
								;BIT 0 = 0 (NO CRC ON TRANSMIT)
								;BIT 1 = 1 (REQUEST TO SEND)
								;BIT 2 = 0 (DON'T CARE)
								;BIT 3 = 1 (TRANSMIT ENABLE)
								;BIT 4 = 0 (DO NOT SEND BREAK)
								;BITS 6.5 = 11 (TRANSMIT 8 BITS/CHAR)
								;BIT 7 = 1 (DATA TERMINAL READY)
		DB	WR1					;SELECT WRITE REGISTER 1
		DB	_Tx_INT_EN|_Int_All_Rx_Char_NP		(0x1A)
		; DB	_Ext_INT_EN|_Tx_INT_EN|_Int_All_Rx_Char_NP|_WAIT_READY_R_T|_WAIT_READY_EN
								;BIT 0 = 1 (EXTERNAL INTERRUPTS)
								;BIT 1 = 1 (ENABLE TRANSMIT INTERRUPT)
								;BIT 2 = 0 (DO NOT CARE)
								;BITS 4. 3 = 11 (RECEIVE INTERRUPTS ON ALL CHARS. PARITY DOES NOT AFFECT VECTOR)
								;BITS 7.6.5 = 000 (NO WAIT/READY FUNCTION)



		; INITIALIZE CHANNEL B
		; _____________________________

		db	8					;OUTPUT 8 BYTES
		db	SIO_B_C			;DESTINATION IS COMMAND REGISTER B

		;iNITIALIZE BAUD RATE CONTROL
		db	_Reset_STAT_INT|WR4	;SELECT WRITE REGISTER 4 & RESET EXTERNAL/STATUS INTERRUPT
		db	_Stop_1_bit|_X16_Clock_mode
	
		; ; INITIALIZE RECEIVE CONTROL
		db	WR3		;SELECT WRITE REGISTER 3
		db	_Rx_Enable|_RX_8_bits
		; ;iNITIALIZE TRANSMIT CONTROL
		db	WR5					;SELECT WRITE REGISTER 5
		db	_Tx_Enable|_Tx_8bits_char
		; DB	_Ext_INT_EN|_Tx_INT_EN|_Int_All_Rx_Char_NP|_WAIT_READY_R_T|_WAIT_READY_EN
		DB	WR1					;SELECT WRITE REGISTER 1
		DB	_Status_Vector
		; DB	_Tx_INT_EN|_Int_All_Rx_Char_NP|_Status_Vector

		DB	0               ; END OF TABLE
		
;****************************************************************************************************************




PIO_Init:
;----------******************* PIO PORT A
		ld A, %11001111                 ;mode 3 in/out
		out (portA_Contr), A         ; set port A as output
		ld 	A,%00000000					; msb=input lsb = output, 0-mosi, 1-clk, 2-ssel, 7-miso
		out (portA_Contr), A         ; set port A as 4 input/ 4 output
		; ld A, SIO_PIO_CTC_stack&0xFF                   ; low byte of INT table
		; out (portA_Contr), A         ; PIO A interrupt vector
		ld A, $03
		out (portA_Contr), A         ; PIO A interrupt disable
		; ld a,SIO_PIO_CTC_stack>>8      ; high byte of INT table
		; ld I,A
		di
;----------******************* PIO PORT B
		ld A, %11001111                 ;mode 3 in/out
		out (portB_Contr), A         ; set port B as output
		ld A, $00                    ;mode 0 output 
		out (portB_Contr), A         ; set port B as output
		ld A, $03
		out (portB_Contr), A         ; PIO B interrupt disable
		ld a,0
		ld (PIO_B_value),a
		out (portB_Data), a
	ret

	if DOALIGN
		align 4
	endif


;********************************************************************************************     
;********************************************************************************************     

		xdef CTC_CH0_Interrupt_Handler,CTC_CH1_Interrupt_Handler,CTC_CH2_Interrupt_Handler,CTC_CH3_Interrupt_Handler

CTC_CH0_Interrupt_Handler:
CTC_CH1_Interrupt_Handler:

		; call 	TX_NAK
		call 	TX_C
		ld 		A,(TempVar1)
		inc 	A
		ld 		(TempVar1),A


		cp 		16							; Z : räkna antal cykler för XMODEM  
		jp 		P,showtimeout				; check if loop should timeout... A>16

		; call	Z,SetupXMODEM_TXandRX		; test if minicom has begun sending Z=0...

		or 		A							; clear carry - > wait for next.
		ld 		E,CTCpulse					; ret from CTC = 0A

		reti

;------------------------------------------------------------------------------


CTC_CH2_Interrupt_Handler:
CTC_CH3_Interrupt_Handler:


showtimeout:
		; call 	writeSTRBelow_CRLF
		; defb    "\0\r\n"
		; defb	"A timout on XMODEM occured !",00
		sub  	A
		ld 		(TempVar1),A
		out 	(portB_Data),A

		scf								; set carry flag 

		ei
		ld 		E,CTCtimeout					; ret from CTC = 0B (timeout)
		reti 





;********************************************************************************************     
;********************************************************************************************     


BOOTclear_p_C_read:
		; ***	återställ avbrottshanterare och vektorer till normalt avbrottshanterare för CTC och SIO 
		; call 	SIO_A_EI					; enable text output
		call 	HC376S_ResetAll
		call 	CTC1_INT_OFF
		ld		HL,CTC_CH1_Interrupt_Handler
		ld		(CTC_CH1_I_Vector),HL		;STORE CTC channel 1 VECTOR
		xor 	A
		ret


CTC_Init:

		; ------Reset all---------------
		ld		A,_Reset|_CW		; reset channel
		out		(CH0),A
		out		(CH1),A
		out		(CH2),A
		out		(CH3),A

		; ------INIT interrupt vectors for SIO /CTC---------------
		ld 		HL,CTC_CH0_I_Vector         ; (F410)
		ld  	A,L					; copy low byte
		out 	(CH0),A


		ld 	 	A,_Counter|_TC_Follow|_Reset|_CW
		out		(CH2),A
		ld		A,2				; time constant defined
		out		(CH2),A			; and loaded into channel 2


		;--------init CTC3 (Unused...)----------------------------------
		ld		A,00000011b		; int on, counter on, prescaler don't care, edge don't care,11000111b
		out		(CH3),A
		ld		A,0AFh			; time constant AFh defined
		out		(CH3),A			; and loaded into channel 3
		; ld		A,10h			; it vector defined in bit 7­3,bit 2­1 don't care, bit 0 = 0

		; out		(CH0),A			; and loaded into channel 0
		ret

		; ------INIT CTC2 Baud frequence  for  10 MHz version SIO------- 
		; 		Baud 		SIO_0,clockmode  CTCprescaler freq
		;		115200		16x				2			3,684	
		;		57600		16x				4			3,684
		;		38400		16x				6			3,684
		;		19200		16x				12			3,684
		; 		9600		16x				24			3,684


;********************************************************************************************     

.end


; 		waitEntry:
; 		call	S_head_tail			; spara inmatat början och slut (heads and tails)

; 		ld		hl,(Comm_Ptr_list)
; 		ld 		de,(Comm_Ptr_list+2)		; next item in list
; 		or 		a					; 0ställ carry
; 		SBC		hl,de 				; antal inmatade tecken i L (in L)
; 		ld 		b,l					; antal i B

; 			; avgör om  wraparound (hl)<(de) (cirkulär buffer)
; 		jp 		P,cont2				;positive result 	(hl)>(de)
; 		ld 		a,bufferSize 			; läsbuffert längd definieras i länkskript (0x64)
; 		add		a,b
; 		ld		b,a					; korrekt antal i B om 'wraparound'
; cont2:
; 		ld	 	ix,Textbuf				; get address of text buffer
; 		ld		(ix),B				; spara antal tecken i början av sträng.
; 		inc 	ix					; strängadress, stega förbi byte med längd

; 		ld		hl,(Comm_Ptr_list+2)		; first string start

; 		ld		DE,inBufferEnd
; c_nextchar:
; 			;	copy from inbuffer to Textbuf...
; 		ld 		a,(hl)				; char from inbuffer
; 		ld		(ix),a			; save char in textbuf
; 		inc		hl
; 		inc 	ix

; 			; check if upper buffer adr 
; 		or 		A		; clear carry
; 		push 	hl	
; 		SBC		hl,de		; S and Z set  hl=de -> inBufferEnd encountered
; 		pop 	hl
; 		jr 		NZ, cont1			; continue
; 			; turnaround (HL) hl = inBuffer.	
; 		ld 		hl,inBuffer			; hl = start of input buf
; cont1:
; 		djnz 	c_nextchar		; count no chars.
; 		xor 	a				; clear a
; 		ld 		(ix),A			; end with 00H
; 		ld		hl,Textbuf
; 		ld		a,(hl)			; A = string length (num)
; 		inc		hl				; HL = addr to first char in string
; 		ret 					; string stored in textbuffer
; 		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


; ReadChar:
; 		CALL	RetInpStatus	;get input status. return. carry = 1 if data available
; 		JR		NC,ReadChar		;wait if no character available
; 		DI                      ;disable interrupts
; 		LD		HL,inBufCount			;reduce input buffer count by 1
; 		DEC		(HL)
; 		LD		HL, (inHeadAdr)		;Get   character from head of input buffer
; 		LD		C, (HL)
; 		CALL	incInPointer			;Move head pointer up 1
; 		LD		(inHeadAdr) ,HL
; 		LD		A,C
; 		EI						;Reenable interrupts
; 		RET
; 			;return input status (carry    =1   if input data is available)
; RetInpStatus:
; 		LD		A, (inBufCount)		;Test input buffer count
; 		OR		A				;Clear carry always
; 		RET		Z				; Return, carry = 0 if no data
; 		SCF                     ;Set carry
; 		RET                     ; Return. carry = 1 if data available
; 		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


; 			; Save copies of inHeadAdr and inTailAdr to memory...
; S_head_tail:

; 		ld 		ix,Comm_Ptr_list+2
; 		ld 		b,list_len-2
; bmve:
; 		ld 		a,(ix)
; 		ld 		(ix+2),A
; 		dec		ix
; 		djnz 	bmve		; shift data uppåt...

; 		LD		HL,(inHeadAdr)		;GET   CHARACTER FROM HEAD OF INPUT BUFFER
; 		ld 		(Comm_Ptr_list),HL
; 		ret
; 		;-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

		;*************************************
		; subrutin: incInPointer
		; Öka pekarens värde för läsbuffer med 'wraparound'
		; invärde: hl = pekare
		; svar: hl = pekaren ökad inkl. 'wraparound'
		; Använda register: af,de,hl
		;***************************************
; incInPointer:
; 		inc		HL					; öka pekarens värde
; 		ld		DE,inBufferEnd		; jämför med värde för buffert maxvärde
; 		ld		A,L
; 		cp		E
; 		ret		NZ					;retur om värdena är olika L-E
; 		ld		A,H
; 		cp		D
; 		ret		NZ					;retur om värdena är olika H-D
; 		ld		HL,inBuffer			;om pekaren har hamnat på max -> inBuffer (start).
; 		ret                   	 	; retur med Z
		
