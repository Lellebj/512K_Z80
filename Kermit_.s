;XMODEM_SUB.s

		include 	"Z80_Params_.inc"
 
			xdef 	RAM_Start,SetupXMODEM_TXandRX,purgeRXA,purgeRXB,TX_EMP,TX_NAK,TX_ACK,SIO_A_DI,SIO_A_EI,SIO_A_RESET
			xdef 	SIO_A_RTS_OFF,SIO_A_RTS_ON,SIO_A_TXRX_INToff,SIO_A_TXon,SIO_A_RXon,SIO_A_TXRX_INTon,TX_C,TX_X
			xdef 	doImportKERMIT
			GLOBAL 	KermitACK, KermitNAK
;********************************************************		
;		Routines in order to read data via XMODEM on SIO_0 chA
;********************************************************		
			section 	Kermit

;**************************************************************************
;**				SetupXMODEM_TX and RX:									**
;**************************************************************************

doImportKERMIT:

			call 	writeSTRBelow
			DB 		0,"Väntar på minicom kermit... ",CR,LF,00
			xor 	A
			;		*** 	läs in tecken till dess ^A [01] hittas.

			call 	waitForFinishedPrintout		; vänta på utskrift till skärm.
			
			;------------INIT SIO för KERMIT----------------------------------------

			; ld 		HL,(commAdr1)

			ld		HL,recMinicomTecken      	; Avbrott för SIO A, när tecken kommer från minicom
			ld		(SIO_Int_Read_Vec),HL		; spara avbrottsvektor (recMinicomTecken) i SIO_Int_Read_Vec
			; ld 		A,_Reset_STAT_INT|_Reset_STAT_INT	
			; out		(SIO_A_C),A
			ld		A,_EN_INT_Nxt_Rx_Char|WR1			;skrivreg 0 ange WR1 ( sätt igång  INT när nästa tecken finns)
			out		(SIO_A_C),A
			ld		A,_Rx_INT_First_Char		;'wait' aktiverat , interrupt vid nästa RX tecken 
			; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
			out		(SIO_A_C),A		;buffer overrun is a spec RX condition
			
			; ***	Kod för att starta CTS kontroll.....
			; ld		A,WR3		;SELECT WRITE REGISTER 3
			; out		(SIO_A_C),A	
			; ld 		A,_Rx_Enable|_RX_8_bits|_Auto_Enable  
			; out		(SIO_A_C),A	

			call  	purgeRXA

			; ld 		HL,(commAdr1)
			ld 		C,1					; block number
			ld 		A,$0F
			call	p_srbank0
			ld 		HL,PktS				; adress för Paket S 

nextC:		

MainLoop:

			ei
			call 		WaitForNextChar					; vänta på avbrott från SIO/CTC  avbrott rutin 'recMinicomTecken', resultat A/E
			; 							; avbrott: recMinicomTecken aktiverar WAIT/RDY, resultat i A och E. HL-> (XBaddr)
			call 		Start_WaitRDY		; starta wait/RDY igen, för nästa tecken
			; ei


			; call 	SIO_A_RTS_ON			; 8 bitars teckenlängd (6&5), break (4), TX enable, RTS enable


			; call 	SIO_A_RTS_OFF			; 8 bitars teckenlängd (6&5), ingen break (4), TX enable

			; call 	Stop_WaitRDY

			ld  	A,E
			cp		SOH					;Sök 'MARK' /SOH  ^A
			jp		NZ,MainLoop		; om inte; sök nytt tecken.
			ld 		DE,00			; typ 1 block check  (börjar med längd -> slut datapaket)

			; ***	Paket längd (kodad)
			; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
getLength:
			out  	($60),A					; start pulseview

			in 		A,(SIO_A_D) ; Läs datat  wait/RDY aktiverat
			call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde (A är oförändrad!)

			sub 	CHAR_OFFSET 
			ld 		(PktLen), A				; Avkodad längd i A sparas i PktLen
			ld 		B,00
			ld 		C,A        			 ; BC = Loopräknare för resten av paketet (exkl. MARK & LEN) (B oförändrat 0)

			; ***	Sekvensnummer
			; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
			in 		A,(SIO_A_D) 		; Läs datat  wait/RDY aktiverat
			call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde A är oförändrad!
			ld 		(PktSeq), A				; Oavkodad sekvensnummer i A sparas i PktSeq
			sub 	CHAR_OFFSET
			dec 	C						; minska byte-räknare.... Avkodad sekvensnr.

			; ***	Läs pakettyp
			; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
			in 		A,(SIO_A_D) 		; Läs datat  wait/RDY aktiverat
			ld 		(PktType), A			; Avkodad sekvensnummer i A sparas i PktType
			call 	UppdateraBlockCheck		; uppdatera blockcheck i DE med inläst värde A är oförändrad!
			dec 	C						; minska byte-räknare....
			dec 	C
			
			; ***	Kolla om långa datapaket används
			; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
			ld  	A,(PktLen)
			or  	A			; längd satt till '0'
			jr 	 	NZ,.normalDpaket		; normalt paket ...
			ld 		A,(PktType)
			cp 		'D'
			jr 	 	NZ,.normalDpaket        ; inget datapaket

				; läs in långa datapaket (längd nästa två bytes)
				; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
			in 		A,(SIO_A_D) ; Läs datat högbyte wait/RDY aktiverat
			call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde A är oförändrad!
			push  	HL 				; spara slutadress för block
			push  	DE 				; spara blockcheck
				sub 	CHAR_OFFSET
				ld 	 	B,A 			; fixa multiplikation  95 * högbyte(avkodat värde i A)
				ld 		DE,$5F
				ld  	HL,00
.multhbyte:
				add 	HL,DE
				djnz 	.multhbyte

			in 		A,(SIO_A_D) ; Läs datat lågbyte wait/RDY aktiverat
            ; call 	InChar					; SIO läser in till A !
				ld 		B,A					; spara A
				sub 	CHAR_OFFSET
				ld  	D,0
				ld 		E,A
				add 	HL,DE			; lägg till lågbyte -> HL. Carry aldrig 1
				
			pop  	DE 				; återställ blockcheck
			ld  	A,B				; läs tillbaka lågbyte (A)
			call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde A är oförändrad!

			in 		A,(SIO_A_D) ; Läs HCHECK  (wait/RDY aktiverat)	
			call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde A är oförändrad!

				ld 	 	B,H
				ld 		C,l			; BC är antalet bytes vid långa paket
			pop 		HL 				; återställ slutadress för block
			dec 	BC 
				
		; ***	Läs Datafält  <BC> stycken bytes..
		; ***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	***	
.normalDpaket:


			; ***	Aktivera minnesbank '0' för att spara paketdata D
RecvDataLoop:
         	; ***	BC = Loopräknare för resten av paketet (exkl. MARK & LEN)
            ld 		A,B				; kolla specialfall där B = 0, C = 0 (dvs. inget datafält)
			or 		C
			jr 	 	Z,SlutData

			in 		A,(SIO_A_D) ; Läs datat  wait/RDY aktiverat
         	call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde A är oförändrad!

			exx 
			ld 		HL,PktType
			ld 		D,A
			ld  	A,'D'
			cp 		(HL)
			ld 		A,D
			exx 
			jr 		NZ,.noquote			; 'quote control characters' endast vid datapaket

			; *** 	kolla om tecken är '#'
			cp 		'#' 				; kolla 'quote control characters'
			jr 		NZ,.noquote

			; ***	läs in nästa tecken och XOR$40

			in 		A,(SIO_A_D) ; Läs datat  wait/RDY aktiverat
         	call 	UppdateraBlockCheck	; uppdatera blockcheck i DE med inläst värde A är oförändrad!
			dec 	BC
			cp 		'#'
			jr 		Z,.noquote
			xor  	A,$40				; ctl(x)


.noquote:
	        ld 		(HL), a				; spara paketdata på olika ställen beroende på typ
	        inc 	HL
            dec 	BC 
			jr 		RecvDataLoop
			;------------------------------------------------------------------ 

SlutData:	; ***	hoppar hit om inget datafält finns (B=0, C=0)
			; ***	6. Kontrollera block check 
			; ld  	HL,PktCHSum
			in 		A,(SIO_A_D) ; Läs filens block check wait/RDY aktiverat
			; ld 		(HL), a						; spara block check (1byte från host)
           	ld 		D,A        			 ; D = Mottagen block check 

			in 		A,(SIO_A_D) ; Läs datat 'CR' ($0D) wait/RDY aktiverat
			; cp 		CR
			; jp 		nz, CRCheck

            ; Beräkna Kermit-typ1 block check : ((c + (c & 0xC0)/0x40) & 0x3F) + 0x20
            ld 		A,E					; läs låg byte från DE
            and 	0xC0
            rlca
            rlca
            add 	A,E
            and 	0x3F
            add 	A,CHAR_OFFSET 		; A = Beräknad kodad block check 

            cp 		D					; jämför block check (om 1byte metod anv.) (beräknad) med värde från host.
            jp 		NZ,BadCheck

			call 	Stop_WaitRDY		; stäng av wait/RDY för SIO-CPU, ej SIO interrupt

            ; Analysera pakettyp
            ld 		A,(PktType)
            cp 		'S'          ; Send-Init packet?
            jr 		Z,HandleS

            cp 		'F'          ; File Header packet?
            jr 		Z,HandleF

            cp 		'A'          ; Attribute packet?
            jr 		Z,HandleA

            cp 		'D'          ; Data packet?
            jr 		Z,HandleD

            cp 		'Z'          ; End of File (EOF)?
            jr 		Z,HandleZ

            cp 		'B'          ; Break Transmission (EOT)?
            jr 		Z,HandleB

            jp 		MainLoop

            or 		A            ; Clear Carry = OK paket checksum
            ret
			; ***	-----------------------------------------------------------------



			; ***			Uppdatera Kermit checksumma typ 1 i DE
			; ***	-----------------------------------------------------------------
UppdateraBlockCheck:
           	push 	AF         ; Spara oavkodat värde i A 
            add 	A,E			; genererar ev carry flagga
			ld 		E,A			; uppdatera E
			pop 	AF			; återställ värde i A.
			ld 	(PktCHSum),DE	; uppdatera block check värde i DE till PktCHSum
			ret  	NC			; retur utan carry

			inc 	D			; addera 1 till D om carry, ny block check i DE
			ld 	(PktCHSum),DE	; uppdatera block check värde i DE till PktCHSum
			ret				; retur med carry 

			
HandleS:
			; Svara med ACK på Send-Init
			call KermitACK

			 			; ***	Aktivera minnesbank 'F' för att spara paket F & A
			; ***	Aktivera minnesbank 'F' för att spara paketdata F
			ld 		HL,PktF				; adress för Paket F 

			jp MainLoop

HandleF:
           	; Här kan du extrahera filnamn om du har ett filsystem (t.ex. CP/M)
            call KortKermitACK
			; ***	Aktivera minnesbank 'F' för att spara paketdata A
			ld 		HL,PktA				; adress för Paket A 

        	jp MainLoop

HandleA:	; Attribut, tid, storlek mm.
            ; Skriv ut eller lagra rådata (avkodad)
            call KortKermitACK
			xor   	A				; spara nummer 0 på minnesbanken i A
			call	 p_srbank0		; om datapaket ska lagras.

			ld 		HL,(commAdr1)		; börja spara data på minnesbank 0 vid angiven adress i kommando

            jp MainLoop

HandleD:
           ; Skriv ut eller lagra rådata (avkodad)
            ; call ProcessData
			; testa fördröjning

			
            call KermitACK
            jp MainLoop

HandleZ:
            call KortKermitACK
            jp MainLoop

HandleB:
           call KortKermitACK
            ; Överföring klar!
            ; halt
			call 	restoreSIO_0IO 			; get normal keyboard/screen function
			; call 	waitForFinishedPrintout		; vänta på utskrift till skärm.
			; ****	Vänta på SIO utskriftsbuff...

			ld 		BC,$01FF
.waitSIOp:		
			dec 	BC 
			ld 		A,B
			or 		C 
			jr 		NZ,	.waitSIOp
			call 	writeSTRBelow_CRLF
			defb    "\0\r\n"
			defb	"Filen inläst OK !",00

            scf             ; Set Carry = Fel checksumma paket
            ret



BadCheck:

			; halt
			call 	restoreSIO_0IO 			; get normal keyboard/screen function
			call 	writeSTRBelow_CRLF
			defb    "\0\r\n"
			defb	"Checksummafel för kermit inträffade!",00
			ret

; CRCheck:

; 		; halt
; 		call 	restoreSIO_0IO 			; get normal keyboard/screen function
; 		call 	writeSTRBelow_CRLF
; 		defb    "\0\r\n"
; 		defb	"Sista tecken inte CR !!!",00
; 		ret




Error:
            ; Vid fel, skicka NAK för att begära omsändning
            call KermitNAK
            jp MainLoop


		; 	Verifiera OK - kort sträng	
		;********************************************************************************************     
KortKermitACK:
			push  	BC
			ld 		IX,KortAckStr+1	; hoppa över inledande '0' i utskriftssträng
			jr 		GemAck

		; 	Verifiera OK - lång sträng	
		;********************************************************************************************     
KermitACK:
			push  	BC
			ld 		IX,AckStr+1		; hoppa över inledande '0' i utskriftssträng
GemAck:
			ld 		A,(PktSeq)		; sekvensnummer ej avkodat
			ld  	(IX+2),A	; fixar sekvensnummer +32 i sträng
			
			ld  	A,(IX+1)	; ladda längd +32 från strängen
			sub 	A,CHAR_OFFSET	; längd avkodad
			ld 	 	B,A

			ld  	a,00			; start checksumma
			push  	IX 				; spara IX antingen för kort eller lång ACK
.CHsumCount:
			add  	A,(IX+1)		; addera alla värden fram till checksumma
			inc  	IX

			djnz 	.CHsumCount		
			; (IX+1) pekar på checksumman
			ld  	C,A				; slutsumman i C
			; Beräkna Kermit-typ1 checksumma: ((A + (A & 0xC0)/0x40) & 0x3F) + CHAR_OFFSET
            and 0xC0
            rlca
            rlca
            add a, c
            and 0x3F
            add a, CHAR_OFFSET ; A = Beräknad kodad checksumma
	
			ld  	(IX+1),A	; fixar checksum +32 i strängen

			pop 	IX			; återställ IX till start av ACK sträng (kort eller lång)
			; ld 		IX,AckStr	
			ld 		A,(IX+1)			; läs längd (LEN) till A
			add 	A,3
			sub 	A,CHAR_OFFSET			; avkodad längd
			ld  	B,A				; loopa genom alla värden
.ACKloop:
			ld 		A, (IX)
            call 	P_TX_EMP
			inc 	IX
			djnz 	.ACKloop

			pop 	BC
            
			ret

			;     MARK, 			LEN ,			SEQ, TYPE, MAXL, TIMO, NPAD, PADC, EOL,  QCTL, EBQ, BCT, RPT, 		CAPAS,    		WSLOTS, MaxLx1, Maxl2,  chksum, CR
AckStr: 	defb  0,MARK, AckEnd-AckStr-4+CHAR_OFFSET, 0,	  'Y', '~', 0x2F, 0x20, 0x40, CR+32, '#', 'N', '1',  ' ', %00111010+CHAR_OFFSET, 0x20,   0x35,   0x55,   	0,  CR,0
																																	;	2*95 +32
AckEnd:		defb 	AckEnd-AckStr
			even
KortAckStr: defb  0,MARK, 3+CHAR_OFFSET, 				0,	  'Y',  	0,  CR,0

KortAckEnd:	defb 	KortAckEnd-KortAckStr
			even

		; ***	Utskrift till C-Kermit/wermit
		;********************************************************************************************     
KermitNAK:
            ld a, MARK
            call P_TX_EMP
            ld a, 3 + CHAR_OFFSET
            call P_TX_EMP
            ld a, (PktSeq)
            add a, CHAR_OFFSET
            call P_TX_EMP
            ld a, 'N'       ; 'N' = NAK
            call P_TX_EMP
            ld a, 0x20 + 40 ; Förenklad exempelchecksumma för NAK
            call P_TX_EMP
 			ld 	A,CR
            call P_TX_EMP
           ret

					

;********************************************************************************************     



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
		; ***	Utskrift till C-Kermit/wermit


;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;		Avbrottshantering vid  KERMIT
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

recMinicomTecken:
				;***	läs tecken från minicom till dess [01] ^A
		; out  	($60),A					; start pulseview
	
		call 	Start_WaitRDY			; aktivera wait - RDY för SIO-CPU

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
