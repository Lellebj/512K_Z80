;Kermit_protocols.s

		include 	"Z80_Params_.inc"
 
		GLOBAL   	doImportKERMIT


        section     Kermit_sektion_gammal
;********************************************************		
;		Filöverföring med C-Kermit
;********************************************************		
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

doImportKERMIT:

		call 	writeSTRBelow
		DB 		0,"Väntar på S paket från minicom... ",CR,LF,00
		xor 	A
		;		*** 	läs in tecken till dess ^A [01] hittas.


		;------------INIT SIO för KERMIT----------------------------------------

		; ld 		HL,(commAdr1)

		ld		HL,recMinicomTecken      	; Avbrott för SIO A, när tecken kommer från minicom
		ld		(SIO_Int_Read_Vec),HL		; spara avbrottsvektor (recMinicomTecken) i SIO_Int_Read_Vec
		ld 		A,_Reset_STAT_INT|_Reset_STAT_INT	
		ld		A,_EN_INT_Nxt_Rx_Char|WR1			;skrivreg 0 ange WR1 ( sätt igång  INT när nästa tecken finns)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char		;'wait' aktiverat , interrupt vid nästa RX tecken 
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition

		call  	purgeRXA

		; ld 		HL,(commAdr1)
		ld 		HL,$A810
		ld 		C,1					; block number
		xor  	A
		ld  	($A804),A
nextC:		
		ei

nextBlock:
		ld		A,_EN_INT_Nxt_Rx_Char|WR1				;skrivreg 0 ange WR1 ( sätt igång  INT när nästa tecken finns)
		out		(SIO_A_C),A
		ld		A,_Rx_INT_First_Char			;'wait' aktiverat , interrupt vid nästa RX tecken 
		; ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char			;'wait' aktiverat , interrupt vid nästa RX tecken 
		out		(SIO_A_C),A		;buffer overrun is a spec RX condition
		ei


		call 	SIO_A_RTS_ON			; 8 bitars teckenlängd (6&5), break (4), TX enable, RTS enable

		halt						;vänta på första tecken SIO A

		call 	SIO_A_RTS_OFF			; 8 bitars teckenlängd (6&5), ingen break (4), TX enable

		; ***	stäng av wait funktion
		ld		A,WR1			;Skriv SIO skrivreg  WR0: ange skrivregister 1 WR1
		out		(SIO_A_C),A
		ld		A,_WAIT_READY_R_T|_Rx_INT_First_Char		;stäng av wait funktion från nu
		out		(SIO_A_C),A

		; ***	kolla om (  ) har ställt till 1 -> lagra värde i E, flagga i A.

		ld  	A,E

		cp		SOH					;check for SOH  [01]
		jp		Z,startcap
		cp		CR					;check for ^M  [0D]
		jp		z,endcap

		ld 		A,($A804)
		cp 		1
		jr   	NZ,nextC
sparaC:
		ld  	A,E
		ld   	(HL),A
		inc  	HL
		jr  	nextC

		; ld		E,EOT_FOUND			;eot found (end of transmission)
startcap:
		;***	starta lagring av paket
		ld 		A,($A804)
		inc   	A  			; flagga Z ej aktiv (NZ)
		ld  	($A804),A
		jr 		sparaC

endcap:	
		;***	stoppa lagring av paket
		ld 		A,($A804)
		cp  	1
		jr  	z,endcap2
		jr 	 	nextC

endcap2:		
		inc   	A  			; flagga Z ej aktiv (NZ)
		ld  	($A804),A

		CALL 	InitBuffers			;Nollställ in/ut buffers, SIO A. INTERRUPT SYSTEM
		call 	SIO_A_TXRX_INTon
		call 	SIO_A_RTS_ON

		ld  	IY,ACKstring
		call 	WriteLine

.n_tecken:
; 		inc  	IY
; 		ld  	A,(IY)
; 		out		(SIO_A_D),A			; skicka tecken till SIO A (utan avbrott)
; 		call	TX_EMP

; 		ld  	A,(IY)
; 		cp 		CR					; vagnretur 0Dh ?
		; jr 	    NZ,.n_tecken
		jr 	    .n_tecken


		halt 
		halt 
		halt 

ACKstring:
		defb 	 0x00, 0x01, ',', ' ', 'Y', '~', '*', ' ', '@', '-', '#', 'N', '1', '~', 0x0D, 0x00, 0x00


		out 	(portB_Data),A

		jr 		nextC 					; one more 'C' -> goto .nextC

		

blockFinished:
		call	TX_ACK					;when no error
		inc		C						;prepare next block to receive
		sub		A
		ld		(TempVar2),A			;clear bad block counter
		jp 		nextC

;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
;*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*

recMinicomTecken:
				;***	läs tecken från minicom till dess [01] ^A
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
		ld 		($A808),A
		ld    	E,A

		reti


        section     Kermit_sektion
; ==============================================================================
; Minimalistisk Kermit-mottagare för Z80 SBC med Z80 SIO
; Kompileras med VASM: vasmz80_std -Fbin -o kermit.bin kermit_sio.asm
; ==============================================================================

; --- Hårdvarukonfiguration (Ändra så det matchar din SBC) ---
SIO_A_C     .equ 0x01       ; SIO Kanal A Kontrollport
SIO_A_D     .equ 0x00       ; SIO Kanal A Dataport

; --- Kermit Protokoll-konstanter ---
MARK        .equ 0x01       ; SOH (Ctrl-A) Start på Kermit-paket
MAX_BUFF    .equ 100        ; Maximal paketlängd
CHAR_OFFSET .equ 32         ; Kermit-kodning offset ('#')

            .org 0x0000     ; Startadress i RAM/ROM för din SBC

Start:
            ld sp, 0xFFFF   ; Initiera stackpekare
            call InitSIO    ; Initiera serieporten

MainLoop:
            call RecvPacket ; Vänta på och läs in ett paket
            jr c, Error     ; Hoppa om fel uppstod (Timeout/Checksumme-fel)

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
            jr MainLoop

HandleS:
            ; Svara med ACK på Send-Init
            call SendACK
            jr MainLoop

HandleF:
            ; Här kan du extrahera filnamn om du har ett filsystem (t.ex. CP/M)
            call SendACK
            jr MainLoop

HandleD:
            ; Skriv ut eller lagra rådata (avkodad)
            call ProcessData
            call SendACK
            jr MainLoop

HandleZ:
            call SendACK
            jr MainLoop

HandleB:
            call SendACK
            ; Överföring klar!
            halt

Error:
            ; Vid fel, skicka NAK för att begära omsändning
            call SendNAK
            jr MainLoop

; ==============================================================================
; HÅRDVARUDRIVRUTINER (Z80 SIO)
; ==============================================================================

InitSIO:
            ; Initiera SIO Kanal A (8 bitar, 1 stoppbit, ingen paritet)
            ld hl, SIO_Init_Seq
            ld b, SIO_Init_End - SIO_Init_Seq
InitLoop:
            ld a, (hl)
            out (SIO_A_C), a
            inc hl
            djnz InitLoop
            ret

SIO_Init_Seq:
            .byte 0x00, 0x18 ; WR0: Channel Reset
            .byte 0x04, 0x44 ; WR4: X16 klocka, 1 stoppbit, ingen paritet
            .byte 0x03, 0xC1 ; WR3: RX 8 bitar, RX Enable
            .byte 0x05, 0x68 ; WR5: TX 8 bitar, TX Enable, RTS/DTR bitar låga
SIO_Init_End:

; --- Läs ett tecken från SIO (med blocking/polling) ---
InChar:
            in a, (SIO_A_C)
            bit 0, a        ; RX Character Available?
            jr z, InChar
            in a, (SIO_A_D) ; Läs datat
            ret

; --- Skicka ett tecken via SIO ---
OutChar:
            push af
OutChar_Wait:
            in a, (SIO_A_C)
            bit 2, a        ; TX Buffer Empty?
            jr z, OutChar_Wait
            pop af
            out (SIO_A_D), a
            ret

; ==============================================================================
; KERMIT PROTOKOLL-IMPLEMENTERING
; ==============================================================================

RecvPacket:
            ; 1. Sök efter START-markör (SOH)
WaitMark:
            call InChar
            cp MARK
            jr nz, WaitMark

            ld c, 0         ; Nollställ checksumma (Kermit typ 1 Checksum)

            ; 2. Läs LÄNGD (kodad)
            call InChar
            ld b, a         ; Spara rå-längd för checksumma
            add a, c
            ld c, a         ; Uppdatera checksumma
            ld a, b
            sub CHAR_OFFSET ; Avkoda längd
            ld (PktLen), a
            ld b, a         ; B = Loopräknare för resten av paketet (exkl. MARK & LEN)

            ; 3. Läs SEKVENSSNUMMER
            call InChar
            ld d, a         ; Spara rå-sekvens
            add a, c
            ld c, a         ; Uppdatera checksumma
            ld a, d
            sub CHAR_OFFSET
            ld (PktSeq), a
            dec b

            ; 4. Läs PAKETTYP ('S', 'F', 'D', etc.)
            call InChar
            ld (PktType), a
            add a, c
            ld c, a         ; Uppdatera checksumma
            dec b

            ; 5. Läs DATA-fältet
            ld hl, PktData
RecvDataLoop:
            ld a, b
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
            call InChar     ; Läs inkommande checksumma
            ld d, a         ; D = Mottagen checksumma

            ; Beräkna Kermit-stil checksumma: ((c + (c & 0xC0)/0x40) & 0x3F) + 0x20
            ld a, c
            and 0xC0
            rlca
            rlca
            add a, c
            and 0x3F
            add a, CHAR_OFFSET ; A = Beräknad kodad checksumma

            cp d
            jr nz, BadCheck
            or a            ; Clear Carry = Success
            ret
BadCheck:
            scf             ; Set Carry = Fel
            ret

; --- Skicka ACK/NAK svar ---
SendACK:
            ld a, MARK
            call OutChar
            ld a, 3 + CHAR_OFFSET ; Längd (3 bytes: Len, Seq, Type)
            call OutChar
            ld a, (PktSeq)
            add a, CHAR_OFFSET
            call OutChar
            ld a, 'Y'       ; 'Y' = ACK
            call OutChar
            ; För enklast möjliga ACK hårdkodar vi en statisk eller simpel checksumma här,
            ; men i skarp drift bör den beräknas enligt formeln ovan.
            ld a, 0x20 + 35 ; Förenklad exempelchecksumma för ACK
            call OutChar
            ret

SendNAK:
            ld a, MARK
            call OutChar
            ld a, 3 + CHAR_OFFSET
            call OutChar
            ld a, (PktSeq)
            add a, CHAR_OFFSET
            call OutChar
            ld a, 'N'       ; 'N' = NAK
            call OutChar
            ld a, 0x20 + 40 ; Förenklad exempelchecksumma för NAK
            call OutChar
            ret

; --- Processa och skicka data till konsol/skärm ---
ProcessData:
            ld hl, PktData
            ld a, (PktLen)
            sub 3           ; Subtrahera Seq, Type, Checksum för att få ren datalängd
            ld b, a
PrintLoop:
            ld a, (hl)
            ; (Här kan du lägga till avkodning för Kermit "Control-prefixing" om filer innehåller binärdata)
            call OutChar    ; Skickar rådata tillbaka ut på terminalen som demo
            inc hl
            djnz PrintLoop
            ret

; ==============================================================================
; BUFFERTAR & RAM
; ==============================================================================
PktLen:     .byte 0
PktSeq:     .byte 0
PktType:    .byte 0
PktData:    .space MAX_BUFF ; Reservera minne för inkommande data


    ; KERMIT
; set line /dev/ttyUSB0   (eller COM1 på Windows)
; set speed 9600
; set carrier-watch off
