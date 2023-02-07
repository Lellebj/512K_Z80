
		INCLUDE "Z80_Params_.inc"
		Section IOLIB
					
				
					; Interrupts
					; 11A    Unbuffered Input/Output Using an DART          394
					; 11B    Unbuffered Input/Output Using a PIO 404
					; 11C    Buffered Input/Output Using an DART          413
					; 11D   Real-Time Clock and Calendar 425


					;*************************************************************************************************************
					;*************************************************************************************************************
					; Unbuffered Input/Output
					; 														I if full
					; 														5. INIT: none
					; 		Title                 Simple interrupt input and output using an DART
					; 								and single character buffers
					; 		Name:                 SINTIO
					; 		Purpose:       This program consists of 5 subroutines which
					; 					perform-interrupt driven input and output using
					; 					an DART.
					; 					ReadChar
					; 						Read a character
					; 					INST
					; 						Determine input status (whether input
					; 						buffer is empty)
					; 					OUTCH
					; 						Write a character
					; 					OUTST
					; 						Determine output status (whether output
					; 						buffer is full)
					; 					INIT
					; 						Initialize DART and interrupt system
					; 		Entry:         ReadChar
					; 						No parameters
					; 					INST
					; 						No parameters
					; 					OUTCH
					; 						Register A = character to transmit
					; 					OUTST
					; 						No parameters
					; 					INIT
					; 						No parameters
					; 		Exit:          ReadChar
					; 						Register A = character
					; 					INST
					; 						Carry = 0 if input buffer is empty,
					; 						1 if character is available
					; 					OUTCH
					; 						No parameters
					; 					OUTST
					; 						Carry = 0 if output buffer is not
					; 						full, 1 if i t is full
					; 					INIT
					; 						No parameters
					; 		Registers used: ReadChar - AF
					; 						INST - AF
					; 						OUTCH - AF
					; 						OUTST - AF
					; 						INIT - AF,BC,HL,I

					; 		Time:           ReadChar
					; 						72 cycles if a character is available
					; 						INST
					; 						27 cycles
					; 							OUTCH
					; 							150 cycles if output buffer is not full
					; 								and output interrupt is expected
					; 							OUTST
					; 							27 cycles
					; 							INIT
					; 							618 cycles
					; 							RDHDLR
					; 							82 cycles
					; 							WRHDLR
					; 							160 cycles
					; 		Size:              Program 202 bytes
					; 							Data      5 bytes

					; 		,DART EQUATES
					; 		DART IS PROGRAMMED FOR:
					; 			ASYNCHRONOUS OPERATION
					; 			16 X BAUD RATE
					; 			8-BIT CHARACTERS
					; 		, 1 1/2 STOP BITS
					; 		,ARBITRARY DART PORT ADDRESSES
					;***********************************************************************************************************************
					;***********************************************************************************************************************
;
;
; INCH_11A:
; 		CALL	INST_11A				;GET INPUT STATUS
; 		JR		NC,INCH_11A				;WAIT IF NO CHARACTER AVAILABLE
; 		DI							;DISABLE INTERRUPTS
; 		SUB		A
; 		LD		(RECDF),A			;INDICATE INPUT BUFFER EMPTY
; 		LD		A,(RECDAT)			;GET CHARACTER FROM INPUT BUFFER
; 		EI							;ENABLE INTERRUPTS
; 		RET
; 			;RETURN INPUT STATUS (CARRY = 1 IF INPUT DATA IS AVAILABLE)
; INST_11A:
; 		LD		A,(RECDF)			;GET DATA READY FLAG
; 		RRA							;SET CARRY FROM DATA READY FLAG
; 									;IF CARRY = 1, CHARACTER IS AVAILABLE
; 		RET
; 			;WRITE CHARACTER

; OUTCH_11A:
; 		PUSH	AF					;SAVE CHARACTER TO WRITE
; 		;WAIT FOR CHARACTER BUFFER TO EMPTY, THEN STORE NEXT CHARACTER
; WAITOOC_11A:
; 		CALL	OUTST_11A				;GET OUTPUT STATUS
; 		JR		C,WAITOOC_11A		;WAIT IF OUTPUT BUFFER IS FULL
; 		DI							;DISABLE INTERRUPTS WHILE LOOKING AT
; 									; SOFTWARE FLAGS
; 		POP		AF					;GET CHARACTER
; 		LD		(TRNDAT),A			;STORE CHARACTER IN OUTPUT BUFFER
; 		LD		A,0FFH				;INDICATE OUTPUT BUFFER FULL
; 		LD		(TRNDF),A
; 		LD		A,(OutINTExpect)				;TEST OUTPUT INTERRUPT EXPECTED FLAG
; 		OR		A
; 		CALL	Z,OUTDAT_11A			;OUTPUT CHARACTER IMMEDIATELY IF
; 									; NO OUTPUT INTERRUPT EXPECTED
; 		EI                     	    ;ENABLE INTERRUPTS
; 		RET
; 			;OUTPUT STATUS (CARRY    =1   IF OUTPUT BUFFER IS FULL)
; OUTST_11A:
; 		LD		A,(TRNDF)			;GET TRANSMIT FLAG
; 		RRA							;SET CARRY FROM TRANSMIT FLAG
; 		RET							; CARRY = 1 IF BUFFER FULL
; 		;INITIALIZE INTERRUPT SYSTEM AND DART
; INIT_11A:
; 		DI							;DISABLE INTERRUPTS FOR INITIALIZATION
; 			; INITIALIZE SOFTWARE FLAGS
; 		SUB		A
; 		LD		(RECDF),A			;NO INPUT DATA AVAILABLE
; 		LD		(TRNDF),A			;OUTPUT BUFFER EMPTY
; 		LD		(OutINTExpect),A				;NO OUTPUT INTERRUPT EXPECTED
; 									; DART IS READY TO TRANSMIT INITIALLY
; 			;INITIALIZE INTERRUPT VECTORS
; 		LD      A,DART_Int_Vec >> 8			;GET INTERRUPT PAGE NUMBER
; 		LD      I,A				;SET INTERRUPT VECTOR IN zao
; 		IM      2				;INTERRUPT MODE 2 - VECTORS IN TABLE
; 								; ON INTERRUPT PAGE
; 		LD		HL,RDHDLR_11A			;STORE READ VECTOR (INPUT INTERRUPT)
; 		LD		(DART_Int_Read_Vec),HL
; 		LD		HL,WRHDLR_11A			;STORE WRITE VECTOR (OUTPUT INTERRUPT)
; 		LD		(DART_Int_WR_Vec),HL
; 		LD		HL,EXHDLR_11A			;STORE EXTERNAL/STATUS VECTOR
; 		LD		(DART_Int_EXT_Vec),HL
; 		LD		HL,REHDLR_11A			;STORE RECEIVE ERROR VECTOR
; 		LD		(DART_Int_Spec_Vec),HL
; 			; INITIALIZE DART
; 		LD		HL,DARTINT			;GET BASE OF INITIALIZATION ARRAY
; 		CALL	IPORTS_11A            	;INITIALIZE DART
; 		EI							;ENABLE INTERRUPTS
; 		RET

; 			;INPUT (READ) INTERRUPT HANDLER
; RDHDLR_11A:
; 		PUSH	AF					;SAVE AF
; RD1_11A:	IN		A, (DART_A_D)			;READ DATA FROM DART
; 		LD		(RECDAT), A			;SAVE DATA IN INPUT BUFFER
; 		LD		A,0FFH
; 		LD		(RECDF),A			;INDICATE INPUT DATA AVAILABLE
; 		POP		AF					;RESTORE AF
; 		EI							;REENABLE INTERRUPTS
; 		RETI
; 			;OUTPUT (WRITE) INTERRUPT HANDLER
; WRHDLR_11A:
; 		PUSH	AF
; 		LD		A,(TRNDF)			;GET DATA AVAILABLE FLAG
; 		RRA
; 		JR		NC,NODATA_11A			;JUMP IF NO DATA TO TRANSMIT
; 		CALL	OUTDAT_11A				;OUTPUT DATA TO DART
; 		JR		WRDONE_11A

; 		; IF AN OUTPUT INTERRUPT OCCURS WHEN NO DATA IS AVAILABLE.
; 		; WE MUST RESET IT TO AVOID AN ENDLESS LOOP. LATER. WHEN A
; 		; CHARACTER BECOMES AVAILABLE, WE NEED TO KNOW THAT AN OUTPUT
; 		; INTERRUPT HAS OCCURRED WITHOUT BEING SERVICED. THE KEY HERE
; 		; IS THE OUTPUT INTERRUPT EXPECTED FLAG OlE. THIS FLAG IS
; 		; CLEARED WHEN AN OUTPUT INTERRUPT HAS OCCURRED BUT HAS NOT
; 		; BEEN SERVICED. IT IS ALSO CLEARED INITIALLY SINCE THE
; 		; DART STARTS OUT READY. OlE IS SET WHENEVER DATA IS ACTUALLY
; 		; SENT TO THE DART. THUS THE OUTPUT ROUTINE OUTCH CAN CHECK
; 		; OlE TO DETERMINE WHETHER TO SEND THE DATA IMMEDIATELY
; 		; OR WAIT FOR AN OUTPUT INTERRUPT.
; 		; THE PROBLEM IS THAT AN.OUTPUT DEVICE MAY REQUEST SERVICE BEFORE
; 		; THE COMPUTER HAS ANYTHING TO SEND (UNLIKE AN INPUT DEVICE
; 		; THAT HAS DATA WHEN IT REQUESTS SERVICE). THE OlE FLAG
; 		; SOLVES THE PROBLEM OF AN UNSERVICED OUTPUT INTERRUPT ASSERTING
; 		; ITSELF REPEATEDLY. WHILE STILL ENSURING THE RECOGNITION OF
; 		; OUTPUT INTERRUPTS.
; NODATA_11A:
; 		SUB		A
; 		LD		(OutINTExpect),A				;DO NOT EXPECT AN INTERRUPT
; 		OUT		(DART_A_C),A			;SELECT REGISTER 0
; 		LD		A,00101000B			;RESET DART TRANSMITTER INTERRUPT
; 		OUT		(DART_A_C),A
; WRDONE_11A:
; 		POP		AF					;RESTORE AF
; 		EI
; 		RETI
; 			;EXTERNAL/STATUS CHANGED INTERRUPT HANDLER
; EXHDLR_11A:
; 		PUSH	AF
; 		LD		A,00010000B			;RESET STATUS INTERRUPT
; 		OUT		(DART_A_C),A
; 		EI							;DCD OR CTS CHANGED STATE, OR A BREAK

; 		POP		AF					; WAS DETECTED
; 		RETI						; SERVICE HERE IF NECESSARY
; 			;SPECIAL RECEIVE ERROR INTERRUPT
; REHDLR_11A:
; 		PUSH	AF
; 		LD		A,00110000B			;RESET RECEIVE ERROR INTERRUPT
; 		OUT		(DART_A_C),A
; 		EI							; FRAMING ERROR OR OVERRUN ERROR
; 		POP		AF					; OCCURRED
; 		RETI						; SERVICE HERE IF NECESSARY

; 		;*************************************
; 		; ROUTINE: OUTDAT
; 		;PURPOSE: SEND CHARACTER TO DART
; 		;ENTRY: TRNDAT = CHARACTER
; 		;EXIT: NONE
; 		;REGISTERS USED: AF
; 		;***************************************
; OUTDAT_11A:
; 		LD		A,(TRNDAT)			; GET DATA FROM OUTPUT BUFFER
; 		OUT		(DART_A_D),A			; SEND DATA TO DART
; 		SUB		A					; INDICATE OUTPUT BUFFER EMPTY
; 		LD		(TRNDF),A
; 		DEC		A					; INDICATE OUTPUT INTERRUPT EXPECTED
; 		LD		(OutINTExpect),A				; OlE = FF HEX
; 		RET
		
; 		;**************************************
; 		;ROUTINE: IPORTS
; 		;PURPOSE: INITIALIZE 1/0 PORTS
; 		;ENTRY: HL = BASE ADDRESS OF INITIALIZATION ARRAY
; 		;EXIT: DATA OUTPUT TO PORTS
; 		;REGISTERS USED: AF.BC.HL
; 		;************************************

; IPORTS_11A:
; 			;GET NUMBER OF DATA BYTES TO SEND TO CURRENT PORT
; 			;EXIT IF NUMBER OF BYTES IS O. INDICATING TERMINATOR
; 		LD		A,(HL)				;GET NUMBER OF BYTES
; 		OR		A					;TEST FOR ZERO (TERMINATOR)
; 		RET		Z					;RETURN IF NUMBER OF BYTES = 0
; 		LD		B,A
; 		INC		HL					;POINT TO PORT ADDRESS (NEXT BYTE)
; 			;C = PORT ADDRESS
; 			;HL = BASE ADDRESS OF OUTPUT DATA
; 		LD      C,(HL)				;GET PORT ADDRESS
; 		INC     HL					;POINT TO FIRST DATA VALUE (NEXT BYTE)
; 			;OUTPUT DATA AND CONTINUE TO NEXT PORT
; 		OTIR						;SEND DATA VALUES TO PORT
; 		JR      IPORTS_11A				;CONTINUE TO NEXT PORT ENTRY
; 			;DART INITIALIZATION DATA
; DARTINT_11A:
; 			;RESET CHANNEL A
; 		DB		1					;OUTPUT 1 BYTE
; 		DB		DART_A_C				;DESTINATION IS CHANNEL A COMMAND/STATUS
; 		DB		00011000B			;SELECT WRITE REGISTER 0
; 									;BITS 2,1,0 = 0 (WRITE REGISTER 0)
; 									;BITS 5.4.3 = 011 (CHANNEL RESET)
; 									;BITS 7,6 = 0 (DO NOT CARE)
; 			;SET INTERRUPT VECTOR AND ALLOW STATUS TO AFFECT IT
; 		DB		4					;OUTPUT 2 BYTES
; 		DB		DART_B_C				;DESTINATION IS COMMAND REGISTER B
; 		DB		00000010B			; SELECT WRITE REGISTER 2
; 		DB		DART_Int_Vec&0FFH			;SET INTERRUPT VECTOR FOR DART
; 		DB		00000001B			;SELECT WRITE REGISTER 1
; 		DB		00000100B			;ALLOW STATUS TO AFFECT VECTOR
; 			; INITIALIZE CHANNEL A
; 		DB		8					;OUTPUT 8 BYTES
; 		DB		DART_A_C				;DESTINATION IS COMMAND REGISTER A
; 			; INITIALIZE BAUD RATE CONTROL
; 		DB		00010100B			;SELECT WRITE REGISTER 4
; 									; RESET EXTERNAL/STATUS INTERRUPT
; 		DB		01001000B			;BIT 0 = 0 (NO PARITY)
; 									;BIT 1 = 0 (DON'T CARE)
; 									;BITS 3.2 = 10 (1 1/2 STOP BITS)
; 									;BITS 5,4 = 00 <nOWT CARE)
; 									;BITS 7,6 = 01 (16 TIMES CLOCK)
; 			;INITIALIZE RECEIVE CONTROL
; 		DB		00000011B			;SELECT WRITE REGISTER 3
; 		DB		11000001B			;BIT 0 = 1 (RECEIVE ENABLE)
; 									; BITS 4,3,2,1 = 0 (DON"T CARE)
; 									;BIT 5 = 0 (NO AUTO ENABLE)
; 									;BIT 7,6 = 11 (RECEIVE 8 BITS/CHAR)
; 			; INITIALIZE TRANSMIT CONTROL
; 		DB		00000101B			;SELECT WRITE REGISTER 5
; 		DB		11101010B			;BIT 0 = 0 (NO CRC ON TRANSMIT)
; 									;BIT 1 = 1 (REQUEST TO SEND)
; 									;BIT 2 = 0 (DON'T CARE)
; 									;BIT 3 = 1 (TRANSMIT ENABLE)
; 									;BIT 4 = 0 (DO NOT SEND BREAK)
; 									;BITS 6,5 = 11 (TRANSMIT 8 BITS/CHAR)
; 									;BIT 7 = 1 (DATA TERMINAL READY)
; 		DB		00000001B			;SELECT WRITE REGISTER 1
; 		DB		00011011B			;BIT 0 = 1 (EXTERNAL INTERRUPTS)
; 									;BIT 1 = 1 (ENABLE TRANSMIT INTERRUPT)
; 									;IBIT 2 = 0 (DO NOT CARE)
; 									;BITS 4,3 = 11 (RECEIVE INTERRUPTS ON
; 									; ALL CHARS, PARITY DOES NOT AFFECT
; 									; VECTOR)
; 									;BITS 7,6,5 = 000 (NO WAIT/READY
; 									; FUNCTION)

; 		DB	0						;TERMINATOR FOR INITIALIZATION ARRAY
; 		; DATA SECTION
; RECDAT:	DS		1					;RECEIVE DATA
; RECDF: 	DS		1					;RECEIVE DATA FLAG
; 									; (0 = NO DATA. FF = DATA AVAILABLE)
; TRNDAT: DS		1					; TRANSMIT DATA
; TRNDF: 	DS		1					;TRANSMIT DATA FLAG
; 									; (0 = BUFFER EMPTY. FF = BUFFER FULL)
; OutINTExpect:	DS		1					;OUTPUT INTERRUPT EXPECTED
; 									; (0 = NO INTERRUPT EXPECTED,
; 									; FF = INTERRUPT EXPECTED)



; 		; SAMPLE EXECUTION:


; 		;CHARACTER EQUATES
; ESCAPE		EQU		1BH                ;ASCII ESCAPE CHARACTER
; TESTCH		EQU		'A'             ;TEST CHARACTER = A
; SC11A:
; 		CALL	INIT_11A            ; INITIALIZE DART. INTERRUPT SYSTEM
; 			;SIMPLE EXAMPLE - READ AND ECHO CHARACTERS
; 			;UNTIL AN ESC IS RECEIVED
; LOOP_11A:
; 		CALL	INCH_11A			; READ CHARACTER
; 		PUSH	AF
; 		CALL	OUTCH_11A			; ECHO CHARACTER
; 		POP		AF
; 		CP		ESCAPE			;IS CHARACTER AN ESCAPE?
; 		JR		NZ,LOOP_11A			;STAY IN LOOP IF NOT
; 			;AN ASYNCHRONOUS EXAMPLE
; 			; OUTPUT "An TO CONSOLE CONTINUOUSLY. BUT ALSO LOOK AT
; 			; INPUT SIDE. READING AND ECHOING ANY INPUT CHARACTERS
; ASYNLP_11A:
; 			;OUTPUT    AN "A" IF OUTPUT IS NOT BUSY
; 		CALL	OUTST_11A			;IS OUTPUT BUSY?
; 		JR		C,ASYNLP_11A        ;JUMP IF IT IS
; 		LD		A,TESTCH
; 		CALL	OUTCH_11A			;OUTPUT TEST CHARACTER
; 			; CHECK INPUT PORT
; 			; ECHO CHARACTER IF ONE IS AVAILABLE
; 			; EXIT ON ESCAPE CHARACTER
; 		CALL	INST_11A			; IS INPUT DATA AVAILABLE?
; 		JR		NC,ASYNLP_11A		;JUMP IF NOT (SEND ANOTHER "A")
; 		CALL	INCH_11A			;GET CHARACTER
; 		CP		ESCAPE			;IS IT AN ESCAPE?
; 		JR		Z,DONE_11A			;BRANCH IF IT IS
; 		CALL	OUTCH_11A			;ELSE ECHO CHARACTER
; 		JP		ASYNLP_11A			;AND CONTINUE
; DONE_11A:
; 		JP     LOOP_11A



					;***********************************************************************************************************************
					;***********************************************************************************************************************
					; Unbuffered Input/Output
					; 													5. INIT: none
					; 		Title                Simple interrupt input and output usinQ a                zao
					; 							PIa and single character buffers
					; 		Name:                PINTIO
					; 	Purpose:        This program consists of 5 subroutines which
					; 					perform interrupt driven input and output using
					; 					a Z80 PIO.
					; 					INCH
					; 						Read a character
					; 					INST
					; 						Determine input status (whether input
					; 						buffer is empty)
					; 					OUTCH
					; 						Write a character
					; 					OUTST
					; 						Determine output status (whether output
					; 						buffer is full)
					; 					INIT
					; 						Initialize PIO and interrupt system
					; 	Entry:          INCH
					; 						No parameters
					; 					INST
					; 						No parameters
					; 					OUTCH
					; 						Register A = character to transmit
					; 					OUTST
					; 						No parameters
					; 					INIT
					; 						No parameters
					; 	Exit :          INCH
					; 						Register A = character
					; 					INST
					; 						Carry = 0 if input buffer is empty,
					; 						1 if character is available
					; 					OUTCH
					; 						No parameters
					; 					OUTST
					; 						Carry = 0 if output buffer is not
					; 						full, 1 if it is full
					; 					INIT
					; 						No parameters
					; 	Registers used: INCH
					; 						A.F
					; 					INST
					; 						A,F
					; 					OUTCH
					; 						A,F
					; 					OUTST
					; 						A,F
					; 					INIT
					; 						A,F,eC,HL,I
					; 		Time:               INCH
					; 							72 cycles if a character is available
					; 							INST
					; 							27 cycles
					; 							OUTCH
					; 							150 cycles if output buffer is not full
					; 								and output interrupt is expected
					; 							OUTST
					; 							27 cycles
					; 							INIT
					; 							377 cycles
					; 							RDHDLR
					; 							82 cycles
					; 							WRHDLR
					; 							178 cycles
					; 		Size:               Program 166 bytes
					; 							Data      5 bytes
					; 		;PIO EQUATES
					; 		; PIO IS PROGRAMMED FOR:
					; 			PORT A INPUT
					; 			PORT B OUTPUT
					; 		;ARBITRARY PIO PORT ADDRESSES
					;***********************************************************************************************************************
					;***********************************************************************************************************************


; PIOAD    EQU      90H             ;PORT A DATA
; PIOAC    EQU      91H             ;PORT A CONTROL
; PIOBD    EQU      92H             ;PORT B DATA
; PIOBC    EQU      93H             :PORT B CONTROL
; INTRPV   EQU      8000H           ;BASE OF INTERRUPT VECTORS
; PIOIVA   EQU      INTRPV          ; INTERRUPT VECTOR FOR PORT A
; PI0IVB   EQU      INTRPV+2        ; INTERRUPT VECTOR FOR PORT B
; 		:READ CHARACTER
; INCH:
; 		CALL      INST              ~GET INPUT STATUS
; 		JR        NC,INCH           ;WAIT IF NO CHARACTER AVAILABLE
; 		Dl                          ;DISABLE INTERRUPTS
; 		SUB       A
; 		LD        (RECDF),A         ;INDICATE INPUT BUFFER EMPTY
; 		LD        A. (RECDAT)       :OET CHARACTER FROM INPUT BUFFER
; 		El                          ;REENABLE INTERRUPTS
; 		RET
; 		~RETURN   INPUT STATUS (CARRY    =1   IF INPUT DATA IS AVAILABLE)
; INST:
; 		LD        A, (RECDF)        ;GET DATA READY FLAG
; 		RRA                         ;SET CARRY FROM DATA READY FLAG
; 									: IF CARRY = 1, CHARACTER IS AVAILABLE
; 		RET
; 		; WRITE CHARACTER
; OUTCH:
; 		PUSH      AF                :SAVE CHARACTER TO WRITE


; 		;WAIT FOR CHARACTER BUFFER TO EMPTY, THEN STORE NEXT CHARACTER
; WAITOC:
; 		CALL        OUTST           ;GET OUTPUT STATUS
; 		JR          C, WAITOC       ;WAIT IF OUTPUT BUFFER IS FULL
; 		01                          :DISABLE INTERRUPTS WHILE LOOKING AT
; 									~ SOFTWARE FLAGS
; 		POP         AF              :GET CHARACTER
; 		LD          <TRNDAT> .A     :STORE CHARACTER IN OUTPUT BUFFER
; 		LD          A.OFFH          : INDICATE OUTPUT BUFFER FULL
; 		LD          <TRNDF)'A
; 		LD          A,(OutINTExpect)         :TEST OUTPUT INTERRUPT EXPECTED FLAG
; 		OR          A
; 		CALL        Z.OUTDAT        :OUTPUT CHARACTER IMMEDIATELY IF
; 									: NO OUTPUT INTERRUPT EXPECTED
; 		EI                          :ENABLE INTERRUPTS
; 		RET
; 		,OUTPUT STATUS (CARRY = 1 IF OUTPUT BUFFER IS FULL)
; OUTST:
; 		LD          A. <TRNDF)      ;GET TRANSMIT FLAG
; 		RRA                         ~SET CARRY FROM TRANSMIT FLAG
; 		RET                         ; CARRY = 1 IF OUTPUT BUFFER FULL
; 		: INITIALIZE PIO AND INTERRUPT SYSTEM
; INIT:
; 		DI                          :DISABLE INTERRUPTS
; 		;INITIALIZE SOFTWARE FLAGS
; 		SUB     A
; 		LD      (RECDF>.A       ;NO INPUT DATA AVAILABLE
; 		LD      (TRNDF>.A       :OUTPUT BUFFER EMPTY
; 		LD      (OutINTExpect>.A         :NO OUTPUT INTERRUPT EXPECTED
; 								: DEVICE IS READY INITIALLY
; 		~INITIALIZE      INTERRUPT VECTORS
; 		LD          A.INTRPV SHR 8 :GET HIGH BYTE OF INTERRUPT PAGE
; 		LD          I.A              ;SET INTERRUPT VECTOR IN zao
; 		1M          2                ; INTERRUPT MODE 2 - VECTORS IN TABLE
; 									~ ON INTERRUPT PAGE
; 		LD          HL.RDHDLR        ,STORE READ VECTOR (INPUT INTERRUPT)
; 		LD          (PIOIVA).HL
; 		LD          HL.WRHDLR        ,STORE WRITE VECTOR (OUTPUT INTERRUPT)
; 		LD          (PIOIVB).HL
; 		,INITIALIZE PIO
; 		LD      HL.PIOINT           :BASE ADDRESS OF INITIALIZATION ARRAY
; 		CALL    IPORTS              ; INITIALIZE PIO
; 		EI                          , ENABLE INTERRUPTS
; 		RET
; 		;INPUT (READ) INTERRUPT HANDLER
; RDHDLR:
; 		PUSH        AF
; 		IN          A.(PIOAD)       :READ DATA FROM PIO
; 		LD          (RECDAT> .A     ,SAVE DATA IN INPUT BUFFER
; 		LD     A,OFFH           ;INDICATE INPUT DATA AVAILABLE
; 		LD     (RECDF),A
; 		POP    AF
; 		EI                      ;REENABLE INTERRUPTS
; 		RETI
; 		;OUTPUT (WRITE) INTERRUPT HANDLER
; WRHDLR:
; 		PUSH    AF
; 		LD      A, <TRNDF)      ;GET DATA AVAILABLE FLAG
; 		RRA
; 		JR     NC,NODATA        ;JUMP IF NO DATA TO TRANSMIT
; 		CALL   OUTDAT           ;OUTPUT DATA TO PIO
; 		JR     WRDONE
; 		;IF AN OUTPUT INTERRUPT OCCURS WHEN NO DATA IS AVAILABLE,
; 			WE MUST DISABLE IT TO AVOID AN ENDLESS LOOP. LATER, WHEN A
; 			CHARACTER BECOMES AVAILABLE. WE NEED TO KNOW THAT AN OUTPUT
; 			INTERRUPT HAS OCCURRED WITHOUT BEING SERVICED. THE KEY HERE
; 		; IS THE OUTPUT INTERRUPT EXPECTED FLAG OlE. THIS FLAG IS
; 			CLEARED WHEN AN OUTPUT INTERRUPT HAS OCCURRED BUT HAS NOT
; 			BEEN SERVICED. IT IS ALSO CLEARED INITIALLY SINCE THE
; 			OUTPUT DEVICE IS ASSUMED TO START OUT READY. OlE IS SET
; 		, WHENEVER DATA IS ACTUALLY SENT TO THE PIO. THUS THE OUTPUT ROUTINE
; 			OUTCH CAN CHECK OlE TO DETERMINE WHETHER TO SEND THE DATA
; 		; IMMEDIATELY OR WAIT FOR AN OUTPUT INTERRUPT.
; 		;THE PROBLEM IS THAT AN OUTPUT DEVICE MAY REQUEST SERVICE BEFORE
; 			THE COMPUTER HAS ANYTHING TO SEND (UNLIKE AN INPUT DEVICE
; 			THAT HAS DATA WHEN IT REQUESTS SERVICE). THE OlE FLAG SOLVES
; 		, THE PROBLEM OF AN UNSERVICED OUTPUT INTERRUPT ASSERTING ITSELF
; 			REPEATEDLY, WHILE STILL ENSURING THE RECOGNITION OF
; 			OUTPUT INTERRUPTS.
; NODATA:
; 		SUB     A
; 		LD      (OlE) .A        ; INDICATE NO OUTPUT INTERRUPT EXPECTED
; 		LD      A.OOOOOOllB     ,DISABLE OUTPUT INTERRUPTS
; 		OUT     (PIOBC).A
; WRDONE:
; 		POP     AF              ;RESTORE REGISTERS
; 		EI
; 		RETI

; 		'*************************************
; 		; ROUTINE: OUTDAT
; 		; PURPOSE: SEND CHARACTER TO PIO PORT B
; 		; ENTRY: TRNDAT = CHARACTER
; 		,EXIT: NONE
; 		;REGISTERS USED: AF
; 		;***************************************
; OUTDAT:
; 		LD      A. <TRNDAT>     ;GET DATA FROM OUTPUT BUFFER
; 		OUT     (PIOBD).A       ,SEND DATA TO PIO
; 		SUB     A               ; INDICATE OUTPUT BUFFER EMPTY


; 		LD       (TRNDF),A
; 		DEC      A                 ;INDICATE OUTPUT INTERRUPT EXPECTED
; 		LD       (OutINTExpect),A           ; OlE = FF HEX
; 		LD       A,10000011B       ;ENABLE OUTPUT INTERRUPTS
; 		OUT      (PIOBC).A
; 		RET
; 		;**************************************
; 		; ROUTINE: IPORTS
; 		; PURPOSE: INITIALIZE I/O PORTS
; 		; ENTRY: HL = BASE ADDRESS OF INITIALIZATION ARRAY
; 		:EXIT: DATA OUTPUT TO PORTS
; 		;REGISTERS USED: AF,BC,HL
; 		;************************************
; IPORTS:
; 		;GET NUMBER OF DATA BYTES TO SEND TO CURRENT PORT
; 		;EXIT IF NUMBER OF BYTES IS O. INDICATING TERMINATOR
; 		LD      A, (HL)          ; GET NUMBER OF BYTES
; 		OR      A               ;TEST FOR ZERO (TERMINATOR)
; 		RET     Z               :RETURN IF NUMBER OF BYTES = 0
; 		LD      B,A
; 		INC     HL              ;POINT TO PORT ADDRESS (NEXT BYTE)
; 		:C = PORT ADDRESS
; 		;HL = BASE ADDRESS OF OUTPUT DATA
; 		LD      C.(HL)          :GET PORT ADDRESS
; 		INC     HL              ;POINT TO FIRST DATA VALUE (NEXT BYTE)
; 		;OUTPUT DATA AND CONTINUE TO NEXT PORT
; 		OTIR                    ;SEND DATA VALUES TO PORT
; 		JR      IPORTS          tCONTINUE TO NEXT PORT ENTRY
; 		;PIO INITIALIZATION DATA
; 		t PORT A = INPUT
; 		; PORT B = OUTPUT
; PIOINT:
; 		DB       3               ;OUTPUT 3 BYTES
; 		DB       PIOAC           tDESTINATION IS PORT A CONTROL
; 		DB       PIOIVA AND OFFH ;SET INTERRUPT VECTOR FOR PORT A
; 		DB       10001111B       ;BITS 3,2,1,0 = 1111 (MODE SELECT)
; 								; BITS 5,4 = 00 (DON"T CARE)
; 								;BITS 7,6 = 01 (INPUT MODE)
; 		DB       10000111B       ;BITS 3.2.1.0 = 0111 (INTERRUPT CONTROL)
; 								:BITS 6.5.4 = 000 (DON'T CARE)
; 								;BITS 7 = 1 (ENABLE INTERRUPTS)
; 		DB       3               :OUTPUT 3 BYTES
; 		DB       PIOBC           tDESTINATION IS PORT B CONTROL
; 		DB       PIOIVB AND OFFH ;SET INTERRUPT VECTOR FOR PORT B
; 		DB       11001111B       ;BITS 3,2,1,0 = 1111 (MODE SELECT)
; 								;BITS 5,4 = 00 (DON'T CARE)
; 								;BITS 7.6 = 00 (CONTROL MODE)
; 		DB       00000111B       :BITS 3.2.1.0 = 0111 (INTERRUPT CONTROL)
; 								:BIT 4,5,6 = 000 (DON'T CARE)
; 								;BITS 7 = 0 (DISABLE INTERRUPTS)

; 		DB        0               ;TERMINATOR FOR INITIALIZATION ARRAY
; 		; DATA SECTION
; RECDAT: DS       1                  ;RECEIVE DATA
; RECDF: DS        1                  ;RECEIVE DATA FLAG
; 									; (0 = NO DATA. FF      = DATA)
; TRNDAT: DS          1               ;TRANSMIT DATA
; TRNDF: DS           1               ;TRANSMIT DATA FLAG
; 									; (0 = BUFFER EMPTY. FF = BUFFER FULL)
; OlE:      DS        1               ;OUTPUT INTERRUPT EXPECTED
; 									; (0 = NO INTERRUPT EXPECTED,
; 									; FF = INTERRUPT EXPECTED)



; 		SAMPLE EXECUTION:


; 		;CHARACTER EQUATES
; ESCAPE    EQU     1BH               ; ASCI I ESCAPE CHARACTER
; TESTCH    EQU     ~A~               ;TEST CHARACTER = A
; SCllB:
; 		CALL      INIT            ; INITIALIZE PIO, INTERRUPT SYSTEM
; 		;SIMPLE EXAMPLE - READ AND ECHO CHARACTERS
; 		: UNTIL AN ESC IS RECEIVED
; LOOP:
; 		CALL      INCH            ;READ CHARACTER
; 		PUSH      AF
; 		CALL      OUTCH           ;ECHO CHARACTER
; 		POP       AF
; 		CP        ESCAPE          ;IS CHARACTER AN ESCAPE?
; 		•.JR      NZ,LOOP         ;STAY IN LOOP IF NOT
; 		;AN ASYNCHRONOUS EXAMPLE
; 		; OUTPUT "A" TO CONSOLE CONTINUOUSLY. BUT ALSO LOOK AT
; 		; INPUT SIDE, READING AND ECHOING ANY INPUT CHARACTERS
; ASYNLP:
; 		;OUTPUT   AN "A" IF OUTPUT IS NOT BUSY
; 		CALL      OUTST           ;IS OUTPUT BUSY?
; 		JR        C,ASYNLP        ;JUMP IF IT IS
; 		LD        A,TESTCH
; 		CALL      OUTCH           ;OUTPUT TEST CHARACTER
; 		;CHECK INPUT PORT
; 		;ECHO CHARACTER IF ONE IS AVAILABLE
; 		,EXIT ON ESCAPE CHARACTER
; 		CALL    INST            ;IS INPUT DATA AVAILABLE?
; 		JR      NC.ASYNLP       ;JUMP IF NOT (SEND ANOTHER "A")
; 		CALL    INCH            ;GET THE CHARACTER
; 		CP      ESCAPE          ;IS IT AN ESCAPE CHARACTER?


; 		JR       Z.ASDONE   I JUMP IF IT IS
; 		CALL     OUTCH      ;ELSE ECHO CHARACTER
; 		JP       ASVNLP     ;AND CONTINUE
; ASDONE:
; 		JP       LOOP





			;****************************************************************************************************************
			;****************************************************************************************************************
			; Buffered Input/Output
			; Using an DART (SINTB)                                                                                    11C
			; 		Title              Interrupt input and output using a ZSO DART and
			; 						multiple-character buffers
			; 		Name:              SINTB
			; 		Purpose:           This program consists of 5 subroutines which
			; 						perform interrupt driven input and output using
			; 						a ZSO DART.
			; 						ReadChar
			; 							Read a character
			; 						RetInpStatus
			; 							Determine input status (whether input
			; 							buffer is empty)
			; 						WriteChar
			; 							Write a character
			; 						RetOutStatus
			; 							Determine output status (whether output
			; 							buffer is full)
			; 						InitBuffers
			; 							Initialize DART and interrupt system
			; 		Entry:             ReadChar
			; 							No parameters
			; 						RetInpStatus
			; 							No pat'ameters
			; 						WriteChar
			; 							Register A = character to transmit
			; 						RetOutStatus
			; 							No parameters
			; 						InitBuffers
			; 							No parameters
			; 		Exit :          ReadChar(INCH)
			; 							Register A = character
			; 						RetInpStatus(INST)
			; 							Carry = 0 if input buffer is empty,
			; 							1 if character is available
			; 						WriteChar(OUTCH)
			; 							No parameters
			; 						OUTST
			; 							Carry = 0 if output buffer is not
			; 							full. 1 if it is full
			; 						InitBuffers
			; 							No parameters
			; 		Reqisters used: ReadChar
			; 							AF,C,DE,HL
			; 						RetInpStatus
			; 							AF
			; 						WriteChar
			; 							AF,DE,HL
			; 						RetOutStatus
			; 							AF
			; 						InitBuffers
			; 							AF,BC,HL,I
			; 		Time:           ReadChar
			; 							Approximately 197 cycles if a character is
			; 							available
			; 						RetInpStatus
			; 							39 cycles
			; 						WriteChar
			; 							Approximately 240 cycles if output buffer
			; 							is not full and output interrupt is expected
			; 						RetOutStatus
			; 							34 cycles
			; 						InitBuffers
			; 							732 cycles
			; 						ReadINTHandler
			; 							Approximately 249 cycles
			; 						WriteINTHandler
			; 							Approximately 308 cycles
			; 		Size:           Program 299 bytes
			; 						Data     11 bytes plus size of buffers
			; 		:DART EQUATES
			; 			DART IS PROGRAMMED FOR:
			; 			ASYNCHRONOUS OPERATION
			; 			16 X BAUD RATE
			; 			8-BIT CHARACTERS
			; 		; 1 1/2 STOP BITS
			;****************************************************************************************************************
			;****************************************************************************************************************

		; Section IOLIB


		GLOBAL 	InitBuffers,ReadChar,WriteChar, WriteLine, WriteLineCRNL, ReadLine, CRLF, puts_crlf,cleanInBuffer,cleanOutBuffer
		GLOBAL	S_head_tail, inBufferEnd, inBuffer, writeSTRBelow, writeSTRBelow_CRLF,waitForKey
		GLOBAL	PIO_Init,CTC_Init,DART_Init,InitInterrupt


			;ARBITRARY DART PORT ADDRESSES
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
R_LOOP:
		call	ReadChar				;read character
		push	AF
		call	WriteChar				;echo character
		pop		AF
		cp		CRChar					;is character an cr?
		jr		NZ,R_LOOP				;stay in loop if not

		call	CRNL
			; copy from inbuf to cursor buffer...
waitEntry:
		call	S_head_tail			; save input heads and tails


		ld		hl,(Comm_Ptr_list)
		ld 		de,(Comm_Ptr_list+2)		; next item in list
		or 		a				; clear carry
		SBC		hl,de 			; number of chars in string (in L)
		ld 		b,l				; store in B

			; detect wraparound (hl)<(de)
		jp 		P,cont2			;positive result 	(hl)>(de)
		ld 		a,bufferSize 			; length of input buffer
		add		a,b
		ld		b,a				; correct count in B when wraparound
cont2:
		ld	 	ix,Textbuf				; get address of text buffer
		ld		(ix),B		; save length to start of str.
		inc 	ix			; resulting  string adr, skip byte with length

		ld		hl,(Comm_Ptr_list+2)		; first string start

		ld		DE,inBufferEnd
c_nextchar:
			;	copy from inbuffer to Textbuf...
		ld 		a,(hl)				; char from inbuffer
		ld		(ix),a			; save char in textbuf
		inc		hl
		inc 	ix

			; check if upper buffer adr 
		or 		A		; clear carry
		push 	hl	
		SBC		hl,de		; S and Z set  hl=de -> inBufferEnd encountered
		pop 	hl
		jr 		NZ, cont1			; continue
			; turnaround (HL) hl = inBuffer.	
		ld 		hl,inBuffer			; hl = start of input buf
cont1:
		djnz 	c_nextchar		; count no chars.
		xor 	a				; clear a
		ld 		(ix),A			; end with 00H
		ld		hl,Textbuf
		ld		a,(hl)			; A = string length (num)
		inc		hl				; HL = addr to first char in string
		ret 					; string stored in textbuffer

ReadChar:
		CALL	RetInpStatus	;get input status. return. carry = 1 if data available
		JR		NC,ReadChar		;wait if no character available
		DI                      ;disable interrupts
		LD		HL,inBufCount			;reduce input buffer count by 1
		DEC		(HL)
		LD		HL, (inHeadAdr)		;Get   character from head of input buffer
		LD		C, (HL)
		CALL	incInPointer			;Move head pointer up 1
		LD		(inHeadAdr) ,HL
		LD		A,C
		EI						;Reenable interrupts
		RET
			;return input status (carry    =1   if input data is available)
RetInpStatus:
		LD		A, (inBufCount)		;Test input buffer count
		OR		A				;Clear carry always
		RET		Z				; Return, carry = 0 if no data
		SCF                     ;Set carry
		RET                     ; Return. carry = 1 if data available


			; Save copies of inHeadAdr and inTailAdr to memory...
S_head_tail:

		ld 		ix,Comm_Ptr_list+2
		ld 		b,list_len-2
bmve:
		ld 		a,(ix)
		ld 		(ix+2),A
		dec		ix
		djnz 	bmve		; shift data upwards...

		LD		HL, (inHeadAdr)		;GET   CHARACTER FROM HEAD OF INPUT BUFFER
		ld 		(Comm_Ptr_list),HL
		ret
			; Write line from address in iy (until char = 00)
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
		; djnz	nxtchr
		jr 		nxtchr
		ret					; return on maxlength
			; WriteLine from address (iy) (until char = 00)and add CRLF 
WriteLineCRNL:
		call	WriteLine
			; Entry for excl. output CRNL
CRNL:
CRLF:
		call	puts_crlf
		ret

			;Write character
WriteChar:
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
		call	Z,CharToDart			; output character immediately if
										; output interrupt not expected
		pop 	HL		
		pop 	DE				
		ei						;reenable interrupts
		ret
			;output status (carry=1 if buffer is full)
GetOutStatus:
		ld		A, (OutBufCount)		; get current output buffer count
		cp		outBufferSize			; compare to maximum
		ccf						; complement carry
		ret						; carry = 1 if buffer full, 0 if not
			; INITIALIZE DART, Interrupt system
InitBuffers:
			; initialize buffer counters and pointers.
		sub		A
		ld		(OutINTExpect),A	; indicate no output interruptS
		ld		(inBufCount),A		; buffer counters = 0
		ld		(OutBufCount),A
		call	cleanInBuffer
		call	cleanOutBuffer
		call 	InitInterrupt		; init interrupt vectors
		ret

cleanInBuffer:
		sub		A
		ld		(inBufCount),A		; buffer counters = 0
		ld 		(inbufferDeactivate),A  ; clear flag for input buffer update...
		ld		HL,inBuffer			; all buffer pointers = base address
		ld		(inHeadAdr),HL
		ld		(inTailAdr),HL
		ret
cleanOutBuffer:
		sub		A
		ld		(OutBufCount),A
		ld		HL,outBuffer
		ld		(outHeadAdr),HL
		ld		(outTailAdr),HL
		ret
InitInterrupt:
			;INITIALIZE INTERRUPT VECTORS (DART)
			; initialize . interrupt flag
		ld		A,DART_Int_Vec>>8		;GET HIGH BYTE OF INTERRUPT PAGE
		ld		I,A             ;SET INTERRUPT VECTOR IN zao
		im		2               ; INTERRUPT MODE 2 - VECTORS IN TABLE
		ld		HL,ReadINTHandler       ; ON INTERRUPT PAGE
		ld		(DART_Int_Read_Vec),HL		;STORE READ VECTOR
		ld		HL,WriteINTHandler
		ld		(DART_Int_WR_Vec),HL		;STORE WRITE VECTOR
		ld		HL,ExternINTHandler
		ld		(DART_Int_EXT_Vec),HL		;STORE EXTERNAL/STATUS VECTOR
		ld		HL,SpecINTHandler
		ld		(DART_Int_Spec_Vec),HL		;STORE SPECIAL RECEIVE VECTOR

				; INT Vectors  for the CTC 
		ld		HL,CTC_CH0_Interrupt_Handler
		ld		(CTC_CH0_I_Vector),HL		;STORE CTC channel 0 VECTOR
		ld		HL,CTC_CH1_Interrupt_Handler
		ld		(CTC_CH1_I_Vector),HL		;STORE CTC channel 1 VECTOR
		ld		HL,CTC_CH2_Interrupt_Handler
		ld		(CTC_CH2_I_Vector),HL		;STORE CTC channel 2 VECTOR
		ld		HL,CTC_CH3_Interrupt_Handler
		ld		(CTC_CH3_I_Vector),HL		;STORE CTC channel 3 VECTOR



		ret
DART_Init:		
		;INITIALIZE I/O PORTS
		ld      HL,DARTINT		;BASE ADDRESS OF INITIALIZATION ARRAY
		call    InitDARTPorts			; INITIALIZE DART
		ei						; ENABLE INTERRUPTS
		ret
			;INPUT (READ) INTERRUPT HANDLER
ReadINTHandler:
		push	AF				;SAVE REGISTERS
		push	BC
		push	DE
		push   	HL


		in		A,(DART_A_D)		; read data from dart
		ld		C,A					; save data in register c
		ld 		a,(inbufferDeactivate)
		cp 		00 					; =0 		
		jr 		nz,exitRHandler

		ld		HL,inBufCount		; any room in input buffer?
		ld		A, (HL)
		cp		bufferSize
		jr		NC,exitRHandler		; jump if no room
		inc		(HL)				; increment input buffer counter
		ld		HL, (inTailAdr)		; store character at tail of input buffer
		ld		(HL),C
		call	incInPointer		; increment tail pointer
		ld		(inTailAdr), HL
exitRHandler:
		pop		HL				;restore registers
		pop		DE
		pop		BC
		pop		AF
		ei						;reenable interrupts
		reti
			;output (write) interrupt handler
WriteINTHandler:
		push	AF					;save registers
		push	BC
		push	DE
		push	HL
		ld		A, (OutBufCount)			;get output buffer counter
		or		A					;test for empty buffer
		jr		Z,nodata			;jump if no data to transmit
		call	CharToDart				;else output data
		jr		wrdone
			;if an output interrupt occurs when no data is available.
			; we must disable output interrupts to avoid an endless loop.
			; when the next character is ready, it must be sent immediately
			; since no interrupt will occur. this state in which an output
			; interrupt has occurred but has not been serviced is indicated
			; by clearing ole (output interrupt expected flag).
nodata:
		sub		A
		ld		(OutINTExpect),a				;00 not expect an interrupt
		out		(DART_A_C),a			;select register 0
		ld		a,00101000b			;reset transmitter interrupt
		out		(DART_A_C),a
wrdone:
		pop		HL					;restore registers
		pop		DE
		pop		BC
		pop		AF
		ei
		reti
			;external/status changed interrupt handler
ExternINTHandler:
		push	AF
		ld		A,00010000b			;reset status interrupt

		out		(DART_A_C),a
		pop		AF
		ei							; dcd or cts line changed state. or a
		reti						; break was detected
									; service here if necessary
			;special receive error interrupt
SpecINTHandler:
		push	AF
		ld		A,00110000b			;reset receive error interrupt
		out		(DART_A_C),a
		pop		AF
		ei							;framing error or overrun error occurred
		reti						; service here if necessary

		;*************************************
		; 	routine: chartodart
		; 	purpose: send character to dart
		; 	entry: none
		;	exit: none
		;	registers used: af.de.hl
		;***************************************
CharToDart:
		ld		HL,(outHeadAdr)
		ld		A, (HL)					;get data from head of output buffer
		out		(DART_A_D),A			;output data
		call	incOutPointer			; increment head pointer
		ld		(outHeadAdr),HL
		ld		HL,OutBufCount			;decrement output buffer count
		dec		(HL)
		ld		a,0ffh
		ld		(OutINTExpect),a		;expect an output interrupt
		ret


		;*************************************
		; routine: incinpointer
		; purpose: increment pointer into input
		;			buffer with wraparound
		; entry: hl = pointer
		;exit: hl = pointer incremented with wraparound
		;registers used: af.de.hl
		;***************************************
incInPointer:
		INC		HL					; increment pointer
		LD		DE,inBufferEnd			;compare pointer. end of buffer
		LD		A,L
		CP		E
		RET		NZ
		LD		A,H
		CP		D
		RET		NZ					;return if not equal
		LD		HL,inBuffer				;if pointer at end of buffer.
		RET                   	  ; set it back to base address
		
		
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
		; routine: initdartports
		; purpose: initialize i/o ports
		; entry: hl = base address of initialization array
		;exit: data output to ports
		;registers used: af.bc.hl
		;************************************
InitDARTPorts:
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
		jr      InitDARTPorts			;continue to next port entry
		;DART initialization data
DARTINT:
		; Reset channel a
		db	1					;output 1 byte
		db	DART_A_C				;to channel a command/status
		db	_Ch_Reset			;select write register 0
								;bits 2.1.0    0 (write register 0)
								;bits 5,4,3 = 011 (channel reset)
								;bits 7,6 = 0 (do not care)


		;sET INTERRUPT VECTOR AND ALLOW STATUS TO AFFECT IT
		db	4					;OUTPUT 2 BYTES
		db	DART_B_C			;DESTINATION IS COMMAND REGISTER B
		db	02					;SELECT WRITE REGISTER 2
		db	DART_Int_Vec&0FFH	;SET INTERRUPT VECTOR FOR DART

		db	01					;SELECT WRITE REGISTER 1
		db	_Status_Vector		;TURN ON STATUS AFFECTS VECTOR

		; INITIALIZE CHANNEL A
		db	8					;OUTPUT 8 BYTES
		db	DART_A_C			;DESTINATION IS COMMAND REGISTER A

		;iNITIALIZE BAUD RATE CONTROL
		db	_Reset_STAT_INT|4	;SELECT WRITE REGISTER 4 & RESET EXTERNAL/STATUS INTERRUPT
		db	_Stop_1_bit|_X32_Clock_mode
								;BIT 0 = 0 (NO PARITY)
								;BIT 1 = 0 (DON'T CARE)
								;BITS 3,2 = 01 (1 1/2 STOP BITS)
								; BITS 5.4 = 00 (DON-'T CARE)
								;BITS 7.6 = 10 (32 TIMES CLOCK)
	
		; INITIALIZE RECEIVE CONTROL
		db	03		;SELECT WRITE REGISTER 3
		db	_Rx_Enable|_RX_8_bits|_Auto_Enable
								;BIT 0 = 1 (RECEIVE ENABLE)
								; BITS 4,3,2,1 = 0 (DON-'T CARE)
								;BIT 5 = 0 (NO AUTO ENABLE)
								;BIT 7.6 = 11 (RECEIVE 8 BITS/CHAR)
		;iNITIALIZE TRANSMIT CONTROL
		db	05					;SELECT WRITE REGISTER 5
		db	_RTS_Enable|_Tx_Enable|_Tx_8bits_char
								;BIT 0 = 0 (NO CRC ON TRANSMIT)
								;BIT 1 = 1 (REQUEST TO SEND)
								;BIT 2 = 0 (DON'T CARE)
								;BIT 3 = 1 (TRANSMIT ENABLE)
								;BIT 4 = 0 (DO NOT SEND BREAK)
								;BITS 6.5 = 11 (TRANSMIT 8 BITS/CHAR)
								;BIT 7 = 1 (DATA TERMINAL READY)
		DB	01					;SELECT WRITE REGISTER 1
		DB	_Tx_INT_EN|_Int_All_Rx_Char_NP
		; DB	_Ext_INT_EN|_Tx_INT_EN|_Int_All_Rx_Char_NP|_WAIT_READY_R_T|_WAIT_READY_EN
								;BIT 0 = 1 (EXTERNAL INTERRUPTS)
								;BIT 1 = 1 (ENABLE TRANSMIT INTERRUPT)
								;BIT 2 = 0 (DO NOT CARE)
								;BITS 4. 3 = 11 (RECEIVE INTERRUPTS ON ALL CHARS. PARITY DOES NOT AFFECT VECTOR)
								;BITS 7.6.5 = 000 (NO WAIT/READY
								; FUNCTION)
		DB	0               ; END OF TABLE
		; DATA SECTION
inHeadAdr:	DS	2					; address of oldest character in input buffer
inTailAdr:	DS	2					; address of newest character in input buffer
inBufCount:	DS	1					;number of characters in input buffer 
outHeadAdr:	DS	2					;address of oldest character in output buffer
outTailAdr:	DS	2					;address of newest character in output buffer
OutBufCount:	DS	1				;number of characters in output buffer
OutINTExpect:	DS	1					;output interrupt expected
								; (0 = no interrupt expected.
								; ff = interrupt expected)


		; SAMPLE EXECUTION:


		;CHARACTER EQUATES
; ; ESCAPE	EQU     1BH					;ASCII ESCAPE CHARACTER
; ; TESTCH	EQU     'A'					;TEST CHARACTER = A
; 		global SC11C
; SC11C:
; 		CALL	InitBuffers				;INITIALIZE DART. INTERRUPT SYSTEM
; 			;SIMPLE EXAMPLE - READ AND ECHO CHARACTER
; 			; UNTIL AN ESC IS RECEIVED
; LOOP:
; 		CALL	ReadChar				;READ CHARACTER
; 		PUSH	AF
; 		CALL	WriteChar				;ECHO CHARACTER
; 		POP		AF
; 		CP		ESCAPE				;IS CHARACTER AN ESCAPE?
; 		JR		NZ,LOOP				;STAY IN LOOP IF NOT
; 			;AN ASYNCHRONOUS EXAMPLE
; 			; OUTPUT "A" TO CONSOLE CONTINUOUSLY BUT ALSO LOOK AT
; 			; INPUT SIDE. READING AND ECHOING ANY INPUT CHARACTERS
; ASYNLP:
; 			;OUTPUT   AN "A" IF OUTPUT IS NOT BUSY
; 		CALL	RetOutStatus				;IS OUTPUT BUSY?
; 		JR		C,ASYNLP			;JUMP IF IT IS
; 		LD		A,TESTCH
; 		CALL	WriteChar				;OUTPUT CHARACTER
; 			;CHECK INPUT PORT
; 			;ECHO CHARACTER IF ONE IS AVAILABLE
; 			;EXIT ON ESCAPE CHARACTER
; 		CALL	RetInpStatus				;IS INPUT DATA AVAILABLE?
; 		JR		NC, ASYNLP			;JUMP I F NOT (SEND ANOTHER .. A" )
; 		CALL	ReadChar				; GET CHARACTER
; 		CP		ESCAPE				;IS IT AN ESCAPE CHARACTER?
; 		JR		Z,DONE				;BRANCH IF IT IS


; 		CALL	WriteChar				;ELSE ECHO CHARACTER
; 		JP		ASYNLP				;AND CONTINUE
; DONE:
; 		JP		LOOP


;##############################################################
; Write the null-terminated string starting after the call
; instruction invoking this subroutine to the console.
; Clobbers AF, C
;##############################################################
writeSTRBelow:
        ex      (sp),iy                 ; iy = @ of string to print
		call	WriteLine
        inc     iy                      ; point past the end of the string
        ex      (sp),iy
        ret

writeSTRBelow_CRLF:
       ex      (sp),iy                 ; iy = @ of string to print
		call	WriteLineCRNL
        inc     iy                      ; point past the end of the string
        ex      (sp),iy
        ret

;##############################################################
; Print a CRLF 
; Clobbers AF, C
;##############################################################
puts_crlf:
        call    writeSTRBelow
        defb    '\0\r\n\0'
        ret




PIO_Init:
;----------******************* PIO PORT A
		ld A, %11001111                 ;mode 3 in/out
		out (portA_Contr), A         ; set port A as output
		ld 	A,%00000000					; msb=input lsb = output, 0-mosi, 1-clk, 2-ssel, 7-miso
		out (portA_Contr), A         ; set port A as 4 input/ 4 output
		; ld A, Interupt_vector&0xFF                   ; low byte of INT table
		; out (portA_Contr), A         ; PIO A interrupt vector
		ld A, $03
		out (portA_Contr), A         ; PIO A interrupt disable
		; ld a,Interupt_vector>>8      ; high byte of INT table
		; ld I,A
		di
;----------******************* PIO PORT B
		ld A, %11001111                 ;mode 3 in/out
		out (portB_Contr), A         ; set port A as output
		ld A, $00                    ;mode 0 output 
		out (portB_Contr), A         ; set port A as output
		ld A, $03
		out (portB_Contr), A         ; PIO A interrupt disable
		ld a,0
		ld (PIO_B_value),a
		out (portB_Data), a
	ret


