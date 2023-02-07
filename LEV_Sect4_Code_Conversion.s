	
		include "Z80_Params_.inc"
	
	ifndef ONESECTION
		section	Functions	

	else
		section singleAssembly
	endif


		
		GLOBAL 	BN2BCD,BCD2BN, BN2HEX,Bin2Hex8,Bin2Hex16,HEX2BN,BN2DEC,DEC2BN,LC2UC
		GLOBAL	DumpRegisters,MFILL,BLKMOV,putDEtoScreen


		; Code Conversion
		; 4A      Binary to BCD Conversion       167
		; 4B      BCD to Binary Conversion       170
		; 4C      Binary to Hexadecimal ASCII Conversion                 172
		; 4D      Hexadecimal ASCII to Binary Conversion                 175
		; 4E      Conversion of a Binary Number to Decimal ASCII               178
		; 4F      Conversion of ASCII Decimal to Binary             183
		; 4G      Lower-Case to Upper-Case Translation             187
		; 4H      ASCII to EBCDIC Conversion           189
		; 41      EBCDIC to ASCII Conversion           192


	;
		
		; Binary t0 BCD C0nversi0n (BN2BCD)                                                                                4A
		; C0nverts 0ne byte 0f binary data t0 tw0 bytes
		; 0f BCD data.                                                    Registers Used: AF, C, HL
		; Pr0cedure: The pr0gram subtracts 100 repeat-                 Executi0n Time: 497 cycles maximum; depends 0n
		; 																the number 0f subtracti0ns required t0 determine the
		; edly fr0m the 0riginal data t0 determine the                    tens and hundreds digits
		; hundreds digit, then subtracts 10 repeatedly                    Pr0gram Size: 27 bytes
		; fr0m the remainder t0 determine the tens digit,                 Data Mem0ry Required: N0ne
		; and finally shifts the tens digit left f0ur p0siti0ns
		; and c0mbines it with the 0nes digit.

		; Entry C0nditi0ns                                           Exit C0nditi0ns
		; Binary data in A                                           Hundreds digit in H
		; 														Tens and 0nes digits in L
		; Examples
		; 1.    Data:      (A) = 6E I6 (110 decimal)                 2.    Data:      (A) = B7 16 (183 decimal)
		; 	Result:     (H) = 01 16 (hundreds digit)                   Result:     (H) = 01 16 (hundreds digit)
		; 				(L) = 10 16 (tens and 0nes digits)                         (L) = 83 16 (tens and 0nes digits)

		; 			Tit Ie                    Binary t0 BCD c0nversi0n
		; 			Name:                     BN2BCD
		; 			Put"p0se:                 C0nvet"t 0ne byte c.f binat"y data t0:. t . . . 0
		; 										bytes c.f BCD dat
		; 			Entry:                    Register A      binat"y data
		; 			Exit:                     Register H      High byte 0f BCD data
		; 										Reg i stet" L   L0 . . . yte 0f BCD data
		; 			Registet"s used: A F, C, HL                                                                                                       167
		; 168      C0DE C0NVERSI0N
		; /h0me/lellebj/Hämtningar/Z80_Assembly_Language_Subr0utines_1983.txt
		;           Time:                 497 cycles maximum
		;           Size:                 Pr0gram 27 bytes
;*************************************************************************************************

BN2BCD:
			; CALCULATE   100'S DIGIT - DIVIDE BY 100
			; H = QU0TI ENT
			; A = REMAINDER
			LD       H,0FFH			;START QU0TIENT AT -1
		D100LP:
			INC		H				;ADD 1 T0 QU0TIENT
			SUB		100				;SUBTRACT 100
			JR		NC,D100LP		;JUMP IF DIFFERENCE STILL P0SITIVE
			ADD		A,100			;ADD THE LAST 100 BACK


				; CALCULATE 10'S AND 1'S DIGITS
				; DIVIDE REMAINDER 0F THE 100'S DIGIT BY 10
				; L = 10'S DIGIT
				; A 1"S DIGIT
				
			LD		L,0FFH				;~START QU0TIENT AT -1
		D10LP:
			INC		L					;ADD 1 T0 QU0TIENT
			JR		NC,D10LP			;JUMP IF DIFFERENCE STILL P0SITIVE
			ADD		A,10				;ADD THE LAST 10 BACK

				;C0MBINE    1'S AND 10'S DIGITS
			LD		C,A					;SAVE 1'S DIGIT IN C
			LD		A,L
			RLCA						;M0VE 10'S T0 HIGH NIBBLE 0F A
			RLCA
			RLCA
			RLCA
			OR		C                  ;0R IN THE 1'S DIGIT
				;RETURN    WITH L   = L0W   BYTE, H   = HIGH   BYTE
			LD		L,A
			RET

				; SAMPLE EXECUTI0N:

		SC4A:
				; C0NVERT    0A HEXADECIMAL T0 10 BCD
			LD		A,0AH
			CALL	BN2BCD          ;H = 0, L           10H
				; C0NVERT       FF HEXADECIMAL T0 255 BCD
			LD     A,0FFH
			CALL   BN2BCD           ;H = 02H, L       55H

		;C0NVERT 0 HEXADECIMAL T0 0 BCD
			LD      A,0
			CALL    BN2BCD          ;H = 0, L     0

			JR    SC4A
;*************************************************************************************************

				; BCD t0 Binary C0nversi0n (BCD2BN)											4B
				; Entry C0nditi0ns                                       Exit C0nditi0ns
				; BCD data in A                                          Binary data in A

				; Examples
				; I.    Data:     (A) = 99 16                            2.    Data:    (A) = 23 16
				;      Result:    (A) = 63 16 = 9910                          Result:   (A) = 17 16 = 23 10

				;                Title                 BCD t0 binary c0nversi0n
				;                Name:                 BCD2BN
				;                Purp0se:              C0nvert 0ne byte 0f BCD data t0 0ne
				;                                      byte 0f binary da
				;                Entry:                Register A       BCD data
				;                Exit:                 Register A = Binary data
				;                Registers used: A,B,C,F
				;                Time:                 60 cycles


				;                                      4B BCD T0 BINARY C0NVERSI0N (BCD2BN)   171
				;           Size:           Pr0gram 14 bytes
;*************************************************************************************************



BCD2BN:
          ;MULTIPLY UPPER NIBBLE BY 10 AND SAVE IT
          ; UPPER NIBBLE * 10 = UPPER NIBBLE * (8 + 2)
          LD		B,A				; SAVE 0RIGINAL BCD VALUE IN B
          AND		0F0H			;MASK 0FF UPPER NIBBLE
          RRCA						;SHIFT RIGHT 1 BIT
          LD		C,A             ;C = UPPER NIBBLE * 8
          RRCA						; SHIFT RIGHT 2 M0RE TiMES
		RRCA						;A = UPPER NIBBLE * 2
          ADD		A,C
          LD		C,A				;C = UF'PER NIBBLE * (8+2)
          ;GET L0WER NIBBLE AND ADD IT T0 THE
          ; BINARY EQUIVALENT 0F THE UPPER NIBBLE
          LD		A,B				; GET 0RIGINAL VALUE BACK
          AND		0FH				; MASK 0FF UPPER NIBBLE
          ADD		A,C				; ADD T0( BINARY UPPER NIBBLE
          RET

        ;   SAMPLE EXECUTI0N:
SC4B:
          ;C0NVERT 0 BCD T0 0 HEXADECIMAL
          LD		A,0
          CALL		BCD2BN          ; A = 0H
          ;C0NVERT 99 BCD T0 63 HEXADECIMAL
          LD		A,099H
          CALL		BCD2BN          ;A=63H
          ;C0NVERT 23 BCD T0 17 HEXADECIMAL
          LD		A,23H
          CALL		BCD2BN          ;A=17H
          JR		SC4B

;*************************************************************************************************

					; Binary t0 Hexadecimal ASCII  1 or 2 bytes
					; C0nversi0n (BN2HEX8, BN2HEX16)												4C
					;    C0nverts 0ne byte 0f binary data t0 tw0
					;                                                               Registers Used: AF, B, HL
					;            Size:             Pr0gram 28 bytes
;*************************************************************************************************
Bin2Hex16:
			;**************************************
			;  input in DE
			;  output 4 chars to textbuf (HL),
			;	output:  HL-> end of string '0'
			;**********************************
			push 	DE
			call	cnv_byte
			jr      entr8
Bin2Hex8:
			;**************************************
			;  input in E
			;  output 2 chars to textbuf (HL)
			;	output:  HL-> end of string '0'
			;**********************************
			push  	DE
entr8:		ld		D,E					; move low byte to d
			call	cnv_byte
			xor 	A			; reset a
			ld 		(hl),A		; put 0 to end of chars, (hl)-> end of chars
			pop 	DE
			ret


cnv_byte:	ld 		A,D
			rra								;move high nibble to low nibble
			rra
			rra							
			rra
			and		0FH						;get high nibble
         	call	AddToT_Buf				;convert high nibble to ascii
			ld 		A,D
			and		0FH						;get low nibble
         	call	AddToT_Buf				;convert low nibble to ascii
			ret

           ;-----------------------------------
           ; subr0uti addtot_buf
           ; purp0se:    c0nvert a hexadecimal digit t0 asci i
           ; entry: a = binary data in l0wer nibble
           ;exit: a = ascii character
           ;registers used: a,f
           ;-----------------------------------
           
AddToT_Buf:
			cp		10
			jr		C,.AT1				;jump if high nibble < 10
			add		A,7					; else add 7 s0 after add i ng .' 0" the
										; character will be in 'a' .. 'f'
.AT1:
			add		A,'0'				;add ascii 0 t0 make a character
			ld		(hl),A				; add the char to text buffer
			inc 	hl
			ret


        ;    SAMPLE EXECUTI0N:


        ;    ; C0NVERT 0 T0 '0'
		; 	LD		DE,1234
		; 	ld		hl,T_Buffer
		; 	CALL	Bin2Hex16/ Bin2Hex8  
		; 		; result in (T_buffer)

;*************************************************************************************************

				; Hexadecimal ASCII t0 Binary
				; C0nversi0n (HEX2BN)                                                                                          4D

				; C0nverts tw0 ASCII characters (represent-
				; ing two hexadecimal digits) t0 one byte 0f                   Registers Used: AF, B
				; binary data.                                                 Executi0n Time: 148 cycles plus tw0 extra cycles f0r
				; 															each n0n-decimal digit
				; Pr0cedure: The program c0nverts each ASCII
				; 															Pr0gram Size: 24 bytes
				; character separately t0 a hexadecimal digit. This            Data Mem0ry Required: N0ne
				; inv0lves a simple subtracti0n 0f 3016 (ASCII 0)
				; if the digit is decimal. If the digit is n0n-decimal,
				; an0ther 7 must be subtracted t0 acc0unt f0r the         less significant digit. The pr0gram d0es n0t
				; break between ASCII 9 (3916) and ASCII A                check the validity 0f the ASCII characters (that
				; (4116). The pr0gram then shifts the m0re signif-        is, whether they are indeed the ASCII represen-
				; icant digit left f0ur bits and c0mbines it with the     tati0ns 0f hexadecimal digits).
				; Entry C0nditi0ns                                        Exit C0nditi0ns
				; M0re significant ASCII digit in H, less signifi-        Binary data in A
				; cant ASCII digit in L
				; Examples
				; 1.    Data:      (H) = 44 16 (ASCII D)                  2.    Data:     (H) = 31 16 (ASCII 1)
				; 				(L) = 37 16 (ASCII 7)                                  (L) = 42 16 (ASCII B)
				; 	Result:     (A) = D7 16                                 Result:     (A) = IB 16
				; Title                    Hex ASCII t0 binary
				; Name:                     HEX2BN

				; Purp0se:                  C0nvert tw0 ASCII characters t0 0ne
				; 							byte 0f binary data
				; Entry:                    Register H = ASCII m0re significant digit																											175
				; 176      C0DE C0NVERSI0N
                ;              Register L   = ASCII   less significant digit
				; Exit:              Register A = Binary data
				; Register"s used: AF, B
				; Time:              Appr0ximately 148 cycles
				; Size:              Pr0gram 24 bytes
;*************************************************************************************************


HEX2BN:
			LD		A,L					;get l0w character
			CALL	A2HEX				;c0nvert it t0 hexadecimal
			LD		B,A					;save hex value in b
			LD		A,H					;get high character
			CALL	A2HEX				; c0nvert it t0 hexadecimal
			RRCA						;shift hex value t0 upper 4 bits
			RRCA
			RRCA
			RRCA
			OR      B					;or in low hex value
			RET

			;---------------------------------------
			; subr0utine: a2hex
			; purp0se: c0nvert ascii digit t0 a hex digit
			; entry: a = ascii hexadecimal digit
			;exit: a = binary value 0f ascii digit
			;registers used: a,f
			;--------------------------------------
A2HEX:
			SUB		'0'					;subtract ascii 0ffset
			CP		10
			JR		C,A2HEX1			;branch if a is a decimal digit
			SUB		7					;else subtract 0ffset f0r letters
A2HEX1:
			RET

			;    SAMPLE EXECUTI0N:

SC4D:
			; C0NVERT "C7" T0 C7 HEXADECIMAL
			LD		H,'C'
			LD		L,'7'
			CALL	HEX2BN				;A=C7H
			; C0NVERT "2F' T0 2F HEXADECIMAL
			LD		H,'2'
			LD		L,'F'

			CALL	HEX2BN				;A=2FH
			;C0NVERT ~2A~ T0 2A HEXADECIMAL
			LD		H,'2'
			LD		L,'A'
			CALL	HEX2BN				;A=2AH
			JR		SC4D


;*************************************************************************************************
				; C0nversi0n 0f a Binary Number t0
				; Decimal ASCII (BN2DEC)                                                                                      4E

				; C0nverts a 16-bit signed binary number int0					Registers Used: AF, BC, DE, HL
				; an ASCII string. The string c0nsists 0f the               Executi0n Time: Appr0ximately 7200 cycles
				; length 0f the number in bytes, an ASCII minus             Pr0gram Size: 107 bytes
				; sign (if needed), and the ASCII digits. N0te that         Data Mem0ry Required: F0ur bytes anywhere in
				; the length is a binary number, n0t an ASCII               mem0ry f0r the buffer p0inter (tw0 bytes starting at
				; number.                                                   address BUFPTR), thelength 0fthe buffer (0ne byte
				; 														at address CURLEN), and the sign 0f the 0riginal
				; Pr0cedure: The pr0gram takes the abs0lute              value (0ne byte at address NGFLAG). This data
				; value 0f the number if it is negative. The pr0gram        mem0ry d0es n0t include the 0utput buffer which
				; then keeps dividing the abs0lute value by 10              sh0uld be seven bytes l0ng.
				; until the qu0tient bec0mes 0. It c0nverts each
				; digit 0f the qu0tient t0 ASCII by adding ASCII 0     minus sign (in fr0nt) if the 0riginal number was
				; and c0ncatenates the digits al0ng with an ASCII      negative.

				; Entry C0nditi0ns                                     Exit C0nditi0ns
				; Base address 0f 0utput buffer in HL                  0rder in buffer:
				; Value t0 c0nvert in DE                                    Length 0f the string in bytes (a binary number)
				; 														ASCII - (if 0riginal number was negative)
				; 														ASCII digits (m0st significant digit first
				; Examples
				; I.   Data:     Value t0 c0nvert = 3EB7'6             2.     Data:    Value t0 c0nvert = FFC8'6
				; 	Result (in 0utput buffer):                            Result (in 0utput buffer):
				; 			05 (number 0f bytes in buffer)                         03 (number 0f bytes in buffer)
				; 			31 (ASCII I)                                           2D (ASCII-)
				; 			36 (ASCII 6)                                           35 (ASCII 5)
				; 			30 (ASCII 0)                                           36 (ASCII 6)
				; 			35 (ASCII 5)                                           That is, FFC8'6 = -56 10, when c0nsidered as a
				; 			35 (ASCII 5)                                             signed tw0's c0mplement number
				; 			That is, 3EB7'6 = 16055 10

				; 		Title              Binary t0 decimal ASCII
				; 		Name:              BN2DEC
				; 		Purp0se:           C0nvert a 16-bit signed binary number
				; 							t0 ASCI I data
				; 		Entry:             Register   H = High byte 0f 0utput buffer address
				; 							Register   L   L0w byte 0f 0utput buffer address
				; 							Register   D = High byte 0f value t0 c0nvert
				; 							Register   E = L0w byte 0f value t0 c0nvert
				; 		Exit:              The first byte 0f the buffer is the length,
				; 							f0ll0wed by the characters.
				; 		Regi sters used:. AF, BC, DE, HL
				; 		Time:              Appr0ximately 7,200 cycles
				; 		Size:              Pr0gram 107 bytes
				; 							Data      4 bytes
;*************************************************************************************************


BN2DEC:
			; SAVE PARAMETERS
			LD		(BUFPTR),HL			;ST0RE THE BUFFER P0INTER
			EX		DE,HL
			LD		A,0
			LD		(CURLEN),A			;CURRENT BUFFER LENGTH IS 0
			LD		A,H
			LD		(NGFLAG),A			;SAVE SIGN 0F VALUE
			OR		A					;SET FLAGS FR0M VALUE
			JP		P,CNVERT_4E			;JUMP IF VALUE IS P0SITIVE
			EX		DE,HL				;ELSE TAKE ABS0LUTE VALUE (0 - VALUE)
			LD		HL,0
			OR		A					;CLEAR CARRY
			SBC		HL,DE				;SUBTRACT VALUE FR0M 0
          ;C0NVERT VALUE T0 A STRING
CNVERT_4E:
			;HL := HL DIV 10     (DIVIDEND, QU0TIENT)
			;DE := HL M0D 10     (REMAINDER)
			LD		E,0					;REMAINDER = 0
			LD		B,16				;16 BITS IN DIVIDEND
			OR		A					;CLEAR CARRY T0 START
DVL00P:
			;SHIFT THE NEXT BIT 0F THE QU0TIENT INT0 BIT 0 0F THE DIVIDEND
			; SHIFT NEXT M0ST SIGNIFICANT BIT 0F DIVIDEr.D INT0
			;LEAST SIGNIFICANT BIT 0F REMAINDER
			;HL    H0LDS B0TH DIVIDEND AND QU0TIENT. QU0TIENT IS SHIFTED
			;     IN AS THE DIVIDEND IS SHIFTED 0UT.
			;E    IS THE REMAINDER.
			;D0 A 24-BIT SHIFT LEFT, SHIFTING
			; CARRY T0 L, L T0 H, H T0 E
			RL		L					; CARRY (NEXT BIT 0F QU0TIENT) T0 BIT 0
			RL		H					; SHIFT HIGH BYTE
			RL		E					;SHIFT NEXT BIT 0F DIVIDEND
			;IF REMAINDER IS 10 0R M0RE, NEXT BIT 0F 
			;QU0TIENT IS 1 (THIS BIT IS PLACED IN CARRY)
			LD		A,E
			SUB		10					;SUBTRACT 10 FR0M REMAINDER
			CCF							;C0MPLEMENT CARRY
										; (THIS IS NEXT BIT 0F QU0TIENT)
			JR		NC,DECCNT			; JUMP IF REMAINDER IS LESS THAN 10
			LD		E,A					;0THERWISE REMAINDER = DIFFERENCE
										; BETWEEN PREVI0US REMAINDER AND 10
DECCNT:
			DJNZ	DVL00P				; C0NTINUE UNTIL ALL BITS ARE D0NE
			;SHIFT LAST CARRY INT0 QU0TIENT
			RL		L					;LAST BIT 0F QU0TIENT T0 BIT 0
			RL		H
			;INSERT     THE NEXT CHARACTER IN ASCI I
CHINS:
			LD		A,E
			ADD		A,'0'				;~C0NVERT   0 ••• 9 T0 ASCII '0' ••• '9'
			CALL	INSERT
			;IF     QU0TIENT IS N0T 0 THEN KEEP DIVIDING
			LD		A,H
			OR		L
			JR		NZ,CNVERT_4E
EXIT:
			LD		A,(NGFLAG)
			OR		A
			JP		P,P0S				;BRANCH I F 0R ICi I NAL VALUE WAS P0S I TI VE
			LD		A,'-'			;ELSE
			CALL	INSERT				; PUT A MINUS SIGN IN FR0NT
P0S:
			RET							;RETURN
			;----------------------------------
			; SUBR0UTINE: INSERT
			; PURP0SE: INSERT THE CHARACTER IN REGISTER A AT THE
			;			FR0NT 0F THE BUFFER
			; ENTRY: CURLEN = LENGTH 0F BUFFER
			;		BUFPTR = CURRENT ADDRESS 0F LAST CHARACTER IN BUFFER
			;EXIT: REGISTER A INSERTED IMMEDIATELY AFTER LENGTH BYTE
			;REGISTERS USED: AF,B,C,D,E
			;----------------------------------
INSERT:
			PUSH	HL					;SAVE HL
			PUSH	AF					;SAVE CHARACTER T0 INSERT
			;M0VE ENTIRE BUFFER UP 1 BYTE IN MEM0RY
			LD		HL, (BUFPTR)		;GET BUFFER P0INTER
			LD		D,H					;HL = S0URCE (CURRENT END 0F BUFFER)
			LD		E,L
			INC		DE					;DE = DESTINATI0N (CURRENT END + 1)
			LD		(BUFPTR),DE			;ST0RE NEW BUFFER P0INTER
			LD		A, (CURLEN)
			OR		A					;TEST F0R CURLEN = 0
			JR		Z,EXITMR			;.JUMP IF ZER0 (N0THING T0 M0VE,
										; .JUST ST0RE THE CHARACTER.\
			LD		C,A					;BC = L00P C0UNTER
			LD		B,0
			LDDR						;M0VE ENTIRE BUFFER UP 1 BYTE
EXITMR:
			LD		A, (CURLEN)			; INCREMENT CURRENT LENGTH BY 1
			INC		A
			LD		(CURLEN),A
			LD		(HL),A				;UPDATE LENGTH BYTE 0F BUFFER
			EX		DE,HL				;HL P0INTS T0 FIRST CHARACTER IN BUFFER
			POP		AF					;GET CHARACTER T0 INSERT
			LD		(HL),A				; INSERT CHARACTER AT FR0NT 0F BUFFER
			POP		HL					;REST0RE HL
			RET
			; DATA
BUFPTR: DS	2							;ADDRESS 0F LAST CHARACTER IN BUFFER
CURLEN: DS	1							;CURRENT LENGTH 0F BUFFER
NGFLAG: DS	1							; SIGN 0F 0R I GI NAL VALUE



			; SAMPLE EXECUTI0N:


SC4E:
		; C0NVERT 0 T0 .' 0·'
		LD		HL,BUFFER			;HL = BASE ADDRESS 0F BUFFER
		LD		DE,0				;DE = 0
		CALL	BN2DEC				; C0NVERT
									; BUFFER SH0ULD = .' 0·'
		;C0NVERT 32767 T0     ~32767"'
		LD		HL,BUFFER			;HL = BASE ADDRESS 0F BUFFER
		LD		DE,32767			;DE = 32767
		CALL	BN2DEC				; C0NVERT
									; BUFFER SH0ULD = "32767"'
		;C0NVERT -32768 T0    ~-32768"
		LD		HL,BUFFER			;HL  = BASE ADDRESS 0F BUFFER
		LD		DE,-32768			; DE = -3271':'8
		CALL	BN2DEC				; C0NVERT
		JR		SC4E				; BUFFER SH0ULD = ~-32768'

BUFFER: DS	7						;7-BYTE BUFFER


;*************************************************************************************************
				; Conversi0n of ASCII Decimal to Binary (DEC2BN)                                                                                             4F

				; C0nverts an ASCII string c0nsisting 0f the
				; 													Registers Used: AF, BC, DE, HL

				; Entry C0nditi0ns                                      ExIt C0nditi0ns
				; Base address 0f string in HL                          Binary value in HL
				; 													Carry flag is 0 if the string was valid; Carry flag
				; 														is I if the string c0ntained an invalid charac-
				; 														ter.
				; 													N0te that the result is a signed tw0's c0mplement
				; 														16-bit number.
				; Examples
				; 1.   Data:    String c0nsists 0f                      Result:   (H) = 04 16 (m0re significant byte 0f binary
				; 				04 (number 0f bytes in string)                       data)
				; 				31 (ASCII I)                                     (L) = 02 16 (less significant byte 0f binary
				; 				32 (ASCII 2)                                         data)
				; 				33 (ASCII 3)                                           That is, the number +I ,234 10 = 0402 16
				; 				34 (ASCII 4)
				; 			That is, the number is +1,234 10

				; 2.   Data:   String c0nsists 0f                         Result:   (H) = 80 16 (m0re significant byte 0f binary
				; 				06 (number 0f bytes in string)                         data)
				; 				2D (ASCII-)                                        (L) = 1216 (less significant byte 0f binary
				; 				33 (ASCII 3)                                           data)
				; 				32 (ASCII 2)                                             That is, the number - 32, 75010 = 8012 16
				; 				37 (ASCII 7)
				; 				35 (ASCII 5)
				; 				30 (ASCII 0)
				; 			That is, the number is -32,750 10

				; 		Title                   Decimal AsciI t0 binary
				; 		Name:                   DEC2BN

				; 		Purp0se:               C0nvert ASCII characters t0 tw0 bytes 0f binary
				; 								data
				; 		Entry:                  HL = Base address 0f input buffer
				; 		Exit:                   HL    Binary value
				; 								if n0 err0rs then
				; 									Carry = 0
				; 								else
				; 									Carry
				; 		Registers used: A F. BC. DE. HL
				; 		Time:                   Appr0ximately 152 cycles per byte plus
				; 								a maximum 0f 186 cycles 0verhead
				; 		Size:                   Pr0gram         79 bytes
				; 								Data             1 byte
;*************************************************************************************************

DEC2BN:
			;INITIALIZE - SAVE LENGTH. CLEAR SIGN AND VALUE
			LD		A,(HL)			;SAVE LENGTH IN B
			LD		B,A
			INC		HL				;P0INT T0 BYTE AFTER LENGTH
			SUB 	A
			LD		(NGFLAG_4F),A		;ASSUME NUMBER IS P0SITIVE
			LD		DE,0			;START WITH VALUE = 0
			;CHECK F0R EMPTY BUFFER
			OR B					;IS BUFFER LENGTH ZER0?
			JR		Z,EREXIT		;YES, EXIT WITH VALUE = 0
			; CHECK F0R MINUS 0R PLUS SIGN IN FR0NT
INIT1:
			LD		A,(HL)				;GET FIRST CHARACTER
			CP		'-'					;IS IT A MINUS SIGN?
			JR		NZ,PLUS				; N0, BRANCH
			LD		A,0FFH
			LD		(NGFLAG_4F),A			;YES, MAKE SIGN 0F NUMBER NEGATIVE
			JR		SKIP				;SKIP 0VER MINUS SIGN
PLUS:
			CP		'+'					;IS FIRST CHARACTER A PLUS SIGN?
			JR		NZ,CHKDIG			; N0, START C0NVERSI0N
SKIP:		INC		HL					; S~: I P 0VER THE SIGN BYTE
			DEC		B					;DECREMENT C0UNT
			JR		Z,EREXIT			; ERR0R EXIT IF 0NLY A SIGN IN BUFFER
			;C0NVERSI0N L00P
			; C0NTINUE UNTIL THE BUFFER IS EMPTY
			; 0R A N0N-NUMERIC CHARACTER IS F0UND
CNVERT_4F:
			LD		A, (Hl)				;GET NEXT CHARACTER
CHKDIG: 	SUB		'0'
			JR		C,EREXIT			;ERR0R IF < ·'0' (N0T A DIGIT)
			CP		9+1
			JR		NC,EREXIT			;ERR0R IF > '9' (N0T A DIGIT)
			LD		C,A                ; CHARACTER IS DIGIT, SAVE IT
			;VALID DECIMAL DIGIT S0 VALUE := VALUE * 10  = VALUE * (8 + 2)
			;      (VALUE * 8) + (VALUE * 2)
			PUSH	HL					; SAVE BUFFER P0 INTER
			EX		DE, HL				; HL = VALUE
			ADD		HL,HL				; * 2
			LD		E,L					;SAVE TIMES 2 IN DE
			LD		D,H
			ADD		HL,HL				; * 4
			ADD		HL,HL				; * 8
			ADD		HL,DE				;VALUE = VALUE * (8+2)
			;ADD IN   THE NEXT DIGIT
			; VALUE   := VALUE + DIGIT
			LD		E,C					;M0VE NEXT DIGIT T0 E
			LD		D,0					; HIGH BYTE IS 0
			ADD		HL,DE				; ADD DIGIT T0 VALUE
			EX		DE,HL				;DE = VALUE
			POP		HL					;P0INT T0 NEXT CHARACTER
			INC		HL
			DJNZ	CNVERT_4F				;C0NTINUE C0NVERSI0N
			;C0NVERSI0N IS C0MPLETE. CHECK SIGN
			EX		DE,HL				;HL = VALUE
			LD		A,(NGFLAG_4F)
			OR		A
			JR		Z,OKEXIT			;..JUMP IF THE VALUE WAS P0SITIVE
			EX		DE,HL				;ELSE REPLACE VALUE WITH -VALUE
			LD		HL,0
			OR		A					;CLEAR CARRY
			SBC		HL,DE				;SUBTRACT VALUE FR0M 0
			;N0 ERR0RS, EXIT WITH CARRY CLEAR
OKEXIT:
			OR		A					;CLEAR CARRY
			RET
			;AN ERR0R. EXIT WITH CARRY SET
EREXIT:
			EX		DE,HL				;HL = VALUE
			SCF							;SET CARRY T0 INDICATE ERR0R
			RET
			; DATA
NGFLAG_4F: DS	1							;SIGN 0F NUMBER

			; ; SAMPLE EXECUTI0N:

			; ; C0NVERT .' 1234"
			; LD		HL,S1			;HL    = BASE ADDRESS 0F S1
			; CALL	DEC2BN
			; 						;H    = 04,   L   = D2   HEX
			; ;C0NVERT /+32767~
			; LD		HL,S2			;HL    = BASE ADDRESS 0F       S2
			; CALL	DEC2BN
			; 						;H    = 7F, L = FF HEX
			; ;C0NVERT ~-32768~
			; LD		HL,S3			;HL    = BASE ADDRESS 0F S3
			; CALL	DEC2BN
			; 						;H    = 80    HEX, L = 00 HEX
			; S1:       DB	4, "1234"
			; S2:       DB	6, "+32767"
			; S3:       DB	6, "-32768"


;*************************************************************************************************
				; L0wer-Case t0 Upper-Case
				; Translati0n (LC2UC)                                                                                             4G
				; C0nverts an ASCII l0wer-case letter t0 its
				; upper-case equivalent.                                        Registers Used: AF
				; Pr0cedure: The pr0gram uses c0mparis0ns t0                 Executi0n Time: 45 cycles if the 0riginal character is
				; 															a l0wer-case letter, fewer cycles 0therwise
				; determine whether the data is an ASCII l0wer-
				; 															Pr0gram Size: 11 bytes
				; case letter. If it is, the pr0gram subtracts 20,6
				; 															Data Mem0ry Required: N0ne
				; fr0m it, thus c0nverting it t0 its upper-case equiv-
				; alent. If it is n0t, the pr0gram leaves it unchanged.
				; Entry C0nditi0ns                                         Exit C0nditi0ns
				; Character in A                                           If an ASCII l0wer-case letter is present in A,
				; 														then its upper-case equivalent is returned in A.
				; 														In all 0ther cases, A is unchanged
				; Examples
				; I.    Data:      (A) = 62'6 (ASCII b)                    2.    Data:      (A) = 54'6 (ASCII T)
				; 	Result:     (A) = 42 16 (ASCII B)                        Result:     (A) = 54 16 (ASCII T)
				; 			Title 					 L0wer-case t0 upper-case translati0n
				; 			Name:                     LC2UC
				; 			Purp0se:                 C0nvert 0ne ASCII character t0 upper case fr0m
				; 										l0wer case if necessary
				; 			Entry:                    Register A     L0wer-case ASCII character
				; 			Exit:                     Register A = Upper-case ASCII character if A
				; 													is l0wer case, else A is unchanged
				; 			Registers used: A F
				; 		Time:               45 cycles if A is l0wer case. less 0therwise
				; 		Size:               Pr0gram 11 bytes
				; 							Data     n0ne
;*************************************************************************************************

LC2UC:
			CP		'a'
			JR		C,EXIT_4G				; BRANCH IF < "a"       (N0T L0WER CASE)
			CP		'z'+1
			JR		NC,EXIT_4G				; BRANCH IF :> ~ z·" (N0T L0WER CASE)
			SUB		'a'-'A'				; CHANGE ... a·.... "'z'" int0 ·'A·' •• ,··z·"
EXIT_4G:
			RET

			; SAMPLE EXECUTI0N:
SC4G:
			;C0NVERT L0WER CASE E T0 UPPER CASE
			LD		A,'e'
			CALL	LC2UC				;A='E'=45H
			;C0NVERT L0WER CASE Z T0 UPPER CASE
			LD		A,'z'
			CALL	LC2UC				; A='Z'=5AH
			;C0NVERT UPPER CASE A T0 UPPER CASE A
			LD		A,'A'
			CALL	LC2UC				;A='A'=41H
			JR		SC4G


;*************************************************************************************************
				; ASCII t0 EBCDIC C0nversi0n (ASC2EB)                                                                           4H
				; C0nverts an ASCII character t0 its EBCDIC
				; equivalent.                                                   Registers Used: AF, DE, HL
				; Pr0cedure: The pr0gram uses a simple table                 executi0n Time: 55 cycles
				; l00kup with the data as the index and address                 Pr0gram Size: II bytes, plus 128 bytes f0r the c0n-
				; 															versi0n table
				; EBCDIC as the base. A printable ASCII charac-
				; 															Data Mem0ry Required: N0ne
				; ter with n0 EBCDIC equivalent is translated t0
				; an EBCDIC space (4016); a n0n-printable ASCII
				; character with n0 EBCDIC equivalent is trans-
				; lated t0 an EBCDIC NUL (0016).
				; Entry C0nditi0ns                                         Exit C0nditi0ns
				; ASCII character in A                                     EBCDIC equivalent in A
				; Examples
				; I.    Data:      (A)   = 35'6 (ASCII 5)                  3.    Data:      (A) = 2A'6 (ASCII *)
				; 	Result:     (A) = F5'6 (EBCDIC 5)                        Result:     (A) = 5C'6 (EBCDIC *)
				; 2.    Data:      (A) = 77'6 (ASCII w)
				; 	Result:     (A) = A6'6 (EBCDIC w)
				; 			Title                      ASCII t0 EBCDIC c0nversi0n
				; 			Name:                      ASC2EB
				; 			PUurp0se:                  C0nvert an ASCII character t0 its
				; 										c0rresp0nding EBCDIC character
				; 			Enh'Y:                     Register A   ASCI I chat"acter"
				; 			Ed t:                      Register A   EBCDIC character.
				; 		Registers used: AF,DE,HL
				; 		Time:            55 cycles
				; 		Size:            Pr0gram 11 bytes
				; 						Data    128 bytes f0r the table
;*************************************************************************************************


ASC2EB:
			LD		HL,EBCDIC			;GET BASE ADDRESS 0F EBCDIC TABLE
			AND		011111111B			;BE SURE BIT 7 = 0
			LD		E,A					;USE ASCII AS INDEX INT0 EBCDIC TABLE
			LD		D,0
			ADD		HL,DE
			LD		A, (HL)				;GET EBCDIC
			RET
;ASCII T0 EBCDIC TABLE
;   A PRINTABLE ASCII CHARACTER WITH N0 EBCDIC EQUIVALENT IS
;   TRANSLATED T0 AN EBCDIC SPACE (040H), A N0NPRINTABLE ASCII CHARACTER
; WITH N0 EQUIVALENT IS TRANSLATED T0 A EBCDIC NUL (000H)
EBCDIC:
			;		 NUL  S0H  STX  ETX  E0T  ENQ  ACK  BEL			;ASCII
			DB      000H,001H,002H,003H,037H,02DH,02EH,02FH         ;EBCDIC
			;		 BS   HT   LF   VT   FF   CR   S0   SI			;ASCII
			DB      016H,005H,025H,00BH,00CH,00DH,00EH,00FH         ;EBCDIC
			;		DLE	 DC1  DC2  DC3  DC4  NAK  SYN  ETB			;ASCII
			DB      010H,011H,012H,013H,03CH,03DH,032H,026H         ;EBCDIC
			;		CAN  EM   SUB  ESC  IFS  IGS  IRS  IUS			;ASCII
			DB      018H,019H,03FH,027H,01CH,01DH,01EH,01FH         ;EBCDIC
			;		SPACE ! 	"   #	 $	  %	   & 	'			;ASCII
			DB      040H,05AH,07FH,07BH,05BH,06CH,050H,00DH         ;EBCDIC
			;		(     ) 	*   +    ,    _	   .   /			;ASCII
			DB      04DH,05DH,05CH,04EH,06BH,060H,04BH,061H         ;EBCDIC
			;		0	 1    2    3    4    5    6    7			;ASCII
			DB		0F0H,0F1H,0F2H,0F3H,0F4H,0F5H,0F6H,0F7H         ;EBCDIC
			;		 8    9    : 	;	 <     =   >	?			;ASCII
			DB      0F8H,0F9H,07AH,05EH,04CH,07EH,06EH,06FH         ;EBCDIC
			;        @    A    B    C    D    E    F    G			;ASCII
			DB      07CH,0C1H,0C2H,0C3H,0C4H,0C5H,0C6H,0C7H         ;EBCDIC
			;		 H    I    J    K    L    M    N    0			;ASCII
			DB		0C8H,0C9H,0D1H,0D2H,0D3H,0D4H,0D5H,0D6H         ;EBCDIC
			;		 P    Q    R    S    T    U    V    W			;ASCII
			DB		0D7H,0D8H,0D9H,0E2H,0E3H,0E4H,0E5H,0E6H         ;EBCDIC
			;	     X    Y    Z    [    \    ]    ^	-			;ASCII
			DB		0E7H,0E8H,0E9H,040H,0E0H,040H,040H,06DH         ;EBCDIC
			;		 '	  a	   b    c 	 d    e    f    g			;ASCII
			DB		009H,081H,082H,083H,084H,085H,086H,087H         ;EBCDIC
			;		 h 	  i    j    k    l	  m    n    0			;ASCII
			DB		088H,089H,091H,092H,093H,094H,095H,096H			;EBCDIC
			;		 p    q    r    s    t    u    v    w			;ASCII
			DB		097H,098H,099H,0A2H,0A3H,0A4H,0A5H,0A6H         ;EBCDIC
			;		 x    y    z    {   |     }    ~    DEL			; ASCII
			DB		0A7H,0A8H,0A9H,0C0H,06AH,0D0H,0A1H,007H         ;EBCDIC

        ; SAMPLE EXECUTI0N:

SC4H:
			; C0NVERT ASC I I -' A -' T0 EBCD I C
			LD		A,'A'			; ASCI I ---A-'
			CALL	ASC2EB			; EBCD I C -' A -'   0C 1 H

			; C0NVERT ASC I I '1 -' T0 EBCD I C
			LD		A,'1'			;ASCII '1'
			CALL	ASC2EB			; EBCD I C '1 -'       0F 1 H

			;C0NVERT ASCII 'a' T0 EBCDIC
			LD		A, 'a'			; ASC I I -' a-'
			CALL	ASC2EB			; EBCD I C -' a -'          081 H

			JR	SC4H

;*************************************************************************************************
;*************************************************************************************************
					; EBCDIC t0 ASCII C0nversi0n (EB2ASC)                                                                        41
					; C0nverts an EBCDIC character t0 its ASCII
					; equivalent.                                                Registers Used: AF, DE, HL
					; Pr0cedure: The pr0gram uses a simple table              Executi0n Time: 48 cycles
					; l00kup with the data as the index and address              Pr0gram Size: 9 bytes, plus 256 bytes f0r the c0n-
					; 														versi0n table
					; ASCII as the base. A printable EBCDIC charac-
					; 														Data Mem0ry Required: N0ne
					; ter with n0 ASCII equivalent is translated t0 an
					; ASCII space (2016); a n0n-printable EBCDIC
					; character with n0 ASCII equivalent is trans-
					; lated t0 an ASCII NUL (00 16)'
					; Entry C0nditi0ns                                      Exit C0nditi0ns
					; EBCDIC character in A                                 ASCII equivalent in A
					; Examples
					; I.    Data:      (A) = 85'6 (EBCDIC e)                2.    Data:     (A) = 4E'6 (EBCDIC +)
					; 	Result:     (A) = 65'6 (ASCII e)                      Result:    (A) = 2B'6 (ASCII +)
					; 			Title                    EBCDIC t0 ASCII c0nversi0n
					; 			Name:                    EB2ASC
					; 			Purp0se:                 C0nvert an EBCDIC character t0 its
					; 										c0rresp0nding ASCII character
					; 			Entr'y:                  Register A   EBCDIC character
					; 			Exit:                    Register A   ASCII character
					; 			Registers used: AF ,DE, H L
					; 		Time:               48 cycles
					; 		Size:               Pr0gram 9 bytes
					; 							Data    256 bytes f0r the table
;*************************************************************************************************
;*************************************************************************************************


EB2ASC:
			LD		HL,ASCII			;0ET BASE ADDRESS 0F ASCII TABLE
			LD		E,A					; USE EBCDIC AS INDEX
			LD		D,0
			ADD		HL,DE
			LD		A,(HL)				;0ET ASC II CHARACTER
			RET
; EBCDIC T0 ASCII TABLE
; A PRINTABLE EBCDIC CHARACTER WITH N0 ASCII EQUIVALENT IS
; TRANSLATED T0 AN ASCII SPACE (020H). A N0NPRINTABLE EBCDIC CHARACTER
; WITH N0 EQUIVALENT IS TRANSLATED T0 AN ASCII NUL (000H)
ASCII:
			; 	    NUL   S0H  STX  ETX       HT        DEL				;EBCDIC
			DB      000H,001H,002H,003H,000H,009H,000H,07FH				;ASCII
			;						VT   FF   CR   S0	S1				;EBCDIC
			DB		000H,000H,000H,00BH,00CH,00DH,00EH,00FH				;ASCII
			;	     DLE  DCl  DC2  DC3			   BS					;EBCDIC
			DB      010H,011H,012H,013H,000H,000H,008H,000H  			;ASCII
			;		 CAN  EM             IFS  IGS  IRS  IUS				;EBCDIC
			DB		018H,019H,000H,000H,01CH,01DH,01EH,01FH				;ASCII
			;								  LF   ETB  ESC				;EBCDIC
			DB		000H,000H,000H,000H,000H,00AH,017H,01BH				;ASCII
			;						          ENQ  ACK  BEL				;EBCDIC	
			DB      000H,000H,000H,000H,000H,005H,006H,007H				;ASCII
			;                  SYN						EOT				;EBCDIC
			DB		000H,000H,016H,000H,000H,000H,000H,004H				;ASCII
			;                            DC4  NAK  		SUB 			;EBCDIC
			DB		000H,000H,000H,000H,014H,015H,000H,01AH				;ASCII
			;     SPACE													;EBCDIC
			DB		' ' ,000H,000H,000H,000H,000H,000H,000H				;ASCII
			;						.	 <	  (    +					;EBCDIC
			DB      000H,000H,' ' ,'.' ,'<' ,'(' ,'+' ,' '				;ASCII
			;	  	 &													;EBCDIC
			DB		'&' ,000H,000H,000H,000H,000H,000H,000H				;ASCII
			;				   !    $    *    )    ;  					;EBCDIC
			DB      000H,000H,'!' ,'$' ,'*' ,')' ,';' ,' '				;ASCII
			;		 _    /												;EBCDIC
			DB      '_' ,'/' ,000H,000H,000H,000H,000H,000H				;ASCII
			;                  |    ,    %    -    >    ?     			;EBCDIC
			DB      000H,000H,'|' ,',' ,'%' ,'-' ,'>' ,'?'  			;ASCII
			;															;EBCDIC
			DB      000H,000H,000H,000H,000H,000H,000H,000H 			;ASCII
			;             `    :    #    @    '    =    "				;EBCDIC
			DB      000H,'`' ,':' ,'#' ,'@' ,' ' ,'=' ,'"'   			;ASCII
			;             a    b    c    d    e    f    g				;EBCDIC
			DB      000H,'a' ,'b' ,'c' ,'d' ,'e' ,'f' ,'g'  			;ASCII
			;		h     i												;EBCDIC
			DB      'h' ,'i' ,000H,000H,000H,000H,000H,000H				;ASCII
			;			  j    k    l    m    n    o    p				;EBCDIC
			DB      000H,'j' ,'k' ,'l' ,'m' ,'n' ,'o' ,'p'				;ASCII
			;		q    r												;EBCDIC
			DB      'q' ,'r' ,000H,000H,000H,000H,000H,000H  			;ASCII
			;			 ~	   s    t    u    v    w    x				;EBCDIC
			DB      000H,'~' ,'s' ,'t' ,'u' ,'v' ,'w' ,'x'				;ASCII
			;	     y    z												;EBCDIC
			DB      'y' ,'z' ,000H,000H,000H,000H,000H,000H 			;ASCII
			;															;EBCDIC
			DB      000H,000H,000H,000H,000H,000H,000H,000H  			;ASCII
			;                                                     		;EBCDIC
			DB      000H,000H,000H,000H,000H,000H,000H,000H  			;ASCII
			;		 {    A    B    C    D    E    F    G           	;EBCDIC
			DB      '{' ,'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G'				;ASCII
			;		 H    I                                     		;EBCDIC
			DB      'H' ,'I' ,000H,000H,000H,000H,000H,000H   			;ASCII
			;		 }    J    K    L    M    N    O    P       		;EBCDIC
			DB      '}' ,'J' ,'K' ,'L' ,'M' ,'N' ,'O' ,'P'      		;ASCII
			;	     Q    R                                   			;EBCDIC
			DB      'Q' ,'R' ,000H,000H,000H,000H,000H,000H				;ASCII
			;		 \         S    T    U    V    W    X				;EBCDIC
			DB      '\\' ,000H,'S' ,'T' ,'U' ,'V' ,'W' ,'X'     			;ASCII
			;		 Y    Z                                				;EBCDIC
			DB      'Y' ,'Z' ,000H,000H,000H,000H,000H,000H				;ASCII
			;        0    1    2    3    4    5    6    7				;EBCDIC
			DB      '0' ,'1' ,'2' ,'3' ,'4' ,'5' ,'6' ,'7'     			;ASCII
			;		9													;EBCDIC
			DB      '9' ,000H,000H,000H,000H,000H,000H,000H				;ASCII



        ;  SAMPLE EXECUTI0N:


SC4I:
			;C0NVERT EBCDIC            'A'    T0 ASCII
			LD		A,0C1H                        ;EBCDIC           ~A~
			CALL	EB2ASC                        ;ASCII "'A~             = 041H
			;C0NVERT EBCDIC            ~1'"   T0 ASCII
			LD		A,0F1H                        ;EBCDIC ~1"
			CALL	EB2ASC                        ; ASCI I "1" = 031H
			;C0NVERT EBCDIC            ~a"    T0 ASCII
			LD		A,081H                        ; EBCDIC .' a'"
			CALL	EB2ASC                        ; ASCII "'a'" = 061H
			JR		SC4I


;*************************************************************************************************
;*************************************************************************************************

putDEtoScreen:
		; Binary to HEX  BN2HEX   DE->(HL)
		ld 		hl,T_BUFFER
		inc		hl
		call	Bin2Hex16			;result in T_buffer
		ld 		iy,T_BUFFER
		call	WriteLine
		ret


;*************************************************************************************************
;*************************************************************************************************
		;  Dump registers. Saves all registers, display to screen.
		;	Dump of flag registers

		GLOBAL	RegLabels1,RegLabels2,RegLabels3, display_n_8registers

DumpRegisters:
		ld		(SP_value),sp
		ld		(reg16_value),hl
		; ex		(sp),hl
		; ld		(PC_value),hl
		; ex		(sp),hl
		ld 		hl,(SP_value)		; get value of sp to hl
		push 	hl				; push value of hl(=sp) to (sp)
		ld		hl,(reg16_value)	
		push	ix
		push	iy
		push	af
		push	BC
		push	de
		push 	hl
		ex		af,af'
		exx
		push	af
		push	BC
		push	de
		push 	hl
		ex		af,af'
		exx




; 			;	clear Textbuf
; 		ld 		a,0
; 		ld 		de,Textbuf
; 		ld 		b,0x40
; .zeroTextBuf:
; 		ld 		(de),A
; 		djnz	.zeroTextBuf		

		

		ld		ix,(SP_value)		; ix  points to stack

		ld 		iy,RegLabels1
		call 	WriteLineCRNL

		ld		hl,S1x
		call	display_4_registers
		call  	CRLF

		ld 		iy,RegLabels2
		call 	WriteLineCRNL

		ld		hl,S1x
		call	display_4_registers
		call 	CRLF

		ld 		iy,RegLabels3
		call	WriteLineCRNL

		ld		hl,S1x
		call	display_4_registers
		call 	CRLF

			; display status flags  	DB		38," S Z X H X P N C",0
		ld		A,(ix+16)
		ld 		c,a			; store A
		ld		hl,S1x+1
		ld		iy,RegFlags+1

		ld 		b,05
		call	add_space		; add (b) spaces to (hl), advance hl	

		ld 		b,8
all_flags:
		sla		c			; shift msb bit to C flag

		call	C,disp_flags			; set act. flag in text.buf

		call	adv_iy
		djnz	all_flags


		xor 	a					; clear A
		ld 		(hl),a				; set end of string
		call	adjust_txtbuf_length
		ld 		iy,S1x
		call 	WriteLineCRNL

		call 	waitForKey
		call 	CRLF


		exx
		ex		af,af'
		pop	hl
		pop	de
		pop	bc
		pop af
		exx
		ex	af,af'
		pop hl
		pop	de
		pop	BC
		pop	af
		pop	iy
		pop	ix

		ld 		sp,(SP_value)		; restore value of sp

		ret
;********************************************************************************************
;********************************************************************************************	

disp_flags:
		ld 		A,(iy)
		ld		(hl),A
		inc		hl
		ld		a,(iy+1)
		ld 		(hl),a
		inc		hl
		ret
adv_iy:
		inc		iy
		inc		iy
		ret
;********************************************************************************************
;********************************************************************************************	


		;	load 4 16bit values in buffer ->(HL)
display_4_registers:
		ld 		b,04
		
display_regs:
		ld		(reg16_value),bc		; save the # regs /line

		ld 		b,05
			; add (b) spaces to (hl), advance hl	
		call	add_space


		ld		d,(ix+1)
		ld		e,(ix)		; get value from temp.stack..to de..?
		dec		ix
		dec		ix			; ix points to next rec in stack

		call	Bin2Hex16			;result added to (HL)-> to last 0x00. hl updatd (+4)

		ld		bc,(reg16_value)
		djnz 	display_regs			; display (b) registers with spaces...

		; call	adjust_txtbuf_length
		xor 	a
		ld 		(hl),A

		ld 		iy,S1x
		call	WriteLineCRNL
		ret


;********************************************************************************************
;********************************************************************************************	

;********************************************************************************************
;********************************************************************************************	

		;	load n 8bit values in buffer ->(HL); i.e. S1x
		; 	ix points to data.
		;	n = reg B
display_n_8registers:
		
		ld		(reg16_value),bc		; save the # regs /line

		ld 		b,02
			; add (b) spaces to (hl), advance hl	
		call	add_space

		ld		e,(ix)		; get value from (ix) to e
		inc		ix

		call	Bin2Hex8			;result added to (HL)-> to last 0x00. hl updatd (+2)

		ld		bc,(reg16_value)
		djnz 	display_n_8registers			; display (b) registers with spaces...

		; call	adjust_txtbuf_length
		xor 	a
		ld 		(hl),A

		ld 		iy,S1x
		call 	WriteLineCRNL
		ret


;********************************************************************************************
;********************************************************************************************	
		global 	add_space

		; set new length in string pos 0 (HL)-> end of string

add_space:
		ld 		a,' '
		ld 		(hl),A
		inc 	hl
		djnz	add_space
		ret

adjust_txtbuf_length:

		ld		de,S1x
		SBC		hl,de		; calc new length of string
		ld 		a,l
		ld 		(de),a		; store in first pos
		add		hl,de		; restore to first pos after spaces
		ret
;********************************************************************************************
;********************************************************************************************	




		; Array Manipulation and Indexing
		; 5A      Memory Fill    195
		; 5B      Block Move    198

				;*****************************************************************************************************
				;*****************************************************************************************************
				; Memory Fill (MFILL)                                                                                                        5A
				; Places a specified value in each byte of a mem-
				; ory area of known size, starting at a given ad-                 Registers Used: AF, BC, DE, HL
				; dress.                                                          Execution Time: Approximately 21 cycles per byte
				;         Registers used: AF.BC.DE.HL
				;         Time:                Approximately 21 cycles per byte plus
				;                                 50 cycles overhead
				;         Size:                Program 11 bytes
				;                                 Data    None
				;*****************************************************************************************************
				;*****************************************************************************************************

MFILL:
			LD		(HL),A			;FILL FIRST BYTE WITH VALUE
			LD		D,H				;DESTINATION PTR = SOURCE PTR + 1
			LD		E,L
			INC		DE
			DEC		BC				;ELIMINATE FIRST BYTE FROM COUNT
			LD		A,B				;ARE THERE MORE BYTES TO FILL?
			OR		C
			RET		Z				;NO. RETURN - SIZE WAS 1
			LDIR					;YES. USE BLOCK MOVE TO FILL REST
									; BY MOVING VALUE AHEAD 1 BYTE
			RET



			; SAMPLE EXECUTION:


SC5A:
			;FILL BF1 THROUGH BF1+15 WITH 00
; 			LD		HL,BF1			:STARTING ADDRESS
; 			LD		BC,SIZEI		JNUMBER OF BYTES
; 			LD		A,0				;VALUE TO FILL
; 			CALL	MFILL			;FILL MEMORY
; 			;FILL BF2 THROUGH BF2+l999 WITH FF
; 			LD		HL,BF2			;STARTING ADDRESS
; 			LD		BC,SIZE2		;NUMBER OF BYTES
; 			LD		A,0FFH			;VALUE TO FILL
; 			CALL	MFILL			;FILL MEMORY
; 			JR		SC5A
; SIZEl	EQU	16					;SIZE OF BUFFER 1 (10 HEX)
; SIZE2	EQU	2000				; SIZE OF BUFFER 2 (07DO HEX)
; BF1:	DS	SIZE1
; BF2:	DS	SIZE2


				;*****************************************************************************************************
				;*****************************************************************************************************
				; Block Move (BLKMOV)                                                                                                 58
				; Moves a block of data from a source area to
				; a destination area.                                              Registers Used: AF, BC, DE, HL
				; 		Title               Block Move
				; 		Name:                BLKMOV
				; 		Purpose:             Move data from source to destination
				; 		Entry:               Register'H     = High byte of source address
				; 							Register L     = Low byte of sou,rce address
				; 							Register D     = High byte of destination address
				; 							Register E     = Low byte of destination address     ,
				; 							Register B     = High byte of number of bytes to move,
				; 							Register C     = Low byte of number of bytes to move
				; 		Exit:                Data moved from source to destination
				; 		Registers used:AF,BC,DE,HL
				; 		Time:                21 cycles per byte plus 97 cycles overhead
				; 							if no overlap exists, 134 cycles overhead
				; 							if overlap occurs
				; 		Size:                Program 27 bytes
				;*****************************************************************************************************
				;*****************************************************************************************************


BLKMOV:
			LD		A,B				;IS SIZE OF AREA O?
			OR		C
			RET		Z				;YES, RETURN WITH NOTHING MOVED
				; DETERMINE IF DESTINATION AREA IS ABOVE SOURCE AREA AND OVERLAPS
				; IT (OVERLAP CAN BE MOD 64K). OVERLAP OCCURS IF
				; STARTING DESTINATION ADDRESS MINUS STARTING SOURCE ADDRESS
				; (MOD 64K) IS LESS THAN NUMBER OF BYTES TO MOVE
			EX		DE,HL			;CALCULATE DESTINATION - SOURCE
			PUSH	HL				;SAVE DESTINATION
			AND		A				;CLEAR CARRY
			SBC		HL,DE
			AND		A				;THEN    SUBTRACT AREA SIZE
			SBC		HL,BC
			POP		HL				;RESTORE    DESTINATION
			EX		DE,HL
			JR		NC,DOLEFT		;JUMP    IF NO PROBLEM WITH OVERLAP
				; DESTINATION     AREA IS ABOVE SOURCE AREA AND OVERLAPS IT
				; MOVE   FROM HIGHEST ADDRESS TO AVOID DESTROYING DATA
			ADD		HL,BC			;SOURCE = SOURCE + LENGTH - 1
			DEC		HL
			EX		DE,HL			;DEST = DEST + LENGTH - 1
			ADD		HL,BC
			DEC		HL
			EX		DE,HL
			LDDR					;BLOCK MOVE HIGH TO LOW
			RET
			; ORDINARY     MOVE STARTING AT LOWEST ADDRESS
DOLEFT:
			LDIR					;BLOCK    MOVE LOW TO HIGH
			RET



			; SAMPLE EXECUTION:


; SOURCE    EQU		2000H             ;BASE    ADDRESS OF SOURCE AREA
; DEST      EQU		2010H             ;BASE    ADDRESS OF DESTINATION AREA
; LEN       EQU		11H               ;NUMBER    OF BYTES TO MOVE
;           ;MOVE   11 HEX BYTES FROM 2000-2010 HEX TO 2010-2020 HEX
; SC5B:
; 			LD		HL,SOURCE
; 			LD		DE,DEST
; 			LD		BC,LEN
; 			CALL	BLKMOV            ;MOVE    DATA FROM SOURCE TO DESTINATION
; 			JR		SC5B



		align 4




.END



