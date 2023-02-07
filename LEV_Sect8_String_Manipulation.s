
		include "Z80_Params_.inc"

	
	ifndef ONESECTION
		section	Functions	

	else
		section singleAssembly
	endif


		GLOBAL strCompare,CONCAT,POS,COPY,DELETE,INSERT_STR,src_size,isHex,isChar,dumpMemory
		xref	isDelimit
		
		; String Manipulation
		; 8A    String Compare    288
		; 8B    String Concatenation 292
		; 8C    Find the Position of a Substring 297
		; 8D    Copy a Substring from a String 302
		; 8E    Delete a Substring from a String 308
		; 8F    Insert a Substring into a String 313

		;****************************************************************************************************************
		;****************************************************************************************************************
		; String Compare (StrCompare)                                                                                      8A
		;     Compares two strings and sets the Carry and
		;                                                         Registers Used: AF, BC, DE, HL
		; Zero flags appropriately. The Zero flag is set to I
		;                                                         Execution nme:
		;                                                         larger.
		; Examples
		; I.    Data:     String 1 = OS'PRINT' (05 is the length of the   3.    Data:      String 1 = OS'PRINT' (05 is the length of the
		;                   string)                                                          string)
		;                 String 2 = 03'END' (03 is the length of the                      String 2 = 06'SYSTEM' (06 is the length of
		;                   string)                                                          the string)
		;     Result:    Zero flag = 0 (strings are not identical)            Result:     Zero flag = 0 (strings are not identical)
		;                 Carry flag = 0 (string 2 is not larger than                      Carry flag = I (string 2 is larger than string I)
		;                   string 1)
		;                                                                 of ASCII characters. Note that the byte preceding
		; 2.    Data:     String 1 = OS'PRINT' (05 is the length of the   the actual characters contains a hexadecimal
		;                   string)                                       number (the length of the string), not a character.
		;                 String 2 = 02'PR' (02 is the length of the      We have represented this byte as two hexadecimal
		;                   string)
		;                                                                 digits in front of the string. The string itself is
		;     Result:    Zero flag = 0 (strings are not identical)
		;                 Carry flag = 0 (string 2 is not larger than     shown surrounded by single quotation marks.
		;                                           a length byte which precedes it.
		;           Exit:                IF string 1 = string 2 THEN
		;                                   Z=1,C=O
		;                                 IF string 1 > string 2 THEN
		;                                   Z=O,C=O
		;                                 IF string 1 < string 2 THEN
		;                                   Z=O,C=l
		;           Registers used: AF,BC,DE,HL
		;           Time:                91 cycles overhead plus 60 cycles per byte plus
		;                                 40 cycles if strings are identical
		;                                 through length of shorter
		;           Size:                Program 32 bytes
		;                                 Data     2 bytes
		;****************************************************************************************************************
		;****************************************************************************************************************


strCompare:
					;determine which string is shorter
					;length of shorter = number of bytes to compare
		LD		A,(HL)				;save length of string 1
		LD		(LENS1),A
		LD		A, (DE)				;save length of string 2
		LD		(LENS2),A
		CP		(HL)				;compare to length of string 1
		JR		C,str2ISshorter		;jump if string 2 is shorter
		LD		A, (HL)				;else string 1 is shorter
					;compare strings through length of shorter
str2ISshorter:
		OR		A					;test length of shorter string
		JR		Z,cmpLength		;compare lengths ; if length is zero

		LD		B,A					;b = number of bytes to compare
		EX		DE,HL				;de = string 1
									;hl = string 2
cmpLoop:
		INC		HL					;increment to next bytes
		INC		DE
		LD		A,(DE)              ;get a byte of string 1
		CP		(HL)				;compare to byte of string 2
		RET		NZ					;return with flags set if bytes not EQUAL
		DJNZ	cmpLoop				;continue through all bytes
			; strings same through length of shorter
			; so use lengths to set flags
cmpLength:
		LD		A,(LENS1)			;compare lengths
		LD		HL,LENS2
		CP		(HL)
		RET							; return with flags set or cleared
		; DATA
LENS1:	DS		1					;length of string 1
LENS2:	DS		1					;length of string 2

		; 5AMPLE EXECUTION:


SC8A:
		LD		HL,S1				;base address of string 1
		LD		DE,S2				;base address of string 2
		CALL	strCompare			;compare strings
									;comparing "string 1" and "string 2"
									; results in string 1 less than
									; string 2. so z=o.c=1
		JR      SC8A				;loop for another test
S1:		DB		20H					;string 1
S2:		DB		20H					;string 2

	;****************************************************************************************************************
	;****************************************************************************************************************

		GLOBAL 		isDelimit,skipPriorDelimit,skipCharsUntilDelim


delimChars:
		db   ' _&/#,=',CR,0,0	
				; 		isDelimit(S)  is char in (HL) any of the delimiters specified ? =>Z, else ~Z
				; 		if char in (HL) is '0' ->  set C, else NC
				; 		Parameters returned; HL - Address of char
	

isDelimit:
		push 	DE
				; HL points to string, DE points to delimiters
		ld 		DE,delimChars
		ld 		a,(HL) 			; char from string
		or 		a 				; is a (DE) = 0 ?
		jr 		z,exitZero

nxtdelim:

		ld 		A,(DE)			; actual delimiter
		cp 		(HL)			; check present delimiter
		jr 		z,exitDelim		; Z set


		inc  	DE				; next delimiter
		ld  	A,(DE)
		or 		A 				; =0? 		
		jr 		nz,nxtdelim     ; if no -> next delimiter
		inc 	a 				; clear Z flag
exitDelim:
		scf	
		ccf						; clear Carry (Z is set if (HL) is delimiter, cleared otherwise)
		pop 	DE
		ret

exitZero:
		; carry flag always cleared.
		inc 	a 			; clear Z flag
		scf 				; set Carry-char = '0'
		pop 	DE
		ret


skipPriorDelimit:
				; increase HL until non delimiter (NZ) or #0 (C) 
				; HL points to acutal pos in 'Textbuf'
		inc 	HL 				; skip past string length or next char	
		call	isDelimit		;delimiters specified ? =>Z, else ~Z
								;char in (HL) is '0' ->  set C, else NC

		ld 		a,(HL)			; A = value of actl. char						
		ret 	C 				; end of string '0' or 'CR' found

		ret 	NZ 				; NZ -> (HL) points to non delimiter
		jr 		skipPriorDelimit


skipCharsUntilDelim:
				; increase HL until delimiter (NZ) or #0 (Z) 
		inc 	HL 				; skip past string length or next char		
		call	isDelimit		;delimiters specified ? =>Z, else ~Z
								;char in (HL) is '0' ->  set C, else NC
		ld 		a,(HL)			; A = value of actl. char						
		ret 	C				; end of string '0' or 'CR' found
		ret  	Z				; Z -> (HL) points to delimiter
		jr 		skipCharsUntilDelim


	;****************************************************************************************************************
	;****************************************************************************************************************

isHex:
		; ***	Check if characters are HEX ? 
		; ***	from (HL)  .. 0..9,A..F -> NC  others -> C
		ld 		A,(HL)
		sub 	'0'
		jp 		M,.setCarry 			; less than '0'
		cp 		10						
		jp 		P,.checkAF			; bigger than '9'
		jp 		.nextChar			; char between 0..9 -> OK

.checkAF:
		and 	~$20				; clear bit 5  ($DF) mask to Upper case

		sub 	7		
		cp 		$0A
		jp 		M,.setCarry			; less than 'A'
		cp 		$10		
		jp 		P,.setCarry			; bigger than 'F'
		jp		.nextChar			; char between A..F -> OK
.setCarry:
		scf
		ret							; return with Carry, value in A is NOT HEX
.nextChar:	
		scf
		ccf
		ret							; return without Carry, value in A is HEX

	;****************************************************************************************************************
	;****************************************************************************************************************

isChar:
		; ***	Check if characters are Char ? 
		; ***	from (HL)  $21 .. $-7E -> NC  others -> C
		ld 		A,(HL)
		cp	 	$21
		jp 		M,.setCarry 		; less than '!'
		cp 		$7F						
		jp 		P,.setCarry			; bigger than '~'
		jr 		.nextChar			; char between A..F -> OK
.setCarry:
		ld 		A,'.'				; set resulting char = '.' in NOT char
		scf
		ret							; return with Carry, value in A is NOT char
.nextChar:	
		scf
		ccf
		ret							; return without Carry, value in A is char


	;****************************************************************************************************************
	;****************************************************************************************************************



				;****************************************************************************************************************
				;****************************************************************************************************************
				; String Concatenation (CONCAT)                                                                                            8B
				; 	is
				; 																with the Carry flag cleared (no errors) and string I
				; Examples
				; I.    Data:     Maximum length of string I = OE'6 = 1410       2.    Data:    String I = 07'JOHNSON' (07 is the length of
				; 				String 1= 07'JOHNSON' (07 is the length of                      the string)
				; 				the string)                                                 String 2= 09',RICHARD'(09 is the length of
				; 				String 2 = OS',DON' (OS is the length of the                    the string)
				; 				string)                                           Result:   String 1= OE'JOHNSON, RICHA' (OE'6 =
				; 	Result:    String 1= OC'JOHNSON, DON'(OC'6= 1210                            1410 is the maximum length allowed, so the
				; 				is the length of the combined string with                     last two characters of string 2 have been
				; 				string 2 placed after string I)                               dropped)
				; 				Carry = 0, since the concatenation did not                    Carry= I, since the concatenation produced
				; 				produce a string exceeding the maximum                        a string longer than the maximum length.
				; 				length.
				; 			Title                   String Concatenation
				; 			Name:                   CONCAT
				; 			Purpose:                 Concatenate 2 strings into one string
				; 			Entry:                  Register pair HL = Base address of string 1
				; 									Register pair DE = Base address of string 2
				; 									Register B = Maximum length of string 1
				; 										A string is a maximum of 255 bytes long plus
				; 										a length byte which precedes it.
				; 			Exit:                   String 1 := string 1 concatenated with string 2
				; 									If no errors then
				; 										CARRY := 0
				; 									else
				; 										begin
				; 											CARRY := 1
				; 											if the concatenation makes string 1 too
				; 											long, concatenate only enough of string 2
				; 											to give string 1 its maximum length.
				; 											if length(stringl) > maximum length then
				; 											no concatenation is done
				; 										end,
				; 		Registers used: AF.BC.DE.HL
				; 		Time:             Approximately 21 * (length of string 2) cycles
				; 							plus 288 cycles overhead
				; 		Size:             Program 83 bytes
				; 							Data     5 bytes
				;****************************************************************************************************************
				;****************************************************************************************************************



CONCAT:
		; DETERMINE WHERE TO START CONCATENATING
		; CONCATENATION STARTS AT THE END OF STRING 1
		; END OF STRING 1 = BASEl + LENGTHl + 1, WHERE
		; THE EXTRA 1 MAKES UP FOR THE LENGTH BYTE
		; NEW CHARACTERS COME FROM STRING 2, STARTING AT
		; BASE2 + 1 (SKIPPING OVER LENGTH BYTE)
		LD		(S1ADR),HL		;SAVE ADDRESS OF STRING 1
		PUSH	BC				;SAVE MAXIMUM LENGTH OF STRING 1
		LD		A, (HL)			;SAVE LENGTH OF STRING 1
		LD		(S1LEN),A
		LD		C,A				;ENDI = BASEl + LENGTHI + 1
		LD		B,0
		ADD		HL,BC
		INC		HL				;HL = START OF CONCATENATION
		LD		A,(DE)			;SAVE LENGTH OF STRING 2
		LD		(S2LEN),A
		INC		DE				;DE = FIRST CHARACTER OF STRING 2
		POP		BC				; RESTORE MAX I MUM LENGTH
			;DETERMINE HOW MANY CHARACTERS TO CONCATENATE
		LD      C,A				;ADD LENGTHS OF STRINGS
		LD      A,(S1LEN)
		ADD     A,C
		JR	    C,TOOLNG		; JUMP IF SUM EXCEEDS 255
		CP      B				;COMPARE TO MAXIMUM LENGTH
		JR      Z,LENOK			;JUMP IF NEW STRING IS MAX LENGTH
		JR      C,LENOK			; OR LESS
			; COMBINED STRING IS TOO LONG
			; INDICATE A STRING OVERFLOW, STRGOV := OFFH
			; NUMBER OF CHARACTERS TO CONCATENATE    MAXLEN - SlLEN
			; LENGTH OF STRING 1 = MAXIMUM LENGTH
TOOLNG:
		LD		A,0FFH			; INDICATE STRING OVERFLOW
		LD		(STRGOV),A
		LD		A, (S1LEN)		;CALCULATE MAXLEN - S1LEN
		LD		C,A
		LD		A,B
		SUB		C
		RET  	C				;EXIT IF ORIGINAL STRING TOO LONG
		LD		(S2LEN),A		;CHANGE S2LEN TO MAXLEN - SlLEN
		LD		A,B				;LENGTH OF STRING 1 = MAXIMUM
		LD		(S1LEN), A
		JR		DOCAT			;PERFORM CONCATENATION
			; RESULTING LENGTH DOES NOT EXCEED MAXIMUM
			; LENGTH OF STRING 1 = SILEN + S2LEN
			; INDICATE NO OVERFLOW. STRGOV := 0
			; NUMBER OF CHARACTERS TO CONCATENATE = LENGTH OF STRING 2
LENOK:
		LD		(S1LEN),A		;SAVE SUM OF LENGTHS
		SUB		A				;INDICATE NO OVERFLOW
		LD		(STRGOV),A
			; CONCATENATE STRINGS BY MOVING CHARACTERS FROM STRING 2
			; TO END OF STRING 1
DOCAT:
		LD		A,(S2LEN)		;GET NUMBER OF CHARACTERS
		OR		A
		JR		Z,EXIT			;EXIT IF NOTHING TO CONCATENATE
		LD		C,A				;BC = NUMBER OF CHARACTERS
		LD		B,0
		EX		DE,HL			;DE = DESTINATION
								;HL = SOURCE
		LDIR					;MOVE CHARACTERS
EXIT:
		LD      A,(S1LEN)		;ESTABLISH NEW LENGTH OF STRING    1
		LD      HL,(S1ADR)
		LD      (HL),A
		LD      A,(STRGOV)		;CARRY = 1 IF OVERFLOW. 0 IF NOT
		RRA
		RET
			;DATA
S1ADR:    DS      2				;BASE ADDRESS OF STRING 1
S1LEN:    DS      1				;LENGTH OF STRING 1
S2LEN:    DS      1				;LENGTH OF STRING 2
STRGOV:   DS      1				;STRING OVERFLOW FLAG



		; SAMPLE EXECUTION:


; SC8B:
; 		LD      HL,S1_8B           ;HL = BASE ADDRESS OF SI
; 		LD      DE,S2_8B           ;DE = BASE ADDRESS OF S2
; 		LD      B,20H           ;B = MAXIMUM LENGTH OF STRING 1
; 		CALL    CONCAT          ;CONCATENATE STRINGS
; 		JR      SC8B            ;RESULT OF CONCATENATING
; 								; "LASTNAME" AND ". FIRSTNAME"
; 								;IS S1 = 13H,"LASTNAME. FIRSTNAME"



				;****************************************************************************************************************
				;****************************************************************************************************************
				; Find the Position of a Substring (POS)                                                                                    8C
				; Examples
				; I.    Data:      String = lD'ENTER SPEED IN MILES                      3.    Data:    String= IO'LETYI = Xl + R7'(1016= 16 10 is
				; 				PER HOUR' (lD 16 = 29 10 is the length of                            the length of the string)
				; 				the string)                                                        Substring = 02'R4' (02 is the length of the
				; 				Substring = OS'MILES' (05 is the length of                             substring)
				; 				the substring)                                           Result:   A contains 0, since the substring 'R4' does not
				; 	Result:     A contains 10 16 (16 10 ), the index at which the                      appear in the string LET YI = Xl + R7.
				; 				substring 'MILES' starts.
				; 																	4.    Data:    String = 07'RESTORE' (07 is the length of
				; 																						the string)
				; 																					Substring = 03'RES' (03 is the length of the
				; 2.    Data:      String= IB'SALES FIGURES FOR JUNE                                      substring)
				; 					1981 '(IBI6 = 2710 is the length of the string)
				; 																			Result:   A contains I, the index at which the substring
				; 				Substring = 04'JUNE' (04 is the length of the
				; 																						'RES' starts. An index of I indicates that
				; 				substring)
				; 																						the substring could be an abbreviation of
				; 	Result:     A contains 13 16 (19 10 ), the index at which the                      the string. Interactive programs, such as
				; 				substring 'JUNE' starts.                                             BASIC intepreters and word processors,
				; 																						often use such abbreviations to save on
				; 																						typing and storage.
				; 				Title                        Find the position of a substring in a string
				; 				Name:                        P~S
				; 				Purpose:                     Search for the first occurrence of a substring
				; 											within a string and return its starting index.
				; 											If the substring is not found a 0 is returned.
				; 				Entry:                       Register pair HL = Base address of string
				; 											Register pair DE = Base address of substring
				; 												A string is a maximum of 255 bytes long plus
				; 												a length byte which precedes it.
				; 				Exit:                        If the substring is found then
				; 											Register A = its starting index
				; 											else
				; 											Register A = 0
				; 				Registers used: AF,BC,DE,HL
				; 				Time:                        Since the algorithm is so data-dependent,
				; 						a simple formula is impossible, but the
				; 						following statements are true. and a
				; 						worst case is given.
				; 						154 cycles overhead
				; 						Each match of 1 character takes 56 cycles
				; 						A mismatch takes 148 cycles
				; 						Worst case timing will be when the
				; 						string and substring always match
				; 						except for the last character of the
				; 						substring. such as
				; 							string = ~AAAAAAAAAB~
				; 							substring = ~AAB'
				; 		Size:              Program 69 bytes
				; 						Data     7 bytes
				;****************************************************************************************************************
				;****************************************************************************************************************


POS:
			; SET UP TEMPORARIES
			; EXIT IF STRING OR SUBSTRING HAS ZERO LENGTH
		LD		(STRING),HL		;SAVE STRING ADDRESS
		EX		DE,HL
		LD		A, (HL)			;TEST LENGTH OF SUBSTRING
		OR		A
		JR		Z,NOTFND		;EXIT IF LENGTH OF SUBSTRING = 0
		INC		HL				;MOVE PAST LENGTH BYTE OF SUBSTRING
		LD		(SUBSTG),HL		;SAVE SUBSTRING ADDRESS
		LD		(SUBLEN),A
		LD		C,A				;C = SUBSTRING LENGTH
		LD		A, (DE)			;TEST LENGTH OF STRING
		OR		A
		JR		Z,NOTFND			;EXIT IF LENGTH OF STRING = 0
			; NUMBER OF SEARCHES = STRING LENGTH - SUBSTRING LENGTH
			;  + 1. AFTER THAT. NO USE SEARCHING SINCE THERE AREN'T
			;  ENOUGH CHARACTERS LEFT TO HOLD SUBSTRING
			; 
			; IF SUBSTRING IS LONGER THAN STRING, EXIT IMMEDIATELY AND
			; INDICATE SUBSTRING NOT FOUND
		SUB		C				;A = STRING LENGTH - SUBSTRING LENGTH
		JR		C,NOTFND		;EXIT IF STRING SHORTER THAN SUBSTRING
		INC		A				;COUNT = DIFFERENCE IN LENGTHS + 1
		LD		B,A
		SUB		A				;INITIAL STARTING INDEX = 0
		LD		(INDEX),A

			; ,SEARCH UNTIL REMAINING STRING SHORTER THAN SUBSTRING
SLP1:
		LD		HL,INDEX		;INCREMENT STARTING INDEX
		INC		(HL)
		LD		HL,SUBLEN		;C = LENGTH OF SUBSTRING
		LD		C, (HL)
		LD		HL, (STRING)	; INCREMENT TO NEXT BYTE OF STRING
		INC		HL
		LD		(STRING),HL		;HL = NEXT ADDRESS IN STRING
		LD		DE, (SUBSTG)	; DE = STARTING ADDRESS OF SUBSTRING
								;C = CURRENT VALUE OF COUNT
		;TRY TO MATCH SUBSTRING STARTING AT INDEX
		;MATCH INVOLVES COMPARING CORRESPONDING CHARACTERS
		; ONE AT A TIME
cmpLoop_8C:
		LD		A, (DE)			;GET A CHARACTER OF SUBSTRING
		CP		(HL)			;COMPARE TO CHARACTER OF STRING
		JR		NZ,SLP2			; ,JUMP IF NOT SAME
		DEC		C
		JR		Z,FOUND			; ,JUMP IF SUBSTRING FOUND
		INC		HL				; PROCEED TO NEXT CHARACTERS
		INC		DE
		JR		cmpLoop_8C
		; ARRIVE HERE IF MATCH FAILS, SUBSTRING NOT YET FOUND
SLP2:	
		DJNZ	SLP1			;TRY NEXT HIGHER INDEX IF
								; ENOUGH STRING LEFT
		JR		NOTFND			;ELSE EXIT NOT FOUND
		;FOUND SUBSTRING, RETURN ITS STARTING INDEX
FOUND:
		LD		A, (INDEX)		;SUBSTRING FOUND, A    STARTING INDEX
		RET
		;COULD NOT FIND SUBSTRING, RETURN 0 AS INDEX
NOTFND:
		SUB		A				;SUBSTRING NOT FOUND, A = 0
		RET
		;DATA
STRING:		DS	2				;BASE ADDRESS OF STRING
SUBSTG:		DS	2				;BASE ADDRESS OF SUBSTRING
SLEN:		DS	1				;LENGTH OF STRING
SUBLEN:		DS	1				;LENGTH OF SUBSTRING
INDEX:		DS	1				;CURRENT INDEX INTO STRING

		; SAMPLE EXECUTION:
SC8C:
		LD		HL,STG_8C			;HL = BASE ADDRESS OF STRING
		LD		DE,SSTG_8C		;DE = BASE ADDRESS OF SUBSTRING
		CALL	POS				;FIND POSITION OF SUBSTRING
								; SEARCHING "AAAAAAAAAB" FOR "AAB"
								; RESULTS IN REGISTER A = 8
		JR		SC8C			;LOOP   FOR ANOTHER TEST
			; ~TEST   DATA, CHANGE FOR OTHER VALUES
STG_8C:	DB		0AH				;LENGTH OF STRING
		DB		"AAAAAAAAAB                       " ;32 BYTE MAX LENGTH
SSTG_8C:	DB		3H				;LENGTH OF SUBSTRING
		DB		"AAB                              " ;32 BYTE MAX LENGTH

				;****************************************************************************************************************
				;****************************************************************************************************************
				; Copy a Substring from a String (COPY)                                                                                        8D
				; 			°
				; length of and the Carry flag will be set to 1. If                the substring, the Carry flag is cleared.
				; Examples
				; I.    Data:    String= IO'LET YI = R7 + X4'                      3.    Data:    String = 16'9414 HEGENBERGER DRIVE'
				; 				(10'6 = 16 10 is the length of the string)                       (16'6 = 2210 is the length of the string)
				; 			Maximum length of substring = 2                                  Maximum length of substring = 10'6 = 16 10
				; 			Number of bytes to copy = 2                                      Number of bytes to copy = 11'6 = 1710
				; 			Starting index = 5                                               Starting index = 6
				; 	Result:   Substring = 02'Y I' (2 is the length of the            Result:   Substring = IO'HEGENBERGER DRIV'
				; 				substring)                                                       (10'6 = 16 10 is the length of the substring)
				; 			Two bytes from the string were copied,                           Carry= I, since the number of bytes to copy
				; 				starting at character #5 (that is, characters                    exceeded the maximum length of the sub-
				; 				5 and 6)                                                         string.
				; 			Carry = 0, since no problems occurred in
				; 				forming the substring.
				; 2.    Data:    String = OE'8657 POWELL ST'
				; 				(OE'6 = 1410 is the length of the string)
				; 			Maximum length of substring = 10'6 = 16 10
				; 			Number of bytes to copy = OD'6 = 1310
				; 			Starting index = 6
				; 	Result:   Substring = 09'POWELL ST' (09 is the
				; 				length of the substring)
				; 			Carry = I, since there were not enough
				; 				characters available in the string to provide
				; 				the specified number of bytes to copy.
				; 			Tit le                     Copy a substring from a string
				; 			Name:                      COpy
				; 		Purpose:             Copy a substring from a string given a starting
				; 							index and the number of bytes
				; 		Entry:               Register pair HL = Address of source string      ~
				; 							Register pair DE = Address of destination string~
				; 							Register A = Maximum length of destination
				; 											string
				; 							Register B = Number of bytes to copy
				; 							Register C   Starting index into source string
				; 										Index of 1 is first character of
				; 										string
				; 								A string is a maximum of 255 bytes long plus
				; 								a length byte which precedes it.
				; 		Exit:                Destination string := The substring from the
				; 							string.
				; 							if no errors then
				; 								CARRY := 0
				; 							else
				; 								begin
				; 								the following conditions cause an
				; 								error and the CARRY flag = 1.
				; 								if (index = 0) or (maxlen = 0) or
				; 										(index> length(source» then
				; 									the destination string will have a zero
				; 									length.
				; 								if (index + count - 1) > length(source)
				; 								then
				; 									the destination string becomes everything
				; 									from index to the end of source string.
				; 								END~
				; 		Registers used: AF.BC.DE.HL
				; 		Time:                Approximately (21   *   count) cycles plus 237
				; 							cycles overhead.
				; 		Size:                Program 73 bytes
				; 							Data     2 bytes
				;****************************************************************************************************************
				;****************************************************************************************************************


COPY:
			; SAVE    MAXIMUM LENGTH OF DESTINATION STRING
		LD		(MAXLEN),A		;SAVE MAXIMUM LENGTH
			; INITIALIZE    LENGTH OF DESTINATION STRING AND ERROR FLAG
		SUB		A
		LD		(DE),A			;LENGTH OF DESTINATION STRING = ZERO
		LD		(CPYERR),A		;ASSUME NO ERRORS

			; IF NUMBER OF BYTES TO COPY IS O. EXIT WITH NO ERRORS
		OR		B				;TEST NUMBER OF BYTES TO COPY
		RET		Z               ;EXIT WITH NO ERRORS
								; CARRY = 0
			;IF MAXIMUM LENGTH IS 0, TAKE ERROR EXIT
		LD		A, (MAXLEN)		;TEST MAXIMUM LENGTH
		OR		A
		JR		Z,EREXIT		;ERROR EXIT IF MAX LENGTH IS 0
			;IF STARTING INDEX IS ZERO, TAKE ERROR EXIT
		LD		A,C				;TEST STARTING INDEX
		OR		A
		JR		Z,EREXIT		;ERROR EXIT IF INDEX IS 0
			;IF STARTING INDEX IS GREATER THAN LENGTH OF SOURCE
			; STRING, TAKE ERROR EXIT
		LD		A, (HL)			;GET LENGTH OF SOURCE STRING
		CP		C				;COMPARE TO STARTING INDEX
		RET		C				;ERROR EXIT IF LENGTH LESS THAN INDEX
								; CARRY = 1
		; CHECK IF COPY AREA FITS IN SOURCE STRING
		; OTHERWISE, COPY ONLY TO END OF STRING
		; COPY AREA FITS IF STARTING INDEX + NUMBER OF
		; CHARACTERS TO COPY - 1 IS LESS THAN OR EQUAL TO
		; LENGTH OF SOURCE STRING
		; NOTE THAT STRINGS ARE NEVER MORE THAN 255 BYTES LONG
		LD		A,C				;FORM STARTING INDEX + COpy LENGTH
		ADD		A,B
		JR		C,RECALC		;JUMP IF SUM> 255
		DEC		A
		CP		(HL)
		JR		C,CNT1OK		;JUMP IF MORE THAN ENOUGH TO COPY
		JR		Z,CNT1OK		;JUMP IF EXACTLY ENOUGH
		;CALLER ASKED FOR TOO MANY CHARACTERS. RETURN EVERYTHING
		; BETWEEN INDEX AND END OF SOURCE STRING.
		; SET COUNT := LENGTH(SOURCE) - INDEX + 1;
RECALC:
		LD		A,0FFH			; INDICATE TRUNCATION OF COUNT
		LD		(CPYERR),A
		LD		A, (HL)			;COUNT   = LENGTH   - INDEX +   1
		SUB		C
		INC		A
		LD		B,A				;CHANGE NUMBER OF BYTES
			; CHECK IF COUNT LESS THAN OR EQUAL TO MAXIMUM LENGTH OF
			; DESTINATION STRING. IF NOT, SET COUNT TO MAXIMUM LENGTH
			; IF COUNT > MAXLEN THEN
			; COUNT := MAXLEN
CNT1OK:
		LD		A, (MAXLEN)		;IS MAX LENGTH LARGE ENOUGH?
		CP		B
		JR		NC,CNT2OK		; ~UMP IF IT IS
		LD		B,A				;ELSE LIMIT COPY TO MAXLEN
		LD		A,0FFH			; INDICATE STRING OVERFLOW
		LD		(CPYERR),A
			; MOVE    SUBSTRING TO DESTINATION STRING
CNT2OK:
		LD		A,B				;TEST    NUMBER OF BYTES TO COPY
		OR		A
		JR		Z,EREXIT		;ERROR    EXIT IF NO BYTES TO COpy
		LD		B,0				;START    COPYING AT STARTING INDEX
		ADD		HL,BC
		LD		(DE), A			;SET LENGTH OF DESTINATION STRING
		LD		C,A				;RESTORE NUMBER OF BYTES
		INC		DE				;MOVE DESTINATION ADDRESS PAST
								; LENGTH BYTE
		LDIR					;COPY SUBSTRING

			; CHECK    FOR COPY ERROR
		LD		A, (CPYERR)		;TEST    FOR ERRORS
OKEXIT_8D:
		OR		A
		RET		Z               ;RETURN    WITH C = o IF NO ERRORS
			; ERROR    EXIT
EREXIT:
		SCF						;SET    CARRY TO INDICATE AN ERROR
		RET
			; DATA    SECTION
MAXLEN:	DS	1					;MAXIMUM    LENGTH OF DESTINATION STRING
CPYERR: DS	1					;COPY    ERROR FLAG

		; SAMPLE EXECUTION:
SC8D:
		LD		HL,SSTG_8D			;SOURCE STRING
		LD		DE, DSTG_8D		;DESTINATION STRING
		LD		A,(IDX_8D)
		LD		C,A				;STARTING    INDEX FOR COPYING
		LD		A, (CNT_8D)
		LD		B,A				;NUMBER OF BYTES TO COPY
		LD		A, (MXLEN_8D)		;MAXIMUM LENGTH OF SUBSTRING
		CALL	COPY			;COPY SUBSTRING
								;COPYING 3 CHARACTERS STARTING AT
								;INDEX 4 FROM '12.345E+l0' GIVES '345'
		JR		SC8D            ;LOOP FOR MORE TESTING
			; DATA    SECTION
IDX_8D:	DB	4					;STARTING INDEX FOR COPYING
CNT_8D:	DB	3					;NUMBER OF CHARACTERS TO COPY
MXLEN_8D:	DB	20H					; MAXIMUM LENGTH OF DESTINATION STRING
SSTG_8D:	DB	0AH					;LENGTH OF STRING
		DB	"12.345E+10                     " ;32 BYTE MAX LENGTH
DSTG_8D:	DB	0					;LENGTH OF SUBSTRING
		DB  "                               " ; 32 BYTE MAX LENGTH



				;****************************************************************************************************************
				;****************************************************************************************************************
				; Delete a Substring from a String (DELETE)                                                                                       8E

				; 		1. STRING LENGTH = 20 16 (3210)                             1. If the number of bytes to delete is 0, the
				; 			STARTING INDEX = 19 16 (25 10)                        program exits with the Carry flag cleared (no errors)
				; 			NUMBER OF BYTES TO DELETE = 08                        and the string unchanged.
				; 		Since there are exactly eight bytes left in the string       2. If the string does not even extend to the specified
				; 	starting at index 19 16 , all the routine must do is trun-   starting index, the program exits with the Carry flag
				; 	cate (that is, cut off the end of the string). This takes    set to I (indicating an error) and the string unchanged.
				; 			21 * 0 + 224 = 224 cycles                                3. If the number of bytes to delete exceeds the
				; 		2. STRING LENGTH = 40 16 (6410)                           number available, the program deletes all bytes from
				; 			STARTING INDEX = 19 16 (25 10 )                       the starting index to the end of the string and exits
				; 			NUMBER OF BYTES TO DELETE = 08                        with the Carry flag set to I (indicating an error).




				; Entry Conditions                                                   Exit Conditions
				; Base address of string in HL                                       Substring deleted from string. If no errors occur,
				; Number of bytes to delete in B                                     the Carry flag is cleared. If the starting index is 0
				; Starting index to delete from in C                                 or beyond the length of the string, the Carry flag
				; 																is set and the string is unchanged. If the number
				; 																of bytes to delete would go beyond the end ofthe
				; 																string, the Carry flag is set and the characters
				; 																from the starting index to the end of the string
				; 																are deleted.



				; Examples
				; I.    Data:     String = 26'SALES FOR MARCH AND                 2.    Data:    String = 28'THE PRICE IS $3.00 ($2.00
				; 				APRIL OF THIS YEAR'                                            BEFORE JUNE I)' (28 16 = 40 10 is the
				; 				(26 16 = 3810 is the length of the string)                     length of the string)
				; 				Number of bytes to delete = OA 16 = 1010                       Number of bytes to delete = 30 16 = 48 10
				; 				Starting index to delete from = 10 16 = 16 10                  Starting index to delete from = 13 16 = 19 10
				; 	Result:    String= I C 'SALES FOR MARCH OF THIS                 Result:   String = 12'THE PRICE IS $3.00' (12 16 =
				; 				YEAR' (IC I6 = 28 10 is the length of the                      18 10 is the length of the string with all
				; 				string with ten bytes deleted starting with                    remaining bytes deleted)
				; 				the 16th character-the deleted material is                   Carry = I, since there were not as many bytes
				; 				'AND APRIL')                                                   left in the string as were supposed to be
				; 				Carry= 0, since no problems occurred in the                      deleted.
				; 				deletion.




				; 			Title:                   Delete a substring from a string
				; 			Name:                    Delete



				; 			Purpose:                 Delete a substring from a string given a
				; 										starting index and a length
				; 			Entry:                   Register pair HL = Base address of string
				; 										Register B = Number of bytes to delete
				; 										Register C = Starting index into the string.
				; 												An index of 1 is the first character
				; 										A string is a maximum of 255 bytes long plus
				; 										a length byte which precedes it.
				; 			Exit :                   Substring deleted.
				; 										if no errors then
				; 										CARRY := 0
				; 										else

				; 							begin
				; 								the following conditions cause an
				; 								error with .CARRY = 1.
				; 								if (index = 0) or (index> length(string»
				; 								then do not change string
				; 								if count is too large then
				; 								delete only the characters from
				; 								index to end of string

				; 		Registers used: AF,BC,DE,HL
				; 		Time:             Approximately 21 * (LENGTH(STRG)-INDEX-COUNT+l)
				; 							plus 224 cycles overhead
				; 		Size:             Program 58 bytes
				; 							Data     1 bytes
				;****************************************************************************************************************
				;****************************************************************************************************************



DELETE:
			;INITIALIZE ERROR INDICATOR (DELERR) TO 0
		SUB		A
		LD		(DELERR),A		;ASSUME NO ERRORS
			;CHECK IF COUNT AND INDEX ARE BOTH NON-ZERO
		OR		B				;TEST NUMBER OF BYTES TO DELETE
		RET		Z				;RETURN WITH CARRY = 0 (NO ERRORS) IF
								; 0 BYTES TO DELETE
		LD		A,C				;TEST STARTING INDEX
		OR		A
		SCF						;CARRY = 1
		RET		Z				;ERROR EXIT (CARRY = 1) IF
			; STARTING INDEX = 0
			; CHECK IF STARTING INDEX WITHIN STRING
			; ERROR EXIT IF NOT
		LD		A, (HL)			;GET LENGTH
		CP		C				;IS INDEX WITHIN STRING?
		RET		C				;NO, TAKE ERROR EXIT
			;BE SURE ENOUGH CHARACTERS ARE AVAILABLE
			; IF NOT, DELETE ONLY TO END OF STRING
			; IF INDEX + NUMBER OF CHARACTERS - 1 > LENGTH(STRING) THEN
			; NUMBER OF CHARACTERS := LENGTH(STRING) - INDEX + 1
		LD		A,C				;,GET INDEX
		ADD		A,B				;ADD NUMBER OF CHARACTERS TO DELETE
		JR		C,TRUNC			;TRUNCATE IF SUM> 255
		LD		E,A				;SAVE SUM AS STARTING INDEX FOR MOVE
		DEC		A
		CP		(HL)			;COMPARE TO LENGTH
		JR		C,CNTOK			;JUMP IF ENOUGH CHARACTERS AVAILABLE
		JR		Z,TRUNC			;TRUNCATE BUT NO ERRORS (EXACTLY ENOUGH
								;CHARACTERS)
		LD		A,0FFH			;INDICATE ERROR - NOT ENOUGH CHARACTERS
		LD		(DELERR),A		;AVAILABLE FOR DELETION
			;TRUNCATE STRING - NO COMPACTING NECESSARY
			; STRING LENGTH = INDEX - 1
TRUNC:
		LD		A,C				;STRING LENGTH   = INDEX     - 1
		DEC		A
		LD		(HL),A
		LD		A, (DELERR)
		RRA						;CARRY   =0   IF NO ERRORS
		RET						;EXIT
			;DELETE SUBSTRING BY COMPACTING
			; MOVE ALL CHARACTERS ABOVE DELETED AREA DOWN
			;NEW LENGTH = OLD LENGTH - NUMBER OF BYTES TO DELETE
CNTOK:
		LD		A, (HL)
		LD		D,A				;SAVE OLD LENGTH
		SUB		B				;SET NEW LENGTH
		LD		(HL),A
			;CALCULATE NUMBER OF CHARACTERS TO MOVE
			; NUMBER = STRING LENGTH - (INDEX + NUMBER OF BYTES) + 1
		LD		A,D				;OET OLD LENGTH
		SUB		E				;SUBTRACT INDEX + NUMBER OF BYTES
		INC		A				;A = NUMBER OF CHARACTERS TO MOVE
			;CALCULATE SOURCE AND DESTINATION ADDRESSES FOR MOVE
			;SOURCE = BASE + INDEX + NUMBER OF BYTES TO DELETE
			;DESTINATION = BASE + INDEX
		PUSH	HL				;SAVE STRING ADDRESS
		LD		B,0				;DESTINATION = BASE + INDEX
		ADD		HL,BC
		EX		(SP),HL			; SOURCE = BASE + INDEX + NUMBER
		LD		D,0				;OF BYTES TO DELETE
		ADD		HL,DE			;HL = SOURCE (ABOVE DELETED AREA)
		POP		DE				;DE = DESTINATION
		LD		C,A				;BC = NUMBER OF CHARACTERS TO MOVE
		LDIR					;COMPACT STRING BY MOVING DOWN
		; GOOD EXIT
OKEXIT_8E:
		OR		A					;CLEAR CARRY, NO ERRORS
		RET
		; DATA
DELERR: DS	1						;DELETE ERROR FLAG

		; SAMPLE EXECUTION:

SC8E:
		LD		HL,SSTG_8E 			;HL   = BASE   ADDRESS OF STRING
		LD		A,(IDX_8E)
		LD		C,A					;C    = STARTING INDEX FOR DELETION
		LD		A, (CNT_8E)
		LD		B,A					;B= NUMBER OF CHARACTERS TO DELETE
		CALL	DELETE				;DELETE CHARACTERS
									;DELETING 4 CHARACTERS STARTING AT INDEX 1
									; FROM ".JOE HANDOVER" LEAVES "HANDOVER"
		JR		SC8E				;LOOP FOR ANOTHER TEST
			; DATA SECTION
IDX_8E:	DB      1                    ;STARTING INDEX FOR DELETION
CNT_8E:	DB      4                    ;NUMBER OF CHARACTERS TO DELETE
SSTG_8E:	DB      12                   ;LENGTH OF STRING
		DB      ".JOE HANDOVER"



				;****************************************************************************************************************
				;****************************************************************************************************************
				; Insert a Substring into a String (INSERT)                                                                                      8F
				; 	produces a string longer than the maximum.
				; 																Program Size: 90 bytes
				; 	Examples
				; 																Data Memory Required: One byte anywhere in
				; 		1. STRING LENGTH = 20'6 (3210)                            RAM for an error flag (address INS ERR).
				; 			STARTING INDEX = 19'6 (25 10 )
				; 			MAXIMUM LENGTH = 30'6 (48 10 )                         Special Cases:
				; 			SUBSTRING LENGTH = 06                                     I. If the length ofthe substring (the insertion) is 0,
				; 		We want to insert a substring six bytes long, start-       the program exits with the Carry flag cleared (no
				; 	ing at the 25th character. Since eight bytes must be         errors) and the string unchanged.
				; 			2. If the starting index for the insertion is 0 (that      4. If the starting index of the insertion is beyond
				; 		is, the insertion would start in the length byte), the      the end of the string, the program concatenates the
				; 		program exits with the Carry flag set to I (indicating      insertion at the end of the string and indicates an
				; 		an error) and the string unchanged.                         error by setting the Carry flag to I.
				; 		3. If the string with the substring inserted exceeds         5. If the original length of the string exceeds its
				; 		the specified maximum length, the program inserts           specified maximum length, the program exits with
				; 		only enough characters to reach the maximum length.         the Carry flag set to 1 (indicating an error) and the
				; 		The Carry flag is set to I to indicate that the insertion   string unchanged.
				; 		has been truncated.
				; Examples
				; I.    Data:    String = OA'JOHN SMITH' (OA'6 = 1010 is               2.    Data:    String = OA'JOHN SMITH' (OA'6 = 1010 is
				; 				the length of the string)                                            the length of the string)
				; 			Substring = 08'WILLIAM' (08 is the length                            Substring = OC'ROCKEFELLER' (OC'6 =
				; 				of the substring)                                                     1210 is the length of the substring)
				; 			Maximum length of string = 14'6 = 20 10                              Maximum length of string = 14'6 = 20 10
				; 			Starting index = 06                                                  Starting index = 06
				; 	Result:   String = 12'JOHN WILLIAM SMITH'                            Result:   String= 14'JOHN ROCKEFELLESMITH'
				; 				(12'6 = 18 10 is the length of the string                            (14'6 = 20 10 is the length ofthe string with
				; 				with the substring inserted)                                         as much of the substring inserted as the
				; 			Carry = 0, since no problems occurred in the                           maximum length would allow)
				; 				insertion.                                                         Carry = I, since some of the substring could
				; 																					not be inserted without exceeding the maxi-
				; 																					mum length of the string.
				; 		Tit Ie:         Insert a substring into a string
				; 		Name:           Insert
				; 		Purpose:        Insert a substring into a string given a
				; 						starting index
				; 		Entry:          Register pair HL     Address of string
				; 						Register pair DE     Address of substring to
				; 												insert
				; 						Register B   Maximum length of string
				; 						Register C = Starting index to insert the
				; 									sl.Jbstring
				; 							A string is a maximum of 255 bytes long plus
				; 							a length byte which precedes it.
				; 		Exit:           Substring inserted into string.
				; 						if no errors then
				; 							CARRY = 0
				; 						else
				; 							begin
				; 							the following conditions cause the
				; 							CARRY flag to be set.
				; 							if index = 0 then
				; 								do not insert the substring
				; 							if length(strg) > maximum length then
				; 								do not insert the substring
				; 							if index> length(strg) then
				; 								concatenate substg onto the end of the
				; 								source string
				; 							if length(strg)+length(substring) > maxlen
				; 								then insert only enough of the substring
				; 								to reach maximum length
				; 							end;
				; 		Registers used: AF,BC,DE,HL
				; 		Time:           Approxi'matel y
				; 						21 * (LENGTH(STRG) - INDEX + 1) +
				; 						21 * (LENGTH(SUBSTG»  +
				; 						290 cycles overhead
				; 		Size:           Program 90 bytes
				; 						Data     1 byte
				;****************************************************************************************************************
				;****************************************************************************************************************



INSERT_STR:
			;INITIALIZE ERROR FLAG
		SUB		A				;ERROR FLAG    o (NO ERRORS)
		LD		(INSERR) ,A
			;GET SUBSTRING AND STRING LENGTHS
			; IF LENGTH(SUBSTG) = 0 THEN EXIT BUT NO ERROR
		LD		A, (DE)			; TEST LENGTH OF SUBSTRING
		OR		A
		RET		Z				;EXIT IF SUBSTRING EMPTY
								; CARRY = 0 (NO ERRORS)
			;IF STARTING INDEX IS ZERO, TAKE ERROR EXIT
IDX0:
		LD		A,C				;TEST STARTING INDEX
		OR		A
		SCF						;ASSUME AN ERROR
		RET		Z				;RETURN WITH ERROR IF INDEX = 0
			;CHECK WHETHER INSERTION WILL MAKE STRING TOO LONG
			; IF IT WILL. TRUNCATE SUBSTRING AND SET
			; TRUNCATION FLAG.
			; INSERTION TOO LONG IF STRING LENGTH + SUBSTRING LENGTH
			; EXCEEDS MAXIMUM LENGTH. REMEMBER. STRINGS CANNOT BE
			; MORE THAN 255 BYTES LONG
CHKLEN:
		LD		A,(DE)			;TOTAL = STRING + SUBSTRING
		ADD		A,(HL)
		JR		C,TRUNC_8F			;TRUNCATE SUBSTRING IF NEW LENGTH> 255
		CP		B				;COMPARE TO MAXIMUM LENGTH OF STRING
		LD		A,(DE)			;A = LENGTH OF SUBSTRING
		JR		C,IDXLEN		;JUMP IF TOTAL < MAX LENGTH
		JR		Z,IDXLEN		; OR EQUAL
			;SUBSTRING DOES NOT FIT. SO TRUNCATE IT
			; SET ERROR FLAG TO INDICATE TRUNCATION
			; LENGTH THAT FITS = MAXIMUM LENGTH - STRING LENGTH
TRUNC_8F:
		LD		A,0FFH          ; INDICATE SUBSTRING TRUNCATED
		LD		(INSERR), A
		LD		A,B             ;LENGTH = MAX - STRING LENGTH
		SUB		(HL)
		RET		C               ;RETURN WITH ERROR IF STRING TOO
		SCF						; LONG INITIALLY OR ALREADY MAX
		RET		Z               ; LENGTH SO NO ROOM FOR SUBSTRING
			;CHECK IF INDEX WITHIN STRING. IF NOT, CONCATENATE
			; SUBSTRING ONTO END OF STRING
IDXLEN:
		LD		B,A				;B = LENGTH OF SUBSTRING
		LD		A,(HL)			;GET STRING LENGTH
		CP	C               ;COMPARE TO INDEX
		JR		NC,LENOK_8F		;JUMP IF STARTING INDEX WITHIN STRING
			;INDEX NOT WITHIN STRING, SO CONCATENATE
			; NEW LENGTH OF STRING = OLD LENGTH + SUBSTRING LENGTH
		LD		C, A			; SAVE CURRENT STRING LENGTH
		ADD		A, B			; ADD LENGTH OF SUBSTRING
		LD		(HL),A			;SET NEW LENGTH OF STRING
			;SET ADDRESSES FOR CONCATENATION
			; DE = STRING ADDRESS + LENGTH(STRING) + 1
			; HL = SUBSTRING ADDRESS
		EX		DE,HL			;HL        SUBSTRING ADDRESS
		LD		A,C				;DE      = END OF STRING
		INC		A
		ADD		A,E
		LD		E,A
		JR		NC,IDXL1
		INC		D
IDXL1:
		LD		A,0FFH			;INDICATE INSERTION ERROR
		LD		(INSERR), A
		JR		MVESUB			;JUST MOVE, NOTHING TO OPEN UP
			; OPEN UP SPACE IN SOURCE STRING FOR SUBSTRING BY MOVING
			; CHARACTERS FROM END OF SOURCE STRING DOWN TO INDEX, UP BY
			; SIZE OF SUBSTRING.
			; A = LENGTH(STRING)
LENOK_8F:
		PUSH    BC				;SAVE LENGTH OF SUBSTRING
		PUSH    DE				;,SAVE ADDRESS OF SUBSTRING
			;NEW LENGTH OF STRING   = OLD    LENGTH + SUBSTRING LENGTH
		LD		E,A			; DE   = STRING LENGTH
		LD		D,0
		ADD		A,B
		LD		(HL),A			;STORE NEW LENGTH OF STRING
			; CALCULATE NUMBER OF CHARACTERS TO MOVE
			; = STRING LENGTH - STARTING INDEX + 1
		LD		A,E				;GET ORIGINAL LENGTH OF STRING
		SUB		C
		INC		A				;A     = NUMBER   OF CHARACTERS TO MOVE
			;CALCULATE ADDRESS OF LAST CHARACTER IN STRING. THIS IS
			; SOURCE ADDRESS = STRING ADDRESS + LENGTH(STRING)
		ADD	HL,DE				;HL POINTS TO LAST CHARACTER IN STRING
		LD		E,L				;DE ALSO
		LD		D,H
			;CALCULATE DESTINATION ADDRESS
			; = STRING ADDRESS + LENGTH(STRING) + LENGTH OF SUBSTRING
			;THIS MOVE MUST START AT HIGHEST ADDRESS AND WORK DOWN
			; TO AVOID OVERWRITING PART OF THE STRING
		LD		C,B				;BC = LENGTH OF SUBSTRING
		LD		B,0
		ADD		HL,BC
		EX		DE,HL			;HL = SOURCE ADDRESS
								;DE = DESTINATION ADDRESS
		LD		C,A             ;BC = NLiMBER OF CHARACTERS TO MOVE
		LDDR					;OPEN UP FOR SUBSTRING
			;RESTORE REGISTERS
		EX		DE,HL
		INC		DE				;DE = ADDRESS TO MOVE STRING TO
		POP		HL				;HL = ADDRESS OF SUBSTRING
		POP		BC				;B = LENGTH OF SUBSTRING
			;MOVE SUBSTRING INTO OPEN AREA
			; HL = ADDRESS OF SUBSTRING
			; DE = ADDRESS TO MOVE SUBSTRING TO
			; C = LENGTH OF SUBSTRING
MVESUB:
		INC		HL				;INCREMENT PAST LENGTH BYTE OF SUBSTRING
		LD		C,B				;BC = LENGTH OF SUBSTRING TO MOVE
		LD		B,0
		LDIR					;MOVE SUBSTRING INTO OPEN AREA
		LD		A, (INSERR)		;GET ERROR FLAG
		RRA						;IF INSERR <> 0 THEN CARRY = 1
								;TO INDICATE AN ERROR
		RET
			;DATA SECTION
INSERR: DS	1					;FLAG USED TO INDICATE ERROR

		; SAMPLE EXECUTION:
SC8F:
		LD		HL,STG_8F 		;HL = BASE ADDRESS OF STRING
		LD		DE,SSTG_8F		;DE = BASE ADDRESS OF SUBSTRING
		LD		A,(IDX_8F)
		LD		C,A				;C = STARTING INDEX FOR INSERTION
		LD		A,(MXLEN_8F)
		LD		B,A				;B = MAXIMUM LENGTH OF STRING
		CALL	INSERT_STR		;INSERT SUBSTRING
								;RESULT OF INSERTING ~-~ INTO ~123456~ AT
								; INDEX 1 IS ~-123456~
		JR		SC8F			;LOOP FOR ANOTHER TEST
			; DATA SECTION
IDX_8F:	DB	1					;STARTING INDEX FOR I NSERTI ON
MXLEN_8F:	DB	20H					;MAXIMUM LENGTH OF DESTINATION
STG_8F:	DB	06H					;LENGTH OF STRING
		DB	"123456                                 " ;32 BYTE MAX LENGTH
SSTG_8F:	DB	1					;LENGTH OF SUBSTRING
		DB  "-                                      " ;32 BYTE MAX LENGTH

;********************************************************************************************
;********************************************************************************************	
		;		Copy the area of variables to RAM memory  (F000)
Init_RAM_HEAP:
		GLOBAL	Init_RAM_HEAP

		ld		DE,SRAM_VAR_START		; defined in linker script
		ld		hl,zero_byte
		
		ld 		BC,HEAP_SIZE			; defined in linker script
.cl_vars:
		ldi
		dec 	hl
		jp		PE,.cl_vars			; 		P/V is set if BC – 1 ≠ 0; otherwise, it is reset.
		ret
zero_byte:	db  0
;********************************************************************************************
;***************************************************************************************************
;***************************************************************************************************

		; 		dump memory content to screen. alt 1  dm 100  < without address>
		; 										alt 2  dm $1234,100  < with address>


dumpMemory:
		; ***	Dump memory from either lvl1 or PCval and lvl2 or lvl1 bytes
		xref 	add_space

		; check if lvl2 is zero  

		ld 		HL,commAdr1
		call	checkZero16 			; check if (commAdr1)=0 2 bytes -> Z 
		jr 		NZ,.adrSizeTyped 		; dump memory ; address and size are typed

		; ***	Only size typed, address from PCvalue
		ld 		HL,(PCvalue)				; HL = start address
		ld 		(commAdr1),HL 			; temp storage of PCvalue

.adrSizeTyped:
		; ***	both address and size typed
		ld 		DE,commAdr1				; DE = start address
		ld 		BC,commLvl1				; BC = number of bytes

		ld  	A,(DE)
		and 	$F0 					; adjust to nearest 16 byte block
		ld 		(DE),A
		ld 		DE,(commAdr1)			; HL = start address

		ld 		BC,(commLvl1) 			; get the size...  divide by $10
		srl 	B 
		RR 		C
		srl 	B 
		RR 		C
		srl 	B 
		RR 		C
		srl 	B 
		RR 		C					; BC = number of lines, nearest higher 16 byte block

display_BC_bytes:
		push 	bc					; save the line counter

		ld 		HL,dumpText+1		; new buffer for text output


		call	Bin2Hex16			;address in DE -> result added to (HL)-> to last 0x00. hl updatd (+4)
		
		ld  	A,':'
		ld 		(HL),A  
		inc 	HL
		push 	DE 			; store adress of first char

		xor 	A						; clear A
		ld 		(generalFlags),A 		; indicate first round , hexvalues
		call 	displayBytes
		pop 	DE					; pop back address of first char.
 
		ld 		A,'|'
				ld 		(HL),A  
		inc 	HL


		ld 		A,$0F
		ld 		(generalFlags),A 		; indicate second round , chars
		call 	displayBytes
	
		ld 		A,'|'
				ld 		(HL),A  
		inc 	HL

		xor 	a
		ld 		(hl),A

		ld 		iy,dumpText
		call	WriteLineCRNL

		pop 	bc						; pop back the line counter
		dec 	bc 						; decrease # lines...
		ld  	A,0
		cp    	B
		jr 		NZ,display_BC_bytes
		cp 		C
		jr 		NZ,display_BC_bytes



		ret



displayBytes:

		ld 		b,$10
displayLoop:
		ld 		A,B
		push 	BC			; save the # byte counter
	
		cp 		$08			; is B ( A-8) 8 bytes ?
		ld 		A,$01
		jr 		NZ,.noextraSpace
		add 	$03 	
.noextraSpace:
		ld 		B,A
			; add (b) spaces to (hl), advance hl	
		call	add_space

		ld  	A,(generalFlags)
		or  	A 				; check if zero ->  first round - Hex values
		jr  	Z,firstRound

		; ***	print out the ascii characters.
		; ***	if val isChar	

		ex 		DE,HL 				; HL -> memory bytes
		call 	isChar				; is (HL) char ? return with Carry-> value in A is always '.'
		ex 		DE,HL				; swithch HL back to text buf dumpText		
		
		ld 		(HL),A
		inc 	HL
		inc 	DE
 		pop 	bc					; pop back the # byte counter
		djnz 	displayLoop			; display (b) bytes  with spaces...

		ret 



firstRound:		
		push 	DE
		ld		A,(DE)				; get value from memory to E..?
		ld 		E,A

		call	Bin2Hex8			;result added to (HL)-> to last 0x00. hl updatd (+4)
		
		pop 	DE
		inc 	DE


		pop 	bc					; pop back the # byte counter
		djnz 	displayLoop			; display (b) bytes  with spaces...

		ret 

checkZero16:
		; ***	check if (HL),(HL+1) = 0 ?		
		ld 		A,00
		cp 		(HL)				; ix zero		
		ret 	NZ	
		inc 	HL
		cp 		(HL)				; is zero ?	
		dec  	HL	
		ret							; return with either Z or NZ



dumpText: DC	$80 00


;***************************************************************************************************
;***************************************************************************************************
;***************************************************************************************************




		;section 	STR_HEAP 
		;	space for string constants
		GLOBAL	DateBuf,MsgText1,st2g1,st1g2,steq,subst
		GLOBAL	RegLabels1,RegLabels2,RegLabels3,RegFlags
		xdef	Str0,Str2,Str3,Str4,Str7,sourctext1,sourctext2,endtext,src_size
		
		align 	1
String_HEAP_Start:
DateBuf:		DB		"  2022-12-30_17:22   ",0,0

MsgText1:		DB		"Hello, enter command: >_",  0, 0

st2g1:			DB		0x0F,"Str 2 > Str 1 !",0,0
st1g2:			DB		0x0F,"Str 1 > Str 2 !",0,0
steq:			DB		0x0F,"Strings Equal !",0,0
subst:			DB		4,"seco",0,0


RegLabels1:		DB		" |...PC...|...SP...|...IX...|...IY...|",0
RegLabels2:		DB		" |...AF...|...BC...|...DE...|...HL...|",0
RegLabels3:		DB		" |...AF'..|...BC'..|...DE'..|...HL'..|",0
RegFlags:		DB		" S Z X H X P N C",0

Str0:			defb 	23,"Hello, finnally here !",0,0
Str2:			defb	23,"This is the second Bank",0,0
Str3:			defb	23,"This is the third Bank",0,0
Str4:			defb	23,"This is the 4'th Bank",0,0
Str7:			defb	23,"This is the 7'th Bank",0,0

sourctext1:		DB		10,"First Str:",0,0
sourctext2:		DB		14,"Appendix..../a",0,0	

src_size:		equ		sourctext2-sourctext1

endtext:
String_HEAP_end:


		align 4


.END
