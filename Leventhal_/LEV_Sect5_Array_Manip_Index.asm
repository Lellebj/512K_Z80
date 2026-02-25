
		; Array Manipulation and Indexing
		; 5A      Memory Fill    195
		; 5B      Block Move    198
		; 5C      Two-Dimensional Byte Array Indexing              201
		; 5D      Two-Dimensional Word Array Indexing               205
		; 5E      N-Dimensional Array Indexing 209

				;*****************************************************************************************************
				;*****************************************************************************************************
				; Memory Fill (MFILL)                                                                                                        5A
				; Places a specified value in each byte of a mem-
				; ory area of known size, starting at a given ad-                 Registers Used: AF, BC, DE, HL
				; dress.                                                          Execution Time: Approximately 21 cycles per byte
				;                                                                 plus 50 cycles overhead
				; Procedure: The program stores the specified
				;                                                                 Program Size: II bytes
				; value in the first byte and then uses a block move
				;                                                                 Data Memory Required: None
				; to fill the remaining bytes. The block move
				;                                                                 Special Cases:
				; simply transfers the value a byte ahead during
				;                                                                 I. A size of 0000 16 is interpreted as 10000 16 , It there-
				; each iteration.                                                       fore causes the program to fill 65,536 bytes with
				;                                                                     the specified value.
				;                                                                 2. Filling areas occupied or used by the program
				;                                                                     itself will cause unpredictable results. Obviously,
				;                                                                     filling the stack area requires special caution,
				;                                                                     since the return address is saved there.
				; Entry Conditions                                           Exit Conditions
				; Starting address of memory area in HL                      The area from the base address through the
				; Area size (number of bytes) in Be                          number of bytes given by the area size is filled
				;                                                         with the specified value. The area thus filled
				; Value to be placed in memory in A                          starts at BASE and continues through BASE +
				;                                                         SIZE - 1 (BASE is the base address and SIZE is
				;                                                         the area size).
				; Examples
				; I.    Data:    Value = FFI6                                2.     Data:      Value = 00 16 (Z80 operation code for NOP)
				;             Area size (in bytes) = 0380 16                                Area size (in bytes) = I C65 16
				;             Base address = IAEO l6                                        Base address = E34C 16
				;     Result:   FF 16 placed in addresses IAE0 16 through        Result:      00 16 placed in addresses E34C 16 through
				;                 IE5F 16                                                        FFB0 16
				;         Title               Memo:;tr-y fill
				;         Name:                MFILL
				;         Purpose:             Fill an area of memory with a value
				;         Entry:               Register    H     = High byte of base address
				;                                 Register    L       Low byte of base address
				;                                 Register    B       High byte of area size
				;                                 Register    C     = Low byte of area size
				;                                 Register    A     = Value to be placed in memory
				;                                 Note: A size of 0 is interpreted as 65536
				;         Exit:                Area filled with value
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
			LD		HL,BF1			:STARTING ADDRESS
			LD		BC,SIZEI		JNUMBER OF BYTES
			LD		A,0				;VALUE TO FILL
			CALL	MFILL			;FILL MEMORY
			;FILL BF2 THROUGH BF2+l999 WITH FF
			LD		HL,BF2			;STARTING ADDRESS
			LD		BC,SIZE2		;NUMBER OF BYTES
			LD		A,0FFH			;VALUE TO FILL
			CALL	MFILL			;FILL MEMORY
			JR		SC5A
SIZEl	EQU	16					;SIZE OF BUFFER 1 (10 HEX)
SIZE2	EQU	2000				; SIZE OF BUFFER 2 (07DO HEX)
BF1:	DS	SIZE1
BF2:	DS	SIZE2


				;*****************************************************************************************************
				;*****************************************************************************************************
				; Block Move (BLKMOV)                                                                                                 58
				; Moves a block of data from a source area to
				; a destination area.                                              Registers Used: AF, BC, DE, HL
				; Procedure: The program determines if the                      Execution Time: 21 cycles per byte plus 97 cycles
				; base address of the destination area is within the               overhead if data can be moved starting from the
				; source area. If it is, then working up from the                  lowest address (i.e., left) or 134 cycles overhead if
				; 																data must be moved starting from the highest
				; base address would overwrite some sourcedata.                    address (i.e., right) because of overlap.
				; To avoid overwriting, the program works down
				; from the highest address (this is sometimes called               Program Size: 27 bytes
				; a move right). If the base address of the destina-               Data Memory Required: None
				; tion area is not within the source area, the
				; program simply moves the data starting from                      Special Cases:
				; the lowest address (this is sometimes called a                     I. A size (number of bytes to move) of 0 causes an
				; 																	immediate exit with no memory changed.
				; move left). An area size (number of bytes to
				; 																2. Moving data to or from areas occupied or used
				; move) of 000016 causes an exit with no memory                         by the program itself or by the stack will have
				; changed. The program provides automatic ad-                           unpredictable results.
				; dress wraparound mod 64K.
				; Entry Conditions                                            Exit Conditions
				; Base address of source area in HL                           The block of memory is moved from the source
				; Base address of destination area in DE                      area to the destination area. If the number of
				; 															bytes to be moved is NBYTES, the base address
				; Number of bytes to move in register BC                      of the destination area is DEST, and the base
				; 															address of the source area is SOURCE, then the
				; 															data in addresses SOURCE through SOURCE
				; 															+ NBYTES - I is moved to addresses DEST
				; 															through DEST + NBYTES - 1.
				; Examples
				; I.    Data:    Number of bytes to move = 0200 16            2.    Data:     Number of bytes to move = IB7 AI6
				; 			Base address of destination area = 05DI 16                   Base address of destination area = C946 16
				; 			Base address of source area = 035E I6                        Base address of source area = C300 16
				; 	Result:   The contents of locations 035E I6 through         Result:   The contents of locations C300 16 through
				; 				055D I6 are moved to 05DI 16 through                        DE79 16 are moved to C946 16 through
				; 				07DO l6                                                     E4BF I6
				; Note that Example 2 is a more difficult prob-     This would destroy the old contents of C94616,
				; lem than Example I because the source and des-       which are needed later in the move. The solution
				; tination areas overlap. If, for instance, the pro-   to this problem is to move the data starting from
				; gram were simply to move data to the destination     the highest address if the destination area is
				; area starting from the lowest address, it would      above the source area but overlaps it.
				; initially move the contents of C30016 to C94616.
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


SOURCE    EQU		2000H             ;BASE    ADDRESS OF SOURCE AREA
DEST      EQU		2010H             ;BASE    ADDRESS OF DESTINATION AREA
LEN       EQU		11H               ;NUMBER    OF BYTES TO MOVE
          ;MOVE   11 HEX BYTES FROM 2000-2010 HEX TO 2010-2020 HEX
SC5B:
			LD		HL,SOURCE
			LD		DE,DEST
			LD		BC,LEN
			CALL	BLKMOV            ;MOVE    DATA FROM SOURCE TO DESTINATION
			JR		SC5B


				;*******************************************************************************************************************
				;*******************************************************************************************************************
				; Two-Dimensional Byte Array
				; Indexing (D2BYTE)                                                                                         5C
				; Calculates the address of an element of a
				; two-dimensional byte-length array, given the               Registers Used: AF, BC, DE, HL
				; base address of the array, the two subscripts of           Execution Time: Approximately 1100 cycles, de-
				; 														pending mainly on the amount of time required to
				; the element, and the size of a row (that is, the           perform the multiplication.
				; number of columns). The array is assumed to be             Program Size: 44 bytes
				; stored in row major order (that is, by rows) and           Data Memory Required: Four bytes anywhere in
				; both subscripts are assumed to begin at O.                 memory to hold the return address (two bytes start-
				; Procedure: The program multiplies the row               ing at address RETADR) and the column subscript
				; 														(two bytes starting at address SS2).
				; size (number of columns in a row) times the row
				; subscript (since the elements are stored by rows)
				; and adds the product to the column subscript. It
				; then adds the sum to the base address. The              standard shift-and-add algorithm (see Subrou-
				; program performs the multiplication using a             tine 6A).
				; Entry Conditions                                        Exit Conditions
				; Order in stack (starting from the top)
				; Less significant byte of return address                 Address of element in HL
				; More significant byte of return address
				; Less significant byte of column subscript
				; More significant byte of column subscript
				; Less significant byte of the size of a row (in bytes)
				; More significant byte of the size of a row (in bytes)
				; Less significant byte of row subscript
				; More significant byte of row subscript
				; Less significant byte of base address of array
				; More significant byte of base address of array
				; Examples
				; 1.   Data:   Base address = 3COO'6                      Result:   Element address = 3COO'6 + 0003'6 * 0018'6     +
				; 			Column subscript = 0004'6                              0004'6 = 3COO'6 + 0048'6 + 0004'6 =
				; 			Size of row (number of columns) = 0018'6               3C4C'6
				; 			Row subscript = 0003'6                               That is, the address of ARRAY(3,4) is 3C4C'6

				; Note that all subscripts are hexadecimal                  Result:   Element address = 6A4A'6 +0002,6 * 0050,6+
				; 																		0035'6 = 6A4A'6 + 00AO'6 + 0035 16 =
				; (35 16 = 53 10)'                                                        6BIF 16
				; The general formula is
				; 																	That is, the address of ARRA Y(2,35) is
				; 	ELEMENT ADDRESS = ARRAY BASE                                       6BIF'6
				; 	ADDRESS+ ROW SUBSCRIPT * ROW SIZE
				; 	+ COLUMN SUBSCRIPT                                     Note that we refer to the size of the row sub-
				; 														script; the size is the number of consecutive
				; 														memory addresses for which the subscript has
				; 														the same value. This is also the number of bytes
				; 2.    Data:    Base address = 6A4A'6                      from the starting address of an element to the
				; 			Column subscript = 0035'6
				; 			Size of row (number of columns) = 0050'6   starting address of the element with the same
				; 			Row subscript = 0002'6                     column subscript but a row subscript one larger.
				; 			Title                  Two-dimensional byte array indexing
				; 			Name:                  D2BYTE
				; 			Purpose:               Given the base address of a byte array, two
				; 									subscripts 'I~.'J'. and the size of the first
				; 									subscript in bytes, calculate the address of
				; 									A[I.JJ. The array is assumed to be stored in
				; 									row major order (A[O,OJ. A[O,lJ, •••• A[K,LJ),
				; 									and both dimensions are assumed to begin at
				; 									zero as in the following Pascal declaration:
				; 									A:ARRAY[O •• 2.0 •• 7J OF BYTE;
				; 			Entry:                 TOP OF STACK
				; 									Low byte of return address.
				; 									High byte of return address,
				; 									Low byte of second subscript (column element), ;
				; 									High byte of second subscript (column element),;
				; 									Low byte of first subscript size, in bytes,
				; 									High byte of first subscript size, in bytes.
				; 									Low byte of first subscript (row element),
				; 									High byte of f host subscript (row element),
				; 									Low byte of array base address,
				; 									High byte of array base address.
				; 									NOTE:
				; 									The first subscript size is length of a row
				; 									in bytes
				; 		Exit:              Register H = High byte of element address
				; 							Register L = Low byte of element address
				; 		Registers used: AF,BC,DE,HL
				; 		Time:              Approximately 1100 cycles
				; 		Size:              Program 44 bytes
				; 							Data     4 bytes
				;*******************************************************************************************************************
				;*******************************************************************************************************************


D2BYTE:
				;SAVE RETURN ADDRESS
			POP		HL
			LD		(RETADR),HL
				;GET SECOND SUBSCRIPT
			POP		HL
			LD		(SS2),HL
				;GET SIZE OF FIRST SUBSCRIPT (ROW LENGTH), FIRST SUBSCRIPT
			POP		DE              ; GET LENGTH OF ROW
			POP		BC              ;GET FIRST SUBSCRIPT
				;MULTIPLY FIRST SUBSCRIPT * ROW LENGTH USING SHIFT AND ADD
				; ALGORITHM. PRODUCT IS IN HL
			LD		HL,0            ; PRODUCT = 0
			LD		A,15            ;COUNT = BIT LENGTH - 1
MLP:
			SLA		E                 ;SHIFT LOW BYTE OF MULTIPLIER
			RL		D                 ;ROTATE HIGH BYTE OF MULTIPLIER
			JR		NC,MLPl           ; JUMP IF MSB OF MULTIPLIER = 0
			ADD		HL,BC             ;ADD MULTIPLICAND TO PARTIAL PRODUCT
MLP1:     	ADD		HL,HL             ;SHIFT PARTIAL PRODUCT
			DEC		A
			JR		NZ,MLP            ; CONTINUE THROUGH 15 BITS
				;DO LAST ADD IF MSB OF MULTIPLIER IS 1
			OR		D                ;SIGN FLAG = MSB OF MULTIPLIER
			JP		P,MLP2
			ADD		HL,BC           ;ADD IN MULTIPLICAND IF SIGN = 1
				;ADD IN SECOND SUBSCRIPT
MLP2:    	 LD		DE,(SS2)
			ADD		HL,DE
				;ADD BASE ADDRESS TO FORM FINAL ADDRESS
			POP		DE              ;GET BASE ADDRESS OF ARRAY
			ADD		HL,DE           ;ADD BASE TO INDEX
				;RETURN TO CALLER
			LD		DE,(RETADR)       ;RESTORE RETURN ADDRESS TO STACK
			PUSH	DE
			RET
			; DATA


RETADR: DS	2					;TEMPORARY FOR RETURN ADDRESS
SS2:    DS	2					;TEMPORARY FOR SECOND SUBSCRIPT

        ;  SAMPLE EXECUTION:


SC5C:
			LD		HL,ARY         ;PUSH BASE ADDRESS OF ARRAY
			PUSH	HL
			LD		HL,(SUBS1)     ;PUSH FIRST SUBSCRIPT
			PUSH	HL
			LD		HL, (SSUBS1)   ;PUSH SIZE OF FIRST SUBSCRIPT
			PUSH	HL
			LD		HL,(SUBS2)     ;PUSH SECOND SUBSCRIPT
			PUSH	HL
			CALL		D2BYTE      ;CALCULATE ADDRESS
									;FOR THE INITIAL TEST DATA
									;HL = ADDRESS OF ARY(2,4)
									;	= ARY + (2*8) + 4
									; = ARY + 20 (CONTENTS ARE 21)
									;NOTE BOTH SUBSCRIPTS START AT 0
			JR		SC5C
			; DATA
SUBS1:	DW		2              ;,SUBSCRIPT 1
SSUBS1: DW		8              ;SIZE OF SUBSCRIPT   1
SUBS2: 	DW		4              ;SUBSCRIPT 2
		;THE ARRAY (3 ROWS OF 8 COLUMNS)
ARY:    DB      1 ,2 ,3 ,4 ,5 ,6 ,7 ,8
        DB      9 ,10,11,12,13,14,15,16
        DB      17,18,19,20,21,22,23,24


				;*******************************************************************************************************************
				;*******************************************************************************************************************
				; Two-Dimensional Word Array
				; Indexing (02WORO)                                                                                                  50
				; Calculates the starting address of an element
				; of a two-dimensional word-length (l6-bit) array,                 Registers Used: AF, BC, DE, HL
				; given the base address of the array, the two                     Execution Time: Approximately IlOO cycles, de-
				; 																pending mainly on how long it takes to multiply row
				; subscripts of the element, and the size of a row in              size times row subscript
				; bytes. The array is assumed to be stored in row                  Program Size: 45 bytes
				; major order (that is, by rows) and both sub-                     Data Memory Required: Four bytes anywhere in
				; scripts are assumed to begin at O.                               memory to hold the return address (two bytes start-
				; Procedure: The program multiplies the row                     ing at address RETADR) and the column subscript
				; 																(two bytes starting at address SS2)
				; size (in bytes) times the row subscript (since the
				; elements are stored by row), adds the product to
				; the doubled column subscript (doubled because
				; each element occupies two bytes), and adds the                standard shift-and-add algorithm (see Subrou-
				; sum to the base address. The program uses a                   tine 6A) to multiply.
				; Entry Conditions                                              Exit Conditions
				; Order In stack (starting at the top)
				; Less significant byte of return address                       Starting address of element in HL
				; More significant byte of return address                       The element occupies the address in HL and the
				; Less significant byte of column subscript                       next higher address
				; More significant byte of column subscript
				; Less significant byte of size of rows (in bytes)
				; More significant byte of size of rows (in bytes)
				; Less significant byte of row subscript
				; More significant byte of row subscript
				; Less significant byte of base address of array
				; More significant byte of base address of array

				; Examples
				; I.   Data:   Base address = 5El4 16                           Result:   Element starting address = 5EI4 16 + 0005 16 *
				; 			Column subscript = 0008 16                                   00lC 16 + 0008 16 * 2 = 5EI4 16 + 008C l6 +
				; 			Size of a row (in bytes) = 00lC 16 (i.e., each               0010 16 = 5EB0 16
				; 			row has 001410 or 000E 16 word-length ele-               That is, the starting address of ARRAY(5,8)
				; 			ments)                                                     is 5EB0 16 and the element occupies 5EB0 16
				; 			Row SUbscript = 0005 16                                      and 5EBI 16
				; 2.    Data:    Base address = BI00 16                       Note that one parameter ofthis routine is the
				; 			Column subscript = 0002 16
				; 														size of a row in bytes. The size for word-length
				; 			Size of a row (in bytes) = 0008 16 (i.e., each
				; 				row has four word-length elements)      elements is the number of columns per row
				; 			Row subscript = 0006 16                   times 2 (the size of an element in bytes). The
				; 														reason we chose this parameter rather than the
				; Result: Element starting address = BlOO 16 + 0006 16 *
				; 			0008 16 + 0002 16 * 2 = BI00 16 + 0030 16 +
				; 														number    of columns or the maximum column
				; 			0004 16 = B134 16                           index is that this parameter can be calculated
				; 		That is, the starting address of ARRAY(6,2)    once (when the array bounds are determined)
				; 			is B134 16 and the element occupies B134 16 and used whenever the array is accessed. The
				; 			and B135 16
				; 														alternative parameters (number of columns or
				; The general formula is                                   maximum column index) would require extra
				; 														calculations during each indexing operation.
				; 	ELEMENT STARTING ADDRESS = ARRAY
				; 	BASE ADDRESS + ROW SUBSCRIPT *
				; 	SIZE OF ROW + COLUMN SUBSCRIPT * 2
				; 			Title                     Two-dimensIonal word array indexing
				; 			Name:                     D2WORD
				; 			Purpose:                  Given the base address of a word array, two
				; 										subscripts 'I','J', and the size of the first
				; 										subscript in bytes, calculate the address of
				; 										A[I,Jl. The array is assumed to be stored in
				; 										row major order (A[O,Ol, A[O,ll, ••• , A[K,Ll),
				; 										and both dimensions are assumed to begin at
				; 										zero as in the following Pascal declaration:
				; 										A:ARRAV[O •• 2,O •• 7l OF WORD~
				; 			Entry:                    TOP OF STACK
				; 										Low byte of return address,
				; 										High byte of return address,
				; 										Low byte of second subscript (column element)
				; 										High byte of second subscript (column element);
				; 										Low byte of first subscript size, in bytes,
				; 										High byte of first subscript size, in bytes,
				; 										Low byte of first subscript (row element),
				; 										High byte of first subscript (row element),
				; 										Low byte of array base address,
				; 										High byte of array base address,
				; 										NOTE:
				; 										The first subscript size is length of a row
				; 										in words if 2
				; 		Exit:                Register H   = High   byte of element address
				; 							Register L   = High   byte of element address
				; 		Registers used: AF.BC.DE.HL
				; 		Time:                Approximately 1100 cycles
				; 		Size:                Program 45 bytes
				; 							Data     4 bytes
				;*******************************************************************************************************************
				;*******************************************************************************************************************


D2WORD:
				;SAVE    RETURN ADDRESS
			POP		HL
			LD		(RETADR),HL
				;GET    SECOND SUBSCRIPT, MULTIPLY BY 2 FOR WORD-LENGTH ELEMENTS
			POP		HL
			ADD		HL, HL          ;*2
			LD		(SS2),HL
				;GET    SIZE OF FIRST SUBSCRIPT (ROW LENGTH), FIRST SUBSCRIPT
			POP		DE				; GET LENGTH OF ROW
			POP		BC				;GET FIRST SUBSCRIPT
				;MULTIPLY FIRST SUBSCRIPT * ROW LENGTH USING SHIFT AND ADD
				; ALGORITHM. PRODUCT IS IN HL
			LD		HL,0			; PRODUCT = 0
			LD		A,15			;COUNT = BIT LENGTH - 1
MLP:
			SLA		E				;SHIFT LOW BYTE OF MULTIPLIER
			RL		D				;ROTATE HIGH BYTE OF MULTIPLIER
			JR		NC,MLP1			;JUMP IF MSB OF MULTIPLIER = 0
			ADD		HL,BC			;ADD MULTIPLICAND TO PARTIAL PRODUCT
MLP1 :		ADD		HL,HL			;SHIFT PARTIAL PRODUCT
			DEC		A
			JR		NZ,MLP			; CONTINUE THROUGH 15 BITS
				; ADD MULTIPLICAND IN LAST TIME IF MSB OF MULTIPLIER IS 1
			OR		D				;SIGN FLAG = MSB OF MULTIPLIER
			JP		P,MLP2
			ADD		HL,BC			;ADD IN MULTIPLICAND IF SIGN = 1
				;ADD IN SECOND SUBSCRIPT
MLP2:		LD		DE,(SS2)
			ADD		HL,DE
				;ADD BASE ADDRESS TO FORM FINAL ADDRESS
			POP		DE				;GET BASE ADDRESS OF ARRAY
			ADD		HL,DE			;ADD BASE TO INDEX
				;RETURN TO CALLER
			LD		DE,(RETADR)		;RESTORE RETURN ADDRESS TO STACK
			PUSH	DE
			RET

			 	;DATA
RETADR: DS	2                ;TEMPORARY FOR RETURN ADDRESS
SS2:    DS	2                ;TEMPORARY FOR SECOND SUBSCRIPT

	; SAMPLE EXECUTION:

SC5D:
			LD		HL.ARY           ;PUSH   BASE ADDRESS OF ARRAY
			PUSH	HL
			LD		HL. (SUBS1)      ;PUSH FIRST SUBSCRIPT
			PUSH	HL
			LD		HL. (SSUBS 1)    ;PUSH SIZE OF FIRST SUBSCRIPT
			PUSH	HL
			LD		HL.(SUBS2)       ;PUSH SECOND SUBSRIPT
			PUSH	HL
			CALL	D2WORD           ;CALCULATE ADDRESS
									;FOR THE INITIAL TEST DATA
									;HL = ADDRESS OF ARY(2.4)
									;		ARY + (2*16) + 4 * 2
									;   = ARY + 40 (CONTENTS ARE 2100H)
									;NOTE BOTH SUBSCRIPTS START AT 0
			JR       SC5D
         ;DATA
SUBS1: 	DW		2                ;SUBSCRIPT 1
SSUBSl:	DW		16               ;SIZE OF SUBSCRIPT 1
SUBS2: 	DW		4                ;SUBSCRIPT 2

;THE ARRAY (3 ROWS OF 8 COLUMNS)
ARY:    DW      0100H,0200H,0300H,0400H,0500H,0600H,0700H,0800H
        DW      0900H,1000H,1100H,1200H,1300H,1400H,1500H,1600H
        DW      1700H,1800H,1900H,2000H,2100H,2200H,2300H,2400H


				;*******************************************************************************************************************
				;*******************************************************************************************************************
				; N-Dimensional Array
				; Indexing (NDIM)                                                                                            5E
				; Calculates the starting address of an element
				; of an N-dimensional array given the base address          Registers Used: AF, Be, DE, HL
				; and N pairs of sizes and subscripts. The size of a        Execution Time: Approximately 1300 cycles per
				; 														dimension plus 165 cycles overhead (depending
				; dimension is the number of bytes from the start-          mainly on how much time is required to perform the
				; ing address of an element to the starting address         multiplications)
				; of the element with an index one larger in the            Program Size: 120 bytes
				; dimension but the same in all other dimensions.           Data Memory Required: Five bytes anywhere in
				; The array is assumed to be stored in row major            memory to hold the return address (two bytes start-
				; 														ing at address RETADR), the accumulated offset
				; order (that is, organized so that subscripts to the       (two bytes starting at address OFFSET), and the
				; right change before subscripts to the left).              number of dimensions (one byte at address
				; Note that the size of the rightmost subscript is       NUMDIM)
				; simply the size of the elements (in bytes); the size      Special Case: If the number of dimensions is 0, the
				; 														program returns with the base address in HL.
				; of the next subscript is the size of the elements
				; times the maximum value of the rightmost sub-
				; script plus I, and so forth. All subscripts are        case (an integral power of2), the program reduc-
				; assumed to begin at O. Otherwise, the user must        es the multiplication to left shifts. Otherwise, it
				; normalize the subscripts. (See the second exam-        performs each multiplication using the shift-
				; ple at the end of the listing.)                        and-add algorithm of Subroutine 6A. Once the
				; Procedure: The program loops on each dimen-         program has calculated the overall offset, it adds
				; sion, calculating the offset in that dimension as      that offset to the base address to obtain the
				; the subscript times the size. If the size is an easy   starting address of the element.

				; Entry Conditions                                       Exit Conditions
				; Order In stack (starting from the top)

				; Less significant byte of return address                Starting address of element in HL
				; More significant byte of return address                The element occupies memory addresses START
				; Less significant byte of number of dimensions            through START + SIZE - I, where START
				; More significant byte of number of dimensions            is the calculated address and SIZE is the size
				; (not used)                                             of an element in bytes.

				; Less significant byte of size of rightmost dimen-
				; sion
				; More significant byte of size of rightmost dimen-
				; SIOn
				; Less significant byte of rightmost subscript
				; More significant byte of rightmost subscript
				; Less significant byte of size of leftmost dimen-
				; 	SlOn
				; More significant byte of size of leftmost dimen-
				; sion
				; Less significant byte of leftmost subscript
				; More significant byte of leftmost subscript
				; Less significant byte of base address of array
				; More significant byte of base address of array

				; Example
				; 1.    Data:    Base address = 3C00 16                           The general formula is
				; 			Number of dimensions = 0003 16
				; 			Rightmost subscript = 0005 16                      STARTING ADDRESS = BASE ADDRESS +
				; 			Rightmost size = 0003 16 (3-byte entries)          N-I
				; 			Middle subscript = 0003 16
				; 			Middle size = 0012 16 (six 3-byte entries)
				; 																L     SUBSCRIPTj      * SIZEj
				; 																i=O
				; 			Leftmost subscript = 0004 16
				; 			Leftmost size = 007E 16 (seven sets of six 3-    where
				; 				byte entries)                                    N is the number of dimensions
				; 	Result:   Element starting address = 3COO 16 + 0005 16 *     SUBSCRIPTj is the ith subscript
				; 				0003 16 + 0003 16 * 0012 16 + 0004 16 *          SIZE j is the size of the ith dimension
				; 				007E 16 = 3C00 16 + 000F 16 + 0036 16 +
				; 				0lF8 16 = 3E3D 16                                Note that we use the size of each dimension as
				; 			That is, the element is ARRAY(4,3,5); it         a parameter to reduce the number of repetitive
				; 				occupies addresses 3E3D 16 through 3E3F 16
				; 				(the maximum values of the various sub-
				; 																multiplications and to generalize the procedure.
				; 				scripts are 6 (leftmost) and 5 (middle) with   The sizes can be calculated and saved as soon as
				; 				each element occupying three bytes)            the bounds of the array are known. Those sizes
				; 																can then be used whenever indexing is per-
				; 																formed on that array. Obviously, the sizes do not
				; 																change if the bounds are fixed, and they should
				; 																not be recalculated as part of each indexing
				; 																operation. The sizes are also general, since the
				; 																elements can themselves consist of any number
				; 																of bytes.
				; Title          N-dimensional array indexing
				; Name:          NDIM
				; Purpose:       Calculate the address of an element in an
				; 			N-dimensional array given the base address.
				; 			N pairs of size in bytes and subscript. and the
				; 			number of dimensions of the array. The array is
				; 			assumed to be stored in row major order
				; 			(A[0.0.01.A[0.0.11 ••••• A[0.1.01.A[0.1.11 •••• ).
				; 			Also. it is assumed that all dimensions begin
				; 			at 0 as in the following Pascal declaration:
				; 				A:ARRAY[0 •• 10.0 •• 3.0 •• 51 OF SOMETHING
				; 			For arrays that do not begin at 0 boundaries.
				; 			normalization must be performed before calling
				; 			this routine. An example is given at the end.
				; Entry:         TOP OF STACK
				; 				Low byte of return address.
				; 				High byte of return address.
				; 				Low byte of number dimensions.
				; 				High byte of number dimensions (not used).
				; 				Low byte of dim N-l size
				; 				High byte of dim N-l size
				; 				Low byte of dim N-l subscript
				; 				High byte of dim N-l subscript
				; 				Low byte of dim N-2 size
				; 				High byte of dim N-2 size
				; 				Low byte of dim N-2 subscript
				; 				High byte of dim N-2 subscript


				; 				Low byte of dim 0 size
				; 				High byte of dim 0 size
				; 				Low byte of dim 0 subscript
				; 				High byte of dim 0 subscript
				; 				Low byte of array base address
				; 				High byte of array base address
				; 			NOTE:
				; 				All sizes are in bytes
				; Exit:          Register H   = High    byte of address
				; 			Register L   = Low    byte of address
				; Registers used: AF.BC.DE.HL
				; Time:          Approximately 1300 cycles per dimension
				; 			plus 165 cycles overhead
				; 		Size:              Program 120 bytes
				; 							Data      5 bytes
				;*******************************************************************************************************************
				;*******************************************************************************************************************


NDIM:
			;POP PARAMETERS
			POP		HL
			LD		(RETADR),HL
			;QFFSET := 0
			LD		HL,0
			LD		(OFFSET),HL
				;GET NUMBER OF DIMENSIONS AND TEST FOR 0
			POP		HL
			LD		A,L
			LD		(NUMDIM),A      ;GET NUMBER OF DIMENSIONS
			OR		A               ;TEST FOR 0
			JR		Z,ADBASE        ;RETURN WITH BASE ADDRESS IN HL
									; IF THERE ARE NO DIMENSIONS
				;LOOP ON EACH DIMENSION
				; DOING OFFSET := OFFSET + (SUBSCRIPT   *   SIZE)
LOOP:
			POP		DE                 ;GET SIZE
			POP		HL                 ;GET SUBSCRIPT
			CALL	NXTOFF             ;OFFSET := OFFSET + (SUBSCRIPT   *   SIZE)
			LD		HL,NUMDIM
			DEC		(HL)               ;DECREMENT NUMBER OF DIMENSIONS
			JR		NZ,LOOP            ;CONTINUE THROUGH ALL DIMENSIONS
ADBASE:
				;CALCULATE STARTING ADDRESS OF ELEMENT
				;OFFSET = BASE + OFFSET
			LD		HL,(OFFSET)
			POP		DE              ; GET BASE ADDRESS
			ADD		HL,DE           ;SUM WITH OFFSET
				;RESTORE RETURN ADDRESS AND EXIT
			LD		DE,(RETADR)
			PUSH	DE
			RET
			;------------------------------
			;SUBROUTINE NXTOFF
			; PURPOSE: OFFSET := OFFSET + (SUBSCRIPT * SIZE);
			; ENTRY: OFFSET = CURRENT OFFSET
			;		DE = CURRENT SIZE OF THIS DIMEMSION
			;        HL = CURRENT SUBSCRIPT
			;EXIT: OFFSET = OFFSET + (SUBSCRIPT + SIZE);
			;REGISTERS USED: AF, BC. DE, HL
			;------------------------------
NXTOFF:
			PUSH	HL                   ;SAVE CURRENT SUBSCRIPT IN STACK
				;CHECK   IF SIZE IS POWER OF 2 LESS THAN 256
			LD		A,D
			OR		A               ;HIGH BYTE = 0 ?
			JR		NZ,BIGSZ        ;JUMP IF SIZE IS LARGE
			LD		A,E                  ;A = LOW BYTE OF SIZE
			LD		HL,EASYAY            ;HL = BASE ADDRESS OF EASYAY
			LD		B,SZEASY             ;B = SIZE OF EASY ARRAY
			LD		C,0                  ;C = SHIFT COUNTER
EASYLP:
			CP		(HL)
			JR		Z,ISEASY             ;JUMP IF SIZE IS A POWER OF 2
			INC		HL                   ; INCREMENT TO NEXT BYTE OF EASYAY
			INC		C                    ;INCREMENT SHIFT COUNTER
			DJNZ	EASYLP               ;DECREMENT COUNT
			JR		BIGSZ                ;JUMP IF SIZE IS NOT EASY
ISEASY:
			POP		HL                   ;GET SUBSCRIPT
			LD		A,C                  ;GET NUMBER OF SHIFTS
			OR		A                    ;TEST FOR 0
			JR		Z,ADDOFF             ;JUMP IF SHIFT FACTOR   =0
				;ELEMENT SIZE     *   SUBSCRIPT REDUCES TO LEFT SHIFTS
			LD		B,A                   ;B = SHIFT COUNT
SHIFT:
			ADD		HL,HL                ;MULTIPLY SUBSCRIPT BY 2
			DJNZ	SHIFT                ;CONTINUE UNTIL DONE
			JR		ADDOFF               ;DONE SO ADD OFFSET + SUBSCRIPT

BIGSZ:
				;SIZE IS NOT POWER OF 2, MULTIPLY
				; ELEMENT SIZE TIMES SUBSCRIPT THE HARD WAY
			POP		BC              ~GET SUBSCRIPT

				;MULTIPLY FIRST SUBSCRIPT * ROW LENGTH USING SHIFT AND ADD
				;ALGORITHM. RESULT IS IN HL
				; BC = SUBSCRIPT (MULTIPLICAND)
          		; DE = SIZE (MULTIPLIER)
          LD  	HL,0            ;PRODUCT =     °
          LD	A,15           	;COUNT = BIT LENGTH - 1
                                            
MLP:
			SLA		E                   ;SHIFT LOW BYTE OF MULTIPLIER
			RL		D                   ; ROTATE HIGH BYTE OF MULTIPLIER
			JR		NC,MLP1		 		; JUMP IF MSB OF MULTIPLIER = 0
			ADD		HL,BC				;ADD MULTIPLICAND TO PARTIAL PRODUCT
MLP1 :		ADD		HL,HL               ;SHIFT PARTIAL PRODUCT
			DEC		A
			JR		NZ,MLP        	  ;CONTINUE THROUGH 15 BITS
				;ADD   IN MULTIPLICAND LAST TIME IF MSB OF MULTIPLIER IS 1
			OR		D               	;SIGN FLAG = MSB OF MULTIPLIER
			JP		P,ADDOFF
			ADD		HL,BC				;ADD   IN MULTIPLICAND IF SIGN = 1
				; ADD SUBSCRIPT     *   SIZE TO OFFSET
ADDOFF:
			EX		DE,HL
			LD		HL,(OFFSET)           ;GET OFFSET
			ADD		HL,DE                 ;ADD PRODUCT OF SUBSCRIPT   *    SIZE
			LD		(OFFSET),HL           ;SAVE OFFSET
			RET
EASYAY:                               ;SHIFT FACTOR
			DB	1                     ;0
			DB	2                     ;1
			DB	4                     ;2
			DB	8                     ;3
			DB	16                    ;4
			DB	32                    ;5
			DB	64                    ;6
			DB	128                   ;7
SZEASY		EQU	$-EASYAY
        ; DATA
RETADR: DS	2                     ;TEMPORARY FOR RETURN ADDRESS
OFFSET: DS	2                     ;TEMPORARY FOR PARTIAL OFFSET
NUMDIM: DS	1                     ;NUMBER OF DIMENSIONS



        ;   SAMPLE EXECUTION:


SC5E:
				;FIND ADDRESS OF AYH1.3.0]
				;SINCE LOWER BOUNDS OF ARRAY 1 ARE ALL ZERO IT IS NOT
				;NECESSARY TO NORMALIZE THEM
				;PUSH BASE ADDRESS OF ARRAY 1
			LD		HL,AY1
			PUSH	HL
				; PUSH SUBSCRIPT/SIZE FOR DIMENSION 1
			LD		HL,1
			PUSH	HL              ; SUBSCRIPT
			LD		HL,A1SZ1
			PUSH	HL              ;SIZE
				; PUSH SUBSCRIPT/SIZE FOR DIMENSION 2
			LD		HL,3
			PUSH	HL              ; SUBSCRIPT
			LD		HL.A1SZ2
			PUSH	HL              ; SIZE
				;PUSH SUBSCRIPT/SIZE FOR DIMENSION 3
			LD		HL,0
			PUSH	HL                ; SUBSCRIPT
			LD		HL,AISZ3
			PUSH	HL                ;SIZE
				;PUSH NUMBER OF DIMENSIONS
			LD		HL,AIDIM
			PUSH	HL
			CALL	NDIM            ;CALCULATE ADDRESS
									;AY = STARTING ADDRESS OF ARY1(1,3,0)
									;	= ARY + (1*126) + (3*21) + (0*3)
									;	= ARY + 189
				;CALCULATE ADDRESS OF AY2[-1.6J
				;SINCE LOWER BOUNDS OF AY2 DO NOT START AT O. SUBSCRIPTS
				;MUST BE NORMALIZED
				;PUSH BASE ADDRESS OF ARRAY 2
			LD		HL,AY2
			PUSH	HL
				; PUSH (SUBSCRIPT - LOWER BOUND)/SIZE FOR DIMENSION 1
			LD		HL,-1
			LD		DE,-A2D1L       ;NEGATIVE OF LOWER BOUND
			ADD		HL,DE           ;ADD NEGATIVE TO NORMALIZE TO 0
			PUSH	HL              ; SUBSCRIPT
			LD		HL,A2SZ1
			PUSH	HL              ;SIZE
				; PUSH (SUBSCRIPT - LOWER BOUND)/SIZE FOR DIMENSION 2
			LD		HL,6
			LD		DE.-A2D2L       ;NEGATIVE OF LOWER BOUND
			ADD		HL,DE           ; ADD NEGATIVE TO NORMALIZE TO 0
			PUSH	HL              ; SUBSCRIPT
			LD		HL,A2SZ2
			PUSH	HL              ;SIZE
			;PUSH NUMBER OF DIMENSIONS
			LD		HL,A2DIM
			PUSH	HL
			CALL	NDIM            ;CALCULATE ADDRESS
									;AY=STARTING ADDRESS OF ARY1(-1,6)
									;=ARY+«(-1)-(-S»*18)+«6-2)*2)
									;=ARY + 80
			JR		SCSE
; DATA
; AY1 : ARRAY[AIDIL •• AIDIH.AID2L •• AID2H.AID3L •• AID3Hl 3-BYTE ELEMENTS
;            [0          30             SO             6]
A1D1M    EQU     3                           ;NUMBER OF DIMENSIONS
A1D1L    EQU     0                           ; LOW BOUND OF DIMENSION 1
A1D1H    EQU     3                           ;HIGH BOUND OF DIMENSION 1
A1D2L    EQU     0                           ; LOW BOUND OF DI MENS I ON 2
A1D2H    EQU     5                           ; HIGH BOUND OF DIMENSION 2

A1D3L    EQU      0                      ;,LOW BOUND OF DIMENSION 3
A1D3H    EQU      6                      ;,HIGH BOUND OF DIMENSION 3
A1SZ3    EQLI     3                      ;,SIZE OF ELEMENT IN DIMENSION 3
A1SZ2    EQU      ((A1D3H-A1D3L)+1)*A1SZ3 	;SIZE OF ELEMENT IN DIMENSION 2
A1SZ1    EQU      ((A1D2H-A1D2L)+1)*A1SZ2	;SIZE OF ELEMENT IN DIMENSION 1
AY1:     DS       ((A1D1H-A1D1L)+1)*A1SZ1 	; ARRAY
;AY2 : ARRAY[AIDIL •• AIDIH.A1D2L •• A1D2HJ   OF WORD
;
A2DIM   EQU		2						     ;NUMBER OF DIMENSIONS
A2D1L   EQU     -5                            ;LOW BOUND OF DIMENSION 1
A2D1H   EQU     -1                            ;HIGH BOUND OF DIMENSION 1.
A2D2L   EQU     2                             ;LOW BOUND OF DIMENSION 2
A2D2H   EQU     10                            ;HIGH BOUND OF DIMENSION 2
A2SZ2   EQU     2                             ;SIZE OF ELEMENT IN DIMENSION 2
A2SZ1   EQU     ((A2D2H-A2D2L)+1)*A2SZ2        ;SIZE OF ELEMENT IN DIMENSION 1
AY2:    DS      ((A2D1H-A2D1L)+1)*A2SZ1        ; ARRAY
    
	
	     END
