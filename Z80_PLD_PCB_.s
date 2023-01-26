;Z80_PLD_PCB_.asm

		include 	"Z80_Params_.inc"
 

		
;********************************************************		
;		section MainSRam			; main program in sram
;********************************************************		

			section Samples


			xref	Bin2Hex8,Bin2Hex16,  HEX2BN, BN2DEC,BN2DEC_S,DEC2BN,MFILL, BLKMOV,strCompare,CONCAT,POS,COPY,DELETE,INSERT_STR
			xref	InitBuffers, ReadLine, WriteChar, ReadChar, S_head_tail
			xref	Textbuf, inBufferEnd,inBuffer,cleanInBuffer,cleanOutBuffer,InitInterrupt
			
			xref	st2g1,st1g2,steq,subst
			xref	RegLabels1,RegLabels2,RegLabels3,RegFlags
			xref	sourctext1,sourctext2,endtext,src_size, writeSTRBelow,isHex
		
		xdef 	PLD_PCB_Start
		xref 	A_RTS_OFF,A_RTS_ON


	;***************************************************************
	;SAMPLE EXECUTION:
	;***************************************************************


DO_Debug:	equ	1		; Set to 1 to show debug printing, else 0 


		align 9

PLD_PCB_Start:	


		ld 	A,$01
		out (_CE_RST_BANK),A 		;// set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH

		; ld 	A,$00
		; out (_CE_RST_BANK),A 		;// set bank register (HC374) in high imp. defaults to bank #0
		; jp 	RTestprog



		call	Init_RAM_HEAP			; put zero values to addr $F000 - $F200

		CALL 	InitBuffers			;INITIALIZE in/Out buffers,	;INITIALIZE DART. INTERRUPT SYSTEM

		call	PIO_Init
		
		; call 	CTC_Init

		call 	DART_Init
		
		call	S_head_tail			; save input heads and tails

		; call	sh_test
		;call 	Flash_WR_Test
		;ld		HL,$2010
		;call	Flash_SE_Erase

		call	CRLF
		call 	writeSTRBelow
		defb   	"\0\r\n"
		defb	"##########################################################\r\n"
		defb	"The Z80 Board Awakened 2023\r\n"
		defb	"    git: @@GIT_VERSION@@\r\n"
		defb	"    build: @@DATE@@\r\n"
		defb	"\r\n"
		defb	"Mix CTC/DART interrupt.\r\n"
		defb	"\0"


		call	CRLF

		; call	writeSTRBelow_CRLF
		; db		"  PIO init: D0-3 outputs ! ",0

next_line:
		; ld 		a,(0x8800)
		; ; ld 		A,3
		; out 	(portB_Data),A
		; inc 	A
		; ld 		(0x8800),A

		sub  	A
		ld 		(0x8800),A

		; call 	RX_EMP
		; call 	TX_EMP

		call 	initCommParseTable


		; 	*** Print prompt text to screen, value of PC and content in memory
		ld 		DE,(PCvalue)
		ld  	A,'['
		call 	WriteChar

		; ***	Address in parenthesis
		call	putDEtoScreen

		call 	writeSTRBelow
		DB 		0,"] = ", 00

		; ***	Value of the bytes in address (2 bytes) to screen
		ld 		HL,(PCvalue)
		ld 		D,(HL)
		inc 	HL
		ld 		E,(HL)
		call	putDEtoScreen

		call 	writeSTRBelow
		DB 		0," ,enter command: >_", 00

		ld 		hl,Textbuf
		call 	ReadLine

		ld 		iy,Textbuf
		call	WriteLineCRNL

		;***  	compare input
		; ld 		HL,Textbuf
		; ld 		DE,command_list+2
		; call	strCompare

	;***************************************************************
	;	Find /Identify command:
	;***************************************************************

		ld 		HL,Textbuf
		call 	skipPriorDelimit			; set (HL) first char

		jp 		C,inputerror 				; end encountered; no command (empty line)	

		push 	HL
		pop  	DE							; typed command start in DE

		; ***	Search command in 'command_list:'
		;  		DE = typed command first char in DE (Textbuf)
		ld 		HL,command_list+1			; first char in first command in the list

scanCommandList:
		ld 		C,(HL)						; command # in C
		inc 	HL 							; (HL)=first char
		ld 		B,(HL)						; # chars in command in list
		inc 	HL 							; (HL)=first char
		push 	DE 							; save start of typed string (DE) for later

findCommandInList:
		ld 		a,(DE)						; next typed char
		cp		(HL)
		jr 		nz,findNextITEM				; different chars-> test next item in list
		inc 	DE
		inc 	HL  
		djnz 	findCommandInList
		
		; ***	test char (DE); should be a delimiter...
		ex 		DE,HL
		call 	isDelimit
						;delimiters found ? =>Z, else ~Z
						;char in (HL) is '0' ->  set C, else NC
		ex 		DE,HL

		; call 	writeSTRBelow
		; DB 		0,"pop 	HL.. !",CR,LF,00
		; call 	DumpRegisters

		jr 		nz,findNextITEM    			; command match but is longer (more chars) cp list					
		; ***	All char do match...

		jp 		matchInList

findNextITEM:
		; ***	find next ITEM or LISTEND
		ld 		a,(HL)
		cp		ITEM
		jr 		z,nextInList

		cp 		LISTEND
		jr 		NZ,.cont

		ld 		A,$FF
		ld 		(PCinpFlag),A			; indicate ev. typed address to change PCV or bytes ...
		pop 	HL					; HL start of typed string (again)
		ld 		A,(HL)
		jp 		checkaddress			; No more commands to check, check if address entered , '$'
										; or direct input of bytes.....

.cont:	inc 	HL
		jr 		findNextITEM

nextInList:
		inc 	HL						; points to item #
		pop 	DE 						; DE start of typed string (again)
		jr 		scanCommandList


	;***************************************************************
	;	Semantic error occurred in input :
	;***************************************************************

inputerror:
		call 	writeSTRBelow
		DB 		0,"Input Semantic Error... !",CR,LF,00
		; call 	DumpRegisters
		jp 		next_line



command_list:

		db		ITEM,1,4,"load",STEND,1,1,1,0
		db		ITEM,2,2,"dm",	STEND,1,1,1,0
		db		ITEM,3,2,"pc",	STEND,1,1,1,0
		db		ITEM,4,2,"cm",	STEND,1,1,1,0
		db		ITEM,5,3,"exe",	STEND,1,1,1,0
		db		ITEM,6,2,"go",	STEND,1,1,1,0
		db		ITEM,7,2,"++",	STEND,1,1,1,0
		db		ITEM,8,2,"--",	STEND,1,1,1,0
		db		ITEM,9,1,"nop",	STEND,1,1,1,0
		db		LISTEND


		; ld		HL,$6000
		; ld		(packetBaseAddress),HL			; store the address for target code (for error correction)
		; ld		A,01
		; ld		(prevPacketByte01),A 				; store of packet numbers

		; call 	SetupXMODEM_TXandRX					 
	

;/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
;---------------------------------------------------------------------------------
;/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/

;		skipPriorDelimit 		; increase A0 until non delimiter (NZ) or #0 (Z) 
;		***		store string value in textarea and reference in table
;		skipCharsUntilDelim		; increase A0 until blank (NZ) or #0 (Z) 
;		***		read/store either address or lvalue and store in table
; 		isDelimit(S)  is char in (A0) any of the delimiters specified in (A1) ? =>Z, else ~Z
; 		Parameters returned; A0 - Address of char

;		commParseTable
;		***************************************************
;		***	decode input line;
;		*** <cmd>    "TEXT"  	$xxyyzz  xxyyzz
;		*** command textstring 	address	 lvalue
;		************************************************************
;		*** commParseTable:
;		*** 00 : W : offset in jumptable  (F080)
;		*** 04 : L : address 1  (F084-F087)
;		*** 08 : L : address 2  (F088-F08B)
;		*** 10 : L : lvalue1  (F090-F09F)
;		*** 20 : L : lvalue2  (F0A0-F0AF)
;		*** 57 : L : text1   (F0B0-F0D7)
;		*** 58 : L : text2 	 (F0D8-F0FF)	
;		***
;		***--------------------------------------

;/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
;---------------------------------------------------------------------------------
;/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/

		; call	skipCharsUntilDelim			; set (HL) to first delimiter
		; push 	HL
		; pop  	BC							; typed command end+1 in BC

		; ***	Prepares the commParseTable. Consumes HL,B,A
initCommParseTable:
		ld 		A,0
		ld 		(PCinpFlag),A
		ld 		(PCinpFlag+1),A
		ld 		HL,commParseTable

		ld 		B,$80
		sub 	A 					; clear A.
.icpt:
		ld 		(HL),A
		inc 	HL
		djnz 	.icpt

		ret

;---------------------------------------------------------------------------------
;/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/

	;***************************************************************
	;	Command identified -> now search for stringh :
	;***************************************************************

matchInList:
		; ***	Command found. Then, check for string input "<string>"
		; ***	DE points to first delimiter after command 
						;delimiters found ? =>Z, else ~Z
						;char in (HL) is '0' ->  set C, else NC

		inc 	sp
		inc 	sp
		ld 		HL,commParseTable
		ld 		(HL),C 					; store the command number in (commParseTable)
		call 	writeSTRBelow_CRLF
		DB 		0,"Found a valid command  see (C).. !",CR,LF,00

		ld 		H,D
		ld		L,E						; DE -> HL -> first char after command
		call 	skipPriorDelimit 			; look for next char (  '"' ?)
		jp 		C,temp_finish				; C set from 'skipPriorDelimit'

		ld 		A,(HL)
		cp 		'"'							; beginning of string ?̣
		jr 		NZ,checkaddress
		; ***	extract string 
		inc 	HL 				; skip '"' (HL)-> first char

		ld 		D,H
		ld		E,L						; DE -> first char after '"' <source>
		call 	skipCharsUntilDelim			; find second '"'
		dec 	HL 						; skip first delimiter (ev. CR)
		ld 		A,(HL)
		cp 		'"' 					; found second '"' ??
		jp 		NZ,inputerror
		dec 	HL 						; skip second '"'
	;***************************************************************
	;	copy string to  'commParseTable'
	;***************************************************************

		and 	A
		sbc 	HL,DE 					; amount of chars...
		ld 		B,H						; amount of chars...
		ld 		C,L						; amount of chars...
		inc 	BC

		ld 		HL,commStr1			; address for first string
		ld 		A,(HL)
		or 		A 					; =0?
		jr 		Z, .strone
		ld 		HL,commStr2			; address for second string
		ld 		A,(HL)
		or  	A					; =0 ?
		jp 		NZ,inputerror			; too many strings
.strone:
		ex 		DE,HL 				; HL = <source>, DE = <dest>, size = BC
		ldir 						; make the copy
		ex		DE,HL
		inc 	DE 					; (DE) past the second '"'

		jr 		matchInList


	;***************************************************************
	;	Check if address is specified in input 
	;***************************************************************


checkaddress:
		ld 		(PCinpFlag+1),A		; if value '(PCinpFlag+1)' == '$' -> address input
 		cp 		'$'				; identified string ??
		ld 		A,0
		jr 		NZ,getLvalue
		inc 	HL 				; skip past '$'

chkADR1:
	; ***		Check where to store address...
		ld 		IX,commAdr1
		or 		(IX)			; check if zero ? (already stored)
		jr 		NZ,chkADR2
		jr 		makeASCIItoHEX
chkADR2:
		ld 		IX,commAdr2
		or 		(IX)			; check if zero ? (already stored)
		jp 		NZ,inputerror	; error : No more addresses to store
		jr 		makeASCIItoHEX

getLvalue:
		ld 		IX,commLvl1
		or 		(IX)			; check if zero ? (already stored)
		jr 		NZ,chkLVL2
		jr 		makeASCIItoHEX

chkLVL2:
		ld 		IX,commLvl2
		or 		(IX)			; check if zero ? (already stored)
		jp 		NZ,inputerror	; error : No more addresses to store


makeASCIItoHEX:
		ld 		D,H
		ld		E,L						; DE -> first char after '$' <source>
		call 	skipCharsUntilDelim			; find next delimiter or CR
		ld 		A,(HL)

	;***************************************************************
	;	copy string to  'commParseTable', IX points to dest address.
	;***************************************************************

		and 	A						; clear C
		sbc 	HL,DE 					; amount of chars...->HL ( H=0)

		bit 	0,L 					; even or odd (=1)?
		jp 		NZ,inputerror

		ld 		B,L 					; char counter

		ex 		DE,HL					; HL -> first char after '$' <source>	
nextByte:
		call 	isHex					; return with Carry, some chars are NOT HEX
		jp 		C,inputerror			; non HEX char
		sla 	A
		sla 	A
		sla 	A
		sla 	A
		ld 		C,A
		inc 	HL
		dec 	B
		call 	isHex					; return with Carry, some chars are NOT HEX
		jp 		C,inputerror			; non HEX char
		or 		C						; put together on byte in A
		inc 	HL
		ld 		(IX),A
		inc 	IX
		djnz 	nextByte

		ld  	A,(PCinpFlag)
		or  	A 						; =0 -> normal parameter save
		jr 	   	Z,normalParam

		ld  	A,(PCinpFlag+1) 		; address input for PCV ?
		cp 		'$' 					; adress flag ?
		jr 		Z,changePCVal
		; ***	Store Bytes from LVL1 to (PCval) 


		; ***	reset flag
		ld 		A,0
		ld 		(PCinpFlag),A
		ld 		(PCinpFlag+1),A

changePCVal:
		; ***	Change PCvalue from 'commAdr1'
		ld 		HL,(commAdr1)
		ld 		A,H
		ld 		H,L
		ld 		L,A
		ld 		(PCvalue),HL
		; ***	reset flag
		ld 		A,0
		ld 		(PCinpFlag),A
		ld 		(PCinpFlag+1),A

normalParam:
		call 	DumpRegisters


temp_finish:
		call 	writeSTRBelow
		DB 		0,"Finish parsing !",CR,LF,00
		; call 	DumpRegisters
		jp 		next_line

	;***************************************************************
	;	Check if LVALUE is specified in input 
	;***************************************************************

awaitstart: 
		call 	writeSTRBelow
		DB 		0,"awaitstart: !",CR,LF,00


		call 	RX_EMP
		halt


		ld 		A,'C'
		out 	(DART_A_D),A			; send the 'C' character after ~ 1 sec


		jr 		awaitstart		


;************
		ld 		hl,Textbuf
		; call	ReadLine 			;to textbuf  (A=length of input string)

		ld		HL,T_BUFFER			;HL = BASE ADDRESS 0F BUFFER
		ld		DE,Textbuf			;DE = 32767
		call	BN2DEC				; C0NVERT
		jp		textloop


		ld 		hl,Textbuf
		call	DEC2BN			; result in HL

		ld 		E,L
			; Binary to HEX  BN2HEX   E->(HL)
		ld 		hl,T_BUFFER
		inc		hl
		call	Bin2Hex8			;result in T_buffer

		ld 		iy,T_BUFFER
		call 	WriteLineCRNL

		ld 		iy,Textbuf
		call	WriteLineCRNL

		jp 		next_line
;********************************************************************************************



CTC_Init:
		;init CH 0 and 1
		ld 	 A,_Rising|_Timer|_Prescaler|_TC_Follow|_Reset|_CW
		out		(CH0),A 		; CH0 is on hold now
		ld		A,109			; time constant (prescaler; 126; 93; 6MHz -> 1 sec peroid) 232/101; 
		; ld		A,126			; time constant (prescaler; 109; 66; 3,684MHz -> 1 sec peroid;   
									; time constant (prescaler; 109; 198; 3,684MHz -> 3, sec peroid;  
		out		(CH0),A			; and loaded into channel 0
		
		
		ld	A,_Counter|_Rising|_TC_Follow|_Reset|_CW	
		out		(CH1),A			; CH1 counter
		ld		A,198			; time constant 66 defined
		out		(CH1),A			; and loaded into channel 2
	
		ld 		HL,CTC_CH0_I_Vector          (F410)
		ld  	A,L					; copy low byte
		out 	(CH0),A



		;init CH2
		ld 	 A,_Counter|_Prescaler|_Rising|_TC_Follow|_Reset|_CW
		out		(CH2),A
		ld		A,0FFh			; time constant 255d defined
		out		(CH2),A			; and loaded into channel 2
								; T02 outputs f= CPU_CLK/(256*256)
		;init CH3
								;input TRG of CH3 is supplied by clock signal from TO2
								;CH3 divides TO2 clock by AFh
								;CH3 interupts CPU appr. every 2sec to service int routine CT3_ZERO (flashes LED)
		ld		A,00000011b		; int on, counter on, prescaler don't care, edge don't care,11000111b
								; time trigger don't care, time constant follows
								; sw reset, this is a ctrl cmd
		out		(CH3),A
		ld		A,0AFh			; time constant AFh defined
		out		(CH3),A			; and loaded into channel 3
		; ld		A,10h			; it vector defined in bit 7­3,bit 2­1 don't care, bit 0 = 0

		; out		(CH0),A			; and loaded into channel 0
		ret


;********************************************************************************************	
		xdef CTC_CH0_Interrupt_Handler,CTC_CH1_Interrupt_Handler,CTC_CH2_Interrupt_Handler,CTC_CH3_Interrupt_Handler

CTC_CH0_Interrupt_Handler:
CTC_CH1_Interrupt_Handler:
		push 	AF
		; ld 		a,(0x8800)
		; ; ; ld 		A,3
		; out 	(portB_Data),A
		; inc 	A
		; ld 		(0x8800),A

		ld 		A,(0x8800)
		inc 	A
		ld 		(0x8800),A

		out 	(portB_Data),A

		cp 		30						; Z is set 
		jp 		P,showtimeout			; check if lopp should timeout... A>30

		in		A,(DART_A_C)			; read RRx ;test next test char available
		bit 	0,A						; char available ?
		call	Z,SetupXMODEM_TXandRX		; test if minicom has begun sending Z=0...
		ccf								; clear carry - > wait for next.
		pop 	AF
		ei
		reti

;------------------------------------------------------------------------------




		pop 	AF
		nop
		ei
		reti


CTC_CH2_Interrupt_Handler:
CTC_CH3_Interrupt_Handler:



showtimeout:
		call 	writeSTRBelow_CRLF
		defb    "\0\r\n"
		defb	"A timout on XMODEM occured !",00
		sub  	A
		ld 		(0x8800),A

		scf								; set carry flag 
		pop 	AF
		ei
		reti 


bit_test9:
	db	0x01,0x02,0x80,0x40


debug:		equ	0		; Set to 1 to show debug printing, else 0 


	; Spin loop here because there is nothing else to do
halt_loop:
	halt
	jp	halt_loop





;*******************************************************************************     
;*******************************************************************************     




textloop:
		; LD		HL,sourctext1
		; LD		DE,S1x
		; LD		BC,src_size
		; CALL 	BLKMOV		;	MOVE DATA FROM SOURCE TO DESTINATION


		; LD		HL,sourctext2
		; LD		DE,S2x
		; LD		BC,14
		; CALL 	BLKMOV		;	MOVE DATA FROM SOURCE TO DESTINATION

		; test of string concat
		; LD		HL,S1_8B		;HL = BASE ADDRESS OF S1
		; LD		DE,S2_8B		;DE = BASE ADDRESS OF S2
		; LD		B,40			;B = MAXIMUM LENGTH OF STRING 1
		; CALL 	CONCAT 			;CONCATENATE STRINGS to S1_8B


		; test of POS
		; LD		HL,Str2			;HL = BASE ADDRESS OF STRING
		; LD		DE,subst		;DE = BASE ADDRESS OF SUBSTRING	
		; CALL	POS				;FIND POSITION OF SUBSTRING
								; RESULTS IN REGISTER A = 8


		; test copy
		; LD		HL,Str4			; SOURCE STRING
		; LD		DE,COPY_BUFFER	;	DESTINATION STRING
		
		; LD		C,4				; STARTING INDEX FOR COPYING

		; LD		B,6				; NUMBER OF BYTES TO COPY
		; LD		A, 25			; MAXIMUM LENGTH OF SUBSTRING
		; CALL 	COPY			; COPY SUBSTRING

		; ld 		iy,COPY_BUFFER
		; rst		8				;WriteLineCRNL ; print the copy string


		; test DELETE
		LD		HL,Str0		;HL	= BASE 	ADDRESS OF STRING
		LD		A,8			
		LD		C,8				;	C= STARTING INDEX FOR DELETION
		LD		A,4			
		LD		B,4			; B = NUMBER OF CHARACTERS TO DELETE
		CALL 	DELETE 			; DELETE CHARACTERS
									; DELETING 4 CHARACTERS STARTING AT INDEX 1
		; ld 		iy,Str0
		; rst		8				;WriteLineCRNL ; print the copy string


		;test INSERT

		LD		HL,Str3				; HL = BASE ADDRESS OF STRING
		LD		DE,subst			; DE = BASE ADDRESS OF SUBSTRING

		LD		C,7					; C = STARTING INDEX FOR INSERTION

		LD		B,0x40				; B = MAXIMUM LENGTH OF STRING
		CALL 	INSERT_STR			; INSERT SUBSTRING
		ld 		iy,Str3
		; call	WriteLineCRNL 		; print the modified string


		jp		next_line

		;TEST DATA. CHANGE FOR OTHER VALUES
S1_8B:	DB		8H				; LENGTH OF SI
		DB      "LASTNAME                        "	; 32 BYTE MAX LENGTH
S2_8B:	DB		0BH				;LENGTH OF S2
		DB		". FIRSTNAME                     "	; 32 BYTE MAX LENGTH

;********************************************************************************************
;********************************************************************************************	
sh_test:
		; turn shadow off then halt
		xor A
		out (_CE_RST_BANK),A 		;// clear '64K_SRAM' signal

		halt

		ld	A,$80
		out (_Z80_BankCS),A			;// set '64K_SRAM' signal
		ld 	A,1
		out (_CE_RST_BANK),A 		; engage 3-state on bank#
		ret


;********************************************************************************************
;********************************************************************************************	
			;9H JUMP TABLE (JTAB)   353
        ; Title               Jump table
        ; Name:               JTAB
        ; Purpose:            Given an index, jump to the subroutine with
        ;                     that index in a table.
        ; Entry:              Register A is the subroutine number (0 to
        ;                                LENSUB-l, the number of subroutines)
        ;                                LENSUB must be less than or equal to
        ;                                128.
        ; Exit:               If the routine number is valid then
        ;                       execute the routine
        ;                     else
        ;                       Carry flag = 1
        ; Registers used: AF
        ; Time:               117 cycles plus execution time of subroutine
        ; Size:               Program 21 bytes plus size of table (2*LENSUB)

        ;EXIT WITH CARRY SET IF ROUTINE NUMBER IS INVALID
        ; THAT IS, IF IT IS TOO LARGE FOR TABLE OLENSUB -     1)


JTAB:
		CP		LENSUB			;COMPARE ROUTINE NUMBER, TABLE SIZE
		CCF						;COMPLEMENT CARRY FOR ERROR INDICATOR
		RET		C				;RETURN IF ROUTINE NUMBER TOO LARGE
									; WITH CARRY SET
		; INDEX INTO TABLE OF WORD-LENGTH ADDRESSES
		; LEAVE REGISTER PAIRS UNCHANGED SO THEY CAN BE USED FOR PASSING PARAMETERS

		PUSH	HL				;SAVE HL
		ADD		A,A				;DOUBLE INDEX FOR WORD-LENGTH ENTRIES
		LD		HL,JMPTAB		;INDEX INTO TABLE USING 8-BIT
		ADD		A,L			; ADDITION TO AVOID DISTURBING
		LD		L,A				; ANOTHER REGISTER PAIR
		LD		A,0
		ADC		A,H
		LD		H,A			; ACCESS ROUTINE ADDRESS
			;OBTAIN ROUTINE ADDRESS FROM TABLE AND TRANSFER
			;CONTROL TO IT, LEAVING ALL REGISTER PAIRS UNCHANGED

		LD		A, (HL)			;MOVE ROUTINE ADDRESS TO HL
		INC		HL
		LD		H, (HL)
		LD		L,A
		EX		(SP),HL				;RESTORE OLD HL, PUSH ROUTINE ADDRESS
		RET						; JUMP TO ROUTI NE

LENSUB		EQU		3				;NUMBER OF SUBROUTINES IN TABLE
JMPTAB:                            ;JUMP TABLE
		DW		SUB0			;ROUTINE 0
		DW		SUB1			;ROUTINE 1
		DW		SUB2			;ROUTINE 2
           ;THREE TEST SUBROUTINES FOR JUMP TABLE
SUB0:
		LD		A,1				; TEST ROUTI NE 0 SETS (A)    1
		RET
SUB1:
		LD		A,2				; TEST ROUTI NE 1 SETS (A) = 2
		RET
SUB2:
		LD		A,3				;TEST ROUTINE 2 SETS (A)      3
		RET



			;SAMPLE EXECUTION:


SC9H:
		SUB		A				;EXECUTE ROUTINE 0
		CALL	JTAB			; AFTER EXECUTION, (A)   =1

		LD		A,1				;EXECUTE ROUTINE 1
		CALL	JTAB			; AFTER EXECUTION, (A) = 2
		LD		A,2				;EXECUTE ROUTINE 2
		CALL	JTAB			; AFTER EXECUTION, (A)   3
		LD		A,3				;EXECUTE ROUTINE 3
		CALL	JTAB			; AFTER EXECUTION, CARRY   1
		JR		SC9H			;LOOP FOR MORE TESTS


;********************************************************************************************
;********************************************************************************************	
		xref  	RDATA,RDATA_END,TB_length

		;--------------------------------------------------
		; ld A,5
		; ld 	A,$00	
		; out (_Z80_BankCS),A		;// set bank register number 	
		ld 	A,$01
		out (_CE_RST_BANK),A 		;// set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH

		out (_8Bitsout),A

		ld A, $0F                 ;mode 1 out
		out (portA_Contr), A         ; set port A as output
		ld A,$EB

Rtll:	

		ld (40000),A
		ld A,0
		ld A,(40000)

		out (portA_Data),A		; Data to PIO port A
		out (_8Bitsout),A
		;--------------------------------------------------
		ld	DE,$8200
		ld	HL,RDATA
		ld	BC,TB_length
		; ld	BC,RDATA_END-RDATA
		ldir


DART_A_RESET:
		ld	a,00110000b
		out	(DART_A_C),A		;write into WR0: error reset, select WR0

		ld	a,018h				;write into WR0: channel reset
		out (DART_A_C),A 

		ld	a,004h				;write into WR0: select WR4
		out	(DART_A_C),A
		ld	a,44h				;44h write into WR4: clkx16,1 stop bit, no parity
		out (DART_A_C),A

		ld	a,005h				;write into WR0: select WR5
		out (DART_A_C),A
		ld	a,01101000b			;NO DTR , TX 8bit, BREAK off, TX on(4), RTS inactive (bit 2)
		ld	a,01101010b			;NO DTR , TX 8bit, BREAK off, TX on(4), RTS active (bit 2)
		out (DART_A_C),A
DART_A_EI:
			;enable SIO channel A RX
		ld	a,003h				;write into WR0: select WR3
		out (DART_A_C),A
		ld	a,11000001b				;RX 8bit, auto enable off 8(bit 5), RX on (bit 0)
		ld	a,11100001b				;RX 8bit, auto enable on 8(bit 5), RX on (bit 0)
		out (DART_A_C),A
		;Channel A RX active


		ld 	HL,Str0
tstout:
		ld 	A,(HL)
		out (DART_A_D),A
		inc HL
		ld D,A
chkTX:
		in	A,(DART_A_C)		; read status
		bit	2,A					; all sent ?
		jr z,chkTX				; not all sent..

		ld 	A,(HL)
		cp	0
		jr 	z,endmsg

		ld	A,D
		djnz	tstout

endmsg:
chkRX:
		in	A,(DART_A_C)		; read status
		bit	0,A					; char present ??
		jr z,chkRX				; check again

		in 	A,(DART_A_D)		; read the char.

		out (DART_A_D),A
chkTX2:
		in	A,(DART_A_C)		; read status
		bit	2,A					; all sent ?
		jr z,chkTX2
		
		jr	endmsg				; not all sent..




		halt
		halt
		halt
		inc A
		jr Rtll			



.boot_msg:
	db	'\r\n\n'
	db	'##############################################################################\r\n'
	db	'Z80 Retro Board 2063.3 -- sd_test.asm\r\n'
	db	'      git: @@GIT_VERSION@@\r\n'
	db	'    build: @@DATE@@\r\n'
	db	'\0'







.end
