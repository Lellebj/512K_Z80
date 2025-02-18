;Z80_PLD_PCB_.asm

		include 	"Z80_Params_.inc"
 

		
;********************************************************		
;		section MainSRam			; main program in sram
;********************************************************		

	
		section	Monitor



			xref	Bin2Hex8,Bin2Hex16,  HEX2BN, BN2DEC,BN2DEC_S,DEC2BN,MFILL, BLKMOV,strCompare,CONCAT,POS,COPY,DELETE,INSERT_STR
			xref	InitBuffers, ReadLine, WriteChar, ReadChar, S_head_tail
			xref	Textbuf, inBufferEnd,inBuffer,cleanInBuffer,cleanOutBuffer,InitInterrupt
			xref	dumpMemory

			xref	st2g1,st1g2,steq,subst
			xref	RegLabels1,RegLabels2,RegLabels3,RegFlags
			xref	sourctext1,sourctext2,endtext,src_size, writeSTRBelow,isHex

			xref 	crc16_2,CRC16
		
		xdef 	PLD_PCB_Start
		xref 	A_RTS_OFF,A_RTS_ON


	;***************************************************************
	;SAMPLE EXECUTION:
	;***************************************************************


DO_Debug:	equ	1		; Set to 1 to show debug printing, else 0 

PLD_PCB_Start:	
		; ***	should be start address $D000
		jr 		.initRH
		; Code in $D002-D005 = '0000' - 'AAAA': copy from flash
		; Code in $D002-D005 = 'CCCC': code uploaded from xmodem/or DMA. Do not copy from flash
		; db 		"AAAA"
		db 		"CCCC"
		align	3

.initRH:
		ld 		A,$33
		out 	(gpio_out),A



		

		; call	Init_RAM_HEAP			; put zero values to addr $F000 - $FFF0
		ld		DE,SRAM_VAR_START		; defined in linker script
		ld		hl,zero_byte
		
		ld 		BC,HEAP_SIZE			; defined in linker script

.cl_vars:
		ldi
		dec 	hl
		jp		PE,.cl_vars			; 		P/V is set if BC – 1 ≠ 0; otherwise, it is reset.

		ld 		($F120),SP
		ld 		A,$AA
		out 	(gpio_out),A

		CALL 	InitBuffers			;INITIALIZE in/Out buffers,	;INITIALIZE SIO_0. INTERRUPT SYSTEM
			; initialize buffer counters and pointers.
	 
		call	PIO_Init
		
		call 	CTC_Init
		ld 		A,$CC
		out 	(gpio_out),A

		call 	SIO_0_Init
		
		call	S_head_tail			; save input heads and tails
		ld 		A,$81
		out 	(gpio_out),A

		; call	sh_test
		;call 	Flash_WR_Test
		;ld		HL,$2010
		;call	Flash_SE_Erase

		call	CRLF
		call 	writeSTRBelow
		defb   	"\0\r\n"
		defb	"##########################################################\r\n"
		defb	"The Z80 Board Awakened 2024\r\n"
		defb	"    git: @@GIT_VERSION@@\r\n"
		defb	"    build: @@DATE@@\r\n"
		defb	"    FLASH->SRAM 0xD000.\r\n"
		defb	"\0"


		call	CRLF

next_line:

		call 	initCommParseTable

		; ***	indicate memory banks   F[x]  Flash memory bank x
		; ***	indicate memory banks   S[y]  SRAM memory bank y
				; if bit 7 (memBankID) = 1  no FLASH memory is selected
				; if bit 7 (memBankID) = 0 FLASH memory is lower 32k and SRAM upper 32k
		ld 		HL,T_BUFFER+1 			; prepare output buffer		
		ld 		(HL),'F'	
		ld 		A,(memBankID)
		bit 	7,A					; bit 7 set -> 64kSRAM
		jr 		Z,useFlash
		ld 		(HL),'S'	
useFlash:
		inc 	HL
		ld 		(HL),'['	
		inc 	HL
		bit 	7,A					; bit 7 set -> 64kSRAM
		jr 		Z,.IDflash

		; ***	show sram bank number
		and 	$0F					; sram bank #
		call 	AddToT_Buf			; convert to 1 ascii char in (HL+)
		jr 		.cont
.IDflash:
		; ***	show flash bank number
		srl 	A
		srl 	A
		srl 	A
		srl 	A
		and 	A,$07				; flash bank #
		call	AddToT_Buf			; convert to 1 ascii char in (HL+)
.cont:
		ld  	(HL),']'
		inc 	HL

		; 	*** Print prompt text to screen, value of PC and content in memory
		ld 		DE,(PCvalue)
		ld  	(HL),'['
		inc 	HL

		; ***	Address in parenthesis
		call 	Bin2Hex16
		ld  	(HL),']'
		inc 	HL
		ld  	(HL),'='
		inc 	HL

		; ***	Value of the bytes in address (2 bytes) to screen
		push 	HL
		ld 		HL,(PCvalue)
		ld 		D,(HL)
		inc 	HL
		ld 		E,(HL)
		pop 	HL
		call	Bin2Hex16
		ld  	(HL),'-'
		inc 	HL
		ld  	(HL),'>'
		inc 	HL

		ld  	(HL),$00
		ld 		iy,T_BUFFER
		call	WriteLine

		ld 		hl,Textbuf
		call 	ReadLine

		ld 		iy,Textbuf
		call	WriteLineCRNL
		; ld 		A,5
		; out 	(portA_Data),A

		;***  	compare input
		; ld 		HL,Textbuf
		; ld 		DE,command_list+2
		; call	strCompare

	;***************************************************************
	;	Find /Identify command:
	;***************************************************************

		ld 		HL,Textbuf
		call 	skipPriorDelimit			; set (HL) first char
		; ld 		A,7
		; out 	(portA_Data),A

		jp 		C,temp_finish 				; end encountered; no command (empty line)	

		push 	HL
		pop  	DE							; typed command start in DE

		; ***	Search command in 'command_list:'
		;  		DE = typed command first char in DE (Textbuf)
		ld 		HL,command_list+1			; first char in first command in the list
		; ld 		A,9
		; out 	(portA_Data),A

scanCommandList:
		ld 		C,(HL)						; command # in C
		inc 	HL 							; (HL)=first char
		ld 		B,(HL)						; # chars in command in list
		inc 	HL 							; (HL)=first char
		push 	DE 							; save start of typed string (DE) for later

findCommandInList:
		ld 		a,(DE)						; next typed char
		; or 		$20							; make typed char lower case
		cp		(HL)
		jr 		nz,findNextITEM				; different chars-> test next item in list
		inc 	DE
		inc 	HL  
		djnz 	findCommandInList
		
		; ***	test char (DE); should be a delimiter...
		; ex 		DE,HL
		; call 	isDelimit
		; 				;delimiters found ? =>Z, else ~Z
		; 				;char in (HL) is '0' ->  set C, else NC
		; ex 		DE,HL

		; call 	writeSTRBelow
		; DB 		0,"pop 	HL.. !",CR,LF,00
		; call 	DumpRegisters

		; jr 		nz,findNextITEM    			; command match but is longer (more chars) cp list					

		; ***	Found a matching command, All char do match...
		; 		(HL) points to first after command
		jp 		matchInList
zero_byte:	db  0

findNextITEM:
		; ***	find next ITEM or LISTEND
		ld 		a,(HL)
		cp		ITEM
		jr 		z,nextInList

		cp 		LISTEND
		jr 		NZ,.cont

		; ***	Command list did not match; check if direct address '$' or byte input
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
		push   	HL
		call 	writeSTRBelow
		DB 		0,"Input Semantic Error... ! code(DE):",00
		pop 	DE
		call 	putDEtoScreen
		call 	CRLF
		jp 		next_line

command_addresses:
		defw 	00
		defw 	p_load			;1
		defw 	p_dumpmem		;2
		defw 	p_pc			;3
		defw 	p_clearmem		;4
		defw 	p_exe			;5
		defw 	p_go			;6
		defw 	p_incDecPC		;7
		defw 	p_incDecPC		;8
		defw 	p_FON			;9
		defw 	p_FOFF			;10
		defw 	p_flwr			;11. write data to EEPROM
		defw 	p_flse			;12. sector erase
		defw 	p_xmod			;13. transfer files via x-modem
		defw 	p_reset			;14. Jump to $0000
		defw	p_C_Read		;15. Read from SD card   sdrd  "file"  $Addr
		defw	p_C_Write		;16. Write to SD card   sdrd  "file"  $Addr.l $Addr.h/Num
		defw	p_C_Read		;17. Read from USB   sdrd  "file"  $Addr
		defw	p_C_Write		;18. Write to USB   sdrd  "file"  $Addr.l $Addr.h/Num
		defw	p_C_Delete		;19. delete file on SD card   sdrd  "file"  $Addr
		defw	p_C_Delete		;20. delete file on USB  sdrd  "file"  $Addr.l $Addr.h/Num
		defw 	p_C_Read		;21. List root level files/dirs on sd card
		defw 	p_C_Read 		;22. List root level files/dirs on USB
		defw	p_cptFl			;23. copy from mem to flash memory (on selected bank)
		defw 	p_flbank		;24. Set flash bank #
		defw	p_srbank		;25. Set sram bank #
command_list:
;		*** command textstring1/2 	address1/2	 lvalue1/2

		db		ITEM,1,4,"load",STEND,%100010,0
		db		ITEM,2,2,"dm",	STEND,%000010,0
		db		ITEM,3,2,"pc",	STEND,%000000,0
		db		ITEM,4,2,"cm",	STEND,%000000,0
		db		ITEM,5,3,"exe",	STEND,%000000,0
		db		ITEM,6,2,"go",	STEND,%000000,0
		db		ITEM,7,2,"++",	STEND,%000000,0
		db		ITEM,8,2,"--",	STEND,%000000,0
		db		ITEM,9,4,"f-on",	STEND,%000000,0
		db		ITEM,9,8,"flash-on",	STEND,%000000,0
		db		ITEM,10,5,"f-off",	STEND,%000000,0
		db		ITEM,10,9,"flash-off",	STEND,%000000,0
		db		ITEM,11,4,"flwr",	STEND,%001110,0		; write to flash: flwr  <$Addr.mem> <$Addr.flash>  <Num> (0E)
		db		ITEM,12,4,"flse",	STEND,%000000,0
		db		ITEM,13,4,"xmod",	STEND,%001000,0		; xmodem from PC   xmod <address>
		db		ITEM,14,3,"rst",	STEND,%000000,0
		db		ITEM,15,4,"sdrd",	STEND,%101000,0		; Read from SD card   sdrd  "file"  $Addr
		db		ITEM,16,4,"sdwr",	STEND,%101010,0 	; Write to SD card   sdwr  "file"  $Addr.l  $Addr.h/Num (2A/2C)
		db		ITEM,17,5,"usbrd",	STEND,%101000,0		; Read from USB      usbrd  "file"  $Addr
		db		ITEM,18,5,"usbwr",	STEND,%101010,0 	; Write to USB        usbwr  "file"  $Addr.l  $Addr.h/Num (2A/2C)
		db		ITEM,19,5,"sddel",	STEND,%100000,0 	; Delete file on SD card   sddel  "file"  
		db		ITEM,20,6,"usbdel",	STEND,%100000,0 	; Delete file on USB   usbdel  "file"  
		db		ITEM,21,5,"sddir",	STEND,%100000,0 	; List root level files/dirs on sd card
		db		ITEM,22,6,"usbdir",	STEND,%100000,0 	; List root level files/dirs on USB
		db 		ITEM,23,5,"cptfl",	STEND,%001100,0		; copy from adress range to flash   cptfl  $Addr   $Addr
		db 		ITEM,24,6,"flbank",	STEND,%000010,0		; set flash bank #	
		db 		ITEM,25,6,"srbank",	STEND,%000010,0		; set sram bank #	
		db		ITEM,26,3,"nop",	STEND,%000000,0
		db		LISTEND
commListLen  equ   27

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
		inc 	sp						; restore PC from PUSH in <scanCommandList>
		inc 	HL 						; HL point to first after <STEND>  ->req arguments
		ld 		A,(HL) 					; A= required arguments
 		ld 		HL,commParseTable
		ld 		(HL),C 					; store the command number in (commParseTable, F080)
		inc 	HL
		ld 		(HL),A					; store required arguments  in (commParseTable+1, F081)
		; call 	writeSTRBelow_CRLF
		; DB 		0,"Found a valid command  see (C).. !",CR,LF,00
		ld 		H,D
		ld		L,E		
		dec 	HL							; DE -> HL -> last char before delimiter
paramLoopEntry:	

		call 	skipPriorDelimit 			; look for next char (  '"' ?)
		jp 		C,executeCommand			; C set from 'skipPriorDelimit', no command parameters

		ld 		A,(HL)

		cp 		'"'							; beginning of string ?̣
		jr 		NZ,checkaddress				;  NZ -> (HL) points to non delimiter
		; ***	extract string 
		inc 	HL 							; skip '"' (HL)-> first char

		ld 		D,H
		ld		E,L							; DE -> first char after '"' <source>
		call 	skipCharsUntilDelim			; find second '"'
		dec 	HL 							; skip first delimiter (ev. CR)
		ld 		A,(HL)
		cp 		'"' 						; found second '"' ??
		jp 		NZ,inputerror
		dec 	HL 							; skip second '"'
	;***************************************************************
	;	copy string to  'commParseTable'
	;***************************************************************

		and 	A
		sbc 	HL,DE 						; amount of chars...
		ld 		B,H							; amount of chars...
		ld 		C,L							; amount of chars...
		inc 	BC

		ld 		HL,commStr1				; address for first string
		ld 		A,(HL)
		or 		A 						; =0?
		jr 		Z, .strone
		ld 		HL,commStr2				; address for second string
		ld 		A,(HL)
		or  	A						; =0 ?
		jp 		NZ,inputerror			; too many strings
.strone:
		ex 		DE,HL 					; HL = <source>, DE = <dest>, size = BC
		ldir 							; make the copy
		; ex		DE,HL
		inc 	HL 						; (HL) past the second '"'

		jr 		paramLoopEntry


	;***************************************************************
	;	Check if address is specified in input 
	;***************************************************************


checkaddress:
		; ***	A = (HL), first char after delimiter

		ld 		(PCinpFlag+1),A			; if value '(PCinpFlag+1)' == '$' -> address input
		cp 		'$'						; identified address id
		ld 		A,0
		jr 		NZ,getLvalue			; first value of A (PCinpFlag+1) is '$' ??
		inc 	HL 						; skip past '$'

chkADR1:
	; ***		Check where to store address...

		ld 		IX,commAdr1
		; ld 		A,0
		cp 		(IX)			; check if zero ? (already stored)
		jr 		NZ,chkADR2
		cp 		(IX+1)			; check if zero (byte 2)? (already stored)
		jr 		NZ,chkADR2
		jr 		makeASCIItoHEX
chkADR2:

		ld 		IX,commAdr2
		; ld 		A,0
		cp 		(IX)			; check if zero ? (already stored)
		jp 		NZ,inputerror	; error : No more addresses to store
		cp 		(IX+1)			; check if zero (byte 2)? (already stored)
		jp 		NZ,inputerror	; error : No more addresses to store
		jr 		makeASCIItoHEX

getLvalue:
		ld 		IX,commLvl1
		cp 		(IX)			; check if zero ? (already stored)
		jr 		NZ,chkLVL2
		cp 		(IX+1)			; check if zero (byte 2)? (already stored)
		jr 		NZ,chkLVL2
		jr 		makeASCIItoHEX

chkLVL2:
		ld 		IX,commLvl2
		cp 		(IX)			; check if zero ? (already stored)
		jp 		NZ,inputerror	; error : No more addresses to store
		cp 		(IX+1)			; check if zero (byte 2) ? (already stored)
		jp 		NZ,inputerror	; error : No more addresses to store


makeASCIItoHEX:
		; ***	copy from command line to adr or lvalue in table
		; ***	only two bytes (four chars)....
		; 		IX point to destination...
		ld 		D,H
		ld		E,L						; DE -> first char after '$' <source>
		call 	skipCharsUntilDelim			; find next delimiter or CR ; adr in HL
		ld 		A,(HL)

	;***************************************************************
	;	copy string to  'commParseTable', IX points to dest address.
	;***************************************************************

		and 	A						; clear C
		sbc 	HL,DE 					; amount of chars...->HL ( H=0); DE -> first char after '$' <source>

		; ***	Do not check even or odd...
		; bit 	0,L 					; even or odd (=1)?
		; jp 		NZ,inputerror

		ld 		B,L 					; char counter
		ld 		HL,00					;  

		ex 		DE,HL					; HL -> first char after '$' <source>	

nextHalfByte:

		call 	isHex					; return with Carry, value in A is NOT HEX
		jp 		C,inputerror			; 		; return with Carry, value in A is NOT HEX

		sla 	E						; shift left E-> Carry
		rl 		D 						; Carry -> rotate left D
		sla 	E						; shift left E-> Carry
		rl 		D 						; Carry -> rotate left D
		sla 	E						; shift left E-> Carry
		rl 		D 						; Carry -> rotate left D
		sla 	E						; shift left E-> Carry
		rl 		D 						; Carry -> rotate left D
		or      E  						; A OR E(0..3) are zero
		ld 		E,A

		ld 		(IX),E					; store E value [big endian]
		ld 		(IX+1),D				; store D value [big endian]

		inc 	HL
		; inc 	IX
		djnz 	nextHalfByte
		;***	HL should point to first delimiter...

byteEnd:

		ld  	A,(PCinpFlag)
		or  	A 						; check Z; =0 -> normal parameter save
		jp 	   	Z,paramLoopEntry

		ld  	A,(PCinpFlag+1) 		; address input for PCValue ?
		cp 		'$' 					; adress flag ?
		jr 		Z,changePCVal
		; ***	Store Bytes from LVL1 to (PCval) this is not finished...

		push 	HL
		ld 		HL,(PCvalue)	
		ld 		DE,commLvl1
		ld 		A,(DE)
		ld 		(HL),A
		inc 	HL
		inc 	DE
		ld 		A,(DE)
		or 		A
		jr 		Z,noHighNib
		ld 		(HL),A
noHighNib:
		pop 	HL		
		; ***	reset flag
		; ld 		A,0
		; ld 		(PCinpFlag),A
		; ld 		(PCinpFlag+1),A
		jp 		paramLoopEntry 				; loop and check for more parameters

changePCVal:
		; ***	Change PCvalue from 'commAdr1'
		push 	HL
		ld 		HL,(commAdr1)
		ld 		(PCvalue),HL
		pop 	HL 						; restore value of first delimiter
		jp 		paramLoopEntry 				; loop and check for more parameters

executeCommand:	
		; ***	execute commands (and arguments)
		; call 	writeSTRBelow
		; DB 		0,"Finish parsing !",CR,LF,00
		; call 	DumpRegisters			; checkpoint for list of arguments

		ld 		A,(PCinpFlag)
		or 		A   					; check if zero  
		jr 		NZ,.noJump

		call 	JPTable01
.noJump:
		; ***	reset flag
		ld 		A,0
		ld 		(PCinpFlag),A
		ld 		(PCinpFlag+1),A

		
		; jp 		paramLoopEntry 				; loop and check for more parameters

temp_finish:
		; call 	DumpRegisters
		jp 		next_line

	;***************************************************************
	;	Check if LVALUE is specified in input 
	;***************************************************************

JPTable01:
		ld 		A,(commParseTable) 			; retrieve command number
		or 	  	A 							; error if 0
		jp 		Z,inputerror
		cp 		commListLen
		ccf 						;complement carry for error indicator

		jp      C,inputerror

		push  	HL
		sla 	A 							; multiply *2
		ld 		HL,command_addresses

		add		A, L						; addition to avoid disturbing
		ld 		L,A							; another register pair
		ld		A,0
		adc		A,H
		ld		H, A						; access routine address

										;obtain routine address from table and transfer 
										;control to it, leaving all register pairs unchanged
		
		ld 		A,(HL)

		inc 	HL
		ld 		H,(HL)
		ld 		L,A

		ex 		(SP),HL					;restore old HL, push routine address
		ret 							; jump to routine

		; exx
		; ld		HL,$0077
		; exx	
		; call DumpRegisters
argumentsError:
		call 	writeSTRBelow
		DB 		0,"Some arguments mismatch !",CR,LF,00
		ret


p_reset:
		xor 	A
		ld 		(memBankID),A			; set memory banks #0
		call 	setFLASHBank			; FLASH bank #0
		xor 	A
		call 	setSRAMBank				; ram bank #0

		call 	enableFLASH			; start from FLASH

		call 	enableIC620_OE 			; enable the outputs.

		jp $0000

p_load:
		ret
p_dumpmem:
		call 	dumpMemory

		ret
p_pc:
		ret
p_clearmem:
		ret
p_exe:
		ret
p_go:

		ld 		A,(TempVar1)
		inc 	A
		ld 		(TempVar1),A
		cp 		15
		call 	DumpRegisters
		
		ret 	P
		jr 		p_go
p_incDecPC:
		ld 		HL,commLvl1
		ld 		A,0
		ld 		E,(HL)
		inc 	HL
		ld 		D,(hl) 				; DE = (commLvl1)
		; ***	if both D and E is 0 -> DE = 1 		; no param -> A=1
		cp 		E
		jr 		NZ,.justOne
		cp 		D
		jr 		NZ,.justOne
		ld 		DE,1
.justOne:
		ld 		A,(commParseTable)
		ld 		HL,(PCvalue)
		cp 		7 					; ++ (increase) ??
		jr  	nz,.sub
		add 	HL,DE 				; increase HL (PCvalue) with DE
		jr 		.common
.sub:
		and 	A					; clear C
		sbc 	HL,DE 				; decrease HL (PCvalue) with DE
.common:
		ld 		(PCvalue),HL
		ret

p_FON:
		; ***  Activate FLASH MEMORY (set 64K_SRAM signal 0)

		call 	enableFLASH


		call 	writeSTRBelow
		DB 		0," Use 256k FLASH (7 banks),lower 32k and SRAM (bank16),upper 32k !",CR,LF,00
		ret

p_FOFF:
		; ***  Do Not USE FLASH MEMORY(set 64K_SRAM signal 1)
		call 	disableFLASH

		call 	writeSTRBelow
		DB 		0," Use only 512k (16 banks) SRAM !",CR,LF,00

		ret

p_flwr:
		; *** 	testwrite to EEPROM
		
		; call 	checkArgsTAL				; check necessary args ("string" $Adr1   Lvl1)
		; jp		NZ,argumentsError			; show argument error and return
		; 	*** Check if flashmem is enabled
		ld 		A,(memBankID)
		bit 	7,A
		jr 		NZ,errNoFlash

		call 	Flash_WR_Test
		ret
errNoFlash:
		call 	writeSTRBelow
		DB 		0," Can't write to deselected FLASH !",CR,LF,00

		ret


p_flse:
		; *** 	erase the sector that contain the address of HL

		push	HL
		ld 		HL,$2010
		call 	Flash_SE_Erase
		pop 	HL
		ret

p_xmod:
		; ***	Transfer files via x-modem
		; ***	Check commParseTable+1 if required parameters

		ld 		A,(commParseTable+1)
		bit 	0,A 			; should be a <2-textstring 	1-address	 0-lvalue>
		jr 		Z,.nxta
		; ***	check the commLvl1 if zero
		ld 		DE,(commLvl1)
.nxta:		


		call 	doImportXMODEM

		call 	SIO_0_A_TXRX_INTon
		call 	CTC1_INT_OFF
	
		ret

p_C_Read:

		call 	checkArgsTAL				; check necessary args
		jp		NZ,argumentsError			; show argument error and return

		ld 		DE,CTC_delay_INT_handler
		ld 		(CTC_CH1_I_Vector),DE

		call 	purgeRXB
		call 	initSIOBInterrupt			; turn on interrupt on SIO B (CH376S)
		call 	HC376S_ResetAll
		call 	HC376S_CheckConnection
		ld 		A,(commParseTable)
		cp 		15							; 15 read SD; 17-read USB
		jr 		Z,.doSD
		cp 		21							; 21 read SD enumerate, 22 read USB enumerate
		jr 		Z,.doSD
		call 	HC376S_setUSBMode
		call 	HC376S_diskConnectionStatus		; dont use with SD card
		jr 		.cont
.doSD:
		call 	HC376S_setSDMode
		
.cont:
		call 	HC376S_USBdiskMount				; ret with NZ  on failure
		jr 		NZ,abort
		call 	HC376S_setFileName
		call 	HC376S_fileOpen
		jr 		NZ,abort

		call 	HC376S_getFileSize
		call 	HC376S_fileRead
		call 	HC376S_fileClose
abort:

		; ***	reset the interrupt handler for CTC
		call 	HC376S_ResetAll
		call 	CTC1_INT_OFF
		ld		HL,CTC_CH1_Interrupt_Handler
		ld		(CTC_CH1_I_Vector),HL		;STORE CTC channel 1 VECTOR
		ret

		
p_C_Write:
		call 	checkArgsTAL				; check necessary args ("string" $Adr1   Lvl1)
		jr 		Z,.contWR
		; ***	check alternative (2 adresses)
		ld 		A,%101100					; alt. with "string" $Adr1 < $Adr2
		ld 		(HL),A						; HL-> (commParseTable+1);
		inc 	HL			; HL-> (commParseTable+2); get the resulting arguments counted
		cp 		(HL) 						; compare resulting arguments with req arguments

		jp		NZ,argumentsError			; show argument error and return
		
		; ***	calculate size from addresses $Adr2 - $Adr1
		scf
		ccf
		ld 		HL,(commAdr2)
		ld		DE,(commAdr1)
		sbc		HL,DE
		ld 		(commLvl1),HL				; resulting size in commLvl1


.contWR:
		ld 		DE,CTC_delay_INT_handler
		ld 		(CTC_CH1_I_Vector),DE

		call 	purgeRXB
		call 	initSIOBInterrupt			; turn on interrupt on SIO B (CH376S)
		call 	HC376S_ResetAll
		call 	HC376S_CheckConnection
		ld 		A,(commParseTable)
		cp 		16							; 16 read SD; 18-read USB
		jr 		Z,.doSD
		call 	HC376S_setUSBMode
		call 	HC376S_diskConnectionStatus		; dont use with SD card
		jr 		.cont
.doSD:
		call 	HC376S_setSDMode
.cont:
		call 	HC376S_USBdiskMount				; ret with NZ  on failure
		jr 		NZ,abort
		call 	HC376S_setFileName
		call 	HC376S_fileCreate
		jr		NZ,abort
		call 	HC376S_fileWrite
		
		call 	HC376S_fileClose
		call 	HC376S_ResetAll
		jr 		abort

p_C_Delete:
		call 	checkArgsTAL				; check necessary args ("string" $Adr1  )
		jP 		NZ,argumentsError
		ld 		DE,CTC_delay_INT_handler
		ld 		(CTC_CH1_I_Vector),DE

		call 	purgeRXB
		call 	initSIOBInterrupt			; turn on interrupt on SIO B (CH376S)

		call 	HC376S_ResetAll
		call 	HC376S_CheckConnection
		ld 		A,(commParseTable)
		cp 		19							; 19 delete file SD; 20-delete file USB
		jr 		Z,.doSD
		call 	HC376S_setUSBMode
		call 	HC376S_diskConnectionStatus
		jr 		.cont
.doSD:
		call 	HC376S_setSDMode
.cont:
		call 	HC376S_USBdiskMount
		 
		; call 	HC376S_fileOpen
		; call 	HC376S_getFileSize
		; call 	HC376S_fileRead

		call 	HC376S_fileDelete
		call 	HC376S_ResetAll

		jp 		abort

p_cptFl:
p_flbank:
		; ***	set flash bank #
		ld 		A,(commLvl1) 			; load param into A
		call 	setFLASHBank				; change to bank
		ret
p_srbank:
		; ***	set sram bank #
		ld 		A,(commLvl1) 			; load param into A
		call 	setSRAMBank 			; change to bank
		ret


;********************************************************************************************     
;********************************************************************************************     

		; ***	Check commParseTable+1 if required parameters
checkArgsTAL:		
;		*** command textstring1/2 	address1/2	 lvalue1/2

		; ***	try to connect to USB
		ld 		HL,commParseTable+2				; resulting typed arguments
		ld 		A,0
		ld 		(HL),A

		ld 		IX,commStr1			; commStr1 =			0xB0
		call 	shift_0_1:
		ld 		IX,commStr2			; commStr2 =			0xD8
		call 	shift_0_1:
		ld 		IX,commAdr1			; commAdr1 =			0x84
		call 	shift_0_1:
		ld 		IX,commAdr2			; commAdr2 =			0x88
		call 	shift_0_1:
		ld 		IX,commLvl1			; commLvl1 =			0x90
		call 	shift_0_1:
		ld 		IX,commLvl2			; commLvl2 =			0xA0
		call 	shift_0_1:

		ld 		A,(HL)						; get the resulting arguments counted
		dec 	HL
		cp 		(HL) 						; compare resulting arguments with req arguments
		ret 	 					; return with Z or NZ  arguments

shift_0_1:
		cp 		(IX)
		jr 		NZ,shiftIn1
		cp 		(IX+1)
		jr 		NZ,shiftIn1
		; ***	both =0 shift in '0'
		sla		(HL)
		ret
shiftIn1:
		; ***	least one not '0' shift in '1'
		scf		
		rl 		(HL)
		ret


;********************************************************************************************     
;********************************************************************************************     

bit_test9:
	db	0x01,0x02,0x80,0x40


; debug:		equ	0		; Set to 1 to show debug printing, else 0 


	; Spin loop here because there is nothing else to do
; halt_loop:
; 	halt
; 	jp	halt_loop





;*******************************************************************************     
;*******************************************************************************     



		; ld 		hl,Textbuf
		; ; call	ReadLine 			;to textbuf  (A=length of input string)

		; ld		HL,T_BUFFER			;HL = BASE ADDRESS 0F BUFFER
		; ld		DE,Textbuf			;DE = 32767
		; call	BN2DEC				; C0NVERT
		; jp		textloop


		; ld 		hl,Textbuf
		; call	DEC2BN			; result in HL

		; ld 		E,L
		; 	; Binary to HEX  BN2HEX   E->(HL)
		; ld 		hl,T_BUFFER
		; inc		hl
		; call	Bin2Hex8			;result in T_buffer

		; ld 		iy,T_BUFFER
		; call 	WriteLineCRNL

		; ld 		iy,Textbuf
		; call	WriteLineCRNL

		; jp 		next_line



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
		; call 		WriteLineCRNL ; print the copy string


		; ; test DELETE
		; LD		HL,Str0		;HL	= BASE 	ADDRESS OF STRING
		; LD		A,8			
		; LD		C,8				;	C= STARTING INDEX FOR DELETION
		; LD		A,4			
		; LD		B,4			; B = NUMBER OF CHARACTERS TO DELETE
		; CALL 	DELETE 			; DELETE CHARACTERS
									; DELETING 4 CHARACTERS STARTING AT INDEX 1
		; ld 		iy,Str0
		; call 		WriteLineCRNL ; print the copy string


; 		;test INSERT

; 		LD		HL,Str3				; HL = BASE ADDRESS OF STRING
; 		LD		DE,subst			; DE = BASE ADDRESS OF SUBSTRING

; 		LD		C,7					; C = STARTING INDEX FOR INSERTION

; 		LD		B,0x40				; B = MAXIMUM LENGTH OF STRING
; 		CALL 	INSERT_STR			; INSERT SUBSTRING
; 		ld 		iy,Str3
; 		; call	WriteLineCRNL 		; print the modified string


; 		jp		next_line

; 		;TEST DATA. CHANGE FOR OTHER VALUES
; S1_8B:	DB		8H				; LENGTH OF SI
; 		DB      "LASTNAME                        "	; 32 BYTE MAX LENGTH
; S2_8B:	DB		0BH				;LENGTH OF S2
; 		DB		". FIRSTNAME                     "	; 32 BYTE MAX LENGTH

;********************************************************************************************
;********************************************************************************************	
; sh_test:
; 		; turn shadow off then halt
; 		xor A
; 		out (_CE_RST_BANK),A 		;// clear '64K_SRAM' signal

; 		halt

; 		ld	A,$80
; 		out (_Z80_BankCS),A			;// set '64K_SRAM' signal
; 		ld 	A,1
; 		out (_CE_RST_BANK),A 		; engage 3-state on bank#
; 		ret


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


; JTAB:
; 		CP		LENSUB			;COMPARE ROUTINE NUMBER, TABLE SIZE
; 		CCF						;COMPLEMENT CARRY FOR ERROR INDICATOR
; 		RET		C				;RETURN IF ROUTINE NUMBER TOO LARGE
; 									; WITH CARRY SET
; 		; INDEX INTO TABLE OF WORD-LENGTH ADDRESSES
; 		; LEAVE REGISTER PAIRS UNCHANGED SO THEY CAN BE USED FOR PASSING PARAMETERS

; 		PUSH	HL				;SAVE HL
; 		ADD		A,A				;DOUBLE INDEX FOR WORD-LENGTH ENTRIES
; 		LD		HL,JMPTAB		;INDEX INTO TABLE USING 8-BIT
; 		ADD		A,L			; ADDITION TO AVOID DISTURBING
; 		LD		L,A				; ANOTHER REGISTER PAIR
; 		LD		A,0
; 		ADC		A,H
; 		LD		H,A			; ACCESS ROUTINE ADDRESS
; 			;OBTAIN ROUTINE ADDRESS FROM TABLE AND TRANSFER
; 			;CONTROL TO IT, LEAVING ALL REGISTER PAIRS UNCHANGED

; 		LD		A, (HL)			;MOVE ROUTINE ADDRESS TO HL
; 		INC		HL
; 		LD		H, (HL)
; 		LD		L,A
; 		EX		(SP),HL				;RESTORE OLD HL, PUSH ROUTINE ADDRESS
; 		RET						; JUMP TO ROUTI NE

; LENSUB		EQU		3				;NUMBER OF SUBROUTINES IN TABLE
; JMPTAB:                            ;JUMP TABLE
; 		DW		SUB0			;ROUTINE 0
; 		DW		SUB1			;ROUTINE 1
; 		DW		SUB2			;ROUTINE 2
;            ;THREE TEST SUBROUTINES FOR JUMP TABLE
; SUB0:
; 		LD		A,1				; TEST ROUTI NE 0 SETS (A)    1
; 		RET
; SUB1:
; 		LD		A,2				; TEST ROUTI NE 1 SETS (A) = 2
; 		RET
; SUB2:
; 		LD		A,3				;TEST ROUTINE 2 SETS (A)      3
; 		RET



; 			;SAMPLE EXECUTION:


; SC9H:
; 		SUB		A				;EXECUTE ROUTINE 0
; 		CALL	JTAB			; AFTER EXECUTION, (A)   =1

; 		LD		A,1				;EXECUTE ROUTINE 1
; 		CALL	JTAB			; AFTER EXECUTION, (A) = 2
; 		LD		A,2				;EXECUTE ROUTINE 2
; 		CALL	JTAB			; AFTER EXECUTION, (A)   3
; 		LD		A,3				;EXECUTE ROUTINE 3
; 		CALL	JTAB			; AFTER EXECUTION, CARRY   1
; 		JR		SC9H			;LOOP FOR MORE TESTS


;********************************************************************************************
;********************************************************************************************	
; 		xref  	RDATA,RDATA_END,TB_length

; 		;--------------------------------------------------
; 		; ld A,5
; 		; ld 	A,$00	
; 		; out (_Z80_BankCS),A		;// set bank register number 	
; 		ld 	A,$01
; 		out (_CE_RST_BANK),A 		;// set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH

; 		out (_8Bitsout),A

; 		ld A, $0F                 ;mode 1 out
; 		out (portA_Contr), A         ; set port A as output
; 		ld A,$EB

; Rtll:	

; 		ld (40000),A
; 		ld A,0
; 		ld A,(40000)

; 		out (portA_Data),A		; Data to PIO port A
; 		out (_8Bitsout),A
; 		;--------------------------------------------------
; 		ld	DE,$8200
; 		ld	HL,RDATA
; 		ld	BC,TB_length
; 		; ld	BC,RDATA_END-RDATA
; 		ldir


; SIO_0_A_RESET:
; 		ld	a,00110000b
; 		out	(SIO_0_A_C),A		;write into WR0: error reset, select WR0

; 		ld	a,018h				;write into WR0: channel reset
; 		out (SIO_0_A_C),A 

; 		ld	a,004h				;write into WR0: select WR4
; 		out	(SIO_0_A_C),A
; 		ld	a,44h				;44h write into WR4: clkx16,1 stop bit, no parity
; 		out (SIO_0_A_C),A

; 		ld	a,005h				;write into WR0: select WR5
; 		out (SIO_0_A_C),A
; 		ld	a,01101000b			;NO DTR , TX 8bit, BREAK off, TX on(4), RTS inactive (bit 2)
; 		ld	a,01101010b			;NO DTR , TX 8bit, BREAK off, TX on(4), RTS active (bit 2)
; 		out (SIO_0_A_C),A
; SIO_0_A_EI:
; 			;enable SIO channel A RX
; 		ld	a,003h				;write into WR0: select WR3
; 		out (SIO_0_A_C),A
; 		ld	a,11000001b				;RX 8bit, auto enable off 8(bit 5), RX on (bit 0)
; 		ld	a,11100001b				;RX 8bit, auto enable on 8(bit 5), RX on (bit 0)
; 		out (SIO_0_A_C),A
; 		;Channel A RX active


; 		ld 	HL,Str0
; tstout:
; 		ld 	A,(HL)
; 		out (SIO_0_A_D),A
; 		inc HL
; 		ld D,A
; chkTX:
; 		in	A,(SIO_0_A_C)		; read status
; 		bit	2,A					; all sent ?
; 		jr z,chkTX				; not all sent..

; 		ld 	A,(HL)
; 		cp	0
; 		jr 	z,endmsg

; 		ld	A,D
; 		djnz	tstout

; endmsg:
; chkRX:
; 		in	A,(SIO_0_A_C)		; read status
; 		bit	0,A					; char present ??
; 		jr z,chkRX				; check again

; 		in 	A,(SIO_0_A_D)		; read the char.

; 		out (SIO_0_A_D),A
; chkTX2:
; 		in	A,(SIO_0_A_C)		; read status
; 		bit	2,A					; all sent ?
; 		jr z,chkTX2
		
; 		jr	endmsg				; not all sent..




; 		halt
; 		halt
; 		halt
; 		inc A
; 		jr Rtll			

; 	if DOALIGN
; 		align 4
; 	endif


.end
