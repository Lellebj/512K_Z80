;Z80_PLD_PCB_.asm

		include 	"Z80_Params_.inc"
 

		
;********************************************************		
;		section MainSRam			; main program in sram
;********************************************************		

			section Samples


			xref	Bin2Hex8,Bin2Hex16,  HEX2BN, BN2DEC,BN2DEC_S,DEC2BN,MFILL, BLKMOV,STRCMP,CONCAT,POS,COPY,DELETE,INSERT_STR
			xref	InitBuffers, ReadLine, WriteChar, ReadChar, S_head_tail
			xref	Textbuf, inBufferEnd,inBuffer
			
			xref	st2g1,st1g2,steq,subst
			xref	RegLabels1,RegLabels2,RegLabels3,RegFlags
			xref	sourctext1,sourctext2,endtext,src_size, printSTRBelow
		
		xdef 	PLD_PCB_Start


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

		CALL 	InitBuffers			;INITIALIZE DART. INTERRUPT SYSTEM

		call	PIO_Init
		
		call 	CTC_Init

		call 	DART_Init
		
		call	S_head_tail			; save input heads and tails


		; call	sh_test
		;call 	Flash_WR_Test
		;ld		HL,$2010
		;call	Flash_SE_Erase

		call	CRLF
		call 	printSTRBelow
	defb    "\r\n\n"
	defb	"##########################################################\r\n"
	defb	"The Z80 Board Awakened 2023\r\n"
	defb	"    git: @@GIT_VERSION@@\r\n"
	defb	"    build: @@DATE@@\r\n"
	defb	"\r\n"
	defb	"Mix CTC/DART interrupt.\r\n"
	defb	"\0"


		call	CRLF
		; call	CRLF

		; call	printSTRBelow_CRLF
		; db		"  PIO init: D0-3 outputs ! ",0

next_line:
		; ld 		a,(0x8800)
		; ; ld 		A,3
		; out 	(portB_Data),A
		; inc 	A
		; ld 		(0x8800),A

		ld 		iy,MsgText1
		call	WriteLine
;************
		ld 		hl,Textbuf
		call	ReadLine 			;to textbuf  (A=length of input string)
		
		; call 	DumpRegisters 


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

		jr 		next_line
;********************************************************************************************

; _DI 		equ 	$80		; D7 - 1 enables interrupt
; _Counter 	equ 	$40		; D6 - 1 Counter Mode (no prescaler)		0 - Timer Mode  
; _Prescaler equ 	$20		; D5 - 1 Prescaler 256		0 - Prescaler 16
; _Rising 	equ 	$10		; D4 - 1 CLK/TRG rising		0 - CLK/TRG falling
; _CLK_TRG_Start 	equ $08 ; D3 - 1 CLK/TRG start timer  0 - automatic start during LOAD_BASE
; _TC_Followequ 	$04		; D2 - 1 time constant follows
; _Reset 	equ 	$02		; D1 - 1 Software reset
; _CW 		equ 	$01		; D0 - 1 Control word 		0 - Vector	
		


CTC_Init:
		;init CH 0 and 1
		ld 	 A,_Rising|_Prescaler|_TC_Follow|_Reset|_CW
		out		(CH0),A 		; CH0 is on hold now
		ld		A,109			; time constant (prescaler; 126; 93; 6MHz -> 1 sec peroid) 232/101; 
		; ld		A,126			; time constant (prescaler; 109; 66; 3,684MHz -> 1 sec peroid; 
		out		(CH0),A			; and loaded into channel 0
		
		
		ld	A,_INT_EN|_Counter|_Prescaler|_Rising|_TC_Follow|_Reset|_CW	
		out		(CH1),A			; CH1 counter
		ld		A,66			; time constant 66 defined
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


		ld 		A,'C'
		out 	(DART_A_D),A			; send the 'C' character after ~ 1 sec

		ld 		a,(0x8800)
		inc 	A
		ld 		(0x8800),A


		cp 		30						; Z is set 
		jr 		z,showtimeout				; check if lopp should timeout...

		in		A,(DART_A_C)			;read RRx ;test next test char available
		bit 	0,A						; char available ?
		jr 		z,blockstart			; test if minicom has begun sending Z=0...

		; NZ is set
		pop 	AF
		reti

;------------------------------------------------------------------------------




		pop 	AF
		nop
		ei
		reti


CTC_CH2_Interrupt_Handler:
CTC_CH3_Interrupt_Handler:


blockstart:
		; jsr 	SetupXMODEM_TXandRX
		call 	printSTRBelow
		defb    "\r\n"
		defb	"blockstart : start reading the blocks  !\r\n"

		pop 	AF
		reti 


showtimeout:
		call 	printSTRBelow
		defb    "\r\n"
		defb	"A timout on XMODEM occured !\r\n"

		pop 	AF
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






command_list:

		db		ETX,1,"list",0
		db		ETX,2,"dm",0
		db		ETX,3,"pc",0
		db		ETX,4,"cm",0
		db		ETX,5,"$",0
		db		ETX,6,"exe",0
		db		ETX,7,"go",0
		db		ETX,8,"++",0
		db		ETX,9,"--",0
		db		ETB


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
		LD		HL,Str4			; SOURCE STRING
		LD		DE,COPY_BUFFER	;	DESTINATION STRING
		
		LD		C,4				; STARTING INDEX FOR COPYING

		LD		B,6				; NUMBER OF BYTES TO COPY
		LD		A, 25			; MAXIMUM LENGTH OF SUBSTRING
		CALL 	COPY			; COPY SUBSTRING

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
