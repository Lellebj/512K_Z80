;XMODEM_SUB.s

		include 	"Z80_Params_.inc"
 
		xref 	PLD_PCB_Start
		xdef 	RAM_Start,SetupXMODEM_TXandRX,RX_EMP,TX_EMP,TX_NAK,TX_ACK,DART_A_DI,DART_A_EI,DART_A_RESET
		xdef 	A_RTS_OFF,A_RTS_ON,DART_A_TXRX_INToff,DART_A_TXon,DART_A_RXon,DART_A_TXRX_INTon,TX_C,TX_X
;********************************************************		
;		Routines in order to read data via XMODEM on DART chA
;********************************************************		

		section Functions
RAM_Start:
		jp 		PLD_PCB_Start

;********************************************************************************************
;********************************************************************************************
;********************************************************************************************

DART_A_EI:
		;enable DART channel A RX
		ld		a,003h			;write into WR0: select WR3
		out		(DART_A_C),A
		ld		a,0C1h			;RX 8bit, auto enable off, RX on
		out		(DART_A_C),A	Channel A RX active
		RET


DART_A_TXRX_INToff:
		;enable DART channel A RX
		ld		A,WR1			;write into WR0: select WR1
		out		(DART_A_C),A
		ld		A,00h			;RX and TX interrupt off
		out		(DART_A_C),A	Channel A RX 
		RET

DART_A_TX_INTon:
		;enable DART channel A RX
		ld		A,WR1							;write into WR0: select WR1
		out		(DART_A_C),A
		ld		A,_Tx_INT_EN		 			;TX interrupt on
		out		(DART_A_C),A	Channel A RX active
		RET

DART_A_RX_INTon:
		;enable DART channel A RX
		ld		A,WR1							;write into WR0: select WR1
		out		(DART_A_C),A
		ld		A,_Int_All_Rx_Char_NP		 			;TX interrupt on
		out		(DART_A_C),A	Channel A RX active
		RET

DART_A_TXRX_INTon:
		;enable DART channel A RX
		ld		A,WR1							;write into WR0: select WR1
		out		(DART_A_C),A
		ld		A,_Tx_INT_EN|_Int_All_Rx_Char_NP		 			;RX and TX interrupt on
		out		(DART_A_C),A	Channel A RX active
		RET


A_RTS_OFF:
		;signaling the host go or nogo for reception
		ld		a,005h			;write into WR0: select WR5
		out		(DART_A_C),A
		ld		a,_Tx_8bits_char|_Tx_Enable 				;TX 8bit, BREAK off, TX on, RTS inactive
		ld		a,0E8h			
		out		(DART_A_C),A 
		ret 
		
		
A_RTS_ON:
		; signaling the host go or nogo for reception
		ld		a,005h			;write into WR0: select WR5
		out		(DART_A_C),A
		; ld		a,_Tx_8bits_char|_Tx_Enable|_RTS_Enable 		;TX 8bit, BREAK off, TX on, RTS active
		ld		a,0EAh	
		out		(DART_A_C),A 
		ret 
		
	
	
DART_A_DI:
		;disable DART channel A RX
		ld		a,WR3			;write into WR0: select WR3
		out		(DART_A_C),A
		ld		a,_RX_8_bits|_Rx_Disable			;RX 8bit, auto enable off, RX off
		out		(DART_A_C),A
		;Channel A RX inactive
		ret

RX_CHA_AVAILABLE:
		;  character received routine
		push	AF				;backup AF
		call	A_RTS_OFF
		in		A,(DART_A_D)		;read RX character into A

		;examine received character:
		cp		0Dh				;was last RX char a CR ?
		jp		z,RX_CR
		cp		08h				;was last RX char a BS ?
		jp		z,RX_BS
		cp		7Fh				;was last RX char a DEL ?
		jp		z,RX_BS

		;echo any other received character back to host
		out		(DART_A_D),A


		;do something useful with the received character here !
		call	TX_EMP
		call	RX_EMP			;flush receive buffer
		jp		EO_CH_AV

RX_CR:
		;do something on carriage return reception here
		jp		EO_CH_AV

RX_BS:
		;do something on backspace reception here
		jp		EO_CH_AV
EO_CH_AV:
		ei						;see comments below
		call	A_RTS_ON		;see comments below
		pop		AF				;restore AF
		Reti
	

SPEC_RX_CONDITON:
		jp		0000h

RX_EMP:
		; flushing the receive buffer
		;check for RX buffer empty
		;modifies A
		sub		a				;clear a, write into WR0: select RR0
		out		(DART_A_C),A
		in		A,(DART_A_C)		;read RRx
		bit		0,A
		ret		z				;if any rx char left in rx buffer

		in		A,(DART_A_D)		;read that char
		jp		RX_EMP		


TX_EMP:
		; ransmitting a character to host
		; check for TX buffer empty
		sub		a				;clear a, write into WR0: select RR0
		inc		a				;select RR1
		out		(DART_A_C),A
		in		A,(DART_A_C)	;read RRx
		bit		0,A
		jp		z,TX_EMP
		ret
	

;**************************************************************************
;**				SetupXMODEM_TX and RX:									**
;**************************************************************************

SetupXMODEM_TXandRX:
		; SETUP 1

		; DART interrupt vector table

		ld		HL,BYTE_AVAILABLE       	; ON INTERRUPT PAGE
		ld		(DART_Int_Read_Vec),HL		;STORE READ VECTOR
		; ld		HL,WriteINTHandler
		; ld		(DART_Int_WR_Vec),HL		;STORE WRITE VECTOR
		; ld		HL,ExternINTHandler
		; ld		(DART_Int_EXT_Vec),HL		;STORE EXTERNAL/STATUS VECTOR
		ld		HL,SPEC_RX_CONDITON
		ld		(DART_Int_Spec_Vec),HL		;STORE SPECIAL RECEIVE VECTOR

		ld 		HL,$1015
		ld 		(TempVar2),HL			;holds number of unsuccessful block transfers/block during download	


		; ld		a,_Ch_Reset|WR0
		; out 		(DART_A_C),A 
		; ld		a,WR4			;write into WR0: channel reset
		; out		(DART_A_C),A
		; ld		a,44h			;44h write into WR4: clkx16,1 stop bit, no parity
		; out		(DART_A_C),A 
	

		 ;write into WR0: select WR5		
		; ld		a,WR5			;write into WR0: select WR5
		; out		(DART_A_C),A
		; ld		a,0E8h			;DTR active, TX 8bit, BREAK off, TX on, RTS inactive
		; out 		(DART_A_C),A
		
		; ld 		a,WR1			;write into WR0: select WR1
		; out		(DART_B_C),A
		; ld		a,00000100b		;no interrupt in CH B, special RX condition affects vect
		; out		(DART_B_C),A 
		

		
		; ld		a,WR2			;write into WR0: select WR2
		; out		(DART_B_C),A
		; ld		a,10h			;write into WR2: cmd line int vect (see int vec table)
		; out 		(DART_B_C),A 	;bits D3,D2,D1 are changed according to RX condition	
	
		;  	Stop CTC channel 1 from sending interrupts.
		ld	A,_Counter|_Rising|_Reset|_CW	
		out		(CH1),A			; CH1 counter


		; SETUP 2
		sub		a
		ld		(TempVar2),A		;reset bad blocks counter
		ld		C,1h			;C holds first block nr to expect
		ld 		HL,$B000		;set lower destination address of file
		call	DART_A_TXRX_INTon
		call	A_RTS_ON
		call 	TX_NAK			;'C' indicates CRC mode ready for transmisDARTn to host


REC_BLOCK:
		;set block transfer mode
		ld		a,_EN_INT_Nx_Char|WR1			;write into WR0 cmd4 and select WR1 ( enable INT on next char)
								;_EN_INT_Nx_Char|WR1
		out		(DART_A_C),A
		ld		a,_WAIT_READY_EN|_WAIT_READY_R_T|_Rx_INT_First_Char		;wait active, interrupt on first RX character
		out		(DART_A_C),A		;buffer overrun is a spec RX condition

		ei

		call	A_RTS_ON

		halt					;await first rx char

		call	A_RTS_OFF
		
		ld		a,WR1			;write into WR0: select WR1
		out		(DART_A_C),A
		ld		a,00101000b		;wait function inactive
								;_WAIT_READY_R_T|_Rx_INT_First_Char
		out		(DART_A_C),A
	

		;check return code of block reception (e holds return code)
		ld		a,e
		cp		NUL				;block finished, no error
		jp		z,l_210
		cp		$02				;eot found (end of transmission)
		jp		z,l_211
		cp		$03				;chk sum error
		jp		z,l_613
		ld		a,10h
		jp		l_612
l_210:
		call	TX_ACK			 ;when no error
		inc		C				;prepare next block to receive
		sub		A
		ld		(TempVar2),A		;clear bad block counter
		jp		REC_BLOCK

	

l_211: 
		call	TX_NAK 			;on eot
		ld		A,01h
		jp 		l_612

l_613: 
		call 	TX_ACK 			;on chk sum error
		scf
		ccf						;clear carry flag
		ld		DE,0080h		;subtract 80h
		sbc 	HL,DE 			;from HL, so HL is reset to block start address

		ld		A,(TempVar2)		;count bad blocks in TempVar2
		inc		A
		ld		(TempVar2),A
		cp		09h
		jp		z,l_612			;abort download after 9 attempts to transfer a block
		jp 		REC_BLOCK		;repeat block reception


l_612:
DLD_END:
		ret


		;  	interrupt service routine during file transfer
BYTE_AVAILABLE:
EXP_SOH_EOT:
		in		A,(DART_A_D)			;read RX byte into A
l_205:
		cp		SOH					;check for SOH
		jp		z,EXP_BLK_NR
		cp		EOT					;check for EOT
		jp		nz,L_2020
		ld		e,2h
		reti






		;await block number
EXP_BLK_NR:
		in		A,(DART_A_D)		;read RX byte into A	
		cp		C					;check for match of block nr
		jp		nz,L_2020



		;await complement of block number
		ld		A,C					;copy block nr to expect into A
		CPL							;and cpl A	
		ld		E,A					;E holds cpl of block nr to expect

EXP_CPL_BLK_NR:
		in		A,(DART_A_D)		;read RX byte into A
		cp		E					;check for cpl of block nr
		jp		nz,L_2020


		;await data block
		ld		D,0h				;start value of checksum
		ld		B,80h				;defines block size 128byte

EXP_DATA:
		in		A,(DART_A_D)		;read RX byte into A
		ld		(HL),A				;update
		add		A,D
		ld		D,A					;checksum in D
		inc		HL					;dest address +1
		djnz	EXP_DATA			;loop until block finished




EXP_CHK_SUM:
		in		A,(DART_A_D)		;read RX byte into A
		;
		;ld		a,045h				;for debug only

		cp		D					;check for checksum match	
		jp		z,L_2021
		ld		e,3h
		reti


L_2020: 
		ld		E,1h
		RETI
L_2021: 
		ld		E,0h
		RETI						;return when block received completely






		;*******************­­­­­­­Int routine on RX overflow­­­­­­­­­­­­­­­­­­­­­*******************
SPEC_BYTE_COND:						;in case of RX overflow prepare abort of transfer
		ld		HL,DLD_END
		push	HL
		reti


			
TX_NAK:
		ld 		a,NAK				;send NAK 15h to host
		out		(DART_A_D),A
		call	TX_EMP
		RET



TX_ACK:
		ld		 A,ACK				;send AK to host
		out		(DART_A_D),A
		call	TX_EMP
		ret


TX_C:
		ld		 A,'C'				;send 'C' to host
		out		(DART_A_D),A
		call	TX_EMP
		RET

TX_X:
		ld		 a,'X'				;send 'C' to host
		out		(DART_A_D),A
		call	TX_EMP
		RET





	;***************************************************************
	;SAMPLE EXECUTION:
	;***************************************************************



