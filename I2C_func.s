;* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;* TRIANGLE.ASM
;* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;* Purpose: Test of I2C bit-banging using the J1A
;* Target: 705J1A
;* Author: Brad Bierschenk, MMD Applications
;* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;* Tested using Maxim I 2 C DAC IC, MAX517
;* Has a "2-wire interface" (another word for I 2 C)
;*
;* This code continuously sends 8-bit data to the
;* Digital to Analog IC, incrementing from $00 to
;* $FF, and back down again. This creates a
;* triangular waveform at the output of the DAC chip.
;*
;* The SCL frequency is approximately 28 kHz. This is
;* completely arbitrary.
;* -------------------------------------------------------------
;* Assembler Equates
;* -------------------------------------------------------------
		include 	"Z80_Params_.inc"
		Section Functions
RAMSPACE	EQU	$C0				;RAM start address
ROMSPACE	EQU	$300			;EPROM start address
PORTA		EQU $00				;Port A
PORTB		EQU $01				;Port B
DDRA		EQU $04				;Data direction A
DDRB		EQU $05				;Data direction B
;* -------------------------------------------------------------
;* Emulated I2C lines on Port A pins
;* Need a clock (SCL) and data (SDA)
;* -------------------------------------------------------------
SCL			EQU	0				;Serial clock
SDA			EQU	1				;Serial data
DACADDR		EQU	27				;Slave address of DAC
;* -------------------------------------------------------------
;* RAM Variables
;* -------------------------------------------------------------
; ORG			RAMSPACE
BitCounter: DB	1				;Used to count bits in a Tx
Value:		DB	1				;Used to store data value
Direction:	DB	1				;Indicates increment or	decrement
;* -------------------------------------------------------------
;* Start of program code
;* -------------------------------------------------------------
; ORG			ROMSPACE			;Start of EPROM
Start:
		;Initialize variables
		ld  	A,00
		ld 		(Value),A				;Clear all RAM variables
		ld		(BitCounter),A		
		ld		(Direction),A			
		;Setup parallel ports

		ld		A,0xCF				; set PIO B to bit mode
		ld		(DDRA),A			;driven high to start
		out 	(portA_Contr),A
	
;* -------------------------------------------------------------
;* This main loop just ramps up and down the data
;* value that is sent to the DAC chip.
;* -------------------------------------------------------------
TxLoop:
		LD		A,(Direction)			;Increment or decrement?
		jr 		Z,GoUp
GoDown:
		LD		A,(Value)				;Decrement
		jr 		NZ,GD2					;Change direction if needed
		ld 		A,00
		ld 		(Direction),A
		jr		SendIt
GD2:
		ld 		HL,Value
		DEC		(HL)				;Decrement the data value
		jr		SendIt
GoUp:
		LD		A,(Value)				;Increment
		CP		A,$FF				;Change direction if needed
		jr 		NZ,GU2
		ld 		HL,Direction
		inc		(HL)				;Increment the data value
		jr		SendIt
GU2:
		ld 		HL,Value
		inc		(HL)

;* -------------------------------------------------------------
;* Send the I 2 C transmission, including START, address,
;* data, and STOP
;* -------------------------------------------------------------
SendIt:
		;START condition
		call	I2CStartBit			;Give START condition

		;ADDRESS byte, consists of 7-bit address + 0 as LSbit
		LD		A,DACADDR			;Slave device address
		sla		A					;Need this to align address
		call		I2CTxByte			;Send the eight bits

		;DATA bytes
		LD		A,$00				;$00 is command byte for DAC
		call	I2CTxByte			;Send the 8 bits
		LD		A,(Value)				;Value is value to set DAC

		call 	I2CTxByte			;Send it

		;STOP condition
		call	I2CStopBit 			;Give STOP condition
		call	I2CBitDelay 		;Wait a bit
		jr		TxLoop				;Repeat
	

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; I2CTxByte
; Transmit the byte in Acc to the SDA pin
; (Acc will not be restored on return)
; Must be careful to change SDA values only while SCL is low,
; otherwise a STOP or START could be implied
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CTxByte:
		;Initialize variable
		ld 		HL,BitCounter
		LD		D,$08
		ld 		D,(HL)		

I2CNextBit:
		RL		A					;Shift MSbit into Carry
		jr		NC,SendLow 		;Send low bit or high bit
SendHigh:
		ld 		HL,PORTA
		SET		SDA,(HL)		;Set the data bit value
		call	I2CSetupDelay 	;Give some time for data
setup:
		ld 		HL,PORTA
		SET		SCL,(HL)		;Clock it in
		call	I2CSetupDelay	;Wait a bit
		jr		I2CTxCont		;Continue

SendLow:
		ld 		HL,PORTA
		res		SDA,(HL)
		call	I2CBitDelay
		SET		SCL,(HL)
		call		I2CBitDelay
I2CTxCont:
		ld 		HL,PORTA
		res		SCL,(HL)		;Restore clock to low state
		ld 		HL,BitCounter
		DEC		(HL)		;Decrement the bit counter
		jr 		Z,I2CAckPoll		;Last bit?
		jr		I2CNextBit

I2CAckPoll:
		ld 		HL,PORTA
		SET		SDA,(HL)
		ld 		HL,DDRA
		res		SDA,(HL)		;Set SDA as input
		call		I2CSetupDelay
		ld 		HL,PORTA
		SET		SCL,(HL)		;Clock the line to get ACK
		call	I2CBitDelay
		; BRSET	SDA,PORTA,I2CNoAck	;Look for ACK from slave device
		bit 	SDA,(HL)
		jr 		Z,I2CNoAck

		res		SCL,(HL)		;Restore clock line
		ld 		HL,DDRA
		SET		SDA,(HL)		;SDA back as output
		ret
	
		;No acknowledgment received from slave device
		;Some error action can be performed here
		;For now, just restore the bus
I2CNoAck:
		ld 		HL,PORTA
		res		SCL,(HL)
		ld 		HL,DDRA
		SET		SDA,(HL)
		ret

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; A START condition is defined as a falling edge
; on SDA while SCL is high
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-
I2CStartBit:
		ld 		HL,PORTA
		res		SDA,(HL)
		call	I2CBitDelay
		res		SCL,(HL)
		ret


;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; A STOP condition is defined as a rising edge
; on SDA while SCL is high
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CStopBit:
		ld 		HL,PORTA
		res		SDA,(HL)
		SET		SCL,(HL)
		SET		SDA,(HL)
		call	I2CBitDelay
		ret

;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; Provide some data setup time to allow
; SDA to stabilize in slave device
; Completely arbitrary delay (10 cycles)
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CSetupDelay:
		NOP
		NOP
		ret
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Bit delay to provide (approximately) the desired
; SCL frequency
; Again, this is arbitrary (16 cycles)
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
I2CBitDelay:
		NOP
		NOP
		NOP
		NOP
		NOP
		ret
;* -------------------------------------------------------------
;* Vector Definitions
;* -------------------------------------------------------------
		; ORG		$07FE		;Reset vector
		; FDB		Start		