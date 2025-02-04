;SIO_0 with Asynchronous Circular Buffer implementation
;
;By Brian Chiha - brian.chiha@gmail.com
;Feb 2021
;
;This is a Proof of work example to implement a Circular Buffer
;with serial data transmission using the Z80 SIO_0 for the TEC computer.
;
;There are three parts to this program:
;1. A Producer - This is data coming from the SIO_0 from an external source.
;2. A Consumer - This is the TEC that consumes data on a key press or automatically
;3. A Background Task - This is the TEC multiplexing the LED Display
;
;A Circular Buffer is a finite set of memory that has a pointer to the Head and a pointer
;to the end off the buffer.  A producer will place data at the head of the buffer, and a
;consumer will remove data at the tail of the buffer.  Once the head pointer reaches the
;end of the finite buffer, it will wrap around to the start of the buffer.  Likewise for 
;the tail pointer.  If head pointer reaches the tail pointer, no more data is accepted and
;the producer will wait until a free spot is available.  If the tail pointer reaches the 
;head pointer, the buffer is empty and the consumer waits.  
;An issue arises when the head and tail pointer are pointing to the same location.  Is
;the buffer empty or full?  To determine this, the following logic is applied.
;If the head pointer = tail pointer, then the buffer is empty.
;If the head pointer + 1 = tail pointer, then the buffer is full.
;A simple process of bit masking on the pointers will reset them to the start of the buffer
;if they reach the end.  Pointer movement and buffer adding/taking is to be done while
;interrupts are disabled to ensure no data will be corrupted by another interrupt.
;
;The producer with do the following:
; 
; - Read the current value of the head pointer
; - If the head pointer + 1 equals the tail pointer, the buffer is full and raise an error
; - Otherwise, store the data in the head position and increase the head pointer
;
;The consumer will do the following:
; - Read the current value of the tail pointer
; - If the tail pointer equals the head pointer the buffer is empty and exit
; - Otherwise, read the data in the tail position and increase the tail pointer

;In order to visualize the circular buffer is working, LED will display the current size of the
;buffer, the current byte that the consumer reads and buffer overflow or transmit status flag.
;The producer will activate when data is received.  The consumer will activate on any key press
;and will echo the data back to the terminal.  If the '+' key is pressed, the comsumer will 
;switch between Transmit on Key Press, or Transmit automatically if data is available.

;Note on keyboard and Monitor:
;Since Interrupt mode 2 uses the interrrupt register 'I', any monitor that uses this register
;to store the keyboard key pressed will not work with this program.  JMON is the only monitor
;that will work as it doesn't use the interrupt register to store the keyboard key press.
;Hardware wise, for the keyboard to work it requires EITHER a 4k7 resistor between the
;NMI (pin 17 on Z-80) and D6 (pin 10 on the Z-80) OR the DAT (LCD) expanstion board fitted
;to port 3.  The current TEC-1D boards have the JMON MOD resitor connection already there.


include "Z80_Params_.inc"



;INTERRUPT VECTOR TABLE SETUP
;The interrupt will call one of these service routines depending on the type of interrupt
;There are 4 reasons the interrupt will occur:
; 1. Transmit Buffer Empty - Indicating that data can be sent to the SIO_0
; 2. External/Status Change - Indicating a change in the modem line or break condition
; 3. Receive Character Available - Indicating that data has been sent to CPU
; 4. Special Receive Condition - Indicates a buffer overrun or parity error condtion has occured
;
;Interrupt mode 2 (IM 2), requires a 16 bit table of addresses. The High byte of the 
;address is the value in the interrupt register 'I'.  The Low byte of the address is
;placed on the data bus from the SIO_0 when an interrupt is triggered. The follwing table
;shows what bits are set on the data bus.  This is used to index the vector table:
;Note: D0, D4-7 are set via Write Register 2 (Channel B on the SIO_0).  this is set to 00H
;


;********************************************************		
		section BasicIO			; FTDI 232 communication
;********************************************************		
			ORG     0400H 
START:
;Initialize interrupt system and SIO_0
			DI                          ;Disable interrupts
						
;Initialise interrupt vectors
			LD      HL,SIO_IntVectors           ;Get Interupt high page number
			LD      A,H                 ;Save H in A
			LD      I,A                 ;Set interrupt vector high address (0B)
			IM      2                   ;Interrupt Mode 2, Vector in table

;Link interrupt vector address to handler routines
			LD      HL,Read_Handle      ;Store Read Vector
			LD      (SIO_ReadVector),HL         ;
			LD      HL,Write_Handle     ;Store Write Vector
			LD      (SIO_WriteVector),HL         ;
			LD      HL,External_Handle  ;Store External Status Vector
			LD      (SIO_ExternalVector),HL         ;
			LD      HL,Error_Handle     ;Store Receive Error Vector
			LD      (SIO_SpecialVector),HL         ;

;Initialise the SIO_0
			CALL    Init_SIO_0            ;Set up the SIO_0 

;Initialise Screen and other data
			XOR     A                   ;Reset A
			LD      HL,BufCnt           ;Buffer count
			LD      B,04H               ;Four Bytes to clear/set
S1:
			LD      (HL),A				;Load 0 to HL
			INC     HL					;Move to next address j (BufStat)
			DJNZ    S1					;Do 3 times
;Set Buffer Head and Tail pointers based of LSB of circular buffer
			LD      HL,CBufLoc			;Load Circular buffer address
			LD      A,L					;Head/Tail = LSB of buffer
			LD      (CBufHead),A		;Save initial Head pointer
			LD      (CBufTail),A		;Save initial Tail pointer

			EI							;Enable Interrrupts

;Start Background task of updating the screen buffer and multiplexing the LED's
;This will loop continually until the SIO_0 sends an interrupt.
WAIT_LOOP:
			CALL    Read_a_Key			;Read the keyboard
			CALL    update_LED			;Update the screen buffer
			CALL    Disp_SCR_Buffer		;Display the Screen Buffer
			LD      A,(AutoTransmit)	;Check for automatic transmit
			OR		A					;Is it set?
			JR      Z,WAIT_LOOP			;No, just repeat
			CALL    Transmit_Buffer         ;Check for non empty buffer and transmit
			JR      WAIT_LOOP

;SIO_0 Interrupt Handlers
;----------------------
;These four routines handle the four interrupts that the SIO_0 produces.  See above.
;When an Intrrupt is triggered, the CPU automaticaly disables interrupts, ensuring
;no other intrrupts occur when one is being handled.  Before exiting the routine,
;interrupts are to be reenabled.  RETI (Return from interrupt) is the same as RET but
;the SIO_0 recognises this instruction indicating that the interrupt routined has ended.

;Receive Character Available Interrupt handler
Read_Handle:
			PUSH    AF                  ;Save AF
;Check if buffer is full?
			LD      A,(CBufHead)		;Get the HEAD pointer
			LD      B,A					;Save in B
			LD      A,(CBufTail)		;Get the TAIL pointer
			DEC     A					;Decrease it by one
			AND     CBufSize			;Mask for wrap around
			CP      B					;Is HEAD = TAIL - 1?
			JR      NZ,Read_OK			;Different so save to buffer
;Buffer is full
			LD		A,0EEH				;Buffer is full
			LD		(BufStat),A			;Put EE in BUFF overflow
			IN		A,(SIO_A_D)		;Read overflow byte to clear interrupt
			LD		(CByteRec),A		;Save data in input buffer
			JR		Read_EXIT			;Exit Safely
;Buffer in not full
Read_OK:    
			IN		A,(SIO_A_D)		;Read data from SIO_0
			LD		(CByteRec),A		;Save data in input buffer
			LD		HL,CBufLoc			;Load Buffer in HL
			LD		L,B					;Load Head Pointer to L to index the Circular Buffer
			LD		(HL),A				;Save Data at head of buffer

			LD		A,L					;Load Head Pointer to A
			INC		A					;Increase Head pointer by 1
			AND		CBufSize			;Mask for wrap around
			LD		(CBufHead),A		;Save new head

			LD		HL,BufCnt			;Load the current buffer count
			INC		(HL)				;Increase the buffer count by 1

Read_EXIT:  
			POP		AF					;Restore AF
			EI							;Reenable Interrupts
			RETI						;Return from Interrupt
	  
;Transmit Buffer Empty Interrupt Handler, When a character is transmitted, this
;interrupt will be called when the SIO_0 clears its buffer.  It then checks for 
;more data to send.  If no more data is to be sent, to stop this interrupt from
;being repeatingly triggered, a command to reset the Transmit interrupt is sent
Write_Handle:
			PUSH	AF					;Save AF
			LD		A,(AutoTransmit)	;Check Automatic Transmit Flag
			OR		A					;If Zero then just dont transmit
			JR		Z,Reset_TX_INT		;Reset transmit interrupt
			CALL	Transmit_Buffer		;Do the Transmit, Carry flag is set if buffer is empty
			JR		NC,Exit_from_Write	;Data was tramitted, Exit Safely
Reset_TX_INT:
;Buffer is Empty, reset transmit interrupt
			LD		A,00101000B			;Reset SIO_0 Transmit Interrupt
			OUT		(SIO_A_C),A		;Write into WR0
Exit_from_Write:
			POP		AF					;Restore AF
			EI							;Reenable Interrupts
			RETI						;Return from Interrupt

;External Status/Change Interrupt Handler.  Not handled, Just reset the status interrupt
External_Handle:
			PUSH	AF					;Save AF
			LD		A,00010000B			;Reset Status Interrupt
			OUT		(SIO_A_C),A		;Write into WR0
			POP		AF					;Restore AF
			EI							;Reenable Interrupts
			RETI                        ;Return from Interrupt

;Special Receive Interrupt Handler.  Not handled, Just reset the status interrupt
Error_Handle:
			PUSH	AF					;Save AF
			LD		A,00110000B			;Reset Receive Error Interrupt
			OUT		(SIO_A_C),A		;Write into WR0
			POP		AF					;Restore AF
			EI							;Reenable Interrupts
			RETI						;Return from Interrupt

;Consume one byte if any to consume
Transmit_Buffer:
			DI							;Disable interrupts
;Check if buffer is empty?
			LD		A,(CBufTail)		;Get the TAIL pointer
			LD		B,A					;Save in B
			LD		A,(CBufHead)		;Get the HEAD pointer
			CP		B					;Does TAIL=HEAD?
			JR		NZ,TX_data_Tail		;No, Transmit data at Tail
;Buffer is Empty, set the carry flag and exit
			SCF							;Set the Carry Flag
			EI							;Restore interrupts
			RET							;Exit
;Buffer is not empty
TX_data_Tail:
			LD		HL,CBufLoc			;Load Buffer in HL
			LD		L,B					;Load Tail Pointer to L to index the Circular Buffer
			LD		A,(HL)				;Get byte at Tail.
			OUT		(SIO_A_D),A		;Transmit byte to SIO_0
;Output has occured
			LD		A,L					;Load Tail Pointer to A
			INC		A					;Increase Tail pointer by 1
			AND		CBufSize			;Mask for wrap around
			LD		(CBufTail),A		;Save new tail

			LD		HL,BufCnt			;Load the current buffer count
			DEC		(HL)				;Decrease the buffer count by 1

			OR		A					;Reset Carry Flag
			EI							;Restore interrupts
			RET							;Exit

;SIO_0 Configuration Routines
;--------------------------

Init_SIO_0:            
			LD		HL,CTLTBL			;Setup data location
			CALL	Setup_SIO_0			;Setup the SIO_0
			RET							;Exit

;Initialize the SIO_0, Requires 3 bits of information. Number of control bytes to send,
;the port to send it to and the control data.
Setup_SIO_0:              
			LD		A,(HL)				;Load Control Table (Bytes)
			OR		A					;Test for zero, no more data to load
			RET     Z                   ;Return if zero
			LD      B,A                 ;Save number of control bytes in B
			INC     HL                  ;Move to Port address
			LD      C,(HL)              ;Load C with port address (for OTIR)
			INC     HL                  ;Move to control data

			OTIR                        ;Output HL data, B times, to port C
			JR      Setup_SIO_0              ;Jump to the next port

;Control Table data for SIO_0. Refer to Z80 SIO_0 Technical Manual for more information
;on the bits set.  
CTLTBL:              
;Reset Channel A
			DB 01H                      ;1 Line
			DB SIO_A_C                   ;A Port Command
			DB 00011000B                ;write into WR0: channel reset
			
;Set Interrupt Vector and allow status to affect it. The WR2 allows the user to set
;the default base address of the vector table. Bits 1,2 and 3 are set based on the
;interrupt.  The other bits can be set here, Since my vector tables starts at 0B00,
;the register can just be set to 0;
			DB 04H                      ;4 Lines
			DB SIO_B_C                   ;B Port Command
			DB 00000010B                ;write into WR0: select WR2
			DB 00000000B                ;write into WR2: set base interrupt vector for SIO_0 (0B00)
			DB 00000001B                ;write into WR0: select WR1
			DB 00000100B                ;write into WR1: allow status to affect vector
			
;Initialise Channel A
			DB 08H                      ;8 Lines
			DB SIO_A_C                   ;A Port Command
			DB 00010100B                ;write into WR0: select WR4 / Reset Int
			DB 11000100B                ;write into WR4: presc. 64x, 1 stop bit, no parityx
			DB 00000011B                ;write into WR0: select WR3
			DB 11000001B                ;write into WR3: 8 bits/RX char; auto enable OFF; RX enable
			DB 00000101B                ;write into WR0: select WR5
			DB 01101010B                ;write into WR5: TX 8 bits, TX Enable, No RTS
			DB 00000001B                ;write into WR0: select WR1
			DB 00011011B                ;write into WR1: Int on All RX (No Parity), TX Int, Ex Int
			
			DB 00H                      ;End Initialisation Array

;Background Utilities
;--------------------

;Scan the Keyboard for input.  If any key is pressed except for '+', transmit data from the
;circular buffer to the SIO_0 if data is available.  If '+' is pressed, toggle the auto
;transmit flag. Must
Read_a_Key:
			IN      A,(03)              ;Check if key is pressed
			BIT     6,A                 ;If bit 6 is set (D6) the key has been pressed
			JR      Z,K1                ;Key pressed, action it
			XOR     A                   ;Store 0 for last key if no key pressed
			LD      (KeyPressed),A          ;
			RET                         ;Exit
K1:
;Key has been pressed
			LD      A,(KeyPressed)          ;Check if it was the same
			OR      A                   ;
			RET     NZ                  ;Just return if same
			LD      A,0FFH
			LD      (KeyPressed),A          ;Store FF in key press  
			IN      A,(00)              ;Get actual key
			AND     1FH                 ;Mask upper bits

			CP      10H                 ;Has the '+' key been pressed?
			JP      NZ,Transmit_Buffer      ;No, Jump to Transmit Byte routine and exit

;+ Pressed, toggle the auto transmit flag
			LD      A,(AutoTransmit)          ;Get the Automatic transmit flag
			CPL                         ;toggle it
			LD      (AutoTransmit),A          ;Store toggled value back
			LD      A,(BufStat)          ;Get the state of the Buffer
			XOR     0AAH                ;Toggle it (00) or (AA) 
			LD      (BufStat),A          ;Store toggled value back
			RET                         ;Exit

;Multiplex the Display
Disp_SCR_Buffer:                        ;Multiplex the displays
			LD      B,20H               ;Segment Reference
			LD      HL,LedScreen           ;Set HL to Display Buffer

SCAN_LOOP:
			LD      A,(HL)              ;Get Segment Value at HL
			OUT     (02),A              ;Set on Segment
			LD      A,B                 ;Get Segment reference
			OUT     (01),A              ;Activate segment
			LD      B,80H               ;Segment delay
D_LOOP:     DJNZ    D_LOOP
			INC     HL                  ;move to next location
			LD      B,A                 ;Save Segment reference
			XOR     A                   ;Clear A
			OUT     (01),A              ;Deactivate Segment
			RRC     B                   ;Move Segment Reference on to the Right
			JR      NC,SCAN_LOOP        ;If not passed the last segment, scan next segment
			OUT     (02),A              ;Clear port 2
			RET

;Update the LedScreen with Byte Received, Atomic Flag and Buffer size
update_LED:
			LD      BC,LedScreen		;Location of screen buffer
			LD      HL,BufCnt           ;Byte Recieved
			LD      A,(HL)
			CALL    CON_A               ;Convert A to Segment Hex, Store in BC
			INC     HL
			LD      A,(HL)              ;Atomic Flag
			CALL    CON_A               ;Convert A to Segment Hex, Store in BC
			INC     HL
			LD      A,(HL)              ;Buffer count
			CALL    CON_A               ;Convert A to Segment Hex, Store in BC
			RET

;Convert A to two display bytes for Seven Segment, Store Result in location of BC
CON_A:
			PUSH    AF                  ;Save A to keep original value
			RLCA                        ;Shift upper nibble to lower for masking
			RLCA
			RLCA
			RLCA
			CALL    CON_NIBBLE          ;Convert Lower nibble to segment hex
			POP     AF                  ;Restore A

CON_NIBBLE:
			AND     0FH                 ;Only look at lower nibble for indexing
			LD      DE,DISP_COD_TAB     ;Reference Segment convert table
			ADD     A,E                 ;Index table with A
			LD      E,A                 ;Update DE with index
			LD      A,(DE)              ;Look up table
			LD      (BC),A              ;Save it to display buffer
			INC     BC                  ;Increment buffer location
			RET

;Hex to Seven Segment lookup table
DISP_COD_TAB:
			DB      0EBH,028H,0CDH,0ADH,02EH,0A7H,0E7H,029H
			DB      0EFH,0AFH,06FH,0E6H,0C3H,0ECH,0C7H,047H
