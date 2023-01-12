;Z80_PLD_PCB_.asm



; ;Function addresses
; setcursor		= $3100
; writecommand	= $3180
; writedata		= $3140
; testBusyFlag    = $31B0
; HD44780_main_init = $3000




; PIO addressess...
portA_Contr:	equ _Z80PIO_Base + 1
portB_Contr:	equ _Z80PIO_Base + 3
portA_Data:		equ _Z80PIO_Base + 0
portB_Data:		equ _Z80PIO_Base + 2


; DART stuff

DART_A_D:		equ	_Z80DART_Base
; DARTCAD			EQU		DART_A_D				;DART CHANNEL A DATA
DART_A_C:		equ	_Z80DART_Base+2
; DARTCAS			EQU		DART_A_C				;DART CHANNEL A COMMANDS/STATUS
DART_B_D:		equ	_Z80DART_Base+1
; DARTCBD			EQU		DART_B_D				;DART  CHANNEL B DATA
DART_B_C:		equ	_Z80DART_Base+3
; DARTCBS			EQU		DART_B_C				;DART CHANNEL B COMMANDS/STATUS

DART_Int_Vec		EQU     $F400				; INTERRUPT VECTOR 
; DARTIV				EQU		DART_Int_Vec			; INTERRUPT VECTOR
DART_Int_WR_Vec		EQU     DART_Int_Vec+8		;DART CHANNEL A WRITE INTERRUPT VECTOR
; DARTWV	EQU		DART_Int_Vec+8		;DART CHANNEL A WRITE INTERRUPT VECTOR
DART_Int_EXT_Vec	EQU     DART_Int_Vec+10		;DART CHANNEL A EXTERNAL/STATUS ; INTERRUPT VECTOR
; DARTEV	EQU		DART_Int_Vec+10     	;DART CHANNEL A EXTERNAL/STATUS INTERRUPT VECTOR
DART_Int_Read_Vec	EQU     DART_Int_Vec+12		;DART CHANNEL A READ INTERRUPT VECTOR
; DARTRV	EQU		DART_Int_Vec+12		;DART CHANNEL A READ INTERRUPT VECTOR
DART_Int_Spec_Vec	EQU     DART_Int_Vec+14		;DART CHANNEL A SPECIAL RECEIVE INTERRUPT VECTOR
; DARTSV	EQU		DART_Int_Vec+14		;DART CHANNEL A SPECIAL RECEIVE


;Write register 0
WR0 equ 0
_Reset_STAT_INT	equ 2<<3			; 10   000 0010 -> 001 0000
_Ch_Reset		equ 3<<3			; 18
_EN_INT_Nx_Char	equ 4<<3			; 20  000 0100  -> 010 0000
_Reset_TxINT_Pend equ 5<<3			; 28   000 0101 ->  010 1000
_Error_Reset 	equ 6<<3			; 30	
_Ret_F_Int		equ 7<<3			; 38  0000111  ->  0011 1000

;Write register 1
WR1 equ 1
_Ext_INT_EN			equ 1
_Tx_INT_EN 			equ 2
_Status_Vector		equ 4
_Rx_INT_Disable		equ 0
_Rx_INT_First_Char  equ $08
_INT_All_Rx_Char_P 	equ $10 
_Int_All_Rx_Char_NP	equ $18
_WAIT_READY_R_T 	equ $20
_WAIT_READY_FUNC	equ $40
_WAIT_READY_EN		equ $80

WR2 equ 2

;Write register 3
WR3 equ 3
_Rx_Enable 			equ $01
_Auto_Enable		equ $20
_RX_5_bits			equ $00
_RX_6_bits			equ $40
_RX_7_bits			equ $80
_RX_8_bits			equ $C0

;Write register 4
WR4 equ 4
_Parity_EN			equ $01
_Parity_Even		equ $02
_Stop_1_bit			equ $04
_Stop_1_2_bit		equ $08
_Stop_2_bit			equ $0C
_X1_Clock_mode		equ $00
_X16_Clock_mode		equ $40
_X32_Clock_mode		equ $80
_X64_Clock_mode		equ $C0

;Write register 5
WR5 equ 5
_RTS_Enable			equ $02
_Tx_Enable 			equ $08
_Send_Break			equ $10
_Tx_5bits_char		equ $00
_Tx_7bits_char		equ $20
_Tx_6bits_char		equ $40
_Tx_8bits_char		equ $60
_DTR_Enable 		equ $80





; CTC stuff
CH0		equ 	_Z80CTC_Base	
CH1		equ 	_Z80CTC_Base+1	
CH2		equ 	_Z80CTC_Base+2	
CH3		equ 	_Z80CTC_Base+3	

_INT_EN 	equ 	$80		; D7 - 1 enables interrupt
_Counter 	equ 	$40		; D6 - 1 Counter Mode  		0 - Timer Mode
_Prescaler	equ 	$20		; D5 - 1 Prescaler 256		0 - Prescaler 16
_Rising 	equ 	$10		; D4 - 1 CLK/TRG rising		0 - CLK/TRG falling
_CLK_TRG_Start 	equ $08		; D3 - 1 CLK/TRG start timer  0 - automatic start during LOAD_BASE
_TC_Follow 	equ 	$04		; D2 - 1 time constant follows
_Reset 		equ 	$02		; D1 - 1 Software reset
_CW 		equ 	$01		; D0 - 1 Control word 		0 - Vector	

; Interrupt vectors for CTC
CTC_CH0_I_Vector	EQU     DART_Int_Vec+$10		;CTC channel 0 interrupt vector 
CTC_CH1_I_Vector	EQU     DART_Int_Vec+$12		;CTC channel 1 interrupt vector 
CTC_CH2_I_Vector	EQU     DART_Int_Vec+$14		;CTC channel 2 interrupt vector 
CTC_CH3_I_Vector	EQU     DART_Int_Vec+$16		;CTC channel 3 interrupt vector 

; Data tables  (upper EPROM/FLASH)
; Interupt_vector		= DART_Int_Vec
; stack  				= $FF00
Heap				= $F000				; space for variable storage

; variables  	(upper ram)
PIO_B_value:		= $F1D0
Result_NumToHex:	= $F1F0


; _RAMSTART		= $8010	
; _EETESTPROG		= $100	
	
; _Z80PIO_Base	= $0
; _Z80CTC_Base	= $10
; _Z80DART_Base	= $20
; _Z80_BankCS  	= 0x30;		/*	 30 - 3F   */
; _8Bitsout 		= $40;		/*	 40 - 4F  */
; _CE_RST_BANK 	= 0x80;		/*	 40 - 4F  */


EscChar:		EQU		1BH				;ASCII EscChar CHARACTER
TestChar:		EQU		'A'				;TEST CHARACTER = A
CRChar:			EQU		0DH				; carrige return
LFChar:			EQU		0AH				; line feed
NUL			EQU		00
SOH			EQU		01
STX			EQU		02
ETX			EQU		03
EOT			EQU		04
ENQ			EQU		05
ACK			EQU		06
BEL			EQU		07
BS			EQU		08h
HT			EQU		09h
LF			EQU		0AH
VT			EQU		0BH
FF			EQU		0CH
CR			EQU		0DH
SO			EQU		0Eh
SI			EQU		0Fh
DLE			EQU		10h
DC1			EQU		11h
DC2			EQU		12h
DC3			EQU		13h
DC4			EQU		14h
NAK			EQU		15h
SYN			EQU		16h
ETB			EQU		17h
CAN			EQU		18h
EM			EQU		19h
SUB			EQU		1Ah
ESC			EQU		1Bh
FS			EQU		1Ch
GS			EQU		1Dh
RS			EQU		1Eh
US			EQU		1Fh
SP			EQU		20h


; 	Variables in SRAM   HEAP TOP  - Moved to load script !
;DATA LOCATIONS
; LedScreen:		EQU     0D00H ;Byte Recieved, Atomic Flag and Buffer size for LED display      (6-bytes)
; BufCnt:			EQU     0D06H ;Number of bytes active in the buffer                            (1-byte)
; BufStat:		EQU     0D07H ;The state of the buffer okay (00), error (EE), auto (AA)        (1-byte)
; CByteRec:		EQU     0D08H ;Current byte received from DART                                  (1-byte)
; AutoTransmit:	EQU     0D09H ;Flag to automatically transmit to DART, or by key press FF=auto  (1-byte)
; KeyPressed:		EQU     0D0AH ;Key Pressed Flag, 00-False, FF-True                             (1-byte)

;BUFFER CONFIGURATION
; CBufLoc:		EQU     0B00H ;Location of circular buffer
; CBufHead:		EQU     0D0BH ;Pointer to the Head of the Circular buffer                      (1-byte)
; CBufTail:		EQU     0D0CH ;Pointer to the Tail of the Circular buffer                      (1-byte)

;BUFFER SIZES, change to suit
; BUFF16:			EQU     0FH   ;16 bytes
; BUFF32:			EQU     1FH   ;32 bytes
; BUFF64:			EQU     3FH   ;64 bytes
; BUFF128:		EQU     7FH   ;128 bytes
; BUFF256:		EQU     0FFH  ;256 bytes
; CBufSize:		EQU     BUFF32  ;16 bytes (Change if required)


;INTERRUPT VECTOR TABLE SETUP
;The interrupt will call one of these service routines depending on the type of interrupt
;There are 4 reasons the interrupt will occur:
; 1. Transmit Buffer Empty - Indicating that data can be sent to the DART
; 2. External/Status Change - Indicating a change in the modem line or break condition
; 3. Receive Character Available - Indicating that data has been sent to CPU
; 4. Special Receive Condition - Indicates a buffer overrun or parity error condtion has occured
;
;Interrupt mode 2 (IM 2), requires a 16 bit table of addresses. The High byte of the 
;address is the value in the interrupt register 'I'.  The Low byte of the address is
;placed on the data bus from the DART when an interrupt is triggered. The follwing table
;shows what bits are set on the data bus.  This is used to index the vector table:
;Note: D0, D4-7 are set via Write Register 2 (Channel B on the DART).  this is set to 00H
;
; Channel   D3  D2  D1  Addr  Interrupt type
; -------   --  --  --  ----  --------------
;    B       0   0   0   00H  Transmit Buffer Empty
;    B       0   0   1   02H  External/Status Change
;    B       0   1   0   04H  Receive Character Available
;    B       0   1   1   06H  Special Receive Condition
;    A       1   0   0   08H  Transmit Buffer Empty
;    A       1   0   1   0AH  External/Status Change
;    A       1   1   0   0CH  Receive Character Available
;    A       1   1   1   0EH  Special Receive Condition
;
; DART_IntVectors:		EQU		0E00H      ;Interrupt Vector Base
; DART_WriteVector:		EQU		DART_IntVectors+08H ;Write Interrupt Vector
; DART_ExternalVector:	EQU		DART_IntVectors+0AH ;External Status Interrupt Vector
; DART_ReadVector:		EQU		DART_IntVectors+0CH ;Read Interrupt Vector
; DART_SpecialVector:		EQU		DART_IntVectors+0EH ;Special Receive Interrupt Vector


	; DART_A_D			EQU 	1CH					;DART CHANNEL A DATA
	; DART_B_D			EQU 	1EH					; DART CHANNEL B DATA
	; DART_A_C			EQU		1DH					;DART CHANNEL A COMMANDS/STATUS
	; DART_B_C			EQU		1FH					;DART CHANNEL B COMMANDS/STATUS

;****************************************************************************


flash_disable:		equ	0x70		; dummy-read from this port to disable the FLASH

gpio_in:		equ     portA_Data		; GP input port (repl with PIO A)
gpio_out:		equ		portA_Data		; GP output port (repl with PIO A)



; Define the memory size to be used for the CP/M configuration
MEM:    equ 60

; The CPM origin will be at: (MEM-7)*1024
; This screwy convention is due to the way that that the CP/M origin is defined.
CPM_BASE:	equ	(MEM-7)*1024

LOAD_BASE:	equ	0xc000		; where the boot loader reads the image from the SD card

stacktop: equ	LOAD_BASE	; So the SD loader does not overwrite!

