

/*

SPI is eventually the most simple interface to implement. It was designed especially to work with low end processors. I tend to use SPI for all my micro controller designs (*1).

Unless one intends to use some dedicated hardware, like a shift register (useful if speed is of concern), all it needs are 3 port bits for the data interface, plus one port bit per device connected (*2) for it's basic signals:

    MOSI - Out - Master Out Slave In, the data the CPU sends out
    MISO - In - Master In Slave Out, the data returned by external hardware
    SCLK - Out - Serial Clock, a signal to be toggled once per bit

and

    SS/CS - Out - Slave Select, also called Chip Select, selecting the external interface.

The great advantage is that SPI does not require any specific timing (*3) as long as the basic sequence is followed. It can be handled as slow as a bit per day. Any interface able to provide three output lines and one input line can be used. Including a classic PC parallel port. Well, or a Z80 PIO.

Let's say we have a Z80-PIO connected to a Z80-CPU at I/O address 20h (*4) and use port A with its first 4 lines assigned as (*4):

    A0 - MISO - In
    A1 - MOSI - Out
    A2 - SCLK - Out
    A3 - SS/CS - Out

We also assume the device to be operated in Mode 0 (CPOL=0, CPHA=0) as the RFM95 does. For other modes this may need to be adapted.

A subroutine to initialize this setup may look like this:

(All references relate to the Z80 Family CPU Peripherals User Manual UM008101-0601)

*/

PIO1   EQU  20h     ; First PIO
PIO1AD EQU  PIO1+0  ; Data Register
PIO1AC EQU  PIO1+2  ; Command Register

SPI_INIT:
; Initialized SPI transfer on port A
; A destroyed

       LD   A,011001111b  ; Set Mode 3, Control (see p.189)
       OUT  (PIO1AC),A
       LD   A,000001110b  ; Line 1/2/3 as output all other input (see p.190) (*5)
       OUT  (PIO1AC),A
       LD   A,000000111b  ; Interrupt control, no interrupt (see p.191)
       OUT  (PIO1AC),A
       LD   A,000001111b  ; Set CS high to deselect
       OUT  (PIO1AD),A
       RET

;   By calling SPI_INIT the interface will be put into operating condition. 
;   Since sending and receiving is interleaved a single routine can be used: 

SPI_CLK EQU  4
SPI_CS  EQU  8 

SPI_SENDREC:
; Starts or continues an SPI transaction by SENDing a byte
; while RECeiving the peripherals response at the same time
; Called with byte to send in A
; Returns with byte received in A
       PUSH DE
       PUSH BC
       LD   D,A           ; Byte to send in D
       LD   A,000000000b  ; CS Low, Clock Low
       OUT  (PIO1AD),A
       LD   B,8           ; 8 bits per byte
BITLOOP:
       LD   A,000000000   ; Clock Low, Data Low
       RL   D             ; Isolate bit to send
       JR   NC,BCLR       ; Is it a zero bit?
       LD   A,000000100   ; No? Then set data high
BCLR:
       OUT  (PIO1AD),A    ; Set data value
       MOV  C,A           ; Save value with clock low (*6)
       OR   A,SPI_CLK     ; Set clock
       OUT  (PIO1AD),A    ; Data and Clock
       IN   A,(PIO1AD)    ; Read input data
       RRCA               ; Isolate input bit to carry
       RRCA
       MOV  A,C           ; Restore Data with clock cleared (*7)
       OUT  (PIO1AD),A    ; Clear Clock (*6)
       RL   E             ; Insert bit from Carry into E
       DJNZ BITLOOP       ; Next bit?

       LD   A,E           ; Return value in A
       POP  BC
       POP  DE
       RET


/*
The important part is, as mentioned, to stay with the sequence:

    Activate CS
    Apply MOSI
    Raise Clock
    Read MISO
    Lower Clock
    Repeat step 2 thru 5 for all bits/bytes to be transferred
    Deactivate CS

Note that the routine does not perform step 7, it leaves chip select active. This is the base for multi-byte transfer, important for the RFM95, as each of it's transfers always starts with an address followed by one or more data bytes. CS has to stay active during such a transfer over all bytes to be written (and read), acting as transaction marker.

The SENDREC function is written in a way that multiple bytes can be chained by calling it in sequence as often as needed. Of course we now need a another function to end a transfer:
*/

SPI_DONE:
; Ends an SPI transaction
; A destroyed
       LD   A,000001111b  ; Set CS high to deselect
       OUT  (PIO1AD),A
       RET


/*
Let's for example put the RFM95 into LoRa mode. 
This requires to set the high bit of the Operation Mode Register at address 01h needs (See p.102 of the manual). 
To do so we need to send one byte with the registers address, followed by the new value, all in in one transaction handled by CS:
*/

;...
       LD   A,01h         ; Address of Operation Mode Register
       CALL SPI_SENDREC   ; Start transaction and send address
       LD   A,010000000b  ; Set LoRa mode + Sleep
       CALL SPI_SENDREC   ; Continue transaction and send data
       CALL SPI_DONE      ; End transaction
;...





