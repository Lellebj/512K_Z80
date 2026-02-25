; Test out PIO
;  --
; Copy a block of memory from one location to another.
; PIO A #7
; PIO A #6     
; PIO A #5     RS
; PIO A #4     Enable
; PIO A #3     DB07
; PIO A #2     DB06
; PIO A #1     DB05
; PIO A #0     DB04
;  wire 'WR' connect to GND
	
;* LCD VSS & K (blue, brown) pin to ground
;* LCD VCC & A (green, red) pin to 5V
;* 10K resistor:
;* ends to +5V and ground
;* wiper to LCD VO pin (pin 3) 
;
;
; Return registers
;      BC - Zero
; $3000 se bmZ8536_HD44780_MAIN_INIT
#target bin
include "HD_44780_defs.inc"

; Z8536 parametres
Z8536_Set_Wait_T3 	= $4280
Z8536_Init			= $4200

#code HD44780_INIT, $3000, *            ;     
	
		;//put the LCD into 4 bit or 8 bit mode
		;// this is according to the hitachi HD44780 datasheet
		;// figure 24, pg 46
		push ix
		push iy

		ld DE, $FFFF			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait.
		ld DE, D_50ms			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; resulting wating 50 ms

  		ld a,LCD_FUNCTIONSET| LCD_8BITMODE| LCD_2LINE| LCD_5x8DOTS	;$38
		call writecommand
		ld DE, D_50ms			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; resulting wating 50 ms


		ld a,LCD_FUNCTIONSET| LCD_8BITMODE| LCD_2LINE| LCD_5x8DOTS	;$38
		call writecommand
		ld DE, D_5ms			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 5 ms

		
		ld a,LCD_FUNCTIONSET| LCD_8BITMODE| LCD_2LINE| LCD_5x8DOTS	;$38
		call writecommand
		ld DE, D_5ms			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 0.5 ms
		

		;// Set # lines, font size, etc.
		ld a,LCD_FUNCTIONSET| LCD_8BITMODE| LCD_2LINE| LCD_5x8DOTS	;$38
		call writecommand
		ld DE, D_1ms			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 0.1 ms

		; turn the display on with no cursor or blinking default
		ld a,LCD_DISPLAYCONTROL| LCD_DISPLAYON | LCD_CURSOROFF| LCD_BLINKOFF  ; = $0C
		call writecommand
		ld DE, D_100us			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 0.1 ms

		;// set the entry mode
		;// Initialize to default text direction (for romance languages)
		ld a,LCD_ENTRYMODESET| LCD_ENTRYLEFT    	; =$06
		call writecommand
		ld DE, D_100us			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 0.1 ms

		; Clear display....
		ld a,LCD_CLEARDISPLAY
		call writecommand
		ld DE, D_50ms			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait.

		pop iy
		pop ix
		ret
;************************************************************************
_rowoffset:             defb 0,0x40,$14,0x54

#code cursor, $3100, *
;********************************************************************
setcursor:
						; row in D, col in E
		push IX
		push DE
		ld ix, _rowoffset
		ld a,d			; a= row
		cp 03
		jr nz, tr2
		ld d, (ix+3)
		jr sum
tr2:	cp 02
		jr nz, tr1
		ld d,(ix+2)
		jr sum
tr1:	cp 01
		jr nz, tr0
		ld d,(ix+1)
		jr sum
tr0:	ld d,(ix+0)
sum:	
		ld a,e
		add a,d				; a is final adress
		set 7,a				; LCD_SETDDRAMADDR  command
		pop DE
		pop IX
		jr writecommand 	; continue with 'command'


#code wr_data, $3140, *
;****************************************************************
writedata:   ; A contains value
		; a contain 8 bits data ; data -> rs = 1
		push AF
		out (CIOA),A		;send to command address rs pin =0

		in A,(CIOB)
		or LCD_E| LCD_RS	
		out (CIOB),A		; set E pulse and RS high
		nop
		nop
		nop
		and ~(LCD_E| LCD_RS)
		out (CIOB),A		; ; set E pulse and RS low
		
		push DE
		ld DE, D_100us			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 0.1 ms

		pop DE
		pop AF
		ret

;****************************************************************



;****************************************************************
#code wr_comm, $3180, *
writecommand:   ; A contains value
		; a contain 8 bits data ; command -> rs = 0
		push AF
		out (CIOA),A		;send to command address rs pin =0

		in A,(CIOB)
		or LCD_E
		out (CIOB),A		; set E pulse high
		nop
		nop
		nop
		and ~LCD_E
		out (CIOB),A		; ; set E pulse low
		
		push DE
		ld DE, D_100us			; timeconstant for Timer3
		call Z8536_Set_Wait_T3	; set timer and wait. 0.1 ms

		pop DE
		pop AF
		ret

;****************************************************************

;****************************************************************
#code t_busy, $31B0, *
testBusyFlag:   ; A contains value
		; check DB7 if HD44780 is busy
		push BC
		push hl

		ld b, $20
wait_busy:
		nop
		ld hl,$1234
		ld hl,$1234
		ld hl,$5678
		djnz wait_busy
		
;		in A,(CIOB)
;		or LCD_E| LCD_RW
;		out (CIOB),A		; set E and RW pulse high
;		in A,(CIOA)
;		ld C,A
;		nop
;		nop
;		nop
;		in A,(CIOB)$
;		and ~(LCD_E| LCD_RW)
;		out (CIOB),A		; ; set E and RW pulse low
;		ld A,C
;		bit 7,A				; test bit 7
;		jr NZ, testBusyFlag
		pop hl
		pop BC
		ret

;****************************************************************


.end



