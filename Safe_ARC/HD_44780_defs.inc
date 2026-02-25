
;CIO hardware addresses
CIOC:   EQU     $A0       		; CIO PORT C
CIOB:   EQU     CIOC+1  		; CIO PORT B
CIOA:   equ     CIOC+2  		; CIO PORT A
CIOCTL: equ     CIOC+3  		; CIO CTRL PORT

;I/D	= 1:Increment
;I/D	= 0:Decrement
;S	= 1:Accompanies display shift
;S/C	= 1:Display shift
;S/C	= 0:Cursor move
;R/L= 1:Shift to the right
;R/L= 0:Shift to the left
;DL= 1:8 bits, DL = 0: 4 bits
;N= 1:2 lines, N = 0: 1 line
;F = 1:5 × 10 dots, F = 0: 5 × 8 dots
;BF	= 1:Internally operating
;BF	= 0:Instructions acceptable DDRAM: Display data RAM

LCD_CLEARDISPLAY	equ		0x01
LCD_RETURNHOME 		equ		0x02

;// flags for display entry mode
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Entry mode set 			0 	0 	0 	0 	0 	0 	0 	1 	I/D S 	Sets cursor move direction
;and specifies display shift. These operations are performed during data write and read.
LCD_ENTRYMODESET 	equ		0x04
;********************************
LCD_ENTRYRIGHT 		equ		0x00
LCD_ENTRYLEFT 		equ		0x02
LCD_ENTRYSHIFTINCREMENT 	equ		0x01
LCD_ENTRYSHIFTDECREMENT 	equ		0x00

;// flags for display on/off control
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Display on/off control 	0 	0 	0 	0 	0 	0 	1 	D 	C 	B 	Sets entire display (D) on/off, 37 μs
;cursor on/off (C), and
;blinking of cursor position character (B).
LCD_DISPLAYCONTROL 	equ		0x08
;********************************
LCD_DISPLAYON 		equ		0x04
LCD_DISPLAYOFF 		equ		0x00
LCD_CURSORON 		equ		0x02
LCD_CURSOROFF 		equ		0x00
LCD_BLINKON 		equ		0x01
LCD_BLINKOFF 		equ		0x00

;// flags for display/cursor shift
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Cursor or display shift 	0 	0 	0 	0 	0 	1 	S/C R/L — 	— 	Moves cursor and shifts
;display without changing DDRAM contents.
LCD_CURSORSHIFT 	equ		0x10
;********************************
LCD_DISPLAYMOVE 	equ		0x08
LCD_CURSORMOVE 		equ		0x00
LCD_MOVERIGHT 		equ		0x04
LCD_MOVELEFT 		equ		0x00

;// flags for function set
;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Function set 				0 	0 	0 	0 	1 	DL	 N 	F	— 	— 	Sets interface data length 37 μs
;(DL), number of display lines
;(N), and character font (F).
LCD_FUNCTIONSET 	equ		0x20
;*******************************
LCD_8BITMODE 		equ		0x10
LCD_4BITMODE 		equ		0x00
LCD_2LINE 			equ		0x08
LCD_1LINE 			equ		0x00
LCD_5x10DOTS 		equ		0x04
LCD_5x8DOTS 		equ		0x00

;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Set CGRAM address 			0 	0 	0 	1 	ACG ACG ACG ACG ACG ACG Sets CGRAM address.
;CGRAM data is sent and received after this setting.
LCD_SETCGRAMADDR 	equ		0x40

;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Set DDRAM address 			0 	0 	1 	ADD ADD ADD ADD ADD ADD ADD Sets DDRAM address.
;DDRAM data is sent and received after this setting.
LCD_SETDDRAMADDR	equ		0x80

;							RS	R_W	DB7	DB6	DB5	DB4	DB3	DB2	DB1	DB0
;Read busy flag & address 	0 	1 	BF 	AC 	AC 	AC 	AC 	AC 	AC 	AC
;Reads busy flag (BF) indicating internal operation is being performed and 
;reads address counter contents.

;HD44780 pin assignments
LCD_RS  =1<<0;
LCD_RW  =1<<1;
LCD_E   =1<<2;

;Time constants @ 4MHz
D_50ms  = $86A0
D_20ms  = $9C40
D_15ms  = $7530
D_5ms   = $2710
D_1.5ms = $0BBB
D_1ms   = $07DD
D_500us = $03E8
D_100us = $00C8
;class LiquidCrystal : public Print {
;public:
;
;  LiquidCrystal(uint8_t rs, uint8_t enable,	uint8_t d0, uint8_t d1, uint8_t d2, uint8_t d3,
;			uint8_t d4, uint8_t d5, uint8_t d6, uint8_t d7);
;	LiquidCrystal(uint8_t rs, uint8_t rw, uint8_t enable,
;			uint8_t d0, uint8_t d1, uint8_t d2, uint8_t d3,
;			uint8_t d4, uint8_t d5, uint8_t d6, uint8_t d7);
;	LiquidCrystal(uint8_t rs, uint8_t rw, uint8_t enable,
;			uint8_t d0, uint8_t d1, uint8_t d2, uint8_t d3);
;	LiquidCrystal(uint8_t rs, uint8_t enable,
;			uint8_t d0, uint8_t d1, uint8_t d2, uint8_t d3);
;
;	void init(uint8_t fourbitmode, uint8_t rs, uint8_t rw, uint8_t enable,
;			uint8_t d0, uint8_t d1, uint8_t d2, uint8_t d3,
;			uint8_t d4, uint8_t d5, uint8_t d6, uint8_t d7);
;		
;	void begin(uint8_t cols, uint8_t rows, uint8_t charsize = LCD_5x8DOTS);
;
;	void clear();
 ; void home();
;;
;  void noDisplay();
;  void display();
;  void noBlink();
;  void blink();
;  void noCursor();
;  void cursor();
;  void scrollDisplayLeft();
;  void scrollDisplayRight();
;  void leftToRight();
;  void rightToLeft();
;  void autoscroll();
;  void noAutoscroll();

;  void setRowOffsets(int row1, int row2, int row3, int row4);
;  void createChar(uint8_t, uint8_t[]);
;  void setCursor(uint8_t, uint8_t); 
;  virtual size_t write(uint8_t);
;  void command(uint8_t);
  
;  using Print::write;
;private:
;  void send(uint8_t, uint8_t);
;  void write4bits(uint8_t);
;  void write8bits(uint8_t);
;  void pulseEnable();

;  uint8_t _rs_pin; // LOW: command.  HIGH: character.
;  uint8_t _rw_pin; // LOW: write to LCD.  HIGH: read from LCD.
;  uint8_t _enable_pin; // activated by a HIGH pulse.
;  uint8_t _data_pins[8];

;  uint8_t _displayfunction;
;  uint8_t _displaycontrol;
;  uint8_t _displaymode;

;  uint8_t _initialized;

;  uint8_t _numlines;
;  uint8_t _row_offsets[4];
;

