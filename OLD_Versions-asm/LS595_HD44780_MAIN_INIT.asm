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
 ; PIO_HD44780_main_init
#target bin


;IORQ595_SRCLR10:        equ $70
IORQ595_SRCK11:         equ $C0
IORQ_LS74_E3:           equ $E0



#code HD44780_INIT, $3000, *            ; EEPROM#2 area        

        ;ld a, %00001111   ;mode 0 out
        ;out (portB_Contr), a         ; set port A as output
        ;ld a, $0   ;0 out
        ;out (portB_Data), a         ; Port A = 0

      
        ;//put the LCD into 4 bit or 8 bit mode
        ;// this is according to the hitachi HD44780 datasheet
        ;// figure 24, pg 46
        ;// we start in 8bit mode, try to set 4 bit mode
        push ix
        push iy

init:   ld a,$03
        call write4bits
        ld b,$8F	; ~25cycles/3µs/loop@8MHz-> $FF loops
wait1_init:
		nop
		nop
                LD ix,$1234
                LD ix,$5678
		djnz wait1_init
 
        ld a,$03
        call write4bits
         ld b,$40	; ~25cycles/3µs/loop@8MHz-> $40 loops
wait2_init:
		nop
		nop
                LD ix,$5678
		djnz wait2_init
		
	ld a,$03
        call write4bits
         ld b,$40	; ~25cycles/3µs/loop@2MHz-> $40 loops
wait3_init:
		nop
                LD iy,$5678
		nop
		djnz wait3_init


        ld a,$02
        call write4bits ; // finally, set to 4-bit interface

        ;// finally, set # lines, font size, etc.
        ld a,(_displayfunction) ; = $08
        or $20                  ;Resulting 28
        call command

 
        ;// turn the display on with no cursor or blinking default
        ;_displaycontrol = 0x04  ;//LCD_DISPLAYON(04) | LCD_CURSOROFF (02) | LCD_BLINKOFF (01);  

        ld a,(_displaycontrol)  ; = $04
        or $08                  ;Resulting $0C
        call command

        ld a,$01
        call command     ;//LCD_CLEARDISPLAY);  // clear display, set cursor position to zero
        ld b, $FF 		; 14µs/loop->  	$FF loop (8E)		
wait_cl:
        nop
        LD iy,$5678
        nop
        djnz wait_cl    ;// wait on clear

        ld a,(_displaymode)     ; =$02
        or $04          ; Resulting $06
        call command

        pop iy
        pop ix
        ret
        ;// Initialize to default text direction (for romance languages)
        ;_displaymode = 0x02     ;//LCD_ENTRYLEFT | LCD_ENTRYSHIFTDECREMENT;
        ;// set the entry mode
;************************************************************************


_displayfunction:       defb $08
_displaycontrol:        defb $04
_displaymode:           defb $02
_rowoffset:             defb 0,0x40,20,0x54


;********************************************************************
        align 16
setcursor:
						; row in D, col in E
		push ix
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
		add a,d				; a is adress
		set 7,a				; LCD_SETDDRAMADDR  command
		pop ix
		; continue with 'command'

	align 16
command:             ; a contains value
        ld d,0       ; indicate rs pin =0 (pin 5)
        call send
        ret

;****************************************************************
        align 16
write:  ld d,$10                ; a contains value
                        ; indicate rs pin =1 (pin 5 set)
        call send
        ret


;****************************************************************
send:                   ; a-value; d-mode 1(data), 0(command)
        ld c,a          ;copy a to d
        srl a
        srl a
        srl a
        srl a
        or d            ; add eventually pin 5 (RS)
        call write4bits
        ld a,c
        and $0f
        or d
        call write4bits
        ret

;****************************************************************


;****************************************************************
write4bits:   ; A contains value
        push BC
        
        ;out (portB_Data), a 
        call sendTo595  ;
        call pulseEnable
        pop BC
        ret

;****************************************************************
res_595_LS74bits:   ; A contains value

        push af
        ld a,$AB
        ;out (IORQ595_SRCLR10),A
        ;res 0,A
        ;out (IORQ595_SRCLR10),A
        ;set 0,A
        ;out (IORQ595_SRCLR10),A
        ;pop af
        ret

;****************************************************************
pulseEnable:     ; pulse the enable pin... (pin 4)
                ; a contains data...
        push af        
        push bc 
        push hl
        ld c,IORQ_LS74_E3
        res 0,a       
        out (C),A   ; set pin 4 (enable) LOW
        set 0,a
        out (C),A   ; set pin 4 (enable) HIGH    
        nop
        res 0,a
        out (C),A   ; set pin 4 (enable) LOW
        ld b, $80       ; 20 cyc, 10 µs@2Mhz-> (16X-> 150-160µs)
wait_pe:
        nop
        ld hl,$1234
        ld hl,$1234
        ld hl,$5678
        djnz wait_pe

        pop hl
        pop BC
        pop af
        ret

;****************************************************************
        align 8

sendTo595:              ;// send contents of A to 74LS595 
        push AF
        push BC
        ld b, $6        ; send five bits (D0-D3 + RS); one more than act bits...  595 phenomenon
        scf
        ccf                         ; resets the carry flag

next:   out (IORQ595_SRCK11), a     ;only D0 get stored in 595 in
        sra c                         
        rra                         ;lsb of c in to CY then msb in a...   
        
        djnz  next
        ;res 1,a
        ;out (IORQ595_RCK12),A         ; latch OE on 595, pin12 
        ;nop
        ;nop 
        ;set 0,a
        ;out (IORQ595_RCK12),A        ; latch OE on 595, pin12 
        ;set 1,a
        ;out (IORQ595_G13),A          ; Open 3-state output
            
        
        pop BC
        pop AF
        ret
;****************************************************************


.end



