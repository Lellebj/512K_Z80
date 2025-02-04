		include "Z80_Params_.inc"

	ifndef ONESECTION
		section Functions
	else
		section singleAssembly
	endif


        GLOBAL  Flash_WR_Test,Flash_SE_Erase
		GLOBAL 	enableFLASH,enableIC620_OE,setFLASHBank,setSRAMBank,disableFLASH,disableIC620_OE

;********************************************************************************************
;********************************************************************************************	
Flash_WR_Test:

		; ***	size in commLvl1
		; ***	memory address in commAdr1
		; ***	flash address in commAdr2
		; ***	erase sectors



		; erase present flash section
		ld 		BC,(commLvl1)		; size of block

		ld 		HL,(commAdr2)		; first adress in flash sector
		ld 		DE,(commAdr1)		; first adress in memory
nextSector:
		ld 		A,H
		and 	A,$F0 				; select high nibble
		ld 		(TempVar4),A		; save flash adr (highest nibble)
		call	Flash_SE_Erase
wrOneByte:
		; ***	Write one byte
		call	WR_sequence
		ld 		A,(DE)				; get byte from memory
		ld		(HL),A				; set FLASH byte/address to be programmed
		call	WR_toggle			; wait for toggle
		inc 	DE
		inc 	HL
 	; call DumpRegisters

		dec 	BC
		xor  	A 					; A=0
		cp 		A,B
		jr 		NZ,checkSector
		cp 		A,C
		jr 		NZ,checkSector
		jr 		finishWrite
checkSector:
		; ***	compare highest nibblbe to detect sector change
		push 	DE
			ld 		A,H
			and 	A,$F0 				; select high nibble
			ld 		D,A 		 		; high byte of stored flash address
			ld 		A,(TempVar4)
			xor 	A,D					; check if same sector
		pop 	DE
		jr 		NZ,nextSector
		jr 		wrOneByte



		; call	enableFLASH 	;// clear '64K_SRAM' signal


		; call	WR_sequence
		; ld 	A,$A0
		; ld	hl,$2012
		; ld	(hl),A
		; call	WR_toggle

		; ld	A,$80
		; out (_Z80_BankCS),A			;// set '64K_SRAM' signal
finishWrite:
		ret
;********************************************************************************************
;********************************************************************************************	
		; ***	erase the sector that contain the address of HL
Flash_SE_Erase:

		ld 		A,$AA
		ld		($5555),A
		ld 		A,$55
		ld		($2AAA),A
		ld 		A,$80
		ld		($5555),A
		ld 		A,$AA
		ld		($5555),A
		ld 		A,$55
		ld		($2AAA),A
		ld 		A,$30
		ld		(HL),A
		call	WR_toggle	; indicate end of sector erase cycle.

		; ld	A,$80
		; out (_Z80_BankCS),A			;// set '64K_SRAM' signal
		ret


WR_sequence:
		ld 		A,$AA
		ld		($5555),A
		ld 		A,$55
		ld		($2AAA),A
		ld 		A,$A0
		ld		($5555),a
		ret

WR_toggle:

		ld 		A,(HL)
		push 	BC
new_toggle:
			ld		B,(HL)
			xor		A,B			; A = A xor B  
			and 	A,$40		; keep bit 6 final result 0 or not
			ld 		A,B			; move (HL) to A
			jr		NZ,new_toggle
		pop 	BC		
		ret					; return if toggl finished


;********************************************************************************************
;********************************************************************************************	


disableIC620_OE:
		; ***	Set IC620 pin 1 high
		ld A,0
		out (_CE_RST_BANK),A			;IC620 (HC374) goes to high impedance.. all signals = GND
		; ld 	A,$00					; FLASH memory is lower 32k and SRAM upper 32k
		; out (_Z80_BankCS),A			; set bank register number 0 and 64K_SRAM=0	
		; ld 	A,$01
		; out (_CE_RST_BANK),A		; set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH
		ret


;********************************************************************************************
;********************************************************************************************	

enableIC620_OE: 
		; ***	Set IC620 pin 1 low
		ld A,1
		out (_CE_RST_BANK),A			;IC620 (HC374) goes to high impedance.. all signals = GND
		; ld 	A,$00					; FLASH memory is lower 32k and SRAM upper 32k
		; out (_Z80_BankCS),A			; set bank register number 0 and 64K_SRAM=0	
		; ld 	A,$01
		; out (_CE_RST_BANK),A		; set bank register (HC374) #0 | Bit 7 set 0 -> 32kSRAM/32kFLASH
		ret


;********************************************************************************************
;********************************************************************************************	



.end
