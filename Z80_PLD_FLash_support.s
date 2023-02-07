		include "Z80_Params_.inc"

	ifndef ONESECTION
		section Functions
	else
		section singleAssembly
	endif


        GLOBAL  Flash_WR_Test,Flash_SE_Erase

;********************************************************************************************
;********************************************************************************************	
Flash_WR_Test:

		ld	A,$00
		out (_Z80_BankCS),A			;// clear '64K_SRAM' signal

		call	WR_sequence
		ld 	A,$77
		ld	hl,$2010
		ld	(hl),A
		call	WR_toggle

		call	WR_sequence
		ld 	A,$A0
		ld	hl,$2012
		ld	(hl),A
		call	WR_toggle

		ld	A,$80
		out (_Z80_BankCS),A			;// set '64K_SRAM' signal

		ret
;********************************************************************************************
;********************************************************************************************	
Flash_SE_Erase:
		; ***	erase the sector that contain the address of HL
		ld	A,$00
		out (_Z80_BankCS),A			;// clear '64K_SRAM' signal

		ld 	A,$AA
		ld	($5555),A
		ld 	A,$55
		ld	($2AAA),A
		ld 	A,$80
		ld	($5555),A
		ld 	A,$AA
		ld	($5555),A
		ld 	A,$55
		ld	($2AAA),A
		ld 	A,$30
		ld	(HL),A
		call	WR_toggle	; indicate end of sector erase cycle.

		ld	A,$80
		out (_Z80_BankCS),A			;// set '64K_SRAM' signal
		ret


WR_sequence:
		ld 	A,$AA
		ld	($5555),A
		ld 	A,$55
		ld	($2AAA),A
		ld 	A,$A0
		ld	($5555),a
		ret

WR_toggle:
		ld 	A,(HL)
		
new_toggle:
		ld	B,(HL)
		xor	B			; A = A xor B
		and $40			; keep bit 6
		ret	z			; return if toggl finished
		ld 	A,B			; move to A	
		jr	new_toggle

.end
