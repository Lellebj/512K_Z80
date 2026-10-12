

		INCLUDE "Z80_Params_.inc"


;*************************************************
		section 	LAB_RGBLED

;****************************************************************
		call	CRLF
		call 	writeSTRBelow
		defb   	"\r\n"
		defb	" Testing RGB-LED at PIO A\r\n"
		defb	" Testing: git @@GIT_VERSION@@: @@DATE@@\r\n"
		defb	"\0"
		call 	waitForFinishedPrintout

        		;------------INIT CTC (2 sec timing for 'C'/NAK process----------------
		;init CH 0 and 1
		ld 	 	A,_Counter|_Rising|_TC_Follow|_Reset|_CW
		out		(CH3),A 		; CH0 is on hold now
		ld		A,$10			; time constant (prescaler; $42;$DA; 14390,625 khz -> 2, sec peroid;  
		out		(CH3),A			; and loaded into channel 0
		
		; ld		A,_INT_EN|_Counter|_Rising|_TC_Follow|_Reset|_CW	
		; out		(CH1),A			; CH1 counter
		; ld		A,$DA			; time constant 
		; out		(CH1),A			; and loaded into channel 2

Testagain:
        	; Wait for CTC to finish counting
            
        in      A,(CH3 )    ; read current value of downcounter
        cp      $00         ; compare with zero (assuming downcounter is at zero when finished)
        jr      Z,CTC_Done   ; if zero, jump to CTC_Done
        jr     Testagain
 CTC_Done:        
        ; CTC has finished counting, proceed with the rest of the code
        ld      A, (TempVar8)
        inc    A
        ld     (TempVar8),a
        out     (portA_Data),A
        jp      Testagain
