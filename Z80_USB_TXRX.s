;


		INCLUDE "Z80_Params_.inc"


;*************************************************
		section 	Functions

;****************************************************************
USB_TEXT_LABLES		equ 1		; No additional text when using USB.
								; 1-low level of info; 15-high level of info

USB_INT_SUCCESS		equ $14
USB_INT_CONNECT 	equ $15
USB_INT_DISCONNECT 	equ $16
USB_INT_BUF_OVER 	equ $17
USB_INT_USB_READY 	equ $18
USB_INT_DISK_READ 	equ $1D	
USB_INT_DISK_WRITE 	equ $1E
USB_INT_DISK_ERR 	equ $1F	
CMD_RET_SUCCESS 	equ $51
CMD_RET_ABORT	 	equ $5F
ERR_OPEN_DIR		equ $41
ERR_MISS_FILE 		equ $42
ERR_FOUND_NAME		equ $43
ERR_DISK_DISCON		equ $82
ERR_LARGE_SECTOR	equ $84
ERR_TYPE_ERROR		equ $92
ERR_BPB_EROR		equ $A1 
ERR_DISK_FULL		equ $B1 
ERR_FDT_OVER		equ $B2
ERR_FILE_CLOSE		equ $B4
CTC_TIMEOUT 		equ $EE

	
		xref 	delay_D0_ms,PrintD0ToScreen,SetHC376Timer
		GLOBAL	getResponseFromUSB,HC376S_CheckConnection,HC376S_ResetAll,HC376S_setUSBMode,HC376S_diskConnectionStatus
		GLOBAL 	HC376S_USBdiskMount,HC376S_setFileName,HC376S_fileOpen,HC376S_fileClose,HC376S_fileCreate
		GLOBAL 	HC376S_getFileSize,HC376S_fileRead,HC376S_fileDelete,HC376S_fileWrite,HC376S_setSDMode
		GLOBAL  delay100ms,CTC_delay_INT_handler,ReadUSBHandler

HC376S_CheckConnection::

		ld 		DE,CTC_delay_INT_handler
		ld 		(CTC_CH1_I_Vector),DE

		call 	beginUART

		ld 		E,$06
		call 	outByte367S

		ld	 	E,$81					;(hspace+8)			; Testvalue $55 response $AA
		call 	outByte367S

		call 	delay10ms   				; start timout counter 1 ms

		call 	waitForResponse 		; Z is set if no response from 376S, response in E
		jp 		Z,endtest				; branch on timeout
		; ; call 	getResponseFromUSB		; get the actual data, in D0
		; ; response in E

		ld 		B,$81
		cpl 
		cp 		B 						; compare complement response with B 
		jr 		Z,connection_pass

connection_fail:

		call 	writeSTRBelow_CRLF
		DB 		0,">Connection to CH376S - FAILED.", 00


		xor 	A
		inc 	A
		ret		; NZ

connection_pass:
	if (USB_TEXT_LABLES>1)
		call 	writeSTRBelow_CRLF
		DB 		0,">Connection to CH376S was successful.", 00
	endif
		xor 	A
		ret		; Z 
  
;****************************************************************
	if DOALIGN
		align 4
	endif


HC376S_ResetAll::
		ld 		DE,CTC_delay_INT_handler
		ld 		(CTC_CH1_I_Vector),DE

		call	beginUART		

		ld 		E,$05
		call 	outByte367S
		
		call	delay200ms    			; 200 msec
		halt	

		ret

; **###############################################################

; ****************************************************************
	if DOALIGN
		align 4
	endif

HC376S_setUSBMode::

		call 	beginUART

		ld 		E,$15
		call 	outByte367S
		
		ld 		E,$06				; Code used to enable read/write communication and monitoring of the USB stick
		call 	outByte367S
		
		call 	delay20ms			;delay 20 ms

		call	waitForResponse	 		;test rxrdy-B
		jr 		Z,NoUSBpres			; no response from 'waitForResponse'
		
		cp	 	CMD_RET_SUCCESS
		jp 		NZ,someUSBerror

		call 	delay10ms
		call 	waitForResponse				; read data in inport -> A&E
	
	if (USB_TEXT_LABLES>2)
		call 	writeSTRBelow_CRLF
		db		0,">USB Mode command acknowledged !",0,0
	endif
		ld 		A,E	
		cp	 	USB_INT_CONNECT				; compare A & USB_INT_CONNECT
		jr		NZ,NoUSBpres

	if (USB_TEXT_LABLES>3)
		call 	writeSTRBelow_CRLF
		db		0,">USB is present.",0,0
	endif	
		xor 	A
		ret									; return with Z


NoUSBpres:
		call 	writeSTRBelow_CRLF
		db	0,">No USB is present.",0,0
		jr 		retNZ
NoSDpres:
		call 	writeSTRBelow_CRLF
		db	0,">No SD card is present.",0,0
		jr 		retNZ
		
someUSBerror:		
		call 	writeSTRBelow_CRLF
		db		0,">CH376S error! .",0,0
retNZ:
		xor 	A
		inc 	A
		ret		; NZ

;**###############################################################
;**################################################################
		
;****************************************************************
	if DOALIGN
		align 4
	endif

HC376S_setSDMode::

		call 	beginUART

		ld 		E,$15
		call 	outByte367S
		
		ld 		E,$03				; Code used to enable read/write communication and monitoring of the SD card
		call 	outByte367S
		
		call 	delay100ms			;delay 20 ms

		call	waitForResponse	 		;test rxrdy-B
		jr 		Z,norespSD			; no response from 'waitForResponse'

		; cp	 	CMD_RET_SUCCESS
		; jr 		NZ,someUSBerror

		; call 	delay100ms
		; call 	waitForResponse				; read data in inport -> A&E
	
	; if (USB_TEXT_LABLES>4)
	; 	call 	writeSTRBelow_CRLF
	; 	db		0,"SD Mode command acknowledged !",0,0
	; endif
		ld 		A,E	
		ld 		D,00
		; cp	 	USB_INT_CONNECT				; compare A & USB_INT_CONNECT
		; jp		NZ,NoSDpres

	if (USB_TEXT_LABLES>5)
		call 	writeSTRBelow
		db		0,"SD card response OK.  Code:",0,0
		call 	putDEtoScreen
		call	CRLF
	endif
		xor 	A
		ret									; return with Z

norespSD:
		call 	writeSTRBelow_CRLF
		db		" SD card no response",0,0
		ret



;**###############################################################
;**################################################################
	
	if DOALIGN
		align 4
	endif

HC376S_diskConnectionStatus::
		; ***	Does not apply to SD card's
		call 	beginUART

		ld 		E,$30
		call 	outByte367S

		call 	delay100ms
		call 	waitForResponse 			; Z is set if no response from 376S 
		jp 		Z,endtest					; branch on timeout
											; if not : get the actual data, in A&E
		cp 		A,USB_INT_SUCCESS
		jr 		NZ,.connFailed

	if (USB_TEXT_LABLES>6)
		call 	writeSTRBelow_CRLF
		db		0,">Connection to USB OK.",0,0
	endif

		ret
	
.connFailed:
		call 	writeSTRBelow_CRLF
		db		0,">Connection to USB - FAILED.",0,0
		ret

;************************************************************************
;************************************************************************

;**######################################################################
;**######################################################################

	if DOALIGN
		align 4
	endif


HC376S_USBdiskMount::

		call 	beginUART
		
		ld 		E,$31
		call 	outByte367S
		
		call 	delay1s				; 250 msec
		call 	waitForResponse 		; Z is set if no response from 376S 
		jp 		Z,endtest				; branch on timeout
										; get the actual data, in A&E
		ld 		(TempVar5),A
		cp 		USB_INT_SUCCESS
		jr 		NZ,.connFailed
	if (USB_TEXT_LABLES>7)
		call 	writeSTRBelow_CRLF
		db		0,"> Mounted - OK.",0,0
	endif
		xor 	A					; A = 0  Z set
		ret
	
.connFailed:
		ld 		E,A
		ld 		D,00
		call 	writeSTRBelow
		db		0,">Failed to Mount disk.  Code:",0,0
		call 	putDEtoScreen
		call	CRLF
		ld 		A,$54					; indicate mount failure A-non zero
		inc 	A
		ret

;************************************************************************
;************************************************************************

;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif

; rfile_name:
	; db "TESTAS.TXT",0,0
	; db "PROVIDE.txt",0,0
	; db "TOTBIN1.TXT",0,0
	even
HC376S_setFileName::

		call 	beginUART
	
		ld 		E,$2F			; char '/'
		call 	outByte367S
		call 	outByte367S        ;// Every filename must have this byte '/'to indicate the start of the file name.

		push 	HL	
		ld 		HL,commStr1

.nxtchar:	
		ld 		E,(HL)
		inc 	HL
		call 	outByte367S
		or 		A  					; test if A=0  'end of string'
		jr 		NZ,.nxtchar

		pop 	HL

		call 	delay50ms    			; 50 msec
		halt

		ret 	

;************************************************************************
;************************************************************************

;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif


HC376S_fileOpen::

	
		call 	writeSTRBelow
		db		">File open : ",0,0

		ld 		IY,commStr1				;move.l 	USB_filename_ptr,A0 
		dec 	IY
		call 	WriteLineCRNL

		call 	beginUART
	
		ld 		E,$32	
		call 	outByte367S
		call 	delay1s				; 250 msec
		call 	waitForResponse 		; Z is set if no response from 376S , data in A&E
		jp		Z,endtest				; branch on timeout

nxtFileOpen:

		cp		USB_INT_SUCCESS			; compare A with USB_INT_SUCCESS ($14)
		jr 		Z,.openOK

		cp 		USB_INT_DISK_READ		; compare A with USB_INT_DISK_READ ($1D) - enumeration
		jr 		Z,doEnumeration

		cp		ERR_MISS_FILE			; compare A with ERR_MISS_FILE		
		jr 		Z,openNoFileName

		call 	writeSTRBelow_CRLF
		db		0,">Failed to open file.",0,0
		ld 		A,$65					; indicate mount failure A-non zero
		inc 	A
		ret

	
.openOK:
		call 	writeSTRBelow
		db		0," opened successfully, ",0,0
		xor 	A						; A= 0 , Z set
		ret

openNoFileName:
		call 	writeSTRBelow_CRLF
		db		0,">File not found.!",0,0
		ld 		A,$76					; indicate mount failure A-non zero 
		inc 	A						; ret with NZ
		ret

doEnumeration:
		ld 		IY,S1x
		; *** 	CMD_RD_USB_DATA0
		call 	beginUART
	
		ld 		E,$27					; CMD_RD_USB_DATA0			
		call 	outByte367S				; CMD_RD_USB_DATA0

		call 	delay2ms
		call 	waitForResponse 		; Z is set if no response from 376S 
		ld 		(IY),E					;read char		 store in adressblock (HL)
		inc 	IY
	
		ld 		A,E				; get the actual (first byte) data -> length, in D0
		ld 		B,E 			; loop counter
.loop:
		call 	delay10ms
		call 	waitForResponse 		; Z is set if no response from 376S 

		ld 		(IY),E					;read char		 store in adressblock (HL)
		inc 	IY
		djnz 	.loop

		call 	beginUART
	
		ld 		E,$33					; CMD_FILE_ENUM_GO	
		call 	outByte367S				; CMD_RD_USB_DATA0
			
		call 	delay250ms
		call 	waitForResponse 		; Z is set if no response from 376S 
		push 	AF

		ld 		IY,S1x 				; start of file name text
		ld 		B,(IY+$0C)					; UINT8	DIR_Attr;	dir(10) or file(20)
		ld 		A,00
		ld 		(IY+$0C),A					; string eof

		call 	WriteLine
		call    writeSTRBelow
		db 		0,"\t",0,0
		ld 		A,B
		cp 		$10 					; directory ?
		jr  	Z,.dDir
		call 	writeSTRBelow
		db 		0,"\t\t",0,0
		jr 		.common

.dDir:	call 	writeSTRBelow
		db 		0,"Dir\t",0,0

.common:
		ld 		D,(IY+$12) 				; get size in DE
		ld 		E,(IY+$11) 				; get size in DE
		call 	putDEtoScreen
		call 	CRLF

		pop 	AF						; retrieve last msg from HC376
		cp 		ERR_MISS_FILE
		JP 		NZ,nxtFileOpen
		call 	CRLF
		inc		A						; reset Z -> NZ 		
		ret 							; return with NZ


;**###############################################################
;**################################################################


; /* FAT数据区中文件目录信息 */
; typedef struct _FAT_DIR_INFO
; {
;    UINT8	DIR_Name[11];					/* 00H,文件名,共11字节,不足处填空格 */
;    UINT8	DIR_Attr;						/* 0BH,文件属性,参考后面的说明 */
;    UINT8	DIR_NTRes;						/* 0CH */
;    UINT8	DIR_CrtTimeTenth;				/* 0DH,文件创建的时间,以0.1秒单位计数 */
;    UINT16	DIR_CrtTime;					/* 0EH,文件创建的时间 */
;    UINT16	DIR_CrtDate;					/* 10H,文件创建的日期 */
;    UINT16	DIR_LstAccDate;					/* 12H,最近一次存取操作的日期 */
;    UINT16	DIR_FstClusHI;					/* 14H */
;    UINT16	DIR_WrtTime;					/* 16H,文件修改时间,参考前面的宏MAKE_FILE_TIME */
;    UINT16	DIR_WrtDate;					/* 18H,文件修改日期,参考前面的宏MAKE_FILE_DATE */
;    UINT16	DIR_FstClusLO;					/* 1AH */
;    UINT32	DIR_FileSize;					/* 1CH,文件长度 */
; } FAT_DIR_INFO, *P_FAT_DIR_INFO;			/* 20H */

;************************************************************************
;************************************************************************

;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif


HC376S_fileClose::

	if (USB_TEXT_LABLES>8)
		call 	writeSTRBelow
		db		0,">File close : ",0,0
	endif 

		call 	beginUART
	
		ld 		E,$36
		call 	outByte367S

		ld 		E,01      		;closeCmd = 0x00 = close without updating file Size, 0x01 = close and update file Size
		call 	outByte367S

		call 	delay100ms				; delay max 100 msec
		call 	waitForResponse 		; Z is set if no response from 376S 
		jp 		Z,endtest				; branch on timeout

		ld 		A,E						; get the actual data, in A
		cp		USB_INT_SUCCESS
		jr 		NZ,.closeFailed
	
	if (USB_TEXT_LABLES>9)
		call 	writeSTRBelow_CRLF
		db		" closed successfully..",0,0
	endif	
		ret
	
.closeFailed:

		; call 	PrintD0ToScreenHEX
		call 	writeSTRBelow_CRLF
		db		" failed to close file.",0,0
		ret

;************************************************************************
;************************************************************************



;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif

		; ***	Create file; return Z if true.
HC376S_fileCreate::

		call 	writeSTRBelow
		db		" >Create File : ",0,0
		ld 		IY,commStr1
		dec 	IY
		call 	WriteLineCRNL

		call 	beginUART
	
		ld 		E,$34					; Create File
		call 	outByte367S

		call 	delay500ms				; delay max 100 msec
		call 	waitForResponse 		; Z is set if no response from 376S 
		jp 		Z,endtest				; branch on timeout

				;  the actual data is in A&E
		cp		USB_INT_SUCCESS
		jr 		NZ,.createFailed
	
	if (USB_TEXT_LABLES>10)
		call 	writeSTRBelow_CRLF
		db		" >File created successfully..",0,0
	endif	
		xor 	A				; Z set
		ret						; Z set, setBytesRead return true

	
.createFailed:

		ld 		E,A
		ld 		D,00
		call 	writeSTRBelow
		db		" >Failed to create file..  Code:",0,0
		call 	putDEtoScreen
		call	CRLF
		ld 		A,$87					; indicate mount failure A-non zero
		inc 	A 						; indicate NZ
		ret

;************************************************************************
;************************************************************************


;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif


HC376S_getFileSize::

		call 	beginUART
	
		ld 		E,$0C	
		call 	outByte367S
		ld 		E,$68
		call 	outByte367S

		call 	delay1s				; delay max 200 ms

		call 	waitForResponse 		; Z is set if response from 376S 
		jr 		Z,endtest				; branch on timeout


		ld 		B,3
		ld 		HL,T_BUFFER  			; $D8
		ld 		(HL),A
		inc 	HL

.siz4:
		call 	delay10ms 				; delay max 10 ms
		call 	waitForResponse			; get the actual data, in D0
		jr 		Z,endtest				; branch on timeout

		ld 		(HL),A
		inc 	HL
		djnz 	.siz4

.finalsize:
		call 	writeSTRBelow
		db		" file size : ",0,0
		ld 		DE,(T_BUFFER)			; filesize restrict to max 65535 bytes; only two least bytes
		call 	putDEtoScreen
		call	CRLF
		
		ret

;************************************************************************
;************************************************************************

endtest:
		call 	writeSTRBelow_CRLF
		DB 		0,">Connection to CH376S - TIMEOUT.", 00

		ret

;**######################################################################
;**######################################################################

		; *** IN D;Z set, setBytesRead return true; return false (NZ)
setBytesRead::
		;***		Value in D
		call 	beginUART
	
		ld 		E,$3A					; Byte Read	
		call 	outByte367S
	
		ld 		E,D						; will be $80 bytes /block
		call 	outByte367S

		ld 		E,0						; 2'nd value = 0
		call 	outByte367S

		call 	delay100ms
		call 	waitForResponse 		; Z is set if no response from 376S 

		ld 		A,E						; get the actual data, E->A
		cp		USB_INT_DISK_READ		; read the CH376S message. 

										; If equal to 0x1D, data is present, so return true. Will return 0x14 if no data is present.
		ret 							; ret true ($1D) or false ($14)								

;************************************************************************
;************************************************************************


;**######################################################################
;**######################################################################

		; Z set, continueRead return true, ; Z cleared -> continueRead return false(NZ)
continueRead:
		call 	beginUART
	
		ld 		E,$3B					; Byte Read	
		call 	outByte367S

		call 	delay100ms
		call 	waitForResponse 		; Z is set if no response from 376S 

		ld 		A,E						; get the actual data, in A&E
		cp		USB_INT_SUCCESS			; read the CH376S message. 
										; If equal to 0x1D, data is present, so return true. Will return 0x14 if no data is present.
		ret 							; ret true Z ($14), other false NZ							

;************************************************************************
;************************************************************************


;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif

		;***		usbrd  "TOTBIN1.TXT"  $140000
HC376S_fileRead::

		push  	HL
		ld 		HL,(commAdr1)			; set the target address
		
		call 	writeSTRBelow_CRLF
		db		" Reading File !. ",0,0

.nextblock:		
		ld 		D,$80 	 		; The maximum value is 0x80  =  128 bytes
		call 	setBytesRead	; This tells the CH376S module how many bytes to read on the next reading step.
								; In this example, we will read 0x80 bytes at a time. 
								; Returns true (Z)if there are bytes to read, false (NZ)if there are no more bytes to read.
		jr 		NZ,endBlockRead

		call 	beginUART
	
		ld 		E,$27					; CMD_RD_USB_DATA0			
		call 	outByte367S				; CMD_RD_USB_DATA0

		call 	delay2ms
		call 	waitForResponse 		; Z is set if no response from 376S 
	
		ld 		A,E				; get the actual (first byte) data -> length, in D0
		ld 		B,E 			; loop counter
.loop:
		call 	delay2ms
		call 	waitForResponse 		; Z is set if no response from 376S 

		ld 		(HL),E					;read char		 store in adressblock (HL)
		inc 	HL
		djnz 	.loop

.noaction:
		call 	continueRead		; prepares the module for further reading. If false, stop reading.
									; You need the continueRead() method if the data to be read from the USB device is greater than numBytes.
		jr 		Z,.nextblock 

endBlockRead: 	 				; setBytesRead returned false or continueRead returned false

	if (USB_TEXT_LABLES>11)
		call 	writeSTRBelow_CRLF
		db		" No more DATA !.",0,0
	endif
		pop 	HL
		ret

;************************************************************************
;************************************************************************


;**######################################################################
;**######################################################################

		; Z set, continueRead return true; NZ-return false(no more data)
setByteWrite:
		;***		Value numBytes in D
		call 	beginUART
	
		ld 		E,$3C					; Byte Read	
		call 	outByte367S

		ld 		E,D
		call 	outByte367S

		ld 		E,00
		call 	outByte367S

		call 	delay1s
		call 	waitForResponse 		; Z is set if no response from 376S 
		; jr		Z,.retfalse				; branch on timeout

				;  the actual data is in A&E
		cp 		USB_INT_DISK_WRITE		; check the disk write status CH376S message. 
										; If equal to 0x1D, data is present, so return true. Will return 0x14 if no data is present.
		ret 	Z 						;			; Z set, continueRead return true; NZ-return false(no more data)


; .retfalse:
; 		ld 		A,$98
; 		inc 	A						; Z cleared -> continueRead return  NZ  return false ($14)	
; 		ret

;************************************************************************
;************************************************************************



;**######################################################################
;**######################################################################
	if DOALIGN
		align 4
	endif


HC376S_fileWrite::

		ld 		DE,(commLvl1) 				; actual file lenght
		ld 		(charLen),DE 				; save in heap

		ld 		HL,(commAdr1) 				; set the target start address in HL
		ex 		DE,HL						; DE = target start address in HL
		add		HL,DE
		ld 		(commAdr2),HL 				; set the target end address
		ex 		DE,HL					; set the target start address in HL

		call 	writeSTRBelow_CRLF
		db		" Writing to File !. ",0,0
		ld 		DE,$0080
		ld 		(packLen),DE
		push 	DE
		call 	delay500ms
		halt
		pop 	DE
nextblockW:	
		 
		ld 		D,E					; only low byte used (E)
		call 	setByteWrite		; This tells the CH376S module how many bytes to write on the next step.
									; In this example, 0x80 (D1) bytes will be written at a time. 
									; Returns true (Z)if there are bytes to read, false (NZ)if there are no more bytes to read.
		jr 		NZ,endBlockWrite

		call 	beginUART

		ld 		E,$2D			;     //WR_REQ_DATA
		call 	outByte367S				; CMD_WR_USB_DATA0
		
		call 	delay1s
		call 	waitForResponse		; // wait for an acknowledgement from the CH376S module
				;	//WR_REQ_DATA results in byte# -> B
		ld 		B,A
.loopD:
		;***		B contains amount of loops   (packLen), HL points to data
.loop:

		ld 		A,(HL)
		ld 		E,A
		inc 	HL
		call 	outByte367S				;write char to buffer
		djnz 	.loopD

		ld 		(sdWRpointer),HL		; restore file pointer for later

		; call 	delay500ms
		; call 	waitForResponse		; // wait for an acknowledgement from the CH376S module

	; if (USB_TEXT_LABLES>13)
	; 	call 	writeSTRBelow
	; 	db		" Write code (normally FF and 14): ",0,0
	; endif
; 				; response in A&E

					; code from block write(pack length)
		; ld 		D,00
		; call 	putDEtoScreen
		; ld 		A,','
		; call 	WriteChar

		call 	beginUART
	
		ld 		E,$3D				; CMD_BYTE_WR_GO        
		call 	outByte367S		

		call 	delay500ms
		call 	waitForResponse		

		; ld 		E,A 				; code from block write(pack length)
		; ld 		D,00
		; call 	putDEtoScreen
		; call	CRLF

		; ld 		DE,(sdWRpointer)				; code from block write(pack length)
		; call 	putDEtoScreen
		; call	CRLF


		;****	Calculate length of next data chunk
		and 	A						; reset carry
		ld 		HL,(sdWRpointer)		; restore file pointer for later
		; 		commAdr2 - HL - packlen < 0  ? 
	
		ld 		DE,(commAdr2)
		ex 		DE,HL
		sbc 	HL,DE					; HL = commAdr2 - HL
		ld 		DE,(packLen)				; DE = actl block size (packLen)
		sbc 	HL,DE					; HL = commAdr2 - HL - packlen
		jp 		M,lastBlock				; lastblock < pack size ...
		ld 		HL,(sdWRpointer)		; restore file pointer for later

		ld  	a,$AA
		
		; call DumpRegisters
		jp 		nextblockW

lastBlock:
		; ***	
		add 	HL,DE 					; the last (reduced) block size -> DE [HL = commAdr2 - HL - packlen +packLen ]
		push 	HL
		pop 	DE						; the last (reduced) block size -> DE
		ld 		(packLen),DE
		ld 		HL,(sdWRpointer)		; restore file pointer for later

		ld 		A,00
	; call DumpRegisters

		cp 		D
		; 		commAdr2 = HL  ?
		jp 		M,endBlockWrite
		jp	 	NZ,nextblockW			; jump to nextblock if DE not zero
		cp 		E
		jp	 	NZ,nextblockW			; jump to nextblock if DE not zero


; .W_timeout:
; 	if (USB_TEXT_LABLES>14)
; 		call 	immputstring
; 		dc.b	">Timeout on Write File !.",CHAR_LF,CHAR_CR,0,0
; 		even
; 	endif


endBlockWrite: 	 				; setByteWrite returned false or continueRead returned false


	if (USB_TEXT_LABLES>15)
		call 	writeSTRBelow_CRLF
		db		" No more DATA !.",0,0
	endif

		ret

;************************************************************************
;************************************************************************




;**######################################################################
;**######################################################################

			; return true ->Z set;   return false with  NZ
HC376S_fileDelete:

		call 	writeSTRBelow
		db		" >Delete File : ",0,0

		ld 		IY,commStr1				;move.l 	USB_filename_ptr,A0 
		dec 	IY
		call 	WriteLineCRNL


		call 	HC376S_setFileName


		call 	beginUART
	
		ld 		E,$35					; Delete File	
		call 	outByte367S

		call	delay100ms
		call 	waitForResponse 		; Z is set if noresponse from 376S 
		jp 		Z,endtest				; branch on timeout

				; get the actual data, in A&E
		cp		USB_INT_SUCCESS 		; read the CH376S message. 
 										; If equal to 0x1D, data is present, so return true. Will return 0x14 if no data is present.

		ret 	Z					; return true ($14); Z set, continueRead return true

; 		clr.w 	D0				; Z set
; 		rts						; return true; Z cleared -> continueRead return  NZ

		jp 		openNoFileName		; show no file found

;************************************************************************
;************************************************************************

		; return with no response  (Z); return with response -> NZ, result in A&E
getResponseFromUSB:
		in 		A,(sio_bc)
		bit 	0,A 		 			;test rxrdy-B, bit 0
		ret 	Z						; return (no chars available)  (Z)
						; 
		; ***	No timeout - read data.

		in  	A,(sio_bd)		  		;read char from SIO B
		ld 		E,A
noresp:
		ret						; return with response -> NZ
; 
delay_D0_ms:	
; 		***		time in msecs in D0

; 		call 	SetHC376Timer


; 		call 	resetTimer68230
; 		call 	startTimer68230
; .timetest:
; 		call 	testTimeout				; test timer 
; 		beq 	.timetest

; 		rts

   		;wait for a response from the CH376S. If CH376S responds, it will be true. If it times out, it will be false. 
		;Response in A&E, use CTC timeout, Z -> no response, NZ -> 376S has responded
waitForResponse:  
		ei
		halt    
		ld 		A,(CTCdelayFlag)
		cp 		CTC_TIMEOUT 						; if A=EE, Z is set, timeout (set by CTC interrupt)
		ret 	Z							; return with Z -> timeout set
				; call 	getResponseFromUSB
				; jr 		Z,.loop
		; *** 	SIO B interrupt place data in E				; 
		; ***	No timeout - read data.

		call	CTC1_INT_OFF			; stop CTC sending timeout's  (A=0)
		inc 	A
		ld 		A,E
		ret								; NZ set, 376S has responded


		; ***	send UART init code $57,$AB
beginUART:
		ld 	    E,$57
		call 	outByte367S

		ld 		E,$AB
		call 	outByte367S
		ret
		;***		Data in E, send byte to HC376S
outByte367S:
.loop: 
   		sub		a				;clear a, write into WR0: select RR0
		inc		a				;select RR1
		out		(sio_bc),A
        in		A,(sio_bc)	    ;read TRx , set when all char are sent 'all sent' 
        bit		0,A
        jr      z,.loop
    	
        ld      A,E
        out     (sio_bd),A      ; send actl. byte

		ret

		; *** 	interrupt at input from HC376S
ReadUSBHandler:
		in  	A,(sio_bd)		  		;read char from SIO B
		ld 		E,A

		out 	(gpioB),A			; out to ledbardm

		; in  	A,(CH1)
		; ld 		(TempVar8),A
		; in  	A,(CH0)
		; ld 		(TempVar7),A
; 		cp 		USB_INT_CONNECT
; 		jr  	NZ,.p2
; 		call 	writeSTRBelow_CRLF
; 		db		0,">USB_INT_CONNECT",0,0
; 		jr 		.p3
; .p2:
; 		cp 		USB_INT_DISCONNECT
; 		jr 		NZ,.p3
; 		call 	writeSTRBelow_CRLF
; 		db		0,">USB_INT_DISCONNECT",0,0
.p3:
		

		ei
		reti
;******************************************************************************

		GLOBAL		delay2s,delay1s,delay500ms,delay200ms,delay100ms,delay50ms,delay20ms,delay10ms

delay2s:	
		ld 		DE,$7DFA		; 8MHz: $48D9  10MHz:  9BFC, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay1s:	
		ld 		DE,$5AD9		; 8MHz: $48D9  10MHz:  5AD9, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay500ms:	
		ld 		DE,$2DD9		; 8MHz: $24D9  10MHz:  2DD9, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay250ms:	
		ld 		DE,$287A		; 8MHz: $24D9  10MHz:  287A, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay200ms:
		ld 		DE,$12D9		; 8MHz: $197D  10MHz:  12D9, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay100ms:
		ld 		DE,$155D		; 8MHz: $1647  10MHz:  155D, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay50ms:
		ld 		DE,$087A		; 8MHz: $0B47  10MHz:  087A, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay20ms:
		ld 		DE,$0A27		; 8MHz: $180D  10MHz:  0A27, Prescaler
		ld 		A,_Prescaler
		jr 		CTC_Delay
delay10ms:
		ld 		DE,$197D		; 8MHz: $147D  10MHz:  197D, Prescaler
		ld 		A,00
		jr 		CTC_Delay
delay5ms:
		ld 		DE,$0B8E		; 8MHz: $24D9  10MHz:  0B8E, Prescaler
		ld 		A,00
		jr 		CTC_Delay
delay2ms:
		ld 		DE,$057D		; 8MHz: $1914  10MHz:  057D, Prescaler
		ld 		A,00
		jr 		CTC_Delay
delay1ms:
		ld 		DE,$0D18		; 8MHz: $24D9  10MHz:  0D18, Prescaler
		ld 		A,00
		jr 		CTC_Delay
CTC_Delay:
		;init CH 0 and 1 as interrupt on timeout
		; A is set or cleared with _Prescaler
		; value in DE
		di
		or 	 	_Rising|_Timer|_TC_Follow|_Reset|_CW		; timer 14390 Hz
		out		(CH0),A 		; CH0 is on hold now
		ld		A,D				; time constant (prescaler; 126; 93; 6MHz -> 1 sec peroid) 232/101; 
								; time constant (prescaler; 181; 79; 14390,625 khz -> 2, sec peroid;  
		out		(CH0),A			; and loaded into channel 0
		
		ld		A,_INT_EN|_Counter|_Rising|_TC_Follow|_Reset|_CW	
		out		(CH1),A			; CH1 counter
		ld		A,E			; time constant 66 defined
		out		(CH1),A			; and loaded into channel 2
		xor 	A 				; clear A
		ld 		(CTCdelayFlag),A ; reset timeout flag
		ei

		ret

; reset timeout flag
resDelayFlag:
		ld 		A,00
		ld 		(CTCdelayFlag),A ; reset timeout flag
		ret


CTC_delay_INT_handler:

		ld		A,_Counter|_Rising|_Reset|_CW	
		out		(CH1),A					; reset and turn off interrupt CH1
		ld		A,CTC_TIMEOUT 			; set timeout flag
		ld 		(CTCdelayFlag),A 		; reset timeout flag

		ei
		reti


; PrintD0ToScreenHEX:
; 		***		print D0 to sceen on one row
; 		move.w	#8,-(sp)
; 		pea		Cstr2
; 		call 	bintohexstr		; result in Cstr2
; 		lea 	Cstr2,A0 
; 		call 	putstring_cr
; 		rts

; PrintD0ToScreenDEC:
; 		***		print D0 to sceen on one row
; 		pea		Cstr2
; 		call 	bintodecstr		; result in Cstr2
; 		lea 	Cstr2,A0 
; 		call 	putstring_cr
; 		rts



;**###############################################################
;**################################################################

		xdef 	blockstart_USB,blockend_USB

blockstart_USB:
	;***		len = BC4 ??
	; dw	$0000,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF
	; dw	$12345678,$1111,$2222,$3333,$4444,$5555,$6666,$7777
	; dw	$8888,$9999,$AAAA,$BBBB,$CCCC,$DDDD,$EEEE,$FFFF   ; 256 bytes
	; dw	$0018,$11E0,$11E1,$11E2,$11E3,$11E4,$11E5,$11E6
	; dw	$11E7,$11E8,$11E9,$11EA,$11EB,$11EC,$11ED,$11EE
	; dw	$11E0,$11E1,$11E2,$11E3,$11E4,$11E5,$11E6,$11E7
blockend_USB:



	end


