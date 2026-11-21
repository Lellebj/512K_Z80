	/*
	* ------------------------------------------------------------
	*	Linker script for Z80-512K
	*/

	
	ENTRY(ram)

	sram  = 0x8000;          /* start of user memory                     */
	flash = 0x0000;

	MEMORY
	{
	RAM  : org = sram,  l = 0x8000     /* program can use all RAM > 4K */
	FLASH  : org = flash,  l = 0x8000     /* program can use all RAM > 4K */
	}

	stacktop =	0xFFFE	;

	PROVIDE(_String_HEAP	= 0xF800);
	
	PROVIDE(_RAMSTART		= 0xD000);	
	PROVIDE(_EETESTPROG		= 0x100);	
	PROVIDE(_NMISTART		= 0x66);	
	PROVIDE(_FLASHSTART		= 0x00);	
	
	PROVIDE(_Z80PIO_Base	= 0x0);
	PROVIDE(_Z80CTC_Base	= 0x10);
	PROVIDE(_Z80SIO_Base	= 0x20);
	PROVIDE(_Z80_BankCS  	= 0x30);		/*	 30 - 3F   */
	PROVIDE(_8Bitsout 		= 0x40);		/*	 40 - 4F  */
	PROVIDE(_CE_RST_BANK 	= 0x50);		/*	 50 - 5F  */
	PROVIDE(_CE_HD44780_DATA = 0x60);		/*	 50 - 5F  */
	PROVIDE(_CE_HD44780_CTRL = 0x70);		/*	 50 - 5F  */

	PROVIDE( SRAM_VAR_END = 0xFFF0 );
	PROVIDE( SRAM_VAR_START =  _String_HEAP);
	PROVIDE( HEAP_SIZE 	 	= SRAM_VAR_END-SRAM_VAR_START);

	PROVIDE( bufferSize		=	0x3B   );				/* bufferlängd  på läsbuffert */
	PROVIDE( Textbufsize	=	0x40  )	;				/*text input buffer , läsbuffert vid inmatning*/
	PROVIDE( Textbuf		= _String_HEAP);			/*text buffer */

	PROVIDE( S1x			=	0x40 + _String_HEAP);  	/*Textbuf + Textbufsize used by DumpRegisters, SD/USB enum*/
	PROVIDE( paramStack 	=   0x3E + _String_HEAP); 	/* adresspekare för utskrift av tal i textform */	 
	PROVIDE( commParseTable =	0x80 + _String_HEAP);  	/*command parser table, size $80-$100=$80 (128 bytes) */
	PROVIDE( commAdr1 =			0x84 + _String_HEAP);  	/*DS	4	command address 1  4 byte*/  
	PROVIDE( commAdr2 =			0x88 + _String_HEAP);  	/*DS	2	command address 2  4 byte*/
	PROVIDE( commLvl1 =			0x90 + _String_HEAP);  	/*DS	1	command lValue 1   16 byte*/
	PROVIDE( commLvl2 =			0xA0 + _String_HEAP);  	/*DS	2	command lValue 2 	 16 byte*/
	PROVIDE( commStr1 =			0xB0 + _String_HEAP);  	/*DS	40	Command string 1 	 40 byte*/
	PROVIDE( commStr2 =			0xD8 + _String_HEAP);  	/*DS	2	command string 2 	 40 byte  -> 0xFF*/

	PROVIDE( T_BUFFER 		=	0x100 + _String_HEAP);		/*;32-BYTE BUFFER */

	PROVIDE( SP_value		=	0x120 + _String_HEAP);		/* DS	2	sparat värde på stackpek  */
	PROVIDE( SP_valueP		=	0x122 + _String_HEAP);		/* DS	2	sparat värde på stackpek vid utskriftkö  */
	PROVIDE( reg16_value	=	0x124 + _String_HEAP);		/* DS	2	*/
	PROVIDE( reg16_value2	=	0x126 + _String_HEAP);		/* DS	2	*/
	PROVIDE( validCommand	=	0x128 + _String_HEAP);		/* DS	1	command number found 1 byte */
	PROVIDE( PCvalue		=	0x12A + _String_HEAP);		/* DS	2	actual PC value , 2 bytes */
	PROVIDE( PCinpFlag		=	0x12C + _String_HEAP);		/* DS	2	indicates input of typed address 2 byte */
	PROVIDE( generalFlags	=	0x12E + _String_HEAP);		/* DS	2	indicates general flags, 2 byte */
	PROVIDE( inBufferPtr	=	0x130 + _String_HEAP);      /* DS	2	Extrapekare för inbuffer  DS 2*/
	PROVIDE( inBufferVal	=	0x132 + _String_HEAP);      /* DS	1	Värde > 0 om nytt tecken finns DS 1 */
	PROVIDE( inBufferOffset	=	0x133 + _String_HEAP);      /* DS	2	Offset > 0 om cursor inte är på sista position */
	PROVIDE( inbufferDeactivate = 0x135+ _String_HEAP); 	/* DS	1	om > 0: uppdatera INTE läsbuffer vid avbrottshantering */			
	PROVIDE( ESCsequence	= 	0x136 + _String_HEAP); 		/* DS	1	om > 0: hantera escape sekvens vid inmatning, sparas ej i läsbuffer. */			
	PROVIDE( InsertMode		= 	0x137 + _String_HEAP); 		/* DS	1	om > 0: hantera escape sekvens vid inmatning, sparas ej i läsbuffer. */			
	PROVIDE( memBankID		= 	0x138 + _String_HEAP); 			/* DS	1	memoryBank value  b7-flash on/off b4-6:EEPBank b0-3:RAMBank */	
	PROVIDE( rstBankID		= 	0x139 + _String_HEAP); 			/* DS	1	minnesbank  bit3-flash på av bit2 förhindra reset av krets IC622 bit0 OE för IC620 */	

	PROVIDE( inBufCount		=	0x13A + _String_HEAP);			/*	DS	1	number of characters in input buffer DS	1	*/
	PROVIDE( outHeadAdr		=	0x13C + _String_HEAP);			/*	DS	2	address of oldest character in output buffer	DS	2	*/
	PROVIDE( outTailAdr		=	0x13E + _String_HEAP);			/*	DS	2	address of newest character in output buffer	DS	2	*/
	PROVIDE( OutBufCount	=	0x140 + _String_HEAP);			/*	DS	1	number of characters in output buffer	DS	1	*/
	PROVIDE( OutINTExpect	=	0x141 + _String_HEAP);			/*	DS	1	output interrupt expected	DS	1	*/
																/*; (0 = no interrupt expected. ff = interrupt expected)*/
	PROVIDE( XBAddr			=	0x142 + _String_HEAP);			/*	DS	2	XMODEM block start address  2byte	*/
	PROVIDE( XMChkSum		=	0x144 + _String_HEAP);			/*	DS	2	XMODEM block checksum  2byte	*/
	PROVIDE( SDUSB_Helper	=	0x146 + _String_HEAP);			/*	DS	2	SD och USB hjälpvariabel  2byte	*/

	PROVIDE( TempVar0		=	0x148 + _String_HEAP);			/*	DS	2	Temp1		*/
	PROVIDE( TempVar1		=	0x14A + _String_HEAP);			/*	DS	1	Temp1		*/
	PROVIDE( TempVar2		=	0x14B + _String_HEAP);			/*	DS	1	Temp2		*/
	PROVIDE( TempVar3		=	0x14C + _String_HEAP);			/*	DS	1	Temp3		*/
	PROVIDE( TempVar4		=	0x14D + _String_HEAP);			/*	DS	1	Temp4		*/
	PROVIDE( TempVar5		=	0x14E + _String_HEAP);			/*	DS	1	Temp5		*/
	PROVIDE( TempVar6		=	0x14F + _String_HEAP);			/*	DS	1	Temp6		*/
	PROVIDE( TempVar7		=	0x150 + _String_HEAP);			/*	DS	1	Temp7		*/
	PROVIDE( TempVar8		=	0x151 + _String_HEAP);			/*	DS	1	Temp8		*/

	PROVIDE( Temp_CTRL	 	= 	0x152 + _String_HEAP);			/*	DS  1 	temp byte 	*/
	PROVIDE( CTCdelayFlag 	= 	0x153 + _String_HEAP);			/*	DS  1 	CTC flags that timeout has occured 1 byte	*/
	PROVIDE( packLen 		=	0x154 + _String_HEAP);			/*	DS	2	bytelength of packet   2-byte */
	PROVIDE( charLen 		=	0x156 + _String_HEAP);			/*	DS	2	length of written file   2-byte */
	PROVIDE( sdWRpointer 	=	0x158 + _String_HEAP);			/*	DS 	2	bytepointer to write file   2-byte */

	PROVIDE( LENS1			=	0x15A + _String_HEAP);			/*	DS  1	length of string 1 */
	PROVIDE( LENS2			=	0x15B + _String_HEAP);			/*	DS	1	length of string 2 */
	PROVIDE( S1ADR			=	0x15C + _String_HEAP);			/*	DS  2	BASE ADDRESS OF STRING 1 */
	PROVIDE( S1LEN			=	0x15E + _String_HEAP);			/*	DS  1	LENGTH OF STRING 1 */
	PROVIDE( S2LEN			=	0x15F + _String_HEAP);			/*	DS  1	LENGTH OF STRING 2 */
	PROVIDE( STRGOV			=	0x160 + _String_HEAP);			/*	DS  1	STRING OVERFLOW FLAG */
	PROVIDE( STRING			=	0x162 + _String_HEAP);			/*	DS	2	BASE ADDRESS OF STRING */
	PROVIDE( SUBSTG			=	0x164 + _String_HEAP);			/*	DS	2	BASE ADDRESS OF SUBSTRING */
	PROVIDE( SLEN			=	0x166 + _String_HEAP);			/*	DS	1	LENGTH OF STRING */
	PROVIDE( SUBLEN			=	0x167 + _String_HEAP);			/*	DS	1	LENGTH OF SUBSTRING */
	PROVIDE( INDEX			=	0x168 + _String_HEAP);			/*	DS	1	CURRENT INDEX INTO STRING */
	PROVIDE( MAXLEN			=	0x169 + _String_HEAP);			/*	DS	1	MAXIMUM    LENGTH OF DESTINATION STRING */
	PROVIDE( CPYERR			=	0x16A + _String_HEAP);			/*	DS	1	COPY    ERROR FLAG */
	PROVIDE( DELERR			=	0x16B + _String_HEAP);			/*	DS	1	DELETE ERROR FLAG */
	PROVIDE( INSERR			=	0x16E + _String_HEAP);			/*	DS	1	FLAG USED TO INDICATE ERROR */
	PROVIDE( dumpText		=	0x170 + _String_HEAP);			/*	DC	$80 '/'  */

	PROVIDE( outBufferSize	=	0xF0	);							/*	;SIZE OF OUTPUT BUFFER */
	PROVIDE( outBuffer		=	0x200 + _String_HEAP);	         	 	/*;OUTPUT BUFFER */
	PROVIDE( endOutBuffer	=	0x2F0 + _String_HEAP);	/*		END OF OUTPUT BUFFER */

	/* PROVIDE( list_len		=	0x48	);				/*	 length of command pointer list */
	/* PROVIDE( Comm_Ptr_list	=	0x380 + _String_HEAP); 	/*	 command pointer list  L=0x80*/

	SIO_PIO_CTC_stack		=	0x300 + _String_HEAP ;    	  	/* Interrupt vektorer för IO-kretsarna  (FB00) $20 bytes*/
	
	/*   C-Kermit */
	PROVIDE( PktLen 		=  0x320 + _String_HEAP) ; 		/* C-kermit paketlängd  2 byte */
	PROVIDE( PktSeq     	=  0x322 + _String_HEAP) ;  		/* C-kermit sekvensnummer 1 byte */
	PROVIDE( PktType    	=  0x323 + _String_HEAP) ;  		/* C-kermit paket typ  1 byte */
	PROVIDE( PktStart    	=  0x324 + _String_HEAP) ;  		/* C-kermit paket typ  adress för paketstart inkl. längd 2 byte */
	PROVIDE( PktSlut    	=  0x326 + _String_HEAP) ;  		/* C-kermit paket typ  adress för paketslut sista data 2 byte */
	PROVIDE( PktCHSum    	=  0x328 + _String_HEAP) ;  		/* C-kermit paket typ  Checksumma för paket 2 byte */
	PROVIDE( PktS    		=  0x010				) ;  		/* C-kermit paket typ  S Adress i ram bank 'S' 40 byte */
	PROVIDE( PktF    		=  0x050				) ;  		/* C-kermit paket typ  F Adress i ram bank 'F' 40 byte */
	PROVIDE( PktA    		=  0x0A0 				) ;  		/* C-kermit paket typ  A Adress i ram bank 'A' 40 byte */

	PROVIDE( PktData    	=  0x32A + _String_HEAP) ; 	/* MAX_BUFF ; Reservera minne för inkommande data $100 bytes  */
						/*  Nästa adr   0x42A + _String_HEAP   */
/*	PROVIDE( packetBaseAddress = 	0x480 + _String_HEAP);  Används ej  */
/*	PROVIDE( prevPacketByte01	=	0x580 + _String_HEAP);  Används ej 	*/

	/*	; Code in $D008-D00D = $00000000 - $AAAAAAAA: copy from flash
		; Code in $D008-D00D = $CCCCCCCC: code uploaded from xmodem/or DMA. Do not copy from flash
		; Code in $D008-D00D = $33333333: code uploaded from Arduino. Do not copy from flash, 
	*/
/*	PROVIDE( BootCode		=	0x33333333);	*/
	PROVIDE( BootCode		=	0xCCCCCCCC);	
/*	PROVIDE( BootCode		=	0x11111111);	*/	
	PROVIDE( BootCodeAdr	=	_RAMSTART+8);	

	SECTIONS
	{

	


		Dummy_Block  0x8000 :
			{
			*.o (EXCLUDE_SECTION( Z80_EEP*.o)	SD_USB_Start  ) 
			*.o (EXCLUDE_SECTION (*.o )EEtestprog)
			*.o (EXCLUDE_SECTION (*.o )FLASH_Startup)
			*.o (EXCLUDE_SECTION (*.o )INT_IM1)
			*.o (EXCLUDE_SECTION (*.o )I2C_Func)
			*.o (EXCLUDE_SECTION (*.o )LAB_RGBLED)
			*.o (EXCLUDE_FILE( I2C_func.o)   I2C_Func) ;
			*.o (EXCLUDE_FILE( Z80_EEP*.o)	SD_USB_Start  ) 
			*.o (EXCLUDE_FILE( SPIbyPIO_SDcard*.o)	SPI_SD_Code  ) 
			*.o (EXCLUDE_FILE( TEST_SPI_SDcard.s*.o)	SPI_SD_Code  ) 
	
			*.o(EEtestprog)
			*.o(FLASH_Startup)
			*.o(INT_IM1)
			. = ALIGN(0x20);
			}	> RAM

		LAB_RGBLED  0xA000 : 
			{
				KEEP(*.o(LAB_RGBLED))
			. = ALIGN(0x20);
	
			}


		TheMonitor  _RAMSTART :
		{


			KEEP(*.o(Monitor))
			_MonEnd = .;
			. = ALIGN(0x20);
/*
		    KEEP( *.o(SD_USB_Start))
			_USB_EEprog_end = .;
			. = ALIGN(0x20);
*/
			KEEP( *.o(INIT_BODY))
            . = ALIGN(0x20);

			KEEP(*.o (Functions))
			_Funk_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (CodeConversion))
			_CodeConv_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (Strings))
			_Strings_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (InOutInterrupt))
			_IOI_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (CH376S_Support))
			_USB_end = .;
			. = ALIGN(0x20); 

			KEEP(*.o (Xmodems))
			_XMODEM_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (Kermit))
			_KERMIT_end = .;
			. = ALIGN(0x20);




		}	> RAM

/*
			*.o (LCD_DRIVER)
			_LCD_END = .;
			. = ALIGN(0x20);

			*.o (I2C_Func)
			_I2C_end = .;
			. = ALIGN(0x20);

			KEEP ( *.o (SD_SPI_FUNC))
			_SD_SPI_end = .;
*/

	_end = .;

	}
