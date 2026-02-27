	/*
	* ------------------------------------------------------------
	*	Linker script for Z80-512K
	*	make all -B -f MakeBOOT, kopiera till Flash mem 0000, btm-000
	*/

	
	ENTRY(ram)

	sram  = 0x8000;          /* start of user memory                     */
	flash = 0x0000;

	MEMORY
	{
	RAM  : org = sram,  l = 0x8000     /* program can use all RAM > 4K */
	FLASH  : org = flash,  l = 0x8000     /* program can use all RAM > 4K */
	}

	
	PROVIDE(_String_HEAP	= 0xF000);
	
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

	PROVIDE( SRAM_VAR_END = 0xFFF0 );
	PROVIDE( SRAM_VAR_START =  _String_HEAP);
	PROVIDE( HEAP_SIZE 	 	= SRAM_VAR_END-SRAM_VAR_START);

	Textbufsize				=		0x40;			; /*text input buffer */
	PROVIDE( Textbuf		= _String_HEAP);		; /*text buffer */

	PROVIDE( S1x			=	0x40 + _String_HEAP);  	/*Textbuf + Textbufsize used by DumpRegisters, SD/USB enum*/
	PROVIDE( commParseTable =	0x80 + _String_HEAP);  	/*command parser table, size $80-$100=$80 (128 bytes) */
	PROVIDE( commAdr1 =			0x84 + _String_HEAP);  	/*command address 1  4 byte*/  
	PROVIDE( commAdr2 =			0x88 + _String_HEAP);  	/*command address 2  4 byte*/
	PROVIDE( commLvl1 =			0x90 + _String_HEAP);  	/*command lValue 1   16 byte*/
	PROVIDE( commLvl2 =			0xA0 + _String_HEAP);  	/*command lValue 2 	 16 byte*/
	PROVIDE( commStr1 =			0xB0 + _String_HEAP);  	/*command string 1 	 40 byte*/
	PROVIDE( commStr2 =			0xD8 + _String_HEAP);  	/*command string 2 	 40 byte  -> 0xFF*/

	PROVIDE( T_BUFFER 		=	0x100 + _String_HEAP);		/*;16-BYTE BUFFER */

	PROVIDE( SP_value		=	0x120 + _String_HEAP);
	PROVIDE( reg16_value	=	0x124 + _String_HEAP);
	PROVIDE( reg16_value2	=	0x126 + _String_HEAP);
	PROVIDE( validCommand	=	0x128 + _String_HEAP);		/* command number found 1 byte */
	PROVIDE( PCvalue		=	0x12A + _String_HEAP);		/* actual PC value , 2 bytes */
	PROVIDE( PCinpFlag		=	0x12C + _String_HEAP);		/* indicates input of typed address 2 byte */
	PROVIDE( generalFlags	=	0x12E + _String_HEAP);		/* indicates general flags, 2 byte */
	PROVIDE( bufferSize		=	0x64		);				/*;SIZE OF INgeneral flags,  */
	PROVIDE( inBuffer  		 =	0x130 + _String_HEAP);			/*	INPUT BUFFER */
	PROVIDE( inBufferEnd	=	0x194 + _String_HEAP);      	/*;END OF INPUT BUFFER */
	PROVIDE( inbufferDeactivate = 0x196+ _String_HEAP); 		/* if > 0: do not update inbuffer at char read */			
	PROVIDE( memBankID		= 	0x198 + _String_HEAP); 			/* memoryBank value  b7-flash on/off b4-6:EEPBank b0-3:RAMBank */	
	PROVIDE( rstBankID		= 	0x199 + _String_HEAP); 			/* reset memoryBank  b3-flash on/off b2 reset inhibit/ b0 OE IC62 */	

	PROVIDE( inHeadAdr		=	0x19A + _String_HEAP);			/*	address of oldest character in input buffer DS	2	*/
	PROVIDE( inTailAdr		=	0x19C + _String_HEAP);			/*	address of newest character in input buffer DS	2	*/
	PROVIDE( inBufCount		=	0x19E + _String_HEAP);			/*	number of characters in input buffer DS	1	*/
	PROVIDE( outHeadAdr		=	0x1A0 + _String_HEAP);			/*	address of oldest character in output buffer	DS	2	*/
	PROVIDE( outTailAdr		=	0x1A2 + _String_HEAP);			/*	address of newest character in output buffer	DS	2	*/
	PROVIDE( OutBufCount	=	0x1A4 + _String_HEAP);			/*	number of characters in output buffer	DS	1	*/
	PROVIDE( OutINTExpect	=	0x1A6 + _String_HEAP);			/*	output interrupt expected	DS	1	*/
														/*; (0 = no interrupt expected.
														; ff = interrupt expected)*/
	PROVIDE( XBAddr			=	0x1A8 + _String_HEAP);			/*	XMODEM block start address  2byte	*/
	PROVIDE( XMChkSum		=	0x1AA + _String_HEAP);			/*	XMODEM block checksum  2byte	*/

	PROVIDE( TempVar1		=	0x1B1 + _String_HEAP);			/*	Temp1	DS	1	*/
	PROVIDE( TempVar2		=	0x1B2 + _String_HEAP);			/*	Temp2	DS	1	*/
	PROVIDE( TempVar3		=	0x1B3 + _String_HEAP);			/*	Temp3	DS	1	*/
	PROVIDE( TempVar4		=	0x1B4 + _String_HEAP);			/*	Temp4	DS	1	*/
	PROVIDE( TempVar5		=	0x1B5 + _String_HEAP);			/*	Temp5	DS	1	*/
	PROVIDE( TempVar6		=	0x1B6 + _String_HEAP);			/*	Temp6	DS	1	*/
	PROVIDE( TempVar7		=	0x1B7 + _String_HEAP);			/*	Temp7	DS	1	*/
	PROVIDE( TempVar8		=	0x1B8 + _String_HEAP);			/*	Temp8	DS	1	*/

	PROVIDE( CTCdelayFlag 	= 	0x1BA + _String_HEAP);			/*	CTC flags that timeout has occured 1 byte	*/
	PROVIDE( packLen 		=	0x1BC + _String_HEAP);			/*	length of packet   2-byte */
	PROVIDE( charLen 		=	0x1BE + _String_HEAP);			/*	length of written file   2-byte */
	PROVIDE( sdWRpointer 	=	0x1C0 + _String_HEAP);			/*	pointer to write file   2-byte */

	PROVIDE( LENS1			=	0x1C2 + _String_HEAP);			/*	DS  1	length of string 1 */
	PROVIDE( LENS2			=	0x1C3 + _String_HEAP);			/*	DS	1	length of string 2 */
	PROVIDE( S1ADR			=	0x1C4 + _String_HEAP);			/*	DS  2	BASE ADDRESS OF STRING 1 */
	PROVIDE( S1LEN			=	0x1C6 + _String_HEAP);			/*	DS  1	LENGTH OF STRING 1 */
	PROVIDE( S2LEN			=	0x1C7 + _String_HEAP);			/*	DS  1	LENGTH OF STRING 2 */
	PROVIDE( STRGOV			=	0x1C8 + _String_HEAP);			/*	DS  1	STRING OVERFLOW FLAG */
	PROVIDE( STRING			=	0x1C9 + _String_HEAP);			/*	DS	2	BASE ADDRESS OF STRING */
	PROVIDE( SUBSTG			=	0x1CB + _String_HEAP);			/*	DS	2	BASE ADDRESS OF SUBSTRING */
	PROVIDE( SLEN			=	0x1CD + _String_HEAP);			/*	DS	1	LENGTH OF STRING */
	PROVIDE( SUBLEN			=	0x1CF + _String_HEAP);			/*	DS	1	LENGTH OF SUBSTRING */
	PROVIDE( INDEX			=	0x1D0 + _String_HEAP);			/*	DS	1	CURRENT INDEX INTO STRING */
	PROVIDE( MAXLEN			=	0x1D1 + _String_HEAP);			/*	DS	1	MAXIMUM    LENGTH OF DESTINATION STRING */
	PROVIDE( CPYERR			=	0x1D2 + _String_HEAP);			/*	DS	1	COPY    ERROR FLAG */
	PROVIDE( DELERR			=	0x1D3 + _String_HEAP);			/*	DS	1	DELETE ERROR FLAG */
	PROVIDE( INSERR			=	0x1D4 + _String_HEAP);			/*	DS	1	FLAG USED TO INDICATE ERROR */
	PROVIDE( dumpText		=	0x1D6 + _String_HEAP);			/*	DC	$80 ''  */

	PROVIDE( outBufferSize	=	0xF0	);							/*	;SIZE OF OUTPUT BUFFER */
	PROVIDE( outBuffer		=	0x280 + _String_HEAP);	         	 	/*;OUTPUT BUFFER */
	PROVIDE( endOutBuffer	=	0x370 + _String_HEAP);	/*		END OF OUTPUT BUFFER */

	PROVIDE( list_len		=	0x48	);				/*	 length of command pointer list */
	PROVIDE( Comm_Ptr_list	=	0x380 + _String_HEAP); 	/*	 command pointer list */
	PROVIDE( packetBaseAddress = 	0x480 + _String_HEAP);
	PROVIDE( prevPacketByte01	=	0x580 + _String_HEAP);	
	
	/*	; Code in $D008-D00D = $00000000 - $AAAAAAAA: copy from flash
		; Code in $D008-D00D = $CCCCCCCC: code uploaded from xmodem/or DMA. Do not copy from flash
		; Code in $D008-D00D = $33333333: code uploaded from Arduino. Do not copy from flash, 
	*/
/*	PROVIDE( BootCode		=	0x33333333);	*/
	PROVIDE( BootCode		=	0xCCCCCCCC);		
	PROVIDE( BootCodeAdr	=	_RAMSTART+8);	

	SECTIONS
	{




		FLASH_block  _FLASHSTART :
			{
			. = 0x00;
			KEEP(*.o(FLASH_Startup))

			. = _NMISTART;	
			KEEP( *.o(INT_IM1))
			. = ALIGN (0x80);

			KEEP( *.o(INIT_BODY))
            . = ALIGN(0x20);

		    KEEP(*.o(SD_USB_Start))
			_BootSDUSB_EEprog_end = .;
			. = ALIGN(0x20);
	
			KEEP(*.o (CH376S_Support))
			_BootUSB_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (Functions))
			.= ALIGN(0x20) ;


			KEEP(*.o (InOutInterrupt))
			_BootIOI_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (CodeConversion))
			_BootCodeConv_end = .;
			. = ALIGN(0x20);

			KEEP(*.o (Strings))
			_BootStrings_end = .;
			. = ALIGN(0x20);

			}  >FLASH

		Dummy  0xD000 :
		{

			*.o (EXCLUDE_SECTION (*.o ) Monitor  ) ;

/*
		     *.o(EEtestprog)
			_EET_EEprog_end = .;
			. = ALIGN(0x20);
*/
	/*

			EXCLUDE_FILE (*Z80_EEP_.o) *(EEtestprog)
			EXCLUDE_SECTION(Z80_EEP_.o ) *(EEtestprog)

			*.o (EXCLUDE_FILE(Z80_PLD_PCB_.o  )  Monitor ) ;
			*.o (EXCLUDE_FILE( I2C_func.o)   I2C_Func) ;
			*.o (EXCLUDE_FILE(XMODEM*.o )  Xmodems ) ;
			*.o (EXCLUDE_FILE(I2C_func.o)  I2C_Func)
			*.o (EXCLUDE_FILE(Z80_PLD_FLash_support.o)  Functions)

			*.o (EXCLUDE_SECTION (I2C_func.o   XMODEM*.o) I2C_*  Xmodems) ;
			*.o (EXCLUDE_SECTION (*.o ) Functions  ) ;
			*.o (EXCLUDE_SECTION (*.o ) Monitor  ) ;
	*/
		}	> RAM
	_end = .;

	}
