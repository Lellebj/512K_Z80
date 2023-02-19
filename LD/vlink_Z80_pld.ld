	/*
	* ------------------------------------------------------------
	*	Linker script for Z80-512K
	*/

	
	ENTRY(ram)

	sram  = 0x8000;          /* start of user memory                     */
	eeprom = 0x0000;

	MEMORY
	{
	RAM  : org = sram,  l = 0xFFFF     /* program can use all RAM > 4K */
	EEPROM  : org = eeprom,  l = 0x8000     /* program can use all RAM > 4K */
	}

	
	/* ROM absolute addresses */
	/*PROVIDE(_FIRMWARE       = 0x00FC0000);  /* firmware address             */
	/*PROVIDE(_FIRMWARE_REV   = 0x00FC0400);  /* firmware revision code       */

	PROVIDE(_String_HEAP	= 0xF000);
	
	PROVIDE(_RAMSTART		= 0x8000);	
	PROVIDE(_FUNCTIONS		= 0xA000);	
	PROVIDE(_IOLIB			= 0x9000);	
	PROVIDE(_SD_SPI_LIB		= 0xB000);	
	PROVIDE(_EETESTPROG		= 0x100);	
	PROVIDE(_NMISTART		= 0x66);	
	PROVIDE(_EEPROMSTART	= 0x00);	
	
	PROVIDE(_Z80PIO_Base	= 0x0);
	PROVIDE(_Z80CTC_Base	= 0x10);
	PROVIDE(_Z80DART_Base	= 0x20);
	PROVIDE(_Z80_BankCS  	= 0x30);		/*	 30 - 3F   */
	PROVIDE(_8Bitsout 		= 0x40);		/*	 40 - 4F  */
	PROVIDE(_CE_RST_BANK 	= 0x80);		/*	 80 - 8F  */

	PROVIDE( SRAM_VAR_END = 0xFFF0 );
	PROVIDE( SRAM_VAR_START =  _String_HEAP);
	PROVIDE( HEAP_SIZE 	 	= SRAM_VAR_END-SRAM_VAR_START);

	Textbufsize				=		0x40;			; /*text input buffer */
	PROVIDE( Textbuf		= _String_HEAP);		; /*text buffer */

	PROVIDE( S1x			=	0x40 + _String_HEAP);  	/*Textbuf + Textbufsize */
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
	PROVIDE( validCommand	=	0x127 + _String_HEAP);		/* command number found 1 byte */
	PROVIDE( PCvalue		=	0x12A + _String_HEAP);		/* actual PC value , 2 bytes */
	PROVIDE( PCinpFlag		=	0x12C + _String_HEAP);		/* indicates input of typed address 2 byte */
	PROVIDE( generalFlags	=	0x12E + _String_HEAP);		/* indicates general flags, 2 byte */
	PROVIDE( bufferSize		=	0x64		);				/*;SIZE OF INgeneral flags,  */
	PROVIDE( inBuffer  		 =	0x130 + _String_HEAP);			/*	INPUT BUFFER */
	PROVIDE( inBufferEnd	=	0x194 + _String_HEAP);      	/*;END OF INPUT BUFFER */
	PROVIDE( inbufferDeactivate = 0x196+ _String_HEAP); 		/* if > 0: do not update inbuffer at char read */			
	PROVIDE( memBankID		= 	0x198 + _String_HEAP); 			/* memoryBank value  b7-flash on/off b4-6:EEPBank b0-3:RAMBank */	

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

	PROVIDE( outBufferSize	=	160	);							/*	;SIZE OF OUTPUT BUFFER */
	PROVIDE( outBuffer		=	0x200+ _String_HEAP);	         	 	/*;OUTPUT BUFFER */
	PROVIDE( endOutBuffer	=	0x2F0+ _String_HEAP);	/*		END OF OUTPUT BUFFER */

	PROVIDE( list_len		=	0x48	);				/*	 length of command pointer list */
	PROVIDE( Comm_Ptr_list	=	0x300 + _String_HEAP); 	/*	 command pointer list */
	PROVIDE( packetBaseAddress = 	0x400 + _String_HEAP);
	PROVIDE( prevPacketByte01	=	0x500 + _String_HEAP);	




	SECTIONS
	{



		EEPROM_Startup _EEPROMSTART :
			{
			 KEEP(*.o(EEPROM_Startup))
			} > EEPROM

		RST08  0x08 : { *.o(RST08)}> EEPROM
		RST10  0x10 : { *.o(RST10)}> EEPROM
		RST18  0x18 : { *.o(RST18)}> EEPROM
		RST20  0x20 : { *.o(RST20)}> EEPROM
		RST28  0x28 : { *.o(RST28)}> EEPROM
		RST30  0x30 : { *.o(RST30)}> EEPROM
		RST38  0x38 : { *.o(RST38)}> EEPROM


		INT_IM1 _NMISTART : 
			{

			KEEP( *.o(INT_IM1))

			} > EEPROM

		EEtestprog _EETESTPROG :
			{
			_EE_init = .;
			KEEP( *.o(EEtestprog))
			_EEprog_end = .;
			} > EEPROM

/*
		singleAssembly	_RAMSTART :
		{
			KEEP( *.o(singleAssembly))  :
			_singleAssemblySRam_end  = .;
		}	> RAM
*/


		Monitor	_RAMSTART  :
		{
			KEEP( *.o(Monitor))
			_MainSRam_end  = .;
		}	> RAM


		Functions	ALIGN (0x400)  :
		/* Functions	_FUNCTIONS : */
		{
			KEEP ( *.o (Functions)) ;

			_Funk_end = .;
		} 	> RAM


		SD_SPI_FUNC		ALIGN (0x20)  :
		/* SD_SPI_FUNC	_SD_SPI_LIB : */
		{
			KEEP ( *.o (SD_SPI_FUNC))
			_SD_SPI_end = .;
		} 	> RAM

		SD_HELLO 	ALIGN (0x20)  :
		/* SD_HELLO   0xC000 : */
		{
			KEEP ( *.o (SD_HELLO))
		} 	> RAM

/*
		STR_HEAP		ALIGN (0x20)  :
		/* STR_HEAP	_String_HEAP :  /
		{
			_HEAP_init = .;
			KEEP ( *.o (HEAP))
			_HEAP_end = .;
		} 	> RAM
*/

	_end = .;

	}
