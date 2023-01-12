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
	PROVIDE( S2x			=	0x80 + _String_HEAP);  	/*Textbuf + Textbufsize */

	cur_resp_size	=		40;
	PROVIDE( cur_resp		=		0xC0 + _String_HEAP);		/*	; response from cursor */
	PROVIDE( T_BUFFER 		=		0xE0 + _String_HEAP);		/*;16-BYTE BUFFER */
	PROVIDE( COPY_BUFFER 	=		0x10 + _String_HEAP);		/*;16-BYTE BUFFER */

	PROVIDE( SP_value		=		0x20+ _String_HEAP);
	PROVIDE( reg16_value	=		0x24+ _String_HEAP);
	PROVIDE( num_chars		=		0x28+ _String_HEAP);
	PROVIDE( bufferSize		=		0x64		);						/*;SIZE OF INPUT BUFFER */
	PROVIDE( inBuffer		=		0x30+ _String_HEAP);			/*	;INPUT BUFFER */
	PROVIDE( inBufferEnd	=		0x30 + bufferSize + _String_HEAP);      		/*;END OF INPUT BUFFER */
	PROVIDE( outBufferSize	=		160	);							/*	;SIZE OF OUTPUT BUFFER */
	PROVIDE( outBuffer		=		0xA0 + _String_HEAP);	         	 	/*;OUTPUT BUFFER */
	PROVIDE( endOutBuffer	=		0xA0 + outBufferSize + _String_HEAP);	/*		END OF OUTPUT BUFFER */

	PROVIDE( list_len		=		0x48	);				/*	 length of command pointer list */
	PROVIDE( Comm_Ptr_list	=		0x200 + _String_HEAP); 	/*	 command pointer list */


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

		Samples	_RAMSTART :
		{
			KEEP( *.o(Samples))
			_MainSRam_end  = .;
		}	> RAM


		IOLIB	_IOLIB :
		{
			KEEP ( *.o (IOLIB))
			_IOLIB_end = .;
		} 	> RAM


		Functions	_FUNCTIONS :
		{
			KEEP ( *.o (Functions))
			_Funk_end = .;
		} 	> RAM


		SD_SPI_FUNC	_SD_SPI_LIB :
		{
			KEEP ( *.o (SD_SPI_FUNC))
			_SD_SPI_end = .;
		} 	> RAM

		SD_HELLO   0xC000 :
		{
			KEEP ( *.o (SD_HELLO))
		} 	> RAM

/*
		STR_HEAP	_String_HEAP :
		{
			_HEAP_init = .;
			KEEP ( *.o (HEAP))
			_HEAP_end = .;
		} 	> RAM
*/
		
	

	_end = .;

	}
