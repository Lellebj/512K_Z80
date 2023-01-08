	/*
	*------------------------------------------------------------
	*                                  ___ ___ _
	*  ___ ___ ___ ___ ___       _____|  _| . | |_
	* |  _| . |_ -|  _| . |     |     | . | . | '_|
	* |_| |___|___|___|___|_____|_|_|_|___|___|_,_|
	*                     |_____|     firmware v1.2
	* ------------------------------------------------------------
	* Copyright (c)2020 Ross Bamford
	* See top-level LICENSE.md for licence information.
	*
	* Link script for code that is loaded by the serial-receive
	* boot loader.
	* ------------------------------------------------------------
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
	PROVIDE(_FIRMWARE       = 0x00FC0000);  /* firmware address             */
	PROVIDE(_FIRMWARE_REV   = 0x00FC0400);  /* firmware revision code       */

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
