;****************************************************************************
;
;    Copyright (C) 2021,2022 John Winans
;
;    This library is free software; you can redistribute it and/or
;    modify it under the terms of the GNU Lesser General Public
;    License as published by the Free Software Foundation; either
;    version 2.1 of the License, or (at your option) any later version.
;
;    This library is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public
;    License along with this library; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
;    USA
;
;****************************************************************************

		include 	"Z80_Params_.inc"


	section SD_HELLO
		; org	LOAD_BASE		; the second-stage load address

	xref	printSTRBelow, hexdump_a, gpio_out, gpio_in, 	

	ld	sp,LOAD_BASE

	; XXX Note that the boot loader should have initialized the SIO, CTC etc.
	; XXX Therefore we can just write to them from here.

	ld	de,0
.loop:
	; Display a hello world message.
	inc	de
	ld	a,d
	call	hexdump_a
	ld	a,e
	call	hexdump_a
	call	printSTRBelow
	db	": Hello from the SD card!!!\r\n"
	db	0			; DON'T FORGET the null terminator!

	; waste some time
	ld	hl,0
.dly:
	dec	hl
	ld	a,h
	or	l
	jp	z,.loop			; if done, go back & print again
	jp	.dly

	; ...we never get here

