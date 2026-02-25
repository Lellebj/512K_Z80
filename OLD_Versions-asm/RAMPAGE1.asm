; memcpy --
 ; Copy a block of memory from one location to another.
 ;
 ; Entry registers
 ;      BC - Number of bytes to copy
 ;      DE - Address of source data block
 ;      HL - Address of target data block
 ;
 ; Return registers
 ;      BC - Zero
#target bin


#code   RAM_PAGE1,0xE000, *      ; basic ram               ;RAM mem at E000h
            ld ix,  $E050
            ld d,   #00
            ld b,   #32
            ld a,   $FF
loop1:       ld      (IX+0), d   ;reg b till RAM
            inc d         
            inc IX           ;
            cp d        ; d==255 ??    (Z set)
            jr NZ, loop2       ;Repeat the loop

            ld      (IX+0), b   ;spara sista data $FF
            ld d,   #00
            djnz loop2
        .text       "_0123456789ABCDEF0"

#code   RAM_PAGE2,0xE040, *      ; basic ram               ;RAM mem at E000h
            ld ix,  $E050
            ld d,   #00
            ld b,   #32
            ld a,   $FF
loop2:       ld      (IX+0), d   ;reg b till RAM
            inc d         
            inc IX           ;
            cp d        ; d==255 ??    (Z set)
            jr NZ, loop2       ;Repeat the loop

            ld      (IX+0), b   ;spara sista data $FF
            ld d,   #00
            djnz loop2
        .text       "_0123456789ABCDEF1"

#code   RAM_PAGE3,0xE080, *      ; basic ram               ;RAM mem at E000h
            ld ix,  $E050
            ld d,   #00
            ld b,   #32
            ld a,   $FF
loop3:       ld      (IX+0), d   ;reg b till RAM
            inc d         
            inc IX           ;
            cp d        ; d==255 ??    (Z set)
            jr NZ, loop2       ;Repeat the loop

            ld      (IX+0), b   ;spara sista data $FF
            ld d,   #00
            djnz loop2
        .text       "_0123456789ABCDEF2"
  
    .end


