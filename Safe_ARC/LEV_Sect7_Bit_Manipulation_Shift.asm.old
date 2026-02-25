
Bit Manipulations and Shifts
7A    Bit Field Extraction    267
7B    Bit Field Insertion 270
7C    Multiple-Precision Arithmetic Shift Right      273
7D    Multiple-Precision Logical Shift Left 276
7E    Multiple-Precision Logical Shift Right   279
7F    Multiple-Precision Rotate Right 282
7G    Multiple-Precision Rotate Left   285


266
Bit Field Extraction (BFE)                                                                                                      7A

   Extracts a field of bits from a byte and                      the mask left to align it with the specified lowest
returns the field in the least significant bit posi-             bit position, and obtains the field by logically
tions. The width of the field and its lowest bit                 ANDing the mask with the data. It then normal-
position are parameters.                                         izes the bit field by shifting it right so that it
   Procedure: The program obtains a mask with                    starts in bit O.
the specified number of 1 bits from a table, shifts


        Registers Used: AF, Be, DE, HL                           field starting at bit 5, the program will return only 3
        Execution Time: 21 * LOWEST BIT POSITION                 bits (bits 5 through 7).
        plus 86 cycles overhead. (The lowest bit position
                                                                    2. Both the lowest bit position and the number of
        determines the number of times the mask must be
                                                                 bits in the field are interpreted mod 8. That is, for
        shifted left and the bit field right.)
                                                                 example, bit position 11 is equivalent to bit position 3
        Program Size: 32 bytes                                   and a field of 10 bits is equivalent to a field of 2 bits.
        Data Memory Required: None                               Note, however, that the number of bits in the field is
                                                                 interpreted in the range I to 8. That is, a field of 16
        Special cases:
                                                                 bits is equivalent to a field of 8 bits, not to a field of 0
           I. Requesting a field that would extend beyond        bits.
        the end ofthe byte causes the program to return with
        only the bits through bit 7. That is, no wraparound is     3. Requesting a field of width 0 causes a return
        provided. If, for example, the user asks for a 6-bit     with a result of O.




Entry Conditions                                                 Exit Conditions
Starting (lowest) bit position in the field                      Bit field in A (normallzed to bit 0)
  (0 to 7) in A
Number of bits in the field (1 to 8) in D
Data byte in E




Examples
I.    Data:    Data value = F6 16 '= 111101102                   2.    Data:     Data value = A216 = 101000102
               Lowest bit position = 4                                           Lowest bit position = 6
               Number of hits in the field = 3                                   Number of bits in the field = 5
     Result:   Bit field = 07 16 = 00000111 2                         Result:    Bit field = 02 16 = 00000010 2
               Three bits, starting at bit 4, have been ex-                      Two bits, starting at bit 6, have been ex-
                 tracted (that is, bits 4 through 6).                              tracted (that is, bits 6 and 7); that was all
                                                                                   that was available, although five bits were
                                                                                   requested.

                                                                                                                            267
268     BIT MANIPULATIONS AND SHIFTS




          Ti tle             Bit Field Extraction
          Name:              BFE



          Purpose:           Extract a field of bits from a byte and
                             return the field normalized to bit 0
                             NOTE: IF THE REQUESTED FIELD IS TOO LONG, THEN
                                   ONL Y THE BITS THROUGH BIT 7 WILL BE      ,
                                   RETURNED. FOR EXAMPLE, IF A 4-BIT FIELD IS;
                                   REQUESTED STARTING AT BIT 7, ONLY 1
                                   BIT (BIT 7) WILL BE RETURNED.
          Entry:             Register D        Number of bits in field (1 to 8)
                             Register E      = Data byte
                             Register A        Starting (lowest) bit position in
                                               the field (0 to 7)
          Exit:              Register A      = Field
          Registers used: AF,BC,DE,HL
          Time:              86 cycles overhead plus
                                 (21   *   lowest bit position) cycles
          Size:              Program 32 bytes


BFE:
          JSHIFT DATA TO NORMALIZE TO BIT 0
          J NO SHIFTING NEEDED IF LOWEST POSITION IS 0
          AND     00000111B       ;ONLY ALLOW POSITIONS 0 TO 7
          JR      Z,EXTR          ;JUMP IF NO SHIFTING NEEDED
          LD      B,A             ;MOVE SHIFT COUNT TO B
SHFT:
          SRL        E                     ;SHIFT DATA RIGHT
          DJNZ       SHFT                  ;CONTINUE UNTIL NORMALIZED
          ; EXTRACT FIELD BY MASKING WITH I-'S
EXTR:
          LD         A,D                   ;TEST NUMBER OF BITS FOR ZERO
          OR         A
          RET        Z                     ;EXIT IF NUMBER OF BITS = 0
                                           ;   FIELD IS 0 ON EXIT
          DEC        A                     ;DECREMENT A TO NORMALIZE TO 0
          AND        00000111B             ;ONLY ALLOW 0 THROUGH 7
          LD         C,A                   ;BC     INDEX INTO MASK ARRAY
          LD         B,O
          LD         HL,MSKARY             ;HL = BASE OF MASK ARRAY
          ADD        HL,BC
                                                  7A BIT FIELD EXTRACTION (BFE)   269
          LD      A,E             ;OET DATA
          AND     (HU             ;MASK OFF UNWANTED BITS
          RET
          ; MASK ARRAY WITH 1 TO 8 ONE BITS
MSKARY:
          DB      00000001B
          DB      00000011B
          DB      00000111B
          DB      00001111B
          DB      00011111B
          DB      00111111B
          DB      01111111B
          DB      111111118



          SAMPLE EXECUTION:


SC7A:
          LD      E,00011000B     ;REOISTER E = DATA
          LD      D,3             ; REGISTER D = NUMBER OF BITS
          LD      A,2             ; ACCUMULATOR = LOWEST BIT POS IT I ON
          CALL    BFE             ;EXTRACT 3 BITS STARTING WITH #2
          ,-IR    SC7A               RESULT   = 00000110B
          END
Bit Field Insertion (BFI)                                                                                                            7B

  Inserts a field of bits into a byte. The width of                 them with the specified lowest bit position. It
the field and its starting (lowest) bit position are                logically ANDs the mask with the original data
parameters.                                                         byte, thus clearing the required bit positions,
  Procedure: The program obtains a mask with                        and then logically ORs the result with the shifted
the specified number of 0 bits from a table. It                     bit field.
then shifts the mask and the bit field left to align


        Registers Used: AF, BC, DE, HL                              around is provided. If, for example, the user attempts
        Execution Time: 25 * LOWEST BIT POSITION                    to insert a 6-bit field starting at bit 4, only 4 bits (bits 4
        plus 133 cycles overhead. (The lowest bit position of       through 7) are actually replaced.
        the field determines how many times the mask and               2. Both the starting bit position and the width of
        the field must be shifted left.)                            the bit field (number of bits) are interpreted mod 8.
        Program Size: 40 bytes                                      That is, for example, bit position 11 is the same as bit
                                                                    position 3 and a 12-bit field is the same as a 4-bit field.
        Data Memory Required: None                                  Note, however, that the width of the field is mapped
        Special Coses:                                              into the range I to 8. That is, for example, a 16-bit
           1. Attempting to insert a field that would extend        field is the same as an 8-bit field.
        beyond the end of the byte causes the program to              3. Attempting to insert a field of width 0 causes a
        insert only the bits through bit 7. That is, no wrap-       return with a result of o.




Entry Conditions                                                    Exit Conditions
Data in A                                                           Result in A
Number of bits in the field (1 to 8) in B                           The result is the original data with the bit field
Starting (lowest) bit position of field in C                          inserted, starting at the specified bit position.
Field to insert in E




Examples
1.    Data:    Value = F6 16 = 111101102                            2.    Data:      Value = B8 16 = 101110002
               Lowest bit position = 4                                               Lowest bit position = I
               Number of bits in the field = 2                                       Number of bits in the field = 5
               Bit field = 01 16 = 00000001 2                                        Bit field = 15 16 = 00010101 2
     Result:   Value with bit field inserted =                           Result:     Value with bit field inserted = AA 16 = 101010102
                 D6 16 = 110101102                                                   The 5-bit field has been inserted into the origi-
               The 2-bit field has been inserted into the origi-                       nal value starting at bit I (into bits I through
                 nal value starting at bit 4 (into bits 4 and 5).                      5), changing 111002 (I C 16) to 10101 2 (15 16 ).


270
                                                        7B BIT FIELD INSERTION (BFI)   271


       Tit Ie              Bit Field Insertion
       Name:               BFI



       Purpose:            Insert a field of bits into a byte and return
                           the byte
                           NOTE: IF THE REQUESTED FIELD IS TOO LONG,
                                 ONLY THE BITS THROUGH BIT 7 WILL BE       :
                                 INSERTED. FOR EXAMPLE, IF A 4-BIT FIELD IS;
                                 TO BE INSERTED STARTING AT BIT 7 THEN
                                 ONLY 1 BIT (BIT 7) WILL BE INSERTED.
       Entry:              Register A    = Byte of data
                           Register B    = Number of bits in    the field     (1
                                           to 8)
                           Register    C = Starting (lowest)
                                                          bit position in
                                        which the data will be inserted
                                        (0 to 7)
                           Register E = Field to insert
       Exit:               Register A    = Dat.a
       Registers used: AF,BC,DE,HL
       Time:               133 cycles overhead plus
                             (25 * starting bit position) cycles
       Size:               Program 40 bytes


BFI:
       PUSH       AF                  ;SAVE DATA BYTE
       ; GET MASK WITH REQUIRED       NUMBER OF 0 BITS
       PUSH     BC                    ;SAVE STARTING BIT POSITION
       LD       HL.MSKARY
       LD       A.B                   ;GET NUMBER OF BITS
       AND      A                     ;TEST NUMBER OF BITS FOR 0
       RET      Z                     ;RETURN WITH 0 RESULT IF NUMBER
                                      ; OF BITS IS 0
       DEC        A                   ;NORMALIZE TO 0 ••• 7
       AND        00000111B           ;ONLY ALLOW 0 ••• 7
       LD         C.A
       LD         B,O
       ADD        HL,BC               ;INDEX INTO MASK ARRAY
       LD         D, (HU              ;D = MASK WITH ZEROS FOR CLEARING
       POP        BC                  ;RESTORE STARTING BIT
       ;TEST IF STARTING BIT IS 0
272      BIT MANIPULATIONS AND SHIFTS


          LD        A,C
          AND       00000111B           RESTRICT STARTING BIT TO 0 ••• 7
          .JR       Z,INSRT             .JUMP IF STARtING BIT IS 0
                                          NO ALIGNMENT IS NECESSARY
          JALIGN FIELD TO INSERT AND MASK IF STARTING BIT NON-ZERO
          LD      B,C             JB = STARTING BIT NUMBER
          LD      A, D            ; A = MASK
SFIELD:
          SLA       E                   JSHIFT FIELD LEFT TO INSERT
          RLCA                          JROTATE MASK
          D.JNZ     SFIELD              ;CONTINUE UNTIL ALIGNED
          LD        D,A
           ; INSERT FIELD
INSRT:
          POP       AF                  ;GET DATA BACK
          AND       D                   ;AND OFF MASK AREA
          OR        E                   fOR IN FIELD
          RET
          ; MASK ARRAY - 1 TO 8 ZERO BITS
MSKARY:
          DB        11111110B
          DB        11111100B
          DB        11111000B
          DB        11110000B
          DB        11100000B
          DB        11000000B
          DB        10000000B
          DB        OOOOOOOOB



          SAMPLE EXECUTION:


SC7B:
          LD        A,11111111B         ;REGISTER A = DATA
          LD        B,3                 ;REGISTER B = NUMBER OF BITS
          LD        C,2                 ;REGISTER C = LOWEST BIT POSITION
          LD        E,00000101B         ;REGISTER E = FIELD TO INSERT
          CALL      BFI                 ; INSERT 3-BIT FIELD STARTING AT
          .JR       SC7B                ; BIT 2. RESULT = 111101118
          END
Multiple-Precision Arithmetic Shift Right
(MPASR)                                                                                                                 7C

   Shifts a multi-byte operand right arithmeti-
                                                                      Registers Used: AF, BC, DE, HL
cally by a specified number of bit positions. The
                                                                      ExecutionTime:NUMBER OF SHIFTS * (46+
length of the operand (in bytes) is 255 or less.                      34 * LENGTH OF OPERANDS IN BYTES) +
The Carry flag is set from the last bit shifted out                   59 cycles
of the rightmost bit position. The operand is                         Program Size: 28 bytes
stored with its least significant byte at the lowest                  Data Memory Required: None
address.                                                              Special Cases:
   Procedure: The program obtains the sign bit                           I. If the length of the operand is 0, the program
from the most significant byte, saves that bit in                     exits immediately with the operand unchanged and
                                                                      the Carry flag cleared.
the Carry, and then rotates the entire operand
                                                                        2. If the number of shifts is 0, the program exits
right one bit, starting with the most significant                     immediately with the operand unchanged and the
byte. It repeats the operation for the specified                      Carry flag cleared.
number of shifts.




Entry Conditions                                                 Exit Conditions
Base address of operand in HL                                    Operand shifted right arithmetically by the spec-
Length of the operand in bytes in B                              ified number of bit positions. The original sign
Number of shifts (bit positions) in C                            bit is extended to the right. The Carry flag is set
                                                                 from the last bit shifted out of the rightmost bit
                                                                 position. Carry is cleared if either the number
                                                                 of shifts or the length of the operand is O.




Examples
I.    Data:    Length of operand (in bytes) = 08                 2.    Data:    Length of operand (in bytes) = 04
               Operand = 85A4C719FE06741E I6                                    Operand = 3F6A42D3 16
               Number of shifts = 04                                            Number of shifts = 03
     Result:   Shifted operand = F85A4C719FE06741 16                  Result:   Shifted operand = 07ED485A I6
               This is the original operand shifted right four                  This is the original operand shifted right three
                  bits arithmetically; the four most signifi-                     bits arithmetically; the three most signifi-
                 cant bits all take the value of the original                     cant bits all take the value of the original
                 sign bit (I).                                                    sign bit (0).
               Carry = I, since the last bit shifted from the                   Carry = 0, since the last bit shifted from the
                 rightmost bit position was I.                                    rightmost bit position was O.


                                                                                                                        273
274      BIT MANIPULATIONS AND SHIFTS




           Title              Multiple-Precision Arithmetic Shift Right
           Name:              MPASR



           Purpose:           Arithmetic shift right a multi-byte operand
                              N bits
           Entry:             Register pair HL = Base address of operand
                              Register B = Length of operand in bytes
                              Register C = Number of bits to shift
                                 The operand is stored with ARRAY[Ol as its
                                 least significant byte and ARRAY[LENGTH-ll
                                 its most significant byte, where ARRAY
                                 is its base address.
           Exit :             Operand shifted right with the most significant
                              bit propagated.
                              CARRY := Last bit shifted from least
                                       significant position.
           Registers used: AF,BC,DE,HL
           Time:               59 cycles overhead plus
                                «34     *   length) + 46) cycles per shift
           Size:              Program 28 bytes


MPASR:
           ;EXIT IF NUMBER OF SHIFTS OR LENGTH OF OPERAND IS 0
           ;OR CLEARS CARRY IN EITHER CASE
           LD      A,C
           OR      A
           RET     Z               ;RETURN IF NUMBER OF SHIFTS IS 0
           LD      A,B
           OR      A
           RET     Z               ;RETURN IF LENGTH OF OPERAND IS 0
           ;CALCULATE ADDRESS OF MOST SIGNIFICANT (LAST) BYTE
           LD      E, B            ; ADDRESS OF MSB = BASE + LENGTH-l
           LD      D,O
           ADD     HL,DE
           DEC     HL              ;HL = ADDRESS OF MSB
                                    ;C = NUMBER OF SHIFTS
                                   ;A = LENGTH OF OPERAND
           ;LOOP ON NUMBER OF SHIFTS TO PERFORM
           ;INITIAL CARRY = MOST SIGNIFICANT BIT OF ENTIRE OPERAND
                                7C MULTIPLE-PRECISION ARITHMETIC SHIFT RIGHT (MPASR)   275
LOOP:
         LD        B,(HL)          ;GET MOST SIGNIFICANT BYTE
         RL        B               ;CARRY = MOST SIGNIFICANT BIT
         LD        B,A
         LD        E,L             ;SAVE ADDRESS OF MSB
         LD        D,H
         ;ROTATE   BYTES RIGHT STARTING WITH MOST SIGNIFICANT
ASRLP:
         RR        (HU                 ;ROTATE A BYTE RIGHT
         DEC       HL                  ;DECREMENT TO LESS SIGNIFICANT BYTE
         D.JNZ     ASRLP
CONT:
         LD        L,E                 ;RESTORE ADDRESS OF MSB
         LD        H,D
         DEC       C                   ;DECREMENT NUMBER OF SHIFTS
         JR        NZ,LOOP
         RET



         SAMPLE EXECUTION:


SC7C:
         LD        HL,AY               ;BASE ADDRESS OF OPERAND
         LD        B,SZAY              ;LENGTH OF OPERAND IN BYTES
         LD        C,SHIFTS            ;NUMBER OF SHIFTS
         CALL      MPASR               ;SHIFT
                              ; RESULT OF SHIFTING EDCBA987654321H, 4 BITS IS
                                                    FEDCBA98765432H, C=O
                                  IN MEMORY AY   = 032H
                                            AY+l = 054H
                                            AY+2 = 076H
                                            AY+3 = 098H
                                            AY+4 = OBAH
                                            AY+5   ODCH
                                            AY+6   OFEH
         JR        SC7C
         ; DATA SECTION
SZAY     EQU      7       ;LENGTH OF OPERAND IN BYTES
SHIFTS   EQU      4       ; NUMBER OF SHIFTS
AY:      DB       21H,43H,65H, 87H,OA9H,OCBH, OEDH
         END
Multiple-Precision Logical Shift Left
(MPLSL)                                                                                                                       7D

   Shifts a multi-byte operand left logically by a                      Registers Used: AF, BC, DE
specified number of bit positions. The length of                        execution Time: NUMBER OF SHIFTS * (27+34.
the operand (in bytes) is 255 or less. The Carry                        LENGTH OF OPERAND IN BYTES) + 31 cycles
flag is set from the last bit shifted out of the                        Program Size: 21 bytes
leftmost bit position. The operand is stored with                       Data Memory Required: None
its least significant byte at the lowest address.                       Special Cases:
   Procedure: The program clears the Carry                                 I. If the length of the operand is 0, the program
                                                                        exits immediately with the operand unchanged and
initially (to fill 'Yith a 0 bit) and then shifts the                   the Carry flag cleared.
entire operand left one bit, starting with the least                      2. If the number of shifts is 0, the program exits
significant byte. It repeats the operation for the                      immediately with the operand unchanged and the
specified number of shifts.                                             Carry flag cleared.




Entry Conditions                                                   Exit Conditions
Base address of operand in HL                                      Operand shifted left logically by the specified
Length of operand in bytes in B                                    number of bit positions (the least significant bit
Number of shifts (bit positions) in C                              positions are filled with O's). The Carry flag is set
                                                                   from the last bit shifted out of the leftmost bit
                                                                   position. Carry is cleared if either the number of
                                                                   shifts or the length of the operand is o.




Examples
1.    Data:    Length of operand (in bytes) = 08                   2.     Data:    Length of operand (in bytes) = 04
               Operand = 85A4C719FE0674IE 16                                       Operand = 3F6A42D3 16
               Number of shifts = 04                                               Number of shifts = 03
     Result:   Shifted operand = 5A4C719FE06741 E0 16                    Result:   Shifted operand = FB521698 16
               This is the original operand shifted left four                      This is the original operand shifted left three
                 bits logically; the four least significant bits                     bits logically; the three least significant bits
                 are all cleared.                                                    are all cleared.
               Carry = 0, since the last bit shifted from the                      Carry = I, since the last bit shifted from the
                 leftmost bit position was O.                                        leftmost bit position was I.


276
                                7D MULTIPLE-PRECISION LOGICAL SHIFT IHT (MPLSL)   277



         Tit Ie           Multiple-Precision Logical Shift Left
         Name:            MPLSL



         Purpose:         Logical shift left a multi-byte operand
                          N bits
         Entry:           Register pair HL = Base address of operand
                          Register B = Length of operand in bytes
                          Register C = Number of bits to shift
                            The operand is stored with ARRAY[OJ as its
                            least significant byte and ARRAY[LENGTH-1J
                            its most significant byte, where ARRAY
                            is its base address.
         Exit:            Operand shifted left filling the least
                          significant bits with zeros
                          CARRY := Last bit shifted from
                            most significant position
         Registers used: AF,BC,DE
         Time:             31 cycles overhead plus
                           «34 * length) + 27) cycles per shift
         Size:            Program 21 bytes


MPLSL:
         ;EXIT IF NUMBER OF SHIFTS OR LENGTH OF OPERAND IS 0
         ;OR CLEARS CARRY IN EITHER CASE
         LD      A,C
         OR      A
         RET     Z               ; RETURN IF NUMBER OF SHIFTS IS 0
         LD      A,B
         OR      A
         RET     Z               ;RETURN IF LENGTH OF OPERAND IS 0
         ;LOOP ON NUMBER OF SHIFTS TO PERFORM
         ;A = LENGTH OF OPERAND
         ;C = NUMBER OF SHIFTS
         ;HL = ADDRESS OF LEAST SIGNIFICANT (FIRST) BYTE OF OPERAND
         ;CARRY = 0 INITIALLY FOR LOGICAL SHIFT
LOOP:
         LD         E,L             ;SAVE ADDRESS OF LSB
         LD         D,H
         LD         B,A             ;B = LENGTH OF OPERAND
         OR         A               ;CLEAR CARRY FOR LOGICAL SHIFT
278      BIT MANIPULATIONS AND SHIFTS


           ;ROTATE BYTES STARTING WITH LEAST SIGNIFICANT
LSLLP:
          RL        (HU                 ;ROTATE NEXT BYTE LEFT
          INC       HL                  ; INCREMENT TO MORE SIGNIFICANT BYTE
          DJNZ      LSLLP
          LD        L.E                 ;RESTORE ADDRESS OF LSB
          LD        H.D
          DEC       C                   ;DECREMENT NUMBER OF SHIFTS
          JR        NZ.LOOP
          RET



          SAMPLE EXECUTION:


SC7D:
          LD        HL.AY               ;HL = BASE ADDRESS OF OPERAND
          LD        B.SZAY              ;B = LENGTH OF OPERAND IN BYTES
          LD        C.SHIFTS            ;C = NUMBER OF SHIFTS
          CALL      MPLSL               ;SHIFT
                               ; RESULT OF SHIFTING EDCBA987654321H. 4 BITS IS
                                                     DCBA9876543210H. C=O
                                   IN MEMORY AY   = 010H
                                             AY+l   032H
                                             AY+2 = 054H
                                             AY+3 = 076H
                                             AY+4 = 098H
                                             AY+5 = OBAH
                                             AY+6 = ODCH
          JR        SC7D
          ; DATA SECTION
SZAY      EQU      7        ;LENGTH OF OPERAND IN BYTES
SHIFTS    EQU      4        ; NUMBER OF SHIFTS
AY:       DB       21H.43H.65H.87H.OA9H.OCBH.OEDH
          END
Multiple-Precision Logical Shift Right
(MPLSR)                                                                                                                      7E

   Shifts a multi-byte operand right logically by
                                                                       Registers Used: AF, BC, DE, HL
a specified number of bit positions. The length
                                                                       Execution Time: NUMBEROFSHIFTS *(35+34.
of the operand (in bytes) is 255 or less. The                          LENGTH OF OPERAND IN BYTES)+ 59 cycles
Carry flag is set from the last bit shifted out of                     Program Size: 26 bytes
the rightmost bit position. The operand is stored                      Data Memory Required: None
with its least significant byte at the lowest                          Special cases:
address.                                                                  I. If the length of the operand is 0, the program
   Procedure: The program clears the Carry                             exits immediately with the operand unchanged and
initially (to fill with a 0 bit) and then shifts the                   the Carry flag cleared.

entire operand right one bit, starting with the                           2. If the number of shifts is 0, the program exits
                                                                       immediately with the operand unchanged and the
most significant byte. It repeats the operation                        Carry flag cleared.
for the specified number of shifts.




Entry Conditions                                                  Exit Conditions
Base address of operand in HL                                     Operand shifted right logically by the specified
Length of operand in bytes in B                                     number of bit positions. (The most significant
Number of shifts (bit positions) in C                               bit positions are filled with O's.)
                                                                  The Carry flag is set from the last bit shifted out
                                                                    of the rightmost bit position. Carry is cleared
                                                                    if either the number of shifts or the length of
                                                                    the operand is O.




Examples
1.    Data:    Length of the operand (in bytes) = 08              2.     Data:    Length of operand (in bytes) = 04
               Operand = 85A4C7l9FE0674lE 16                                      Operand = 3F6A42D3 16
               Number of shifts = 04                                              Number of shifts = 03
     Result:   Shifted operand = 085A4C7l9FE06741 16                   Result:    Shifted operand = 07ED485A 16
               This is the original operand shifted right                         This is the original operand shifted right three
                 four bits logically; the four most significant                     bits logically; the three most significant bits
                 bits are all cleared.                                              are all cleared.
               Carry = I, since the last bit shifted from the                     Carry = 0, since the last bit shifted from the
                 rightmost bit position was I.                                      rightmost bit position was O.


                                                                                                                          279
280      BIT MANIPULATIONS AND SHIFTS




          Title               Multiple-Precision Logical Shift Right
          Name:               MPLSR



           Purpose:           Logical shift right a multi-byte operand N bits
           Entry:             Register pair HL = Base address of operand
                              Register B = Length of operand in bytes
                              Register C = Number of bits to shift
                                The operand is stored with ARRAY[O] as its
                                least significant byte and ARRAY[LENGTH-IJ
                                its most significant byte, where ARRAY
                                is its base address.
          Exit :              Operand shifted right filling the most
                              significant bits with zeros
                              CARRY := Last bit shifted from least
                                       significant position
           Registers used: AF,BC,DE,HL
           Time:               59 cycles overhead plus
                               «34 * lengt.h) + 35) cycles per shift

          Size:               Program 26 bytes


MPLSR:
           ;EXIT IF NUMBER OF SHIFTS OR LENGTH OF OPERAND IS 0
           ;OR CLEARS CARRY IN EITHER CASE
           LD      A,C
           OR      A
           RET     Z               ; RETURN I F NUMBER OF SH I FTS ISO
           LD      A,B
           OR      A
           RET     Z               ; RETURN IF LENGTH OF OPERAND IS 0

          ;CALCULATE ADDRESS OF MOST SIGNIFICANT (LAST) BYTE
          LD      E,B             ;ADDRESS OF MSB = BASE+LENGTH-l
          LD      D,O
          ADD     HL,DE
          DEC     HL              ;HL = ADDRESS OF MSB
                                  ;C = NUMBER OF SHIFTS
                                  ;A = LENGTH OF OPERAND
          ;LOOP ON NUMBER OF SHIFTS TO PERFORM
          ;START WITH CARRY = 0 FOR LOGICAL SHIFT
                                    7E MULTIPLE~PRECISION LOGICAL SHIFT RIGHT (MPLSR)   281
LOOP:
         OR         A               ;CLEAR CARRY FOR LOGICAL SHIFT
         LD         B,A             ;B = LENGTH OF OPERAND
         LD         E,L             ;SAVE ADDRESS OF MSB
         LD         D,H
         ; ROTATE   BYTES STARTING WITH MOST SIGNIFICANT
LSRLP:
         RR         (HU                ;ROTATE A BYTE RIGHT
         DEC        HL                 ;DECREMENT TO LESS SIGNIFICANT BYTE
         D,JNZ      LSRLP
         LD         L,E                ;RESTORE ADDRESS OF MSB
         LD         H,D
         DEC        C                  ;DECREMENT NUMBER OF SHIFTS
         ,JR        NZ,LOOP
         RET



         SAMPLE EXECUTION:


SC7E:
         LD         HL,AY        ;HL = BASE ADDRESS OF OPERAND
         LD         B,SZAY       ;B = LENGTH OF OPERAND IN BYTES
         LD         C,SHIFTS     ;C = NUMBER OF SHIFTS
         CALL       MPLSR        ;SHIFT
                               ; RESULT OF SHIFTING EDCBA987654321H, 4 BITS IS
                                                     OEDCBA98765432H, C=O
                                   IN MEMORY AY     032H
                                             AY+l = 054H
                                             AY+2   076H
                                             AY+3 = 098H
                                             AY+4   OBAH
                                             AY+5   ODCH
                                             AY+6 = OOEH
         ,JR        SC7E
         ; DATA SECTION
SZAY     EQU      7        ;LENGTH OF OPERAND IN BYTES
SHIFTS   EQU      4        ;NUMBER OF SHIFTS
AY:      DB       21H,43H,65H,87H,OA9H,OCBH,OEDH
         END
Multiple-Precision Rotate Right (MPRR)                                                                                        7F

    Rotates a multi-byte operand right by a spec-
ified number of bit positions as if the most signif-                    Registers Used: AF, BC. DE, HL, IX
icant bit and least significant bit were connected.                     Execution Time: NUMBER OF ROTATES * (58 +
                                                                        34 * LENGTH OF OPERAND IN BYTES) + 83
The length of the operand (in bytes) is 255 or                          cycles
less. The Carry flag is set from the last bit shifted                   Program Size: 33 bytes
out of the rightmost bit position. The operand is                       Data Memory Required: None
stored with its least significant byte at the lowest                    Special Cases:
address.                                                                   I. If the length of the operand is 0, the program
   Procedure: The program shifts bit 0 of the                           exits immediately with the operand unchanged and
least significant byte of the operand to the Carry                      the Carry flag cleared.
flag and then rotates the entire operand right                            2. If the number of rotates is 0, the program exits
                                                                        immediately with the operand unchanged and the
one bit, starting with the most significant byte. It                    Carry flag cleared.
repeats the operation for the specified number
of rotates.




Entry Conditions                                                   Exit Conditions
Base address of operand in HL                                      Operand rotated right logically by the specified
Length of operand in bytes in B                                    number of bit positions (the most significant bit
Number of rotates (bit positions) in C                             positions are filled from the least significant bit
                                                                   positions). The Carry flag is set from the last bit
                                                                   shifted out of the rightmost bit position. Carry is
                                                                   cleared if either the number of rotates or the
                                                                   length of the operand is O.



Examples
I.    Data:    Length of operand (in bytes) = 08                   2.    Data:     Length of operand (in bytes) = 04
               Operand = 85A4C7l9FE06741E'6                                        Operand = 3F6A42D3'6
               Number of rotates = 04                                              Number of rotates = 03
     Result:   Rotated operand = E85A4C719FE06741'6                     Result:    Rotated operand = 67ED485A'6
               This is the original operand rotated right four                     This is the original operand rotated right
                 bits; the four most significant bits are equiv-                     three bits; the three most significant bits are
                 alent to the original four least significant                        equivalent to the original three least signif-
                 bits.                                                               icant bits.
               Carry = I, since the last bit shifted from the                      Carry = 0, since the last bit shifted from the
                 rightmost bit position was I.                                       rightmost bit position was O.


282
                                     7F MULTIPLE-PRECISION ROTATE RIGHT (MPRR)   283



        Title            Multiple-Precision Rotate Right
        Name:            MPRR



        Purpose:         Rotate right a multi-byte operand N bits
        Entry:           Register pair HL = Base address of operand
                         Register B = Length of operand in bytes
                         Register C = Number of bits to rotate
                           The operand is stored with ARRAY[O] as its
                           least significant byte and ARRAY[LENGTH-l]
                           its most significant byte, where ARRAY
                           is its base address.
        Exit:           Operand rotated right
                        CARRY := Last bit shifted from least
                                 significant position
        Registers used: AF,BC,DE,HL,IX
        Time:             83 cycles overhead plus
                          «34  * length) + 58) cycles per rotate
        Size:            Program 33 bytes


MPRR:
        ;EXIT IF NUMBER OF ROTATES OR LENGTH OF OPERAND IS
        ;OR CLEARS CARRY IN EITHER CASE
                                                                 °
        LD      A,C
        OR
        RET
        LD
                A
                Z
                A,B
                                ;RETURN IF NUMBER OF ROTATES IS        °
        OR      A
        RET     Z               ; RETURN IF LENGTH OF OPERAND IS        0

        ;CALCULATE ADDRESS OF MOST SIGNIFICANT (LAST) BYTE
        PUSH    HL
        POP     IX               ;IX POINTS TO LSB (FIRST BYTE)
        LD      E,B              ; ADDRESS OF MSB = BASE + LENGTH-l
        LD      D,O
        ADD     HL.DE
        DEC     HL              ;HL POINTS TO MSB (LAST BYTE)
                                ;C = NUMBER OF ROTATES
                                 ;A = LENGTH OF OPERAND
        ;LOOP ON NUMBER OF ROTATES TO PERFORM
        ;CARRY = LEAST SIGNIFICANT BIT OF ENTIRE OPERAND
284      BIT MANIPULATIONS AND SHIFTS


LOOP:
           LD         B,(IX+O)            ;GET LSB
           RR         B                   ; CARRY = BIT 0 OF LSB
           LD         B,A                 ;B = LENGTH OF OPERAND IN BYTES
           LD         E,L                 ;SAVE ADDRESS OF MSB
           LD         D,H
           ; ROTATE   BYTES RIGHT STARTING WITH MOST SIGNIFICANT
RRLP:
           RR         (HU                 ;ROTATE A BYTE RIGHT         I
           DEC        HL                  ;DECREMENT TO LESS SIGNIFICANT BYTE
           D.JNZ      RRLP
           LD         L,E                 ;RESTORE ADDRESS OF MSB
           LD         H,D
           DEC        C                   ;DECREMENT NUMBER OF ROTATES
           •.JR       NZ,LOOP
           RET




           SAMPLE EXECUTION:


SC7F:
           LD         HL,AY               ;BASE ADDRESS OF OPERAND
           LD         B,SZAY              ;LENGTH OF OPERAND IN BYTES
           LD         C,ROTATS            ;NUMBER OF ROTATES
           CALL       MPRR     ; ROTATE
                               ;RESULT OF ROTATING EDCBA987654321H, 4 BITS IS
                                                   lEDCBA98765432H, c=o
                                  IN MEMORY AY   = 032H
                                            AY+l = 054H
                                            AY+2   076H
                                            AY+3   098H
                                            AY+4   OBAH
                                            AY+5   ODCH
                                            AY+6   01EH
           ..JR       SC7F
           ; DATA SECTION
SZAY       EQU      7        ;LENGTH OF OPERAND IN BYTES
ROTATS     EQU      4        ;NUMBER OF ROTATES
AY:        DB       21H,43H,65H,87H,OA9H,OCBH,OEDH
           END
Multiple-Precision Rotate Left (MPRL)                                                                                         7G

   Rotates a multi-byte operand left by a speci-
                                                                         Registers Used: AF, BC, DE, HL, IX
fied number of bit positions as if the most signif-
                                                                         Execution Time: NUMBER OF ROTATES * (58 +
icant bit and least significant bit were connected.                      34 * LENGTH OF OPERAND IN BYTES) + 104
The length of the operand (in bytes) is 255 or                           cycles
less. The Carry flag is set from the last bit shifted                    Program Size: 35 bytes
out of the leftmost bit position. The operand is                         Data Memory Required: None
stored with its least significant byte at the lowest                     Special cases:
address.                                                                    I. If the length of the operand is 0, the program
   Procedure: The program shifts bit 7 of the                            exits immediately with the operand unchanged and
                                                                         the Carry flag cleared.
most significant byte of the operand to the
                                                                           2. If the number of rotates is 0, the program exits
Carry flag. It then rotates the entire operand left                      immediately with the operand unchanged and the
one bit, starting with the least significant byte. It                    Carry flag cleared.
repeats the operation for the specified number
of rotates.




Entry Conditions                                                    Exit Conditions
Base address of operand in HL                                       Operand rotated left the specified number of bit
Length of operand in bytes in B                                     positions (the least significant bit positions are
Number of rotates (bit positions) in C                              filled from the most significant bit positions).
                                                                    The Carry flag is set from the last bit shifted out
                                                                    of the leftmost bit position. Carry is cleared if
                                                                    either the number of rotates or the length of the
                                                                    operand is O.



Examples
I.    Data:    Length of operand (in bytes) = 08                    2.    Data:    Length of operand (in bytes) = 04
               Operand = 85A4C7I9FE06741E 16                                       Operand = 3F6A42D3 16
               Number of rotates = 04                                              Number of rotates = 03
     Result:   Rotated operand = 5A4C7I9FE06741E8 16                     Result:   Rotated operand = FB521699 16
               This is the original operand rotated left four                      This is the original operand rotated left three
                 bits; the four least significant bits are equiv-                    bits; the three least significant bits are equiv-
                 alent to the original four most significant                         alent to the original three most significant
                 bits.                                                               bits.
               Carry = 0, since the last bit shifted from the                      Carry = I, since the last bit shifted from the
                 leftmost bit position was O.                                        leftmost bit position was I.


                                                                                                                             285
286     BIT MANIPULATIONS AND SHIFTS




         Tit Ie             Multiple-Precision Rotate Left
         Name:              MPRL



         Purpose:           Rotate left a multi-byte operand N bits
         Entry:              Register pair HL = Base address of operand
                             Register B = Length of operand in bytes
                             Register C = Number of bits to rotate
                               The operand is stored with ARRAY[Ol as its
                               least significant byte and ARRAY[LENGTH-ll
                               its most significant byte, where ARRAY
                               is its base address.
         Exit:               Operand rotated left
                             CARRY := Last bit shifted from most
                                      significant position
         Registers used: AF,BC,DE,HL,IX
         Time:                104 cycles overhead plus
                              «34 * length) + 58) cycles per rotate
         Size:               Program 35 bytes


MPRL:
         ,EXIT IF NUMBER OF ROTATES OR LENGTH OF OPERAND IS 0
         ;OR CLEARS CARRY IN EITHER CASE
         LD      A,C
         OR      A
         RET     Z               ,RETURN IF NUMBER OF ROTATES IS 0
         LD      A,B
         OR      A
         RET     Z               ;RETURN IF LENGTH OF OPERAND IS 0
         ; CALCULATE ADDRESS OF MOST SIGNIFICANT (LAST) BYTE
         PUSH     HL              ;SAVE ADDRESS OF FIRST BYTE
         LD       E,B             ; ADDRESS OF MSB = BASE + LENGTH-l
         LD       0,0
         ADD      HL.DE
         DEC      HL
         PUSH     HL
         POP      IX              ;IX POINTS TO MOST SIGNIFICANT BYTE
         POP      HL              ;HL POINTS TO LEAST SIGNIFICANT BYTE
                                  ;C = NUMBER OF ROTATES
                                  ;A = LENGTH OF OPERAND
         ;LOOP ON NUMBER OF ROTATES TO PERFORM
                                            7G MULTIPLE-PRECISION ROTATE LEFT (MPRL)   287
         ;CARRY     = MOST   SIGNIFICANT BIT OF ENTIRE OPERAND
LOOP:
         LD         B, <IX+O)           ;GET MOST SIGNIFICANT BYTE
         RL         B                   ;CARRY = BIT 7 OF MSB
         LD         B,A                 ;B = LENGTH OF OPERAND IN BYTES
         LD         E,L                 ;SAVE ADDRESS OF LSB
         LD         D,H
         ; ROTATE   BYTES LEFT STARTING WITH LEAST SIGNIFICANT
RLLP:
         RL          (HU                ;ROTATE A BYTE LEFT
         INC        HL                  ; INCREMENT TO MORE SIGNIFICANT BYTE
         DJNl       RLLP
         LD         L,E                 ;RESTORE ADDRESS OF LSB
         LD         H,D
         DEC        C                   ,DECREMENT NUMBER OF ROTATES
         JR          Nl,LOOP
         RET



         SAMPLE EXECUTION:


SC7G:
         LD         HL,AY               ;HL = BASE ADDRESS OF OPERAND
         LD         B,SlAY              ;B = LENGTH OF OPERAND IN BYTES
         LD         C,ROTATS            ;C = NUMBER OF ROTATES
         CALL       MPRL     ; ROTATE
                             ,RESULT    OF ROTATING     EDCBA987654321H, 4 BITS IS
                                                        DCBA987654321EH, C=O
                                  IN MEMORY AY     =   01EH
                                            AY+l   =   032H
                                            AY+2   =   054H
                                            AY+3   =   076H
                                            AY+4   =   0981-4
                                            AY+5       OBAH
                                            AY+6   =   ODCH
         JR         SC7G
         ;DATA SECTION
SlAY     EQU     7        ;LENGTH OF OPERAND IN BYTES
ROTATS   EQU     4        ;NUMBER OF ROTATES
AY:      DB      21H,43H,65H,87H,OA9H,OCBH,OEDH
         END
