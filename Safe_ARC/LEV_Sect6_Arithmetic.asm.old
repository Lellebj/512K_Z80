
        ; Arithmetic
        ; 6A      16-Bit Multiplication 217
        ; 6B      16-Bit Division 220
        ; 6C      16-Bit Comparison     225
        ; 6D      Multiple-Precision Binary Addition         228
        ; 6E      Multiple-Precision Binary Subtraction 231
        ; 6F      Multiple-Precision Binary Multiplication 234
        ; 6G      Multiple-Precision Binary Division         239

        ; 6H    Multiple-Precision Binary Comparison 245
        ; 61    Multiple-Precision Decimal Addition 248
        ; 6J    Multiple-Precision Decimal Subtraction 251
        ; 6K    Multiple-Precision Decimal Multiplication      254
        ; 6L    Multiple-Precision Decimal Division 260
        ; 6M    Multiple-Precision Decimal Comparison 266

16·Bit Multiplication (MUL16)                                                                                 6A

   Multiplies two 16-bit operands and returns
                                                          Registers Used: AF. Be, DE, HL
the less significant (l6-bit) word of the product.
                                                          Execution Time: Approximately 865 to 965 cycles,
   Procedure: The program uses an ordinary                depending largely on the number of I bits in the
shift-and-add algorithm, adding the multipli-             multiplier
cand to the partial product each time it finds a 1        Program Size: 22 bytes
bit in the multiplier. The partial product and the        Data Memory Required: None
mUltiplier are shifted left 15 times (the number
of bits in the multiplier minus 1) to produce
proper alignment. The more significant 16 bits
of the product are lost.




Entry Conditions                                     Exit Conditions
Multiplicand in HL                                   Less significant word of product in HL
Multiplier in DE



Examples
1.    Data:       Multiplier = 0012 16               2.    Data:     Multiplier = 3701 16
                  Multiplicand = 0301 16                             Multiplicand = A045 16
     Result:     Product = 44B2 16                        Result:    Product = AB55 16
                 The more significant word is O.                     This is actually the less significant 16-bit word
                                                                     of the 32-bit product 22FIAB55 16 •
   Note that MULl6 returns only the less signif-     bility with other 16-bit arithmetic operations.
icant word of the product to maintain compati-       The more significant word of the product is lost.




               Title                  16-bit Multiplication
               Name:                  MUL16



               Purpose:               Multiply 2 signed or unsigned 16-bit words and
                                      return a 16-bit signed or unsigned product

                                                                                                             217
218       ARITHMETIC



                                 Answers needing more than 16 bits: bits higher
                                 than bit 15 are lost
           Entry:                Register   L = Low byte of multiplicand
                                 Register   H = High byte of multiplicand
                                 Register   E = Low byte of mul t ipl iet-
                                 Register   D = High byte of multiplier
           Exit :                Product = multiplicand * multiplier
                                 Register L = Low byte of product
                                 Register H = High byte of product
           Registers used: AF.BC,DE,HL
           Time:                 Approximately 865 to 965 cycles
           Size:                 Program    22 bytes


           : INITIALIZE PARTIAL PRODUCT. BIT COUNT
MUL16:
           LD          C,L                  : BC = MULTIPLIER
           LD          8,H
           LD          HL.O                 :PRODUCT = 0
           LD          A.15                 : COUNT = BIT LENGTH - 1
           :SHIFT-AND-ADD ALGORITHM
             IF MSB OF MULTIPLIER IS 1 r ADD MULTIPLICAND TO PARTIAL
           :    PRODUCT
           : SHIFT PARTIAL PRODUCT. MULTIPLIER LEFT 1 BIT
HLP:
           SLA         E                    : SHIFT MULTIPLIER LEFT 1 BIT
           RL          0
           JR          NC.MLP1              ;JUMP IF MS8 OF MULTIPLIER = 0
           ADD         HL.BC                ,ADD MULTIPLICAND TO PARTIAL PRODUCT
HLP 1 :
           ADD         HL.HL                ,SHIFT PARTIAL PRODUCT LEFT
           DEC         A
           JR          NZ.MLP               ;CONTINUE UNTIL COUNT   =0
           : ADD MULTIPLICAND ONE LAST TIME IF MSB OF MULTIPLIER IS 1
           OR          D                    SIGN FLAG = MSB OF MULTIPLIER
           RET         P                    EXIT IF MSB OF MULTIPLIER IS 0
           ADD         HL,BC                ADD MULTIPLICAND TO PRODUCT
           RET



           SAMPLE EXECUTION:
                                    6A 16-81T MULTIPLICATION (MUL 16)   219
SC6A:
        LD     HL,-2     ;HL = MULTIPLICAND
        LD     DE,1023   ;DE = MULTIPLIER
        CALL   MUL16     ; 16-BIT MULTIPLY
                         ;RESULT OF 1023 * -2   = -2046 = OFS02H
                            REGISTER L = 02H
                                     H = F8H
        JR     SC6A
        END
16·Bit Division (SDIV16, UDIV16)                                                                              68

   Divides two 16-bit operands and returns the
quotient and the remainder. There are two entry            Registers Used: AF, BC. DE. HL
points: SDIVI6 divides two 16-bit signed oper-             Execution Time: Approximately 1770 to 2340 cycles,
ands, whereas UDIVI6 divides two 16-bit un-                depending largely on how many trial subtractions are
                                                           successful and thus require the replacement of the
signed operands. If the divisor is 0, the Carry            previous dividend by the remainder
flag is set to I and both quotient and remainder           Program Size: 104 bytes
are set to 0; otherwise, the Carry flag is cleared.        Data Memory Required: 3 bytes anywhere in
   Procedure: If the operands are signed, the              RAM for the sign of the quotient (address SQUaT),
program determines the sign of the quotient and            the sign of the remainder (address SREM), and a
                                                           divide loop counter (address COUNT)
takes the absolute values of any negative oper-
                                                           Special Case: If the divisor is 0, the program
ands. It must also retain the sign of the dividend,        returns with the Carry set to 1, and both the quotient
since that determines the sign of the remainder.           and the remainder set to O.
The program then performs an unsigned division
using a shift-and-subtract algorithm. It shifts
the quotient and dividend left, placing a I bit in
the quotient each time a trial subtraction is         Carry flag is cleared if the division is proper and
successful. If the operands are signed, the program   set if the divisor is O. A 0 divisor also causes a
must negate (that is, subtract from 0) the            return with the quotient and remainder both sct
quotient or remainder if either is negative. The      to O.



Entry Conditions                                      Exit Conditions
Dividend in HL                                        Quotient in HL
Divisor in DE                                         Remainder in DE
                                                      If the divisor is non-zero, Carry = 0 and the
                                                         result is normal.
                                                      If the divisor is 0, Carry = I and both quotient
                                                         and remainder are 0000.




Examples
I.    Data:    Dividend = 03E0 16                     2.    Data:    Dividend = D73A 16
               Divisor = 00B6 16                                     Divisor = 02F 116
     Result:   Quotient (from UDIVI6) = 0005 16            Result:   Quotient (from SDIV16) = FFF3 16
               Remainder (from UDIV16) = 0052 16                     Remainder (from SDIV16) = FD77 16
               Carry = 0 (no divide-by-O error)                      Carry = 0 (no divide-by-O error)


220
                                                             6B 16-BIT DIVISION (SDIV16, UDIV16)   221
   The remainder of a signed division may be            1 from the quotient and add the divisor to the
either positive or negative. In this procedure, the     remainder. The result of Example 2 is then
remainder always takes the sign of the dividend.         Quotient = FFF216 = -14 10
A negative remainder can easily be converted             Remainder (always positive) = 0068 16
into one that is always positive. Simply subtract




          Title                  16-bit Division
          Name:                  SDIV16, UDIV16




          Purpose:               SDIV16
                                   Divide 2 signed 16-bit words and return a
                                   16-bit *igned quotient and remainder

                                 UDIV16
                                   Divide 2 unsigned 16-bit words and return a
                                   16-bit unsigned quotient and remainder

          Entry:                 Register    L        Low byte of dividend
                                 Register    H        High byte of dividend
                                 Register    E        Low byte of divisor
                                 Register    D        High byte of divisor
          Exit:                  Register    L        Low byte of quotient
                                 Register    H        High byte of quotient
                                 Register    E        Low byte of remainder
                                 Register    D   =    High byte of remainder

                                 If no errors then
                                   carry := 0
                                 else
                                   divide-by-zero error
                                   carry := 1
                                   quotient := 0
                                   remainder := 0

          Registers used: AF,BC,DE,HL

          Time:                  Approximately 1770 to 2340 cycles
          Size:                  Program 108 bytes
                                 Data      3 bytes
222      ARITHMETIC


          ,SIGNED DIVISION
SDIV16:
          JDETERMINE SIGN OF QUOTIENT BY EXCLUSIVE ORING HIGH BYTES
          J OF DIVIDEND AND DIVISOR. QUOTIENT IS POSITIVE IF SIGNS
          ,
          , ARE THE SAME. NEGATIVE IF SIGNS ARE DIFFERENT
          JREMAINDER HAS SAME SIGN AS DIVIDEND
          LD      A.H             ,GET HIGH BYTE OF DIVIDEND
          LD      (SREM).A        ,SAVE AS SIGN OF REMAINDER
          XOR     0               ,EXCLUSIVE OR WITH HIGH BYTE OF DIVISOR
          LD      (SQUOT).A       ,SAVE SIGN OF QUOTIENT
          ,TAKE ABSOLUTE VALUE OF DIVISOR
          LD          A.D
          OR          A
          JP          P.CHKDE         ,JUMP IF DIVISOR IS POSITIVE
          SUB         A               ,SUBTRACT DIVISOR FROM ZERO
          SUB         E
          LD          E,A
          SBC         A.A             ,PROPAGATE BORROW (A=FF IF BORROW)
          SUB         o
          LD          0, A.

          ,TAKE ABSOLUTE VALUE OF DIVIDEND
CHKDE:
          LD          A,H
          OR          A.
          JP          P.DODIV         ,JUMP IF DIVIDEND IS POSITIVE
          SUB         A.              ,SUBTRACT DIVIDEND FROM ZERO
          SUB         L
          LD          L,A.
          SBC         A.A             ,PROPAGATE BORROW (A=FF IF BORROW)
          SUB         H
          LD          H,A.
          ,DIVIDE ABSOLUTE VALUES
DODIV:
          CALL        UDIV16
          RET         C               ,EXIT IF DIVIDE BY ZERO
          ,NEGATE     QUOTIENT IF IT IS NEGATIVE
          LD          A.(SQUOT)
          OR          A.
          JP          P.DOREM         ,JUMP IF QUOTIENT IS POSITIVE
          SUB         A.              ,SUBTRACT QUOTIENT FROM ZERO
          SUB         L
          LD          L,A
          SBC         A.A             ,PROPAGATE BORROW (A=FF IF BORROW)
          SUB         H
          LD          H,A.
DOREM:
          ,NEGATE REMAINDER IF IT IS NEGATIVE
          LD      A.(SREM)
          OR      A.
                                             6B 16-BIT DIVISION (SDIV16, UDIV16)   223
          RET     P                ;RETURN IF REMAINDER IS POSITIVE
          SUB     A                ;SUBTRACT REMAINDER FROM ZERO
          SUB     E
          LD      E,A
          SBC     A,A              ;PROPAGATE BORROW (A=FF IF BORROW)
          SUB     D
          LD      D,A
          RET

          ;UNSIGNED DIVISION
UDIV16:
          ; CHECK FOR DIVISION BY ZERO
          LD       A,E
          OR       D
          JR       NZ,DIVIDE       ; BRANCH IF DIVISOR IS NON-ZERO
          LD       HL,O            ;DIVIDE BY 0 ERROR
          LD       D,H
          LD       E,L
          SCF                      ;SET CARRY, INVALID RESULT
          RET

DIVIDE:
          LD      C,L              ;C = LOW BYTE OF DIVIDEND/QUOTIENT
          LD      A,H              ;A = HIGH BYTE OF DIVIDEND/QUOTIENT
          LD      HL,O             ;HL = REMAINDER
          LD      B,16             ;16 BITS IN DIVIDEND
          OR      A                ;CLEAR CARRY TO START

DVLOOP:
          ;SHIFT NEXT BIT OF QUOTIENT INTO BIT 0 OF DIVIDEND
          ;SHIFT NEXT MOST SIGNIFICANT BIT OF DIVIDEND INTO
          ; LEAST SIGNIFICANT BIT OF REMAINDER
          ;BC HOLDS BOTH DIVIDEND AND QUOTIENT. WHILE WE SHIFT A
          ; BIT FROM MSB OF DIVIDEND, WE SHIFT NEXT BIT OF QUOTIENT
          ; IN FROM CARRY
          ;HL HOLDS REMAINDER
          ,
          ;DO A 32-BIT LEFT SHIFT, SHIFTING
          ; CARRY TO C, C TO A, A TO L, L TO H
          RL      C                ;CARRY (NEXT BIT OF QUOTIENT) TO BIT 0,
          RLA                      ; SHIFT REMAINING BYTES
          RL      L
          RL      H                ;CLEARS CARRY SINCE HL WAS 0
          ;IF REMAINDER IS GREATER THAN OR EQUAL TO DIVISOR, NEXT
          ; BIT OF QUOTIENT IS 1. THIS BIT GOES TO CARRY
          PUSH    HL              ;SAVE CURRENT REMAINDER
          SBC     HL,DE           ;SUBTRACT DIVISOR FROM REMAINDER
          CCF                     ,COMPLEMENT BORROW SO 1 INDICATES
                                  ; A SUCCESSFUL SUBTRACTION
                                  ; (THIS IS NEXT BIT OF QUOTIENT)
          JR      C,DROP          ;JUMP IF REMAINDER IS )= DIVIDEND
          EX      (SP),HL         ;OTHERWISE RESTORE REMAINDER
224      ARITHMETIC


DROP:
          INC         SP          ;DROP REMAINDER FROM TOP OF STACK
          INC         SP
          DJNZ        DVLOOP      ;CONTINUE UNTIL ALL BITS DONE
          ;SHIFT LAST CARRY BIT INTO QUOTIENT
          EX      DE,HL           ,DE = REMAINDER
          RL      C               ,CARRY TO C
          LD      L,C             ,L = LOW BYTE OF QUOTIENT
          RLA
          LD      H,A             ,H = HIGH BYTE OF QUOTIENT
          OR      A               ,CLEAR CARRY, VALID RESULT
          RET
          ,DATA
SQUOT:    DS          1           ,SIGN OF QUOTIENT
SREM:     DS          1           ,SIGN OF REMAINDER
COUNT:    DS          1           ,DIVIDE LOOP COUNTER


          SAMPLE EXECUTION:


SC6B:
                                  ;SIGNED DIVISION
          LD          HL,-1023    , HL = DIVIDEND
          LD          DE, 123     , DE = DIVISOR
          CALL        SDIV16      ;QUOTIENT OF -1023 I 123 = -s
                                  , L = FSH
                                  , H = FFH
                                  ;REMAINDER OF -1023 I 123 = -39
                                  , E = D9H
                                  , D = FFH
                                  ,UNSIGNED DIVISION
          LD          HL,64513    , HL = DIVIDEND
          LD          DE, 123     , DE = DIVISOR
          CALL        UDIV16      ,QUOTIENT OF 64513 I 123 = 524
                                  , L = OCH
                                  ; H = 02H
                                  ,REMAINDER OF 64513 I 123 = 61
                                  , E = 3DH
                                  , D = OOH
          JR          SC6B
          END
16·Bit Comparison (CMP16)                                                                               6C

   Compares two 16-bit operands and sets the
flags accordingly. The Zero flag always indicates        Registers Used: AF, HL
whether the numbers are equal. If the operands           Execution Time: 30 cycles if no overflow, 57 cycles
                                                         if overflow
are unsigned, the Carry flag indicates which is
                                                         Program Size: II bytes
larger (Carry = 1 if subtrahend is larger and 0
                                                         Data Memory Required: None
otherwise). If the operands are signed, the Sign
flag indicates which is larger (Sign = 1 if subtra-
hend is larger and 0 otherwise); two's comple-
ment overflow is considered and the Sign flag is      shift uses ADC A,A rather than RLA to set the
inverted if it occurs.                                Sign and Zero flags (RLA would affect only
   Procedure: The program subtracts the subtra-       Carry). Bit 0 of the accumulator must be 1 after
hend from the minuend. If two's complement            the shift (because the Carry was set), thus
overflow occurs (Parity / Overflow flag = 1), the     ensuring that the Zero flag is cleared. Obviously,
program inverts the Sign flag by EXCLUSIVE            the result cannot be 0 if the subtraction causes
ORing the sign bit with 1. This requires an extra     two's complement overflow. Note that after an
right shift to retain the Carry in bit 7 initially,   addition or subtraction, PE (Parity/Overflow
since XOR always clears Carry. The program            flag = 1) means "overflow set" while PO
then sets Carry to ensure a non-zero result and       (Parity / Overflow flag = 0) means "overflow
shifts the data back to the left. The extra left      clear."




Entry Conditions                                      Exit Conditions
Minuend in HL                                         Flags set as if subtrahend had been subtracted
Subtrahend in DE                                      from minuend, with a correction if two's comple-
                                                      ment overflow occurred.

                                                      Zero flag = 1 if the subtrahend and minuend are
                                                      equal; 0 if they are not equal.

                                                      Carry flag = 1 if subtrahend is larger than
                                                      minuend in the unsigned sense; 0 if it is less than
                                                      or equal to the minuend.

                                                      Sign flag = 1 if subtrahend is larger than
                                                      minuend in the signed sense; 0 if it is less than or
                                                      equal to the minuend. This flag is corrected
                                                      (inverted) if two's complement overflow occurs.

                                                                                                        225
226        ARITHMETIC


Examples
1.    Data:    Minuend (HL) = 03EI 16                           3.    Data:    Minuend (HL) = A45D 16
               Subtrahend (DE) = 07E4 16                                       Subtrahend (DE) = 77EI 16
     Result:   Carry = I, indicating subtrahend is larger in         Result:   Carry= 0, indicating subtrahend is not larger
                 unsigned sense.                                                 in unsigned sense.
               Zero = 0, indicating operands are not equal.                    Zero = 0, indicating operands are not equal.
               Sign = I, indicating subtrahend is larger in                    Sign = I, indicating subtrahend is larger in
                 signed sense.                                                   signed sense.
2.    Data:    Minuend (HL) = C51A 16
               Subtrahend (DE) = C51A 16                           In Example 3, the minuend is a negative two's
     Result:   Carry = 0, indicating subtrahend is not larger   complement number, whereas the subtrahend is
                 in unsigned sense.                             a positive two's complement number. Subtract-
               Zero = I, indicating operands are equal.
               Sign = 0, indicating subtrahend is not larger    ing produces a positive result (3C7CI6) with
                 in signed sense.                               two's complement overflow.




               Title                    16-bit Compare
               Name:                    CMP16



               Purpose:                 Compare 2 16-bit signed or unsigned words and
                                        return the C,Z,S flags set or cleared
               Entry:                   Register       L = Low byte of minuend
                                        Register       H = High byte of minuend
                                        Register       E = Low byte of subtrahend
                                        Register       D   High byte of subtrahend
               Exit:                    Flags returned based on minuend - subtrahend
                                        If both the minuend and subtrahend are 2~s
                                          complement numbers, then use the Z and S
                                          flagsJ
                                        Else use the Z and C flags
                                        IF minuend = subtrahend THEN
                                           Z=l,S=O,C=O
                                         IF minuend > subtrahend THEN
                                           Z=O,S=O,C=O
                                         IF minuend < subtrahend THEN
                                           Z=O,S=l,C=l
               Registers used: AF,HL
               Time:                    30 cycles if no overflow, else 57 cycles
                                                    6C 16-81T COMPARISON (CMP16)   227

         Size:            Program 11 bytes



CMP16:
         OR      A                ;CLEAR CARRY
         SBC     HL,DE            ;SUBTRACT SUBTRAHEND FROM MINUEND
         RET     PO               ;RETURN IF NO OVERFLOW
         LD      A,H              ;OVERFLOW - INVERT SIGN FLAG
         RRA                      ;SAVE CARRY IN BIT 7
         XOR     01000000B        ;COMPLEMENT BIT 6 (SIGN BIT)
         SCF                      ;ENSURE A NON-ZERO RESULT
         ADC     A,A              ;RESTORE CARRY, COMPLEMENTED SIGN
                                  ; ZERO FLAG = 0 FOR SURE
         RET



         SAMPLE EXECUTION:


SC6C:
         ;COMPARE -32768 (8000 HEX) AND 1
         ;SINCE -32768 IS THE MOST NEGATIVE 16-BIT NUMBER,
             THIS COMPARISON WILL SURELY CAUSE OVERFLOW
         LD       HL,-32768
         LD       DE,1
         CALL     CMP16           ;CY = 0, Z = 0, S   1
         ;COMPARE -4 (FFFC HEX) AND -1 (FFFF HEX)
         LD      HL,-4
         LD      DE,-l
         CALL    CMP16           ;CY   1, Z = 0, S              1
         ; COMPARE -1234 AND -1234
         LD       HL,-1234
         LD       DE,-1234
         CALL     CMP16            ;CY   = 0,   Z   = 1,   s   =0
         JR      SC6C
         END
Multiple-Precision Binary Addition
(MPBADD)                                                                                              6D

   Adds two multi-byte unsigned binary num-             Registers Used: AF, B, DE, HL
bers. Both numbers are stored with their least          Execution Time: 46 cycles per byte plus 18 cycles
significant bytes at the lowest address. The sum        overhead
replaces the addend. The length of the numbers          Program Size: II bytes
(in bytes) is 255 or less.                              Data Memory Required: None
   Procedure: The program clears the Carry flag         Special Case: A length of 0 causes an immediate
initially and adds the operands one byte at a           exit with the addend unchanged. The Carry flag is
                                                        cleared.
time, starting with the least significant bytes.
The final Carry flag reflects the addition of the
most significant bytes. A length of 00 causes an
immediate exit with no addition.




Entry Conditions                                      Exit Conditions
Base address of addend in HL                          Addend replaced by addend plus adder
Base address of adder in DE
Length of the operands in bytes in B



Example
I.    Data:     Length of operands (in bytes) = 6
                Addend = 19D028AI93EA l6
                Adder = 293EABF059C7 16
     Result:    Addend = 430ED491EDBI l6
                Carry = 0




               Tit Ie                  Multiple-Precision Binary Addition
               Name:                   MPBADD




               Purpose:                Add 2 arrays of binary bytes
                                       Array! = Array! + Array2

228
                                        6D MULTIPLE-PRECISION BINARY ADDITION (MPBADD)   229

          Entry:               Register pair HL = Base address of array 1
                               Register pair DE = Base address of array 2
                               Register B = Length of the arrays
                                 The arrays are unsigned binary numbers with a
                                 maximum length of 255 bytes, ARRAY[OJ is the
                                 least significant byte, and ARRAY[LENGTH-IJ
                                 the most significant byte.
          Exit:                Array1    :=   Arrayl + Array2
          Registers used: AF,B,DE,HL
          Time:                46 cycles per byte plus 18 cycles overhead
          Size:                Program 11 bytes


MPBADD:
          ; CLEAR CARRY, EXIT IF ARRAY LENGTH IS 0
          LD       A,B
          AND      A               ;CLEAR CARRY, TEST ACCUMULATOR
          RET      Z               ;RETURN IF LENGTH = ZERO
LOOP:
          LD       A,   (DE)              ;GET NEXT BYTE
          ADC      A, (HU                 ;ADD BYTES
          LD       (HU,A                  ;STORE SUM
          INC      HL                     ; INCREMENT ARRAY 1 POINTER
          INC      DE                     ; INCREMENT ARRAY2 POINTER
          DJNZ     LOOP                   ;CONTINUE UNTIL COUNTER = 0
          RET




          SAMPLE EXECUTION:



SC6D:
          LD       HL,AYI                 ;HL = BASE ADDRESS OF ARRAY 1
          LD       DE,AY2                 ;DE = BASE ADDRESS OF ARRAY 2
          LD       B,SZAYS                ;B = LENGTH OF ARRAYS IN BYTES
          CALL     MPBADD                 ;ADD THE ARRAYS
                                                      AY1+0   56H
                                                      AY1+l   13H
                                                      AYl+2   CFH
                                                      AY1+3   BAH
                                                      AY1+4   67H
                                                      AY1+5 = 45H
                                                      AY1+6 = 23H
                                                      AY1+7   OlH
230     ARITHMETIC


         JR          SC6D
SZAYS    EQU         8      ~LENGTH   OF ARRAYS IN BYTES
AY1:
         DB          OEFH
         DB          OCDH
         DB          OABH
         DB          089H
         DB          067H
         DB          045H
         DB          023H
         DB          00lH
AY2:
         DB          067H
         DB          045H
         DB          023H
         DB          00lH
         OB          0
         DB          0
         DB          0
         DB          0
         END
Multiple-Precision Binary Subtraction
(MPBSUB)                                                                                                             6E

   Subtracts two multi-byte unsigned binary                       Registers Used: AF, B, DE, HL
numbers. Both numbers are stored with their                       Execution Time: 46 cycles per byte plus 22 cycles
least significant bytes at the lowest address. The                overhead
difference replaces the minuend. The length of                    Program Size: 12 bytes
the numbers (in bytes) is 255 or less.                            Data Memory Required: None
   Procedure: The program clears the Carry flag                   Special Case: A length of 0 causes an immediate
initially and subtracts the operands one byte at a                exit with the minuend unchanged (that is, the
                                                                  difference is equal to the minuend). The Carry flag is
time, starting with the least significant bytes.                  cleared.
The final Carry flag reflects the subtraction of
the most significant bytes. A length of 0 causes
an immediate exit with no subtraction.




Entry Conditions                                                Exit Conditions
Base address of minuend in HL                                   Minuend replaced by minuend minus subtrahend
Base address of subtrahend in DE
Length of the operands in bytes in B




Example
1.    Data:     Length of operands (in bytes) = 4
                Minuend = 2F5BA7C3'6
                Subtrahend = 14DF35B8'6
     Result:    Minuend = lA7CnOB'6
                The Carry flag is set to 0 since no borrow is
                  necessary.




               Title                     Multiple-Precision Binary Subtraction
               Name:                     MPBSUB



                                                                                                                     231
232     ARITHMETIC


          Purpose:             Subtract 2 arrays of binary bytes
                               Minuend = minuend - subtrahend
          Entry:               Register pair HL = Base address of minuend
                               Register pair DE = Base address of subtrahend
                               Register B = Length of the arrays
                                 The arrays are unsigned binary numbers with a
                                 maximum length of 255 bytes, ARRAY[OJ is the
                                 least si9nific~nt byte, and ARRAY[LENGTH-IJ
                                 the most significant byte.
          Exit :               Minuend := minuend - subtrahend
          Registers used: A F, B, DE, HL
          Time:                46 cycles per byte plus 22 cycles overhead
          Size:                Program 12 bytes



MPBSUB:
          ; CLEAR CARRY, EXIT IF ARRAY LENGTH IS 0
          LD       A,B
          AND      A               ;CLEAR CARRY, TEST ACCUMULATOR
          RET      Z               ;RETURN IF LENGTH = ZERO
          EX       DE,HL           ;SWITCH ARRAY POINTERS
                                   ; SO HL POINTS TO SUBTRAHEND
LOOP:
          LD         A, (DE)           ;GET NEXT BYTE OF MINUEND
          SBC        A, (HU            ;SUBTRACT BYTES
          LD         (DE),A            ;STORE DIFFERENCE
          INC        DE                ; INCREMENT MINUEND POINTER
          INC        HL                ; INCREMENT SUBTRAHEND POINTER
          DJNZ       LOOP              ;CONTINUE UNTIL COUNTER = 0
          RET



          SAMPLE EXECUTION:



SC6E:
          LD         HL,AYI            ;HL = BASE ADDRESS OF MINUEND
          LD         DE,AY2            ;DE = BASE ADDRESS OF SUBTRAHEND
          LD         B,SZAYS           ;B = LENGTH OF ARRAYS IN BYTES
          CALL       MPBSUB            ,SUBTRACT THE ARRAYS
                                                   AY1+0    SSH
                                                   AYl+1    aSH
                                                   AY1+2 = aSH
                         6E MULTIPLE-PRECISION BINARY SUBTRACTION (MPBSUB)   233
                                            AY1+3 = 88H
                                            AY1+4 = 67H
                                            AY1+5 = 45H
                                            AY1+6 = 23H
                                            AY1+7 = 01H
        JR    SC6E
SZAYS   EQU   8      ;LENGTH OF ARRAYS IN BYTES
AY1:
        DB    OEFH
        DB    OCDH
        DB    OABH
        DB    089H
        DB    067H
        DB    045H
        DB    023H
        DB    00lH

AY2:
        DB    067H
        DB    045H
        DB    023H
        DB    00lH
        DB    o
        DB    o
        DB    o
        DB    o
        END
Multiple-Precision Binary Multiplication
(MPBMUL)                                                                                                       6F

   Multiplies two multi-byte unsigned binary              Registers Used: AF, BC, DE, HL
numbers. Both numbers are stored with their               Execution Time: Depends on the length of the
least significant byte at the lowest address. The         operands and on the number of I bits in the
product replaces the multiplicand. The length of          multiplicand (requiring actual additions). If the
                                                          average number of I bits in the multiplicand is four
the numbers (in bytes) is 255 or less. Only the           per byte, the execution time is approximately 728 *
less significant bytes of the product are returned        LENGTH2 + 883 * LENGTH + 300 cycles where
to retain compatibility with other multiple-              LENGTH is the number of bytes in the operands.
precision binary operations.                              Program Size: 104 bytes
   Procedure: The program uses an ordinary                Data Memory Required: 261 bytes anywhere in
                                                          RAM. This is temporary storage for the more
shift-and-add algorithm, adding the multiplier to         significant bytes of the product (255 bytes starting at
the partial product each time it finds a 1 bit in the     address HIPROD), the loop counter (2 bytes starting
mUltiplicand. The partial product and the multi-          at address COUNT), the address immediately follow-
                                                          ing the most significant byte of the high product (2
plicand are shifted through the bit length plus 1;        bytes starting at address ENDHP), and the base
the extra loop moves the final Carry into the             address of the multiplier (2 bytes starting at address
product. The program maintains a full double-             MLIER).
length unsigned partial product in memory                 Special Case: A length of 0 causes an immediate
                                                          exit with the product equal to the multiplicand. The
locations starting at HIPROD (more significant            Carry flag is cleared.
bytes) and in the multiplicand (less significant
bytes). The less significant bytes of the product       examined for I bits. A 0 length causes an exit
replace the multiplicand as it is shifted and           with no multiplication.



Entry Conditions                                        Exit Conditions
Base address of multiplicand in HL                      Multiplicand replaced by multiplicand times
Base address of multiplier in DE                         multiplier
Length of the operands in bytes in B



Example
1.    Data:    Length of operands (in bytes)   = 04     to maintain compatibility with other multiple-
               Multiplicand = 0005DIF7 16               precision arithmetic operations. The more signif-
               Multiplier = 00000ABI 16
                                                        icant bits of the product are available starting
     Result:   Multiplicand = 3E39DIC7 16
                                                        with their least significant byte at address
   Note that MPBMUL returns only the less               HIPROD. The user may need to check those
significant bytes (that is, the number of bytes in      bytes for a possible overflow or extend the
the multiplicand and mUltiplier) of the product         operands with additional zeros.

234
                              6F MULTIPLE-PRECISION BINARY MULTIPLICATION (MPBMUL)   235



            Title           Multiple-Precision Binary Multiplication
            Name:           MPBMUL



            Purpose:        Multiply 2 arrays of binary bytes
                            Multiplicand   =
                                           multiplicand * multiplier
            Entry:          Register pair HL = Base address of multiplicand
                            Register pair DE = Base address of multiplier
                            Register B = Length of the arrays
                              The arrays are unsigned binary numbers with a
                              maximum length of 255 bytes, ARRAY[OJ is the
                              least significant byte, and ARRAY[LENGTH-IJ
                              the most significant byte.
            Exit:           Multiplicand := multiplicand      *   multiplier
            Registers used: AF,BC,DE,HL
            Time:           Assuming the average number of 1 bits in multi-
                            plicand is 4 * length, then the time is approxi-;
                            mately
                             (728 * length A 2) + (883 * length) + 300 cycles

            Size:           Program 104 bytes
                            Data    261 bytes




I'IPBMUL:
            ;EXIT IF LENGTH IS ZERO
            LD      A,B
            AND     A               ;IS LENGTH OF ARRAYS          o ?
            RET     Z               ;YES, EXIT
            ; MAKE POINTERS POINT TO END OF OPERANDS
            LD       C,B             ;BC = LENGTH
            LD       B,O
            ADD      HL,BC           ;END = BASE + L~NGTH
            EX       DE,HL           ;DE POINTS TO END OF MULTIPLICAND
            LD       (MLIER) ,HL     ;SAVE ADDRESS OF MULTIPLIER
            LD       HL,HIPROD
            ADD      HL.BC
            LD       (ENDHP).HL      ;SAVE ADDRESS AT END OF HIPROD
            ;SET COUNT TO NUMBER OF BITS IN ARRAY PLUS 1
            ; COUNT := (LENGTH * 8) + 1
236      ARITHMETIC


          LD          L,C            ;MOVE LENGTH TO HL
          LD          H,B
          ADD         HL,HL          ;LENGTH • 8, SHIFT LEFT 3 TIMES
          ADD         HL,HL
          ADD         HL,HL
          INC         HL             ;ADD 1
          LD          (COUNT> ,HL    ;SAVE NUMBER OF BITS TO DO
          ;ZERO HIGH PRODUCT ARRAY
ZEROPD:
          LD          B,C            ;B = LENGTH IN BYTES
          LD          HL,HIPROD      ;GET ADDRESS OF HIPROD
ZEROLP:
          LD          (HU,O          ;STORE 0
          INC         HL
          DJNZ        ZEROLP         ;CONTINUE UNTIL HIPROD ARRAY IS ZERO

          ;MULTIPLY USING SHIFT AND ADD ALGORITHM
          AND     A               ;CLEAR CARRY FIRST TIME THROUGH
LOOP:
          ;SHIFT CARRY INTO HIPROD ARRAY AND LEAST SIGNIFICANT
          ; BIT OF HIPROD ARRAY TO CARRY
          LD      B,C             ,GET LENGTH IN BYTES
          LD      HL,(ENDHP)      ;GET LAST BYTE OF HIPROD + 1
SRPLP:
          DEC         HL             ;BACK UP TO NEXT BYTE
          RR          (HU
          DJNZ        SRPLP          ;CONTINUE UNTIL INDEX = 0
          ;SHIFT CARRY (NEXT BIT OF LOWER PRODUCT) INTO MOST
           J SIGNIFICANT BIT OF MULTIP~ICAND.
          , THIS ALSO SHIFTS NEXT BIT OF MULTIPLICAND TO CARRY
          LD        L,E            ,HL = ADDRESS OF END OF MULTIPLICAND
          LD        H,D
          LD        B,C            ;B = LENGTH IN BYTES
SRAILP:
          DEC         HL             ,BACK UP TO NEXT BYTE
          RR          (HU
          DJNZ        SRAILP         ;CONTINUE UNTIL DONE
          ;IF NEXT BIT OF MULTIPLICAND IS 1 THEN
          J  ADD MULTIPLIER TO HIPROD ARRAY
          JP      NC,DECCNT        ,JUMP IF NEXT BIT IS ZERO
          ;ADD MULTIPLIER TO HIPROD
          PUSH   DE               ;SAVE ADDRESS OF MULTIPLICAND
          LD     DE, (MLIER)      ;DE = ADDRESS OF MULTIPLIER
          LD     HL,HIPROD        ;HL = ADDRESS OF HIPROD
          LD     B,C              ;B = LENGTH IN BYTES
          AND    A                ;CLEAR CARRY
ADDLP:
           LD         A, (DE)        ;GET NEXT MULTIPLIER BYTE
           ADC        A, (HU         ,ADD TO HIPROD
           LD         (HU,A          ;STORE NEW HIPROD
           INC        DE
                                6F MULTIPLE-PRECISION BINARY MULTIPLICATION (MPBMUL)   237

          INC      HL
          DJNZ     ADDLP               ;CONTINUE UNTIL DONE
          POP      DE                  ;RESTORE ADDRESS OF MULTIPLICAND
          ;DECREMENT BIT COUNTER. EXIT IF DONE
          ; DOES NOT CHANGE CARRY!
DECCNT:
          LD       A. (COUNT)
          DEC      A
          LD       (COUNT) ,A
          JP       NZ,LOOP             ;BRANCH IF LSB OF COUNT NOT ZERO
          PUSH     AF                  ;SAVE CARRY
          LD       A. (COUNT+l.l       ;GET HIGH BYTE OF COUNT
          AND      A                   ; IS IT ZERO?
          ,JP      Z. EXIT             ; EXIT IF SO
          DEC      A                   ;DECREMENT HIGH BYTE OF COUNT
          LD       (COUNT+l.l,A
          POP      AF                  ;RESTORE CARRY
          JP       LOOP                ; CONTINUE
EXIT:
          POP      AF                  ;DROP PSW FROM STACK
          RET                          ; RETURN
          ; DATA
COUNT:    DS       2                   ;TEMPORARY FOR LOOP COUNTER
ENDHP:    DS       2                   ;ADDRESS OF LAST BYTE OF HIPROD + 1
MLIER:    DS       2                   ; ADDRESS OF MULTIPLIER
HIPROD:   DS       255                 ;HIGH PRODUCT BUFFER



          SAMPLE EXECUTION:


SC6F:
          LD       HL.AYl              ;HL = ADDRESS OF MULTIPLICAND
          LD       DE.AY2              ;DE = ADDRESS OF MULTIPLIER
          LD       B.SZAYS             ;B = LENGTH OF OPERANDS IN BYTES
          CALL     MPBMUL              ;MULTIPLE-PRECISION BINARY MULTIPLY
                                       ;RESULT OF 12345H * 1234H = 14B60404H
                                         IN MEMORY AYl       04H
                                                   AY1+l     04H
                                                   AY1+2     B6H
                                                   AY1+3      14H
                                                   AY1+4     OOH
                                                   AY1+5     OOH
                                                   AY1+6     OOH
          JR       SC6F
SZAYS     EQU      7         ;LENGTH OF OPERANDS IN BYTES
238    ARITHMETIC


AVI:
        DB          045H
        DB          023H
        DB          OOIH
        DB          0
        DB          Q
        DB          0
        DB          0
AV2:
        DB          034H
        DB          012H
        DB          0
        DB          0
        DB          0
        DB          0
        DB          0

        END
Multiple-Precision Binary Division
(MPBDIV)                                                                                                                  6G

   Divides two multi-byte unsigned binary                       usual shift-and-subtract algorithm, shifting quo-
numbers. Both numbers are stored with their                     tient and dividend and placing a 1 bit in the
least significant byte at the lowest address. The               quotient each time a trial subtraction is success-
quotient replaces the dividend; the address of                  ful. An extra buffer holds the result of the trial
the least significant byte of the remainder is in               subtraction; that buffer is simply switched with
HL. The length ofthe numbers (in bytes) is 255                  the buffer holding the dividend if the trial
or less. The Carry flag is cleared if no errors                 subtraction is successful. The program exits
occur; if a divide by 0 is attempted, the Carry                 immediately, setting the Carry flag, if it finds the
flag is set to I, the dividend is left unchanged,               divisor to be O. The Carry flag is cleared
and the remainder is set to O.                                  otherwise.
   Procedure: The program divides with the




     Registers Used: AF, BC, DE, HL                             starting at address DVEND), the base address ofthe
     Execution Time: Depends on the length of the               divisor (2 bytes starting at address DVSOR), pointers
     operands and on the number of I bits in the quotient       to the two temporary buffers for the high dividend (2
     (requiring a buffer switch). If the average number of      bytes starting at addresses HDEPTR and ODEPTR,
     I bits in the quotient is 4 per byte, the execution time   respectively), a loop counter (2 bytes starting at
     is approximately 1176 * LENGTH2+ 2038 * LENGTH             address COUNT), and a subtraction loop counter (1
     + 515 cycles where LENGTH is the number of bytes           byte at address SUBCNT).
     in the operands.                                           Special cases:
     Program Size: 161 bytes                                       I. A length of 0 causes an immediate exit with the
     Data Memory Required: 522 bytes anywhere in                Carry flag cleared, the quotient equal to the original
     RAM. This is temporary storage for the high divi-          dividend, and the remainder undefined.
     dend (255 bytes starting at address HIDE I), the result       2. A divisor of 0 causes an exit with the Carry flag
     of the trial subtraction (255 bytes starting at address    set to I, the quotient equal to the original dividend,
     HIDE2), the base address of the dividend (2 bytes          and the remainder equal to O.




Entry Conditions                                                Exit Conditions
Base address of dividend in HL                                  Dividend replaced by dividend divided by divisor
Base address of divisor in DE                                   If the divisor is non-zero, Carry = 0 and the
Length of the operands in bytes in B                               result is normal.
                                                                If the divisor is 0, Carry  = 1, the dividend is
                                                                  unchanged, and the remainder is O.
                                                                The remainder is stored starting with its least
                                                                  significant byte at the address in HL.

                                                                                                                      239
240        ARITHMETIC


Example
I.    Data:    Length of operands (in bytes) = 03
               Divisor = 000F45 16
               Dividend = 35A2F7 16
     Result:   Dividend = 000383 16
               Remainder (starting at address in HL) =
                 0003A8 16
               Carry flag is 0 to indicate no divide-by-O error.




               Title                      Multiple-Precision Binary Division
               Name:                      MPBDIV
.,
 '




               Purpose:                  Divide 2 arrays of binary bytes
                                         Dividend = dividend I divisor
               Entry:                     Register pair HL = Base address of dividend
                                          Register pair DE = Base address of divisor
                                          Register B = Length of operands in bytes
                                             The arrays are unsigned binary numbers with a
                                             maximum length of 255 bytes, ARRAY[O] is the
                                             least significant byte, and ARRAY[LENGTH-l]
                                             the most significant byte.
               Exit:                      Dividend := dividend I divisor
                                          Register pair HL = Base address of remainder
                                          If no errors then
                                            carry := 0
                                          ELSE
                                            divide-by-O error
                                            carry := 1
                                            dividend unchanged
                                            remainder := 0
               Registers used: AF,BC,DE,HL
               Time:                      Assuming there are length/2 1 bits in the
                                          quotient then the time is approximately
                                          (1176 * length A 2) + (2038 * length) + 515 cycles;
               Size:                      Program 161 bytes
                                          Data    522 bytes
                                  6G MULTIPLE-PRECISION BINARY DIVISION (MPBDIV)   241


          ;TEST LENGTH OF OPERANDS, INITIALIZE POINTERS
MPBDIV:
          LD      A,B
          OR      A               ;IS LENGTH OF ARRAYS = O?
          ,JP     Z.OKEXIT        ;EXIT IF SO
          LD      (DVEND).HL      ;SAVE BASE ADDRESS OF DIVIDEND
          LD      (DVSOR).DE      ;SAVE BASE ADDRESS OF DIVISOR
          LD      C.B             ;C = LENGTH OF OPERANDS
          ;SET COUNT TO NUMBER OF BITS IN THE ARRAYS
          ; COUNT := (LENGTH * 8) + 1
          LD      L.C             ;HL = LENGTH IN BYTES
          LD      H.O
          ADD     HL.HL           ;LENGTH III :2
          ADD     HL.HL           ;LENGTH * 4
          ADD     HL.HL           ;LENGTH ... 8
          INC     HL              ;LENGTH '" 8 + 1
          LD      (COUNT> • HL    ;SAVE BIT COUNT
          ; ZERO BOTH HIGH DIVIDEND ARRAYS
          LD       HL.HIDEI        ;HL = ADDRESS OF HIDE1
          LD       DE.HIDE2        ;DE = ADDRESS OF HIDE2
          LD       B.C             ;B = LENGTH IN BYTES
          SUB      A               ;GET 0 FOR FILL
ZEROLP:
          LD      (HU .A          ;ZERO HIDE1
          LD      (DE). A         ; AND HIDE2
          INC     HL
          INC     DE
          DJNZ    ZEROLP
          ;SET HIGH DIVIDEND POINTER TO HIDE1
          LD      HL,HIDE1
          LD      (HDEPTR).HL
          ;SET OTHER HIGH DIVIDEND POINTER TO HIDE2
          LD      HL.HIDE:2
          LD      (ODEPTR).HL

          ;CHECK IF DIVISOR IS ZERO BY LOGICALLY ORING ALL BYTES
          LD      HL.(DVSOR)      ;HL = ADDRESS OF DIVISOR
          LD      B.C             ;B = LENGTH IN BYTES
          SUB     A               ;START LOGICAL OR AT 0
CHKOLP:
          OR      (HU             ;OR NEXT BYTE
          INC     HL              ; INCREMENT TO NEXT BYTE
          DJNZ    CHKOLP          ; CONTI NUE UNTI L ALL BYTES ORED
          OR      A               ;SET FLAGS FROM LOGICAL OR
          ,JR     Z.EREXIT        ;ERROR EXIT IF DIVISOR IS 0

          ;DIVIDE USING TRIAL SUBTRACTION ALGORITHM
          OR      A               ;CLEAR CARRY FIRST TIME THROUGH
242      ARITHMETIC


LOOP:
          ;C = LENGTH
          ;DE = ADDRESS OF DIVISOR
          ;CARRY = NEXT BIT OF QUOTIENT
          ;SHIFT CARRY INTO LOWER DIVIDEND ARRAY AS NEXT BIT OF QUOTIENT
          ; AND MOST SIGNIFICANT BIT OF LOWER DIVIDEND TO CARRY
          LD      B,C              ;B = NUMBER OF BYTES TO ROTATE
          LD      HL,(DVEND)       ;HL = ADDRESS OF DIVIDEND
SLLP1:
          RL          (HU             ;ROTATE BYTE OF DIVIDEND LEFT
          INC         HL              ;NEXT BYTE
          D,.JNZ      SLLPl           ;CONTINUE UNTIL ALL BYTES SHIFTED
          ;DECREMENT BIT COUNTER AND EXIT IF DONE
          ;CARRY IS NOT CHANGED!
DECCNT:
          LD          A, (COUNT)
          DEC         A
          LD          (COUNn,A
          ....R       NZ,CONT         ;CONTINUE IF LOWER BYTE NOT ZERO
          LD          A, (COLlNT+l)
          DEC         A
          LD          (COUNT+1), A
          ,.JP        M,OKEXIT        ;EXIT WHEN COUNT BECOMES NEGATIVE

          ;SHIFT CARRY INTO LSB OF UPPER DIVIDEND
CONT:
          LD          HL,(HDEPTR)     :HL = CURRENT HIGH DIVIDEND POINTER
          LD          B,C             ;B = LENGTH IN BYTES
SLLP2:
          RL          (HU             ;ROTATE BYTE OF UPPER DIVIDEND
          INC         HL              ; INCREMENT TO NEXT BYTE
          D,.JNZ      SLLP2           ;CONTINUE UNTIL ALL BYTES SHIFTED

          ;SUBTRACT DIVISOR FROM HIGH DIVIDEND, PLACE DIFFERENCE IN
          ; OTHER HIGH DIVIDEND ARRAY
          PUSH    BC              ; SAVE LENGTH
          LD      A,C
          LD      (SUBCNT),A      ;SUBCNT = LENGTH IN BYTES
          LD      BC,(ODEPTR)     ;BC = OTHER DIVIDEND
          LD      DE, (HDEPTR)    ;DE = HIGH DIVIDEND
          LD      HL,(DVSOR)      ;HL = DIVISOR
          OR      A               ;CLEAR CARRY
SUBLP:
          LD          A, (DE)         ;NEXT BYTE OF HIGH DIVIDEND
          SBC         A, (HU          ;SUBTRACT DIVISOR
          LD          (BC) ,A         ;SAVE IN OTHER HIGH DIVIDEND
           INC        HL              ; INCREMENT POINTERS
           INC        DE
           INC        BC
          LD          A, (SUBCNT)     ;DECREMENT COUNT
          DEC         A
          LD          (SUBCNn ,A
          ....R       NZ,SUBLP        ; CONTINUE UNTIL DIFFERENCE COMPLETE
          POP         BC              ;RESTORE LENGTH
                                 6G MULTIPLE-PRECISION BINARY DIVISION (MPBDIV)   243
          ;IF CARRY IS 1, HIGH DIVIDEND IS LESS THAN DIVISOR
          ; SO NEXT BIT OF QUOTIENT IS o. IF CARRY IS 0
          ; NEXT BIT OF QUOTIENT IS 1 AND WE REPLACE DIVIDEND
          ; WITH REMAINDER BY SWITCHING POINTERS.
          CCF                      ; COMPLEMENT BORROW SO I T EQUALS
                                   ; NEXT BIT OF QUOTIENT
          JR      NC,LOOP         ;JUMP IF NEXT BIT OF QUOTIENT 0
          LD      HL,(HDEPTR)     ;OTHERWISE EXCHANGE HDEPTR AND ODEPTR
          LD      DE, (ODEPTR)
          LD      (ODEPTR),HL
          LD      (HDEPTR),DE
          ;CONTINUE WITH NEXT BIT OF QUOTIENT 1 (CARRY = 1)
          JP      LOOP
          ;SET CARRY TO INDICATE DIVIDE-BY-ZERO ERROR
EREXIT:
          SCF                     ; SET CARRY, INVALID RESULT
          JP       EXIT
          ;CLEAR CARRY TO INDICATE NO ERRORS
OKEXIT:
          OR       A              ; CLEAR CARRY, VALl D RESULT
          ;ARRAY 1 IS QUOTIENT
          ;HDEPTR CONTAINS ADDRESS OF REMAINDER
EXIT:     LD      HL,(HDEPTR)     ;HL = BASE ADDRESS OF REMAINDER
          RET
          ; DATA
DVEND:    DS       2              ; ADDRESS OF DIVIDEND
DVSOR:    DS       2              ; ADDRESS OF DIVISOR
HDEPTR:   DS       2              ;ADDRESS OF CURRENT HIGH DIVIDEND ARRAY
ODEPTR:   DS       2              ;ADDRESS OF OTHER HIGH DIVIDEND ARRAY
COUNT:    DS       2              ;TEMPORARY FOR LOOP COUNTER
SUBCNT:   DS       1              ;SUBTRACT LOOP COUNT
HIDEI :   DS       255            ;HIGH DIVIDEND BUFFER 1
HIDE2:    DS       255            ;HIGH DIVIDEND BUFFER 2



          SAMPLE EXECUTION:


SC6G:
          LD       HL,AYI         ;HL = BASE ADDRESS OF DIVIDEND
          LD       DE,AY2         ;DE = BASE ADDRESS OF DIVISOR
          LD       B,SZAYS        ;B = LENGTH OF ARRAYS IN BYTES
          CALL     MPBDIV         ;MULTIPLE-PRECISION BINARY DIVIDE
                                  ;RESULT OF 14B60404H / 1234H = 12345H
                                    IN MEMORY AY1     = 45H
                                              AY1+1     23H
                                              AY1+2   = OlH
244     ARITHMETIC


                                                AY1+3      OOH
                                                AY1+4    = OOH
                                                AY1+5      OOH
                                                AY1+6      OOH
         JR          SC6G
SZAYS    EQU         7      ;LENGTH OF ARRAYS IN BYTES
AY1:
         DB          004H
         DB          004H
         DB          OB6H
         DB          014H
         DB          0
         DB          0
         DB          0
AY2:
         DB          034H
         DB          012H
         DB          0
         DB          0
         DB          0
         DB          0
         DB          0

         END
Multiple-Precision Binary Comparison
(MPBCMP)                                                                                                               6H

   Compares two multi-byte unsigned binary                          Registers Used: AF, BC, DE, HL
numbers and sets the Carry and Zero flags                           Execution Time: 44 cycles per byte that must be
appropriately. The Zero flag is set to I if the                     examined plus approximately 60 cycles overhead.
operands are equal and to 0 if they are not equal.                  That is, the program continues until it finds cor-
                                                                    responding bytes that are not the same; each pair of
The Carry flag is set to I if the subtrahend is                     bytes it must examine requires 44 cycles.
larger than the minuend; the Carry flag is                          Examples:
cleared otherwise. Thus, the flags are set as if the                   I. Comparing two 6-byte numbers that are equal:
subtrahend had been subtracted from the                                     44 * 6 + 60 = 324 cycles
minuend.                                                              2. Comparing two 8-byte numbers that differ in
                                                                    the next to most significant bytes:
   Procedure: The program compares the oper-
                                                                            44 * 2 + 60 = 148 cycles
ands one byte at a time, starting with the most                     Program Size: 19 bytes
significant bytes and continuing until it finds                     Data Memory Required: None
corresponding bytes that are not equal. If all the                  Special Case: A length of 0 causes an immediate
bytes are equal, it exits with the Zero flag set to                 exit with the Carry flag cleared and the Zero flag set
l. Note that the comparison works through the                       to 1.
operands starting with the most significant
bytes, whereas the subtraction (Subroutine 6E)
starts with the least significant bytes.



Entry Conditions                                               Exit Conditions
Base address of minuend in HL                                  Flags set as if subtrahend had been subtracted
Base address of subtrahend in DE                                 from minuend.
Length of the operands in bytes in B                           Zero flag = 1 if subtrahend and minuend are
                                                                 equal, 0 if they are not equal.
                                                               Carry flag = 1 if subtrahend is larger than
                                                                 minuend in the unsigned sense, 0 if it is less
                                                                 than or equal to the minuend.


Examples
1.    Data:    Length of operands (in bytes) = 6               3.     Data:     Length of operands (in bytes) = 6
               Subtrahend = 19D028Al93EA 16                                     Subtrahend = 19D028Al93EA 16
               Minuend = 4E67BCl5A266 16                                        Minuend = OF37E599lD7C 16
     Result:   Zero flag = 0 (operands are not equal)               Result:     Zero flag = 0 (operands are not equal)
               Carry flag = 0 (subtrahend is not larger than                    Carry flag = I (subtrahend is larger than
                 minuend)                                                         minuend)


                                                                                                                      245
246        ARITHMETIC


2.    Data:     Length of operands (in bytes) = 6
                Subtrahend = 19D028Al93EA I6
                Minuend = 19D028AI93EA I6
     Result:    Zero flag = I (operands are equal)
                Carry flag = 0 (subtrahend is not larger than
                  minuend)




               Title                    Multiple-Precision Binary Comparison
               Name:                    MPBCMP



               Put"pose:                Compare 2 arrays of binary bytes and return
                                        the Carry and Zero flags set or cleared
               Entry:                   Register pair HL = Base address of minuend
                                        Register pair DE        =
                                                           Base address of subtrahend
                                        Register B = Length of operands in bytes
                                           The arrays are unsigned binary number"s wi th a
                                           maximum length of 255 bytes, ARRAY[O] is the
                                           least significant byte, and ARRAY[LENGTH-11
                                           the most significant byte.
               Exit:                    IF minuend = subtrahend THEN
                                          C=0,Z=1
                                        IF minuend ) subtrahend THEN
                                          C=O,Z=O
                                        IF minuend < subtrahend THEN
                                          C=l,Z=O
               Registers used: AF,BC,DE,HL
               Time:                    44 cycles per byte that must be examined plus
                                        60 cycles overhead
               Size:                    Program 19 bytes



MPBCMP:
               JTEST LENGTH OF OPERANDS, SET POINTERS TO MSB'S
               LD      A,B
               OR      A                IS LENGTH OF ARRAYS = O?
               RET     Z                YES, EXIT WITH C=O, Z=l
               LD      C,B              BC = LENGTH
                            6H MULTIPLE-PRECISION BINARY COMPARISON (MPBCMP)   247

        LD      B,O
        ADD     HL,BC
        EX      DE,HL           ~DE POINTS TO END OF MINUEND
        ADD     HL,BC           ~HL POINTS TO END OF SUBTRAHEND
        LD      B,C             ;B = LENGTH
        OR      A               ;CLEAR CARRY INITIALLY
        ~SUBTRACT BYTES, STARTING WITH MOST SIGNIFICANT
        ~EXIT WITH FLAGS SET IF CORRESPONDING BYTES NOT EQUAL
LOOP:
        DEC     HL                  ;BACK UP TO LESS SIGNIFICANT BYTE
        DEC     DE
        LD      A, (DE)             ~GET NEXT BYTE OF MINUEND
        SBC     A, (HU              ;SUBTRACT BYTE OF SUBTRAHEND
        RET     NZ                  ,RETURN IF NOT EQUAL WITH FLAGS
                                    ; SET
        DJNZ    LOOP                ,CONTINUE UNTIL ALL BYTES COMPARED
        RET                         ; EQUAL, RETURN WITH C=O, Z=1



        SAMPLE EXECUTION:


SC6H:
        LD      HL,AYI              ~HL = BASE ADDRESS OF MINUEND
        LD      DE,AY2              ~DE = BASE ADDRESS OF SUBTRAHEND
        LD      B,SZAYS             ;B = LENGTH OF OPERANDS IN BYTES
        CALL    MPBCMP              ~MULTIPLE-PRECISION BINARY COMPARISON
                                    ~RESULT OF COMPARE (7654321H, 1234567H) IS
                                    , C=O,Z=O
        JR      SC6H
SZAYS   EQU     7         ~LENGTH   OF OPERANDS IN BYTES
AYI :
        DB      021H
        DB      043H
        DB      065H
        DB      007H
        DB      0
        DB      0
        DB      0
AY2:
        DB      067H
        DB      045H
        DB      023H
        DB      001H
        DB      0
        DB      0
        DB      0
        END
Multiple-Precision Decimal Addition
(MPDADD)                                                                                                   61

   Adds two multi-byte unsigned decimal num-
bers. Both numbers are stored with their least             Registers Used: AF, B, DE, HL
significant digits at the lowest address. The sum          Execution Time: 50 cycles per byte plus 18 cycles
replaces the addend. The length of the numbers             overhead
(in bytes) is 255 or less.                                 Program Size: 12 bytes
   Procedure: The program first clears the Carry           Data Memory Required: None
flag and then adds the operands one byte (two              Special Case: A length of 0 causes an immediate
                                                           exit with the addend unchanged and the Carry flag
digits) at a time, starting with the least significant     cleared.
digits. The sum replaces the addend. A length of
00 causes an immediate exit with no addition.
The final Carry flag reflects the addition of the
most significant digits.




Entry Conditions                                         Exit Conditions
Base address of addend in HL                             Addend replaced by addend plus adder
Base address of adder in DE
Length of the operands in bytes in register B




Example
1.    Data:    Length of operands (in bytes) = 6
               Addend = 196028819315 16
               Adder = 293471605987 16
     Result:   Addend = 489500425302 16
               Carry = 0




               Tit Ie                  Multiple-Precision Decimal Addition
               Name:                   MPDADD




 248
                                   61 MULTIPLE-PRECISION DECIMAL ADDITION (MPDADD)   249
          Purpose:             Add 2 arrays of BCD bytes
                               Array1 = Array1 + Array2
          Entry:               Register pair HL = Base address of array 1
                               Register pair DE = Base address of array 2
                               Register B = Length of arrays in bytes
                                 The arrays are unsigned BCD numbers with a
                                 maximum length of 255 bytes, ARRAY[O] is the
                                 least significant byte, and ARRAY[LENGTH-1]
                                 the most significant byte.
          Exit:                Array1 := Array1 + Array2
          Registers used: A,B,DE,F,HL
          Time:                50 cycles per byte plus 18 cycles overhead
          Size:                Program 12 bytes


MPDADD:
          ;TEST ARRAY LENGTH FOR ZERO, CLEAR CARRY
          LD       A,B
          OR       A                ;TEST LENGTH AND CLEAR CARRY
          RET      Z                ;EXIT IF LENGTH IS 0
          ;ADD OPERANDS 2 DIGITS AT A TIME
              NOTE CARRY IS 0 INITIALLY
LOOP:
          LD         A, (DE)
          ADC        A, (HU            ;ADD NEXT BYTES
          DAA                         '; CHANGE TO DECIMAL
          LD         (HU,A             ;STORE SUM
          INC        HL                ; INCREMENT TO NEXT BYTE
          INC        DE
          DJNZ       LOOP              ;CONTINUE UNTIL ALL BYTES SUMMED
          RET



          SAMPLE EXECUTION:



SC6I:
          LD         HL.AY1            ;HL = BASE ADDRESS OF ARRAY 1
          LD         DE,AY2            ;DE = BASE ADDRESS OF ARRAY 2
          LD         B,SZAYS           ;B = LENGTH OF ARRAYS IN BYTES
          CALL       MPDADD            ;MULTIPLE-PRECISION BCD ADDITION
                                       ;RESULT OF 1234567 + 1234567 = 2469134
                                         IN MEMORY AY1     = 34H
                                                   AY1+1   = 91H
                                                   AY1+2   = 46H
250     ARITHMETIC


                                                AY1+3      02H
                                                AY1+4      OOH
                                                AY1+5      OOH
                                                AY1+6    = OOH
         JR          SC6I
SZAYS    EQU         7      :LENGTH OF ARRAYS IN BYTES
AYl :
         DB          067H
         DB          045H
         DB          023H
         DB          00lH
         DB          0
         DB          0
         DB          0

AY2:
         DB          067H
         DB          045H
         DB          023H
         DB          00lH
         DB          0
         DB          0
         DB          0

         END
Multiple-Precision Decimal Subtraction
(MPDSUB)                                                                                                      6J

   Subtracts two multi-byte unsigned decimal
                                                          Registers Used: A, B, DE, F, HL
numbers. Both numbers are stored with their
                                                          Execution Time: 50 cycles per byte plus 22 cycles
least significant digits at the lowest address. The       overhead
difference replaces the minuend. The length of            Program Size: 13 bytes
the numbers (in bytes) is 255 or less.                    Data Memory Required: None
   Procedure: The program first clears the Carry          Special Case: A length of 0 causes an immediate
flag and then subtracts the subtrahend from the           exit with the minuend unchanged (that is, the
minuend one byte (two digits) at a time, starting         difference is equal to the minuend). The Carry flag is
                                                          cleared.
with the least significant digits. A length of 0
causes an immediate exit with no subtraction.
The final Carry flag reflects the subtraction of
the most significant digits.




Entry Conditions                                        Exit Conditions
Base address of minuend in HL                           Minuend replaced by minuend minus subtrahend
Base address of subtrahend in DE
Length of the operands in bytes in B



Example
    Data:     Length of operands (in bytes) = 6
              Minuend = 293471605987 16
              Subtrahend = 1960288193151 16
   Result:    Minuend = 097442786672 16
              Carry = 0, since no borrow is necessary




             Title                   Multiple-Precision Decimal Subtraction
             Name:                   MPDSUB




                                                                                                             251
252     ARITHMETIC


          Purpose:             Subtract 2 arrays of BCD bytes
                               Minuend = minuend - subtrahend
          Entry:               Register pair HL = Base address of minuend
                               Register pair DE = Base address of subtrahend
                               Register B = Length of arrays in bytes
                                 The arrays are unsigned BCD numbers with a
                                 maximum length of 255 bytes, ARRAY[O] is the
                                 least significant byte, and ARRAY[LENGTH-1J
                                 the most significant byte.
          Exit:                Minuend := minuend - subtrahend
          Registers used: A,B,DE,F,HL
          Time:                50 cycles per byte plus 22 cycles overhead
          Size:                Program 13 bytes


MPDSUB:
          :TEST ARRAY LENGTH FOR ZERO, CLEAR CARRY
          LD      A,B
          OR      A               :TEST ARRAY LENGTH, CLEAR CARRY
          RET     Z               ;EXIT IF LENGTH IS 0
          EX      DE,HL           :HL = SUBTRAHEND
                                  :DE = MINUEND
          ;SUBTRACT OPERANDS 2 DIGITS AT A TIME
          ; NOTE CARRY IS INITIALLY 0
LOOP:
          LD         A, (DE)           ;GET BYTE OF MINUEND
          SBC        A, (HL>           :SUBTRACT BYTE OF SUBTRAHEND
          DAA                          ;CHANGE TO DECIMAL
          LD         (DE), A           ;STORE BYTE OF DIFFERENCE
          INC        HL                ;INCREMENT TO NEXT BYTE
          INC        DE
          DJNZ       LOOP              ;CONTINUE UNTIL ALL BYTES SUBTRACTED
          RET



          SAMPLE EXECUTION:


SC6J:
          LD         HL,AY1            ;HL = BASE ADDRESS OF MINUEND
          LD         DE,AY2            ;DE = BASE ADDRESS OF SUBTRAHEND
          LD         B,SZAYS           :B = LENGTH OF ARRAYS IN BYTES
          CALL       MPDSUB            ;MULTIPLE-PRECISION BCD SUBTRACTION
                                       ;RESULT OF 2469134 - 1234567 = 1234567
                                         IN MEMORY AY1       67H
                                                   AY1+1   = 45H
                        6J MULTIPLE-PRECISION DECIMAL SUBTRACTION (MPDSUB)   253
                                            AY1+2     = 23H
                                            AY1+3     = 01H
                                            AY1+4       OOH
                                            AY1+5     = OOH
                                            AY1+6     = OOH
        ,JR   SC6,J
SZAYS   EQU   7       ;LENGTH OF ARRAYS IN BYTES
AYl :
        DB    034H
        DB    091H
        DB    046H
        DB    002H
        DB    o
        DB    o
        DB    o
AY2:
        DB    067H
        DB    045H
        DB    023H
        DB    001H
        DB    o
        DB    o
        DB    o
        END
Multiple-Precision Decimal Multiplication
(MPDMU~                                                                                                      6K

   Multiplies two multi-byte unsigned decimal             Registers Used: AF, BC, DE, HL
numbers. Both numbers are stored with their               Execution Time: Depends on the length of the
least significant digits at the lowest address. The       operands and on the size of the digits in the
                                                          multiplicand (since those digits determine how many
product replaces the multiplicand. The length of          times the multiplier must be added to the partial
the numbers (in bytes) is 255 or less. Only the           product). If the average digit in the mUltiplicand has
least significant bytes of the prod uct are returned      a value of 5, then the execution time is approximately
                                                          694 * LENGTH2 + 1555 * LENGTH + 272 cycles
to retain compatibility with other multiple-              where LENGTH is the number of bytes in the
precision decimal operations.                             operands.
   Procedure: The program handles each digit of           Program Size: 167 bytes
the mUltiplicand separately. It masks the digit           Data Memory Required: 520 bytes anywhere in
off, shifts it (if it is the upper nibble of a byte),     RAM. This is temporary storage for the high bytes of
                                                          the partial product (255 bytes starting at address
and then uses it as a counter to determine how            PROD), the mUltiplicand (255 bytes starting at
many times to add the multiplier to the partial           address MCAND), the length ofthe arrays (1 byte at
product. The least significant digit of the partial       address LEN), a digit counter indicating upper or
                                                          lower digit (I byte at address DCNT), a loop counter
product is saved as the next digit of the full            (I byte at address LPCNT), an overflow byte (I byte
product and the partial product is shifted right          at address OVRFLW), pointers to the multiplicand
four bits. The program uses a flag to determine           and multiplier (2 bytes each starting at addresses
                                                          MCADR and MPADR, respectively), and the next
whether it is currently working with the upper or         byte ofthe multiplicand (I byte at address NBYTE).
lower digit of a byte. A length of 00 causes an           Special Case: A length of 0 causes an immediate
exit with no multiplication.                              exit with the multiplicand unchanged. The more
                                                          significant bytes of the product (starting at address
                                                          PROD) are undefined.




Entry Conditions                                        Exit Conditions
Base address of multiplicand in HL                      Multiplicand replaced by multiplicand times
Base address of multiplier in DE                         mUltiplier
Length of the operands in bytes in B




Example
I.    Data:    Length of operands (in bytes) = 04
               Multiplier = 00003518 16                   Note that MPDMUL returns only the less sig-
               Multiplicand = 00006294 16               nificant bytes of the product (that is, the number
     Result:   Multiplicand = 22142292 16               of bytes in the multiplicand and multiplier) to

254
                                  6K MULTIPLE-PRECISION DECIMAL MULTIPLICATION (MPDMUL)           255
maintain compatibility with other multiple-            address PROD. The user may need to check
preclslOn decimal arithmetic operations. The           those bytes for a possible overflow or extend the
more significant bytes of the product are avail-       operands with zeros.
able starting with their least significant digits at




           Title                  Multiple-Precision Decimal           ~ultiplication
           Name:                  MPDMUL




           Purpose:               Multiply 2 arrays of BCD bytes
                                  Multiplicand = multiplicand * multiplier

           Entry:                 Register pair HL        =
                                                     Multiplicand base address
                                  Register pair DE        =
                                                     Multiplier base address
                                  Register B      =
                                               Length of arrays in bytes

                                     The arrays are unsigned BCD numbers with a
                                     maximum length of 255 bytes, ARRAY[O] is the
                                     least significant byte, and ARRAY[LENGTH-1J
                                     the most significant byte.
           Exit:                  Multiplicand := multiplicand            *   multiplier
           Registers used: AF,BC,DE,HL
                                                                                                    ,
           Time:                  Assuming the average digit value of multiplicand;
                                  is 5, the time is approximately                   ;
                                   (694 * length A 2) + (1555 * length) + 272 cycles;

           Size:                  Program 167 bytes
                                  Data    520 bytes




MPDMUL:
           ; INITIALIZE COUNTERS AND POINTERS
           LD       A,B             ;TEST LENGTH OF OPERANDS
           OR       A
           RET      Z               ;EXIT IF LENGTH IS 0
           LD       (LEN), A        ; SAVE LENGTH
           LD       (LPCNT),A       ; LOOP COUNTER               =
                                                     LENGTH IN BYTES
           LD       (MCADR),HL      ; SAVE MULTIPLICAND ADDRESS
           LD       (MPADR) ,DE     ; SAVE MULTIPLIER ADDRESS
256      ARITHMETIC


           ;SAVE MULTIPLICAND IN TEMPORARY BUFFER (MCAND)
           LD       DE. MCAND      ; DE PO I NTS TO TEMPORARY MULTI PLI CAND
           LD       (NBYTE).DE
                                   ; HL POINTS TO MULTIPLICAND
           LD       C.B            ;BC = LENGTH
           LD       B.O
           LDIR                    ; MOVE MULTIPLICAND TO BUFFER
           ;CLEAR PARTIAL PRODUCT. CONSISTING OF UPPER BYTES
              STARTING AT PROD AND LOWER BYTES REPLACING
           ;  MUL TIPLI CAND
           LD       HL.(MCADR)
           LD       A.(LEN)
           CALL     ZEROBUF        ;ZERO MULTIPLICAND
           ;ZERO PRODUCT
           LD      HL,PROD
           CALL    ZEROBUF           ;ZERO PRODUCT ARRAY
           ,
           ;LOOP THROUGH ALL BYTES OF MULTIPLICAND
LOOP:
           LD         A.1
           LD         (DCNT> .A      ;START WITH LOWER DIGIT
           ;LOOP THROUGH 2 DIGITS PER BYTE
           ; DURING LOWER DIGIT DCNT = 1
           ; DURING UPPER DIGIT DCNT = 0
DLOOP:
           SUB        A              ;A   =0
           LD         (OVRFLW),A     ;CLEAR OVERFLOW BYTE
           LD         A. (DCNT>
           OR         A              ;TEST FOR LOWER DIGIT (Z=O)
           LD         HL, (NBYTE)    ;GET NEXT BYTE
           LD         A, (HU
           JR         NZ.DLOOPl      ;JUMP IF LOWER DIGIT
           RRCA                      ;SHIFT UPPER DIGIT RIGHT 4 BITS
           RRCA
           RRCA
           RRCA
DLOOPl :
           AND        OFH            ;KEEP ONLY CURRENT DIGIT
           JR         Z.SDIGIT       ;BRANCH IF DIGIT IS ZERO
           LD         C.A            ;C = DIGIT
           ;ADD MULTIPLIER TO PRODUCT NDIGIT TIMES
ADDLP:
           LD         HL.(MPADR)     ;HL = MULTIPLIER ADDRESS
           LD         DE. PROD       ;DE = PRODUCT ADDRESS
           LD         A.(LEN)
           LD         B.A            ;B = LENGTH
           OR         A              ;CLEAR CARRY INITIALLY
INNER:
           LD         A.(DE)         ; GET NEXT BYTE OF PRODUCT
           ADC        A. (HU         JADD NEXT BYTE OF MULTIPLIER
                              6K MULTIPLE-PRECISION DECIMAL MULTIPLICATION (MPDMUL)   257
          DAA                          ;DECIMAL ADJUST
          LD      (DE) ,A              ;STORE SUM IN PRODUCT
          INC     HL
          INC     DE
          DJNZ    INNER                ;CONTINUE UNTIL ALL BYTES ADDED
          JR      NC,DECND             ;JUMP IF NO OVERFLOW FROM ADDITION
          LD      HL,OVRFLW            ;ELSE INCREMENT OVERFLOW BYTE
          INC     (HU
DECND:
          DEC     C
          JR      NZ,ADDLP             ;CONTINUE UNTIL DIGIT = 0
          ;STORE LEAST SIGNIFICANT DIGIT OF PRODUCT
          ; AS NEXT DIGIT OF MULTIPLICAND
SDIGIT:
          LD      A, (PROD>            ;GET LOW BYTE OF PRODUCT
          AND     OFH
          LD      B,A                  ;SAVE IN B
          LD      A. (DCNT>
          OR      A                    ;TEST FOR LOWER DIGIT (Z=O)
          LD      A,B                  ; A = NEXT DIGIT
          JR      NZ,SDI               ;JUMP IF WORKING ON LOWER· DIGIT
          RRCA                         ;ELSE MOVE DIGIT TO HIGH BITS
          RRCA
          RRCA
          RRCA
SD1:
          LD      HL,(MCADR)           ;PLACE NEXT DIGIT IN MULTIPLICAND
          OR      (HU
          LD      (HU.A
          ;SHIFT PRODUCT RIGHT 1 DIGIT (4 BITS)
          LD      A, (LEN)
          LD      B,A             ;B = LENGTH
          LD      E,A
          LD      0,0
          LD      HL,PROD
          ADD     HL,DE           ;HL POINTS BEYOND END OF PROD
          LD      A, (OVRFLW)     ;A = OVERFLOW BYTE
SHFTLP:
          DEC     HL                   ; DECREMENT, POINT TO NEXT BYTE
          RRD                          ;ROTATE BYTE OF PRODUCT RIGHT 1 DIGIT
          D.JNZ   SHFTLP               ;CONTINUE UNTIL DONE
          ;CHECK IF DONE WITH BOTH DIGITS OF THIS BYTE
          LD      HL,DCNT         ;ARE WE ON LOWER DIGIT?
          DEC     (HU
          JR      Z,DLOOP         ;YES, DO UPPER DIGIT OF SAME BYTE
          ; INCREMENT TO NEXT BYTE AND SEE IF DONE
          LD       HL,(NBVTE)      ; INCREMENT TO NEXT MULTIPLICAND BYTE
          INC      HL
          LD       (NBYTE),HL
258     ARITHMETIC


           LD        HL,(MCADR)    ~INCREMENT   TO NEXT RESULT BYTE
           INC       HL
           LD        (MCADR),HL
           LD        HL,LPCNT      ~DECREMENT   LOOP COUNTER
           DEC       (HL>
           .JR       NZ,LOOP
EXIT:
           RET

           ~-----------------------------------------
           ,ROUTINE: ZEROBUF
           ~PURPOSE: ZERO A BUFFER
           JENTRY~ HL POINTS TO FIRST BYTE OF BUFFER
           ,       LEN = LENGTH OF BUFFER
           ~EXIT:  BUFFER ZEROED
           ~REGISTERS USED: AF,BC,DE,HL
           ,----------------------------------------
ZEROBUF:
           LD        (HL> ,0       ,ZERO FIRST BYTE
           LD        A, (LEN)
           DEC       A
           RET       Z             ,RETURN IF ONLY ONE BYTE
           LD        D,H
           LD        E,L
           INC       DE            ~DE = SECOND BYTE
           LD        CrA           ,BC = LENGTH OF ARRAY
           LD        B,O
           LDIR                    ~CLEAR REST OF BUFFER BY
           RET                     , PROPAGATING ZEROS FROM ONE
                                   , BYTE TO THE NEXT
           ~DATA
LEN:       DS        1             ~LENGTH  OF ARRAYS
DCNT:      DS        1             ~DIGIT COUNTER FOR BYTES
LPCNT:     DS        1             ,LOOP COUNTER
OVRFLW:    DS        1             ~OVERFLOW BYTE
MCADR:     DS        2             ,NEXT BYTE TO STORE INTO
MPADR:     DS        2             ~ADDRESS OF MULTIPLIER
NBYTE:     DS        2             ,NEXT DIGIT OF MULTIPLICAND
PROD:      DS        255           ~PRODUCT BUFFER
MCAND:     DS        255           ,MULTIPLICAND BUFFER



           SAMPLE EXECUTION:


SC6K:
           LD        HL,AY1        ,BASE ADDRESS OF MULTIPLICAND
           LD        DE,AY2        ,BASE ADDRESS OF MULTIPLIER
           LD        B,SZAYS       ~LENGTH OF ARRAYS IN BYTES
           CALL      MPDMUL        ~MULTIPLE-PRECISION BCD MULTIPLICATION
                                   ,RESULT OF 1234 * 1234 = 1522756
                     6K MULTIPLE-PRECISION DECIMAL MULTIPLICATION (MPDMUL)   259
                                 IN MEMORY AY1        = 56H
                                           AY1+1      = 27H
                                           AY1+2      = 52H
                                           AY1+3      = 01H
                                           AY1+4      = OOH
                                           AY1+5      = OOH
                                           AY1+6      = OOH

        JR    SC6K

SZAYS   EQU   7      ;LENGTH OF ARRAYS IN BYTES
AY1:
        DB    034H
        DB    012H
        DB    o
        DB    o
        DB    o
        DB    o
        DB    o
AY2:
        DB    034H
        DB    012H
        DB    o
        DB    o
        DB    o
        DB    o
        DB    o
        END
Multiple-Precision Decimal Division
(MPDDIV)                                                                                                             6L

   Divides two multi-byte unsigned decimal                    Procedure: The program divides by determin-
numbers. Both numbers are stored with their                ing how many times the divisor can be subtracted
least significant digits at the lowest address. The        from the dividend. It saves that number in the
quotient replaces the dividend; the remainder is           quotient, makes the remainder into the new
not returned, but its base address is in memory            dividend, and rotates the dividend and the
locations HDEPTR and HDEPTR+ 1. The                        quotient left one digit. The program exits
length of the numbers (in bytes) is 255 or less.           immediately, setting the Carry flag, if it finds
The Carry flag is cleared if no errors occur; if a         the divisor to be O. The Carry flag is cleared
divide by 0 is attempted, the Carry flag is set to         otherwise.
I, the dividend is unchanged, and the remainder
is set to O.



     Registers Used: AF, BC, DE, HL                        LENGTH), the next digit in the array (I byte at
     Execution, Time: Depends on the length of the         address NDIGIT), the counter for the subtraction
     operands and on the size of the digits in the         loop (I byte at address CNT), pointers to the
     quotient (determining how many times the divisor      dividend, divisor, current high dividend and remain-
     must be subtracted from the dividend). If the         der, and other high dividend (2 bytes each starting at
     average digit in the quotient has a value of 5, the   addresses DVADR, DSADR, HDEPTR, and
     execution time is approximately 1054 * LENGTH2 +      ODEPTR, respectively), and the divide loop counter
     2297 * LENGTH + 390 cycles where LENGTH is the        (2 bytes starting at address COUNT).
     number of bytes in the operands.                      Special Cases:
     Program Size: 168 bytes                                  1. A length of 0 causes an immediate exit with the
     Data Memory Required: 523 bytes anywhere in           Carry flag cleared, the quotient equal to the original
     RAM. This is storage for the high dividend (255       dividend, and the remainder undefined.
     bytes starting at address HIDEI), the result of the      2. A divisor of 0 causes an exit with the Carry flag
     subtraction (255 bytes starting at address HIDE2),    set to I, the quotient equal to the original dividend,
     the length of the operands (I byte at address         and the remainder equal to O.




Entry Conditions                                           Exit Conditions
Base address of dividend in HL                             Dividend replaced by dividend divided by divisor
Base address of divisor in DE                              If the divisor is non-zero, Carry = 0 and the
Length of the operands in bytes in B                          result is normal.
                                                           If the divisor is 0, Carry= 1, the dividend is
                                                             unchanged, and the remainder is O.
                                                           The base address of the remainder (i.e., the
                                                             address of its least significant digits) is in
                                                             HDEPTR and HDEPTR+ 1.

260
                                                      6L MULTIPLE-PRECISION DECIMAL DIVISION (MPDDIV)   261

Example
1.    Data:     Length of operands (in bytes) = 04
                Dividend = 22142298 16
                Divisor = 00006294 16
     Result:    Dividend = 00003518 16
                Remainder (base address in HDEPTR and
                HDEPTR + 1) = 00000006 16
                Carry flag is 0 to indicate no divide-by-O error.




               Tit Ie                     Multiple-Precision Decimal Division
               Name:                      MPDDIV




               Purpose:                   Divide 2 arrays of BCD bytes
                                          Quotient := dividend / divisor

               Entry:                     Register pair HL = Base address of dividend
                                          Register pair DE = Base address of divisor
                                          Register B          =
                                                       Length of operands in bytes

                                              The arrays are unsigned BCD numbers with a
                                              maximum length of 255 bytes, ARRAY[O] is the
                                              least significant byte, and ARRAY[LENOTH-l]
                                              the most significant byte.

               Exit :                     Dividend := dividend / divisor
                                          Remainder := base address in HDEPTR
                                          If no errors then
                                            carry := 0
                                          ELSE
                                            divide-by-O e~ror
                                            carry : = 1
                                            dividend unchanged
                                            remainder := 0

               Registers used: AF,BC,DE,HL

               Time:                      Assuming the average digit value in the
                                          quotient is 5 then the time is approximately
                                          (1054 * length A 2) + (2297 * length) + 390 cycles;

               Size:                      Program 168 bytes
                                          Data    523 bytes
262      ARITHMETIC


MPDDIV:
          ; SAVE PARAMETERS AND CHECK FOR ZERO LENGTH
          LD       (DVADR),HL      ;SAVE DIVIDEND ADDRESS
          LD       (DSADR),DE      ;SAVE DIVISOR ADDRESS
          LD       A,B
          LD       (LENGTH),A      ;SAVE LENGTH
          OR       A               ;TEST LENGTH
          JP       Z,OKEXIT        ;EXIT IF LENGTH    0
          ;ZERO BOTH DIVIDEND BUFFERS
          ; AND SET UP THE DIVIDEND POINTERS
          LD      HL,HIDE1        ;HL = ADDRESS OF HIGH DIVIDEND 1
          LD      (HDEPTR),HL     ;HIGH DIVIDEND PTR = HIDE1
          LD      DE,HIDE2        ;DE = ADDRESS OF HIGH DIVIDEND 2
          LD      (ODEPTR),DE     ;OTHER DIVIDEND PTR = HIDE2
          SUB     A               ;GET 0 TO USE IN FILLING BUFFERS
                                   ;B = LENGTH IN BYTES
           ;FILL BOTH DIVIDEND BUFFERS WITH ZEROS
INITLP:
          LD          (HU ,A        ;ZERO BYTE OF HIDE1
          LD          <DE), A       ;ZERO BYTE OF HIDE2
          INC         HL
          INC         DE
          DJNZ        INITLP

          ;SET COUNT TO NUMBER OF DIGITS PLUS 1
          ; COUNT := (LENGTH * 2) + 1;
          LD      A,(LENGTH)      ;EXTEND LENGTH TO 16 BITS
          LD      L,A
          LD      H,O
          ADD     HL,HL           ;LENGTH * 2
          INC     HL              ;LENGTH * 2 + 1
          LD      (COUNT),HL      ;COUNT = LENGTH * 2 + 1
          ;CHECK FOR DIVIDE BY ZERO
          ; LOGICALLY OR ENTIRE DIVISOR TO SEE IF ALL BYTES ARE 0
          LD      HL,(DSADR)      ;HL = ADDRESS OF DIVISOR
          LD      A,(LENGTH)
          LD      B,A             ;B = LENGTH IN BYTES
          SUB     A               ;START LOGICAL OR WITH 0
DVOl :
          OR          (HU           ;OR NEXT BYTE OF DIVISOR
           INC        HL
          DJNZ        DVOl
          OR          A             ;TEST FOR ZERO DIVISOR
          ,-IR        Z,EREXIT      ; ERROR EX IT IF DIVISOR IS 0
          SUB         A
          LD          (NDIGIT), A   ; START NEXT DIGIT AT 0
           ;DIVIDE BY DETERMINING HOW MANY TIMES DIVISOR CAN
              BE SUBTRACTED FROM DIVIDEND FOR EACH DIGIT
              POSITION
                                  6L MULTIPLE-PRECISION DECIMAL DIVISION (MPDDIV)   263
DVLOOP:
          ; ROTATE LEFT LOWER DIVIDEND AND QUOTIENT:
          ; HIGH DIGIT OF NDIGIT BECOMES LEAST SIGNIFICANT DIGIT
          ; OF QUOTIENT (DIVIDEND ARRAY) AND MOST SIGNIFICANT DIGIT
          ; OF DIVIDEND ARRAY GOES TO HIGH DIGIT OF NDIGIT
          LD       HL,(DVADR)
          CALL     RLARY            ;ROTATE LOW DIVIDEND
          ; IF DIGIT COUNT = 0 THEN WE ARE DONE
          LD       HL, (COUNT)     ; DECREMENT COUNT BY 1
          DEC      HL
          LD        (COUNT),HL
          LD       A,H              ;TEST 16-BIT COUNT FOR 0
          OR       L
          JR        Z,OKEXIT        ;EXIT WHEN COUNT = 0
          ,
          ;ROTATE LEFT HIGH DIVIDEND, LEAST SIGNIFICANT DIGIT
          : OF HIGH DIVIDEND BECOMES HIGH DIGIT OF NDIGIT
          LD      HL,(HDEPTR)

          CALL    RLARY             ;ROTATE HIGH DIVIDEND

          ;SEE HOW MANY TIMES DIVISOR GOES INTO HIGH DIVIDEND
          ; ON EXIT FROM THIS LOOP, HIGH DIGIT OF NDIGIT IS NEXT
          : QUOTIENT DIGIT AND HIGH DIVIDEND IS REMAINDER
          SUB     A                ; CLEAR NUMBER OF TIMES INITIALLY
          LD      (NDIGIT),A

SUBLP:
          LD      HL, <DSADR)       ;HL POINTS TO DIVISOR
          LD      DE, (HDEPTR)      ;DE POINTS TO CURRENT HIGH DIVIDEND
          LD      BC, <ODEPTR.l     ;BC POINTS TO OTHER HIGH DIVIDEND
          LD      A, (LENGTH)
          LD      (CNT) ,A          ;LOOP COUNTER = LENGTH
          OR      A                 ;CLEAR CARRY INITIALLY

INNER:
          LD      A, <DE)           ;GET NEXT BYTE OF DIVIDEND
          SBC     A, (HU            ;SUBTRACT DIVISOR
          DAA                       ;CHANGE TO DECIMAL
          LD      (BC),A            ;STORE DIFFERENCE IN OTHER DIVIDEND
          INC     HL                ; INCREMENT TO NEXT BYTE
          INC     DE
          INC     BC
          LD      A, (CNT.l         ;DECREMENT COUNTER
          DEC     A
          LD      (CNT)' A
          ..IR    NZ,INNER          ;CONTINUE THROUGH ALL BYTES
          JR      C,DVLOOP          ;JUMP WHEN BORROW OCCURS
                                    : NDIGn IS NUMBER OF TIMES DIVISOR
                                    ; GOES INTO ORIGINAL HIGH DIVIDEND
                                    ;HIGH DIVIDEND CONTAINS REMAINDER
264      ARITHMETIC


          ;DIFFERENCE IS NOT NEGATIVE. SO ADD 1 TO
              NUMBER OF SUCCESSFUL SUBTRACTIONS
          • (LOW DIGIT OF NDIGIT)
          LD       HL.NDIGIT       :NDIGIT = NDIGIT +       1
          INC      (HU
          :EXCHANGE POINTERS. THUS MAKING DIFFERENCE NEW DIVIDEND
          LD      HL.(HDEPTR)
          LD      DE. (ODEPTR)
          LD      (HDEPTR).DE
          LD      (ODEPTR).HL
          JR      SUBLP           : CONTI NUE UNTI L 0 I FFERENCE NEG ATI VE
          :NO ERRORS. CLEAR CARRY
OKEXIT:
          OR          A             ;CLEAR CARRY. VALID RESULT
          RET
          :DIVIDE-BY-ZERO ERROR. SET CARRY
EREXIT:
          SCF                       ;SET CARRY. INVALID RESULT
          RET

          :***********************************
          ; SUBROUTINE: RLARY
          ; PURPOSE:    ROTATE LEFT AN ARRAY ONE DIGIT (4 BITS)
          ; ENTRY: HL = BASE ADDRESS OF ARRAY
          ;        LOW DIGIT OF NDIGIT IS DIGIT TO ROTATE THROUGH
          ;EXIT: ARRAY ROTATED LEFT THROUGH LOW DIGIT OF NDIGIT
          ;REGISTERS USED: AF. BC, DE, HL
           ;**********************************
RLARY:
          :SHIFT NDIGIT INTO LOW DIGIT OF ARRAY AND
          ; SHIFT ARRAY LEFT
          LD      A.<LENGTH)
          LD      B.A             ;B = LENGTH OF ARRAY IN BYTES
          LD      A.(NDIGIT)      ;A = NDIGIT
SHIFT:
          RLD                       ;SHIFT BYTE LEFT    1   DIGIT   (4   BITS)
          INC         HL
          DJNZ        SHIFT         ;CONTINUE UNTIL ALL BYTES SHIFTED
          LD          (NDIGIT>.A    :SAVE NEW NEXT DIGIT
          RET

          ; DATA
LENGTH:   OS          1             ;LENGTH OF ARRAYS IN BYTES
NDIGIT:   OS          1             :NEXT DIGIT IN ARRAY
CNT:      OS          1             :COUNTER FOR SUBTRACT LOOP
DVADR:    OS          2             ;DIVIDEND ADDRESS
DSADR:    OS          2             ;DIVISOR ADDRESS
HDEPTR:   OS          2             ;HIGH DIVIDEND POINTER
ODEPTR:   OS          2             :OTHER DIVIDEND POINTER
                                  6L MULTIPLE-PRECISION DECIMAL DIVISION (MPDDIV)   265
COUNT:   DS      2                  ;DIVIDE LOOP COUNTER
HIDE1:   DS      255                ;HIGH DIVIDEND BUFFER 1
HIDE2:   OS      255                ;HIGH DIVIDEND BUFFER 2



         SAMPLE EXECUTION:


SC6L:
         LD      HL,AY1             ;BASE ADDRESS OF DIVIDEND
         LD      DE,AY2             ;BASE ADDRESS OF DIVISOR
         LD      B,SZAYS            ;LENGTH OF ARRAYS IN BYTES
         CALL    MPDDIV             ;MULTIPLE-PRECISION BCD DIVISION
                                    ;RESULT OF 1522756 / 1234 = 1234
                                       IN MEMORY AYI    = 34H
                                                 AY1+1    12H
                                                 AYl+2    OOH
                                                 AY1+3  = OOH
                                                 AYl+4  = OOH
                                                 AYl+5    OOH
                                                 AY1+6    OOH
         JR      SC6L
SZAYS    EQU     7         ; LENGTH OF ARRAYS IN BYTES
         AYl:
                 DB        056H
                 DB        027H
                 DB        052H
                 DB        01H
                 DB        0
                 DB        0
                 DB        0
         AY2:
                 DB        034H
                 DB        012H
                 DB        0
                 DB        0
                 DB        0
                 DB        0
                 DB        0

                 END
Multiple-Precision Decimal Comparison                                                                            6M

   Compares two multi-byte unsigned decimal                    subtrahend had been subtracted from the
(BCD) numbers and sets the Carry and Zero                      minuend.
flags appropriately. The Zero flag is set to I ifthe             Note: This program is exactly the same as
operands are equal and to 0 ifthey are not equal.              Subroutine 6H, the mUltiple-precision binary
The Carry flag is set to I if the subtrahend is                comparison, since the form of the operands does
larger than the minuend; the Carry flag is                     not matter if they are only being compared. See
cleared otherwise. Thus the flags are set as if the            Subroutine 6H for a listing and other details.




Examples
1.    Data:    Length of operands (in bytes) = 6               3.    Data:    Length of operands (in bytes) = 6
               Subtrahend = 196528719340,6                                    Subtrahend = 196528719340'6
               Minuend = 456780153266'6                                       Minuend = 073785991074'6
     Result:   Zero flag = 0 (operands are not equal)               Result:   Zero flag = 0 (operands are not equal)
               Carry flag = 0 (subtrahend is not larger than                  Carry flag = I (subtrahend is larger than
                 minuend)                                                       minuend)

2.    Data:    Length of operands (in bytes) = 6
               Subtrahend = 196528719340'6
               Minuend = 196528719340'6
     Result:   Zero flag = I (operands are equal)
               Carry flag = 0 (subtrahend is not larger than
                 minuend)


