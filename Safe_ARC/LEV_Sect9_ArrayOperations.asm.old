
Array Operations
9A    8-Bit Array Summation 319
9B    l6-Bit Array Summation 322
9C    Find Maximum Byte-Length Element 325
9D    Find Minimum Byte-Length Element 328
9E    Binary Search    331
9F     Quicksort 336
9G     RAM Test 347
9H     Jump Table    352

8-Bit Array Summation (ASUMS)                                                                        9A

    Adds the elements of an array, producing a          Registers Used: AF, B, DE, HL
16-bit sum. The array consists of up to 255 byte-
                                                        Execution nme: Approximately 38 cycles per byte-
length elements.                                        length element plus 49 cycles overhead
   Procedure: The program clears the sum initial-
ly. It then adds elements one at a time to the less     Program Size: 19 bytes
significant byte of the sum, starting at the base       Data Memory Required: None
address. Whenever an addition produces a carry,
                                                        Special Case: An array size of 0 causes an imme-
the program increments the more significant             diate exit with the sum equal to O.
byte of the sum.




Entry Conditions                                      Exit Conditions
Base address of array in HL                           Sum in HL
Size of array in bytes in B




Example
I.    Data:    Array consists of
                F7 16      5A I6
                23 16      16 16
                31 16      CB I6
                70 16      El 16

     Result:   Sum = (HL) = 0307 16




               Title                  8-bit ayyay su.mation
               Name:                  ASUM8



               Puypose:               Sum the elements of an ayyay, yielding a 16-blt
                                      yesult. Maximum size is 255

                                                                                                    319
320       ARRAY OPERATIONS


           Enh-y:              Register pair HL = Base address of array
                               Register B = Size of array in bytes
           Ex it:              Register pair HL      = Sum
           Registers used: AF.B.DE.HL
           Time:               Approximately 38 cycles per element plus
                               49 cycles overhead
           Size:               Program 19 bytes


ASUM8:
           ;TEST ARRAY LENGTH
           ;EXIT WITH SUM = 0 IF NOTHING IN ARRAY
           EX      DE.HL           ;SAVE BASE ADDRESS OF ARRAY
           LD      HL.O            ; INITIALIZE SUM TO 0
           ;CHECK FOR LENGTH OF ZERO
           LD      A.B             ; TEST ARRAY LENGTH
           OR      A
           RET     Z               ;EXIT WITH SUM = 0 IF LENGTH         =0
           ; INITIALIZE ARRAY POINTER. SUM
           EX       DE.HL           ;RESTORE BASE ADDRESS OF ARRAY
                                    ; HIGH BYTE OF SUM = 0
           SUB      A               ;A = LOW BYTE OF SUM = 0
                                    ;D = HIGH BYTE OF SUM
           ;ADD BYTE-LENGTH ELEMENTS TO SUM ONE AT A TIME
           ;INCREMENT HIGH BYTE OF SUM WHENEVER A CARRY OCCURS
SUMLP:
           ADD        A. (HU           ;ADD NEXT BYTE
           JR         NC.DECCNT        ;JUMP IF NO CARRY
           INC        D                ; ELSE INCREMENT HIGH BYTE OF SUM
DECCNT:
           INC        HL
           D•...INZ   SUMLP
EXIT:
           LD         L,A              ;HL   = SUM
           LD         H.D
           RET



           SAMPLE EXECUTION


SC9A:
           LD         HL,BUF           ;HL     BASE ADDRESS OF BUFFER
           LD         A, (BUFSZ)
                                          9A S-BIT ARRAY SUMMATION (ASUMS)   321
        LD      B,A             ;B   = SIZE   OF BUFFER IN BYTES
        CALL    ASUM8
                                ;SUM OF TEST DATA IS 07F8 HEX,
                                ; HL = 07F8H
        JR      SC9A
;TEST DATA, CHANGE FOR OTHER VALUES
SIZE    EQU     010H            ;SIZE OF BUFFER IN BYTES
BUFSZ: DB       SIZE            ,SIZE OF BUFFER IN BYTES
BUF:    DB      OOH             ; BUFFER
        DB      llH             ;DECIMAL ELEMENTS ARE 0,17,34,51,68
        DB      22H             ; 85,102.119.135,153.170.187.204
        DB      33H             , 221.238.255
        DB      44H
        DB      55H
        DB      66H
        DB      77H
        DB      88H
        DB      99H
        DB      OAAH
        DB      OBBH
        DB      OCCH
        DB      ODDH
        DB      OEEH
        DB      OFFH            ,SUM   = 07F8   (2040 DECIMAL)

        END
16-Bit Array Summation (ASUM16)                                                                       9B

   Adds the elements of an array, producing a
24-bit sum. The array consists of up to 255 word-        Registers Used: AF, BC, DE, HL
length (16-bit) elements. The elements are ar-
                                                         Execution nme: Approximately 68 cycles per 16-
ranged in the usual Z80 format with the less             bit element plus 49 cycles overhead
significant bytes first.
   Procedure: The program clears the sum initial-        Program Size: 25 bytes
ly. It then adds elements to the less significant
                                                         Data Memory Required: None
bytes of the sum one at a time, starting at the
base address. Whenever an addition produces a            Special Case: An array size of 0 causes an imme-
carry, the program increments the most signifi-          diate exit with the sum equal to O.
cant byte of the sum.



Entry Conditions                                      Exit Conditions
Base address of array in HL                           Most significant byte of sum in E
Size of array in 16-bit words in B                    Middle and least significant bytes of sum in HL



Example
1.    Data:     Array (in 16-bit words) consists of
                  F7Al 16         5A36 16
                  239B 16         166C 16
                  3105 16         CBF5 16
                  70F2 16         E107 16

     Result:    Sum= 03DBAl 16
                (E) = 03 16
                (HL) = DBAl 16




               Title                    16-bit array summation
               Name:                    ASUM16



322
                                               98 16-81T ARRAY SUMMATION (ASUM16)   323

          Purpose:           Sum the elements of an array. yielding a 24-bit
                             result. Maximum size is 255 16-bit elements
          Entry:             Register pair HL = Base address of array
                             Register B = Size of array in words
          Exit :             Register A      High byte of sum
                             Register H    = Middle byte of sum
                             Register L      Low byte of sum
          Registers used: AF,BC,DE,HL
          Time:              Approximately 68 cycles per element plus
                             49 cycles overhead
          Size:              Program 25 bytes


ASUM16:
          ; TEST ARRAY LENGTH
          ;EXIT WITH SUM = 0 IF NOTHING IN ARRAY
          EX       DE,HL          ;SAVE BASE ADDRESS OF ARRAY
          LD       HL,O           ~INITIALIZE SUM TO 0

          ; CHECK FOR ARRAY LENGTH OF ZERO
          LD       A,B             ;TEST ARRAY LENGTH
          OR       A
          RET      Z               ;EXIT WITH SUM = 0 IF LENGTH            o
          ; INITIALIZE ARRAY POINTER, SUM
          EX       DE,HL           ;BASE ADDRESS BACK TO HL
                                   ; LOW, MIDDLE BYTES OF SUM          o
          LD       C,E             ;C = HIGH BYTE OF SUM = 0
                                   ;D = MIDDLE BYTE OF SUM
                                   ;E = LOW BYTE OF SUM
          ;ADD WORD-LENGTH ELEMENTS TO SUM ONE AT A TIME
          ; INCREMENT HIGH BYTE OF SUM WHENEVER A CARRY OCCURS
SUMLP:
          LD         A,E             ;ADD LOW BYTES OF ELEMENT AND SUM
          ADD        A, (HU
          LD         E,A
           INC       HL              ;ADD HIGH BYTE OF ELEMENT TO
          LD         A,D             ; MIDDLE BYTE OF SUM
          ADC        A, (HL)
          LD         D,A
          ,JR        NC,DECCNT       ;JUMP IF NO CARRY
           INC       C               ; ELSE INCREMENT HIGH BYTE OF SUM
DECCNT:
          INC        HL
          DJNZ       SUMLP
EXIT:
          EX         DE,HL           ;HL     MIDDLE AND LOW BYTES OF SUM
324     ARRAY OPERATIONS


         LD       A,C           ;A      HIGH BYTE OF SUM
         RET




         SAMPLE EXECUTION


SC9B:
         LD       HL,BUF        ;HL = BASE ADDRESS OF BUFFER
         LD       A, (BUFSZ.l
         LD       B,A           ;B = SIZE OF BUFFER IN WORDS
         CALL     ASUM16
                                ;SUM OF TEST DATA IS 31FF8 HEX,
                                ; REGISTER PAIR HL = IFF8H
                                ; REGISTER A = 3

         •.JR     SC9B
;TEST DATA, CHANGE FOR OTHER VALUES
SIZE    EQU     010H            ;SIZE OF BUFFER IN WORDS
BUFSZ: DB       SIZE            ;SIZE OF BUFFER IN WORDS
BUF:     DW       OOOH          ; BUFFER
         DW       111H          ;DECIMAL ELEMENTS ARE 0,273,546,819,1092
         DW       222K          ; 1365,1638,1911,2184,2457,2730,3003,3276
         DW       333H          ; 56797,61166,65535
         DW       444H
         DW       555H
         DW       666H
         DW       777H
         DW       888H
         DW       999H
         DW       OAAAH
         OW       OBBSH
         OW       OCCCH
         OW       ODDDDH
         OW       OEEEEH
         OW       OFFFFH        ; SUM    31FF8 (204792 DECIMAU
         END
Find Maximum Byte-Length
Element (MAXELM)                                                                                                     9C

   Finds the maximum element in an array. The                     Registers Used: AF, B, DE, HL
array consists of up to 255 unsigned byte-length                  Execution Time: Approximately 36 to 58 cycles per
elements.                                                         element plus 35 cycles overhead. If, on the average,
                                                                  the program must replace the maximum in half of
   Procedure: The program exits immediatel~7                      the iterations, the execution time is approximately
(setting Carry to 1) if the array has no elements.                94 * ARRAY SIZEj2 + 35 cycles.
Otherwise, the program assumes that the ele-                      Program Size: 19 bytes
ment at the base address is the maximum. It then                  Data Memory Required: None
                                                                  Special Cases:
proceeds through the array, comparing the sup-
                                                                     I. An array size of 0 causes an immediate exit
posed maximum with each element and retaining                     with the Carry flag set to I to indicate an invalid
the larger value and its address. Finally, the                    result.
program clears Carry to indicate a valid result.                    2. If the largest unsigned value occurs more than
                                                                  once, the program returns with the lowest possible
                                                                  address. That is, it returns with the address closest to
                                                                  the base address that contains the maximum value.




Entry Conditions                                               Exit Conditions
Base address of array in HL                                    Largest unsigned element in A
Size of array in bytes in B                                    Address of largest unsigned element in HL
                                                               Carry = 0 if result is valid; 1 if size of array is 0
                                                                 and result is meaningless.




Example
1.    Data:    Array (in bytes) consists of
                35,6       44'6
                A6'6       59'6
                D2'6       7A'6
                 IB'6       CF'6

     Result:   The largest unsigned element is element #2
                 (02'6)
               (A) = largest element (02'6)
               (HL) = BASE + 2 (lowest address contain-
                 ing 02,6)
               Carry flag = 0, indicating that array size is
                 non-zero and the result is valid.


                                                                                                                     325
326        ARRAY OPERATIONS




            Tit     Ie            Find maximum byte-length element
            Name:                 MAX ELM



            Purpose:              Given the base address and size of an array.
                                  find the largest element
            Entry:                Register pair HL = Base address of array
                                  Register B = Size of array in bytes
            Exit:                 If size of array not zero then
                                    Carry flag = 0
                                    Register A = Largest element
                                    Re~ister pair HL = Address of that element
                                     if there are duplicate values of the largest
                                     element. register pair HL has the address
                                     nearest to the base address
                                  else
                                    Carry flag • 1
            Registers used: AF.B.DE.HL
            Time:                 Approximately 36 to 58 cycles per element
                                  plus 35 cycles overhead
            Size:                 Program 19 bytes


MAXELM:
            ,EXIT WITH CARRY SET IF NO ELEMENTS IN ARRAY
            I-D     A.B             ,TEST ARRAY SIZE
            OR      A
            SCF                     ,SET CARRY TO INDICATE ERROR EXIT
            RET     Z               ,RETURN IF NO ELEMENTS
            ,REPLACE PREVIOUS GUESS AT LARGEST ELEMENT WITH
            , CURRENT ELEMENT. FIRST TIME THROUGH. TAKE FIRST
            , ELEMENT AS GUESS AT LARGEST
MAXLPI      LD      A.(HL)          ,LARGEST = CURRENT ELEMENT
            LD      E.L             ,SAVE ADDRESS OF LARGEST
            LD      D.H
            ,COMPARE CURRENT ELEMENT TO LARGEST
            ,KEEP LOOKING UNLESS CURRENT ELEMENT IS LARGER
MAXLPl :
            DEC          B
            ....R        Z.EXIT
             INC         HL
                             9C FIND MAXIMUM BYTE-LENGTH ELEMENT (MAXELM)   327
        CP       (HU           ;COMPARE CURRENT ELEMENT, LARGEST
        •...IR   NC,MAXLPl     ;CONTINUE UNLESS CURRENT ELEMENT LARGER
        JR       MAXLP         ;ELSE CHANGE LARGEST
EXIT:
        OR       A             ;CLEAR CARRY TO INDICATE NO ERRORS
        EX       DE,HL         ;HL = ADDRESS OF LARGEST ELEMENT
        RET



        SAMPLE EXECUTION:


SC9C:
        LD       HL,ARY        ;HL = BASE ADDRESS OF ARRAY
        LD       B,SZARY       ;B = SIZE OF ARRAY IN BYTES
        CALL     MAX ELM
                               ;RESULT FOR TEST DATA IS
                               ; A = FF HEX (MAXIMUM), HL    = ADDRESS   OF
                               ; FF IN ARY
        JR       SC9C          ;LOOP FOR MORE TESTING
SZARY   EQU      10H           ;SIZE OF ARRAY IN BYTES
ARY:    DB       8
        DB       7
        DB       6
        DB       5
        DB       4
        DB       3
        DB       2
        DB       1
        DB       OFFH
        DB       OFEH
        DB       OFDH
        DB       OFCH
        DB       OFBH
        DB       OFAH
        DB       OF9H
        DB       OF8H
        END
Find Minimum Byte-Length
Element (MINELM)                                                                                                   9[)


   Finds the minimum element in an array. The                     Registers Used: AF, B, DE, HL
array consists of up to 255 unsigned byte-length                  execution Time: Approximately 36 to 65 cycles per
elements.                                                         element plus 35 cycles overhead. If, on the average,
   Procedure: The program exits immediately                       the program must replace the minimum in half of
                                                                  the iterations, the execution time is approximately
(setting Carry to 1) ifthe array has no elements.                 101 * ARRAY SIZE/2 +35 cycles.
Otherwise, the program assumes that the ele-                     Program Size: 21 bytes
ment at the base address is the minimum. It then                 Data Memory Required: None
proceeds through the array, comparing the sup-
                                                                 Special Cases:
posed minimum to each element and retaining                         1. An array size of 0 causes an immediate exit
the smaller value and its address. Finally, the                  with the Carry flag set to 1 to indicate an invalid
program clears Carry to indicate a valid result.                 result.
                                                                    2. If the smallest unsigned value occurs more
                                                                 than once, the program returns with the lowest pos-
                                                                 sible address. That is, it returns with the address
                                                                 closest to the base address that contains the min-
                                                                 imum value.




Entry Conditions                                               Exit Conditions
Base address of array in HL                                    Smallest unsigned element in A
Size of array in bytes in B                                    Address of smallest unsigned element in HL
                                                               Carry = 0 if result is valid; 1 if size of array is 0
                                                                 and result is meaningless.




Example
l.    Data:    Array (in bytes) consists of
                 35'6            44'6
                 A6'6            59'6
                 02'6            7A'6
                 iB'6            CF'6

     Result:   The smallest unsigned element is element #3
                 (lB'6)
               (A) = smallest element (IB'6)
               (HL) = BASE + 3 (lowest address contain-
                    ing lB'6)
               Carry flag = 0, indicating that array size is
                 non-zero and the result is valid.


328
                                         90 FINO MINIMUM BYTE-LENGTH ELEMENT (MINELM)   329



           Title                Find minimum byte-length element
           Name:                MINELM




           Purpose:             Given the base address and size of an array,
                                find the smallest element
           Entry:               Register pair HL = Base address of array
                                Register B = Size of array in bytes
           Exit:                If size of array not zero then
                                  Carry flag = 0
                                  Register A = Smallest element
                                  Register pair HL = Address of that element
                                   if there are duplicate values of the smallest;
                                   element, HL will have the address
                                   nearest to the base address
                                else
                                   Carry flag = 1
           Registers used: AF,B,DE,HL
           Time:                Approximately 36 to 65 cycles per element
                                plus 35 cycles overhead
           Size:                Program 21 bytes


MINELM:
           ;EXIT WITH CARRY SET IF NO ELEMENTS IN ARRAY
           LD      A,B             ;TEST ARRAY SIZE
           OR      A
           SCF                     ;SET CARRY TO INDICATE AN ERROR EXIT
           RET     Z               ;RETURN IF NO ELEMENTS
           ;REPLACE PREVIOUS GUESS AT SMALLEST ELEMENT WITH
           ; CURRENT ELEMENT. FIRST TIME THROUGH, TAKE FIRST
           ; ELEMENT AS GUESS AT SMALLEST
MINLP:     LD      A,(HL)          ;SMALLEST = CURRENT ELEMENT
           LD      E,L             ;SAVE ADDRESS OF SMALLEST
           LD      D,H
           ;COMPARE CURRENT ELEMENT TO SMALLEST
           ;KEEP LOOKING UNLESS CURRENT ELEMENT IS SMALLER
MINLP1 :
           DEC        B
           ,JR        Z, EXIT
330     ARRAY OPERATIONS


         INC      HL
         CP       (HU        ;COMPARE CURRENT ELEMENT, SMALLEST
         JR       C,MINLPl   ;CONTINUE IF CURRENT ELEMENT LARGER
         JR       Z,MINLPl   ; OR SAME
         JR       MINLP      ;ELSE CHANGE SMALLEST
EXIT:
         OR       A          ;CLEAR CARRY TO INDICATE NO ERRORS
         EX       DE,HL      ;HL = ADDRESS OF SMALLEST ELEMENT
         RET



         SAMPLE EXECUTION:


SC9D:
         LD       HL,ARY     ;HL = BASE ADDRESS OF ARRAY
         LD       B,SZARY    ;B = SIZE OF ARRAY IN BYTES
         CALL     MINELM
                             ;RESULT FOR TEST DATA IS
                             ; A = 1 HEX (MINIMUM), HL   = ADDRESS   OF
                             ; 1 IN ARY
         JR       SC9D       ;LOOP FOR MORE TESTING
SZARY    EQU      10H        ;SIZE OF ARRAY IN BYTES
ARY:     DB       S
         DB       7
         DB       6
         DB       5
         DB       4
         DB       3
         DB       2
         DB       1
         DB       OFFH
         DB       OFEH
         DB       OFDH
         DB       OFCH
         DB       OFBH
         DB       OFAH
         DB       OF9H
         DB       OF8H
          END
Binary Search (BINSCH)                                                                                                 9E

    Searches an array of unsigned byte-length
                                                                     Registers Used: AF, BC, DE, HL
elements for a particular value. The elements are
                                                                     ExecuHon Time: Approximately 114 cycles per
assumed to be arranged in increasing order.                          iteration plus 53 cycles overhead. A binary search
Clears Carry if it finds the value and sets Carry                    requires on the order oflog2 N iterations, where N is
to I if it does not. Returns the address of the                      the number of elements in the array.
value if found. The size of the array is specified                   Program Size: 37 bytes
and is a maximum of 255 bytes.                                       Data Memory Required: None
    Procedure: The program performs a binary                         Special Case: A size of 0 causes an immediate exit
search, repeatedly comparing the value with the                      with the Carry flag set to I. That is, the array con-
                                                                     tains no elements and the value surely cannot be
middle remaining element. After each compari-                        found.
son, the program discards the part of the array
that cannot contain the value (because of the                       LOWER BOUND = BASE
ordering). The program retains upper and lower                      UPPER BOUND = GUESS - I = BASE + 6
bounds for the remaining part. If the value is                      GUESS = (UPPER BOUND + LOWER BOUND)/2
larger than the middle element, the program                           = BASE + 3
discards the middle and everything below it. The                    (GUESS) = ARRAY(3) = 07
new lower bound is the address of the middle
element plus 1. If the value is smaller than the                    Since the value (ODI6) is greater than ARRAY(3),
middle element, the program discards the mid-                     the elements below #4 can be discarded. So the
dle and everything above it. The new upper                        result is
bound is the address of the middle element                          LOWER BOUND = GUESS + I = BASE + 4
minus I. The program exits if it finds a match or                   UPPER BOUND = BASE + 6
if there is nothing left to search.                                 GUESS = (UPPER BOUND + LOWER BOUND)/2
    For example, assume that the array is                             = BASE + 5
  0116,0216,0516,0716,0916,0916, OD I6 , 10 16 ,                    (GUESS) = ARRAY(5) = 09
  2E 16 , 37 16 , 5D 16 , 7E 16 , A1 16 , B4 16 , D7 16 , E0 16
                                                                    Since the value (ODI6) is greater than ARRAY(5),
and the value to be found is ODI6. The proce-                     the elements below #6 can be discarded. So the
dure works as follows.                                            result is
  In the first iteration, the lower bound is the
                                                                    LOWER BOUND = GUESS + I = BASE + 6
base address and the upper bound is the address
                                                                    UPPER BOUND = BASE + 6
of the last element. So the result is
                                                                    GUESS = (UPPER BOUND + LOWER BOUND)/2
  LOWER BOUND = BASE                                                  =06
  UPPER BOUND= BASE+ SIZE- 1= BASE+ OF 16                           (GUESS) = ARRAY(6) = OD 16
  GUESS = (UPPER BOUND + LOWER BOUND)/2
    (the result is truncated) = BASE + 7                             Since the value (ODI6) is equal to ARRAY(6),
  (GUESS) = ARRAY(7) = 10 16 = 16 10                              the element has been found. If, on the other
   Since the value (ODI6) is less than ARRAY(7),                  hand, the value were OEI6, the new lower bound
the elements beyond #6 can be discarded. So the                   would be BASE + 7 and there would be nothing
result is                                                         left to search.

                                                                                                                     331
332            ARRAY OPERATIONS


Entry Conditions                                              Exit Conditions
Value to find in A                                            Carry = 0 if the value is found; 1 if it is not
Size of the array in bytes in C                                  found.
Base address of array (address of smallest                    If the value is found, (HL) = its address.
  unsigned element) in HL




Examples
Length of array = 10'6                                        2.    Data:    Value to find = 9B'6
Elements of array are 01'6,02'6,05'6,07'6,09'6.09'6' OD'6'
10,6, 2E'6' 37,6, 5D'6' 7E'6' AI'6' B4'6' D7'6' EO'6               Result:   Carry = I, indicating value not found

I.    Data:       Value to find = OD'6

     Result:      Carry = 0, indicating value found
                  (HL) = BASE + 6 (address containing OD'6)




                Title                    Binary search
                Name:                    BINSCH




                Purpose:                 Search an ordered array of unsigned bytes
                                         with a maximum size of 255 elements
                Entry:                   Register pair HL = Base address of array
                                         Register C = Size of array
                                         Register A = Byte to find
                Exit:                    If the value is found then
                                           Carry flag   0    =
                                           Register pair HL = Address of value
                                         ELSE
                                           Carry flag = 1

                Registers used: AF,BC,DE,HL
                                                   9E BINARY SEARCH (BINSCH)   333

          Time:           Approximately 114 cycles for each iteration of
                          the search loop plus 53 cycles overhead
                          A binary search takes on the order of log
                          base 2 of N searches. where N is the number of
                          elements in the array.
          Size:           Program 37 bytes


BINSCH:
          ,EXIT WITH CARRY SET IF NO ELEMENTS IN ARRAY
          INC     C               :TEST ARRAY SIZE
          DEC     C
          SCF                     :SET CARRY IN CASE SIZE IS 0
          RET     Z               ,RETURN INDICATING VALUE "NOT FOUND
                                  , IF SIZE IS 0
          : INITIALIZE LOWER BOUND. UPPER BOUND OF SEARCH AREA
          ,LOWER BOUND (DE) = BASE ADDRESS
          ;UPPER BOUND (HL) = ADDRESS OF LAST ELEMENT
          ; = BASE ADDRESS + SIZE - 1
          LD       E.L             :LOWER BOUND = BASE ADDRESS
          LD       D.H
          LD       B.O             ,EXTEND SIZE TO 16 BITS
          ADD      HL.BC           ,UPPER BOUND = BASE + SIZE - 1
          DEC      HL
          ,SAVE VALUE BEING SOUGHT
          LD      C. A             ,SAVE VALUE
          ,ITERATION OF BINARY SEARCH
          :1) COMPARE VALUE TO MIDDLE ELEMENT
          :2) IF THEY ARE NOT EQUAL. DISCARD HALF THAT
          •    CANNOT POSSIBLY CONTAIN VALUE (BECAUSE OF ORDERING)
          :3) CONTINUE IF THERE IS ANYTHING LEFT TO SEARCH
LOOP:
          ;HL = UPPER BOUND
          ;DE = LOWER BOUND
          ;C = VALUE TO FIND
          ,FIND MIDDLE ELEMENT
          ,MIDDLE = (UPPER BOUND + LOWER BOUND) / 2
          PUSH    HL              :SAVE UPPER BOUND ON STACK
          ADD     HL.DE           :ADD UPPER BOUND AND LOWER BOUND
          RR      H               ,DIVIDE 17-BIT SUM BY 2
          RR      L
          LD      A. (HU          ;GET MIDDLE ELEMENT
          ,COMPARE MIDDLE ELEMENT AND VALUE
          CP      C               ,COMPARE MIDDLE ELEMENT AND VALUE
          ~R      NC.TOOLRG       :~UMP IF VALUE SAME OR LARGER


          ,MIDDLE ELEMENT LESS THAN VALUE
334     ARRAY OPERATIONS


          , 50 CHANGE LOWER BOUND TO MIDDLE + 1
          , SINCE EVERYTHING BELOW MIDDLE IS EVEN SMALLER
          EX      DE.HL           ,LOWER BOUND = MIDDLE +   1
          INC     DE
          POP     HL              ,RESTORE UPPER BOUND
          JR      CONT
          ,MIDDLE ELEMENT GREATER THAN OR EQUAL TO VALUE
          , SO CHANGE UPPER BOUND TO MIDDLE - 1
          , SINCE EVERYTHING ABOVE MIDDLE IS EVEN LARGER
          ,EXIT WITH CARRY CLEAR IF VALUE FOUND
TOOLRG:
          INC       SP              ,DISCARD OLD UPPER BOUND FROM STACK
          INC       SP
          RET       Z               ,IF MIDDLE ELEMENT SAME AS VALUE
                                    , RETURN WITH CARRY CLEAR
                                    , AND HL = ADDRESS CONTAINING VALUE
          DEC       HL              ,UPPER BOUND = MIDDLE - 1
          ,CONTINUE IF THERE IS ANYTHING LEFT TO BE SEARCHED
          ,NOTHING LEFT WHEN LOWER BOUND ABOVE UPPER BOUND
CONTI
          LD        A.L             ,FORM UPPER BOUND - LOWER BOUND
          CP        E               , MUST SAVE BOTH. SO USE 8-BIT SUBTRACT
          LD        A.H
          SBC       A.D
          JR        NC.LOOP         ,CONTINUE IF ANYTHING LEFT TO SEARCH
          ,NOTHING LEFT TO SEARCH SO COULD NOT FIND VALUE
          ,RETURN WITH CARRY SET (MUST BE OR JR NC WOULD HAVE BRANCHED)
          RET



          SAMPLE EXECUTION


5C9E:
          ,SEARCH   FOR A VALUE THAT IS IN THE ARRAY
          LD        HL.BF           ,HL • BASE ADDRESS OF ARRAY
          LD        A.(BFSZ)
          LD        C.A             ,C = ARRAY SIZE IN BYTES
          LD        A.7             ,A = VALUE TO FIND
          CALL      BINSCH          ,SEARCH
                                    ,CARRY FLAG = 0 (VALUE FOUND)
                                    JHL • BF + 4 (ADDRESS OF 7 IN ARRAY)

          ,SEARCH   FOR A VALUE THAT IS NOT IN THE ARRAY
          LD        HL.BF           JHL • BASE ADDRESS OF ARRAY
          LD        A.(BFSZ)
          LD        C.A             ,C = ARRAY SIZE IN BYTES
          LD        A.O             ,A = VALUE TO FIND
          CALL      BINSCH          ,SEARCH
                                    ,CARRY FLAG = 1 (VALUE NOT FOUND)
                                       9E BINARY SEARCH (BINSCH)   335
        JR       SC9E   .LOOP FOR MORE TESTS
        • DATA
SIZE    EQU      010H   ,SIZE OF ARRAY IN BYTES
BFSZI   DB       SIZE   ,SIZE OF ARRAY IN BYTES
BF:     DB       1      ,BUFFER
        DB       2
        DB       4
        DB       5
        DB       7
        DB       9
        DB       10
        DB       11
        DB       23
        DB       50
        DB       81
        DB       123
        DB       191
        DB       199
        DB       250
        DB       255
        END
Quicksort (QSORT)                                                                                              9F

   Arranges an array of unsigned word-length             Registers Used: AF, BC, DE, HL
elements into ascending order using a quicksort          Execution Time: Approximately N * log2N loops
algorithm. Each iteration selects an element and         through PARTLP plus 2 * N + I overhead calls
divides the array into two parts, one consisting         to SORT. Each iteration of PARTLP takes approx-
                                                         imately 200 cycles and each overhead call to SORT
of elements larger than the selected element and         takes approximately 300 cycles. Thus, the total
the other consisting of elements smaller than the        execution time is on the order of 200 * N * log2N + 300
selected element. Elements equal to the selected         * (2 * N + I).
element may end up in either part. The parts are         Program Size: 206 bytes
then sorted recursively in the same way. The             Data Memory Required: 8 bytes anywhere in
algorithm continues until all parts contain either       RAM for pointers to the first and last elements of a
                                                         partition (2 bytes starting at addresses FIRST and
no elements or only one element. An alternative          LAST, respectively), a pointer to the bottom of the
is to stop recursion when a part contains few            stack (2 bytes starting at address STKBTM), and the
enough elements (say, less than 20) to make a            original value of the stack pointer (2 bytes starting at
                                                         address OLDSP).
bubble sort practical.
                                                         Special case: If the stack overflows (i.e., comes
   The parameters are the array's base address,          too close to its boundary), the program exits with
the address of its last element, and the lowest          the Carry flag set to I.
available stack address. The array can thus
occupy all available memory, as long as there is
room for the stack. Since the procedures that
                                                      parts, dividing them further into parts and stop-
obtain the selected element, compare elements,
                                                      ping when a part contains no elements or only
move forward and backward in the array, and
                                                      one element. Since each recursion places six
swap elements are all subroutines, they could be
                                                      bytes on the stack, the program must guard
changed readily to handle other types of elements.
                                                      against stack overflow by checking whether the
   Ideally, quicksort should divide the array in
                                                      stack has grown to within a small buffer of its
half during each iteration. How closely the
                                                      lowest available address.
procedure approaches this ideal depends on
                                                         Note that the selected element always ends up
how well the selected element is chosen. Since
                                                      in the correct position after an iteration. There-
this element serves as a midpoint or pivot, the
                                                      fore, it need not be included in either partition.
best choice would be the ~entral value (or
                                                         The rules for choosing the middle element are
median). Of course, the true median is unknown.
                                                      as follows, assuming that the first element is #1:
A simple but reasonable approximation is to
select the median of the first, middle, and last          1. If the array has an odd number of ele-
elements.                                             ments, take the one in the center. For example,
                                                      if the array has 11 elements, take #6.
   Procedure: The program first deals with the
entire array. It selects the median of the current       2. If the array has an even number of ele-
first, last, and middle elements to use in dividing   ments and its base address is even, take the
the array. It moves that element to the first         element on the lower (base address) side of the
position and divides the array into two parts or      center. For example, if the array starts in 030016
partitions. It then operates recursively on the       and has 12 elements, take #6.

336
                                                                                                9F QUICKSORT (QSORT)                  337

   3. If the array has an even number of ele-
ments and its base address is odd, take the
element on the upper side of the center. For
example, if the array starts in 030116 and has 12
elements, take #7.




Entry Conditions                                                             Exit Conditions
Base address of array in HL                                                  Array sorted into ascending order, considering
Address of last word of array in DE                                          the elements as unsigned words. Thus, the
Lowest available stack address in BC                                         smallest unsigned word ends up stored starting
                                                                             at the base address. Carry = 0 if the stack did
                                                                             not overflow and the result is proper. Carry = 1
                                                                             if the stack overflowed and the final array is
                                                                             not sorted.




Example
I.    Data:    Length (size) of array = OC 16                                           now in the correct position and need not be
               Elements = 2B 16 • 57 16 , ID I6 , 26 16 ,                               included in either partition.
                 22 16 , 2E 16 , OC I6 , 44 16 ,                                           The first partition may now be sorted recur-
                 17 16 , 4B 16 , 37 16 , 27 16                                          sively in the same way:

     Result:   The result of the first iteration is:                                      Selected element = 'median of the first
                                                                                          (#1 = 27 16 ), middle (#3 = I D I6 ), and last
                  Selected element = median of the first                                  (#7 = OC I6 ) elements. Here, #4 is the
                  (#1 = 2B I6 ), middle (#6 = 2E I6 ), and last                           median and must be exchanged initially
                  (#12 = 27 16 ) elements. The selected ele-                              with #1.
                  ment is therefore # I (2B I6 ), and no swap-
                  ping is necessary since it is already in the                            The final order of the elements in the first
                  first position.                                                       partition is
               At the end of the iteration, the array is
                     27 16 ,17 16 , ID I6 , 26 16 ,
                     22 16 , OC I6 , 2B 16 , 44 16 ,                                      The first partition of the first partition
                     2E 16 , 4B 16 , 37 16 , 57 16 ,                                   (consisting of elements less than ID I6 ) is
                                                                                       OC I6 , 17 16 , This will be referred to as the (I, I)
                  The first partition, consisting of elements                          partition.
               less than 2B 16 , is 27 16 ,17 16 , ID I6 , 26 16 , 22 16 ,                The second partition of the first partitior.
               and OC I6 .                                                             (consisting of elements greater than I D 16) is
                  The second partition, consisting of ele-                             26 16 , 22 16 , and 27 16 ,
               ments greater than 2B 16 , is 44 16 , 2E 16 , 4B 16 ,                      As in the first iteration, the selected ele-
               37 16 , and 57 16 ,                                                     ment (I D 16 ) is in the correct position and
                  Note that the selected element (2B I6 ) is                           need not be considered further.
338     ARRAY OPERATIONS


             The (1,1) partition may now be sorted                 same method. Obviously, quicksort's over-
           recursively as follows:                                 head is large when the number of elements is
                                                                   small. This is why one might use a bubble
              Selected element = median of the first               sort once quicksort has created small enough
           (#1 = OC I6 ), middle (#1 = OC I6 ), and last           partitions.
           (#2 = 17 16) elements. Thus the selected ele-              Note that the example array does not con-
           ment is the first element (#1 = OC I6 ) and no          tain any identical elements. During an itera-
           initial swap is necessary.                              tion, elements that are the same as the
                                                                   selected element are never moved. Thus they
              The final order is obviously the same as the         may end up in either partition. Strictly speak-
           initial order, and the two resulting partitions         ing, then, the two partitions consist of ele-
           contain 0 and I elements, respectively. Thus            ments "less than or possibly equal to the
           the next iteration concludes the recursion,             selected element" and elements "greater than
           and the other partitions are sorted by the              or possibly equal to the selected element."




REFERENCES
   Augenstein, M.J., and Tenenbaum, A.M. Data Structures and PLj I Programming.
Englewood Cliffs, N.J.: Prentice-Hall, 1979, pp. 460-71. There is also a Pascal version
of this book entitled Data Structures Using Pascal (Englewood Cliffs, N.J.: Prentice-
Hall, 1982).
  Bowles, K.L. Microcomputer Problem Solving Using Pascal. New York: Springer-Verlag,
1977, Chapter 15.
   Knuth, D.E. The Art o/Computer Programming, Volume 3: Searching and Sort-
ing. Reading, Mass.: Addison-Wesley, 1973, pp. 114-23.




          TiUe                      Quicksort
          Name:                     QSORT



          Purpose:                  Arrange an array of unsigned words into
                                    .scending order using quicksort, with a
                                    maximum size of 32,767 words
          Entry:                    Register pair HL         =   Address of first word in the
                                                                 array
                                                    9F QUICKSORT (QSORT)   339
                        Register pair DE = Address of last word in the
                                           array
                        Register pair BC = Lowest available stack
                                           address
         EKit:          If the stack did not overflow then
                          array is sorted into ascending order.
                          Carry flag = 0
                        Else
                          Carry flag = 1
         Registers used: AF.BC.DE.HL
         Time:          The timing is highly data-dependent but the
                        quicksort algorithm takes approximately
                        N * log (N) loops through PARTLP. There will be
                        2 * N+1 calls to Sort. The number of recursions
                        will probably be a fraction of N but if all
                        data is the same. the recursion could be up to
                        N. Therefore the amount of stack space should
                        be maximized. NOTE: Each recursion level takes
                        6 bytes of stack space.
                         In the above discussion N is the number of
                         array elements.
                         For example. sorting a 16.3S4-word array took
                         about 27 seconds and 1200 bytes of stack space
                         on a 6 MHz ISO.
         Size:           Program 206 bytes
                         Data      S bytes


QSORT:
         ,WATCH FOR STACK OVERFLOW
         ;CALCULATE A THRESHOLD TO WARN OF OVERFLOW
         ; (10 BYTES FROM THE END OF THE STACK)
         ;SAVE THIS THRESHOLD FOR LATER COMPARISONS
         ;ALSO SAVE THE POSITION OF THIS ROUTINE;S RETURN ADDRESS
         ; IN THE EVENT WE MUST ABORT BECAUSE OF STACK OVERFLOW
         PUSH    HL              ;SAVE BASE ADDRESS OF ARRAY
         LD      HL.I0           ;ADD SMALL BUFFER (10 BYTES) TO
         ADD     HL,BC           ; LOWEST STACK ADDRESS
         LD       (STKBTM).HL    ;SAVE SUM AS BOTTOM OF STACK
                                 ; FOR FIGURING WHEN TO ABORT
         LD      HL,2            ;SAVE POINTER TO RETURN ADDRESS
         ADD     HL.SP           ; IN CASE OF ABORT
         LD       (OLDSP).HL
         POP     HL              ;RESTORE BASE ADDRESS
         ;WORK RECURSIVELY THROUGH THE QUICKSORT ALGORITHM AS
         ; FOLLOWS:
340      ARRAY OPERATIONS


             1. CHECK IF THE PARTITION CONTAINS 0 OR 1 ELEMENT.
                MOVE UP A RECURSION LEVEL IF IT DOES.
             2. USE MEDIAN TO OBTAIN A REASONABLE CENTRAL VALUE
                FOR DIVIDING THE CURRENT PARTITION INTO TWO
                PARTS.
             3. MOVE THROUGH ARRAY SWAPPING ELEMENTS THAT
                ARE OUT OF ORDER UNTIL ALL ELEMENTS BELOW THE
                CENTRAL VALUE ARE AHEAD OF ALL ELEMENTS ABOVE
                THE CENTRAL VALUE. SUBROUTINE COMPARE
                COMPARES ELEMENTS, SWAP EXCHANGES ELEMENTS,
                PREV MOVES UPPER BOUNDARY DOWN ONE ELEMENT,
                AND NEXT MOVES LOWER BOUNDARY UP ONE ELEMENT.
             4. CHECK IF THE STACK IS ABOUT TO OVERFLOW. IF. IT
                IS, ABORT AND EXIT.
             S. ESTABLISH THE BOUNDARIES FOR THE FIRST PARTITION
                (CONSISTING OF ELEMENTS LESS THAN THE CENTRAL VALUE)
                AND SORT IT RECURSIVELY.
             6. ESTABLISH THE BOUNDARIES FOR THE SECOND PARTITION
                (CONSISTING OF ELEMENTS GREATER THAN THE CENTRAL
                VALUE) AND SORT IT RECURSIVELY.
SORT I
           ,SAVE BASE ADDRESS AND FINAL ADDRESS IN LOCAL STORAGE
           LD      (FIRST),HL      ,SAVE FIRST IN LOCAL AREA
           EX      DE, HL
           LD      (LAST),HL       ,SAVE LAST IN LOCAL AREA
           ,CHECK IF PARTITION CONTAINS 0 OR 1 ELEMENTS
           , IT DOES IF FIRST IS EITHER LARGER THAN (0)
           , OR EQUAL TO (1) LAST.
PARTION:
           ,STOP WHEN FIRST >= LAST
           ,DE = ADDRESS OF FIRST
           ;HL = ADDRESS OF LAST
           LD      A,E              ,CALCULATE FIRST - LAST
           SUB     L                , MUST KEEP BOTH, SO USE 8-BIT SUBTRACT
           LD      A,D
           SBC     A,H
           RET     NC               ;IF DIFFERENCE POSITIVE, RETURN
                                    ; THIS PART IS SORTED
           ,USE MEDIAN TO FIND A REASONABLE CENTRAL (PIVOT) ELEMENT
           ,MOVE CENTRAL ELEMENT TO FIRST POSITION
           CALL    MEDIAN          ,SELECT CENTRAL ELEMENT, MOVE IT
                                   ; TO FIRST POSITION
           LD      C,O             ;BIT 0 OF REGISTER C = DIRECTION
                                   ; IF IT~S 0 THEN DIRECTION IS UP
                                   ; ELSE DIRECTION IS DOWN
           ;REORDER ARRAY BY COMPARING OTHER ELEMENTS WITH
             CENTRAL ELEMENT. START BY COMPARING THAT ELEMENT WITH
             LAST ELEMENT. EACH TIME WE FIND AN ELEMENT THAT
             BELONGS IN THE FIRST PART (THAT IS, IT IS LESS THAN
             THE CENTRAL ELEMENT), SWAP IT INTO THE FIRST PART IF IT
                                                       9F QUICKSORT (QSORT)   341
             IS NOT ALREADY THERE AND MOVE THE BOUNDARY OF THE
             FIRST PART DOWN ONE ELEMENT. SIMILARLY, EACH TIME WE
             FIND AN ELEMENT THAT BELONGS IN THE SECOND PART (THAT
             IS, IT IS GREATER THAN THE CENTRAL ELEMENT), SWAP IT INTO
             THE SECOND PART IF IT IS NOT ALREADY THERE AND MOVE
          ; THE BOUNDARY OF THE SECOND PART UP ONE ELEMENT.
          ; ULTIMATELY, THE BOUNDARIES COME TOGETHER
          ; AND THE DIVISION OF THE ARRAY IS THEN COMPLETE
          ;NOTE THAT ELEMENTS EQUAL TO THE CENTRAL ELEMENT ARE NEVER
          ; SWAPPED AND SO MAY END UP IN EITHER PART
PARTLP:
          ; LOOP SORTI NG UNEXAMINED PART OF THE PARTITION
          , UNTIL THERE IS NOTHING LEFT IN IT
          LD       A,E              ;LOWER BOUNDARY - UPPER BOUNDARY
          SUB      L                ; MUST KEEP BOTH, SO USE 8-BIT SUBTRACT
          LD       A,D
          SBC      A, H
          .JR      NC,DONE          ;EXIT WHEN EVERYTHING EXAMINED
           ;COMPARE NEXT 2 ELEMENTS. IF OUT OF ORDER, SWAP THEM
           ;AND CHANGE DIRECTION OF SEARCH
           ; IF FIRST > LAST THEN SWAP
          CALL     COMPARE         ;COMPARE ELEMENTS
          .JR      C,OK            ;.JUMP IF ALREADY IN ORDER
          .JR      Z,OK            ; OF IF ELEMENTS EQUAL
          ,ELEMENTS OUT OF ORDER. SWAP THEM
          CALL    SWAP            ;SWAP ELEMENTS
          INC     C               ;CHANGE DIRECTION
          ;REDUCE SIZE OF UNEXAMINED AREA
          ,IF NEW ELEMENT LESS THAN CENTRAL ELEMENT, MOVE
          ; TOP BOUNDARY DOWN
          ,IF NEW ELEMENT GREATER THAN CENTRAL ELEMENT, MOVE
          ; BOTTOM BOUNDARY UP
          ;IF ELEMENTS EQUAL, CONTINUE IN LATEST DIRECTION
OK:
          BIT     O,C             ;BIT 0 OF C TELLS WHICH WAY TO GO
          .JR     Z,UP            ;.JUMP IF MOVING UP
          EX      DE,HL
          CALL    NEXT            ; ELSE MOVE TOP BOUNDARY DOWN BY
          EX      DE,HL           ; ONE ELEMENT
          .JR     PARTLP
UP:
          CALL    PREV            ; MOVE BOTTOM BOUNDARY UP BY
                                  ; ONE ELEMENT
          .JR     PARTLP
          ;THIS PARTITION HAS NOW BEEN SUBDIVIDED INTO TWO
            PARTITIONS. ONE STARTS AT THE TOP AND ENDS .JUST
            ABOVE THE CENTRAL ELEMENT. THE OTHER STARTS
            .JUST BELOW THE CENTRAL ELEMENT AND CONTINUES
            TO THE BOTTOM. THE CENTRAL ELEMENT IS NOW IN
            ITS PROPER SORTED POSITION AND NEED NOT BE
            INCLUDED IN EITHER PARTITION
342      ARRAY OPERATIONS


DONE:
          ;FIRST CHECK WHETHER STACK MIGHT OVERFLOW
          ;IF IT IS GETTING TOO CLOSE TO THE BOTTOM, ABORT
          ; THE PROGRAM AND EXIT
          LD      HL,(STKBTM)     ;CALCULATE STKBTM - SP
          OR      A               ; CLEAR CARRY
          SBC     HL,SP
          JR      NC,ABORT        ;EXIT IF STACK TOO LARGE
          ;ESTABLISH BOUNDARIES FOR FIRST (LOWER) PARTITION
          ;LOWER BOUNDARY IS SAME AS BEFORE
          ,UPPER BOUNDARY IS ELEMENT JUST BELOW CENTRAL ELEMENT
          ;THEN RECURSIVELY QUICKSORT FIRST PARTITION
          PUSH    DE              ;SAVE ADDRESS OF CENTRAL ELEMENT
          LD      HL,(LAST)
          PUSH    HL              ;SAVE ADDRESS OF LAST
          EX      DE,HL
          CALL    PRE V           ;CALCULATE LAST FOR FIRST PART
          EX      DE,HL
          LD      HL,(FIRST)      ;FIRST IS SAME AS BEFORE
          CALL    SORT            ;QUICKSORT FIRST PART
          ;ESTABLISH BOUNDARIES FOR SECOND (LIPPER) PARTITION
          ;UPPER BOUNDARY IS SAME AS BEFORE
          ;LOWER BOUNDARY IS ELEMENT JUST ABOVE CENTRAL ELEMENT
          ,THEN RECURSIVELY QUICKSORT SECOND PARTITION
          POP     DE              ; LAST I S SAME AS BEFORE
          POP     HL              ;CALCULATE FIRST FOR SECOND PART
          CALL    NEXT
          CALL    SORT            ; QUICKSORT SECOND PART
          OR      A               ; CARRY = 0 FOR NO ERRORS
          RET
          ;ERROR EXIT - SET CARRY
ABORT:    LD      SP,(OLDSP)      ;TOP OF STACK IS ORIGINAL
                                  ; RETURN ADDRESS
          SCF                     ; INDICATE ERROR IN SORT
          RET                     ,RETURN TO ORIGINAL CALLER
          ;******************************
          ; ROUTINE: MEDIAN
          ; PURPOSE: DETERMINE WHICH VALUE IN A PARTITION
          ;        SHOULD BE USED AS THE CENTRAL ELEMENT OR PIVOT
          ; ENTRY: DE = ADDRESS OF FIRST VALUE
          ;        HL = ADDRESS OF LAST VALUE
          ;EXIT: DE IS ADDRESS OF CENTRAL ELEMENT
          ;REGISTERS USED: AF,BC,DE
          ;*******************************
MEDIAN:
          ;DETERMINE ADDRESS OF MIDDLE ELEMENT
          ; MIDDLE := ALIGNED (FIRST + LAST) DIV 2
          LD      A,L               ADD ADDRESSES OF FIRST, LAST
           ADD    A,E               MUST KEEP BOTH, SO USE a-BIT
          LD      C,A                ADD INSTEAD OF 16-BIT
                                                      9F QUICKSORT (QSORT)   343
         LD     A,H
         ADC    A,D
         LD     B,A
         RR     B                ,DIVIDE SUM BY 2, BYTE AT A TIME
         RR     C
         RES    O,C              ;CLEAR   BIT 0 FOR ALIGNMENT
         BIT    O,E              ;ALIGN   MIDDLE TO BOUNDARY OF FIRST
         .JR    Z,MEDl           , JUMP   IF BIT 0 OF FIRST IS 0
         INC    C                ; ELSE   MAKE BIT 0 OF MIDDLE 1
         ;DETERMINE WHICH OF FIRST, MIDDLE, LAST IS
         ; MEDIAN (CENTRAL VALUE)
         ;COMPARE FIRST AND MIDDLE
MEDll
         PUSH    HL              ,SAVE LAST
         LD      L,C
         LD      H,B
         CALL    COMPARE         ;COMPARE FIRST AND MIDDLE
         POP     HL              ;RESTORE LAST
         JR      NC,MIDDI        ;JUMP IF FIRST >= MIDDLE
         ;WE KNOW (MIDDLE> FIRST)
         J  SO COMPARE MIDDLE AND LAST
         PUSH     DE              ;SAVE FIRST
         LD       E,C
         LD       D,B
         CALL     COMPARE         ;COMPARE MIDDLE AND LAST
         POP      DE              ;RESTORE LAST
         JR       C,SWAPMF        ;JUMP IF LAST >= MIDDLE
         JR       Z,SWAPMF        ; MIDDLE IS MEDIAN
         ,WE KNOW (MIDDLE> FIRST) AND (MIDDLE> LAST)
         ; SO COMPARE FIRST AND LAST
         CALL    COMPARE         ;COMPARE FIRST AND LAST
         RET     NC              ;RETURN IF LAST >= FIRST
                                 , FIRST IS MEDIAN
         JR      SWAPLF          ;ELSE LAST IS MEDIAN
         ,WE KNOW (FIRST >= MIDDLE)
         ; SO COMPARE FIRST AND LAST
MIDDl:
         CALL    COMPARE         ;COMPARE LAST AND FIRST
         RET     C               ;RETURN IF LAST >= FIRST
         RET     Z               ; FIRST IS MEDIAN
         ,WE KNOW (FIRST >= MIDDLE) AND (FIRST > LAST)
         , SO COMPARE MIDDLE AND LAST
         PUSH    DE              ;SAVE FIRST
         LD      E,C             ,DE = MIDDLE
         LD      D,B
         CALL    COMPARE         ;COMPARE MIDDLE AND LAST
         POP     DE              ;RESTORE FIRST
         JR      C,SWAPLF        ;JUMP IF LAST> MIDDLE
                                 J LAST IS MEDIAN
344     ARRAY OPERATIONS


          ,MIDDLE IS MEDIAN, SWAP IT WITH FIRST
SWAPMF:
          PUSH    HL               ;SAVE LAST
          LD      L.C              ;HL = ADDRESS OF MIDDLE
          LD      H,B
          CALL    SWAP             ;SWAP MIDDLE, FIRST
          POP     HL               ;RESTORE LAST
          RET
          ;LAST IS MEDIAN, SWAP IT WITH FIRST
SWAPLF:
          CALL    SWAP             ;SWAP FIRST AND LAST
          RET
          ;*******************************
          ; ROUTINE: NEXT
          ; PURPOSE: MAKE HL POINT TO NEXT ELEMENT
          ; ENTRY: HL = ADDRESS OF CURRENT ELEMENT
          ,EXIT: HL = ADDRESS OF NEXT ELEMENT
          ;REGISTERS USED: HL
          ;*******************************
NEXT:
          INC     HL               ;INCREMENT TO NEXT ELEMENT
          INC     HL
          RET
          ;*******************************
          ; ROUTI NE: PREV
          ,PURPOSE: MAKE HL POINT TO PREVIOUS ELEMENT
          ,ENTRY: HL = ADDRESS OF CURRENT ELEMENT
          ,EXITI HL = ADDRESS OF PREVIOUS ELEMENT
          ,REGISTERS USED: HL
          '*******************************
PREV:
          DEC     HL                ;DECREMENT TO PREVIOUS ELEMENT
          DEC     HL
          RET
          '*******************************
          ,ROUTINE: COMPARE
          ,PURPOSE: COMPARE DATA ITEMS POINTED TO BY DE AND HL
          ,ENTRY: DE = ADDRESS OF DATA ELEMENT 1
          ;       HL = ADDRESS OF DATA ELEMENT 2
          ,EXIT: IF ELEMENT 1 > ELEMENT 2 THEN
                       C   =0
                       Z   =0
                  IF ELEMENT 1 < ELEMENT 2 THEN
                       C   =1
                       Z   =0
                  IF ELEMENT 1   = ELEMENT   2 THEN
                       C   =0
          ;            Z   =1
          ;REGISTERS USED: AF
          ;********************************
                                                          9F QUICKSORT (QSORT)   345
COMPARE:
           INC     HL              ;POINT TO HIGH BYTES
           INC     DE
           LD      A.<DE)
           CP      (HL>            ;COMPARE HIGH BYTES
           DEC     DE              ;POINT TO LOW BYTES
           DEC     HL
           RET     NZ              ;RETURN IF HIGH BYTES NOT EQUAL
           LD      A.<DE)          ; OTHERWISE. COMPARE LOW BYTES
           CP      (HL>
           RET

           ;********************************
           ; ROUTI NE: SWAP
           ; PURPOSE: SWAP ELEMENTS POINTED TO BY DE.HL
           ; ENTRY: DE = ADDRESS OF ELEMENT 1
           ;         HL = ADDRESS OF ELEMENT 2
           ;EXIT: ELEMENTS SWAPPED
           ;REGISTERS USED: AF.B
           ;*********************************
SWAP:
           ; SWAP LOW BYTES
           LD       B. (HL>        ;GET ELEMENT 2
           LD       A. <DE)        ;GET ELEMENT 1
           LD       (HL>. A        ;STORE NEW ELEMENT 2
           LD       A.B
           LD       <DE). A        ;STORE NEW ELEMENT 1
           INC      HL
           INC      DE
           ; SWAP HIGH BYTES
           LD       B. (HL>        ;GET ELEMENT 2
           LD       A. <DE)        ;GET ELEMENT 1
           LD        (HL>. A       ;STORE NEW ELEMENT 2
           LD       A.B
           LD        (DE).A        ;STORE NEW ELEMENT 1
           DEC      HL
           DEC      DE
           RET
           ; DATA SECTION
FIRST:     DS       2              ;POINTER TO FIRST ELEMENT OF PART
LAST:      DS       2              ;POINTER TO LAST ELEMENT OF PART
STKBTM:    DS       2              ;THRESHOLD FOR STACK OVERFLOW
OLDSP:     DS       2              ;POINTER TO ORIGINAL RETURN ADDRESS



           SAMPLE EXECUTION:
346     ARRAY OPERATIONS


SC9F:
         ;SORT AN ARRAY BETWEEN BEOBUF (FIRST ELEMENT)
         ; AND ENDBUF (LAST ELEMENT)
         ;START STACK AT 5000 HEX AND ALLOW IT TO EXPAND
         ; AS FAR AS 4FOO HEX
         LD      SP,5000H        ;SET UP A STACK AREA
         LD      BC,4FOOH        ;BC = LOWEST AVAILABLE STACK ADDRESS
         LD      HL,BEOBUF       ;HL = ADDRESS OF FIRST ELEMENT OF ARRAY
         LD      DE, ENDBUF      ;DE = ADDRESS OF LAST ELEMENT OF ARRAY
         CALL     QSORT          ; SORT
                                 ;RESULT FOR TEST DATA IS
                                 ; 0,1,2,3, ... ,14,15
         JR       SC9F           ;LOOP FOR MORE TESTS
        ; DATA SECTION
BEOBUF: DW       15
        DW       14
        DW       13
        DW       12
        DW       11
        DW       10
        DW       9
        DW       8
        DW       7
        DW       6
        DW       5
        DW       4
        DW       3
        DW       2
        DW       1
ENDBUF: DW       0

         END
RAM Test (RAMTST)                                                                                         9G

   Tests a RAM area specified by a base address           Registers Used: AF, BC, DE, HL
and a length in bytes. Writes the values 0, FF 16,        Execution Time: Approximately 633 cycles per
AA 16 (101010102), and 5516 (010101012) into each         byte tested plus 663 cycles overhead
byte and checks whether they can be read back             Program Size: 82 bytes
correctly. Places I in each bit position of each          Data Memory Required: None
byte and checks whether it can be read back               Special Cases:
correctly with all other bits cleared. Clears the            I. An area size of 0000'6 causes an immediate
Carry flag if all tests run properly. If it finds an      exit with no memory tested. The Carry flag is
error, it exits immediately, setting the Carry flag       cleared to indicate no errors.
and returning the test value and the address at              2. Since the routine changes all bytes in the
                                                          tested area, using it to test an area that includes
which the error occurred.                                 itself will have unpredictable results.
   Procedure: The program performs the single                Note that Case I means this routine cannot be
value checks (with 0, FF I6 , AA I6 , and 5516) by        asked to test the entire memory. Such a request
                                                          would be meaningless anyway since it would re-
first filling the memory area and then comparing          quire the routine to test itself.
each byte with the specified value. Filling the              3. Testing a ROM causes a return with an error
entire area first should provide enough delay             indication after the first occasion on which the test
between writing and reading to detect a failure           value differs from the memory's contents.
to retain data (perhaps caused by improperly
designed refresh circuitry). The program then          here it writes the data into memory and attempts
performs the walking bit test, starting with bit 7;    to read it back immediately for a comparison.



Entry Conditions                                       Exit Conditions
Base address of test area in HL                        If an error is found:
Size of test area in bytes in DE                          Carry = I
                                                          Address containing error in HL
                                                          Test value in A
                                                       If no error is found:
                                                          Carry = 0
                                                          All bytes in test area contain 0




Example
I.   Data:   Base address = 0380'6                      Result:   Area tested is the 0200'6 bytes starting at
             Length (size) of area = 0200'6                       address 0380,6, that is, addresses 0380'6
                                                                  through 057F '6' The order of the tests is


                                                                                                          347
348   A.RRAY OPERATIONS


        1.   Write and read 0
        2.   Write and read FF'6
        3.   Write and read AA'6 (10101010 2)
        4.   Write and read 55'6 (01010101 2)
        5.   Walking bit test, starting with I in
             bit 7. That is, start with 100000002
             (80'6) and move the lone position
             right for each subsequent test of a
             byte.




       Tit Ie                    RAM test
       Name:                     RAMTST




       Purpose:                  Test a RAM         (read/write memory) area
                                   1) Write         all o and test
                                   2) Write         all FF hex and test
                                   3) Write         all AA hex and test
                                   4) Write         all 55 hex and test
                                   5) Shift         a single 1 through each bi t,
                                      whi Ie        clearing all other bits

                                     If the program finds an error, it exits
                                     immediately with the Carry flag set and
                                    indicates where the error occurred and
                                    what value it used in the test.
       Entry:                    Register pair HL = Base address of test area
                                 Register pair DE = Size of area in bytes

       Exit:                     If there are no errors then
                                    Carry f lag = 0
                                   test area contains 0 in all bytes
                                 else
                                   Carry flag = 1
                                   Register pair HL = Address of error
                                   Register A = Expected value

       Registers used: AF,BC,DE.HL

       Time:                     Approximately 633 cycles per byte plus
                                 663 cycles overhead

       Size:                     Program 82 bytes
                                                             9G RAM TEST (RAMTST)   349
RAMTST:
          ~EXIT    WITH NO ERRORS IF AREA SIZE IS 0
          LD         A.D             ~TEST AREA SIZE
          OR         E
          RET        Z               ~EXIT    WITH NO ERRORS IF SIZE IS ZERO
          LD         B.D             ~BC   = AREA  SIZE
          LD         C.E
          ~FILL   MEMORY WITH 0 AND TEST
          SUB       A
          CALL      FILCMP
          RET       C               ~EXIT IF ERROR FOUND

          ;FILL MEMORY WITH FF HEX (ALL I~S) AND TEST
          LD      A.OFFH
          CALL    FILCMP
          RET     C               ~EXIT IF ERROR FOUND

          ~FILL    MEMORY WITH AA HEX (ALTERNATING I~S AND     O~S)   AND TEST
          LD         A.OAAH
          CALL       FILCMP
          RET        C               ~EXIT IF ERROR FOUND

          ~FILL    MEMORY WITH 55 HEX (ALTERNATING O~S AND     I~S)   AND TEST
          LD         A.55H
          CALL       FILCMP
          RET        C               ~EXIT IF ERROR FOUND

          ~PERFORM    WALKING BIT TEST. PLACE A 1 IN BIT 7 AND
          ~    SEE IF IT CAN BE READ BACK. THEN MOVE THE 1 TO
          ~    BITS 6. 5. 4. 3. 2. 1. AND 0 AND SEE IF IT CAN
          ~    BE READ BACK
WLKLP:
          LD         A.l0000000B     ~MAKE    BIT 7 1. ALL OTHER BITS 0
WLKLP1:
          LD         (HU.A           ;STORE TEST PATTERN IN MEMORY
          CP         (HU             ~TRY TO READ IT BACK
          SCF                        ~SET CARRY IN CASE OF ERROR
          RET        NZ              ~RETURN IF ERROR
          RRCA                       ~ROTATE PATTERN TO MOVE 1 RIGHT
          CP        10000000B
          JR        NZ.WLKLPI        ~CONTINUE   UNTIL 1 IS BACK IN BIT 7
          LD        (HU.O            ~CLEAR   BYTE JUST CHECKED
          INC       HL
          DEC       BC               ~DECREMENT     AND TEST 16-BIT COUNTER
          LD        A.B
          OR        C
          JR        NZ.WLKLP         ~CONTINUE   UNTIL MEMORY TESTED
          RET                        ~NO   ERRORS (NOTE OR C CLEARS CARRY)


          ~***********************************
          ,ROUTINE; FILCMP
          ~PURPOSE: FILL MEMORY WITH A VALUE AND TEST
                    THAT IT CAN BE READ BACK
350      ARRAY OPERATIONS

           ; ENTRY: A = TEST VALUE
                    HL = BASE ADDRESS
           ,        BC = SIZE OF AREA IN BYTES
           ;EXIT: IF NO ERRORS THEN
                      CARRY FLAG IS 0
                    ELSE
                      CARRY FLAG IS 1
                      HL = ADDRESS OF ERROR
                      DE = BASE ADDRESS
                      BC = SIZE OF AREA IN BYTES
           ;          A = TEST VALUE
           ;REGISTERS USED: AF,BC,DE.HL
           ;************************************
FILCMP:
           PUSH    HL               ;SAVE BASE ADDRESS
           PUSH    BC               ;SAVE SIZE OF AREA
           LD      E,A              ;SAVE TEST VALUE
           LD      (HL> ,A          ;STORE TEST VALUE IN FIRST BYTE
           DEC     BC               ;REMAINING AREA = SIZE - 1
           LD      A.B              ;CHECK IF ANYTHING IN REMAINING AREA
           OR      C
           LD      A,E              ;RESTORE TEST VALUE
           JR      Z,COMPARE        ;BRANCH IF AREA WAS ONLY 1 BYTE
           ;FILL REST OF AREA USING BLOCK MOVE
           ; EACH ITERATION MOVES TEST VALUE TO NEXT HIGHER ADDRESS
           LD      D,H             ;DESTINATION IS ALWAYS SOURCE + 1
           LD      E,L
           INC     DE
           LDIR                    ; FILL MEMORY
           ;NOW THAT MEMORY HAS BEEN FILLED, TEST TO SEE IF
           ; EACH BYTE CAN BE READ BACK CORRECTLY
COMPARE:
           POP     BC               ;RESTORE SIZE OF AREA
           POP     HL               ;RESTORE BASE ADDRESS
           PUSH    HL               ;SAVE BASE ADDRESS
           PUSH    BC               ;SAVE SIZE OF VALUE
           ;COMPARE MEMORY AND TEST VALUE
cmpLoop:
           CPI
           JR      NZ,CMPER        ;JUMP IF NOT EQUAL
           JP      PE,cmpLoop        ;CONTINUE THROUGH ENTIRE AREA
                                   ; NOTE CPI CLEARS P/V FLAG IF IT
                                   ; DECREMENTS BC TO 0
           ,NO ERRORS FOUND, SO CLEAR CARRY
           POP     BC              ;BC = SIZE OF AREA .
           POP     HL              ;HL = BASE ADDRESS
           OR      A               ;CLEAR CARRY, INDICATING NO ERRORS
           RET
           ;ERROR EXIT, SET CARRY
           ;HL = ADDRESS OF ERROR
           ;A = TEST VALUE
                                                     9G RAM TEST (RAMTST)   351
CMPER:
         POP    BC               ;DE = SIZE OF AREA
         POP    DE               ;BC = BASE ADDRESS
         SCF                     ,SET CARRY, INDICATING AN ERROR
         RET



         SAMPLE EXECUTION


SC9G:
         ;TEST RAM FROM 2000 HEX THROUGH 300F HEX
         , SIZE OF AREA = 1010 HEX BYTES
         LD      HL,2000H        ;HL = BASE ADDRESS
         LD      DE,lOlOH        ;DE = NUMBER OF BYTES
         CALL    RAMTST          ;TEST MEMORY
                                 ;CARRY FLAG SHOULD BE 0

         JR      SC9G            ;LOOP FOR MORE TESTING
         END
Jump Table (JTAB)                                                                                            9H

   Transfers control to an address selected from            Registers Used: AF
a table according to an index. The addresses are
stored in the usual Z80 format (less significant            Execution Time: 117 cycles overhead, besides the
                                                            time required to execute the actual subroutine
byte first), starting at address JMPTAB. The
size of the table (number of addresses) is a                Program Size: 21 bytes plus 2 • LEN SUB bytes for
constant, LENSUB, which must be less than or                the table of starting addresses, where LENSUB is the
                                                            number of subroutines
equal to 128. If the index is greater than or equal
to LENSUB, the program returns control imme-                Data Memory Required: None
diately with the Carry flag set to 1.
                                                            Special Case: Entry with an index greater than or
   Procedure: The program first checks if the               equal to LEN SUB causes an immediate exit with
index is greater than or equal to the size of the           the Carry flag set to I.
table (LEN SUB). If it is, the program returns
control with the Carry flag set. If it is not, the
program obtains the starting address of the
appropriate subroutine from the table and jumps
to it.




Entry Conditions                                          Exit Conditions
Index in A                                                If (A) is greater than LENSUB, an immediate
                                                          return with Carry = 1. Otherwise, control is
                                                          transferred to the appropriate subroutine as if
                                                          an indexed call had been performed. The return
                                                          address remains at the top of the stack.




Example
I.    Data:    LENSUB (size of subroutine table) = 03
               Table consists of addresses SUBO, SUB I,
                 and SUB2.
               Index = (A) = 02

     Result:   Control transferred to address SUB2
                (PC= SUB2)


352
                                                         9H JUMP TABLE (JTAB)   353



        Title               Jump table
        Name:               JTAB



        Purpose:            Given an index, jump to the subroutine with
                            that index in a table.
        Entry:              Register A is the subroutine number (0 to
                                       LENSUB-l, the number of subroutines)
                                       LENSUB must be less than or equal to
                                       128.
        Exit:               If the routine number is valid then
                              execute the routine
                            else
                              Carry flag = 1
        Registers used: AF
        Time:               117 cycles plus execution time of subroutine
        Size:               Program 21 bytes plus size of table (2*LENSUB)


        ;EXIT WITH CARRY SET IF ROUTINE NUMBER IS INVALID
        ; THAT IS, IF IT IS TOO LARGE FOR TABLE OLENSUB -     1)
JTAB:
        CP         LENSUB           ;COMPARE ROUTINE NUMBER, TABLE SIZE
        CCF                         ;COMPLEMENT CARRY FOR ERROR INDICATOR
        RET        C                ;RETURN IF ROUTINE NUMBER TOO LARGE
                                    ; WITH CARRY SET
        ; INDEX INTO TABLE OF WORD-LENGTH ADDRESSES
        ; LEAVE REGISTER PAIRS UNCHANGED SO THEY CAN BE USED
            FOR PASSING PARAMETERS
        PUSH     HL              ;SAVE HL
        ADD      A,A             ;DOUBLE INDEX FOR WORD-LENGTH ENTRIES
        LD       HL,JMPTAB       ;INDEX INTO TABLE USING 8-BIT
        ADD      A, L            ; ADDITION TO AVOID DISTURBING
        LD       L,A             ; ANOTHER REGISTER PAIR
        LD       A,O
        ADC      A,H
        LD       H, A            ; ACCESS ROUTINE ADDRESS
        ;OBTAIN ROUTINE ADDRESS FROM TABLE AND TRANSFER
           CONTROL TO IT, LEAVING ALL REGISTER PAIRS UNCHANGED
354       ARRAY OPERATIONS


           LD       A, (HU         ;MOVE ROUTINE ADDRESS TO HL
           INC      HL
           LD       H, (HU
           LD       L,A
           EX       (SP),HL        ;RESTORE OLD HL, PUSH ROUTINE ADDRESS
           RET                     ; JUMP TO ROUTI NE
LENSUB     EQU      3              ;NUMBER OF SUBROUTINES IN TABLE
JMPTAB:                            ;JUMP TABLE
           DW       SUBO           ; ROUTINE (J
           DW       SUB1           ;ROUTINE 1
           DW       SUB2           ;ROUTINE 2
           ;THREE TEST SUBROUTINES FOR JUMP TABLE
SUBO:
           LD       A,1            ; TEST ROUTI NE 0 SETS (A)    1
           RET
SUB 1 :
           LD       A,2            ; TEST ROUTI NE 1 SETS (A) = 2
           RET
SUB2:
           LD       A,3            ;TEST ROUTINE 2 SETS (A)      3
           RET



           SAMPLE EXECUTION:


SC9H:
           SUB      A              ;EXECUTE ROUTINE 0
           CALL     JTAB           ; AFTER EXECUTION, (A)   =1
                                     9H JUMP TABLE (JTAB)   355
LD     A,1    ;EXECUTE ROUTINE 1
CALL   JTAB   ; AFTER EXECUTION, (A) = 2
LD     A,2    ,EXECUTE ROUTINE 2
CALL   JTAB   ; AFTER EXECUTION, (A)   3
LD     A,3    ;EXECUTE ROUTINE 3
CALL   JTAB   ; AFTER EXECUTION, CARRY   1
JR     SC9H   ;LOOP FOR MORE TESTS
END
