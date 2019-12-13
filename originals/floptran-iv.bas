' Byte 1980 - Vol5 n°10 pp196...


49000 X=                                                             :          ;;;leave at least 60 blanks here
  :RETURN                                                                       ;;;to be filled in during a 'LET' operation

49100 TP=TP+1                                                                   ;;;subroutine to get next nonblank character
  :CH=PEEK(TP)                                                                  ;;;and return it in CH
  :IF CH=32 GOTO 49100

49120 RETURN

49190 IF (CH>64) AND (CH<91) GOTO 49300                                         ;;;64<CH<91 Means alphabetic, legal variable name

49200 PRINT                                                                     ;;;abort if unexpected character found
  :PRINT "***ERR0R IN LINE";LI;"***"
  :GOTO 61000

49300 Z=5*(CH-65)+OV                                                            ;;;return the address (5 byte location) of the variable
  :ZH=INT(Z/256)                                                                ;;;'CH'...high and low parts of address in ZH & ZL
  :ZL=Z-256*ZH
  :RETURN

49400 PRINT#1,169                                                               ;;;LDA #ZL
  :PRINT#1,ZL
  :PRINT#1,160                                                                  ;;;LDY #ZH
  :PRINT#1,ZH

49420 PRINT#1,32                                                                ;;;JSR $DA74 with A & Y set up as above
  :PRINT#1,116                                                                  ;;;transfers the value of the variable being
  :PRINT#1,218                                                                  ;;;pointed at to the primary accumulator for floating-
  :PC=PC+7                                                                      ;;;-point operations, 'PRI'
  :RETURN                                                                       ;;;add 7 bytes to the progran-counter & return



49600 DATA 11,219,158,219,42,219,0,0,98,210,133,210                             ;;;data for floating-point ROM subroutine calls, 2-byte pairs
                                                                                ;;;SGN,INT,ABS,USR,FRE,POS
49640 DATA 34,222,69,223,191,214,140,222,158,223,145,223                        ;;;SQR,RND,LOG,EXP,COS,SIN

49680 DATA 238,223,72,224,230,214,40,215,40,215,253,214,228,217,44,222          ;;;TAN,ATN,PEEK,+,-,*, /,^

49720 FOR X=0 TO 19
  :READ ML%(X,0),ML%(X,1)                                                       ;;;load addresses into ML% array, low byte, then high
  :NEXT X


49740 FOR X=0 TO 255
  :LN%(X)=-32767                                                                ;;;initialize LN% array of line numbers so that nonexistent
  :NEXT X                                                                       ;;;lines will be flagged later
  :RETURN


50000 DIM X,LN%(255),GO%(127,1),ML%(19,1)                                       ;;;RUN 50000 to enter program here X compile code
  :P=1025                                                                       ;;;dimension X first so Make it easy to find later
  :GC=0                                                                         ;;;initialize pointer P & GOTO/GOSUB counter GC

50040 L=PEEK(P+2)+256*PEEK(P+3)                                                 ;;;look up line number & scan
  :IF L<>49000 THEN P=PEEK(P)*256*PEEK(P+1)                                     ;;;to locate subroutine 49000 to be used in 'LET'
  :GOTO 50040                                                                   ;;;assignments during later compilation


50100 XL=P+6                                                                    ;;;store location of first space after '=' in line 49000 in XL
  :GOSUB 49600                                                                  ;;;initialize ML% and LN% arrays
  :INPUT "COMPILED DATA FILE NAME";C$
  :OPEN 1,1,1,C$                                                                ;;;get file name & open data file for compiled program

50120 INPUT "ORIGIN FOR CODE";OC                                                ;;;OC stores starting location for compilation
  :PRINT#1,OC                                                                   ;;;write it to tape to begin data file
  :PC=OC                                                                        ;;;initialize program counter PC


50140 INPUT "VARIABLE TABLE ORIGIN";OV                                          ;;;variables occupy 26*5 contiguous locations beginning at OV
  :TP=1028                                                                      ;;;initialize text pointer to first line number of BASIC program

50200 LI=PEEK(TP-1)+256*PEEK(TP)                                                ;;;put line number in LI
  :IF LI>255 GOTO 60000                                                         ;;;compiling program has lines in range 0-255----exit when finished

50220 LN%(LI)=PC-32767                                                          ;;;store current PC in LN%(LI) to be used in GOTOs or GOSUBs later
  :PRINT "COMPILING LINE";LI                                                    ;;;announce to screen what line is being compiled now

50240 GOSUB 49100                                                               ;;;get next nonblank character in CH

50260 IF CH=0 THEN TP=TP+4                                                      ;;;'0' is end-of-line marker move text pointer forward and
  :GOTO 50200                                                                   ;;;loop to get next line number

50280 IF CH=58 GOTO 50240                                                       ;;;'58'' is ':' loop back to allow multiple statements/line

50300 IF CH=136 GOTO 58000                                                      ;;;'136' is 'LET'

50320 IF CH=153 GOTO 59000                                                      ;;;'153' is 'PRINT'

50340 IF CH=133 GOTO 59500                                                      ;;;'133' is 'INPUT'

50340 IF CH=139 GOTO 58500                                                      ;;;'139' is 'IF'

50380 IF (CH=137) OR (CH=141) GOTO 58700                                        ;;;'137' is 'GOTO' and '141' is 'GOSUB'

50400 IF CH=143 GOTO 56500                                                      ;;;'143' is 'REM'

' RETURN
50420 IF CH=142 THEN PRINT#1,96                                                 ;;;'142' is 'RETURN', the 6502 'RTS' instruction, 96 decimal
  :PC=PC+1                                                                      ;;;'RETURN' also ends the compiled program & returns to BASIC after
  :GOTO 50240                                                                   ;;;a 'SYS' command starts machine-language

'
50440 GOSUB 49190                                                               ;;;if none of the above, it better be a variable name check it
  :VL=ZL                                                                        ;;;and store the storage address in VL, VH
  :VH=ZH

50440 GOSUB 49100                                                               ;;;get next character
  :IF CH<>178 GOTO 49200                                                        ;;;it had better be an '=' sign...abort if not!

50480 GOSUB 49100                                                               ;;;next character
  :IF (CH>179) AND (CH<195) GOTO 51000                                          ;;;179<CH<195 means a monadic function


50500 IF CH=171 GOTO 55600                                                      ;;;'171' is a '-' sign

50520 GOSUB 49190                                                               ;;;otherwise, It better be a variable name....
  :WL=ZL                                                                        ;;;store the address of the variable in WL, WH
  :WH=ZH



50540 GOSUB 49100
  :IF (CH=0) OR (CH=58) GOTO 58300                                              ;;;if 'end-of-line' or ':', equation is of the ford 'V=W'

50560 IF (CH<170) OR (CH>174) GOTO 49200                                        ;;;otherwise, expect +,-,*,/,^ and abort if not

50580 FU=CH-155                                                                 ;;;FU now records what operator it was
  :GOSUB 49100                                                                  ;;;get next character
  :GOSUB 49190                                                                  ;;;better be variable name.. .address in ZL, ZH
  :GOSUB 49400                                                                  ;;;write out code to transfer it to primary accumulator

50640 PRINT#1,169                                                               ;;;LDA WL
  :PRINT#1,WL
  :PRINT#1,160                                                                  ;;;LDY WH
  :PRINT#1,WH
  :PC=PC+4                                                                      ;;;count the bytes and increment program counter

50700 IF (FU=15) OR (FU=17) GOTO 51080                                          ;;;take shortcut for '+' or '*' functions

50720 PRINT#1,32                                                                ;;;JSR $D95E transfers 'W' to secondary accumulator
  :PRINT#1,94                                                                   ;;;and sets up for dyadic operation subroutine call
  :PRINT#1,217
  :PC=PC+3                                                                      ;;;count the bytes...
  :GOTO 51080                                                                   ;;;jump down to output code to do the operation & store result

' Monadic function
51000 FU=CH-180                                                                 ;;;arrive here to perform a monadic function
  :GOSUB 49100
  :IF CH<>40 GOTO 49200                                                         ;;;next character better be a '(', else abort!

51040 GOSUB 49100
  :GOSUB 49190                                                                  ;;;get argument of function, address in ZL, ZH
  :GOSUB 49400                                                                  ;;;fetch it to primary floating accumulator

51060 GOSUB 49100
  :IF CH<>41 GOTO 49200                                                         ;;;expect to see a ')' here!!!

51080 PRINT#1,32                                                                ;;;JSR to ROM function
  :PRINT#1,ML%(FU,0)
  :PRINT#1,ML%(FU,1)
  :PC=PC+3

51100 PRINT#1,162                                                               ;;;LDX #VL
  :PRINT#1,VL
  :PRINT#1,160                                                                  ;;;LDY #VH
  :PRINT#1,VH

51120 PRINT#1,32                                                                ;;;JSR $DAA6 with target in X & Y registers
  :PRINT#1,166                                                                  ;;;to store result of calculation from primary accumulator
  :PRINT#1,218
  :PC=PC+7
  :GOTO 50240                                                                   ;;;back to top of main loop

55600 GOSUB 49100                                                               ;;;handle monadic '-' operator
  :GOSUB 49190
  :GOSUB 49400                                                                  ;;;transfer number to be negated to primary floatinq accumulator
  :PRINT#1,169                                                                  ;;;LDA #FF
  :PRINT#1,255

55660 PRINT#1,69                                                                 ;;;EOR $B5 (sign storage for primary accumulator)
  :PRINT#1,181
  :PRINT#1,133                                                                  ;;;STA $B5....now sign is flipped
  :PRINT#1,181
  :PC=PC+6
  :GOTO 51100                                                                   ;;;go store negated number in target

' REM
56500 GOSUB 49100                                                               ;;;handle 'REM' statement
  :IF CH>0 GOTO 56500                                                           ;;;loop until end-of-line encountered

56540 GOTO 50260                                                                ;;;back to top of nain loop

' LET
58000 GOSUB 49100                                                               ;;;handle 'LET' assignment operation
  :GOSUB 49190                                                                  ;;;get address for result to be stored into
  :GOSUB 49100                                                                  ;;;this character better be an '=' sign
  :VL=TP+1                                                                      ;;;save pointer to first character after '=' in VL
  :IF CH<>178 GOTO 49200                                                        ;;;abort if it wasn't '='

58080 GOSUB 49100
  :IF (CH<>58) AND (CH<>0) GOTO 58080                                           ;;;scan until ':' or 'end-of-line' terminates assignment


58100 IF TP-VL>60 THEN PRINT "LINE TOO LONG!"                                   ;;;subroutine 49000 only has room for 60 characters sorry!
  :GOTO 49200

58120 FOR L=VL TO TP-1                                                          ;;;take the whole expression to be evaluated for the 'LET'
  :WL=PEEK(L)
  :POKE XL+L-VL,WL                                                              ;;;and POKE it into subroutine 49000
  :NEXT L

58160 GOSUB 49000                                                               ;;;go evaluate the expression! result returns in variable X
  :FOR L=VL TO TP-1
  :POKE XL+L-VL,32                                                              ;;;and clean up (fill with blanks again) subroutine 49000
  :NEXT L

58180 PRINT#1,24                                                                ;;;CLC
  :PRINT#1,144                                                                  ;;;BCC .+5 (a forced branch over the following 5 bytes)
  :PRINT#1,5
  :PC=PC+3
  :VL=256*PEEK(125)+PEEK(124)+2                                                 ;;;point VL to the place where X is stored now

58220 FOR L=VL TO VL+4                                                          ;;;fetch all 5 bytes of the current value of X
  :PRINT#1,PEEK(L)                                                              ;;;and write then into the compiled file
  :NEXT L
  :WH=INT(PC/256)                                                               ;;;store their location in the conpiled code in WH, WL
  :WL=PC-256*WH

58240 VL=ZL                                                                     ;;;put the address for the result of the 'LET' to be stored into
  :VH=ZH                                                                        ;;;in VL, VH, and fall into the 'assignment' handling routine
  :PC=PC+5

58300 PRINT#1,162                                                               ;;;handle simple assignments of the form 'V=W' here
  :PRINT#1,4                                                                    ;;;LDX #4
  :PRINT#1,189                                                                  ;;;LDA W,X
  :PRINT#1,WL
  :PRINT#1,WH

58340 PRINT#1,157                                                               ;;;STA V,X
  :PRINT#1,VL
  :PRINT#1,VH
  :PRINT#1,202                                                                  ;;;DEX

58360 PRINT#1,16                                                                ;;;BPL .-9
  :PRINT#1,247
  :PC=PC+11                                                                     ;;;transfer finished...count bytes I back to main loop top
  :GOTO 50260

' IF
58500 GOSUB 49100                                                               ;;;handle 'IF' statements
  :GOSUB 49190                                                                  ;;;get variable following the 'IF' ---- address in Z
  :PRINT#1,173                                                                  ;;;LDA Z
  :PRINT#1,ZL
  :PRINT#1,ZH
  :PRINT#1,240                                                                  ;;;BEO .+3 (if variable = 0, skip over following 3 bytes)

58580 PRINT#1,3
  :PC=PC+5
  :GOSUB 49100                                                                  ;;;get next character
  :IF (CH<>137) AND (CH<>141) GOTO 49200                                        ;;;abort if not 'GOTO' or 'GOSUB' else, fall into following

' GOTO / GOSUB
58700 IF CH=141 THEN PRINT#1,32                                                 ;;;handle 'GOSUB' or 'GOTO' statements
  :GOTO 58760                                                                   ;;;'GOSUB' becomes 'JSR'

58740 PRINT#1,76                                                                ;;;and 'GOTO' becomes 'JNP'

58760 GO%(GC,0)=PC-32766                                                        ;;;store in GO% array the address of the conpiled-code 's bytes after
  :PRINT#1,0                                                                    ;;;the opcode, to be filled in later (second-pass)
  :PRINT#1,0                                                                    ;;;print zeroes for now, to hold the spaces
  :PC=PC+3
  :TL=0                                                                         ;;; initialize target line number TL

58800 GOSUB 49100
  :IF (CH<48) OR (CH>57) GOTO 58840                                             ;;;read in ASCII target line number

58820 TL=10*TL+CH-48                                                            ;;;and convert it to a number in TL
  :GOTO 58800


58840 GO%(GC,1)=TL                                                             ;;;record target line number in GO%
  :GC=GC+1                                                                      ;;;increnent GOTO/GOSUB counter GC
  :GOTO 50260                                                                   ;;;back to top of Main loop

' PRINT
59000 GOSUB 49100                                                               ;;;handle a 'PRINT' statement
  :IF (CH=0) OR (CH=58) GOTO 59400                                              ;;;when done with line or statement, print a <CRLF> before quitting

59040 IF (CH=44) OR (CH=59) GOTO 59000                                          ;;;keep going if ',' or ';' encountered within 'PRINT'

59080 GOSUB 49190                                                               ;;;get address of variable to be printed
  :GOSUB 49400                                                                  ;;;transfer it to primary accumulator
  :PRINT#1,32                                                                   ;;;JSR $DCAF converts it to ASCII string at top of page 1
  :PRINT#1,175
  :PRINT#1,220

59100 PRINT#1,162                                                               ;;;LDX #0
  :PRINT#1,0
  :PRINT#1 ,189                                                                 ;;;LDA $100,X
  :PRINT#1,0
  :PRINT#1,1

59120 PRINT#1,240                                                               ;;;BEQ .+6 (end of string is Marked by a zero)
  :PRINT#1,6
  :GOSUB 59300                                                                  ;;;subroutine outputs code to print accumulator on screen
  :PRINT#1,232                                                                  ;;;INX
  :PRINT#1,208                                                                  ;;;BNE ,-$B (always loop back...number is never longer than 16 bytes)
  :PRINT#1,245

59160 PRINT#1,169                                                               ;;;LDA #32 (ASCII blank space)
  :PRINT#1,32
  :GOSUB 59300                                                                  ;;;print accumulator out
  :PC=PC+21                                                                     ;;;count bytes
  :GOTO 59000                                                                   ;;;and continue with print statement, looping to above

59300 PRINT#1,32                                                                ;;;JSR $FFD2 prints out accunulator onto screen
  :PRINT#1,210
  :PRINT#1,255
  :RETURN

59400 PRINT#1,169                                                               ;;;LDA #$D (<CRLF>)
  :PRINT#1,13
  :GOSUB 59300                                                                  ;;;print it
  :PC=PC+5
  :GOTO 50260                                                                   ;;;back to top of Main loop

' INPUT
59500 GOSUB 49100                                                               ;;;handle 'INPUT' statement
  :IF (CH=0) OR (CH=58) GOTO 50260                                              ;;;finished when 'end-of-line' or ':'

59540 IF (CH=44) OR (CH=59) GOTO 59500                                          ;;;keep going if ',' or ';'

59560 GOSUB 49190
  :PRINT#1,169                                                                  ;;;LDA #CH
  :PRINT#1,CH
  :GOSUB 59300                                                                  ;;;print prompting letter on screen before input request

59580 PRINT#1,169                                                               ;;;LDA #$3F (ascii '?')
  :PRINT#1,63
  :GOSUB 59300                                                                  ;;;print it

59600 PRINT#1,162                                                               ;;;LDX #0 (to initialize counter for input characters)
  :PRINT#1,0
  :PRINT#1,32                                                                   ;;;JSR $FFCF (inputs, with cursor for user to edit)
  :PRINT#1,207
  :PRINT#1,255

59620 PRINT#1,149                                                               ;;;STA $A,X (store input stuff in BASIC input buffer)
  :PRINT#1,10
  :PRINT#1,232                                                                  ;;;INX
  :PRINT#1,201                                                                  ;;;CMP #$D (check for <CRLF>)
  :PRINT#1,13

59660 PRINT#1,208                                                               ;;;BNE .-$A (loop and keep inputting)
  :PRINT#1,246
  :GOSUB 59300                                                                  ;;;echo <CRLF>


59680 PRINT#1,169                                                               ;;;LDA #0
  :PRINT#1,0
  :PRINT#1,133                                                                  ;;;STA $72
  :PRINT#1,114
  :PRINT#1,169                                                                  ;;;LDA #$A
  :PRINT#1,10

59700 PRINT#1,133                                                               ;;;STA $71 (set up string pointer to beginning of input)
  :PRINT#1,113
  :PRINT#1,202                                                                  ;;;DEX
  :PRINT#1,138                                                                  ;;;TXA (transfer nuMber of characters in string to A)
  :PRINT#1,32                                                                   ;;;JSR $D68D (convert string to floating-point in prinary accum)
  :PRINT#1,141

59760 PRINT#1,214
  :PRINT#1,162                                                                  ;;;LDX #ZL
  :PRINT#1,ZL
  :PRINT#1,160                                                                  ;;;LDY #ZH (point to target location to store result)
  :PRINT#1,ZH

59780 PRINT#1,32                                                                ;;;JSR $DAA6 (store prinary memory contents in target)
  :PRINT#1,166
  :PRINT#1,218
  :PC=PC+45                                                                     ;;;count bytes (a lot!!)
  :GOTO 59500                                                                   ;;;and loop

60000 PRINT "FIRST PASS FINISHED!!!"                                            ;;;conpiling progran finished
  :PRINT"PROGRAM OCCUPIES" ;OC ; "THROUGH" ;PC-1                                ;;;print statistics

60020 PRINT"VARIABLES OCCUPY";OV;"THROUGH" ;OV+129

60060 PRINT#1,-1                                                                ;;;mark end of first pass on tape file
  :IF GC=0 GOTO 61000                                                           ;;;skip if no junps to be corrected

60080 FOR X=0 TO GC-1
  :PRINT#1,GO%(X,0)+32767                                                       ;;;print byte to be fixed
  :L=GO%(X,1)                                                                   ;;;look up desired line nunber to branch to
  :Z=LN%(L)+32767                                                               ;;;find what the program counter was there

60100 IF Z=0 THEN PRINT "TRANSFER TO NONEXISTENT LINE #";L
  :GOTO 61000                                                                   ;;;catch errors

60120 ZH=INT(Z/256)
  :ZL=Z-256*ZH
  :PRINT#1,ZL                                                                   ;;;record correct jump (JNP or JSR) target
  :PRINT#1,ZH
  :NEXT X                                                                       ;;;for all that need fixing

61000 PRINT#1,-1                                                                ;;;mark end of file
  :CLOSE 1                                                                      ;;;thafs all, folks! !!!!

------------------------
Chargement

10 OPEN 1: INPUT #1,PC:PRINT "BEGIN AT";PC
20 INPUT #1,A:IF A=>0 THEN POKE PC,A:PC=PC+1:GOTO 20
30 PRINT "END AT";PC-1
40 INPUT #1,PC:IF PC<0 THEN CLOSE 1:END
50 INPUT #1,A:POKE PC,A:INPUT #1,A:POKE PC+1,A:GOTO 40

------------------------
Statement                             Description                                  Examples
     Type

LET                     initialize a variable during compilation;               LET A=1
                        must be 60 characters or less, cannot use variables     LET B=EXP(2)/2

PRINT                   output a variable's value to screen;                    PRINT X
                        gives 1 space between values printed and                PRINT P,Q;R
                        a "return" at end                                       PRINT

INPUT                   read in a number from keyboard                          INPUT Z
                        (prompts with variable name)                            INPUT J;K,L

IF                      conditional branch: the GOTO or GOSUB                   IF S GOTO 0
                        following will be executed if the variable              IF T GOSUB 255
                        after the IF is nonzero; skipped otherwise

GOTO                    unconditional transfer to line number                   GOTO 89

GOSUB                   subroutine call; returns to statement                   GOSUB 176
                        following GOSUB when done

REM                     remarks; skip to next line number                       REM THIS IS A TEST

RETURN                  return from subroutine, and return to                   RETURN
                        BASIC control at program end

arithmetic              perform the specified floating-point                    C=D
                        arithmetic operation, statements can                    E=F+G
                        have only one operation to right of                     H=-I
                        equals sign                                             J=ATN(K)


Notes:
1. All variables must have single-letter alphabetic names.
2. All lines must be numbered thru 255.
3. Variables must be initialized before use; failure to do this results in a random initial value.


------------------------
P register: $B0-$B5
S register: $B8-$BF
BASIC Input Buffer: $0A-...

------------------------

FLOPTRAN-IV program
This program calculates the possible outcomes of a dice roll int the game Risk, with the attacker rolling three dive and the defender rolling two dice.

1 LET F=0
  :LET G=0
  :LET H=0
  :LET J=0
  :lET A=7
  :LET B=6
  :LET C=6
  :LET O=1

3 A=A-O
  :IF A GOTO 9

4 GOTO 100

5 LET B=6

7 LET A=6

9 LET D=7
  :LET E=6

11 D=D-O
  :IF D GOTO 21

13 E=E-O
  :IF E GOTO 17

15 GOTO 3

17 LET D=6

21 I=+O

23 V=A
  :W=B
  :X=C
  :Y=D
  :Z=E

25 Q=X-W
  :Q=SGN(Q)
  :Q=Q+O
  :IF Q GOTO 29

27=W
  :X=W
  :W=T

29 Q=W-V
  :Q=SGN(Q)
  :Q=Q+O
  :IF Q GOTO 39

31 T=W
  :W=V
  :V=T

33 Q=X-W
  :Q=SGN(Q)
  :Q=Q+O
  :IF Q GOTO 39

35 T=X
  :X=W
  :W=T

39 Q=Z-Y
  :Q=SGN(Q)
  :Q=Q+O
  :IF Q GOTO 43

41 T=Z
  :Z=Y
  :Y=T

43 A=Z-X
  :Q=SGN(Q=
  :Q=Q+O
  :IF Q GOTO 51

45 Q=V-W
  :Q=SGN(Q)
  :Q=Q+O
  :IF Q GOTO 53

47 GOTO 61

51 Q=Y-W
  :Q=SGN(Q)
  :Q=Q+O
  :IF Q GOTO 71

53 G=G+O
  :GOTO 11

61 F=F+O
  :GOTO 11

71 H=H+O
  :GOTO 11

100 B=B-O
  :IF B GOTO 7

110 C=C+O
  :IF C GOTO 5

120 PRINT F,G,H
  :F=F/I
  :G=G/I
  :H=H/I
  :PRINT F,G,H
  :RETURN

