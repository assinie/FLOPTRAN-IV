Proc L49000
	X=                                                             :
EndProc

Proc L49100
	TP=TP+1 \
	CH=PEEK(TP) \
	IF CH=32 GOTO @L49100

EndProc

Proc L49190
	IF (CH>64) AND (CH<91) GOTO @L49300

Proc L49200
	PRINT \
	PRINT "***ERR0R IN LINE";LI;"***"
EndProc @L61000

Proc L49300
	Z=5*(CH-65)+OV \
	ZH=INT(Z/256) \
	ZL=Z-256*ZH
EndProc

Proc L49400
	;' LDA #ZL
	;' LDY #ZH
	LPRINT 169 \
	LPRINT ZL \
	LPRINT 160 \
	LPRINT ZH

	;' JSR $DA74	; with A & Y set up as above transfers the value of the variable being pointed at to the primary accumulator for floating-point operations
	; 'ATMOS: $DE7B
	; 'ORIC-1: $DE73
	LPRINT 32 \
	LPRINT #7B \
	LPRINT #DE \
	PC=PC+7
EndProc



Proc L49600
	; 180->185
	; SGN,INT,ABS,USR,FRE,POS
	;  D6  D7  D8   D9   DA  DB (214 -> 219)
	; POS(): $D4AA <- Appel avec une valeur dans A (A=0-> POS ecran, A!=0->POS imprimante)
	; POS est appelé avec l'exposant de la valeur en paramètre (octet en $D0)
	; Remplacer $D4AA par $D4B4 pour forcer POS(0) ou par $D4AC pour renvoyer la position de l'imprimante si elle esr en fonction ou de l'écran sinon
	DATA #21,#DF,#BD,#DF,#49,#DF,#21,#00,#85,#D4,#AA,#D4

	;  DC->HEX$, DD->&()
	DATA 0,0,0,0

	; 186->191
	; SQR,RND,LN,EXP,COS,SIN
	;  DE  DF  E0  E1  E2  E3 (222->227)
	;  LOG()->E8
	DATA #2E,#E2,#4F,#E3,#AF,#DC,#AA,#E2,#8B,#E3,#92,#E3

	; 192->194, 170->174
	; TAN,ATN,PEEK,+, -, *,  /, ^
	;  E4  E5   E6  CC CD CE  CF D0 (228->230, 204->208)
	;  E7->DEEK
	DATA #DB,#E3,#3F,#E4,#38,#D9,#25,#DB,#0E,#DB,#BF,#DC,#E7,#DD,#38,#E2

	FOR X=0 TO 21 \
		READ ML%(X,0),ML%(X,1) \
	NEXT X


	FOR X=0 TO 255 \
		LI%(X)=-32767 \
	NEXT X
EndProc


Proc L50000
	DIM X,LI%(255),GO%(127,1),ML%(21,1) \
	P=1281 \
	GC=0

Proc L50040
	L=PEEK(P+2)+256*PEEK(P+3) \
	IF L<>@L49000 THEN P=PEEK(P)+256*PEEK(P+1) \
		GOTO @L50040


	XL=P+6 \
	GOSUB @L49600 \
	INPUT "COMPILED DATA FILE NAME";C$ \
	' OPEN 1,1,1,C$

	INPUT "ORIGIN FOR CODE";OC \
	LPRINT OC \
	PC=OC


	INPUT "VARIABLE TABLE ORIGIN";OV \
	TP=1284

Proc L50200
	LI=PEEK(TP-1)+256*PEEK(TP) \
	IF LI>255 GOTO @L60000

	LI%(LI)=PC-32767 \
	PRINT "COMPILING LINE";LI

&L50240
	GOSUB @L49100

&L50260
	IF CH=0 THEN TP=TP+4 \
	GOTO @L50200

	; ':'
	IF CH=58 GOTO @L50240

	; 'LET
	IF CH=150 GOTO @L58000

	; 'PRINT
	IF CH=186 GOTO @L59000

	; 'INPUT
	IF CH=146 GOTO @L59500

	; 'IF
	IF CH=153 GOTO @L58500

	; GOTO / GOSUB
	IF (CH=151) OR (CH=155) GOTO @L58700

	; 'REM
	IF CH=157 GOTO @L56500

	; 'RETURN
	IF CH=156 THEN LPRINT 96 \
		PC=PC+1 \
		GOTO @L50240


	GOSUB @L49190 \
	VL=ZL \
	VH=ZH

	; '='?
	GOSUB @L49100 \
	IF CH<>212 GOTO @L49200

	; 'HEX$(): $DC
	; '&(): $DD
	; 'SGN -> PEEK: $D6 -> $DB et $DE/$DF, $E8, $E1->E6,
	; 'IF (CH>179) AND (CH<195) GOTO @L51000
	GOSUB @L49100 \
	IF (CH>213) AND (CH<231) GOTO @L51000


	; '-'
	IF CH=205 GOTO @L55600

	GOSUB @L49190 \
	WL=ZL \
	WH=ZH



	GOSUB @L49100 \
	IF (CH=0) OR (CH=58) GOTO @L58300

	; '+', '-', '*', '/', '^'
	; IF (CH<170) OR (CH>174) GOTO @L49200
	IF (CH<204) OR (CH>208) GOTO @L49200

	; 'Premier opérateur: ML%(170-155) <=> ML%(15)
	; 'ORIC/ATMOS: Premier opérateur: ML%(204-187) <=> ML%(17)
	FU=CH-187 \
	GOSUB @L49100 \
	GOSUB @L49190 \
	GOSUB @L49400 \

	; 'LDA #WL
	; 'LDY #WH
	LPRINT 169 \
	LPRINT WL \
	LPRINT 160 \
	LPRINT WH \
	PC=PC+4

	; '+' OU '*'?
	; /?\ BUG?
	;IF (FU=17) OR (FU=19) GOTO @L51080

	; 'JSR $D95E	; Transfert W to S Register
	; 'ATMOS: $DD51
	; 'ORIC-1:$DD4D
	LPRINT 32 \
	LPRINT #51 \
	LPRINT #DD \
	PC=PC+3
EndProc @L51080

' Monadic function
Proc L51000
	; 'Attention: Codes DC/DD non pris en compte (HEX$(), &())
	; 'FU=CH-180
	IF CH=220 OR CH=221 GOTO @L49200

	FU=CH-214 \
	GOSUB @L49100 \
	IF CH<>40 GOTO @L49200

	GOSUB @L49100 \
	GOSUB @L49190 \
	GOSUB @L49400

	GOSUB @L49100 \
	IF CH<>41 GOTO @L49200

&L51080
	; 'JSR ROM Function
	LPRINT 32 \
	LPRINT ML%(FU,0) \
	LPRINT ML%(FU,1) \
	PC=PC+3

&L51100
	; 'LDX #VL
	; 'LDY #VH
	LPRINT 162 \
	LPRINT VL \
	LPRINT 160 \
	LPRINT VH \

	; 'JSR $DAA6	; store P Register contents in target
	; 'ATMOS: $DEB0
	; 'ORIC-1: $DEA8
	LPRINT 32 \
	LPRINT #B0 \
	LPRINT #DE \
	PC=PC+7
EndProc @L50240

Proc L55600
	; 'LDA #$FF
	GOSUB @L49100 \
	GOSUB @L49190 \
	GOSUB @L49400 \
	LPRINT 169 \
	LPRINT 255

	; 'EOR $D5
	; 'STA $D5
	LPRINT 69 \
	LPRINT #D5 \
	LPRINT 133 \
	LPRINT #D5 \
	PC=PC+6
EndProc @L51100

' REM
Proc L56500
	GOSUB @L49100 \
	IF CH>0 GOTO @L56500
EndProc @L50260

' LET
Proc L58000
	GOSUB @L49100 \
	GOSUB @L49190 \
	GOSUB @L49100 \
	VL=TP+1 \
	IF CH<>212 GOTO @L49200

&L58080
	GOSUB @L49100 \
	IF (CH<>58) AND (CH<>0) GOTO @L58080


	IF TP-VL>60 THEN PRINT "LINE TOO LONG!" \
		GOTO @L49200

	FOR L=VL TO TP-1 \
		WL=PEEK(L) \
		POKE XL+L-VL,WL \
	NEXT L

	GOSUB @L49000 \
	FOR L=VL TO TP-1 \
		POKE XL+L-VL,32 \
	NEXT L

	; 'CLC
	; 'BCC +5
	LPRINT 24 \
	LPRINT 144 \
	LPRINT 5 \
	PC=PC+3 \
	VL=256*PEEK(157)+PEEK(156)+2

	FOR L=VL TO VL+4 \
		LPRINT PEEK(L) \
	NEXT L \
	WH=INT(PC/256) \
	WL=PC-256*WH \

	VL=ZL \
	VH=ZH \
	PC=PC+5 \

&L58300
	; 'LDX #$04
	; 'LDA W,X
	LPRINT 162 \
	LPRINT 4 \
	LPRINT 189 \
	LPRINT WL \
	LPRINT WH

	; 'STA V,X
	; 'DEX
	LPRINT 157 \
	LPRINT VL \
	LPRINT VH \
	LPRINT 202 \

	; 'BPL -9
	LPRINT 16 \
	LPRINT 247 \
	PC=PC+11
EndProc @L50260 \

' IF
Proc L58500
	; 'LDA Z
	; 'BEQ +3
	GOSUB @L49100 \
	GOSUB @L49190 \
	LPRINT 173 \
	LPRINT ZL \
	LPRINT ZH \
	LPRINT 240

	LPRINT 3 \
	PC=PC+5 \
	GOSUB @L49100 \
	IF (CH<>151) AND (CH<>155) GOTO @L49200

' GOTO / GOSUB
Proc L58700
	; 'GOSUB'?
	; 'JSR $0000
	IF CH=155 THEN LPRINT 32 \
		GOTO @L58760

	; 'JMP $0000
	LPRINT 76

&L58760
	GO%(GC,0)=PC-32766 \
	LPRINT 0 \
	LPRINT 0 \
	PC=PC+3 \
	TL=0

&L58800
	GOSUB @L49100 \
	IF (CH<48) OR (CH>57) GOTO @L58840

	TL=10*TL+CH-48 \
	GOTO @L58800


&L58840
	GO%(GC,1)=TL \
	GC=GC+1
EndProc @L50260

' PRINT
Proc L59000
	GOSUB @L49100 \
	IF (CH=0) OR (CH=58) GOTO @L59400

	IF (CH=44) OR (CH=59) GOTO @L59000

	; 'JSR $DCAF	; converts P Register to ASCII string at top of page 1
	; 'ATMOS: $E0D5
	; 'ORIC-1: $E0D1
	GOSUB @L49190 \
	GOSUB @L49400 \
	LPRINT 32 \
	LPRINT #D5 \
	LPRINT #E0

	; 'LDX #$00
	; 'LDA $100,X
	LPRINT 162 \
	LPRINT 0 \
	LPRINT 189 \
	LPRINT 0 \
	LPRINT 1

	; 'BEQ +6
	; 'INX
	; 'BNE -11
	LPRINT 240 \
	LPRINT 6 \
	GOSUB @L59300 \
	LPRINT 232 \
	LPRINT 208 \
	LPRINT 245

	; 'LDA #32
	LPRINT 169 \
	LPRINT 32 \
	GOSUB @L59300 \
	PC=PC+21
EndProc @L59000

Proc L59300
	; 'JSR $FFD2	; Print out accumulatror onto screen
	; 'ATMOS: $CCD9
	; 'ORIC-1: $CC12
	LPRINT 32 \
	LPRINT #D9 \
	LPRINT #CC
EndProc

Proc L59400
	; 'LDA #$0D
	; 'LDA #$0A	; Ajout pour Oric/Atmos
	LPRINT 169 \
	LPRINT 13 \
	GOSUB @L59300 \
	LPRINT 169 \
	LPRINT 10 \
	GOSUB @L59300 \
	PC=PC+10
EndProc @L50260

' INPUT
Proc L59500
	GOSUB @L49100 \
	IF (CH=0) OR (CH=58) GOTO @L50260

	IF (CH=44) OR (CH=59) GOTO @L59500

	; 'LDA #CH
	GOSUB @L49190 \
	LPRINT 169 \
	LPRINT CH \
	GOSUB @L59300

	; 'LDA #$3F
	LPRINT 169 \
	LPRINT 63 \
	GOSUB @L59300

	; 'LDX #$00
	; 'JSR $FFCF	; inputs, with cursor for user to edit
	; 'JSR $C5E8	; inputs, with cursor for user to edit
	LPRINT 162 \
	LPRINT 0 \
	LPRINT 32 \
	LPRINT #92 \
	LPRINT #C5

	; 'STA $0A,X	; Store input stuff into BASIC Buffer
	; 'INX
	; 'CMP #$0D
	; Inutile avec Oric/Atmos, le routine $C592 fait tout le travail
;	LPRINT 149 \
;	LPRINT #35 \
;	LPRINT 232 \
;	LPRINT 201 \
;	LPRINT 13

	; 'BNE -10
;	LPRINT 208 \
;	LPRINT 246 \
;	GOSUB @L59300


	; 'LDA #$00
	; 'STA $EA
	; 'LDA #$35
	LPRINT 169 \
	LPRINT 0 \
	LPRINT 133 \
	LPRINT #EA \
	LPRINT 169 \
	LPRINT #35

	; 'STA $E9
;	; 'DEX
;	; 'TXA
	; Oric/Atmos
	; 'JSR $00E8
	; 'JSR $DFE7	; convert string to floating-point in P Register
	LPRINT 133 \
	LPRINT #E9 \
;	LPRINT 202 \
;	LPRINT 138 \
	LPRINT 32 \
	LPRINT #E8 \
	LPRINT #00\
	LPRINT 32 \
	LPRINT #E7 \
	LPRINT #DF


	; 'LDX #ZL
	; 'LDY #ZH
	LPRINT 162 \
	LPRINT ZL \
	LPRINT 160 \
	LPRINT ZH

	; 'JSR $DAA6	; store P Register contents in target
	; 'ATMOS: $DEB0
	; 'ORIC-1: $DEA8
	LPRINT 32 \
	LPRINT #B0 \
	LPRINT #DE \
	PC=PC+45-9
EndProc @L59500

Proc L60000
	PRINT "FIRST PASS FINISHED!!!" \
	PRINT "PROGRAM OCCUPIES" ;OC ; "THROUGH" ;PC-1

	PRINT "VARIABLES OCCUPY";OV;"THROUGH" ;OV+129

	LPRINT -1 \
	IF GC=0 GOTO @L61000

	FOR X=0 TO GC-1 \
		LPRINT GO%(X,0)+32767 \
		L=GO%(X,1) \
		Z=LI%(L)+32767

		IF Z=0 THEN PRINT "TRANSFER TO NONEXISTENT LINE #";L \
			GOTO @L61000

		ZH=INT(Z/256) \
		ZL=Z-256*ZH \
		LPRINT ZL \
		LPRINT ZH \
	NEXT X

Proc L61000
	LPRINT -1 \
	CLOSE 1

End
