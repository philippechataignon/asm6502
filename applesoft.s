;********************************************************************************
;* Disassembly of Applesoft II BASIC, as found in the Apple ][+.                *
;*                                                                              *
;* This project is a conversion of Bob Sander-Cederlof's "S-C DocuMentor:       *
;* Applesoft", with minor edits.  See http://www.txbobsc.com/scsc/scdocumentor/ *
;* for the original disassembly listing.                                        *
;*                                                                              *
;* Changes from the original include conversion of comments to mixed-case,      *
;* correction of typographical errors, and general reformatting to fit          *
;* SourceGen's constraints.  Some text has been changed to fit personal         *
;* preference (e.g. PTR vs. PNTR for "pointer").  In cases where an operand     *
;* expression is too complex, the original can be found in the comment field    *
;* (look for occurrences of "should be").  It is likely some errors have been   *
;* introduced; please consult the original material when in doubt.              *
;*                                                                              *
;* Applesoft is copyright by Microsoft and Apple Computer.                      *
;* Apple ][+ ROM image obtained from AppleWin (Apple2_Plus.rom).                *
;********************************************************************************
;* Project created by Andy McFadden, using 6502bench SourceGen v1.4.            *
;* Last updated 2019/10/27                                                      *
;********************************************************************************
ERR_NOFOR   =     $00
TKN_CNTR    =     $0f
ERR_SYNTAX  =     $10
ERR_NOGOSUB =     $16
ERR_NODATA  =     $2a
ERR_ILLQTY  =     $35
ERR_OVERFLOW =    $45
P_OR        =     $46
ERR_MEMFULL =     $4d
P_AND       =     $50
ERR_UNDEFSTAT =   $5a
P_REL       =     $64
ERR_BADSUBS =     $6b
ERR_REDIMD  =     $78
P_ADD       =     $79
P_MUL       =     $7b
P_PWR       =     $7d
P_NEQ       =     $7f
TOK_FOR     =     $81
TOK_DATA    =     $83
ERR_ZERODIV =     $85
ERR_ILLDIR  =     $95
ERR_BADTYPE =     $a3
TOK_GOTO    =     $ab
ERR_STRLONG =     $b0
TOK_GOSUB   =     $b0
TOK_REM     =     $b2
TOK_PRINT   =     $ba
ERR_FRMCPX  =     $bf
TOK_TAB     =     $c0
TOK_TO      =     $c1
TOK_FN      =     $c2
TOK_SPC     =     $c3
TOK_THEN    =     $c4
TOK_AT      =     $c5
TOK_NOT     =     $c6
TOK_STEP    =     $c7
TOK_PLUS    =     $c8
TOK_MINUS   =     $c9
TOK_GREATER =     $cf
TOK_EQUAL   =     $d0
ERR_CANTCONT =    $d2
TOK_SGN     =     $d2
TOK_SCRN    =     $d7
ERR_UNDEFFUNC =   $e0
GOWARM      =     $00 ;gets "JMP RESTART" (3b)
GOSTROUT    =     $03 ;gets "JMP STROUT" (3b)
USRVEC      =     $0a ;USR() command vector (initially $E199) (3b)
CHARAC      =     $0d ;used by string utility
ENDCHR      =     $0e ;used by string utility
EOL_PNTR    =     $0f
DIMFLG      =     $10
VALTYP      =     $11 ;flag for last FAC operation ($00=num, $FF=str) (2b)
DATAFLG     =     $13
SUBFLG      =     $14
INPUTFLG    =     $15
CPRMASK     =     $16
HGR_SHAPE   =     $1a ;(2b)
HGR_BITS    =     $1c ;hi-res color mask
HGR_COUNT   =     $1d ;hi-res high-order byte of step for line
MON_CH      =     $24 ;cursor horizontal displacement
HBASL       =     $26 ;base address for hi-res drawing (low)
HBASH       =     $27 ;base address for hi-res drawing (high)
MON_H2      =     $2c ;right end of horizontal line drawn by HLINE
MON_V2      =     $2d ;bottom of vertical line drawn by VLINE
HMASK       =     $30 ;hi-res graphics on-the-fly bit mask
MON_INVFLAG =     $32 ;text mask (255=normal, 127=flash, 63=inv)
MON_PROMPT  =     $33 ;prompt character
MON_KSWL    =     $38 ;character input hook (lo)
MON_A1L     =     $3c ;general purpose
MON_A1H     =     $3d ;general purpose
MON_A2L     =     $3e ;general purpose
MON_A2H     =     $3f ;general purpose
LINNUM      =     $50 ;line number (2b)
TEMPPT      =     $52 ;temporary point (2b)
TEMPST      =     $55
INDEX       =     $5e ;temp (stack) pointer for moving strings (2b)
DEST        =     $60 ;pointer (2b)
RESULT      =     $62 ;(5b)
TEXTTAB     =     $67 ;pointer to start of Applesoft program (2b)
VARTAB      =     $69 ;pointer to start of Applesoft variables (2b)
ARYTAB      =     $6b ;pointer to start of Applesoft array space (2b)
STREND      =     $6d ;pointer to end of numeric storage (2b)
FRETOP      =     $6f ;pointer to end of string storage (2b)
FRESPC      =     $71 ;temporary pointer for string-storage routines (2b)
MEMSIZE     =     $73 ;HIMEM (2b)
CURLIN      =     $75 ;current line number (2b)
OLDIN       =     $77 ;last line executed (2b)
OLDTEXT     =     $79 ;old text pointer (2b)
DATLIN      =     $7b ;current lin # from which data is being read (2b)
DATPTR      =     $7d ;points to mem from which data is being read (2b)
INPTR       =     $7f ;(2b)
VARNAM      =     $81 ;holds last-used variable's name (2b)
VARPNT      =     $83 ;pointer to last-used variable's value (2b)
FORPNT      =     $85 ;general pointer (2b)
TXPSV       =     $87 ;pointer (2b)
CPRTYP      =     $89
FNCNAM      =     $8a
TEMP3       =     $8a ;fp math register (5b)
DSCPTR      =     $8c ;pointer (2b)
DSCLEN      =     $8f
JMPADRS     =     $90 ;jump address; $90 is set to $4C (3b)
LENGTH      =     $91
TEMP1       =     $93 ;fp math register
HIGHDS      =     $94 ;block copy pointer (2b)
HIGHTR      =     $96 ;block copy pointer (2b)
TEMP2       =     $98 ;fp math register
TMPEXP      =     $99
EXPON       =     $9a
LOWTR       =     $9b ;general pointer (2b)
FAC         =     $9d ;floating point accumulator (6b)
FAC_SIGN    =     $a2 ;single byte sign of FAC
SERLEN      =     $a3
SHIFT_SIGN_EXT =  $a4
ARG         =     $a5 ;secondary floating point accumulator (6b)
ARG_SIGN    =     $aa
STRNG1      =     $ab ;pointer to a string (2b)
STRNG2      =     $ad ;pointer to a string (2b)
PRGEND      =     $af ;pointer to end of program (2b)
CHRGET      =     $b1 ;get next character or Applesoft token
CHRGOT      =     $b7 ;get next, but don't advance TXTPTR
TXTPTR      =     $b8 ;points at next char or token (2b)
RNDSEED     =     $c9 ;floating point random number (5b)
HGR_DX      =     $d0 ;(2b)
HGR_DY      =     $d2
HGR_QUAD    =     $d3
HGR_E       =     $d4 ;(2b)
LOCK        =     $d6 ;set to $80 to auto-run
ERRFLG      =     $d8 ;$80 if onerr active
ERRLIN      =     $da ;(2b)
ERRPOS      =     $dc ;(2b)
ERRNUM      =     $de
ERRSTK      =     $df
HGR_X       =     $e0 ;(2b)
HGR_Y       =     $e2
HGR_COLOR   =     $e4
HGR_HORIZ   =     $e5 ;byte index from GBASH,L
HGR_PAGE    =     $e6 ;hi-res page to draw on ($20 or $40)
HGR_SCALE   =     $e7 ;hi-res graphics scale factor
HGR_SHAPE_PTR =   $e8 ;hi-res shape table pointer (2b)
HGR_COLLISIONS =  $ea ;collision counter
FIRST       =     $f0
SPEEDZ      =     $f1 ;controls text output speed
TRCFLG      =     $f2
FLASH_BIT   =     $f3 ;=$40 for flash, else =$00
TXTPSV      =     $f4 ;(2b)
CURLSV      =     $f6 ;(2b)
REMSTK      =     $f8             ;stack ptr before each STT
HGR_ROTATION =    $f9
STACK       =     $0100
INPUT_BUFFER =    $0200
AMPERV      =     $03f5
KBD         =     $c000           ;R last key pressed + 128
TXTCLR      =     $c050           ;RW display graphics
MIXCLR      =     $c052           ;RW display full screen
MIXSET      =     $c053           ;RW display split screen
TXTPAGE1    =     $c054           ;RW display page 1
TXTPAGE2    =     $c055           ;RW display page 2 (or read/write aux mem)
LORES       =     $c056           ;RW display lo-res graphics
HIRES       =     $c057           ;RW display hi-res graphics
MON_PLOT    =     $f800           ;lo-res plot at X=Y-reg, Y=Acc
MON_HLINE   =     $f819           ;lo-res horiz line at Y=Acc with X from $2c
MON_VLINE   =     $f828           ;lo-res vert line at X=Y-reg and Y from Acc to $2b
MON_SETCOL  =     $f864           ;set lo-res color to Acc
MON_SCRN    =     $f871           ;load Acc with lo-res value at Y=Acc, X=X-reg
MON_PREAD   =     $fb1e           ;read paddle specifed by X-reg, return in Y-reg
MON_SETTXT  =     $fb39           ;set screen to text mode
MON_SETGR   =     $fb40           ;set screen to graphics mode
MON_TABV    =     $fb5b           ;place cursor at line (A-reg) and column (ch)
MON_HOME    =     $fc58           ;clear screen and reset text output to top-left
MON_WAIT    =     $fca8           ;delay for (26 + 27*Acc + 5*(Acc*Acc))/2 cycles
MON_RD2BIT  =     $fcfa           ;cassette read
MON_RDKEY   =     $fd0c           ;read key
MON_GETLN   =     $fd6a           ;get a line of input
MON_COUT    =     $fded           ;print Acc to output device
MON_INPORT  =     $fe8b
MON_OUTPORT =     $fe95
MON_WRITE   =     $fecd           ;write data to cassette
MON_READ    =     $fefd           ;read data from cassette
MON_READ2   =     $ff02           ;read data from cassette

            org     $d000
; Branch table for tokens.  Entries are (address-1).
TOKEN_ADDR_TABLE
            word    END-1           ;token $80
            word    FOR-1
            word    NEXT-1
            word    DATA-1
            word    INPUT-1
            word    DEL-1
            word    DIM-1
            word    READ-1
            word    GR-1
            word    TEXT-1
            word    PR_NUMBER-1
            word    IN_NUMBER-1
            word    CALL-1
            word    PLOT-1
            word    HLIN-1
            word    VLIN-1
            word    HGR2-1          ;$90
            word    HGR-1
            word    HCOLOR-1
            word    HPLOT-1
            word    DRAW-1
            word    XDRAW-1
            word    HTAB-1
            word    MON_HOME-1      ;HOME command goes directly to monitor routine
            word    ROT-1
            word    SCALE-1
            word    SHLOAD-1
            word    TRACE-1
            word    NOTRACE-1
            word    NORMAL-1
            word    INVERSE-1
            word    FLASH-1
            word    COLOR-1         ;$a0
            word    POP-1
            word    VTAB-1
            word    HIMEM-1
            word    LOMEM-1
            word    ONERR-1
            word    RESUME-1
            word    RECALL-1
            word    STORE-1
            word    SPEED-1
            word    LET-1
            word    GOTO-1
            word    RUN-1
            word    IF-1
            word    RESTORE-1
            word    AMPERV-1        ;jumps directly to the page 3 vector
            word    GOSUB-1         ;$b0
            word    POP-1           ;RETURN and POP go to same handler
            word    REM-1
            word    STOP-1
            word    ONGOTO-1
            word    WAIT-1
            word    LOAD-1
            word    SAVE-1
            word    DEF-1
            word    POKE-1
            word    PRINT-1
            word    CONT-1
            word    LIST-1
            word    CLEAR-1
            word    GET-1
            word    NEW-1           ;$bf
; No direct pointer for $C0-C7: TAB(, TO, FN, SPC(, THEN, AT, NOT, STEP.  Math
; operation addresses are below, in MATHTBL.
; 
; Additional functions follow. Addresses are the actual entry points,
; unadjusted.
            word    SGN             ;$d2
            word    INT
            word    ABS
            word    USRVEC          ;jumps directly to zero-page vector
            word    FRE
            word    ERROR           ;SCRN(
            word    PDL
            word    POS
            word    SQR
            word    RND
            word    LOG
            word    EXP
            word    COS
            word    SIN
            word    TAN             ;$e0
            word    ATN
            word    PEEK
            word    LEN
            word    STR
            word    VAL
            word    ASC
            word    CHRSTR
            word    LEFTSTR
            word    RIGHTSTR
            word    MIDSTR          ;$ea
; Math operator branch table
; 
; One-byte precedence code, followed by two-byte address - 1
; 
; P_OR   $46  "or" is lowest precedence
; P_AND  $50
; P_REL  $64  relational operators
; P_ADD  $79  binary + and -
; P_MUL  $7B  * and /
; P_PWR  $7D  exponentiation
; P_NEQ  $7F  unary - and comparison =
MATHTBL     byte    P_ADD
            word    FADDT-1         ;$C8 +
            byte    P_ADD
            word    FSUBT-1         ;$C9 -
            byte    P_MUL
            word    FMULTT-1        ;$CA *
            byte    P_MUL
            word    FDIVT-1         ;$CB /
            byte    P_PWR
            word    FPWRT-1         ;$CC ^
            byte    P_AND
            word    AND-1           ;$CD AND
            byte    P_OR
            word    OR-1            ;$CE OR
M_NEG       byte    P_NEQ
            word    NEGOP-1         ;$CF >
M_EQU       byte    P_NEQ
            word    EQUOP-1         ;$D0 =
M_REL       byte    P_REL
            word    RELOPS-1        ;$D1 <
;********************************************************************************
;* Token name table                                                             *
;********************************************************************************
TOKEN_NAME_TABLE
            byte   "EN","D"+$80           ;$80
            byte   "FO","R"+$80           ;$81
            byte   "NEX","T"+$80          ;$82
            byte   "DAT","A"+$80          ;$83
            byte   "INPU","T"+$80         ;$84
            byte   "DE","L"+$80           ;$85
            byte   "DI","M"+$80           ;$86
            byte   "REA","D"+$80          ;$87
            byte   "G","R"+$80            ;$88
            byte   "TEX","T"+$80          ;$89
            byte   "PR","#"+$80           ;$8a
            byte   "IN","#"+$80           ;$8b
            byte   "CAL","L"+$80          ;$8c
            byte   "PLO","T"+$80          ;$8d
            byte   "HLI","N"+$80          ;$8e
            byte   "VLI","N"+$80          ;$8f
            byte   "HGR","2"+$80          ;$90
            byte   "HG","R"+$80           ;$91
            byte   "HCOLOR","="+$80       ;$92
            byte   "HPLO","T"+$80         ;$93
            byte   "DRA","W"+$80          ;$94
            byte   "XDRA","W"+$80         ;$95
            byte   "HTA","B"+$80          ;$96
            byte   "HOM","E"+$80          ;$97
            byte   "ROT","="+$80          ;$98
            byte   "SCALE","="+$80        ;$99
            byte   "SHLOA","D"+$80        ;$9a
            byte   "TRAC","E"+$80         ;$9b
            byte   "NOTRAC","E"+$80       ;$9c
            byte   "NORMA","L"+$80        ;$9d
            byte   "INVERS","E"+$80       ;$9e
            byte   "FLAS","H"+$80         ;$9f
            byte   "COLOR","="+$80        ;$a0
            byte   "PO","P"+$80           ;$a1
            byte   "VTA","B"+$80          ;$a2
            byte   "HIMEM",":"+$80        ;$a3
            byte   "LOMEM",":"+$80        ;$a4
            byte   "ONER","R"+$80         ;$a5
            byte   "RESUM","E"+$80        ;$a6
            byte   "RECAL","L"+$80        ;$a7
            byte   "STOR","E"+$80         ;$a8
            byte   "SPEED","="+$80        ;$a9
            byte   "LE","T"+$80           ;$aa
            byte   "GOT","O"+$80          ;$ab
            byte   "RU","N"+$80           ;$ac
            byte   "I","F"+$80            ;$ad
            byte   "RESTOR","E"+$80       ;$ae
            byte   "&"+$80                ;$af
            byte   "GOSU","B"+$80         ;$b0
            byte   "RETUR","N"+$80        ;$b1
            byte   "RE","M"+$80           ;$b2
            byte   "STO","P"+$80          ;$b3
            byte   "O","N"+$80            ;$b4
            byte   "WAI","T"+$80          ;$b5
            byte   "LOA","D"+$80          ;$b6
            byte   "SAV","E"+$80          ;$b7
            byte   "DE","F"+$80           ;$b8
            byte   "POK","E"+$80          ;$b9
            byte   "PRIN","T"+$80         ;$ba
            byte   "CON","T"+$80          ;$bb
            byte   "LIS","T"+$80          ;$bc
            byte   "CLEA","R"+$80         ;$bd
            byte   "GE","T"+$80           ;$be
            byte   "NE","W"+$80           ;$bf
            byte   "TAB","("+$80          ;$c0
            byte   "T","O"+$80            ;$c1
            byte   "F","N"+$80            ;$c2
            byte   "SPC","("+$80          ;$c3
            byte   "THE","N"+$80          ;$c4
            byte   "A","T"+$80            ;$c5
            byte   "NO","T"+$80           ;$c6
            byte   "STE","P"+$80          ;$c7
            byte    "+"+$80             ;$c8
            byte    "-"+$80             ;$c9
            byte    "*"+$80             ;$ca
            byte    "/"+$80             ;$cb
            byte    "^"+$80             ;$cc
            byte   "AN","D"+$80           ;$cd
            byte   "O","R"+$80            ;$ce
            byte    ">"+$80             ;$cf
            byte    "="+$80             ;$d0
            byte    "<"+$80             ;$d1
            byte   "SG","N"+$80           ;$d2
            byte   "IN","T"+$80           ;$d3
            byte   "AB","S"+$80           ;$d4
            byte   "US","R"+$80           ;$d5
            byte   "FR","E"+$80           ;$d6
            byte   "SCRN","("+$80         ;$d7
            byte   "PD","L"+$80           ;$d8
            byte   "PO","S"+$80           ;$d9
            byte   "SQ","R"+$80           ;$da
            byte   "RN","D"+$80           ;$db
            byte   "LO","G"+$80           ;$dc
            byte   "EX","P"+$80           ;$dd
            byte   "CO","S"+$80           ;$de
            byte   "SI","N"+$80           ;$df
            byte   "TA","N"+$80           ;$e0
            byte   "AT","N"+$80           ;$e1
            byte   "PEE","K"+$80          ;$e2
            byte   "LE","N"+$80           ;$e3
            byte   "STR","$"+$80          ;$e4
            byte   "VA","L"+$80           ;$e5
            byte   "AS","C"+$80           ;$e6
            byte   "CHR","$"+$80          ;$e7
            byte   "LEFT","$"+$80         ;$e8
            byte   "RIGHT","$"+$80        ;$e9
            byte   "MID","$"+$80          ;$ea
            byte    $00             ;end of token name table
;********************************************************************************
;* Error messages                                                               *
;*                                                                              *
;* (The code uses error message constants that are defined by subtracting the   *
;* start of the table from the address of the error.  Currently no way to do    *
;* that in SourceGen, so the constants are project symbols instead.)            *
;********************************************************************************
ERROR_MSGS  byte   "NEXT WITHOUT FO","R"+$80
            byte   "SYNTA","X"+$80
            byte   "RETURN WITHOUT GOSU","B"+$80
            byte   "OUT OF DAT","A"+$80
            byte   "ILLEGAL QUANTIT","Y"+$80
            byte   "OVERFLO","W"+$80
            byte   "OUT OF MEMOR","Y"+$80
            byte   "UNDEF'D STATEMEN","T"+$80
            byte   "BAD SUBSCRIP","T"+$80
            byte   "REDIM'D ARRA","Y"+$80
            byte   "DIVISION BY ZER","O"+$80
            byte   "ILLEGAL DIREC","T"+$80
            byte   "TYPE MISMATC","H"+$80
            byte   "STRING TOO LON","G"+$80
            byte   "FORMULA TOO COMPLE","X"+$80
            byte   "CAN'T CONTINU","E"+$80
            byte   "UNDEF'D FUNCTIO","N"+$80
QT_ERROR    byte   " ERROR",$07,$00
QT_IN       byte   " IN ",$00
QT_BREAK    byte   $0d,"BREAK",$07,$00

; Called by NEXT and FOR to scan through the stack for a frame with the same
; variable.
; 
;   FORPNT = address of variable if FOR or NEXT
;          = $xxFF if called from RETURN
;            <<< BUG: should be $FFxx >>>
; 
;   returns .NE. if variable not found,
;           X = stack ptr after skipping all frames
; 
;           .EQ. if variable found
;           X = stack ptr of frame found
GTFORPNT    tsx
            inx
            inx
            inx
            inx
LD36A       lda     STACK+1,x       ;FOR frame here?
            cmp     #TOK_FOR
            bne     LD392           ;no
            lda     FORPNT+1        ;yes; NEXT with no variable?
            bne     LD37F           ;no, variable specified
            lda     STACK+2,x       ;yes, so use this frame
            sta     FORPNT
            lda     STACK+3,x
            sta     FORPNT+1
LD37F       cmp     STACK+3,x       ;is variable in this frame?
            bne     LD38B           ;no
            lda     FORPNT          ;look at 2nd byte too
            cmp     STACK+2,x       ;same variable?
            beq     LD392           ;yes
LD38B       txa                     ;no, so try next frame (if any)
            clc                     ;18 bytes per frame
            adc     #18
            tax
            bne     LD36A           ;...always?
LD392       rts

; Move block of memory up
; 
;   On entry:
;     (Y,A) = HIGHDS = destination end + 1
;     LOWTR = lowest address of source
;     HIGHTR = highest source address + 1
BLTU        jsr     REASON          ;be sure (Y,A) < FRETOP
            sta     STREND          ;new top of array storage
            sty     STREND+1
BLTU2       sec
            lda     HIGHTR          ;compute # of bytes to be moved
            sbc     LOWTR           ;  (from LOWTR through HIGHTR-1)
            sta     INDEX           ;partial page amount
            tay
            lda     HIGHTR+1
            sbc     LOWTR+1
            tax                     ;# of whole pages in X-reg
            inx
            tya                     ;# bytes in partial page
            beq     LD3CE           ;no partial page
            lda     HIGHTR          ;back up HIGHTR # bytes in partial page
            sec
            sbc     INDEX
            sta     HIGHTR
            bcs     LD3B7
            dec     HIGHTR+1
            sec
LD3B7       lda     HIGHDS          ;back up highds # bytes in partial page
            sbc     INDEX
            sta     HIGHDS
            bcs     LD3C7
            dec     HIGHDS+1
            bcc     LD3C7           ;...always

LD3C3       lda     (HIGHTR),y      ;move the bytes
            sta     (HIGHDS),y
LD3C7       dey
            bne     LD3C3           ;loop to end of this 256 bytes
            lda     (HIGHTR),y      ;move one more byte
            sta     (HIGHDS),y
LD3CE       dec     HIGHTR+1        ;down to next block of 256
            dec     HIGHDS+1
            dex                     ;another block of 256 to move?
            bne     LD3C7           ;yes
            rts                     ;no, finished

; Check if enough room left on stack for FOR, GOSUB, or expression evaluation.
CHKMEM      asl     A
            adc     #54
            bcs     MEMERR          ;...mem full err
            sta     INDEX
            tsx
            cpx     INDEX
            bcc     MEMERR          ;...mem full err
            rts

; Check if enough room between arrays and strings.
; 
;   (Y,A) = addr arrays need to grow to
REASON      cpy     FRETOP+1        ;high byte
            bcc     LD40F           ;plenty of room
            bne     LD3ED           ;not enough, try garbage collection
            cmp     FRETOP          ;low byte
            bcc     LD40F           ;enough room
; 
LD3ED       pha                     ;save (Y,A), TEMP1, and TEMP2
            ldx     #9              ;(should be #FAC-TEMP1-1)
            tya
LD3F1       pha
            lda     TEMP1,x
            dex
            bpl     LD3F1
            jsr     GARBAG          ;make as much room as possible
            ldx     #$f7            ;(should be #TEMP1-FAC+1) restore TEMP1 and TEMP2
LD3FC       pla                     ;  and (Y,A)
            sta     FAC,x
            inx
            bmi     LD3FC
            pla
            tay
            pla                     ;did we find enough room?
            cpy     FRETOP+1        ;high byte
            bcc     LD40F           ;yes, at least a page
            bne     MEMERR          ;no, mem full err
            cmp     FRETOP          ;low byte
            bcs     MEMERR          ;no, mem full err
LD40F       rts                     ;yes, return

MEMERR      ldx     #ERR_MEMFULL
;********************************************************************************
;* Handle an error                                                              *
;*                                                                              *
;*   X = offset in error message table                                          *
;*   ERRFLG > 128 if "on err" turned on                                         *
;*   CURLIN+1 = $ff if in direct mode                                           *
;*                                                                              *
;* Entry for SCRN( statement in func table points here.                         *
;********************************************************************************
ERROR       bit     ERRFLG          ;ON ERR turned on?
            bpl     LD419           ;no
            jmp     HANDLERR        ;yes

LD419       jsr     CRDO            ;print <return>
            jsr     OUTQUES         ;print "?"
LD41F       lda     ERROR_MSGS,x
            pha                     ;print message
            jsr     OUTDO
            inx
            pla
            bpl     LD41F
            jsr     STKINI          ;fix stack, et. al.
            lda     #<QT_ERROR      ;print " ERROR" and bell
            ldy     #>QT_ERROR
; Print string at (Y,A)
; Print current line # unless in direct mode
; Fall into warm restart
PRINT_ERROR_LINNUM
            jsr     STROUT          ;print string at (Y,A)
            ldy     CURLIN+1        ;running, or direct?
            iny
            beq     RESTART         ;was $ff, so direct mode
            jsr     INPRT           ;running, so print line number
;********************************************************************************
;* Warm restart entry                                                           *
;*                                                                              *
;* Come here from monitor by Ctrl+C, 0G, 3D0G, or E003G.                        *
;********************************************************************************
RESTART     jsr     CRDO            ;print <return>
            ldx     #"]"+$80        ;prompt character
            jsr     INLIN2          ;read a line
            stx     TXTPTR          ;set up CHRGET to scan the line
            sty     TXTPTR+1
            lsr     ERRFLG          ;clear flag
            jsr     CHRGET
            tax
            beq     RESTART         ;empty line
            ldx     #$ff            ;$ff in hi-byte of CURLIN means
            stx     CURLIN+1        ;  we are in direct mode
            bcc     NUMBERED_LINE   ;CHRGET saw digit, numbered line
            jsr     PARSE_INPUT_LINE ;no number, so parse it
            jmp     TRACE_          ;and try executing it

; Handle numbered line.
NUMBERED_LINE
            ldx     PRGEND          ;squash variable table
            stx     VARTAB
            ldx     PRGEND+1
            stx     VARTAB+1
            jsr     LINGET          ;get line #
            jsr     PARSE_INPUT_LINE ;and parse the input line
            sty     EOL_PNTR        ;save index to input buffer
            jsr     FNDLIN          ;is this line # already in program?
            bcc     PUT_NEW_LINE    ;no
            ldy     #$01            ;yes, so delete it
            lda     (LOWTR),y       ;LOWPTR points at line
            sta     INDEX+1         ;get high byte of forward ptr
            lda     VARTAB
            sta     INDEX
            lda     LOWTR+1
            sta     DEST+1
            lda     LOWTR
            dey
            sbc     (LOWTR),y
            clc
            adc     VARTAB
            sta     VARTAB
            sta     DEST
            lda     VARTAB+1
            adc     #$ff
            sta     VARTAB+1
            sbc     LOWTR+1
            tax
            sec
            lda     LOWTR
            sbc     VARTAB
            tay
            bcs     LD49F
            inx
            dec     DEST+1
LD49F       clc
            adc     INDEX
            bcc     LD4A7
            dec     INDEX+1
            clc
; 
LD4A7       lda     (INDEX),y       ;move higher lines of program
            sta     (DEST),y        ;down over the deleted line
            iny
            bne     LD4A7
            inc     INDEX+1
            inc     DEST+1
            dex
            bne     LD4A7
; 
PUT_NEW_LINE
            lda     INPUT_BUFFER    ;any characters after line #?
            beq     FIX_LINKS       ;no, so nothing to insert
            lda     MEMSIZE         ;yes, so make room and insert line
            ldy     MEMSIZE+1       ;wipe string area clean
            sta     FRETOP
            sty     FRETOP+1
            lda     VARTAB          ;set up BLTU subroutine
            sta     HIGHTR          ;insert new line
            adc     EOL_PNTR
            sta     HIGHDS
            ldy     VARTAB+1
            sty     HIGHTR+1
            bcc     LD4D1
            iny
LD4D1       sty     HIGHDS+1
            jsr     BLTU            ;make room for the line
            lda     LINNUM          ;put line number in line image
            ldy     LINNUM+1
            sta     INPUT_BUFFER-2
            sty     INPUT_BUFFER-1
            lda     STREND
            ldy     STREND+1
            sta     VARTAB
            sty     VARTAB+1
            ldy     EOL_PNTR
; Copy line into program.
LD4EA       lda     INPUT_BUFFER-5,y
            dey
            sta     (LOWTR),y
            bne     LD4EA
; Clear all variables.  Re-establish all forward links.
FIX_LINKS   jsr     SETPTRS         ;clear all variables
            lda     TEXTTAB         ;point index at start of program
            ldy     TEXTTAB+1
            sta     INDEX
            sty     INDEX+1
            clc
LD4FE       ldy     #$01            ;hi-byte of next forward ptr
            lda     (INDEX),y       ;end of program yet?
            bne     LD50F           ;no, keep going
            lda     VARTAB          ;yes
            sta     PRGEND
            lda     VARTAB+1
            sta     PRGEND+1
            jmp     RESTART

LD50F       ldy     #$04            ;find end of this line
LD511       iny                     ;(note maximum length < 256)
            lda     (INDEX),y
            bne     LD511
            iny                     ;compute address of next line
            tya
            adc     INDEX
            tax
            ldy     #$00            ;store forward ptr in this line
            sta     (INDEX),y
            lda     INDEX+1
            adc     #$00            ;A-reg != $ff, so this always clears carry
            iny
            sta     (INDEX),y
            stx     INDEX
            sta     INDEX+1
            bcc     LD4FE           ;...always

; Read a line, and strip off sign bits.
INLIN       ldx     #$80            ;null prompt
INLIN2      stx     MON_PROMPT
            jsr     MON_GETLN
            cpx     #239            ;maximum line length
            bcc     LD539
            ldx     #239            ;truncate at 239 chars
LD539       lda     #$00            ;mark end of line with $00 byte
            sta     INPUT_BUFFER,x
            txa
            beq     LD54C           ;null input line
LD541       lda     INPUT_BUFFER-1,x ;drop sign bits
            and     #$7f
            sta     INPUT_BUFFER-1,x
            dex
            bne     LD541
LD54C       lda     #$00            ;(Y,X) points at buffer - 1
            ldx     #<INPUT_BUFFER+255
            ldy     #(>INPUT_BUFFER)-1
            rts

INCHR       jsr     MON_RDKEY       ;*** ought to be BIT $C010 ***
            and     #$7f
            rts

; Tokenize the input line.
PARSE_INPUT_LINE
            ldx     TXTPTR          ;index into unparsed line
            dex                     ;prepare for INX at PARSE
            ldy     #$04            ;index to parsed output line
            sty     DATAFLG         ;clear sign-bit of DATAFLG
            bit     LOCK            ;is this program locked?
            bpl     PARSE           ;no, go ahead and parse the line
            pla                     ;yes, ignore input and RUN
            pla                     ;  the program
            jsr     SETPTRS         ;clear all variables
            jmp     NEWSTT          ;start running

PARSE       inx                     ;next input character
LD56D       lda     INPUT_BUFFER,x
            bit     DATAFLG         ;in a DATA statement?
            bvs     LD578           ;yes (DATAFLG = $49)
            cmp     #" "            ;ignore blanks
            beq     PARSE
LD578       sta     ENDCHR
            cmp     #'"'            ;start of quotation?
            beq     LD5F2
            bvs     LD5CD           ;branch if in DATA statement
            cmp     #"?"            ;shorthand for PRINT?
            bne     LD588           ;no
            lda     #TOK_PRINT      ;yes, replace with PRINT token
            bne     LD5CD           ;...always

LD588       cmp     #"0"            ;is it a digit, colon, or semi-colon?
            bcc     LD590           ;no, punctuation !"#$%&'()*+,-./
            cmp     #"<"            ;(should be  #';'+1 )
            bcc     LD5CD           ;yes, not a token
; Search token name table for match, starting with current char from input line.
LD590       sty     STRNG2          ;save index to output line
            lda     #<TOKEN_NAME_TABLE
            sta     FAC             ;make ptr for search
            lda     #(>TOKEN_NAME_TABLE)-1
            sta     FAC+1
            ldy     #$00            ;use Y-reg with FAC to address table
            sty     TKN_CNTR        ;holds current token - $80
            dey                     ;prepare for INY a few lines down
            stx     TXTPTR          ;save position in input line
            dex                     ;prepare for INX a few lines down
LD5A2       iny                     ;advance pointer to token table
            bne     LD5A7           ;Y=Y+1 is enough
            inc     FAC+1           ;also need to bump the page
LD5A7       inx                     ;advance pointer to input line
LD5A8       lda     INPUT_BUFFER,x  ;next char from input line
            cmp     #" "            ;this char a blank?
            beq     LD5A7           ;yes, ignore all blanks
            sec                     ;no, compare to char in table
            sbc     (FAC),y         ;same as next char of token name?
            beq     LD5A2           ;yes, continue matching
            cmp     #$80            ;maybe; was it same except for bit 7?
            bne     LD5F9           ;no, skip to next token
            ora     TKN_CNTR        ;yes, end of token; get token #
            cmp     #TOK_AT         ;did we match AT?
            bne     LD5CB           ;no, so no ambiguity
            lda     INPUT_BUFFER+1,x ;AT could be ATN or "A TO"
            cmp     #"N"            ;ATN has precedence over AT
            beq     LD5F9           ;it is ATN, find it the hard way
            cmp     #"O"            ;TO has precedence over AT
            beq     LD5F9           ;it is "A TO", find it the hard way
            lda     #TOK_AT         ;not ATN or "A TO", so use AT
; Store character or token in output line.
LD5CB       ldy     STRNG2          ;get index to output line in Y-reg
LD5CD       inx                     ;advance input index
            iny                     ;advance output index
            sta     INPUT_BUFFER-5,y ;store char or token
            lda     INPUT_BUFFER-5,y ;test for EOL or EOS
            beq     LD610           ;end of line
            sec
            sbc     #":"            ;end of statement?
            beq     LD5E0           ;yes, clear DATAFLG
            cmp     #TOK_DATA-58    ;(TOK_DATA - ':')  DATA token?
            bne     LD5E2           ;no, leave DATAFLG alone
LD5E0       sta     DATAFLG         ;DATAFLG = 0 or $83-$3a = $49
LD5E2       sec                     ;is it a REM token?
            sbc     #TOK_REM-58     ;(TOK_REM - ':')
            bne     LD56D           ;no, continue parsing line
            sta     ENDCHR          ;yes, clear literal flag
; Handle literal (between quotes) or remark, by copying chars up to ENDCHR.
LD5E9       lda     INPUT_BUFFER,x
            beq     LD5CD           ;end of line
            cmp     ENDCHR
            beq     LD5CD           ;found ENDCHR
LD5F2       iny                     ;next output char
            sta     INPUT_BUFFER-5,y
            inx                     ;next input char
            bne     LD5E9           ;...always
; Advance pointer to next token name.
LD5F9       ldx     TXTPTR          ;get pointer to input line in X-reg
            inc     TKN_CNTR        ;bump (token # - $80)
LD5FD       lda     (FAC),y         ;scan through table for BIT7 = 1
            iny                     ;next token one beyond that
            bne     LD604           ;...usually enough to bump Y-reg
            inc     FAC+1           ;next set of 256 token chars
LD604       asl     A               ;see if sign bit set on char
            bcc     LD5FD           ;no, more in this name
            lda     (FAC),y         ;yes, at next name; end of table?
            bne     LD5A8           ;no, not end of table
            lda     INPUT_BUFFER,x  ;yes, so not a keyword
            bpl     LD5CB           ;...always, copy char as is
; end of line
LD610       sta     INPUT_BUFFER-3,y ;store another 00 on end
            dec     TXTPTR+1        ;set TXTPTR = INPUT_BUFFER - 1
            lda     #<INPUT_BUFFER+255
            sta     TXTPTR
            rts

; Search for line
; 
;   LINNUM = line # to find
;   if not found: carry = 0
;                 LOWTR points at next line
;   if found:     carry = 1
;                 LOWTR points at line
FNDLIN      lda     TEXTTAB         ;search from beginning of program
            ldx     TEXTTAB+1
FL1         ldy     #$01            ;search from (X,A)
            sta     LOWTR
            stx     LOWTR+1
            lda     (LOWTR),y
            beq     LD647           ;end of program, and not found
            iny
            iny
            lda     LINNUM+1
            cmp     (LOWTR),y
            bcc     RTS_1           ;if not found
            beq     LD635
            dey
            bne     LD63E
LD635       lda     LINNUM
            dey
            cmp     (LOWTR),y
            bcc     RTS_1           ;past line, not found
            beq     RTS_1           ;if found
LD63E       dey
            lda     (LOWTR),y
            tax
            dey
            lda     (LOWTR),y
            bcs     FL1             ;always

LD647       clc                     ;return carry=0
RTS_1       rts

;********************************************************************************
;* NEW statement                                                                *
;********************************************************************************
NEW         bne     RTS_1           ;ignore if more to the statement
SCRTCH      lda     #$00
            sta     LOCK
            tay
            sta     (TEXTTAB),y
            iny
            sta     (TEXTTAB),y
            lda     TEXTTAB
            adc     #$02            ;carry wasn't cleared, so NEW usually
            sta     VARTAB          ;  adds 3, whereas FP adds 2
            sta     PRGEND
            lda     TEXTTAB+1
            adc     #$00
            sta     VARTAB+1
            sta     PRGEND+1
; 
SETPTRS     jsr     STXTPT          ;set TXTPTR to TXTTAB - 1
            lda     #$00            ;(this could have been byte $2C)
;********************************************************************************
;* CLEAR statement                                                              *
;********************************************************************************
CLEAR       bne     RTS_2           ;ignore if not at end of statement
CLEARC      lda     MEMSIZE         ;clear string area
            ldy     MEMSIZE+1
            sta     FRETOP
            sty     FRETOP+1
            lda     VARTAB          ;clear array area
            ldy     VARTAB+1
            sta     ARYTAB
            sty     ARYTAB+1
            sta     STREND          ;low end of free space
            sty     STREND+1
            jsr     RESTORE         ;set DATA pointer to beginning
; 
STKINI      ldx     #TEMPST
            stx     TEMPPT
            pla                     ;save return address
            tay
            pla
            ldx     #$f8            ;start stack at $f8
            txs                     ;  leaving room for parsing lines
            pha                     ;restore return address
            tya
            pha
            lda     #$00
            sta     OLDTEXT+1
            sta     SUBFLG
RTS_2       rts

; Set TXTPTR to beginning of program.
STXTPT      clc                     ;TXTPTR = TXTTAB - 1
            lda     TEXTTAB
            adc     #$ff
            sta     TXTPTR
            lda     TEXTTAB+1
            adc     #$ff
            sta     TXTPTR+1
            rts

;********************************************************************************
;* LIST statement                                                               *
;********************************************************************************
LIST        bcc     LD6B1           ;no line # specified
            beq     LD6B1           ;---ditto---
            cmp     #TOK_MINUS      ;if dash or comma, start at line 0
            beq     LD6B1           ;it is a dash
            cmp     #","            ;comma?
            bne     RTS_2           ;no, error
LD6B1       jsr     LINGET          ;convert line number if any
            jsr     FNDLIN          ;point LOWTR to 1st line
            jsr     CHRGOT          ;range specified?
            beq     LD6CC           ;no
            cmp     #TOK_MINUS
            beq     LD6C4
            cmp     #","
            bne     RTS_1
LD6C4       jsr     CHRGET          ;get next char
            jsr     LINGET          ;convert second line #
            bne     RTS_2           ;branch if syntax err
LD6CC       pla                     ;pop return address
            pla                     ;(get back by JMP NEWSTT
            lda     LINNUM          ;if no second number, use $FFFF
            ora     LINNUM+1
            bne     LIST_0          ;there was a second number
            lda     #$ff            ;max end range
            sta     LINNUM
            sta     LINNUM+1
LIST_0      ldy     #$01
            lda     (LOWTR),y       ;high byte of link
            beq     LIST_3          ;end of program
            jsr     ISCNTC          ;check if Ctrl+C has been typed
            jsr     CRDO            ;no, print <return>
            iny
            lda     (LOWTR),y       ;get line #, compare with end range
            tax
            iny
            lda     (LOWTR),y
            cmp     LINNUM+1
            bne     LD6F5
            cpx     LINNUM
            beq     LD6F7           ;on last line of range
LD6F5       bcs     LIST_3          ;fnished the range
LD6F7       sty     FORPNT
            jsr     LINPRT          ;print line # from (X,A)
            lda     #" "            ;print space after line #
LIST_1      ldy     FORPNT
            and     #$7f
LIST_2      jsr     OUTDO
            lda     MON_CH          ;if past column 33, start a new line
            cmp     #33
            bcc     LD712           ;< 33
            jsr     CRDO            ;print <return>
            lda     #5              ;and tab over 5
            sta     MON_CH
LD712       iny
            lda     (LOWTR),y
            bne     LIST_4          ;not end of line yet
            tay                     ;end of line
            lda     (LOWTR),y       ;get link to next line
            tax
            iny
            lda     (LOWTR),y
            stx     LOWTR           ;point to next line
            sta     LOWTR+1
            bne     LIST_0          ;branch if not end of program
LIST_3      lda     #$0d            ;print <return>
            jsr     OUTDO
            jmp     NEWSTT          ;to next statement

GETCHR      iny                     ;pick up char from table
            bne     LD731
            inc     FAC+1
LD731       lda     (FAC),y
            rts

LIST_4      bpl     LIST_2          ;branch if not a token
            sec
            sbc     #$7f            ;convert token to index
            tax
            sty     FORPNT          ;save line pointer
            ldy     #<TOKEN_NAME_TABLE
            sty     FAC             ;point FAC to table
            ldy     #(>TOKEN_NAME_TABLE)-1
            sty     FAC+1
            ldy     #$ff
LD746       dex                     ;skip keywords until reach this one
            beq     LD750
LD749       jsr     GETCHR          ;bump Y, get char from table
            bpl     LD749           ;not at end of keyword yet
            bmi     LD746           ;end of keyword, always branches

LD750       lda     #" "            ;found the right keyword
            jsr     OUTDO           ;print leading space
LD755       jsr     GETCHR          ;print the keyword
            bmi     LD75F           ;last char of keyword
            jsr     OUTDO
            bne     LD755           ;...always

LD75F       jsr     OUTDO           ;print last char of keyword
            lda     #" "            ;print trailing space
            bne     LIST_1          ;...always, back to actual line

;********************************************************************************
;* FOR statement                                                                *
;*                                                                              *
;* FOR pushes 18 bytes on the stack:                                            *
;*   2 - TXTPTR                                                                 *
;*   2 - line number                                                            *
;*   5 - initial (current) FOR variable value                                   *
;*   1 - step sign                                                              *
;*   5 - step value                                                             *
;*   2 - address of FOR variable in VARTAB                                      *
;*   1 - FOR token ($81)                                                        *
;********************************************************************************
FOR         lda     #$80
            sta     SUBFLG          ;subscripts not allowed
            jsr     LET             ;do <var> = <exp>, store addr in FORPNT
            jsr     GTFORPNT        ;is this FOR variable active?
            bne     LD777           ;no
            txa                     ;yes, cancel it and enclosed loops
            adc     #$0f            ;carry=1, this adds 16
            tax                     ;X was already S+2
            txs
LD777       pla                     ;pop return address too
            pla
            lda     #$09            ;be certain enough room in stack
            jsr     CHKMEM
            jsr     DATAN           ;scan ahead to next statement
            clc                     ;push statement address on stack
            tya
            adc     TXTPTR
            pha
            lda     TXTPTR+1
            adc     #$00
            pha
            lda     CURLIN+1        ;push line number on stack
            pha
            lda     CURLIN
            pha
            lda     #TOK_TO
            jsr     SYNCHR          ;require TO
            jsr     CHKNUM          ;<var> = <exp> must be numeric
            jsr     FRMNUM          ;get final value, must be numeric
            lda     FAC_SIGN        ;put sign into value in FAC
            ora     #$7f
            and     FAC+1
            sta     FAC+1
            lda     #<STEP          ;set up for return
            ldy     #>STEP          ;  to step
            sta     INDEX
            sty     INDEX+1
            jmp     FRM_STACK_3     ;returns by "JMP (INDEX)"

; STEP phrase of FOR statement.
STEP        lda     #<CON_ONE       ;STEP default=1
            ldy     #>CON_ONE
            jsr     LOAD_FAC_FROM_YA
            jsr     CHRGOT
            cmp     #TOK_STEP
            bne     LD7C3           ;use default value of 1.0
            jsr     CHRGET          ;step specified, get it
            jsr     FRMNUM
LD7C3       jsr     SIGN
            jsr     FRM_STACK_2
            lda     FORPNT+1
            pha
            lda     FORPNT
            pha
            lda     #TOK_FOR
            pha
; Perform NEXT statement.
NEWSTT      tsx                     ;remember the stack position
            stx     REMSTK
            jsr     ISCNTC          ;see if Ctrl+C has been typed
            lda     TXTPTR          ;no, keep executing
            ldy     TXTPTR+1
            ldx     CURLIN+1        ;=$FF if in direct mode
            inx                     ; $FF turns into $00
            beq     LD7E5           ; in direct mode
            sta     OLDTEXT         ;in running mode
            sty     OLDTEXT+1
LD7E5       ldy     #$00
            lda     (TXTPTR),y      ;end of line yet?
            bne     COLON           ;no
            ldy     #$02            ;yes, see if end of program
            lda     (TXTPTR),y
            clc
            beq     GOEND           ;yes, end of program
            iny
            lda     (TXTPTR),y      ;get line # of next line
            sta     CURLIN
            iny
            lda     (TXTPTR),y
            sta     CURLIN+1
            tya                     ;adjust TXTPTR to start
            adc     TXTPTR          ;of new line
            sta     TXTPTR
            bcc     TRACE_
            inc     TXTPTR+1
; 
TRACE_      bit     TRCFLG          ;is trace on?
            bpl     LD81D           ;no
            ldx     CURLIN+1        ;yes, are we running?
            inx
            beq     LD81D           ;not running, so don't trace
            lda     #"#"            ;print '#'
            jsr     OUTDO
            ldx     CURLIN
            lda     CURLIN+1
            jsr     LINPRT          ;print line number
            jsr     OUTSP           ;print trailing space
LD81D       jsr     CHRGET          ;get first chr of statement
            jsr     EXECUTE_STATEMENT ;and start processing
            jmp     NEWSTT          ;back for more

GOEND       beq     END4

; Execute a statement
; 
;   A-reg is first char of statement
;   Carry is set
EXECUTE_STATEMENT
            beq     RTS_3           ;end of line, null statement
EXECUTE_STATEMENT_1
            sbc     #$80            ;first char a token?
            bcc     LD83F           ;not token, must be LET
            cmp     #$40            ;statement-type token?
            bcs     SYNERR_1        ;no, syntax error
            asl     A               ;double to get index
            tay                     ;into address table
            lda     TOKEN_ADDR_TABLE+1,y
            pha                     ;put address on stack
            lda     TOKEN_ADDR_TABLE,y
            pha
            jmp     CHRGET          ;get next chr & rts to routine

LD83F       jmp     LET             ;must be <var> = <exp>

COLON       cmp     #":"
            beq     TRACE_
SYNERR_1    jmp     SYNERR

;********************************************************************************
;* RESTORE statement                                                            *
;********************************************************************************
RESTORE     sec                     ;set DATPTR to beginning of program
            lda     TEXTTAB
            sbc     #$01
            ldy     TEXTTAB+1
            bcs     SETDA
            dey
; Set DATPTR to (Y,A)
SETDA       sta     DATPTR
            sty     DATPTR+1
RTS_3       rts

; See if Ctrl+C typed
ISCNTC      lda     KBD
            cmp     #$83
            beq     LD860
            rts

LD860       jsr     INCHR           ;<<< should be BIT $C010 >>>
CTRL_C_TYPED
            ldx     #$ff            ;Ctrl+C attempted
            bit     ERRFLG          ;ON ERR enabled?
            bpl     LD86C           ;no
            jmp     HANDLERR        ;yes, return err code = 255

LD86C       cmp     #$03            ;since it is Ctrl+C, set Z and C bits
;********************************************************************************
;* STOP statement                                                               *
;********************************************************************************
STOP        bcs     END2            ;carry=1 to force printing "BREAK AT.."
;********************************************************************************
;* END statement                                                                *
;********************************************************************************
END         clc                     ;carry=0 to avoid printing message
END2        bne     RTS_4           ;if not end of statement, do nothing
            lda     TXTPTR
            ldy     TXTPTR+1
            ldx     CURLIN+1
            inx                     ;running?
            beq     LD888           ;no, direct mode
            sta     OLDTEXT
            sty     OLDTEXT+1
            lda     CURLIN
            ldy     CURLIN+1
            sta     OLDIN
            sty     OLDIN+1
LD888       pla
            pla
END4        lda     #<QT_BREAK      ;" BREAK" and bell
            ldy     #>QT_BREAK
            bcc     LD893
            jmp     PRINT_ERROR_LINNUM

LD893       jmp     RESTART

;********************************************************************************
;* CONT statement                                                               *
;********************************************************************************
CONT        bne     RTS_4           ;if not end of statement, do nothing
            ldx     #ERR_CANTCONT
            ldy     OLDTEXT+1       ;meaningful re-entry?
            bne     LD8A1           ;yes
            jmp     ERROR           ;no

LD8A1       lda     OLDTEXT         ;restore TXTPTR
            sta     TXTPTR
            sty     TXTPTR+1
            lda     OLDIN           ;restore line number
            ldy     OLDIN+1
            sta     CURLIN
            sty     CURLIN+1
RTS_4       rts

;********************************************************************************
;* SAVE statement                                                               *
;*                                                                              *
;* Writes program on cassette tape.                                             *
;********************************************************************************
SAVE        sec
            lda     PRGEND          ;compute program length
            sbc     TEXTTAB
            sta     LINNUM
            lda     PRGEND+1
            sbc     TEXTTAB+1
            sta     LINNUM+1
            jsr     VARTIO          ;set up to write 3-byte header
            jsr     MON_WRITE       ;write 'em
            jsr     PROGIO          ;set up to write the program
            jmp     MON_WRITE       ;write it

;********************************************************************************
;* LOAD statement                                                               *
;*                                                                              *
;* Reads a program from cassette tape.                                          *
;********************************************************************************
LOAD        jsr     VARTIO          ;set up to read 3-byte header
            jsr     MON_READ        ;read length, lock byte
            clc
            lda     TEXTTAB         ;compute end address
            adc     LINNUM
            sta     VARTAB
            lda     TEXTTAB+1
            adc     LINNUM+1
            sta     VARTAB+1
            lda     TEMPPT          ;lock byte
            sta     LOCK
            jsr     PROGIO          ;set up to read program
            jsr     MON_READ        ;read it
            bit     LOCK            ;if locked, start running now
            bpl     LD8ED           ;not locked
            jmp     SETPTRS         ;locked, start running

LD8ED       jmp     FIX_LINKS       ;just fix forward pointers

VARTIO      lda     #LINNUM         ;set up to read/write 3-byte header
            ldy     #$00
            sta     MON_A1L
            sty     MON_A1H
            lda     #TEMPPT
            sta     MON_A2L
            sty     MON_A2H
            sty     LOCK
            rts

PROGIO      lda     TEXTTAB         ;set up to read/write program
            ldy     TEXTTAB+1
            sta     MON_A1L
            sty     MON_A1H
            lda     VARTAB
            ldy     VARTAB+1
            sta     MON_A2L
            sty     MON_A2H
            rts

;********************************************************************************
;* RUN statement                                                                *
;********************************************************************************
RUN         php                     ;save status while subtracting
            dec     CURLIN+1        ;if was $FF (meaning direct mode), make it run mode
            plp                     ;get status again (from CHRGET)
            bne     LD91B           ;probably a line number
            jmp     SETPTRS         ;start at beginning of program

LD91B       jsr     CLEARC          ;clear variables
            jmp     GO_TO_LINE      ;join GOSUB statement

;********************************************************************************
;* GOSUB statement                                                              *
;*                                                                              *
;* Leaves 7 bytes on stack:                                                     *
;*   2 - return address (NEWSTT)                                                *
;*   2 - TXTPTR                                                                 *
;*   2 - line #                                                                 *
;*   1 - GOSUB token ($B0)                                                      *
;********************************************************************************
GOSUB       lda     #$03            ;be sure enough room on stack
            jsr     CHKMEM
            lda     TXTPTR+1
            pha
            lda     TXTPTR
            pha
            lda     CURLIN+1
            pha
            lda     CURLIN
            pha
            lda     #TOK_GOSUB
            pha
GO_TO_LINE  jsr     CHRGOT
            jsr     GOTO
            jmp     NEWSTT

;********************************************************************************
;* GOTO statement                                                               *
;*                                                                              *
;* Also used by RUN and GOSUB                                                   *
;********************************************************************************
GOTO        jsr     LINGET          ;get GOTO line
            jsr     REMN            ;point Y to EOL
            lda     CURLIN+1        ;is current page < GOTO page?
            cmp     LINNUM+1
            bcs     LD955           ;search from prog start if not
            tya                     ;otherwise search from next line
            sec
            adc     TXTPTR
            ldx     TXTPTR+1
            bcc     LD959
            inx
            bcs     LD959

LD955       lda     TEXTTAB         ;get program beginning
            ldx     TEXTTAB+1
LD959       jsr     FL1             ;search for GOTO line
            bcc     UNDERR          ;error if not there
            lda     LOWTR           ;TXTPTR = start of the destination line
            sbc     #$01
            sta     TXTPTR
            lda     LOWTR+1
            sbc     #$00
            sta     TXTPTR+1
RTS_5       rts                     ;return to NEWSTT or GOSUB

;********************************************************************************
;* POP and RETURN statements                                                    *
;********************************************************************************
POP         bne     RTS_5
            lda     #$ff
            sta     FORPNT          ;<<< BUG: should be FORPNT+1 >>>
; <<< see "All About Applesoft", pages 100,101 >>>
            jsr     GTFORPNT        ;to cancel FOR/NEXT in sub
            txs
            cmp     #TOK_GOSUB      ;last GOSUB found?
            beq     RETURN
            ldx     #ERR_NOGOSUB
            byte    $2C ; bit    $5aa2           ;fake: BIT xxxx skips ahead to JMP ERROR
UNDERR      ldx     #ERR_UNDEFSTAT
            jmp     ERROR

SYNERR_2    jmp     SYNERR

RETURN      pla                     ;discard GOSUB token
            pla
            cpy     #$42            ;(should be #TOK_POP*2 = $142)
            beq     PULL3           ;branch if a POP
            sta     CURLIN          ;pull line #
            pla
            sta     CURLIN+1
            pla
            sta     TXTPTR          ;pull TXTPTR
            pla
            sta     TXTPTR+1
;********************************************************************************
;* DATA statement                                                               *
;*                                                                              *
;* Executed by skipping to next colon or EOL                                    *
;********************************************************************************
DATA        jsr     DATAN           ;move to next statement
; add Y-reg to TXTPTR
ADDON       tya
            clc
            adc     TXTPTR
            sta     TXTPTR
            bcc     RTS_6
            inc     TXTPTR+1
RTS_6       rts

; Scan ahead to next ':' or EOL
DATAN       ldx     #":"            ;get offset in Y to EOL or ':'
            byte    $2C             ; bit     FAC_SIGN      ;fake
REMN        ldx     #$00            ;to EOL only
            stx     CHARAC
            ldy     #$00
            sty     ENDCHR
LD9AE       lda     ENDCHR          ;trick to count quote parity
            ldx     CHARAC
            sta     CHARAC
            stx     ENDCHR
LD9B6       lda     (TXTPTR),y
            beq     RTS_6           ;end of line
            cmp     ENDCHR
            beq     RTS_6           ;colon if looking for colons
            iny
            cmp     #'"'
            bne     LD9B6
            beq     LD9AE           ;...always

PULL3       pla
            pla
            pla
            rts

;********************************************************************************
;* IF statement                                                                 *
;********************************************************************************
IF          jsr     FRMEVL
            jsr     CHRGOT
            cmp     #TOK_GOTO
            beq     LD9D8
            lda     #TOK_THEN
            jsr     SYNCHR
LD9D8       lda     FAC             ;condition true or false?
            bne     IF_TRUE         ;branch if true
;********************************************************************************
;* REM statement                                                                *
;*                                                                              *
;* Or false IF statement                                                        *
;********************************************************************************
REM         jsr     REMN            ;skip read of line
            beq     ADDON           ;...always

IF_TRUE     jsr     CHRGOT          ;command or number?
            bcs     LD9E9           ;command
            jmp     GOTO            ;number

LD9E9       jmp     EXECUTE_STATEMENT

;********************************************************************************
;* ON statement                                                                 *
;*                                                                              *
;*   ON <exp> GOTO <list>                                                       *
;*   ON <exp> GOSUB <list>                                                      *
;********************************************************************************
ONGOTO      jsr     GETBYT          ;evaluate <exp>, as byte in FAC+4
            pha                     ;save next char on stack
            cmp     #TOK_GOSUB
            beq     ON_2
ON_1        cmp     #TOK_GOTO
            bne     SYNERR_2
ON_2        dec     FAC+4           ;counted to right one yet?
            bne     LDA00           ;no, keep looking
            pla                     ;yes, retrieve cmd
            jmp     EXECUTE_STATEMENT_1 ;and go

LDA00       jsr     CHRGET          ;prime convert subroutine
            jsr     LINGET          ;convert line #
            cmp     #","            ;terminate with comma?
            beq     ON_2            ;yes
            pla                     ;no, end of list, so ignore
RTS_7       rts

; Convert line number
LINGET      ldx     #$00            ;asc # to hex address
            stx     LINNUM          ;in LINNUM
            stx     LINNUM+1
LDA12       bcs     RTS_7           ;not a digit
            sbc     #"/"            ;(should be #'0'-1) convert digit to binary
            sta     CHARAC          ;save the digit
            lda     LINNUM+1        ;check range
            sta     INDEX
            cmp     #$19            ;(should be #>6400) line # too large?
            bcs     ON_1            ;yes, > 63999, go indirectly to "SYNTAX ERROR"
; <<< DANGEROUS CODE >>>
; 
; Note that if A-reg = $AB on the line above, ON_1 will compare = and cause a
; catastrophic jump to $22D9 (for GOTO), or other locations for other calls to
; LINGET.
; 
; You can see this if you first put BRK in $22D9, then type "GO TO 437761".
; 
; Any value from 437760 through 440319 will cause the problem.  ($AB00-ABFF)
; 
; <<< DANGEROUS CODE >>>
            lda     LINNUM          ;multiply by ten
            asl     A
            rol     INDEX
            asl     A
            rol     INDEX
            adc     LINNUM
            sta     LINNUM
            lda     INDEX
            adc     LINNUM+1
            sta     LINNUM+1
            asl     LINNUM
            rol     LINNUM+1
            lda     LINNUM
            adc     CHARAC          ;add digit
            sta     LINNUM
            bcc     LDA40
            inc     LINNUM+1
LDA40       jsr     CHRGET          ;get next char
            jmp     LDA12           ;more converting

;********************************************************************************
;* LET statement                                                                *
;*                                                                              *
;* LET <var> = <exp>                                                            *
;* <var> = <exp>                                                                *
;********************************************************************************
LET         jsr     PTRGET          ;get <var>
            sta     FORPNT
            sty     FORPNT+1
            lda     #TOK_EQUAL
            jsr     SYNCHR
            lda     VALTYP+1        ;save variable type
            pha
            lda     VALTYP
            pha
            jsr     FRMEVL          ;evalute <exp>
            pla
            rol     A
            jsr     CHKVAL
            bne     LET_STRING
            pla
; 
LET2        bpl     LDA77           ;real variable
            jsr     ROUND_FAC       ;integer var: round to 32 bits
            jsr     AYINT           ;truncate to 16 bits
            ldy     #$00
            lda     FAC+3
            sta     (FORPNT),y
            iny
            lda     FAC+4
            sta     (FORPNT),y
            rts

; Real variable = expression
LDA77       jmp     SETFOR

LET_STRING  pla
; Install string, descriptor address is at FAC+3,4
PUTSTR      ldy     #$02            ;string data already in string area?
            lda     (FAC+3),y       ;(string area is between FRETOP HIMEM)
            cmp     FRETOP+1
            bcc     LDA9A           ;yes, data already up there
            bne     LDA8C           ;no
            dey                     ;maybe, test low byte of pointer
            lda     (FAC+3),y
            cmp     FRETOP
            bcc     LDA9A           ;yes, already there
LDA8C       ldy     FAC+4           ;no; descriptor already among variables?
            cpy     VARTAB+1
            bcc     LDA9A           ;no
            bne     LDAA1           ;yes
            lda     FAC+3           ;maybe, compare low byte
            cmp     VARTAB
            bcs     LDAA1           ;yes, descriptor is among variables
LDA9A       lda     FAC+3           ;either string already on top, or
            ldy     FAC+4           ;descriptor is not a variable
            jmp     LDAB7           ;so just store the descriptor

; string not yet in string area, and descriptor is a variable
LDAA1       ldy     #$00            ;point at length in descriptor
            lda     (FAC+3),y       ;get length
            jsr     STRINI          ;make a string that long up above
            lda     DSCPTR          ;set up source ptr for MOVINS
            ldy     DSCPTR+1
            sta     STRNG1
            sty     STRNG1+1
            jsr     MOVINS          ;move string data to new area
            lda     #FAC            ;address of descriptor is in FAC
            ldy     #>FAC
LDAB7       sta     DSCPTR
            sty     DSCPTR+1
            jsr     FRETMS          ;discard descriptor if 'twas temporary
            ldy     #$00            ;copy string descriptor
            lda     (DSCPTR),y
            sta     (FORPNT),y
            iny
            lda     (DSCPTR),y
            sta     (FORPNT),y
            iny
            lda     (DSCPTR),y
            sta     (FORPNT),y
            rts

PR_STRING   jsr     STRPRT
            jsr     CHRGOT
;********************************************************************************
;* PRINT statement                                                              *
;********************************************************************************
PRINT       beq     CRDO            ;no more list, print <return>
PRINT2      beq     RTS_8           ;no more list, don't print <return>
            cmp     #TOK_TAB
            beq     PR_TAB_OR_SPC   ;C=1 for TAB(
            cmp     #TOK_SPC
            clc
            beq     PR_TAB_OR_SPC   ;C=0 for SPC(
            cmp     #","
            clc                     ;<<< no purpose to this >>>
            beq     PR_COMMA
            cmp     #";"
            beq     PR_NEXT_CHAR
            jsr     FRMEVL          ;evaluate expression
            bit     VALTYP          ;string or FP value?
            bmi     PR_STRING       ;string
            jsr     FOUT            ;FP: convert into buffer
            jsr     STRLIT          ;make buffer into string
            jmp     PR_STRING       ;print the string

CRDO        lda     #$0d            ;print <return>
            jsr     OUTDO
NEGATE      eor     #$ff            ;<<< why??? >>>
RTS_8       rts

; Tab to next comma column
; <<< note bug if width of window less than 33 >>>
PR_COMMA    lda     MON_CH
            cmp     #24             ;<<< bug: it should be 32 >>>
            bcc     LDB0E           ;next column, same line
            jsr     CRDO            ;first column, next line
            bne     PR_NEXT_CHAR    ;...always

LDB0E       adc     #16
            and     #$f0            ;round to 16 or 32
            sta     MON_CH
            bcc     PR_NEXT_CHAR    ;...always
; 
PR_TAB_OR_SPC
            php                     ;C=0 for SPC(, C=1 for TAB(
            jsr     GTBYTC          ;get value
            cmp     #")"            ;trailing parenthesis
            beq     LDB21           ;good
            jmp     SYNERR          ;no, syntax error

LDB21       plp                     ;TAB( or SPC(
            bcc     LDB2B           ;SPC(
            dex                     ;TAB(
            txa                     ;calculate spaces needed for TAB(
            sbc     MON_CH
            bcc     PR_NEXT_CHAR    ;already past that column
            tax                     ;now do a SPC( to the specified column
LDB2B       inx
NXSPC       dex
            bne     DOSPC           ;more spaces to print
; 
PR_NEXT_CHAR
            jsr     CHRGET
            jmp     PRINT2          ;continue parsing print list

DOSPC       jsr     OUTSP
            bne     NXSPC           ;...always

; Print string at (Y,A)
STROUT      jsr     STRLIT          ;make (Y,A) printable
; Print string at (FACMO,FACLO)
STRPRT      jsr     FREFAC          ;get address into INDEX, A-reg = length
            tax                     ;use X-reg for counter
            ldy     #$00            ;use Y-reg for scanner
            inx
LDB44       dex
            beq     RTS_8           ;finished
            lda     (INDEX),y       ;next char from string
            jsr     OUTDO           ;print the char
            iny
; <<< next three lines are useless >>>
            cmp     #$0d            ;was it <return>?
            bne     LDB44           ;no
            jsr     NEGATE          ;EOR #$FF would do it, but why?
            jmp     LDB44

OUTSP       lda     #" "            ;print a space
            byte    $2C             ;bit    $3fa9           ;skip over next line
OUTQUES     lda     #"?"            ;print question mark
; Print char from A-reg
; 
; Note: POKE 243,32 ($20 in $F3) will convert output to lower case.  This can be
; cancelled by NORMAL, INVERSE, or FLASH or POKE 243,0.
OUTDO       ora     #$80            ;print A-reg
            cmp     #$a0            ;control chr?
            bcc     LDB64           ;skip if so
            ora     FLASH_BIT       ;=$40 for FLASH, else $00
LDB64       jsr     MON_COUT        ;ANDs with $3F (INVERSE), $7F (FLASH)
            and     #$7f
            pha
            lda     SPEEDZ          ;complement of speed #
            jsr     MON_WAIT        ;so SPEED=255 becomes A=1
            pla
            rts

; Input conversion error: illegal character in numeric field.  Must distinguish
; between INPUT, READ, and GET
INPUTERR    lda     INPUTFLG
            beq     RESPERR         ;taken if INPUT
            bmi     READERR         ;taken if READ
            ldy     #$ff            ;from a GET
            bne     ERLIN           ;...always

READERR     lda     DATLIN          ;tell where the DATA is, rather
            ldy     DATLIN+1        ; than the READ
ERLIN       sta     CURLIN
            sty     CURLIN+1
            jmp     SYNERR

INPERR      pla
; 
RESPERR     bit     ERRFLG          ;ON ERR turned on?
            bpl     LDB90           ;no, give reentry a try
            ldx     #254            ;error code = 254
            jmp     HANDLERR

LDB90       lda     #<ERR_REENTRY   ;"?REENTER"
            ldy     #>ERR_REENTRY
            jsr     STROUT
            lda     OLDTEXT         ;re-execute the whole INPUT statement
            ldy     OLDTEXT+1
            sta     TXTPTR
            sty     TXTPTR+1
            rts

;********************************************************************************
;* GET statement                                                                *
;********************************************************************************
GET         jsr     ERRDIR          ;illegal if in direct mode
            ldx     #<INPUT_BUFFER+1 ;simulate input
            ldy     #>INPUT_BUFFER
            lda     #$00
            sta     INPUT_BUFFER+1
            lda     #$40            ;set up inputflg
            jsr     PROCESS_INPUT_LIST ;<<< can save 1 byte here >>>
            rts                     ;<<< by JMP PROCESS_INPUT_LIST >>>

;********************************************************************************
;* INPUT statement                                                              *
;********************************************************************************
INPUT       cmp     #'"'            ;check for optional prompt string
            bne     LDBC4           ;no, print "?" prompt
            jsr     STRTXT          ;make a printable string out of it
            lda     #";"            ;must have ';' now
            jsr     SYNCHR
            jsr     STRPRT          ;print the string
            jmp     LDBC7

LDBC4       jsr     OUTQUES         ;no string, print "?"
LDBC7       jsr     ERRDIR          ;illegal if in direct mode
            lda     #","            ;prime the buffer
            sta     INPUT_BUFFER-1
            jsr     INLIN
            lda     INPUT_BUFFER
            cmp     #$03            ;Ctrl+C?
            bne     INPUT_FLAG_ZERO ;no
            jmp     CTRL_C_TYPED

NXIN        jsr     OUTQUES         ;print "?"
            jmp     INLIN

;********************************************************************************
;* READ statement                                                               *
;********************************************************************************
READ        ldx     DATPTR          ;(Y,X) points at next DATA statement
            ldy     DATPTR+1
            lda     #$98            ;set INPUTFLG=$98
            byte    $2C             ;bit     $00a9         ;trick to PROCESS_INPUT_LIST
INPUT_FLAG_ZERO
            lda     #$00            ;set INPUTFLG = $00
; Process input list
; 
;   (Y,X) is address of input data string
;   A-reg = value for INPUTFLG: $00 for INPUT
;                               $40 for GET
;                               $98 for READ
PROCESS_INPUT_LIST
            sta     INPUTFLG
            stx     INPTR           ;address of input string
            sty     INPTR+1
PROCESS_INPUT_ITEM
            jsr     PTRGET          ;get address of variable
            sta     FORPNT
            sty     FORPNT+1
            lda     TXTPTR
            ldy     TXTPTR+1        ;save current TXTPTR
            sta     TXPSV           ;which points into program
            sty     TXPSV+1
            ldx     INPTR           ;set TXTPTR to point at input buffer
            ldy     INPTR+1         ;or DATA line
            stx     TXTPTR
            sty     TXTPTR+1
            jsr     CHRGOT          ;get char at ptr
            bne     INSTART         ;not end of line or colon
            bit     INPUTFLG        ;doing a GET?
            bvc     LDC1F           ;no
            jsr     MON_RDKEY       ;yes, get char
            and     #$7f
            sta     INPUT_BUFFER
            ldx     #<INPUT_BUFFER+255
            ldy     #(>INPUT_BUFFER)-1
            bne     LDC27           ;...always

LDC1F       bmi     FINDATA         ;doing a READ
            jsr     OUTQUES         ;doing an INPUT, print "?"
            jsr     NXIN            ;print another "?", and input a line
LDC27       stx     TXTPTR
            sty     TXTPTR+1
INSTART     jsr     CHRGET          ;get next input char
            bit     VALTYP          ;string or numeric?
            bpl     LDC63           ;numeric
            bit     INPUTFLG        ;string -- now what input type?
            bvc     LDC3F           ;not a GET
            inx                     ;GET
            stx     TXTPTR
            lda     #$00
            sta     CHARAC          ;no other terminators than $00
            beq     LDC4B           ;...always

LDC3F       sta     CHARAC
            cmp     #'"'            ;terminate on $00 or quote
            beq     LDC4C
            lda     #":"            ;terminate on $00, colon, or comma
            sta     CHARAC
            lda     #","
LDC4B       clc
LDC4C       sta     ENDCHR
            lda     TXTPTR
            ldy     TXTPTR+1
            adc     #$00            ;skip over quotation mark, if
            bcc     LDC57           ;there was one
            iny
LDC57       jsr     STRLT2          ;build string starting at (Y,A), term by $00, CHARAC, or ENDCHR
            jsr     POINT           ;set TXTPTR to point at string
            jsr     PUTSTR          ;store string in variable
            jmp     INPUT_MORE

LDC63       pha
            lda     INPUT_BUFFER    ;anything in buffer?
            beq     INPFIN          ;no, see if READ or INPUT
INPUT_DATA  pla                     ;READ
            jsr     FIN             ;get fp number at TXTPTR
            lda     VALTYP+1
            jsr     LET2            ;store result in variable
INPUT_MORE  jsr     CHRGOT
            beq     LDC7E           ;end of line or colon
            cmp     #","            ;comma in input?
            beq     LDC7E           ;yes
            jmp     INPUTERR        ;nothing else will do

LDC7E       lda     TXTPTR          ;save position in input buffer
            ldy     TXTPTR+1
            sta     INPTR
            sty     INPTR+1
            lda     TXPSV           ;restore program pointer
            ldy     TXPSV+1
            sta     TXTPTR
            sty     TXTPTR+1
            jsr     CHRGOT          ;next char from program
            beq     INPDONE         ;end of statement
            jsr     CHKCOM          ;better be a comma then
            jmp     PROCESS_INPUT_ITEM

INPFIN      lda     INPUTFLG        ;INPUT or READ
            bne     INPUT_DATA      ;READ
            jmp     INPERR

FINDATA     jsr     DATAN           ;get offset to next colon or EOL
            iny                     ;to first char of next line
            tax                     ;which: EOL or colon?
            bne     LDCB9           ;colon
            ldx     #ERR_NODATA     ;EOL: might be out of data
            iny                     ;check hi-byte of forward ptr
            lda     (TXTPTR),y      ;end of program?
            beq     GERR            ;yes, we are out of data
            iny                     ;pick up the line #
            lda     (TXTPTR),y
            sta     DATLIN
            iny
            lda     (TXTPTR),y
            iny                     ;point at first text char in line
            sta     DATLIN+1
LDCB9       lda     (TXTPTR),y      ;get 1st token of statement
            tax                     ;save token in X-reg
            jsr     ADDON           ;add Y-reg to TXTPTR
            cpx     #TOK_DATA       ;did we find a DATA statement?
            bne     FINDATA         ;not yet
            jmp     INSTART         ;yes, read it

INPDONE     lda     INPTR           ;get pointer in case it was READ
            ldy     INPTR+1
            ldx     INPUTFLG        ;READ or INPUT?
            bpl     LDCD1           ;INPUT
            jmp     SETDA           ;DATA, so store (Y,X) at DATPTR

LDCD1       ldy     #$00            ;INPUT: any more chars on line?
            lda     (INPTR),y
            beq     LDCDE           ;no, all is well
            lda     #<ERR_EXTRA     ;yes, error
            ldy     #>ERR_EXTRA     ;"EXTRA IGNORED"
            jmp     STROUT

LDCDE       rts

ERR_EXTRA   byte   "?EXTRA IGNORED",$0d,$00
ERR_REENTRY byte   "?REENTER",$0d,$00

;********************************************************************************
;* NEXT statement                                                               *
;********************************************************************************
NEXT        bne     NEXT_1          ;variable after NEXT
            ldy     #$00            ;flag by setting FORPNT+1 = 0
            beq     NEXT_2          ;...always

NEXT_1      jsr     PTRGET          ;get ptr to variable in (Y,A)
NEXT_2      sta     FORPNT
            sty     FORPNT+1
            jsr     GTFORPNT        ;find FOR-frame for this variable
            beq     NEXT_3          ;found it
            ldx     #ERR_NOFOR      ;not there, abort
GERR        beq     JERROR          ;...always

NEXT_3      txs
            inx                     ;set stack ptr to point to this frame,
            inx                     ; which trims off any inner loops
            inx
            inx
            txa                     ;low byte of adrs of step value
            inx
            inx
            inx
            inx
            inx
            inx
            stx     DEST            ;low byte adrs of FOR var value
            ldy     #>STACK         ;(Y,A) is address of step value
            jsr     LOAD_FAC_FROM_YA ;step to FAC
            tsx
            lda     STACK+9,x
            sta     FAC_SIGN
            lda     FORPNT
            ldy     FORPNT+1
            jsr     FADD            ;add to FOR value
            jsr     SETFOR          ;put new value back
            ldy     #>STACK         ;(Y,A) is address of end value
            jsr     FCOMP2          ;compare to end value
            tsx
            sec
            sbc     STACK+9,x       ;sign of step
            beq     LDD55           ;branch if FOR complete
            lda     STACK+15,x      ;otherwise set up
            sta     CURLIN          ;FOR line #
            lda     STACK+16,x
            sta     CURLIN+1
            lda     STACK+18,x      ;and set TXTPTR to just
            sta     TXTPTR          ; after FOR statement
            lda     STACK+17,x
            sta     TXTPTR+1
LDD52       jmp     NEWSTT

LDD55       txa                     ;pop off FOR-frame, loop is done
            adc     #17             ;carry is set, so adds 18
            tax
            txs
            jsr     CHRGOT          ;char after variable
            cmp     #","            ;another variable in NEXT?
            bne     LDD52           ;no, go to next statement
            jsr     CHRGET          ;yes, prime for next variable
            jsr     NEXT_1          ;(does not return)
; Evaluate expression, make sure it is numeric
FRMNUM      jsr     FRMEVL
; Make sure FAC is numeric
CHKNUM      clc
            byte    $24             ; bit    MON_KSWL        ;dummy for skip
; Make sure FAC is string
CHKSTR      sec
; Make sure FAC is correct type.
; 
;   if C=0, type must be numeric
;   if C=1, type must be string
CHKVAL      bit     VALTYP          ;$00 if numeric, $FF if string
            bmi     LDD74           ;type is string
            bcs     LDD76           ;not string, but we need string
LDD73       rts                     ;type is correct

LDD74       bcs     LDD73           ;is string and we wanted string
LDD76       ldx     #ERR_BADTYPE    ;type mismatch
JERROR      jmp     ERROR

; Evaluate the expression at TXTPTR, leaving the result in FAC.  Works for both
; string and numeric expressions.
FRMEVL      ldx     TXTPTR          ;decrement TXTPTR
            bne     LDD81
            dec     TXTPTR+1
LDD81       dec     TXTPTR
            ldx     #$00            ;start with precedence = 0
            byte    $24             ;bit    $48             ;track to skip following PHA
; 
FRMEVL_1    pha                     ;push relops flags
            txa
            pha                     ;save last precedence
            lda     #$01
            jsr     CHKMEM          ;check if enough room on stack
            jsr     FRM_ELEMENT     ;get an element
            lda     #$00
            sta     CPRTYP          ;clear comparison operator flags
; 
FRMEVL_2    jsr     CHRGOT          ;check for relational operators
LDD98       sec                     ;> is $CF, = is $D0, < is $D1
            sbc     #TOK_GREATER    ;> is 0, = is 1, < is 2
            bcc     LDDB4           ;not relational operator
            cmp     #3
            bcs     LDDB4           ;not relational operator
            cmp     #1              ;set carry if "=" or "<"
            rol     A               ;now > is 0, = is 3, < is 5
            eor     #$01            ;now > is 1, = is 2, < is 4
            eor     CPRTYP          ;set bits of CPRTYP:  00000<=>
            cmp     CPRTYP          ;check for illegal combinations
            bcc     SNTXERR         ;if less than, a relop was repeated
            sta     CPRTYP
            jsr     CHRGET          ;another operator?
            jmp     LDD98           ;check for <,=,> again

LDDB4       ldx     CPRTYP          ;did we find a relational operator?
            bne     FRM_RELATIONAL  ;yes
            bcs     NOTMATH         ;no, and next token is > $D1
            adc     #TOK_PLUS-193   ;(should be #$CF-TOK_PLUS) no, and next token < $CF
            bcc     NOTMATH         ;if next token < "+"
            adc     VALTYP          ;+ and last result a string?
            bne     LDDC5           ;branch if not
            jmp     CAT             ;concatenate if so

LDDC5       adc     #$ff            ;+-*/ is 0123
            sta     INDEX
            asl     A               ;multiply by 3
            adc     INDEX           ;+-*/ is 0,3,6,9
            tay
; 
; Clear variables
LASTOP      =    $87    ;Overlaps with TXPSV
SGNCPR      =    $ab    ;Overlaps with STRNG1

FRM_PRECEDENCE_TEST
            pla                     ;get last precedence
            cmp     MATHTBL,y
            bcs     FRM_PERFORM_1   ;do now if higher precedence
            jsr     CHKNUM          ;was last result a #?
NXOP        pha                     ;yes, save precedence on stack
SAVOP       jsr     FRM_RECURSE     ;save rest, call FRMEVL recursively
            pla
            ldy     LASTOP
            bpl     PREFNC
            tax
            beq     GOEX            ;exit if no math in expression
            bne     FRM_PERFORM_2   ;...always

; Found one or more relational operators <,=,>
FRM_RELATIONAL
            lsr     VALTYP          ;VALTYP = 0 (numeric), = $FF (string)
            txa                     ;set CPRTYP to 0000<=>C
            rol     A               ;where C=0 if #, C=1 if string
            ldx     TXTPTR          ;back up TXTPTR
            bne     LDDEE
            dec     TXTPTR+1
LDDEE       dec     TXTPTR
            ldy     #<M_REL-178     ;(should be M_REL - MATHTBL)  point at relops entry
            sta     CPRTYP
            bne     FRM_PRECEDENCE_TEST ;...always

PREFNC      cmp     MATHTBL,y
            bcs     FRM_PERFORM_2   ;do now if higher precedence
            bcc     NXOP            ;...always

; Stack this operation and call FRMEVL for another one
FRM_RECURSE lda     MATHTBL+2,y
            pha                     ;push address of operation performer
            lda     MATHTBL+1,y
            pha
            jsr     FRM_STACK_1     ;stack FAC_SIGN and FAC
            lda     CPRTYP          ;A=relop flags, X=precedence byte
            jmp     FRMEVL_1        ;recursively call FRMEVL

SNTXERR     jmp     SYNERR

; Stack (FAC)
; 
; Three entry points:
;   _1, from FRMEVL
;   _2, from STEP
;   _3, from FOR
FRM_STACK_1 lda     FAC_SIGN        ;get FAC_SIGN and push it
            ldx     MATHTBL,y       ;precedence byte from MATHTBL
; Enter here from STEP, to push step sign and value
FRM_STACK_2 tay                     ;FAC_SIGN or SGN(step value)
            pla                     ;pull return address and add 1
            sta     INDEX           ;<<< assumes not on page boundary! >>>
            inc     INDEX           ;place bumped return address in
            pla                     ; INDEX,INDEX+1
            sta     INDEX+1
            tya                     ;FAC_SIGN or SGN(step value)
            pha                     ;push FAC_SIGN or SGN(step value)
; Enter here from FOR, with INDEX = step, to push initial value of FOR variable
FRM_STACK_3 jsr     ROUND_FAC       ;round to 32 bits
            lda     FAC+4           ;push FAC
            pha
            lda     FAC+3
            pha
            lda     FAC+2
            pha
            lda     FAC+1
            pha
            lda     FAC
            pha
            jmp     (INDEX)         ;do RTS funny way

NOTMATH     ldy     #$ff            ;set up to exit routine
            pla
GOEX        beq     EXIT            ;exit if no math to do
; Perform stacked operation.
; 
;   A-reg = precedence byte
;   Stack: 1 - CPRMASK
;          5 - ARG
;          2 - addr of performer
FRM_PERFORM_1
            cmp     #P_REL          ;was it relational operator?
            beq     LDE41           ;yes, allow string compare
            jsr     CHKNUM          ;must be numeric value
LDE41       sty     LASTOP
; 
FRM_PERFORM_2
            pla                     ;get 0000<=>C from stack
            lsr     A               ;shift to 00000<=> form
            sta     CPRMASK         ;00000<=>
            pla
            sta     ARG             ;get floating point value off stack,
            pla                     ; and put it in ARG
            sta     ARG+1
            pla
            sta     ARG+2
            pla
            sta     ARG+3
            pla
            sta     ARG+4
            pla
            sta     ARG+5
            eor     FAC_SIGN        ;save EOR of signs of the operands,
            sta     SGNCPR          ; in case of multiply or divide
EXIT        lda     FAC             ;FAC exponent in A-reg
            rts                     ;status .EQ. if FAC=0; RTS goes to perform operation

; Get element in expression
; 
; Get value of variable or number at TXTPNT, or point to string descriptor if a
; string, and put in FAC.
FRM_ELEMENT lda     #$00            ;assume numeric
            sta     VALTYP
LDE64       jsr     CHRGET
            bcs     LDE6C           ;not a digit
LDE69       jmp     FIN             ;numeric constant

LDE6C       jsr     ISLETC          ;variable name?
            bcs     FRM_VARIABLE    ;yes
            cmp     #"."            ;decimal point
            beq     LDE69           ;yes, numeric constant
            cmp     #TOK_MINUS      ;unary minus?
            beq     MIN             ;yes
            cmp     #TOK_PLUS       ;unary plus
            beq     LDE64           ;yes
            cmp     #'"'            ;string constant?
            bne     NOT_            ;no
; String constant element
; 
;   Set (Y,A) = TXTPTR + carry
STRTXT      lda     TXTPTR          ;add carry to get address of 1st char
            ldy     TXTPTR+1
            adc     #$00
            bcc     LDE8A
            iny
LDE8A       jsr     STRLIT          ;build descriptor to string; get address of descriptor in FAC
            jmp     POINT           ;point TXTPTR after trailing quote

; NOT function
; 
;   if FAC=0, return FAC=1
;   if FAC<>0, return FAC=0
NOT_        cmp     #TOK_NOT
            bne     FN_             ;not NOT, try FN
            ldy     #<M_EQU-178     ;(should be M_EQU - MATHTBL)  point at = comparison
            bne     EQUL            ;...always

; Comparison for equality (= operator).  Also used to evaluate NOT function.
EQUOP       lda     FAC             ;set TRUE if FAC = zero
            bne     LDE9F           ;false
            ldy     #$01            ;true
            byte    $2C             ;bit     $00a0         ;trick to skip next 2 bytes
LDE9F       ldy     #$00            ;false
            jmp     SNGFLT

FN_         cmp     #TOK_FN
            bne     SGN_
            jmp     FUNCT

SGN_        cmp     #TOK_SGN
            bcc     PARCHK
            jmp     UNARY

; Evaluate "(expression)"
PARCHK      jsr     CHKOPN          ;is there a '(' at TXTPTR?
            jsr     FRMEVL          ;yes, evaluate expression
CHKCLS      lda     #")"            ;check for ')'
            byte    $2C             ;bit    $28a9           ;trick
CHKOPN      lda     #"("
            byte    $2C             ;bit    $2ca9           ;trick
CHKCOM      lda     #","            ;comma at TXTPTR?
; Unless char at TXTPTR = A-reg, syntax error
SYNCHR      ldy     #$00
            cmp     (TXTPTR),y
            bne     SYNERR
            jmp     CHRGET          ;match, get next char & return

SYNERR      ldx     #ERR_SYNTAX
            jmp     ERROR

MIN         ldy     #<M_NEG-178     ;(should be M_NEG - MATHTBL)  point at unary minus
EQUL        pla
            pla
            jmp     SAVOP

VPNT        =    $a0    ;Overlaps with FAC+3

FRM_VARIABLE
            jsr     PTRGET          ;so PTRGET can tell we called
            sta     VPNT            ;address of variable
            sty     VPNT+1
            ldx     VALTYP          ;numeric or string?
            beq     LDEE5           ;numeric
            ldx     #$00            ;string
            stx     STRNG1+1
            rts

LDEE5       ldx     VALTYP+1        ;numeric, which type?
            bpl     LDEF6           ;floating point
            ldy     #$00            ;integer
            lda     (VPNT),y
            tax                     ;get value in (A,Y)
            iny
            lda     (VPNT),y
            tay
            txa
            jmp     GIVAYF          ;convert (A,Y) to floating point

LDEF6       jmp     LOAD_FAC_FROM_YA

SCREEN      jsr     CHRGET
            jsr     PLOTFNS         ;get column and row
            txa                     ;row
            ldy     FIRST           ;column
            jsr     MON_SCRN        ;get 4-bit color there
            tay
            jsr     SNGFLT          ;convert Y-reg to real in FAC
            jmp     CHKCLS          ;require ")"

UNARY       cmp     #TOK_SCRN       ;not unary, do special
            beq     SCREEN
            asl     A               ;double token to get index
            pha
            tax
            jsr     CHRGET
            cpx     #$cf            ;(should be TOK_LEFT*2-1)  LEFT$, RIGHT$, and MID$
            bcc     LDF3A           ;not one of the string functions
            jsr     CHKOPN          ;string function, need "("
            jsr     FRMEVL          ;evaluate expression for string
            jsr     CHKCOM          ;require a comma
            jsr     CHKSTR          ;make sure expression is a string
            pla
            tax                     ;retrieve routine pointer
            lda     VPNT+1          ;stack address of string
            pha
            lda     VPNT
            pha
            txa
            pha                     ;stack doubled token
            jsr     GETBYT          ;convert next expression to byte in X-reg
            pla                     ;get doubled token off stack
            tay                     ;use as index to branch
            txa                     ;value of second parameter
            pha                     ;push 2nd param
            jmp     LDF3F           ;join unary functions

LDF3A       jsr     PARCHK          ;require "(expression)"
            pla
            tay                     ;index into function address table
LDF3F       lda     $cfdc,y         ;(should be UNFNC - TOK_SGN - TOK_SGN + $100)
            sta     JMPADRS+1
            lda     $cfdd,y         ;(should be UNFNC - TOK_SGN - TOK_SGN + $101)
            sta     JMPADRS+2
            jsr     JMPADRS         ;does not return for CHR$, LEFT$, RIGHT$, or MID$
            jmp     CHKNUM          ;require numeric result

OR          lda     ARG             ;OR operator
            ora     FAC             ;if result nonzero, it is true
            bne     TRUE
AND         lda     ARG             ;AND operator
            beq     FALSE           ;if either is zero, result is false
            lda     FAC
            bne     TRUE
FALSE       ldy     #$00            ;return FAC=0
            byte    $2C             ;bit    $01a0           ;trick
TRUE        ldy     #$01            ;return FAC=1
            jmp     SNGFLT

; Perform relational operations
RELOPS      jsr     CHKVAL          ;make sure FAC is correct type
            bcs     STRCMP          ;type matches, branch if strings
            lda     ARG_SIGN        ;numeric comparison
            ora     #$7f            ;re-pack value in ARG for FCOMP
            and     ARG+1
            sta     ARG+1
            lda     #ARG
            ldy     #>ARG
            jsr     FCOMP           ;return A-reg = -1,0,1
            tax                     ; as ARG <,=,> FAC
            jmp     NUMCMP

; String comparison
STRCMP      lda     #$00            ;set result type to numeric
            sta     VALTYP
            dec     CPRTYP          ;make CPRTYP 0000<=>0
            jsr     FREFAC
            sta     FAC             ;string length
            stx     FAC+1
            sty     FAC+2
            lda     ARG+3
            ldy     ARG+4
            jsr     FRETMP
            stx     ARG+3
            sty     ARG+4
            tax                     ;len ARG string
            sec
            sbc     FAC             ;set X-reg to smaller len
            beq     LDFA5
            lda     #$01
            bcc     LDFA5
            ldx     FAC
            lda     #$ff
LDFA5       sta     FAC_SIGN        ;flag which shorter
            ldy     #$ff
            inx
STRCMP_1    iny
            dex
            bne     STRCMP_2        ;more chars in both strings
            ldx     FAC_SIGN        ;if = so far, decide by length
; 
NUMCMP      bmi     CMPDONE
            clc
            bcc     CMPDONE         ;...always

STRCMP_2    lda     (ARG+3),y
            cmp     (FAC+1),y
            beq     STRCMP_1        ;same, keep comparing
            ldx     #$ff            ;in case ARG greater
            bcs     CMPDONE         ;it is
            ldx     #$01            ;FAC greater
; 
CMPDONE     inx                     ;convert FF,0,1 to 0,1,2
            txa
            rol     A               ;and to 0,2,4 if C=0, else 1,2,5
            and     CPRMASK         ;00000<=>
            beq     LDFCA           ;if no match: false
            lda     #$01            ;at least one match: true
LDFCA       jmp     FLOAT

;********************************************************************************
;* PDL statement                                                                *
;*                                                                              *
;* <<< note: arg < 4 is not checked >>                                          *
;********************************************************************************
PDL         jsr     CONINT          ;get # in X-reg
            jsr     MON_PREAD       ;read paddle
            jmp     SNGFLT          ;float result

;********************************************************************************
;* DIM statement                                                                *
;********************************************************************************
NXDIM       jsr     CHKCOM          ;separated by commas
DIM         tax                     ;non-zero, flags PTRGET DIM called
            jsr     PTRGET2         ;allocate the array
            jsr     CHRGOT          ;next char
            bne     NXDIM           ;not end of statement
            rts

; PTRGET - general variable scan
; 
; Scans variable name at TXTPTR, and searches the VARTAB and ARYTAB for the
; name.  If not found, create variable of appropriate type.  Return with address
; in VARPNT and (Y,A).
; 
; Actual activity controlled somewhat by two flags:
; 
;   DIMFLG - nonzero if called from DIM
;            else = 0
;   SUBFLG - = $00
;            = $40 if called from GETARYPT
;            = $80 if called from DEF FN
;            = $C1-DA if called from FN
PTRGET      ldx     #$00
            jsr     CHRGOT          ;get first char of variable name
PTRGET2     stx     DIMFLG          ;x is nonzero if from DIM
PTRGET3     sta     VARNAM
            jsr     CHRGOT
            jsr     ISLETC          ;is it a letter?
            bcs     NAMOK           ;yes, okay so far
BADNAM      jmp     SYNERR          ;no, syntax error

NAMOK       ldx     #$00
            stx     VALTYP
            stx     VALTYP+1
            jmp     PTRGET4         ;to branch across $e000 vectors

;********************************************************************************
;* DOS and monitor call BASIC at $E000 and $E003                                *
;********************************************************************************
            jmp     COLD_START

            jmp     RESTART

            byte    $00             ;wasted byte

PTRGET4     jsr     CHRGET          ;second char of variable name
            bcc     LE011           ;numeric
            jsr     ISLETC          ;letter?
            bcc     LE01C           ;no, end of name
LE011       tax                     ;save second char of name in X-reg
LE012       jsr     CHRGET          ;scan to end of variable name
            bcc     LE012           ;numeric
            jsr     ISLETC
            bcs     LE012           ;alpha
LE01C       cmp     #"$"            ;string?
            bne     LE026           ;no
            lda     #$ff
            sta     VALTYP
            bne     LE036           ;...always

LE026       cmp     #"%"            ;integer?
            bne     LE03D           ;no
            lda     SUBFLG          ;yes; integer variable allowed?
            bmi     BADNAM          ;no, syntax error
            lda     #$80            ;yes
            sta     VALTYP+1        ;flag integer mode
            ora     VARNAM
            sta     VARNAM          ;set sign bit on varname
LE036       txa                     ;second char of name
            ora     #$80            ;set sign
            tax
            jsr     CHRGET          ;get terminating char
LE03D       stx     VARNAM+1        ;store second char of name
            sec
            ora     SUBFLG          ;$00 or $40 if subscripts ok, else $80
            sbc     #"("            ;if subflg=$00 and char='('...
            bne     LE049           ;nope
LE046       jmp     ARRAY           ;yes

LE049       bit     SUBFLG          ;check top two bits of SUBFLG
            bmi     LE04F           ;$80
            bvs     LE046           ;$40, called from GETARYPT
LE04F       lda     #$00            ;clear SUBFLG
            sta     SUBFLG
            lda     VARTAB          ;start LOWTR at simple variable table
            ldx     VARTAB+1
            ldy     #$00
LE059       stx     LOWTR+1
LE05B       sta     LOWTR
            cpx     ARYTAB+1        ;end of simple variables?
            bne     LE065           ;no, go on
            cmp     ARYTAB          ;yes; end of arrays?
            beq     NAME_NOT_FOUND  ;yes, make one
LE065       lda     VARNAM          ;same first letter?
            cmp     (LOWTR),y
            bne     LE073           ;not same first letter
            lda     VARNAM+1        ;same second letter?
            iny
            cmp     (LOWTR),y
            beq     SET_VARPNT_AND_YA ;yes, same variable name
            dey                     ;no, bump to next name
LE073       clc
            lda     LOWTR
            adc     #$07
            bcc     LE05B
            inx
            bne     LE059           ;...always

; Check if A-reg is ASCII letter A-Z
; 
; Return carry = 1 if A-Z
;              = 0 if not
; 
; <<< NOTE: faster and shorter code: >>>
;    cmp #'Z'+1  ;compare hi end
;    bcs .1      ;above A-Z
;    cmp #'A'    ;compare lo end
;    rts         ;C=0 if lo, C=1 if A-Z
; .1 clc        ;C=0 if hi
;    rts
ISLETC      cmp     #"A"            ;compare lo end
            bcc     LE086           ;C=0 if low
            sbc     #"["            ;(should be #'Z'+1)  prepare hi end test
            sec                     ;test hi end, restoring A-reg
            sbc     #$a5            ;(should be #-1-'Z')  C=0 if lo, C=1 if A-Z
LE086       rts

; Variable not found, so make one
NAME_NOT_FOUND
            pla                     ;look at return address on stack to
            pha                     ; see if called from FRM_VARIABLE
            cmp     #<FRM_VARIABLE+2
            bne     MAKE_NEW_VARIABLE ;no
            tsx
            lda     STACK+2,x
            cmp     #>FRM_VARIABLE
            bne     MAKE_NEW_VARIABLE ;no
            lda     #<C_ZERO        ;yes, called from FRM_VARIABLE
            ldy     #>C_ZERO        ;point to a constant zero
            rts                     ;new variable used in expression = 0

C_ZERO      word    $0000           ;integer or real zero, or null string

; Make a new simple variable
; 
; Move arrays up 7 bytes to make room for new variable.  Enter 7-byte variable
; data in the hole.
; Clear variables
NUMDIM      =    $0f    
ARYPNT      =    $94    
INDX        =    $99    

MAKE_NEW_VARIABLE
            lda     ARYTAB          ;set up call to BLTU to
            ldy     ARYTAB+1        ; move from ARYTAB through STREND-1
            sta     LOWTR           ; 7 bytes higher
LE0A2       sty     LOWTR+1
            lda     STREND
            ldy     STREND+1
            sta     HIGHTR
            sty     HIGHTR+1
            clc
            adc     #7
            bcc     LE0B2
            iny
LE0B2       sta     ARYPNT
            sty     ARYPNT+1
            jsr     BLTU            ;move array block up
            lda     ARYPNT          ;store new start of arrays
            ldy     ARYPNT+1
            iny
            sta     ARYTAB
            sty     ARYTAB+1
            ldy     #$00
            lda     VARNAM          ;first char of name
            sta     (LOWTR),y
            iny
            lda     VARNAM+1        ;second char of name
            sta     (LOWTR),y
            lda     #$00            ;set five-byte value to 0
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
; Put address of value of variable in VARPNT and (Y,A)
SET_VARPNT_AND_YA
            lda     LOWTR           ;LOWTR points at name of variable
            clc                     ;so add 2 to get to value
            adc     #$02
            ldy     LOWTR+1
            bcc     LE0E8
            iny
LE0E8       sta     VARPNT          ;address in VARPNT and (Y,A)
            sty     VARPNT+1
            rts

; Compute address of first value in array
; 
; ARYPNT = LOWTR + #dims*2 + 5
GETARY      lda     NUMDIM          ;get # of dimensions
GETARY2     asl     A               ;#dims*2 (size of each dim in 2 bytes)
            adc     #5              ;+ 5 (2 for name, 2 for offset to next array, 1 for #dims)
            adc     LOWTR           ;address of this array in ARYTAB
            ldy     LOWTR+1
            bcc     LE0F9
            iny
LE0F9       sta     ARYPNT          ;address of first value in array
            sty     ARYPNT+1
            rts

; <<< meant to be -32768, which would be 9080000000 >>>
; <<< 1 byte short, so picks up $20 from next instruction >>>
NEG32768    byte   $90,$80,$00,$00        ;-32768.00049 in floating point

; Evaluate numeric formula at TXTPTR, converting result to integer 0 <= X <=
; 32767 in FAC+3,4
MAKINT      jsr     CHRGET
            jsr     FRMNUM
; Convert FAC to integer.  Must be positive and less than 32768.
MKINT       lda     FAC_SIGN        ;error if -
            bmi     MI1
; Convert FAC to integer.  Must be -32767 <= FAC <= 32767.
AYINT       lda     FAC             ;exponent of value in FAC
            cmp     #$90            ;abs(value) < 32768?
            bcc     MI2             ;yes, okay for integer
            lda     #<NEG32768      ;no; next few lines are supposed
            ldy     #>NEG32768      ;to allow -32768 ($8000), but do not!
            jsr     FCOMP           ;because compared to -32768.00049
; <<< BUG: A=-32768.00049:A%=A is accepted, but PRINT A,A% shows that A=-
; 32768.0005 (ok), A%=32767 (wrong!) >>>
MI1         bne     IQERR           ;illegal quantity
MI2         jmp     QINT            ;convert to integer

; Locate array element or create an array
ARRAY       lda     SUBFLG          ;subscripts given?
            bne     LE169           ;no
; Parse the subscript list
            lda     DIMFLG          ;yes
            ora     VALTYP+1        ;set high bit if %
            pha                     ;save VALTYP and DIMFLG on stack
            lda     VALTYP
            pha
            ldy     #$00            ;count # dimensions in Y-reg
LE12C       tya                     ;save #dims on stack
            pha
            lda     VARNAM+1        ;save variable name on stack
            pha
            lda     VARNAM
            pha
            jsr     MAKINT          ;evaluate subscript as integer
            pla                     ;restore variable name
            sta     VARNAM
            pla
            sta     VARNAM+1
            pla                     ;restore # dims to Y-reg
            tay
            tsx                     ;copy VALTYP and DIMFLG on stack
            lda     STACK+2,x       ;to leave room for the subscript
            pha
            lda     STACK+1,x
            pha
            lda     FAC+3           ;get subscript value and place in the
            sta     STACK+2,x       ; stack where valtyp & DIMFLG were
            lda     FAC+4
            sta     STACK+1,x
            iny                     ;count the subscript
            jsr     CHRGOT          ;next char
            cmp     #","
            beq     LE12C           ;comma, parse another subscript
            sty     NUMDIM          ;no more subscripts, save #
            jsr     CHKCLS          ;now need ")"
            pla                     ;restore VALTYPE and DIMFLG
            sta     VALTYP
            pla
            sta     VALTYP+1
            and     #$7f            ;isolate DIMFLG
            sta     DIMFLG
; Search array table for this array name
LE169       ldx     ARYTAB          ;(A,X) = start of array table
            lda     ARYTAB+1
LE16D       stx     LOWTR           ;use LOWTR for running pointer
            sta     LOWTR+1
            cmp     STREND+1        ;did we reach the end of arrays yet?
            bne     LE179           ;no, keep searching
            cpx     STREND
            beq     MAKE_NEW_ARRAY  ;yes, this is a new array name
LE179       ldy     #$00            ;point at 1st char of array name
            lda     (LOWTR),y       ;get 1st char of name
            iny                     ;point at 2nd char
            cmp     VARNAM          ;1st char same?
            bne     LE188           ;no, move to next array
            lda     VARNAM+1        ;yes, try 2nd char
            cmp     (LOWTR),y       ;same?
            beq     USE_OLD_ARRAY   ;yes, array found
LE188       iny                     ;point at offset to next array
            lda     (LOWTR),y       ;add offset to running pointer
            clc
            adc     LOWTR
            tax
            iny
            lda     (LOWTR),y
            adc     LOWTR+1
            bcc     LE16D           ;...always

; ERROR: bad subscripts
SUBERR      ldx     #ERR_BADSUBS
            byte    $2C             ;bit    $35a2           ;trick to skip next line
; ERROR: illegal quantity
IQERR       ldx     #ERR_ILLQTY
JER         jmp     ERROR

; Found the array
USE_OLD_ARRAY
            ldx     #ERR_REDIMD     ;set up for redim'd array error
            lda     DIMFLG          ;called from DIM statement?
            bne     JER             ;yes, error
            lda     SUBFLG          ;no, check if any subscripts
            beq     LE1AA           ;yes, need to check the number
            sec                     ;no, signal array found
            rts

LE1AA       jsr     GETARY          ;set ARYPNT = addr of first element
            lda     NUMDIM          ;compare number of dimensions
            ldy     #4
            cmp     (LOWTR),y
            bne     SUBERR          ;not same, subscript error
            jmp     FIND_ARRAY_ELEMENT

; Create a new array, unless called from GETARYPT.
MAKE_NEW_ARRAY
            lda     SUBFLG          ;called from GETARYPT?
            beq     LE1C1           ;no
            ldx     #ERR_NODATA     ;yes, give "out of data" error
            jmp     ERROR

LE1C1       jsr     GETARY          ;put addr of 1st element in ARYPNT
            jsr     REASON          ;make sure enough memory left
; <<< next 3 lines could be written: >>>
;   LDY #0
;   STY STRING2+1
            lda     #$00            ;point Y-reg at variable name slot
            tay
            sta     STRNG2+1        ;start size computation
            ldx     #$05            ;assume 5-bytes per element
            lda     VARNAM          ;stuff variable name in array
            sta     (LOWTR),y
            bpl     LE1D5           ;not integer array
            dex                     ;integer array, decr. size to 4 bytes
LE1D5       iny                     ;point Y-reg at next char of name
            lda     VARNAM+1        ;rest of array name
            sta     (LOWTR),y
            bpl     LE1DE           ;real array, stick with size = 5 bytes
            dex                     ;integer or string array, adjust size
            dex                     ;to integer=3, string=2 bytes
LE1DE       stx     STRNG2          ;store low byte of array element size
            lda     NUMDIM          ;store number of dimensions
            iny                     ; in 5th byte of array
            iny
            iny
            sta     (LOWTR),y
LE1E7       ldx     #11             ;default dimension = 11 elements
            lda     #0              ;for hi byte of dimension if default
            bit     DIMFLG          ;dimensioned array?
            bvc     LE1F7           ;no, use default value
            pla                     ;get specified dim in (A,X)
            clc                     ;# elements is 1 larger than
            adc     #$01            ; dimension value
            tax
            pla
            adc     #$00
LE1F7       iny                     ;add this dimension to array descriptor
            sta     (LOWTR),y
            iny
            txa
            sta     (LOWTR),y
            jsr     MULTIPLY_SUBSCRIPT ;multiply this dimension by running size (LOWTR*STRNG2->(A,X))
            stx     STRNG2          ;store running size in STRNG2
            sta     STRNG2+1
            ldy     INDEX           ;retrieve Y saved by MULTIPLY_SUBSCRIPT
            dec     NUMDIM          ;count down # dims
            bne     LE1E7           ;loop till done
; Now (A,X) has total # bytes of array elements
            adc     ARYPNT+1        ;compute address of end of this array
            bcs     GME             ;...too large, error
            sta     ARYPNT+1
            tay
            txa
            adc     ARYPNT
            bcc     LE21A
            iny
            beq     GME             ;...too large, error
LE21A       jsr     REASON          ;make sure there is room up to (Y,A)
            sta     STREND          ;there is room so save new end of table
            sty     STREND+1        ; and zero the array
            lda     #$00
            inc     STRNG2+1        ;prepare for fast zeroing loop
            ldy     STRNG2          ;# bytes mod 256
            beq     LE22E           ;full page
LE229       dey                     ;clear page full
            sta     (ARYPNT),y
            bne     LE229
LE22E       dec     ARYPNT+1        ;point to next page
            dec     STRNG2+1        ;count the pages
            bne     LE229           ;still more to clear
            inc     ARYPNT+1        ;recover last DEC, point at 1st element
            sec
            lda     STREND          ;compute offset to end of arrays
            sbc     LOWTR           ;and store in array descriptor
            ldy     #2
            sta     (LOWTR),y
            lda     STREND+1
            iny
            sbc     LOWTR+1
            sta     (LOWTR),y
            lda     DIMFLG          ;was this called from DIM statement?
            bne     RTS_9           ;yes, we are finished
            iny                     ;no, now need to find the element
; Find specified array element
; 
; LOWTR,y points at # of dims in array descriptor.  The subscripts are all on
; the stack as integers.
FIND_ARRAY_ELEMENT
            lda     (LOWTR),y       ;get # of dimensions
            sta     NUMDIM
            lda     #$00            ;zero subscript accumulator
            sta     STRNG2
FAE_1       sta     STRNG2+1
            iny
            pla                     ;pull next subscript from stack
            tax                     ;save in FAC+3,4
            sta     FAC+3           ;and compare with dimensioned size
            pla
            sta     FAC+4
            cmp     (LOWTR),y
            bcc     FAE_2           ;subscript not too large
            bne     GSE             ;subscript is too large
            iny                     ;check low byte of subscript
            txa
            cmp     (LOWTR),y
            bcc     FAE_3           ;not too large
; 
GSE         jmp     SUBERR          ;bad subscripts error

GME         jmp     MEMERR          ;mem full error

FAE_2       iny                     ;bump pointer into descriptor
FAE_3       lda     STRNG2+1        ;bypass multiplication if value so
            ora     STRNG2          ; far = 0
            clc
            beq     LE281           ;it is zero so far
            jsr     MULTIPLY_SUBSCRIPT ;not zero, so multiply
            txa                     ;add current subscript
            adc     FAC+3
            tax
            tya
            ldy     INDEX           ;retrieve Y-reg saved by MULTIPLY_SUBSCRIPT
LE281       adc     FAC+4           ;finish adding current subscript
            stx     STRNG2          ;store accumulated offset
            dec     NUMDIM          ;last subscript yet?
            bne     FAE_1           ;no, loop till done
            sta     STRNG2+1        ;yes, now multiply by element size
            ldx     #5              ;start with size = 5
            lda     VARNAM          ;determine variable type
            bpl     LE292           ;not integer
            dex                     ;integer, back down size to 4 bytes
LE292       lda     VARNAM+1        ;discriminate between real and str
            bpl     LE298           ;it is real
            dex                     ;size = 3 if string, = 2 if integer
            dex
LE298       stx     RESULT+2        ;set up multiplier
            lda     #$00            ;hi byte of multiplier
            jsr     MULTIPLY_SUBS_1 ;STRNG2 by element size
            txa                     ;add accumulated offset
            adc     ARYPNT          ;to address of 1st element
            sta     VARPNT          ;to get address of specified element
            tya
            adc     ARYPNT+1
            sta     VARPNT+1
            tay                     ;return with addr in VARPNT
            lda     VARPNT          ; and in (Y,A)
RTS_9       rts

; Multiply STRNG2 by (LOWTR,Y) leaving product in (A,X).  Hi-byte also in Y. 
; Used only by array subscript routines.
MULTIPLY_SUBSCRIPT
            sty     INDEX           ;save Y-reg
            lda     (LOWTR),y       ;get multiplier
            sta     RESULT+2        ;save in result+2,3
            dey
            lda     (LOWTR),y
MULTIPLY_SUBS_1
            sta     RESULT+3        ;low byte of multiplier
            lda     #16             ;multiply 16 bits
            sta     INDX
            ldx     #$00            ;product = 0 initially
            ldy     #$00
LE2C0       txa                     ;double product
            asl     A               ;low byte
            tax
            tya                     ;high byte
            rol     A               ;if too large, set carry
            tay
            bcs     GME             ;too large, "mem full error"
            asl     STRNG2          ;next bit of multiplicand
            rol     STRNG2+1        ; into carry
            bcc     LE2D9           ;bit=0, don't need to add
            clc                     ;bit=1, add into partial product
            txa
            adc     RESULT+2
            tax
            tya
            adc     RESULT+3
            tay
            bcs     GME             ;too large, "mem full error"
LE2D9       dec     INDX            ;16 bits yet?
            bne     LE2C0           ;no, keep shuffling
            rts                     ;yes, product in (Y,X) and (A,X)

;********************************************************************************
;* FRE statement                                                                *
;*                                                                              *
;* Collects garbage and returns # bytes of memory left.                         *
;********************************************************************************
FRE         lda     VALTYP          ;look at value of argument
            beq     LE2E5           ;=0 means real, =$FF means string
            jsr     FREFAC          ;string, so set it free if temp
LE2E5       jsr     GARBAG          ;collect all the garbage in sight
            sec                     ;compute space between arrays and
            lda     FRETOP          ; string temp area
            sbc     STREND
            tay
            lda     FRETOP+1
            sbc     STREND+1        ;free space in (Y,A)
; Fall into GIVAYF to float the value.  Note that values over 32767 will return
; as negative.
; 
; Float the signed integer in (A,Y).
GIVAYF      ldx     #$00            ;mark FAC value type real
            stx     VALTYP
            sta     FAC+1           ;save value from A,Y in mantissa
            sty     FAC+2
            ldx     #$90            ;set exponent to 2^16
            jmp     FLOAT_1         ;convert to signed fp

;********************************************************************************
;* POS statement                                                                *
;*                                                                              *
;* Returns current line position from MON_CH.                                   *
;********************************************************************************
POS         ldy     MON_CH          ;Get (A,Y) = MON_CH, go to GIVAYF
; Float Y-reg into FAC, giving value 0-255
SNGFLT      lda     #$00            ;MSB = 0
            sec                     ;<<< no purpose whatsoever >>>
            beq     GIVAYF          ;...always

; Check for direct or running mode, giving error if direct mode.
ERRDIR      ldx     CURLIN+1        ;=$FF if direct mode
            inx                     ;makes $FF into zero
            bne     RTS_9           ;return if running mode
            ldx     #ERR_ILLDIR     ;direct mode, give error
            byte    $2C             ;bit    LE0A2           ;trick to skip next 2 bytes
UNDFNC      ldx     #ERR_UNDEFFUNC  ;undefined function error
            jmp     ERROR

;********************************************************************************
;* DEF statement                                                                *
;********************************************************************************
DEF         jsr     FNC_            ;parse FN, function name
            jsr     ERRDIR          ;error if in direct mode
            jsr     CHKOPN          ;need "("
            lda     #$80            ;flag PRTGET that called from DEF FN
            sta     SUBFLG          ;allow only simple fp variable for arg
            jsr     PTRGET          ;get ptr to argument
            jsr     CHKNUM          ;must be numeric
            jsr     CHKCLS          ;must have ")" now
            lda     #TOK_EQUAL      ;now need "="
            jsr     SYNCHR          ;or else syntax error
            pha                     ;save char after "="
            lda     VARPNT+1        ;save ptr to argument
            pha
            lda     VARPNT
            pha
            lda     TXTPTR+1        ;save TXTPTR
            pha
            lda     TXTPTR
            pha
            jsr     DATA            ;scan to next statement
            jmp     FNCDATA         ;store above 5 bytes in "value"

; Common routine for DEF FN and FN, to parse FN and the function name
FNC_        lda     #TOK_FN         ;must now see FN token
            jsr     SYNCHR          ;or else syntax error
            ora     #$80            ;set sign bit on 1st char of name,
            sta     SUBFLG          ; making $C0 < SUBFLG < $DB
            jsr     PTRGET3         ; which tells PTRGET who called
            sta     FNCNAM          ;found valid function name, so
            sty     FNCNAM+1        ; save address
            jmp     CHKNUM          ;must be numeric

;********************************************************************************
;* FN statement                                                                 *
;********************************************************************************
FUNCT       jsr     FNC_            ;parse FN, function name
            lda     FNCNAM+1        ;stack function address
            pha                     ;in case of a nested FN call
            lda     FNCNAM
            pha
            jsr     PARCHK          ;must now have "(expression)"
            jsr     CHKNUM          ;must be numeric expression
            pla                     ;get function address back
            sta     FNCNAM
            pla
            sta     FNCNAM+1
            ldy     #$02            ;point at add of argument variable
            lda     (FNCNAM),y
            sta     VARPNT
            tax
            iny
            lda     (FNCNAM),y
            beq     UNDFNC          ;undefined function
            sta     VARPNT+1
            iny                     ;Y=4 now
LE378       lda     (VARPNT),y      ;save old value of argument variable
            pha                     ; on stack, in case also used as
            dey                     ; a normal variable
            bpl     LE378
            ldy     VARPNT+1        ;(Y,X) = address, store FAC in variable
            jsr     STORE_FAC_AT_YX_ROUNDED
            lda     TXTPTR+1        ;remember TXTPTR after FN call
            pha
            lda     TXTPTR
            pha
            lda     (FNCNAM),y      ;Y=0 from MOVMF
            sta     TXTPTR          ;point to function def'n
            iny
            lda     (FNCNAM),y
            sta     TXTPTR+1
            lda     VARPNT+1        ;save address of argument variable
            pha
            lda     VARPNT
            pha
            jsr     FRMNUM          ;evaluate the function expression
            pla                     ;get address of argument variable
            sta     FNCNAM          ; and save it
            pla
            sta     FNCNAM+1
            jsr     CHRGOT          ;must be at ":" or EOL
            beq     LE3A9           ;we are
            jmp     SYNERR          ;we are not, syntax error

LE3A9       pla                     ;retrieve TXTPTR after FN call
            sta     TXTPTR
            pla
            sta     TXTPTR+1
; Stack now has 5-byte value of the argument variable, and FNCNAM points at the
; variable.
; 
; Store five bytes from stack at FNCNAM.
FNCDATA     ldy     #$00
            pla
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            rts

;********************************************************************************
;* STR$ statement                                                               *
;********************************************************************************
STR         jsr     CHKNUM          ;expresson must be numeric
            ldy     #$00            ;start string at STACK-1 ($00FF)
            jsr     FOUT_1          ;convert FAC to string
            pla                     ;pop return off stack
            pla
            lda     #<STACK+255     ;point to STACK-1
            ldy     #(>STACK)-1     ;which=0
            beq     STRLIT          ;...always, create desc & move string

; Get space and make descriptor for string whose address is in FAC+3,4 and whose
; length is in A-reg
STRINI      ldx     FAC+3           ;Y,X = string address
            ldy     FAC+4
            stx     DSCPTR
            sty     DSCPTR+1
; Get space and make descriptor for string whose address is in (Y,X) and whose
; length is in A-reg.
STRSPA      jsr     GETSPA          ;A-reg holds length
            stx     FAC+1           ;save descriptor in FAC
            sty     FAC+2           ;---FAC--- --FAC+1-- --FAC+2--
            sta     FAC             ;<length>  <addr-lo> <addr-hi>
            rts

; Build a descriptor for string starting at (Y,A) and terminated by $00 or
; quotation mark.  Return with descriptor in a temporary and address of
; descriptor in FAC+3,4.
STRLIT      ldx     #'"'            ;set up literal scan to stop on
            stx     CHARAC          ;quotation mark or $00
            stx     ENDCHR
; Build a descriptor for string starting at (Y,A) and terminated by $00, CHARAC,
; or ENDCHR.
; 
; Return with descriptor in a temporary and address of descriptor in FAC+3,4.
STRLT2      sta     STRNG1          ;save address of string
            sty     STRNG1+1
            sta     FAC+1           ;...again
            sty     FAC+2
            ldy     #$ff
LE3F7       iny                     ;find end of string
            lda     (STRNG1),y      ;next string char
            beq     LE408           ;end of string
            cmp     CHARAC          ;alternate terminator #1?
            beq     LE404           ;yes
            cmp     ENDCHR          ;alternate terminator #2?
            bne     LE3F7           ;no, keep scanning
LE404       cmp     #'"'            ;is string ended with quote mark?
            beq     LE409           ;yes, C=1 to include " in string
LE408       clc
LE409       sty     FAC             ;save length
            tya
            adc     STRNG1          ;compute address of end of string
            sta     STRNG2          ;(of 00 byte, or just after ")
            ldx     STRNG1+1
            bcc     LE415
            inx
LE415       stx     STRNG2+1
            lda     STRNG1+1        ;where does the string start?
            beq     LE41F           ;page 0, must be from STR$ function
            cmp     #2              ;page 2?
            bne     PUTNEW          ;no, not page 0 or 2
LE41F       tya                     ;length of string
            jsr     STRINI          ;make space for string
            ldx     STRNG1
            ldy     STRNG1+1
            jsr     MOVSTR          ;move it in
; Store descriptor in temporary descriptor stack.
; 
; The descriptor is now in FAC, FAC+1, FAC+2.  Put address of temp descriptor in
; FAC+3,4.
PUTNEW      ldx     TEMPPT          ;pointer to next temp string slot
            cpx     #TEMPST+9       ;max of 3 temp strings
            bne     PUTEMP          ;room for another one
            ldx     #ERR_FRMCPX     ;too many, formula too complex
JERR        jmp     ERROR

; Clear variables
GARFLG      =    $13    ;overlaps DATAFLG
LASTPT      =    $53    ;overlaps TEMPPT+1
;ARYPNT      =    $94    ;Overlaps HIGHDS

PUTEMP      lda     FAC             ;copy temp descriptor into temp stack
            sta     0,x
            lda     FAC+1
            sta     1,x
            lda     FAC+2
            sta     2,x
            ldy     #$00
            stx     FAC+3           ;address of temp descriptor
            sty     FAC+4           ;in (Y,X) and FAC+3,4
            dey                     ;Y=$FF
            sty     VALTYP          ;flag FAC as string
            stx     LASTPT          ;index of last pointer
            inx                     ;update for next temp entry
            inx
            inx
            stx     TEMPPT
            rts

; Make space for string at bottom of string space.
; 
;   A-reg = # bytes space to make
; 
; Return with A-reg same, and (Y,X) = address of space allocated
GETSPA      lsr     GARFLG          ;clear signbit of flag
LE454       pha                     ;A-reg holds length
            eor     #$ff            ;get -length
            sec
            adc     FRETOP          ;compute starting address of space
            ldy     FRETOP+1        ;for the string
            bcs     LE45F
            dey
LE45F       cpy     STREND+1        ;see if fits in remaining memory
            bcc     LE474           ;no, try garbage
            bne     LE469           ;yes, it fits
            cmp     STREND          ;have to check lower bytes
            bcc     LE474           ;not enuf room yet
LE469       sta     FRETOP          ;there is room so save new FRETOP
            sty     FRETOP+1
            sta     FRESPC
            sty     FRESPC+1
            tax                     ;addr in (Y,X)
            pla                     ;length in A-reg
            rts

LE474       ldx     #ERR_MEMFULL
            lda     GARFLG          ;garbage done yet?
            bmi     JERR            ;yes, memory is really full
            jsr     GARBAG          ;no, try collecting now
            lda     #$80            ;flag that collected garbage already
            sta     GARFLG
            pla                     ;get string length again
            bne     LE454           ;...always

; Shove all referenced strings as high as possible in memory (against HIMEM),
; freeing up space below string area down to STREND.
GARBAG      ldx     MEMSIZE         ;collect from top down
            lda     MEMSIZE+1
FIND_HIGHEST_STRING
            stx     FRETOP          ;one pass through all vars
            sta     FRETOP+1        ;for each active string!
            ldy     #$00
            sty     FNCNAM+1        ;flag in case no strings to collect
            lda     STREND
            ldx     STREND+1
            sta     LOWTR
            stx     LOWTR+1
; Start by collecting temporaries.
            lda     #TEMPST
            ldx     #>TEMPST
            sta     INDEX
            stx     INDEX+1
LE4A0       cmp     TEMPPT          ;finished with temps yet?
            beq     LE4A9           ;yes, now do simple variables
            jsr     CHECK_VARIABLE  ;do a temp
            beq     LE4A0           ;...always

; Now collect simple variables.
LE4A9       lda     #7              ;length of each variable is 7 bytes
            sta     DSCLEN
            lda     VARTAB          ;start at beginning of vartab
            ldx     VARTAB+1
            sta     INDEX
            stx     INDEX+1
LE4B5       cpx     ARYTAB+1        ;finished with simple variables?
            bne     LE4BD           ;no
            cmp     ARYTAB          ;maybe, check low byte
            beq     LE4C2           ;yes, now do arrays
LE4BD       jsr     CHECK_SIMPLE_VARIABLE
            beq     LE4B5           ;...always

; Now collect array variables.
LE4C2       sta     ARYPNT
            stx     ARYPNT+1
            lda     #3              ;descriptors in arrays are 3 bytes each
            sta     DSCLEN
LE4CA       lda     ARYPNT          ;compare to end of arrays
            ldx     ARYPNT+1
LE4CE       cpx     STREND+1        ;finished with arrays yet?
            bne     LE4D9           ;not yet
            cmp     STREND          ;maybe, check low byte
            bne     LE4D9           ;not finished yet
            jmp     MOVE_HIGHEST_STRING_TO_TOP ;finished

LE4D9       sta     INDEX           ;set up ptr to start of array
            stx     INDEX+1
            ldy     #$00            ;point at name of array
            lda     (INDEX),y
            tax                     ;1st letter of name in X-reg
            iny
            lda     (INDEX),y
            php                     ;status from second letter of name
            iny
            lda     (INDEX),y       ;offset to next array
            adc     ARYPNT          ;(carry always clear)
            sta     ARYPNT          ;calculate start of next array
            iny
            lda     (INDEX),y       ;hi byte of offset
            adc     ARYPNT+1
            sta     ARYPNT+1
            plp                     ;get status from 2nd char of name
            bpl     LE4CA           ;not a string array
            txa                     ;set status with 1st char of name
            bmi     LE4CA           ;not a string array
            iny
            lda     (INDEX),y       ;# of dimensions for this array
            ldy     #$00
            asl     A               ;preamble size = 2*#dims + 5
            adc     #5
            adc     INDEX           ;make index point at first element
            sta     INDEX           ; in the array
            bcc     LE50A
            inc     INDEX+1
LE50A       ldx     INDEX+1         ;step thru each string in this array
LE50C       cpx     ARYPNT+1        ;array done?
            bne     LE514           ;no, process next element
            cmp     ARYPNT          ;maybe, check low byte
            beq     LE4CE           ;yes, move to next array
LE514       jsr     CHECK_VARIABLE  ;process the array
            beq     LE50C           ;...always

; Process a simple variable.
CHECK_SIMPLE_VARIABLE
            lda     (INDEX),y       ;look at 1st char of name
            bmi     CHECK_BUMP      ;not a string variable
            iny
            lda     (INDEX),y       ;look at 2nd char of name
            bpl     CHECK_BUMP      ;not a string variable
            iny
; If string is not empty, check if it is highest.
CHECK_VARIABLE
            lda     (INDEX),y       ;get length of string
            beq     CHECK_BUMP      ;ignore string if length is zero
            iny
            lda     (INDEX),y       ;get address of string
            tax
            iny
            lda     (INDEX),y
            cmp     FRETOP+1        ;check if already collected
            bcc     LE538           ;no, below FRETOP
            bne     CHECK_BUMP      ;yes, above FRETOP
            cpx     FRETOP          ;maybe, check low byte
            bcs     CHECK_BUMP      ;yes, above FRETOP
LE538       cmp     LOWTR+1         ;above highest string found?
            bcc     CHECK_BUMP      ;no, ignore for now
            bne     LE542           ;yes, this is the new highest
            cpx     LOWTR           ;maybe, try low byte
            bcc     CHECK_BUMP      ;no, ignore for now
LE542       stx     LOWTR           ;make this the highest string
            sta     LOWTR+1
            lda     INDEX           ;save address of descriptor too
            ldx     INDEX+1
            sta     FNCNAM
            stx     FNCNAM+1
            lda     DSCLEN
            sta     LENGTH
; Add DSCLEN to ptr in INDEX.  Return with Y=0, ptr also in (X,A).
CHECK_BUMP  lda     DSCLEN          ;bump to next variable
            clc
            adc     INDEX
            sta     INDEX
            bcc     CHECK_EXIT
            inc     INDEX+1
CHECK_EXIT  ldx     INDEX+1
            ldy     #$00
            rts

; Found highest non-empty string, so move it to top and go back for another.
; Clear variables
;LASTPT      =    $53    ;Overlaps TEMPPT+1

MOVE_HIGHEST_STRING_TO_TOP
            ldx     FNCNAM+1        ;any string found?
            beq     CHECK_EXIT      ;no, return
            lda     LENGTH          ;get length of variable element
            and     #$04            ;was 7 or 3, make 4 or 0
            lsr     A               ;2 or 0; in simple variables,
            tay                     ; name precedes descriptor
            sta     LENGTH          ;2 or 0
            lda     (FNCNAM),y      ;get length from descriptor
            adc     LOWTR           ;carry already cleared by LSR
            sta     HIGHTR          ;string is btwn LOWTR and HIGHTR
            lda     LOWTR+1
            adc     #$00
            sta     HIGHTR+1
            lda     FRETOP          ;high end destination
            ldx     FRETOP+1
            sta     HIGHDS
            stx     HIGHDS+1
            jsr     BLTU2           ;move string up
            ldy     LENGTH          ;fix its descriptor
            iny                     ;point at address in descriptor
            lda     HIGHDS          ;store new address
            sta     (FNCNAM),y
            tax
            inc     HIGHDS+1        ;correct BTLU's overshoot
            lda     HIGHDS+1
            iny
            sta     (FNCNAM),y
            jmp     FIND_HIGHEST_STRING

; Concatenate two strings.
CAT         lda     FAC+4           ;save address of first descriptor
            pha
            lda     FAC+3
            pha
            jsr     FRM_ELEMENT     ;get second string element
            jsr     CHKSTR          ;must be a string
            pla                     ;recover address of 1st descriptor
            sta     STRNG1
            pla
            sta     STRNG1+1
            ldy     #$00
            lda     (STRNG1),y      ;add lenghts, get concatenated size
            clc
            adc     (FAC+3),y
            bcc     LE5B7           ;ok if < $100
            ldx     #ERR_STRLONG
            jmp     ERROR

LE5B7       jsr     STRINI          ;get space for concatenated strings
            jsr     MOVINS          ;move 1st string
            lda     DSCPTR
            ldy     DSCPTR+1
            jsr     FRETMP
            jsr     MOVSTR_1        ;move 2nd string
            lda     STRNG1
            ldy     STRNG1+1
            jsr     FRETMP
            jsr     PUTNEW          ;set up descriptor
            jmp     FRMEVL_2        ;finish expression

; Get string descriptor pointed at by STRNG1 and move described string to
; FRESPC.
MOVINS      ldy     #$00
            lda     (STRNG1),y
            pha                     ;length
            iny
            lda     (STRNG1),y
            tax                     ;put string pointer in (X,Y)
            iny
            lda     (STRNG1),y
            tay
            pla                     ;retrieve length
; Move string at (Y,X) with length in A-reg to destination whose address is in
; FRESPC,FRESPC+1.
MOVSTR      stx     INDEX           ;put pointer in INDEX
            sty     INDEX+1
MOVSTR_1    tay                     ;length to Y-reg
            beq     LE5F3           ;if length is zero, finished
            pha                     ;save length on stack
LE5EA       dey                     ;move bytes from INDEX to FRESPC
            lda     (INDEX),y
            sta     (FRESPC),y
            tya                     ;test if any left to move
            bne     LE5EA           ;yes, keep moving
            pla                     ;no, finished; get length
LE5F3       clc                     ; and add to FRESPC, so
            adc     FRESPC          ; FRESPC points to next higher
            sta     FRESPC          ; byte (used by concatenation)
            bcc     LE5FC
            inc     FRESPC+1
LE5FC       rts

; If FAC is a temporary string, release descriptor.
FRESTR      jsr     CHKSTR          ;last result a string?
; If string descriptor pointed to be FAC+3,4 is a temporary string, release it.
FREFAC      lda     FAC+3           ;get descriptor pointer
            ldy     FAC+4
; If string descriptor whose address is in (Y,A) is a temporary string, release
; it.
FRETMP      sta     INDEX           ;save the address of the descriptor
            sty     INDEX+1
            jsr     FRETMS          ;free descriptor if it is temporary
            php                     ;remember if temp
            ldy     #$00            ;point at length of string
            lda     (INDEX),y
            pha                     ;save length on stack
            iny
            lda     (INDEX),y
            tax                     ;get address of string in (Y,X)
            iny
            lda     (INDEX),y
            tay
            pla                     ;length in A-reg
            plp                     ;retrieve status, Z=1 if temp
            bne     LE630           ;not a temporary string
            cpy     FRETOP+1        ;is it the lowest string?
            bne     LE630           ;no
            cpx     FRETOP
            bne     LE630           ;no
            pha                     ;yes, push length again
            clc                     ;recover the space used by
            adc     FRETOP          ; the string
            sta     FRETOP
            bcc     LE62F
            inc     FRETOP+1
LE62F       pla                     ;retrieve length again
LE630       stx     INDEX           ;address of string in (Y,X)
            sty     INDEX+1         ;length of string in A-reg
            rts

; Release temporary descriptor if (Y,A) = LASTPT.
FRETMS      cpy     LASTPT+1        ;compare (Y,A) to latest temp
            bne     LE645           ;not same one, cannot release
            cmp     LASTPT
            bne     LE645           ;not same one, cannot release
            sta     TEMPPT          ;update TEMPPT for next temp
            sbc     #3              ;back off LASTPT
            sta     LASTPT
            ldy     #$00            ;now (Y,A) points to top temp
LE645       rts                     ;Z=0 if not temp, Z=1 if temp

;********************************************************************************
;* CHR$ statement                                                               *
;********************************************************************************
CHRSTR      jsr     CONINT          ;convert argument to byte in X-reg
            txa
            pha                     ;save it
            lda     #$01            ;get space for string of length 1
            jsr     STRSPA
            pla                     ;recall the character
            ldy     #$00            ;put in string
            sta     (FAC+1),y
            pla                     ;pop return address
            pla
            jmp     PUTNEW          ;make it a temporary string

;********************************************************************************
;* LEFT$ statement                                                              *
;********************************************************************************
LEFTSTR     jsr     SUBSTRING_SETUP
            cmp     (DSCPTR),y      ;compare 1st parameter to length
            tya                     ;Y=A=0
SUBSTRING_1 bcc     LE666           ;1st parameter smaller, use it
            lda     (DSCPTR),y      ;1st is longer, use string length
            tax                     ;in X-reg
            tya                     ;Y=A=0 again
LE666       pha                     ;push left end of substring
SUBSTRING_2 txa
SUBSTRING_3 pha                     ;push length of substring
            jsr     STRSPA          ;make room for string of A-reg bytes
            lda     DSCPTR          ;release parameter string if temp
            ldy     DSCPTR+1
            jsr     FRETMP
            pla                     ;get length of substring
            tay                     ;in Y-reg
            pla                     ;get left end of substring
            clc                     ;add to pointer to string
            adc     INDEX
            sta     INDEX
            bcc     LE67F
            inc     INDEX+1
LE67F       tya                     ;length
            jsr     MOVSTR_1        ;copy string into space
            jmp     PUTNEW          ;add to temps

;********************************************************************************
;* RIGHT$ statement                                                             *
;********************************************************************************
RIGHTSTR    jsr     SUBSTRING_SETUP
            clc                     ;compute length-width of substring
            sbc     (DSCPTR),y      ;to get starting point in string
            eor     #$ff
            jmp     SUBSTRING_1     ;join LEFT$

;********************************************************************************
;* MID$ statement                                                               *
;********************************************************************************
MIDSTR      lda     #$ff            ;flag whether 2nd parameter
            sta     FAC+4
            jsr     CHRGOT          ;see if ")" yet
            cmp     #")"
            beq     LE6A2           ;yes, no 2nd parameter
            jsr     CHKCOM          ;no, must have comma
            jsr     GETBYT          ;get 2nd param in X-reg
LE6A2       jsr     SUBSTRING_SETUP
            dex                     ;1st parameter - 1
            txa
            pha
            clc
            ldx     #$00
            sbc     (DSCPTR),y
            bcs     SUBSTRING_2
            eor     #$ff
            cmp     FAC+4           ;use smaller of two
            bcc     SUBSTRING_3
            lda     FAC+4
            bcs     SUBSTRING_3     ;...always

; Common setup routine for LEFT$, RIGHT$, MID$: require ")"; pop return adrs,
; get descriptor address, get 1st parameter of command
SUBSTRING_SETUP
            jsr     CHKCLS          ;require ")"
            pla                     ;save return address
            tay                     ; in Y-reg and LENGTH
            pla
            sta     LENGTH
            pla                     ;pop previous return address
            pla                     ; (from GOROUT)
            pla                     ;retrieve 1st parameter
            tax
            pla                     ;get address of string descriptor
            sta     DSCPTR
            pla
            sta     DSCPTR+1
            lda     LENGTH          ;restore return address
            pha
            tya
            pha
            ldy     #$00
            txa                     ;get 1st parameter in A-reg
            beq     GOIQ            ;error if 0
            rts

;********************************************************************************
;* LEN statement                                                                *
;********************************************************************************
LEN         jsr     GETSTR          ;get length in Y-reg, make FAC numeric
            jmp     SNGFLT          ;float Y-reg into FAC

; If last result is a temporary string, free it.  Make VALTYP numeric, return
; length in Y-reg.
GETSTR      jsr     FRESTR          ;if last result is a string, free it
            ldx     #$00            ;make VALTYP numeric
            stx     VALTYP
            tay                     ;length of string to Y-reg
            rts

;********************************************************************************
;* ASC statement                                                                *
;********************************************************************************
ASC         jsr     GETSTR          ;get string, get length in Y-reg
            beq     GOIQ            ;error if length 0
            ldy     #$00
            lda     (INDEX),y       ;get 1st char of string
            tay
            jmp     SNGFLT          ;float Y-reg into FAC

GOIQ        jmp     IQERR           ;illegal quantity error

; Scan to next character and convert expression to single byte in X-reg.
GTBYTC      jsr     CHRGET
; Evaluate expression at TXTPTR, and convert it to single byte in X-reg.
GETBYT      jsr     FRMNUM
; Convert FAC to single-byte integer in X-reg.
CONINT      jsr     MKINT           ;convert if in range -32767 to +32767
            ldx     FAC+3           ;high byte must be zero
            bne     GOIQ            ;value > 255, error
            ldx     FAC+4           ;value in X-reg
            jmp     CHRGOT          ;get next char in A-reg

;********************************************************************************
;* VAL statement                                                                *
;********************************************************************************
VAL         jsr     GETSTR          ;get pointer to string in index
            bne     LE70F           ;length non-zero
            jmp     ZERO_FAC        ;return 0 if length=0

LE70F       ldx     TXTPTR          ;save current TXTPTR
            ldy     TXTPTR+1
            stx     STRNG2
            sty     STRNG2+1
            ldx     INDEX
            stx     TXTPTR          ;point TXTPTR to start of string
            clc
            adc     INDEX           ;add length
            sta     DEST            ;point DEST to end of string + 1
            ldx     INDEX+1
            stx     TXTPTR+1
            bcc     LE727
            inx
LE727       stx     DEST+1
            ldy     #$00            ;save byte that follows string
            lda     (DEST),y        ; on stack
            pha
            lda     #$00            ;and store $00 in its place
            sta     (DEST),y
; <<< That causes a bug if HIMEM=$BFFF, because storing $00 at $C000 is no use;
; $C000 will always be last char typed, so FIN won't terminate until it sees a
; zero at $C010! >>>
            jsr     CHRGOT          ;prime the pump
            jsr     FIN             ;evalute string
            pla                     ;get byte that should follow string
            ldy     #$00            ;and put it back
            sta     (DEST),y
; Copy STRNG2 into TXTPTR.
POINT       ldx     STRNG2
            ldy     STRNG2+1
            stx     TXTPTR
            sty     TXTPTR+1
            rts

; Evalute "EXP1,EXP2"
; 
;   Convert EXP1 to 16-bit number in LINNUM
;   Convert EXP2 to 8-bit number in X-reg
GTNUM       jsr     FRMNUM
            jsr     GETADR
; Evaluate ",expression"
; 
;   Convert expression to single byte in X-reg
COMBYTE     jsr     CHKCOM          ;must have comma first
            jmp     GETBYT          ;convert expression to byte in X-reg

; Convert FAC to a 16-bit value in LINNUM.
GETADR      lda     FAC             ;FAC < 2^16?
            cmp     #$91
            bcs     GOIQ            ;no, illegal quantity
            jsr     QINT            ;convert to integer
            lda     FAC+3           ;copy it into LINNUM
            ldy     FAC+4
            sty     LINNUM          ;to LINNUM
            sta     LINNUM+1
            rts

;********************************************************************************
;* PEEK statement                                                               *
;********************************************************************************
PEEK        lda     LINNUM          ;save LINNUM on stack during peek
            pha
            lda     LINNUM+1
            pha
            jsr     GETADR          ;get address peeking at
            ldy     #$00
            lda     (LINNUM),y      ;take a quick look
            tay                     ;value in Y-reg
            pla                     ;restore LINNUM from stack
            sta     LINNUM+1
            pla
            sta     LINNUM
            jmp     SNGFLT          ;float Y-reg into FAC

;********************************************************************************
;* POKE statement                                                               *
;********************************************************************************
POKE        jsr     GTNUM           ;get the address and value
            txa                     ;value in A,
            ldy     #$00
            sta     (LINNUM),y      ;store it away,
            rts                     ;and that's all for today.

;********************************************************************************
;* WAIT statement                                                               *
;********************************************************************************
WAIT        jsr     GTNUM           ;get address in LINNUM, mask in X-reg
            stx     FORPNT          ;save mask
            ldx     #$00
            jsr     CHRGOT          ;another parameter?
            beq     LE793           ;no, use $00 for exclusive-or
            jsr     COMBYTE         ;get xor-mask
LE793       stx     FORPNT+1        ;save xor-mask here
            ldy     #$00
LE797       lda     (LINNUM),y      ;get byte at address
            eor     FORPNT+1        ;invert specified bits
            and     FORPNT          ;select specified bits
            beq     LE797           ;loop till not 0
RTS_10      rts

; Add 0.5 to FAC
; Clear variables
ARG_EXTENSION =  $92    ;Overlaps LENGTH+1
;SGNCPR      =    $ab    ;flags opp sign in fp routines
FAC_EXTENSION =  $ac    ;Overlaps STRNG1+1

FADDH       lda     #<CON_HALF      ;FAC + 1/2 -> FAC
            ldy     #>CON_HALF
            jmp     FADD

; FAC = (Y,A) - FAC
FSUB        jsr     LOAD_ARG_FROM_YA
; FAC = ARG - FAC
FSUBT       lda     FAC_SIGN        ;complement FAC and add
            eor     #$ff
            sta     FAC_SIGN
            eor     ARG_SIGN        ;fix SGNCPR too
            sta     SGNCPR
            lda     FAC             ;make status show FAC exponent
            jmp     FADDT           ;join FADD

; Shift smaller argument more than 7 bits.
FADD_1      jsr     SHIFT_RIGHT     ;align radix by shifting
            bcc     FADD_3          ;...always

; FAC = (Y,A) + FAC
FADD        jsr     LOAD_ARG_FROM_YA
; FAC = ARG + FAC
FADDT       bne     LE7C6           ;FAC is non-zero
            jmp     COPY_ARG_TO_FAC ;FAC = 0 + ARG

LE7C6       ldx     FAC_EXTENSION
            stx     ARG_EXTENSION
            ldx     #ARG            ;set up to shift ARG
            lda     ARG             ;exponent
; 
FADD_2      tay
            beq     RTS_10          ;if ARG=0, we are finished
            sec
            sbc     FAC             ;get difference of exp
            beq     FADD_3          ;go add if same exp
            bcc     LE7EA           ;arg has smaller exponent
            sty     FAC             ;exp has smaller exponent
            ldy     ARG_SIGN
            sty     FAC_SIGN
            eor     #$ff            ;complement shift count
            adc     #$00            ;carry was set
            ldy     #$00
            sty     ARG_EXTENSION
            ldx     #FAC            ;set up to shift FAC
            bne     LE7EE           ;...always

LE7EA       ldy     #$00
            sty     FAC_EXTENSION
LE7EE       cmp     #$f9            ;shift more than 7 bits?
            bmi     FADD_1          ;yes
            tay                     ;index to # of shifts
            lda     FAC_EXTENSION
            lsr     1,x             ;start shifting...
            jsr     SHIFT_RIGHT_4   ;...complete shifting
FADD_3      bit     SGNCPR          ;do FAC and ARG have same signs?
            bpl     FADD_4          ;yes, add the mantissas
            ldy     #FAC            ;no, subtract smaller from larger
            cpx     #ARG            ;which was adjusted?
            beq     LE806           ;if ARG, do FAC - ARG
            ldy     #ARG            ;if FAC, do ARG - FAC
LE806       sec                     ;subtract smaller from larger (we hope)
            eor     #$ff            ;(if exponents were equal, we might be
            adc     ARG_EXTENSION   ; subtracting larger from smaller)
            sta     FAC_EXTENSION
            lda     4,y
            sbc     4,x
            sta     FAC+4
            lda     3,y
            sbc     3,x
            sta     FAC+3
            lda     2,y
            sbc     2,x
            sta     FAC+2
            lda     1,y
            sbc     1,x
            sta     FAC+1
; Normalize value in FAC.
NORMALIZE_FAC_1
            bcs     NORMALIZE_FAC_2
            jsr     COMPLEMENT_FAC
NORMALIZE_FAC_2
            ldy     #$00            ;shift up signif digit
            tya                     ;start A=0, count shifts in A-reg
            clc
LE832       ldx     FAC+1           ;look at most significant byte
            bne     NORMALIZE_FAC_4 ;some 1-bits here
            ldx     FAC+2           ;high byte of mantissa still zero,
            stx     FAC+1           ; so do a fast 8-bit shuffle
            ldx     FAC+3
            stx     FAC+2
            ldx     FAC+4
            stx     FAC+3
            ldx     FAC_EXTENSION
            stx     FAC+4
            sty     FAC_EXTENSION   ;zero extension byte
            adc     #8              ;bump shift count
            cmp     #32             ;done 4 times yet?
            bne     LE832           ;no, still might be some 1's
; Set FAC = 0 (only necessary to zero exponent and sign cells)
ZERO_FAC    lda     #$00
STA_IN_FAC_SIGN_AND_EXP
            sta     FAC
STA_IN_FAC_SIGN
            sta     FAC_SIGN
            rts

; Add mantissas of FAC and ARG into FAC.
FADD_4      adc     ARG_EXTENSION
            sta     FAC_EXTENSION
            lda     FAC+4
            adc     ARG+4
            sta     FAC+4
            lda     FAC+3
            adc     ARG+3
            sta     FAC+3
            lda     FAC+2
            adc     ARG+2
            sta     FAC+2
            lda     FAC+1
            adc     ARG+1
            sta     FAC+1
            jmp     NORMALIZE_FAC_5

; Finish normalizing FAC.
NORMALIZE_FAC_3
            adc     #1              ;count bits shifted
            asl     FAC_EXTENSION
            rol     FAC+4
            rol     FAC+3
            rol     FAC+2
            rol     FAC+1
; 
NORMALIZE_FAC_4
            bpl     NORMALIZE_FAC_3 ;until top bit = 1
            sec
            sbc     FAC             ;adjust exponent by bits shifted
            bcs     ZERO_FAC        ;underflow, return zero
            eor     #$ff
            adc     #$01            ;2's complement
            sta     FAC             ;carry=0 now
NORMALIZE_FAC_5
            bcc     RTS_11          ;unless mantissa carried
NORMALIZE_FAC_6
            inc     FAC             ;mantissa carried, so shift right
            beq     OVERFLOW        ;overflow if exponent too big
            ror     FAC+1
            ror     FAC+2
            ror     FAC+3
            ror     FAC+4
            ror     FAC_EXTENSION
RTS_11      rts

; 2's complement of FAC
COMPLEMENT_FAC
            lda     FAC_SIGN
            eor     #$ff
            sta     FAC_SIGN
; 2's complement of FAC mantissa only
COMPLEMENT_FAC_MANTISSA
            lda     FAC+1
            eor     #$ff
            sta     FAC+1
            lda     FAC+2
            eor     #$ff
            sta     FAC+2
            lda     FAC+3
            eor     #$ff
            sta     FAC+3
            lda     FAC+4
            eor     #$ff
            sta     FAC+4
            lda     FAC_EXTENSION
            eor     #$ff
            sta     FAC_EXTENSION
            inc     FAC_EXTENSION   ;start incrementing mantissa
            bne     RTS_12
; Increment FAC mantissa.
INCREMENT_FAC_MANTISSA
            inc     FAC+4           ;add carry from extra
            bne     RTS_12
            inc     FAC+3
            bne     RTS_12
            inc     FAC+2
            bne     RTS_12
            inc     FAC+1
RTS_12      rts

OVERFLOW    ldx     #ERR_OVERFLOW
            jmp     ERROR

; Shift 1,X through 5,X right
;   A-reg = negative of shift count
;   X-reg = pointer to bytes to be shifted
; 
;   Return with Y-reg=0, carry=0, extension bits in A-reg
SHIFT_RIGHT_1
            ldx     #RESULT-1       ;shift result right
SHIFT_RIGHT_2
            ldy     4,x             ;shift 8 bits right
            sty     FAC_EXTENSION
            ldy     3,x
            sty     4,x
            ldy     2,x
            sty     3,x
            ldy     1,x
            sty     2,x
            ldy     SHIFT_SIGN_EXT  ;$00 if +, $FF if -
            sty     1,x
; Main entry to right shift subroutine.
SHIFT_RIGHT adc     #8
            bmi     SHIFT_RIGHT_2   ;still more than 8 bits to go
            beq     SHIFT_RIGHT_2   ;exactly 8 more bits to go
            sbc     #8              ;undo ADC above
            tay                     ;remaining shift count
            lda     FAC_EXTENSION
            bcs     SHIFT_RIGHT_5   ;finished shifiting
SHIFT_RIGHT_3
            asl     1,x             ;sign -> carry (sign extension)
            bcc     LE903           ;sign +
            inc     1,x             ;put sign in LSB
LE903       ror     1,x             ;restore value, sign still in carry
            ror     1,x             ;start right shift, inserting sign
; Enter here for short shifts with no sign extension.
SHIFT_RIGHT_4
            ror     2,x
            ror     3,x
            ror     4,x
            ror     A               ;extension
            iny                     ;count the shift
            bne     SHIFT_RIGHT_3
SHIFT_RIGHT_5
            clc                     ;return with carry clear
            rts

CON_ONE     byte   $81,$00,$00,$00,$00
POLY_LOG    byte   3               ;# of coefficients - 1
            byte   $7f,$5e,$56,$cb,$79      ;* X^7 +
            byte   $80,$13,$9b,$0b,$64      ;* X^5 +
            byte   $80,$76,$38,$93,$16      ;* X^3 +
            byte   $82,$38,$aa,$3b,$20      ;* X
; 
CON_SQR_HALF
            byte   $80,$35,$04,$f3,$34
CON_SQR_TWO byte   $81,$35,$04,$f3,$34
CON_NEG_HALF
            byte   $80,$80,$00,$00,$00
CON_LOG_TWO byte   $80,$31,$72,$17,$f8

;********************************************************************************
;* LOG statement                                                                *
;********************************************************************************
LOG         jsr     SIGN            ;get -1,0,+1 in A-reg for FAC
            beq     GIQ             ;LOG(0) is illegal
            bpl     LOG_2           ;>0 is ok
GIQ         jmp     IQERR           ;<= 0 is no good

LOG_2       lda     FAC             ;first get log base 2
            sbc     #$7f            ;save unbiased exponent
            pha
            lda     #$80            ;normalize between .5 and 1
            sta     FAC
            lda     #<CON_SQR_HALF
            ldy     #>CON_SQR_HALF
            jsr     FADD            ;compute via series of odd
            lda     #<CON_SQR_TWO   ; powers of
            ldy     #>CON_SQR_TWO   ; (SQR(2)X-1)/(SQR(2)X+1)
            jsr     FDIV
            lda     #<CON_ONE
            ldy     #>CON_ONE
            jsr     FSUB
            lda     #<POLY_LOG
            ldy     #>POLY_LOG
            jsr     POLYNOMIAL_ODD
            lda     #<CON_NEG_HALF
            ldy     #>CON_NEG_HALF
            jsr     FADD
            pla
            jsr     ADDACC          ;add original exponent
            lda     #<CON_LOG_TWO   ;multiply by log(2) to form
            ldy     #>CON_LOG_TWO   ; natural log of X
; FAC = (Y,A) * FAC
FMULT       jsr     LOAD_ARG_FROM_YA
; FAC = ARG * FAC
FMULTT      bne     LE987           ;FAC .ne. zero
            jmp     RTS_13          ;FAC = 0 * ARG = 0

; <<< why is line above just "RTS"? >>>
LE987       jsr     ADD_EXPONENTS
            lda     #$00
            sta     RESULT          ;init product = 0
            sta     RESULT+1
            sta     RESULT+2
            sta     RESULT+3
            lda     FAC_EXTENSION
            jsr     MULTIPLY_1
            lda     FAC+4
            jsr     MULTIPLY_1
            lda     FAC+3
            jsr     MULTIPLY_1
            lda     FAC+2
            jsr     MULTIPLY_1
            lda     FAC+1
            jsr     MULTIPLY_2
            jmp     COPY_RESULT_INTO_FAC

; Multiply ARG by A-reg into RESULT
MULTIPLY_1  bne     MULTIPLY_2      ;this byte non-zero
            jmp     SHIFT_RIGHT_1   ;A-reg=0, just shift ARG right 8

MULTIPLY_2  lsr     A               ;shift bit into carry
            ora     #$80            ;supply sentinel bit
LE9B8       tay                     ;remaining multiplier to Y-reg
            bcc     LE9D4           ;this multiplier bit = 0
            clc                     ;= 1, so add ARG to RESULT
            lda     RESULT+3
            adc     ARG+4
            sta     RESULT+3
            lda     RESULT+2
            adc     ARG+3
            sta     RESULT+2
            lda     RESULT+1
            adc     ARG+2
            sta     RESULT+1
            lda     RESULT
            adc     ARG+1
            sta     RESULT          ;shift RESULT right 1
LE9D4       ror     RESULT
            ror     RESULT+1
            ror     RESULT+2
            ror     RESULT+3
            ror     FAC_EXTENSION
            tya                     ;remaining multiplier
            lsr     A               ;LSB into carry
            bne     LE9B8           ;if sentinel still here, multiply
RTS_13      rts                     ;8 x 32 completed

; Unpack number at (Y,A) into ARG
LOAD_ARG_FROM_YA
            sta     INDEX           ;use INDEX for ptr
            sty     INDEX+1
            ldy     #4              ;five bytes to move
            lda     (INDEX),y
            sta     ARG+4
            dey
            lda     (INDEX),y
            sta     ARG+3
            dey
            lda     (INDEX),y
            sta     ARG+2
            dey
            lda     (INDEX),y
            sta     ARG_SIGN
            eor     FAC_SIGN        ;set combined sign for multi/div
            sta     SGNCPR
            lda     ARG_SIGN        ;turn on normalized invisible bit
            ora     #$80            ; to complete mantissa
            sta     ARG+1
            dey
            lda     (INDEX),y
            sta     ARG             ;exponent
            lda     FAC             ;set status bits on FAC exponent
            rts

; Add exponents of ARG and FAC (called by FMULT and FDIV).
; 
; Also check for overflow, and set result sign.
ADD_EXPONENTS
            lda     ARG
ADD_EXPONENTS_1
            beq     ZERO            ;if ARG=0, result is zero
            clc
            adc     FAC
            bcc     LEA1B           ;in range
            bmi     JOV             ;overflow
            clc
            byte    $2C             ;bit    $1410           ;trick to skip
LEA1B       bpl     ZERO            ;overflow
            adc     #$80            ;re-bias
            sta     FAC             ;result
            bne     LEA26
            jmp     STA_IN_FAC_SIGN ;result is zero

; <<< Crazy to jump way back there!  Same identical code is below!  Instead of
; BNE .2, JMP STA_IN_FAC_SIGN, only needed BEQ .3 >>>
LEA26       lda     SGNCPR          ;set sign of result
            sta     FAC_SIGN
            rts

; If FAC is positive, give "overflow" error.
; If FAC is negative, set FAC=0, pop one return, and RTS.
; Called from EXP function.
OUTOFRNG    lda     FAC_SIGN
            eor     #$ff
            bmi     JOV             ;error if positive #
; Pop return address and set FAC=0.
ZERO        pla
            pla
            jmp     ZERO_FAC

JOV         jmp     OVERFLOW

; Multiply FAC by 10.
MUL10       jsr     COPY_FAC_TO_ARG_ROUNDED
            tax                     ;test FAC exponent
            beq     LEA4F           ;finished if FAC=0
            clc
            adc     #2              ;add 2 to exponent gives FAC*4
            bcs     JOV             ;overflow
            ldx     #$00
            stx     SGNCPR
            jsr     FADD_2          ;makes FAC*5
            inc     FAC             ;*2, makes FAC*10
            beq     JOV             ;overflow
LEA4F       rts

CON_TEN     byte   $84,$20,$00,$00,$00

; Divide FAC by 10.
DIV10       jsr     COPY_FAC_TO_ARG_ROUNDED
            lda     #<CON_TEN       ;set up to put
            ldy     #>CON_TEN       ; 10 in FAC
            ldx     #$00
; FAC = ARG / (Y,A)
DIV         stx     SGNCPR
LEA60       jsr     LOAD_FAC_FROM_YA
            jmp     FDIVT           ;divide ARG by FAC

; FAC = (Y,A) / FAC
FDIV        jsr     LOAD_ARG_FROM_YA
; FAC = ARG / FAC
FDIVT       beq     LEAE1           ;FAC = 0, divide by zero error
            jsr     ROUND_FAC
            lda     #$00            ;negate FAC exponent, so
            sec                     ; ADD_EXPONENTS forms difference
            sbc     FAC
            sta     FAC
            jsr     ADD_EXPONENTS
            inc     FAC
            beq     JOV             ;overflow
            ldx     #252            ;(should be -4) index for result
            lda     #$01            ;sentinel
LEA80       ldy     ARG+1           ;see if FAC can be subtracted
            cpy     FAC+1
            bne     LEA96
            ldy     ARG+2
            cpy     FAC+2
            bne     LEA96
            ldy     ARG+3
            cpy     FAC+3
            bne     LEA96
            ldy     ARG+4
            cpy     FAC+4
LEA96       php                     ;save the answer, and also roll the
            rol     A               ; bit into the quotient, sentinel out
            bcc     LEAA3           ;no sentinel, still not 8 trips
            inx                     ;8 trips, store byte of quotient
            sta     RESULT+3,x
            beq     LEAD1           ;32 bits completed
            bpl     LEAD5           ;final exit when X-reg=1
            lda     #$01            ;re-start sentinel
LEAA3       plp                     ;get answer, can FAC be subtracted?
            bcs     LEAB4           ;yes, do it
LEAA6       asl     ARG+4           ;no, shift ARG left
            rol     ARG+3
            rol     ARG+2
            rol     ARG+1
            bcs     LEA96           ;another trip
            bmi     LEA80           ;have to compare first
            bpl     LEA96           ;...always

LEAB4       tay                     ;save quotient/sentinel byte
            lda     ARG+4           ;subtract FAC from ARG once
            sbc     FAC+4
            sta     ARG+4
            lda     ARG+3
            sbc     FAC+3
            sta     ARG+3
            lda     ARG+2
            sbc     FAC+2
            sta     ARG+2
            lda     ARG+1
            sbc     FAC+1
            sta     ARG+1
            tya                     ;restore quotient/sentinel byte
            jmp     LEAA6           ;go to shift arg and continue

LEAD1       lda     #$40            ;do a few extension bits
            bne     LEAA3           ;...always

LEAD5       asl     A               ;left justify the extension bits we did
            asl     A
            asl     A
            asl     A
            asl     A
            asl     A
            sta     FAC_EXTENSION
            plp
            jmp     COPY_RESULT_INTO_FAC

LEAE1       ldx     #ERR_ZERODIV
            jmp     ERROR

; Copy RESULT into FAC mantissa, and normalize.
COPY_RESULT_INTO_FAC
            lda     RESULT
            sta     FAC+1
            lda     RESULT+1
            sta     FAC+2
            lda     RESULT+2
            sta     FAC+3
            lda     RESULT+3
            sta     FAC+4
            jmp     NORMALIZE_FAC_2

; Unpack (Y,A) into FAC.
LOAD_FAC_FROM_YA
            sta     INDEX           ;use INDEX for ptr
            sty     INDEX+1
            ldy     #4              ;pick up 5 bytes
            lda     (INDEX),y
            sta     FAC+4
            dey
            lda     (INDEX),y
            sta     FAC+3
            dey
            lda     (INDEX),y
            sta     FAC+2
            dey
            lda     (INDEX),y
            sta     FAC_SIGN        ;first bit is sign
            ora     #$80            ;set normalized invisible bit
            sta     FAC+1
            dey
            lda     (INDEX),y
            sta     FAC             ;exponent
            sty     FAC_EXTENSION   ;Y-reg = 0
            rts

; Round FAC, store in TEMP2.
STORE_FAC_IN_TEMP2_ROUNDED
            ldx     #TEMP2          ;pack FAC into TEMP2
            byte    $2C             ;bit    $93a2           ;trick to branch
; Round FAC, store in TEMP1.
STORE_FAC_IN_TEMP1_ROUNDED
            ldx     #TEMP1          ;pack FAC into TEMP1
            ldy     #>TEMP1         ;hi-byte of TEMP1 same as TEMP2
            beq     STORE_FAC_AT_YX_ROUNDED ;...always

; Round FAC, and store where FORPNT points.
SETFOR      ldx     FORPNT
            ldy     FORPNT+1
; Round FAC, and store at (Y,X).
STORE_FAC_AT_YX_ROUNDED
            jsr     ROUND_FAC       ;round value in FAC using extension
            stx     INDEX           ;use INDEX for ptr
            sty     INDEX+1
            ldy     #4              ;storing 5 packed bytes
            lda     FAC+4
            sta     (INDEX),y
            dey
            lda     FAC+3
            sta     (INDEX),y
            dey
            lda     FAC+2
            sta     (INDEX),y
            dey
            lda     FAC_SIGN        ;pack sign in top bit of mantissa
            ora     #$7f
            and     FAC+1
            sta     (INDEX),y
            dey
            lda     FAC             ;exponent
            sta     (INDEX),y
            sty     FAC_EXTENSION   ;zero the extension
            rts

; Copy ARG into FAC.
COPY_ARG_TO_FAC
            lda     ARG_SIGN        ;copy sign
MFA         sta     FAC_SIGN
            ldx     #5              ;move 5 bytes
LEB59       lda     ARG-1,x
            sta     FAC-1,x
            dex
            bne     LEB59
            stx     FAC_EXTENSION   ;zero extension
            rts

; Round FAC and copy to ARG.
COPY_FAC_TO_ARG_ROUNDED
            jsr     ROUND_FAC       ;round FAC using extension
MAF         ldx     #6              ;copy 6 bytes, includes sign
LEB68       lda     FAC-1,x
            sta     ARG-1,x
            dex
            bne     LEB68
            stx     FAC_EXTENSION   ;zero FAC extension
RTS_14      rts

; Round FAC using extension byte.
ROUND_FAC   lda     FAC
            beq     RTS_14          ;FAC = 0, return
            asl     FAC_EXTENSION   ;is FAC_EXTENSION >= 128?
            bcc     RTS_14          ;no, finished
; Increment mantissa and re-normalize if carry.
INCREMENT_MANTISSA
            jsr     INCREMENT_FAC_MANTISSA ;yes, increment FAC
            bne     RTS_14          ;high byte has bits, finished
            jmp     NORMALIZE_FAC_6 ;hi byte = 0, so shift left

; Test FAC for zero and sign.
; 
;   FAC > 0, return +1
;   FAC = 0, return  0
;   FAC < 0, return -1
SIGN        lda     FAC             ;check sign of FAC and
            beq     RTS_15          ; return -1,0,1 in A-reg
SIGN1       lda     FAC_SIGN
SIGN2       rol     A               ;msbit to carry
            lda     #$ff            ;-1
            bcs     RTS_15          ;msbit = 1
            lda     #$01            ;+1
RTS_15      rts

;********************************************************************************
;* SGN statement                                                                *
;********************************************************************************
SGN         jsr     SIGN            ;convert FAC to -1,0,1
; Convert A-reg into FAC, as signed value -128 to +127.
FLOAT       sta     FAC+1           ;put in high byte of mantissa
            lda     #$00            ;clear 2nd byte of mantissa
            sta     FAC+2
            ldx     #$88            ;use exponent 2^9
; Float unsigned value in FAC+1,2.
; 
;   X-reg = exponent
FLOAT_1     lda     FAC+1           ;msbit=0, set carry; =1, clear carry
            eor     #$ff
            rol     A
; Float unsigned value in FAC+1,2
; 
;   X-reg = exponent
;   C=0 to make value negative
;   C=1 to make value positive
FLOAT_2     lda     #$00            ;clear lower 16 bits of mantissa
            sta     FAC+4
            sta     FAC+3
            stx     FAC             ;store exponent
            sta     FAC_EXTENSION   ;clear extension
            sta     FAC_SIGN        ;make sign positive
            jmp     NORMALIZE_FAC_1 ;if C=0, will negate FAC

;********************************************************************************
;* ABS statement                                                                *
;********************************************************************************
ABS         lsr     FAC_SIGN        ;change sign to +
            rts

; Compare FAC with packed # at (Y,A).
; Return A=1,0,-1 as (Y,A) is <,=,> FAC.
FCOMP       sta     DEST            ;use DEST for ptr
; Special entry from NEXT processor.  DEST already set up.
FCOMP2      sty     DEST+1
            ldy     #$00            ;get exponent of comparand
            lda     (DEST),y
            iny                     ;point at next byte
            tax                     ;exponent to X-reg
            beq     SIGN            ;if comparand=0, SIGN compares FAC
            lda     (DEST),y        ;get hi byte of mantissa
            eor     FAC_SIGN        ;compare with FAC sign
            bmi     SIGN1           ;different signs, SIGN gives answer
            cpx     FAC             ;same sign, so compare exponents
            bne     LEBE9           ;different, so sufficient test
            lda     (DEST),y        ;same exponent, compare mantissa
            ora     #$80            ;set invisible normalized bit
            cmp     FAC+1
            bne     LEBE9           ;not same, so sufficient
            iny                     ;same, compare more mantissa
            lda     (DEST),y
            cmp     FAC+2
            bne     LEBE9           ;not same, so sufficient
            iny                     ;same, compare more mantissa
            lda     (DEST),y
            cmp     FAC+3
            bne     LEBE9           ;not same, so sufficient
            iny                     ;same, compare more mantissa
            lda     #$7f            ;artificial extension byte for comparand
            cmp     FAC_EXTENSION
            lda     (DEST),y
            sbc     FAC+4
            beq     RTS_16          ;numbers are equal, return A-reg=0
LEBE9       lda     FAC_SIGN        ;numbers are different
            bcc     LEBEF           ;FAC is larger magnitude
            eor     #$ff            ;FAC is smaller magnitude
; <<< Note that above three lines can be shortened:
;   .1 ROR            ;put carry into sign bit
;      EOR FAC_SIGN   ;toggle with sign of FAC
; >>>
LEBEF       jmp     SIGN2           ;convert +1 or -1

; Quick integer function.
; 
; Converts fp value in FAC to integer value in FAC+1 ... FAC+4, by shifting
; right with sign extension until fractional bits are out.
; 
; This subroutine assumes the exponent < 32.
QINT        lda     FAC             ;look at FAC exponent
            beq     QINT_3          ;FAC=0, so finished
            sec                     ;get -(number of fractional bits)
            sbc     #$a0            ; in A-reg for shift count
            bit     FAC_SIGN        ;check sign of FAC
            bpl     LEC06           ;positive, continue
            tax                     ;negative, so complement mantissa
            lda     #$ff            ;and set sign extension for shift
            sta     SHIFT_SIGN_EXT
            jsr     COMPLEMENT_FAC_MANTISSA
            txa                     ;restore bit count to A-reg
LEC06       ldx     #FAC            ;point shift subroutine at FAC
            cmp     #$f9            ;more than 7 bits to shift?
            bpl     QINT_2          ;no, short shift
            jsr     SHIFT_RIGHT     ;yes, use general routine
            sty     SHIFT_SIGN_EXT  ;Y=0, clear sign extension
RTS_16      rts

QINT_2      tay                     ;save shift count
            lda     FAC_SIGN        ;get sign bit
            and     #$80
            lsr     FAC+1           ;start right shift
            ora     FAC+1           ;and merge with sign
            sta     FAC+1
            jsr     SHIFT_RIGHT_4   ;jump into middle of shifter
            sty     SHIFT_SIGN_EXT  ;Y=0, clear sign extension
            rts

;********************************************************************************
;* INT statement                                                                *
;*                                                                              *
;* Uses QINT to convert FAC to integer form, and then refloats the integer.     *
;* <<< A faster approach would simply clear the fractional bits by zeroing      *
;* them. >>>                                                                    *
;********************************************************************************
INT         lda     FAC             ;check if exponent < 32
            cmp     #$a0            ;because if > 31 there is no fraction
            bcs     RTS_17          ;no fraction, we are finished
            jsr     QINT            ;use general integer conversion
            sty     FAC_EXTENSION   ;Y=0, clear extension
            lda     FAC_SIGN        ;get sign of value
            sty     FAC_SIGN        ;Y=0, clear sign
            eor     #$80            ;toggle actual sign
            rol     A               ;and save in carry
            lda     #$a0            ;set exponent to 32
            sta     FAC             ; because 4-byte integer now
            lda     FAC+4           ;save low 8 bits of integer form
            sta     CHARAC          ; for exp and power
            jmp     NORMALIZE_FAC_1 ;normalize to finish conversion

QINT_3      sta     FAC+1           ;FAC=0, so clear all 4 bytes for
            sta     FAC+2           ; integer version
            sta     FAC+3
            sta     FAC+4
            tay                     ;Y=0 too
RTS_17      rts

; Convert string to FP value in FAC.
; 
;   String pointed to by TXTPTR
;   First char already scanned by CHRGET
;   A-reg=first char, C=0 if digit
; Clear variables
;LASTPT      =    $53    ;Overlaps TEMPPT+1
;ARG_EXTENSION =  $92    ;Overlaps LENGTH+1
DPFLG       =    $9b    ;Overlaps LOWTR
EXPSGN      =    $9c    ;Overlaps LOWTR+1
;SGNCPR      =    $ab    ;Overlaps STRING1
;FAC_EXTENSION =  $ac    ;Overlaps STRING1+1

FIN         ldy     #$00            ;clear working area ($99..A3)
            ldx     #10             ;TMPEXP, EXPON, DPFLG, EXPSGN, FAC, SERLEN
LEC4E       sty     TMPEXP,x
            dex
            bpl     LEC4E
            bcc     FIN_2           ;first char is a digit
            cmp     #"-"            ;check for leading sign
            bne     LEC5D           ;not minus
            stx     SERLEN          ;minus, set SERLEN = $FF for flag
            beq     FIN_1           ;...always

LEC5D       cmp     #"+"            ;might be plus
            bne     FIN_3           ;not plus either, check decimal point
FIN_1       jsr     CHRGET          ;get next char of string
FIN_2       bcc     FIN_9           ;insert this digit
FIN_3       cmp     #"."            ;check for decimal point
            beq     FIN_10          ;yes
            cmp     #"E"            ;check for exponent part
            bne     FIN_7           ;no, end of number
            jsr     CHRGET          ;yes, start converting exponent
            bcc     FIN_5           ;exponent digit
            cmp     #TOK_MINUS      ;negative exponent?
            beq     LEC85           ;yes
            cmp     #"-"            ;might not be tokenized yet
            beq     LEC85           ;yes, it is negative
            cmp     #TOK_PLUS       ;optional "+"
            beq     FIN_4           ;yes
            cmp     #"+"            ;might not be tokenized yet
            beq     FIN_4           ;yes, found "+"
            bne     FIN_6           ;...always, number completed

LEC85       ror     EXPSGN          ;C=1, set flag negative
; 
FIN_4       jsr     CHRGET          ;get next digit of exponent
FIN_5       bcc     GETEXP          ;char is a digit of exponent
FIN_6       bit     EXPSGN          ;end of number, check exp sign
            bpl     FIN_7           ;positive exponent
            lda     #$00            ;negative exponent
            sec                     ;make 2's complete of exponent
            sbc     EXPON
            jmp     FIN_8

; Found a decimal point.
FIN_10      ror     DPFLG           ;C=1, set DPFLG for decimal point
            bit     DPFLG           ;check if previous dec. pt.
            bvc     FIN_1           ;no previous decimal point
; A second decimal point is taken as a terminator to the numeric string.
; "A=11..22" will give a syntax error, because it is two numbers with no
; operator between.
; "PRINT 11..22" gives no error, because it is just the concatenation of two
; numbers.
; 
; Number terminated, adjust exponent now.
FIN_7       lda     EXPON           ;E-value
FIN_8       sec                     ;modify with count of digits
            sbc     TMPEXP          ; after the decimal point
            sta     EXPON           ;complete current exponent
            beq     LECB9           ;no adjust needed if exp=0
            bpl     LECB2           ;exp>0, multiply by ten
LECA9       jsr     DIV10           ;exp<0, divide by ten
            inc     EXPON           ;until exp=0
            bne     LECA9
            beq     LECB9           ;...always, we are finished

LECB2       jsr     MUL10           ;exp>0, multiply by ten
            dec     EXPON           ;until exp=0
            bne     LECB2
LECB9       lda     SERLEN          ;is whole number negative?
            bmi     LECBE           ;yes
            rts                     ;no, return, whole job done!

LECBE       jmp     NEGOP           ;negative number, so negate FAC

; Accumulate a digit into FAC.
FIN_9       pha                     ;save digit
            bit     DPFLG           ;seen a decimal point yet?
            bpl     LECC8           ;no, still in integer part
            inc     TMPEXP          ;yes, count the fractional digit
LECC8       jsr     MUL10           ;FAC = FAC * 10
            pla                     ;current digit
            sec                     ;<<< shorter here to just "AND #$0F"
            sbc     #"0"            ; to convert ASCII to binary form >>>
            jsr     ADDACC          ;add the digit
            jmp     FIN_1           ;go back for more

; Add A-reg to FAC.
ADDACC      pha                     ;save addend
            jsr     COPY_FAC_TO_ARG_ROUNDED
            pla                     ;get addend again
            jsr     FLOAT           ;convert to fp value in FAC
            lda     ARG_SIGN
            eor     FAC_SIGN
            sta     SGNCPR
            ldx     FAC             ;to signal if FAC=0
            jmp     FADDT           ;perform the addition

; Accumulate digit of exponent.
GETEXP      lda     EXPON           ;check current value
            cmp     #10             ;for more than 2 digits
            bcc     LECF7           ;no, this is 1st or 2nd digit
            lda     #100            ;exponent too big
            bit     EXPSGN          ;unless it is negative
            bmi     LED05           ;large negative exponent makes FAC=0
            jmp     OVERFLOW        ;large positive exponent is error

LECF7       asl     A               ;exponent times 10
            asl     A
            clc
            adc     EXPON
            asl     A
            clc                     ;<<< ASL already did this! >>>
            ldy     #$00            ;add the new digit
            adc     (TXTPTR),y      ;but this is in ASCII
            sec                     ; so adjust back to binary
            sbc     #"0"
LED05       sta     EXPON           ;new value
            jmp     FIN_4           ;back for more

CON_99999999_9
            byte   $9b,$3e,$bc,$1f,$fd      ;99,999,999.9
CON_999999999
            byte   $9e,$6e,$6b,$27,$fd      ;999,999,999
CON_BILLION byte   $9e,$6e,$6b,$28,$00      ;1,000,000,000

; Print "IN <LINE #>".
INPRT       lda     #<QT_IN         ;print " IN "
            ldy     #>QT_IN
            jsr     GO_STROUT
            lda     CURLIN+1
            ldx     CURLIN
; Print (A,X) as decimal integer.
LINPRT      sta     FAC+1           ;print A,X in decimal
            stx     FAC+2
            ldx     #$90            ;exponent = 2 ^ 16
            sec                     ;convert unsigned
            jsr     FLOAT_2         ;convert line # to fp
; Convert FAC to string, and print it.
PRINT_FAC   jsr     FOUT            ;convert FAC to string at stack
; Print string starting at (Y,A).
GO_STROUT   jmp     STROUT          ;print string at (Y,A)

; Convert FAC to string starting at stack.
; Return with (Y,A) pointing at string.
; Clear variables

FOUT        ldy     #$01            ;normal entry puts string at stack...
; STR$ function enters here, with Y-reg=0 so that result string starts at stack-
; 1 (this is used as a flag).
FOUT_1      lda     #"-"            ;in case value negative
            dey                     ;back up ptr
            bit     FAC_SIGN
            bpl     LED41           ;value is +
            iny                     ;value is -
            sta     STACK-1,y       ;emit "-"
LED41       sta     FAC_SIGN        ;make FAC_SIGN positive ($2D)
            sty     STRNG2          ;save string ptr
            iny
            lda     #"0"            ;in case FAC=0
            ldx     FAC             ;number=0?
            bne     LED4F           ;no, FAC not zero
            jmp     FOUT_4          ;yes, finished

LED4F       lda     #$00            ;starting value for TMPEXP
            cpx     #$80            ;any integer part?
            beq     LED57           ;no, btwn .5 and .999999999
            bcs     LED60           ;yes
LED57       lda     #<CON_BILLION   ;multiply by 1e9
            ldy     #>CON_BILLION   ;to give adjustment a head start
            jsr     FMULT
            lda     #$f7            ;(should be -9) exponent adjustment
LED60       sta     TMPEXP          ;0 or -9
; Adjust until 1e8 <= FAC < 1e9.
LED62       lda     #<CON_999999999
            ldy     #>CON_999999999
            jsr     FCOMP           ;compare to 1e9-1
            beq     LED89           ;FAC = 1e9-1
            bpl     LED7F           ;too large, divide by ten
LED6D       lda     #<CON_99999999_9 ;compare to 1e8-.1
            ldy     #>CON_99999999_9
            jsr     FCOMP           ;compare to 1e8-.1
            beq     LED78           ;FAC = 1e8-.1
            bpl     LED86           ;in range, adjustment finished
LED78       jsr     MUL10           ;too small, multiply by ten
            dec     TMPEXP          ;keep track of multiplies
            bne     LED6D           ;...always

LED7F       jsr     DIV10           ;too large, divide by ten
            inc     TMPEXP          ;keep track of divisions
            bne     LED62           ;...always

LED86       jsr     FADDH           ;round adjusted result
LED89       jsr     QINT            ;convert adjusted value to 32-bit integer
; FAC+1 ... FAC+4 is now in integer form with power of ten adjustment in TMPEXP.
; 
; If -10 < TMPEXP > 1, print in decimal form.  Otherwise, print in exponential
; form.
FOUT_2      ldx     #$01            ;assume 1 digit before "."
            lda     TMPEXP          ;check range
            clc
            adc     #10
            bmi     LED9E           ;< .01, use exponential form
            cmp     #11
            bcs     LED9F           ;>= 1e10, use exponential form
            adc     #$ff            ;less 1 gives index for "."
            tax
            lda     #$02            ;set remaining exponent = 0
LED9E       sec                     ;compute remaining exponent
LED9F       sbc     #$02
            sta     EXPON           ;value for "E+xx" or "E-xx"
            stx     TMPEXP          ;index for decimal point
            txa                     ;see if "." comes first
            beq     LEDAA           ;yes
            bpl     LEDBD           ;no, later
LEDAA       ldy     STRNG2          ;get index into string being built
            lda     #"."            ;store a decimal point
            iny
            sta     STACK-1,y
            txa                     ;see if need ".0"
            beq     LEDBB           ;no
            lda     #"0"            ;yes, store "0"
            iny
            sta     STACK-1,y
LEDBB       sty     STRNG2          ;save output index again
; Now divide by powers of ten to get successive digits.
LEDBD       ldy     #$00            ;index to table of powers of ten
            ldx     #$80            ;starting value for digit with direction
LEDC1       lda     FAC+4           ;start by adding -100000000 until
            clc                     ; overshoot.  Then add +10000000,
            adc     DECTBL+3,y      ; then add -1000000, then add
            sta     FAC+4           ; +100000, and so on.
            lda     FAC+3           ;the # of times each power is added
            adc     DECTBL+2,y      ; is 1 more than corresponding digit
            sta     FAC+3
            lda     FAC+2
            adc     DECTBL+1,y
            sta     FAC+2
            lda     FAC+1
            adc     DECTBL,y
            sta     FAC+1
            inx                     ;count the add
            bcs     LEDE5           ;if C=1 and X negative, keep adding
            bpl     LEDC1           ;if C=0 and X positive, keep adding
            bmi     LEDE7           ;if C=0 and X negative, we overshot

LEDE5       bmi     LEDC1           ;if C=1 and X positive, we overshot
LEDE7       txa                     ;overshot, so make X into a digit
            bcc     LEDEE           ;how depends on direction we were going
            eor     #$ff            ;digit = 9-x
            adc     #10
LEDEE       adc     #"/"            ;(should be #'0' - 1)  make digit into ASCII
            iny                     ;advance to next smaller power of ten
            iny
            iny
            iny
            sty     VARPNT          ;save ptr to powers
            ldy     STRNG2          ;get output ptr
            iny                     ;store the digit
            tax                     ;save digit, hi bit is direction
            and     #$7f            ;make sure $30..39 for string
            sta     STACK-1,y
            dec     TMPEXP          ;count the digit
            bne     LEE09           ;not time for "." yet
            lda     #"."            ;time, so store the decimal point
            iny
            sta     STACK-1,y
LEE09       sty     STRNG2          ;save output ptr again
            ldy     VARPNT          ;get ptr to powers
            txa                     ;get digit with hi bit = direction
            eor     #$ff            ;change direction
            and     #$80            ;$00 if adding, $80 if subtracting
            tax
            cpy     #<DECTBL-69     ;(should be DECTBL_END - DECTBL)
            bne     LEDC1           ;not finished yet
; Nine digits have been stored in string.  Now look back and lop off trailing
; zeroes and a trailing decimal point.
FOUT_3      ldy     STRNG2          ;points at last stored char
LEE19       lda     STACK-1,y       ;see if loppable
            dey
            cmp     #"0"            ;suppress trailing zeroes
            beq     LEE19           ;yes, keep looping
            cmp     #"."            ;suppress trailing decimal point
            beq     LEE26           ;".", so write over it
            iny                     ;not ".", so include in string again
LEE26       lda     #"+"            ;prepare for positive exponent "E+xx"
            ldx     EXPON           ;see if any E-value
            beq     FOUT_5          ;no, just mark end of string
            bpl     LEE36           ;yes, and it is positive
            lda     #$00            ;yes, and it is negative
            sec                     ;complement the value
            sbc     EXPON
            tax                     ;get magnitude in X-reg
            lda     #"-"            ;E sign
LEE36       sta     STACK+1,y       ;store sign in string
            lda     #"E"            ;store "E" in string before sign
            sta     STACK,y
            txa                     ;exponent magnitude in A-reg
            ldx     #"/"            ;(should be #'0'-1) seed for exponent digit
            sec                     ;convert to decimal
LEE42       inx                     ;count the subtraction
            sbc     #10             ;ten's digit
            bcs     LEE42           ;more tens to subtract
            adc     #":"            ;(should be #'0'+10) convert remainder to one's digit
            sta     STACK+3,y       ;store one's digit
            txa
            sta     STACK+2,y       ;store ten's digit
            lda     #$00            ;mark end of string with $00
            sta     STACK+4,y
            beq     FOUT_6          ;...always

FOUT_4      sta     STACK-1,y       ;store "0" in ASCII
FOUT_5      lda     #$00            ;store $00 on end of string
            sta     STACK,y
FOUT_6      lda     #<STACK         ;point (Y,A) at beginning of string
            ldy     #>STACK         ;(STR$ started string at STACK-1, but
            rts                     ; STR$ doesn't use (Y,A) anyway.)

CON_HALF    byte   $80,$00,$00,$00,$00      ;fp constant 0.5
; Powers of 10 from 1e8 down to 1, as 32-bit integers, with alternating signs.
DECTBL      byte   $fa,$0a,$1f,$00        ;-100000000
            byte   $00,$98,$96,$80        ;10000000
            byte   $ff,$f0,$bd,$c0        ;-1000000
            byte   $00,$01,$86,$a0        ;100000
            byte   $ff,$ff,$d8,$f0        ;-10000
            byte   $00,$00,$03,$e8        ;1000
            byte   $ff,$ff,$ff,$9c        ;-100
            byte   $00,$00,$00,$0a        ;10
            byte   $ff,$ff,$ff,$ff        ;-1

;********************************************************************************
;* SQR statement                                                                *
;*                                                                              *
;* <<< Unfortunately, rather than a Newton-Raphson iteration, Applesoft uses    *
;* exponentiation SQR(x) = x^.5 >>>                                             *
;********************************************************************************
SQR         jsr     COPY_FAC_TO_ARG_ROUNDED
            lda     #<CON_HALF      ;set up power of 0.5
            ldy     #>CON_HALF
            jsr     LOAD_FAC_FROM_YA
; Exponentiation operation
; 
; ARG ^ FAC = EXP( LOG(ARG) * FAC )
FPWRT       beq     EXP             ;if FAC=0, ARG^FAC=EXP(0)
            lda     ARG             ;if ARG=0, ARG^FAC=0
            bne     LEEA0           ;neither is zero
            jmp     STA_IN_FAC_SIGN_AND_EXP ;set FAC = 0

LEEA0       ldx     #TEMP3          ;save FAC in TEMP3
            ldy     #>TEMP3
            jsr     STORE_FAC_AT_YX_ROUNDED
            lda     ARG_SIGN        ;normally, ARG must be positive
            bpl     LEEBA           ;it is positive, so all is well
            jsr     INT             ;negative, but ok if integral power
            lda     #TEMP3          ;see if INT(FAC)=FAC
            ldy     #>TEMP3
            jsr     FCOMP           ;is it an integer power?
            bne     LEEBA           ;not integral, will cause error later
            tya                     ;mark ARG sign + as it is moved to FAC
            ldy     CHARAC          ;integral, so allow negative ARG
LEEBA       jsr     MFA             ;move argument to FAC
            tya                     ;save flag for negative ARG (0=+)
            pha
            jsr     LOG             ;get log(ARG)
            lda     #TEMP3          ;multiply by power
            ldy     #>TEMP3
            jsr     FMULT
            jsr     EXP             ;E ^ log(FAC)
            pla                     ;get flag for negative ARG
            lsr     A               ;<<< LSR,BCC could be merely BPL >>>
            bcc     RTS_18          ;not negative, finished
; Negate value in FAC.
NEGOP       lda     FAC             ;if FAC=0, no need to complement
            beq     RTS_18          ;yes, FAC=0
            lda     FAC_SIGN        ;no, so toggle sign
            eor     #$ff
            sta     FAC_SIGN
RTS_18      rts

CON_LOG_E   byte   $81,$38,$aa,$3b,$29      ;log(e) to base 2
POLY_EXP    byte   7               ;(# of terms in polynomial) - 1
            byte   $71,$34,$58,$3e,$56      ;(LOG(2)^7)/8!
            byte   $74,$16,$7e,$b3,$1b      ;(LOG(2)^6)/7!
            byte   $77,$2f,$ee,$e3,$85      ;(LOG(2)^5)/6!
            byte   $7a,$1d,$84,$1c,$2a      ;(LOG(2)^4)/5!
            byte   $7c,$63,$59,$58,$0a      ;(LOG(2)^3)/4!
            byte   $7e,$75,$fd,$e7,$c6      ;(LOG(2)^2)/3!
            byte   $80,$31,$72,$18,$10      ;LOG(2)/2!
            byte   $81,$00,$00,$00,$00      ;1

;********************************************************************************
;* EXP statement                                                                *
;*                                                                              *
;* FAC = E ^ FAC                                                                *
;********************************************************************************
SIGNFLG     =    $16    ;Overlaps CPRMASK
;ARG_EXTENSION =  $92    ;Overlaps LENGTH+1
;SGNCPR      =    $ab    ;Overlaps STRNG1
;FAC_EXTENSION =  $ac    ;Overlaps STRNG1+1
SERPNT      =    $ad    ;Overlaps STRNG2

EXP         lda     #<CON_LOG_E     ;convert to power of two problem
            ldy     #>CON_LOG_E     ;E^x = 2^(log2(e)*x)
            jsr     FMULT
            lda     FAC_EXTENSION   ;non-standard rounding here
            adc     #$50            ;round up if extension > $AF
            bcc     LEF19           ;no, don't round up
            jsr     INCREMENT_MANTISSA
LEF19       sta     ARG_EXTENSION   ;strange value
            jsr     MAF             ;copy FAC into ARG
            lda     FAC             ;maximum exponent is < 128
            cmp     #$88            ;within range?
            bcc     LEF27           ;yes
LEF24       jsr     OUTOFRNG        ;overflow if +, return 0.0 if -
LEF27       jsr     INT             ;get INT(FAC)
            lda     CHARAC          ;this is the integral part of the power
            clc                     ;add to exponent bias + 1
            adc     #$81
            beq     LEF24           ;overflow
            sec                     ;back to normal bias
            sbc     #$01
            pha                     ;save exponent
; 
            ldx     #5              ;swap ARG and FAC
LEF37       lda     ARG,x           ;<<< why swap? it is doing      >>>
            ldy     FAC,x           ;<<< -(A-B) when (B-A) is the   >>>
            sta     FAC,x           ;<<< same thing!                >>>
            sty     ARG,x
            dex
            bpl     LEF37
            lda     ARG_EXTENSION
            sta     FAC_EXTENSION
            jsr     FSUBT           ;power-INT(power) --> fractional part
            jsr     NEGOP
            lda     #<POLY_EXP
            ldy     #>POLY_EXP
            jsr     POLYNOMIAL      ;compute F(x) on fractional part
            lda     #$00
            sta     SGNCPR
            pla                     ;get exponent
            jsr     ADD_EXPONENTS_1
            rts                     ;<<< wasted byte here, could have just JMP ADD_EXPONENTS_1 >>>

; Odd polynomial subroutine
; 
;   F(x) = x * P(x^2)
; 
;   where: x is value in FAC
;          (Y,A) points at coefficient table
;          first byte of coeff. table is N
;          coefficients follow, highest power first
; 
;   P(x^2) computed using normal polynomial subroutine
POLYNOMIAL_ODD
            sta     SERPNT          ;save address of coefficient table
            sty     SERPNT+1
            jsr     STORE_FAC_IN_TEMP1_ROUNDED
            lda     #TEMP1          ;Y=0 already, so (Y,A) points at TEMP1
            jsr     FMULT           ;form x^2
            jsr     SERMAIN         ;do series in x^2
            lda     #TEMP1          ;get x again
            ldy     #>TEMP1
            jmp     FMULT           ;multiply x by P(x^2) and exit

; Normal polynomial subroutine
; 
;   P(x) = C(0)*x^n + C(1)*x^(n-1) + ... + C(n)
; 
;   where: x is value in FAC
;          (Y,A) points at coefficient table
;          first byte of coeff. table is N
;          coefficients follow, highest power first
POLYNOMIAL  sta     SERPNT          ;pointer to coefficient table
            sty     SERPNT+1
SERMAIN     jsr     STORE_FAC_IN_TEMP2_ROUNDED
            lda     (SERPNT),y      ;get N
            sta     SERLEN          ;save N
            ldy     SERPNT          ;bump ptr to highest coefficient
            iny                     ; and get ptr into (Y,A)
            tya
            bne     LEF85
            inc     SERPNT+1
LEF85       sta     SERPNT
            ldy     SERPNT+1
LEF89       jsr     FMULT           ;accumulate series terms
            lda     SERPNT          ;bump ptr to next coefficient
            ldy     SERPNT+1
            clc
            adc     #5
            bcc     LEF96
            iny
LEF96       sta     SERPNT
            sty     SERPNT+1
            jsr     FADD            ;add next coefficient
            lda     #TEMP2          ;point at x again
            ldy     #>TEMP2
            dec     SERLEN          ;if series not finished,
            bne     LEF89           ; then add another term
RTS_19      rts                     ;finished

CON_RND_1   byte   $98,$35,$44,$7a        ;<<< these are missing one byte >>>
CON_RND_2   byte   $68,$28,$b1,$46        ;<<< for fp values              >>>

;********************************************************************************
;* RND statement                                                                *
;********************************************************************************
RND         jsr     SIGN            ;reduce argument to -1, 0, or +1
            tax                     ;save argument
            bmi     LEFCC           ;= -1, use current argument for seed
            lda     #RNDSEED        ;use current seed
            ldy     #>RNDSEED
            jsr     LOAD_FAC_FROM_YA
            txa                     ;recall sign of argument
            beq     RTS_19          ;=0, return seed unchanged
            lda     #<CON_RND_1     ;very poor RND algorithm
            ldy     #>CON_RND_1
            jsr     FMULT
            lda     #<CON_RND_2     ;also, constants are truncated
            ldy     #>CON_RND_2     ;<<< this does nothing, due to small exponent >>>
            jsr     FADD
LEFCC       ldx     FAC+4           ;shuffle hi and lo bytes
            lda     FAC+1           ;to supposedly make it more random
            sta     FAC+4
            stx     FAC+1
            lda     #$00            ;make it positive
            sta     FAC_SIGN
            lda     FAC             ;a somewhat random extension
            sta     FAC_EXTENSION
            lda     #$80            ;exponent to make value < 1.0
            sta     FAC
            jsr     NORMALIZE_FAC_2
            ldx     #RNDSEED        ;move FAC to RNDSEED
            ldy     #>RNDSEED
GO_MOVMF    jmp     STORE_FAC_AT_YX_ROUNDED

;********************************************************************************
;* COS statement                                                                *
;********************************************************************************
COS         lda     #<CON_PI_HALF   ;cos(x)=sin(x + PI/2)
            ldy     #>CON_PI_HALF
            jsr     FADD
;********************************************************************************
;* SIN statement                                                                *
;********************************************************************************
SIN         jsr     COPY_FAC_TO_ARG_ROUNDED
            lda     #<CON_PI_DOUB   ;remove multiples of 2*PI
            ldy     #>CON_PI_DOUB   ; by dividing and saving
            ldx     ARG_SIGN        ; the fractional part
            jsr     DIV             ;use sign of argument
            jsr     COPY_FAC_TO_ARG_ROUNDED
            jsr     INT             ;take integer part
            lda     #$00            ;<<< wasted lines, because FSUBT >>>
            sta     SGNCPR          ;<<< changes SGNCPR again        >>>
            jsr     FSUBT           ;subtract to get fractional part
; FAC = angle as a fraction of a full circle
; 
; Now fold the range into a quarter circle.
; 
; <<< there are much simpler ways to do this >>>
            lda     #<QUARTER       ;1/4 - fraction makes
            ldy     #>QUARTER       ;-3/4 <= fraction < 1/4
            jsr     FSUB
            lda     FAC_SIGN        ;test sign of result
            pha                     ;save sign for later unfolding
            bpl     SIN_1           ;already 0...1/4
            jsr     FADDH           ;add 1/2 to shift to -1/4...1/2
            lda     FAC_SIGN        ;test sign
            bmi     SIN_2           ;-1/4...0
            lda     SIGNFLG         ;0...1/2 ; SIGNFLG initialized = 0 in TAN
            eor     #$ff            ; function
            sta     SIGNFLG         ;TAN is only user of SIGNFLG too
; if fall thru, range is 0...1/2
; if branch here, range is 0...1/4
SIN_1       jsr     NEGOP
; if fall thru, range is -1/2...0
; if branch here, range is -1/4...0
SIN_2       lda     #<QUARTER       ;add 1/4 to shift range
            ldy     #>QUARTER       ; to -1/4...1/4
            jsr     FADD
            pla                     ;get saved sign from above
            bpl     LF033
            jsr     NEGOP           ;make range 0...1/4
LF033       lda     #<POLY_SIN      ;do standard SIN series
            ldy     #>POLY_SIN
            jmp     POLYNOMIAL_ODD

;********************************************************************************
;* TAN statement                                                                *
;*                                                                              *
;* Compute TAN(x) = SIN(x) / COS(x)                                             *
;********************************************************************************
TAN         jsr     STORE_FAC_IN_TEMP1_ROUNDED
            lda     #$00            ;SIGNFLG will be toggled of 2nd or 3rd
            sta     SIGNFLG         ; quadrant
            jsr     SIN             ;get SIN(x)
            ldx     #TEMP3          ;save SIN(x) in TEMP3
            ldy     #>TEMP3
            jsr     GO_MOVMF        ;<<< funny way to call MOVMV! >>>
            lda     #TEMP1          ;retrieve x
            ldy     #>TEMP1
            jsr     LOAD_FAC_FROM_YA
            lda     #$00            ;and compute COS(x)
            sta     FAC_SIGN
            lda     SIGNFLG
            jsr     TAN_1           ;weird & dangerous way to get into SIN
            lda     #TEMP3          ;now form SIN/COS
            ldy     #>TEMP3
            jmp     FDIV

TAN_1       pha                     ;shame, shame!
            jmp     SIN_1

CON_PI_HALF byte   $81,$49,$0f,$da,$a2
CON_PI_DOUB byte   $83,$49,$0f,$da,$a2
QUARTER     byte   $7f,$00,$00,$00,$00
POLY_SIN    byte   5                        ;power of polynomial
            byte   $84,$e6,$1a,$2d,$1b      ;(2PI)^11/11!
            byte   $86,$28,$07,$fb,$f8      ;(2PI)^9/9!
            byte   $87,$99,$68,$89,$01      ;(2PI)^7/7!
            byte   $87,$23,$35,$df,$e1      ;(2PI)^5/5!
            byte   $86,$a5,$5d,$e7,$28      ;(2PI)^3/3!
            byte   $83,$49,$0f,$da,$a2      ;2PI
; <<< next 10 bytes are never referenced >>>
            byte   $a6,$d3,$c1,$c8,$d4,$c8,$d5,$c4,$ce,$ca  ;xor with $87 to get "MICROSOFT!"

;********************************************************************************
;* ATN statement                                                                *
;********************************************************************************
ATN         lda     FAC_SIGN        ;fold the argument range first
            pha                     ;save sign for later unfolding
            bpl     LF0A6           ;.ge. 0
            jsr     NEGOP           ;.lt. 0, so complement
LF0A6       lda     FAC             ;if .ge. 1, form reciprocal
            pha                     ;save for later unfolding
            cmp     #$81            ;exponent for .ge. 1
            bcc     LF0B4           ;x < 1
            lda     #<CON_ONE       ;form 1/x
            ldy     #>CON_ONE
            jsr     FDIV
; 0 <= x <= 1
; 0 <= ATN(x) <= PI/8
LF0B4       lda     #<POLY_ATN      ;compute polynomial approximation
            ldy     #>POLY_ATN
            jsr     POLYNOMIAL_ODD
            pla                     ;start to unfold
            cmp     #$81            ;was it .ge. 1?
            bcc     LF0C7           ;no
            lda     #<CON_PI_HALF   ;yes, subtract from PI/2
            ldy     #>CON_PI_HALF
            jsr     FSUB
LF0C7       pla                     ;was it negative?
            bpl     RTS_20          ;no
            jmp     NEGOP           ;yes, complement

RTS_20      rts

POLY_ATN    byte   11              ;power of polynomial
            byte   $76,$b3,$83,$bd,$d3
            byte   $79,$1e,$f4,$a6,$f5
            byte   $7b,$83,$fc,$b0,$10
            byte   $7c,$0c,$1f,$67,$ca
            byte   $7c,$de,$53,$cb,$c1
            byte   $7d,$14,$64,$70,$4c
            byte   $7d,$b7,$ea,$51,$7a
            byte   $7d,$63,$30,$88,$7e
            byte   $7e,$92,$44,$99,$3a
            byte   $7e,$4c,$cc,$91,$c7
            byte   $7f,$aa,$aa,$aa,$13
            byte   $81,$00,$00,$00,$00

; Generic copy of CHRGET subroutine, which is copied into $00B1...00C8 during
; initialization.
; 
; Cornelis Bongers described several improvements to CHRGET in Micro magazine or
; Call-A.P.P.L.E. (I don't remember which or exactly when).
GENERIC_CHRGET
            inc     TXTPTR
            bne     GENERIC_TXTPTR
            inc     TXTPTR+1
GENERIC_TXTPTR
            lda     LEA60           ;<<< actual address filled in later >>>
            cmp     #":"            ;EOS, also top of numeric range
            bcs     LF122           ;not number, might be EOS
            cmp     #" "            ;ignore blanks
            beq     GENERIC_CHRGET
            sec                     ;test for numeric range in way that
            sbc     #"0"            ; clears carry if char is digit
            sec                     ; and leaves char in A-reg
            sbc     #$d0            ;(should be #-'0')
LF122       rts

; Initial value for random number, also copied in along with CHRGET, but
; erroneously:
; <<< the last byte is not copied >>>
            byte   $80,$4f,$c7,$52,$58      ;approx. = .811635157

; Clear variables
;LASTPT      =    $53    ;Overlaps TEMPPT+1

COLD_START  ldx     #$ff            ;set direct mode flag
            stx     CURLIN+1
            ldx     #$fb            ;set stack pointer, leaving room for
            txs                     ; line buffer during parsing
            lda     #<COLD_START    ;set RESTART to COLD_START
            ldy     #>COLD_START    ; until cold start is completed
            sta     GOWARM+1
            sty     GOWARM+2
            sta     GOSTROUT+1      ;also second user vector...
            sty     GOSTROUT+2      ;...we simply must finish COLD_START!
            jsr     NORMAL          ;set normal display mode
            lda     #$4c            ;JMP opcode for 4 vectors
            sta     GOWARM          ;warm start
            sta     GOSTROUT        ;anyone ever use this one?
            sta     JMPADRS         ;used by functions (JMP JMPADRS)
            sta     USRVEC          ;USR function vector
            lda     #<IQERR         ;point USR to illegal quantity
            ldy     #>IQERR         ; error, until user sets it up
            sta     USRVEC+1
            sty     USRVEC+2
; Move generic CHRGET and random seed into place
; 
; <<< Note that loop value is wrong!  The last byte of the random seed is not
; copied into page zero! >>>
            ldx     #$1c            ;(should be #GENERIC_END-GENERIC_CHRGET-1)
LF152       lda     GENERIC_CHRGET-1,x
            sta     CHRGET-1,x
            stx     SPEEDZ          ;on last pass stores $01
            dex
            bne     LF152
; 
            stx     TRCFLG          ;X-reg=0, turn off tracing
            txa                     ;A-reg=0
            sta     SHIFT_SIGN_EXT
            sta     LASTPT+1
            pha                     ;put $00 on stack (what for?)
            lda     #3              ;set length of temp. string descriptors
            sta     DSCLEN          ;for garbage collection subroutine
            jsr     CRDO            ;print <return>
            lda     #$01            ;set up fake forward link
            sta     INPUT_BUFFER-3
            sta     INPUT_BUFFER-4
            ldx     #TEMPST         ;init index to temp string descriptors
            stx     TEMPPT
; Find high end of RAM
            lda     #$00            ;set up pointer to low end of RAM
            ldy     #$08
            sta     LINNUM
            sty     LINNUM+1
            ldy     #$00
LF181       inc     LINNUM+1        ;test first byte of each page
            lda     (LINNUM),y      ;by complementing it and watching
            eor     #$ff            ; it change the same way
            sta     (LINNUM),y
            cmp     (LINNUM),y      ;ROM or empty sockets won't track
            bne     LF195           ;not RAM here
            eor     #$ff            ;restore original value
            sta     (LINNUM),y
            cmp     (LINNUM),y      ;did it track again?
            beq     LF181           ;yes, still in RAM
LF195       ldy     LINNUM          ;no, end of RAM
            lda     LINNUM+1
            and     #$f0            ;force a multiple of 4096 bytes
            sty     MEMSIZE         ;(bad RAM may have yielded a non-multiple)
            sta     MEMSIZE+1
            sty     FRETOP          ;set HIMEM and bottom of strings
            sta     FRETOP+1
            ldx     #$00            ;set program pointer to $0800
            ldy     #$08
            stx     TEXTTAB
            sty     TEXTTAB+1
            ldy     #$00            ;turn off semi-secret LOCK flag
            sty     LOCK
            tya                     ;A-reg=0 too
            sta     (TEXTTAB),y     ;first byte in program space = 0
            inc     TEXTTAB         ;advance past the $00
            bne     LF1B8
            inc     TEXTTAB+1
LF1B8       lda     TEXTTAB
            ldy     TEXTTAB+1
            jsr     REASON          ;set rest of pointers up
            jsr     SCRTCH          ;more pointers
            lda     #<STROUT        ;put correct addresses in two
            ldy     #>STROUT        ; user vectors
            sta     GOSTROUT+1
            sty     GOSTROUT+2
            lda     #<RESTART
            ldy     #>RESTART
            sta     GOWARM+1
            sty     GOWARM+2
            jmp     (GOWARM+1)      ;silly, why not just "JMP RESTART"

;********************************************************************************
;* CALL statement                                                               *
;*                                                                              *
;* Effectively performs a JSR to the specified address, with the following      *
;* register contents:                                                           *
;*                                                                              *
;*   (A,Y) = call address                                                       *
;*   X-reg = $9D                                                                *
;*                                                                              *
;* The called routine can return with RTS, and Applesoft will continue with the *
;* next statement.                                                              *
;********************************************************************************
CALL        jsr     FRMNUM          ;evalute expression for CALL address
            jsr     GETADR          ;convert expression to 16-bit integer
            jmp     (LINNUM)        ; in LINNUM, and jump there

;********************************************************************************
;* IN# statement                                                                *
;*                                                                              *
;* Note: no check for valid slot #, as long as value is < 256 it is accepted.   *
;* Monitor masks value to 4 bits (0-15).                                        *
;********************************************************************************
IN_NUMBER   jsr     GETBYT          ;get slot number in X-reg
            txa                     ;monitor will install in vector
            jmp     MON_INPORT      ;at $38,39

;********************************************************************************
;* PR# statement                                                                *
;*                                                                              *
;* Note: no check for valid slot #, as long as value is < 256 it is accepted.   *
;* Monitor masks value to 4 bits (0-15).                                        *
;********************************************************************************
PR_NUMBER   jsr     GETBYT          ;get slot number in X-reg
            txa                     ;monitor will install in vector
            jmp     MON_OUTPORT     ;at $36,37

; Get two values < 48, with comma separator
; 
; Called for PLOT X,Y
;        and HLIN A,B at Y
;        and VLIN A,B at X
PLOTFNS     jsr     GETBYT          ;get first value in X-reg
            cpx     #48             ;must be < 48
            bcs     GOERR           ;too large
            stx     FIRST           ;save first value
            lda     #","            ;must have a comma
            jsr     SYNCHR
            jsr     GETBYT          ;get second value in X-reg
            cpx     #48             ;must be < 48
            bcs     GOERR           ;too large
            stx     MON_H2          ;save second value
            stx     MON_V2
            rts                     ;second value still in X-reg

GOERR       jmp     IQERR           ;illegal quantity error

; Get "A,B at C" values for HLIN and VLIN
; 
; Put smaller of (A,B) in FIRST, and larger of (A,B) in H2 and V2.  Return with
; X-reg = C-value.
LINCOOR     jsr     PLOTFNS         ;get A,B values
            cpx     FIRST           ;is A < B?
            bcs     LF218           ;yes, in right order
            lda     FIRST           ;no, interchange them
            sta     MON_H2
            sta     MON_V2
            stx     FIRST
LF218       lda     #TOK_AT         ;must have AT next
            jsr     SYNCHR
            jsr     GETBYT          ;get C-value in X-reg
            cpx     #48             ;must be < 48
            bcs     GOERR           ;too large
            rts                     ;C-value in X-reg

;********************************************************************************
;* PLOT statement                                                               *
;********************************************************************************
PLOT        jsr     PLOTFNS         ;get X,Y values
            txa                     ;Y-coord to A-reg for monitor
            ldy     FIRST           ;X-coord to Y-reg for monitor
            cpy     #40             ;X-coord must be < 40
            bcs     GOERR           ;X-coord is too large
            jmp     MON_PLOT        ;plot!

;********************************************************************************
;* HLIN statement                                                               *
;********************************************************************************
HLIN        jsr     LINCOOR         ;get "A,B at C"
            txa                     ;Y-coord in A-reg
            ldy     MON_H2          ;right end of line
            cpy     #40             ;must be < 40
            bcs     GOERR           ;too large
            ldy     FIRST           ;left end of line in Y-reg
            jmp     MON_HLINE       ;let monitor draw line

;********************************************************************************
;* VLIN statement                                                               *
;********************************************************************************
VLIN        jsr     LINCOOR         ;get "A,B at C"
            txa                     ;X-coord in Y-reg
            tay
            cpy     #40             ;X-coord must be < 40
            bcs     GOERR           ;too large
            lda     FIRST           ;top end of line in A-reg
            jmp     MON_VLINE       ;let monitor draw line

;********************************************************************************
;* COLOR= statement                                                             *
;********************************************************************************
COLOR       jsr     GETBYT          ;get color value in X-reg
            txa
            jmp     MON_SETCOL      ;let monitor store color

;********************************************************************************
;* VTAB statement                                                               *
;********************************************************************************
VTAB        jsr     GETBYT          ;get line # in X-reg
            dex                     ;convert to zero base
            txa
            cmp     #24             ;must be 0-23
            bcs     GOERR           ;too large, or was "VTAB 0"
            jmp     MON_TABV        ;let monitor compute base

;********************************************************************************
;* SPEED= statement                                                             *
;********************************************************************************
SPEED       jsr     GETBYT          ;get speed setting in X-reg
            txa                     ;SPEEDZ = $100 - speed
            eor     #$ff            ;so "SPEED=255" is fastest
            tax
            inx
            stx     SPEEDZ
            rts

;********************************************************************************
;* TRACE statement                                                              *
;*                                                                              *
;* Set sign bit in TRCFLG.                                                      *
;********************************************************************************
TRACE       sec
            byte    $90             ;bcc    HIMEM+2         ;fake BCC to skip next opcode
;********************************************************************************
;* NOTRACE statement                                                            *
;********************************************************************************
NOTRACE     clc
            ror     TRCFLG          ;shift carry into TRCFLG
            rts

;********************************************************************************
;* NORMAL statement                                                             *
;********************************************************************************
NORMAL      lda     #$ff            ;set INVFLG = $FF
            bne     N_I_            ;and FLASH_BIT = $00

;********************************************************************************
;* INVERSE statement                                                            *
;********************************************************************************
INVERSE     lda     #$3f            ;set INVFLG = $3F
N_I_        ldx     #$00            ;and FLASH_BIT = $00
N_I_F_      sta     MON_INVFLAG
            stx     FLASH_BIT
            rts

;********************************************************************************
;* FLASH statement                                                              *
;********************************************************************************
FLASH       lda     #$7f            ;set INVFLG = $7F
            ldx     #$40            ;and FLASH_BIT = $40
            bne     N_I_F_          ;...always

;********************************************************************************
;* HIMEM: statement                                                             *
;********************************************************************************
HIMEM       jsr     FRMNUM          ;get value specified for HIMEM
            jsr     GETADR          ; as 16-bit integer
            lda     LINNUM          ;must be above variables and arrays
            cmp     STREND
            lda     LINNUM+1
            sbc     STREND+1
            bcs     SETHI           ;it is above them
JMM         jmp     MEMERR          ;not enough memory

SETHI       lda     LINNUM          ;store new HIMEM: value
            sta     MEMSIZE
            sta     FRETOP          ;<<<note that HIMEM: does not>>>
            lda     LINNUM+1        ;<<<clear string variables.  >>>
            sta     MEMSIZE+1       ;<<<this could be disastrous.>>>
            sta     FRETOP+1
            rts

;********************************************************************************
;* LOMEM: statement                                                             *
;********************************************************************************
LOMEM       jsr     FRMNUM          ;get value specified for LOMEM
            jsr     GETADR          ; as 16-bit integer in LINNUM
            lda     LINNUM          ;must be below HIMEM
            cmp     MEMSIZE
            lda     LINNUM+1
            sbc     MEMSIZE+1
            bcs     JMM             ;above HIMEM, memory error
            lda     LINNUM          ;must be above program
            cmp     VARTAB
            lda     LINNUM+1
            sbc     VARTAB+1
            bcc     JMM             ;not above program, error
            lda     LINNUM          ;store new LOMEM value
            sta     VARTAB
            lda     LINNUM+1
            sta     VARTAB+1
            jmp     CLEARC          ;LOMEM clears variables and arrays

;********************************************************************************
;* ONERR statement                                                              *
;********************************************************************************
ONERR       lda     #TOK_GOTO       ;must be GOTO next
            jsr     SYNCHR
            lda     TXTPTR          ;save TXTPTR for HANDLERR
            sta     TXTPSV
            lda     TXTPTR+1
            sta     TXTPSV+1
            sec                     ;set sign bit of ERRFLG
            ror     ERRFLG
            lda     CURLIN          ;save line # of current line
            sta     CURLSV
            lda     CURLIN+1
            sta     CURLSV+1
            jsr     REMN            ;ignore rest of line <<<why?>>>
            jmp     ADDON           ;continue program

; Routine to handle errors if ONERR GOTO active.
HANDLERR    stx     ERRNUM          ;save error code number
            ldx     REMSTK          ;get stack ptr saved at NEWSTT
            stx     ERRSTK          ;remember it
; <<<could also have done TXS here; see ONERR correction in Applesoft manual.>>>
            lda     CURLIN          ;get line # of offending statement
            sta     ERRLIN          ;so user can see it if desired
            lda     CURLIN+1
            sta     ERRLIN+1
            lda     OLDTEXT         ;also the position in the line
            sta     ERRPOS          ;in case user wants to RESUME
            lda     OLDTEXT+1
            sta     ERRPOS+1
            lda     TXTPSV          ;set up TXTPTR to read target line #
            sta     TXTPTR          ;in "ON ERR GO TO xxxx"
            lda     TXTPSV+1
            sta     TXTPTR+1
            lda     CURLSV          ;line # of "ON ERR" statement
            sta     CURLIN
            lda     CURLSV+1
            sta     CURLIN+1
            jsr     CHRGOT          ;start conversion
            jsr     GOTO            ;goto specified ONERR line
            jmp     NEWSTT

;********************************************************************************
;* RESUME statement                                                             *
;********************************************************************************
RESUME      lda     ERRLIN          ;restore line # and TXTPTR
            sta     CURLIN          ; to re-try offending line
            lda     ERRLIN+1
            sta     CURLIN+1
            lda     ERRPOS
            sta     TXTPTR
            lda     ERRPOS+1
            sta     TXTPTR+1
; <<< ONERR correction in manual is easily by CALL -3288, which is $F328 here.
; >>>
            ldx     ERRSTK          ;retrieve stack ptr as it was
            txs                     ; before statement scanned
            jmp     NEWSTT          ;do statement again

JSYN        jmp     SYNERR

;********************************************************************************
;* DEL statement                                                                *
;********************************************************************************
; Clear variables

DEL         bcs     JSYN            ;error if # not specified
            ldx     PRGEND
            stx     VARTAB
            ldx     PRGEND+1
            stx     VARTAB+1
            jsr     LINGET          ;get beginning of range
            jsr     FNDLIN          ;find this line or next
            lda     LOWTR           ;upper portion of program will
            sta     DEST            ;be moved down to here
            lda     LOWTR+1
            sta     DEST+1
            lda     #","            ;must have a comma next
            jsr     SYNCHR
            jsr     LINGET          ;get end range (does nothing if end range is not specified)
            inc     LINNUM          ;point one past it
            bne     LF357
            inc     LINNUM+1
LF357       jsr     FNDLIN          ;find start line after specified line
            lda     LOWTR           ;which is beginning of portion
            cmp     DEST            ;to be moved down
            lda     LOWTR+1         ;it must be above the target
            sbc     DEST+1
            bcs     LF365           ;it is okay
            rts                     ;nothing to delete

LF365       ldy     #$00            ;move upper portion down now
LF367       lda     (LOWTR),y       ;source...
            sta     (DEST),y        ;...to destination
            inc     LOWTR           ;bump source ptr
            bne     LF371
            inc     LOWTR+1
LF371       inc     DEST            ;bump destination ptr
            bne     LF377
            inc     DEST+1
LF377       lda     VARTAB          ;reached end of program yet?
            cmp     LOWTR
            lda     VARTAB+1
            sbc     LOWTR+1
            bcs     LF367           ;no, keep moving
            ldx     DEST+1          ;store new end of program
            ldy     DEST            ;must subtract 1 first
            bne     LF388
            dex
LF388       dey
            stx     VARTAB+1
            sty     VARTAB
            jmp     FIX_LINKS       ;reset links after a delete

;********************************************************************************
;* GR statement                                                                 *
;********************************************************************************
GR          lda     LORES
            lda     MIXSET
            jmp     MON_SETGR

;********************************************************************************
;* TEXT statement                                                               *
;*                                                                              *
;* <<< better code would be:                                                    *
;*   LDA MIXSET                                                                 *
;*   JMP $FB33                                                                  *
;* >>>                                                                          *
;********************************************************************************
TEXT        lda     TXTPAGE1        ;JMP $FB36 would have
            jmp     MON_SETTXT      ; done both of these

;********************************************************************************
;* STORE statement                                                              *
;********************************************************************************
STORE       jsr     GETARYPT        ;get address of array to be saved
            ldy     #$03            ;forward offset - 1 is size of
            lda     (LOWTR),y       ; this array
            tax
            dey
            lda     (LOWTR),y
            sbc     #$01
            bcs     LF3AF
            dex
LF3AF       sta     LINNUM
            stx     LINNUM+1
            jsr     MON_WRITE
            jsr     TAPEPNT
            jmp     MON_WRITE

;********************************************************************************
;* RECALL statement                                                             *
;********************************************************************************
RECALL      jsr     GETARYPT        ;find array in memory
            jsr     MON_READ        ;read header
            ldy     #$02            ;make sure the new data fits
            lda     (LOWTR),y
            cmp     LINNUM
            iny
            lda     (LOWTR),y
            sbc     LINNUM+1
            bcs     LF3D2           ;it fits
            jmp     MEMERR          ;doesn't fit

LF3D2       jsr     TAPEPNT         ;read the data
            jmp     MON_READ

;********************************************************************************
;* HGR2 statement                                                               *
;********************************************************************************
HGR2        bit     TXTPAGE2        ;select page 2 ($4000-5FFF)
            bit     MIXCLR          ;default to full screen
            lda     #$40            ;set starting page for hi-res
            bne     SETHPG          ;...always

;********************************************************************************
;* HGR statement                                                                *
;********************************************************************************
HGR         lda     #$20            ;set starting page for hi-res
            bit     TXTPAGE1        ;select page 1 ($2000-3FFF)
            bit     MIXSET          ;default to mixed screen
SETHPG      sta     HGR_PAGE        ;base page of hi-res buffer
            lda     HIRES           ;turn on hi-res
            lda     TXTCLR          ;turn on graphics
; Clear screen.
            lda     #$00            ;set for black background
            sta     HGR_BITS
; Fill screen with HGR_BITS.
BKGND       lda     HGR_PAGE        ;put buffer address in HGR_SHAPE
            sta     HGR_SHAPE+1
            ldy     #$00
            sty     HGR_SHAPE
LF3FE       lda     HGR_BITS        ;color byte
            sta     (HGR_SHAPE),y   ;clear hi-res to HGR_BITS
            jsr     COLOR_SHIFT     ;correct for color shift
            iny                     ;(slows clear by factor of 2)
            bne     LF3FE
            inc     HGR_SHAPE+1
            lda     HGR_SHAPE+1
            and     #$1f            ;done? ($40 or $60)
            bne     LF3FE           ;no
            rts                     ;yes, return

; Set the hi-res cursor position.
; 
;   (Y,X) = horizontal coordinate (0-279)
;   A-reg = vertical coordinate   (0-191)
HPOSN       sta     HGR_Y           ;save Y- and X-positions
            stx     HGR_X
            sty     HGR_X+1
            pha                     ;Y-pos also on stack
            and     #$c0            ;calculate base address for Y-pos
            sta     HBASL           ;for Y=ABCDEFGH
            lsr     A               ;HBASL=ABAB0000
            lsr     A
            ora     HBASL
            sta     HBASL
            pla                     ;     A        HBASH     HBASL
            sta     HBASH           ;?-ABCDEFGH  ABCDEFGH  ABAB0000
            asl     A               ;A-BCDEFGH0  ABCDEFGH  ABAB0000
            asl     A               ;B-CDEFGH00  ABCDEFGH  ABAB0000
            asl     A               ;C-DEFGH000  ABCDEFGH  ABAB0000
            rol     HBASH           ;A-DEFGH000  BCDEFGHC  ABAB0000
            asl     A               ;D-EFGH0000  BCDEFGHC  ABAB0000
            rol     HBASH           ;B-EFGH0000  CDEFGHCD  ABAB0000
            asl     A               ;E-FGH00000  CDEFGHCD  ABAB0000
            ror     HBASL           ;0-FGH00000  CDEFGHCD  EABAB000
            lda     HBASH           ;0-CDEFGHCD  CDEFGHCD  EABAB000
            and     #$1f            ;0-000FGHCD  CDEFGHCD  EABAB000
            ora     HGR_PAGE        ;0-PPPFGHCD  CDEFGHCD  EABAB000
            sta     HBASH           ;0-PPPFGHCD  PPPFGHCD  EABAB000
            txa                     ;divide X-pos by 7 for index from base
            cpy     #$00            ;is X-pos < 256?
            beq     LF442           ;yes
; no: 256/7 = 36 rem 4
; carry=1, so ADC #4 is too large; however, ADC #4 clears carry which makes SBC
; #7 only -6, balancing it out.
            ldy     #35
            adc     #$04            ;following INY makes Y=36
LF441       iny
LF442       sbc     #$07
            bcs     LF441
            sty     HGR_HORIZ       ;horizontal index
            tax                     ;use remainder-7 to look up the
            lda     MSKTBL-249,x    ; bit mask (should be MSKTBL-$100+7,X)
            sta     HMASK
            tya                     ;quotient gives byte index
            lsr     A               ;odd or even column?
            lda     HGR_COLOR       ;if on odd byte (carry set)
            sta     HGR_BITS        ; then rotate bits
            bcs     COLOR_SHIFT     ;odd column
            rts                     ;even column

; Plot a dot
; 
;   (Y,X) = horizontal position
;   A-reg = vertical position
HPLOT0      jsr     HPOSN
            lda     HGR_BITS        ;calculate bit posn in GBAS,
            eor     (HBASL),y       ; HGR_HORIZ, and HMASK from
            and     HMASK           ; Y-coord in A-reg,
            eor     (HBASL),y       ; X-coord in X,Y regs.
            sta     (HBASL),y       ;for any 1-bits, substitute
            rts                     ; corresponding bit of HGR_BITS

; Move left or right one pixel.
; 
; If status is +, move right; if -, move left
; If already at left or right edge, wrap around
; 
; Remember bits in hi-res byte are backwards order:
;   byte N  byte N+1
; S7654321  SEDCBA98
MOVE_LEFT_OR_RIGHT
            bpl     MOVE_RIGHT      ;+ move right, - move left
            lda     HMASK           ;move left one pixel
            lsr     A               ;shift mask right, moves dot left
            bcs     LR_2            ;...dot moved to next byte
            eor     #$c0            ;move sign bit back where it was
LR_1        sta     HMASK           ;new mask value
            rts

LR_2        dey                     ;moved to next byte, so decr index
            bpl     LR_3            ;still not past edge
            ldy     #39             ;off left edge, so wrap around screen
LR_3        lda     #$c0            ;new HMASK, rightmost bit on screen
LR_4        sta     HMASK           ;new mask and index
            sty     HGR_HORIZ
            lda     HGR_BITS        ;also need to rotate color
; 
COLOR_SHIFT asl     A               ;rotate low-order 7 bits
            cmp     #$c0            ; of HGR_BITS one bit posn
            bpl     LF489
            lda     HGR_BITS
            eor     #$7f
            sta     HGR_BITS
LF489       rts

; Move right one pixel.
; 
; If already at right edge, wrap around.
MOVE_RIGHT  lda     HMASK
            asl     A               ;shifting byte left moves pixel right
            eor     #$80
; Original:  C0 A0 90 88 84 82 81
; Shifted:   80 40 20 10 08 02 01
; EOR #$80:  00 C0 A0 90 88 84 82
            bmi     LR_1            ;finished
            lda     #$81            ;new mask value
            iny                     ;move to next byte right
            cpy     #40             ;unless that is too far
            bcc     LR_4            ;not too far
            ldy     #$00            ;too far, so wrap around
            bcs     LR_4            ;...always

; "XDRAW" one bit
LRUDX1      clc                     ;C=0 means no 90 degree rotation
LRUDX2      lda     HGR_DX+1        ;C=1 means rotate 90 degrees
            and     #$04            ;if bit2=0 then don't plot
            beq     LRUD4           ;yes, do not plot
            lda     #$7f            ;no, look at what is already there
            and     HMASK
            and     (HBASL),y       ;screen bit = 1?
            bne     LRUD3           ;yes, go clear it
            inc     HGR_COLLISIONS  ;no, count the collision
            lda     #$7f            ;and turn the bit on
            and     HMASK
            bpl     LRUD3           ;...always

; "DRAW" one bit
LRUD1       clc                     ;C=0 means no 90 degree rotation
LRUD2       lda     HGR_DX+1        ;C=1 means rotate
            and     #$04            ;if bit2=0 then do not plot
            beq     LRUD4           ;do not plot
            lda     (HBASL),y
            eor     HGR_BITS        ;1's where any bits not in color
            and     HMASK           ;look at just this bit position
            bne     LRUD3           ;the bit was zero, so plot it
            inc     HGR_COLLISIONS  ;bit is already 1; count collsn
; Toggle bit on screen with A-reg.
LRUD3       eor     (HBASL),y
            sta     (HBASL),y
; Determine where next point will be, and move there.
; 
;   C=0 if no 90 degree rotation
;   C=1 rotates 90 degrees
LRUD4       lda     HGR_DX+1        ;calculate the direction to move
            adc     HGR_QUAD
CON_03      and     #$03            ;wrap around the circle
;   00 - up
;   01 - down
;   10 - right
;   11 - left
            cmp     #$02            ;C=0 if 0 or 1, C=1 if 2 or 3
            ror     A               ;put C into sign, odd/even into C
            bcs     MOVE_LEFT_OR_RIGHT
; 
MOVE_UP_OR_DOWN
            bmi     MOVE_DOWN       ;sign for up/down select
; Move up one pixel
; 
; If already at top, go to bottom.
; 
; Remember:  Y-coord   HBASH     HBASL
;           ABCDEFGH  PPPFGHCD  EABAB000
            clc                     ;move up
            lda     HBASH           ;calc base address of prev line
            bit     CON_1C          ;look at bits 000FGH00 in HBASH
            bne     LF4FF           ;simple , just FGH=FGH-1; GBASH=PPP000CD, GBASL=EABAB000
            asl     HBASL           ;what is "E"?
            bcs     LF4FB           ;E=1, then EFGH=EFGH-1
            bit     CON_03+1        ;look at 000000CD in HBASH
            beq     LF4EB           ;Y-pos is AB000000 form
            adc     #$1f            ;CD <> 0, so CDEFGH=CDEFGH-1
            sec
            bcs     LF4FD           ;...always

LF4EB       adc     #$23            ;enough to make HBASH=PPP11111 later
            pha                     ;save for later
            lda     HBASL           ;HBASL is now ABAB0000 (AB=00,01,10)
;    0000+1011=1011 and carry clear
; or 0101+1011=0000 and carry set
; or 1010+1011=0101 and carry set
            adc     #$b0
            bcs     LF4F6           ;no wrap-around needed
            adc     #$f0            ;change 1011 to 1010 (wrap-around)
LF4F6       sta     HBASL           ;form is now still ABAB0000
            pla                     ;partially modified HBASH
            bcs     LF4FD           ;...always

LF4FB       adc     #$1f
LF4FD       ror     HBASL           ;shift in E, to get EABAB000 form
LF4FF       adc     #$fc            ;finish HBASH mods
UD_1        sta     HBASH
            rts

            byte    $18             ;<<< never used >>>

; Move down one pixel
; 
; If already at bottom, go to top.
; 
; Remember:  Y-coord   HBASH     HBASL
;           ABCDEFGH  PPPFGHCD  EABAB000
MOVE_DOWN   lda     HBASH           ;try it first, by FGH=FGH+1
CON_04      adc     #$04            ;HBASH = PPPFGHCD
            bit     CON_1C          ;is FGH field now zero?
            bne     UD_1            ;no so we are finished
            asl     HBASL           ;yes, ripple the carry as high as necessary; look at "E" bit
            bcc     LF52A           ;now zero; make it 1 and leave
            adc     #$e0            ;carry = 1, so adds $E1
            clc                     ;is "CD" not zero?
            bit     CON_04+1        ;tests bit 2 for carry out of "CD"
            beq     LF52C           ;no carry, finished
; increment "AB" then
; 0000 --> 0101
; 0101 --> 1010
; 1010 --> wrap around to line 0
            lda     HBASL           ;0000  0101  1010
            adc     #$50            ;0101  1010  1111
            eor     #$f0            ;1010  0101  0000
            beq     LF524
            eor     #$f0            ;0101  1010
LF524       sta     HBASL           ;new ABAB0000
            lda     HGR_PAGE        ;wrap around to line zero of group
            bcc     LF52C           ;...always
LF52A       adc     #$e0
LF52C       ror     HBASL
            bcc     UD_1            ;...always

; HLINRL
; (never called by Applesoft)
; 
; Enter with: (A,X) = DX from current point
;             Y-reg = DY from current point
            pha                     ;save A-reg
            lda     #$00            ;clear current point so HGLIN will
            sta     HGR_X           ; act relatively
            sta     HGR_X+1
            sta     HGR_Y
            pla                     ;restore A-reg
; Draw line from last plotted point to (A,X),Y
; 
; Enter with: (A,X) = X of target point
;             Y-reg = Y of target point
HGLIN       pha                     ;compute DX = X - X0
            sec
            sbc     HGR_X
            pha
            txa
            sbc     HGR_X+1
            sta     HGR_QUAD        ;save DX sign (+ = right, - = left)
            bcs     LF550           ;now find abs(DX)
            pla                     ;forms 2's complement
            eor     #$ff
            adc     #$01
            pha
            lda     #$00
            sbc     HGR_QUAD
LF550       sta     HGR_DX+1
            sta     HGR_E+1         ;init HGR_E to abs(X-X0)
            pla
            sta     HGR_DX
            sta     HGR_E
            pla
            sta     HGR_X           ;target X point
            stx     HGR_X+1
            tya                     ;target Y point
            clc                     ;compute DY = Y - HGR_Y
            sbc     HGR_Y           ; and save -abs(Y - HGR_Y) - 1 in HGR_DY
            bcc     LF568           ;(so + means up, - means down)
            eor     #$ff            ;2's complement of DY
            adc     #$fe
LF568       sta     HGR_DY
            sty     HGR_Y           ;target Y point
            ror     HGR_QUAD        ;shift Y-direction into quadrant
            sec                     ;count = DX - (-DY) = # of dots needed
            sbc     HGR_DX
            tax                     ;countl is in X-reg
            lda     #$ff
            sbc     HGR_DX+1
            sta     HGR_COUNT
            ldy     HGR_HORIZ       ;horizontal index
            bcs     MOVEX2          ;...always

; Move left or right one pixel.  A-reg bit 6 has direction.
MOVEX       asl     A               ;put bit 6 into sign position
            jsr     MOVE_LEFT_OR_RIGHT
            sec
; Draw line now.
MOVEX2      lda     HGR_E           ;carry is set
            adc     HGR_DY          ;E = E - deltaY
            sta     HGR_E           ;note: DY is (-delta Y)-1
            lda     HGR_E+1         ;carry clr if HGR_E goes negative
            sbc     #$00
LF58B       sta     HGR_E+1
            lda     (HBASL),y
            eor     HGR_BITS        ;plot a dot
            and     HMASK
            eor     (HBASL),y
            sta     (HBASL),y
            inx                     ;finished all the dots?
            bne     LF59E           ;no
            inc     HGR_COUNT       ;test rest of count
            beq     RTS_22          ;yes, finished
LF59E       lda     HGR_QUAD        ;test direction
            bcs     MOVEX           ;next move is in the X direction
            jsr     MOVE_UP_OR_DOWN ;if clr, neg, move
            clc                     ;E = E + DX
            lda     HGR_E
            adc     HGR_DX
            sta     HGR_E
            lda     HGR_E+1
            adc     HGR_DX+1
            bvc     LF58B           ;...always

MSKTBL      byte   $81,$82,$84,$88,$90,$a0,$c0
CON_1C      byte   $1c             ;mask for "FGH" bits
; Table of COS(90*x/16 degrees)*$100 - 1, with one-byte precision, X=0 to 16
COSINE_TABLE
            byte   $ff,$fe,$fa,$f4,$ec,$e1,$d4,$c5,$b4,$a1,$8d,$78,$61,$49,$31,$18,$ff

; HFIND - calculates current position of hi-res cursor
; (not called by any Applesoft routine)
; 
; Calculate Y-coord from HBASH,L
;       and X-coord from HORIZ and HMASK
            lda     HBASL           ;HBASL = EABAB000
            asl     A               ;E into carry
            lda     HBASH           ;HBASH = PPPFGHCD
            and     #$03            ;000000CD
            rol     A               ;00000CDE
            ora     HBASL           ;EABABCDE
            asl     A               ;ABABCDE0
            asl     A               ;BABCDE00
            asl     A               ;ABCDE000
            sta     HGR_Y           ;all but FGH
            lda     HBASH           ;PPPFGHCD
            lsr     A               ;0PPPFGHC
            lsr     A               ;00PPPFGH
            and     #$07            ;00000FGH
            ora     HGR_Y           ;ABCDEFGH
            sta     HGR_Y           ;that takes care of Y-coordinate
            lda     HGR_HORIZ       ;X = 7*HORIZ + bit pos in HMASK
            asl     A               ;multiply by 7
            adc     HGR_HORIZ       ;3* so far
            asl     A               ;6*
            tax                     ;since 7* might not fit in 1 byte,
            dex                     ; wait till later for last add
            lda     HMASK           ;now find bit position in HMASK
            and     #$7f            ;only look at low seven
LF5F0       inx                     ;count a shift
            lsr     A
            bne     LF5F0           ;still in there
            sta     HGR_X+1         ;zero to hi byte
            txa                     ;6*HORIZ + log2(HMASK)
            clc                     ;add HORIZ one more time
            adc     HGR_HORIZ       ;7*HORIZ + log2(HMASK)
            bcc     LF5FE           ;upper byte = 0
            inc     HGR_X+1         ;upper byte = 1
LF5FE       sta     HGR_X           ;store lower byte
RTS_22      rts

; DRAW0
; (not called by Applesoft)
            stx     HGR_SHAPE       ;save shape address
            sty     HGR_SHAPE+1
; Draw a shape
; 
;   (Y,X) = shape starting address
;   A-reg = rotation ($00-3F)
DRAW1       tax                     ;save rotation ($00-3F)
            lsr     A               ;divide rotation by 16 to get
            lsr     A               ; quadrant (0=up, 1=rt, 2=dwn, 3=lft)
            lsr     A
            lsr     A
            sta     HGR_QUAD
            txa                     ;use low 4 bits of rotation to index
            and     #$0f            ; the trig table
            tax
            ldy     COSINE_TABLE,x  ;save cosine in HGR_DX
            sty     HGR_DX
            eor     #$0f            ;and sine in DY
            tax
            ldy     COSINE_TABLE+1,x
            iny
            sty     HGR_DY
            ldy     HGR_HORIZ       ;index from HBASL,H to byte we're in
            ldx     #$00
            stx     HGR_COLLISIONS  ;clear collision counter
            lda     (HGR_SHAPE,x)   ;get first byte of shape defn
LF626       sta     HGR_DX+1        ;keep shape byte in HGR_DX+1
            ldx     #$80            ;initial values for fractional vectors
            stx     HGR_E           ;.5 in cosine component
            stx     HGR_E+1         ;.5 in sine component
            ldx     HGR_SCALE       ;scale factor
LF630       lda     HGR_E           ;add cosine value to X-value
            sec                     ;if >= 1, then draw
            adc     HGR_DX
            sta     HGR_E           ;only save fractional part
            bcc     LF63D           ;no integral part
            jsr     LRUD1           ;time to plot cosine component
            clc
LF63D       lda     HGR_E+1         ;add sine value to Y-value
            adc     HGR_DY          ;if >= 1, then draw
            sta     HGR_E+1         ;only save fractional part
            bcc     LF648           ;no integral part
            jsr     LRUD2           ;time to plot sine component
LF648       dex                     ;loop on scale factor
            bne     LF630           ;still on same shape item
            lda     HGR_DX+1        ;get next shape item
            lsr     A               ;next 3-bit vector
            lsr     A
            lsr     A
            bne     LF626           ;more in this shape byte
            inc     HGR_SHAPE       ;go to next shape byte
            bne     LF658
            inc     HGR_SHAPE+1
LF658       lda     (HGR_SHAPE,x)   ;next byte of shape definition
            bne     LF626           ;process if not zero
            rts                     ;finished

; XDRAW0
; (not called by Applesoft)
            stx     HGR_SHAPE       ;save shape address
            sty     HGR_SHAPE+1
; XDRAW a shape (same as DRAW, except toggles screen)
; 
;   (Y,X) = shape starting address
;   A-reg = rotation ($00-3F)
XDRAW1      tax                     ;save rotation ($00-3F)
            lsr     A               ;divide rotation by 16 to get
            lsr     A               ; quadrant (0=up, 1=rt, 2=dwn, 3=lft)
            lsr     A
            lsr     A
            sta     HGR_QUAD
            txa                     ;use lwo 4 bits of rotation to index
            and     #$0f            ; the trig table
            tax
            ldy     COSINE_TABLE,x  ;save cosine in HGR_DX
            sty     HGR_DX
            eor     #$0f            ;and sine in DY
            tax
            ldy     COSINE_TABLE+1,x
            iny
            sty     HGR_DY
            ldy     HGR_HORIZ       ;index from HBASL,H to byte we're in
            ldx     #$00
            stx     HGR_COLLISIONS  ;clear collision counter
            lda     (HGR_SHAPE,x)   ;get first byte of shape defn
LF682       sta     HGR_DX+1        ;keep shape byte in HGR_DX+1
            ldx     #$80            ;initial values for fractional vectors
            stx     HGR_E           ;.5 in cosine component
            stx     HGR_E+1         ;.5 in sine component
            ldx     HGR_SCALE       ;scale factor
LF68C       lda     HGR_E           ;add cosine value to X-value
            sec                     ;if >= 1, then draw
            adc     HGR_DX
            sta     HGR_E           ;only save fractional part
            bcc     LF699           ;no integral part
            jsr     LRUDX1          ;time to plot cosine component
            clc
LF699       lda     HGR_E+1         ;add sine value to Y-value
            adc     HGR_DY          ;if >= 1, then draw
            sta     HGR_E+1         ;only save fractional part
            bcc     LF6A4           ;no integral part
            jsr     LRUDX2          ;time to plot sine component
LF6A4       dex                     ;loop on scale factor
            bne     LF68C           ;still on same shape item
            lda     HGR_DX+1        ;get next shape item
            lsr     A               ;next 3-bit vector
            lsr     A
            lsr     A
            bne     LF682           ;more in this shape byte
            inc     HGR_SHAPE       ;go to next shape byte
            bne     LF6B4
            inc     HGR_SHAPE+1
LF6B4       lda     (HGR_SHAPE,x)   ;next byte of shape definition
            bne     LF682           ;process if not zero
            rts                     ;finished

; Get hi-res plotting coordinates (0-279,0-191) from TXTPTR.  Leave registers
; set up for HPOSN:
; 
;   (Y,X) = X-coord
;   A-reg = Y-coord
HFNS        jsr     FRMNUM          ;evaluate expression, must be numeric
            jsr     GETADR          ;convert to 2-byte integer in LINNUM
            ldy     LINNUM+1        ;get horiz coord in X,Y
            ldx     LINNUM
            cpy     #$01            ;(should be #>280) make sure it is < 280
            bcc     LF6CD           ;in range
            bne     GGERR
            cpx     #24             ;(should be #<280)
            bcs     GGERR
LF6CD       txa                     ;save horiz coord on stack
            pha
            tya
            pha
            lda     #","            ;require a comma
            jsr     SYNCHR
            jsr     GETBYT          ;eval exp to single byte in X-reg
            cpx     #192            ;check for range
            bcs     GGERR           ;too big
            stx     FAC             ;save Y-coord
            pla                     ;retrieve horizontal coordinate
            tay
            pla
            tax
            lda     FAC             ;and vertical coordinate
            rts

GGERR       jmp     GOERR           ;illegal quantity error

;********************************************************************************
;* HCOLOR= statement                                                            *
;********************************************************************************
HCOLOR      jsr     GETBYT          ;eval exp to single byte in X
            cpx     #8              ;value must be 0-7
            bcs     GGERR           ;too big
            lda     COLORTBL,x      ;get color pattern
            sta     HGR_COLOR
RTS_23      rts

COLORTBL    byte   $00,$2a,$55,$7f,$80,$aa,$d5,$ff

;********************************************************************************
;* HPLOT statement                                                              *
;*                                                                              *
;*   HPLOT X,Y                                                                  *
;*   HPLOT TO X,Y                                                               *
;*   HPLOT X1,Y1 to X2,Y2                                                       *
;********************************************************************************
; Clear variables
DSCTMP      =    $9d    ;Overlaps FAC

HPLOT       cmp     #TOK_TO         ;HPLOT TO form?
            beq     LF70F           ;yes, start from current location
            jsr     HFNS            ;no, get starting point of line
            jsr     HPLOT0          ;plot the point, and set up for drawing a line from that point
LF708       jsr     CHRGOT          ;character at end of expression
            cmp     #TOK_TO         ;is a line specified?
            bne     RTS_23          ;no, exit
LF70F       jsr     SYNCHR          ;yes, adv. TXTPTR (why not CHRGET)
            jsr     HFNS            ;get coordinates of line end
            sty     DSCTMP          ;set up for line
            tay
            txa
            ldx     DSCTMP
            jsr     HGLIN           ;plot line
            jmp     LF708           ;loop till no more "TO" phrases

;********************************************************************************
;* ROT= statement                                                               *
;********************************************************************************
ROT         jsr     GETBYT          ;eval exp to a byte in X-reg
            stx     HGR_ROTATION
            rts

;********************************************************************************
;* SCALE= statement                                                             *
;********************************************************************************
SCALE       jsr     GETBYT          ;eval exp to a byte in X-reg
            stx     HGR_SCALE
            rts

; Set up for DRAW and XDRAW.
DRWPNT      jsr     GETBYT          ;get shape number in X-reg
            lda     HGR_SHAPE_PTR   ;search for that shape
            sta     HGR_SHAPE       ;set up ptr to beginning of table
            lda     HGR_SHAPE_PTR+1
            sta     HGR_SHAPE+1
            txa
            ldx     #$00
            cmp     (HGR_SHAPE,x)   ;compare to # of shapes in table
            beq     LF741           ;last shape in table
            bcs     GGERR           ;shape # too large
LF741       asl     A               ;double shape# to make an index
            bcc     LF747           ;add 256 if shape # > 127
            inc     HGR_SHAPE+1
            clc
LF747       tay                     ;use index to look up offset for shape
            lda     (HGR_SHAPE),y   ; in offset table
            adc     HGR_SHAPE
            tax
            iny
            lda     (HGR_SHAPE),y
            adc     HGR_SHAPE_PTR+1
            sta     HGR_SHAPE+1     ;save address of shape
            stx     HGR_SHAPE
            jsr     CHRGOT          ;is there any "AT" phrase?
            cmp     #TOK_AT
            bne     LF766           ;no, draw right where we are
            jsr     SYNCHR          ;scan over "AT"
            jsr     HFNS            ;get X- and Y-coords to start drawing it
            jsr     HPOSN           ;set up cursor there
LF766       lda     HGR_ROTATION    ;rotation value
            rts

;********************************************************************************
;* DRAW statement                                                               *
;********************************************************************************
DRAW        jsr     DRWPNT
            jmp     DRAW1

;********************************************************************************
;* XDRAW statement                                                              *
;********************************************************************************
XDRAW       jsr     DRWPNT
            jmp     XDRAW1

;********************************************************************************
;* SHLOAD statement                                                             *
;*                                                                              *
;* Reads a shape table from cassette tape to a position just below HIMEM.       *
;* HIMEM is then moved to just below the table.                                 *
;********************************************************************************
SHLOAD      lda     #>LINNUM        ;set up to read two bytes
            sta     MON_A1H         ; into LINNUM,LINNUM+1
            sta     MON_A2H
            ldy     #LINNUM
            sty     MON_A1L
            iny                     ;LINNUM+1
            sty     MON_A2L
            jsr     MON_READ        ;read tape
            clc                     ;setup to read LINNUM bytes
            lda     MEMSIZE         ;ending at HIMEM-1
            tax
            dex                     ;forming HIMEM-1
            stx     MON_A2L
            sbc     LINNUM          ;forming HIMEM-LINNUM
            pha
            lda     MEMSIZE+1
            tay
            inx                     ;see if HIMEM low byte was zero
            bne     LF796           ;no
            dey                     ;yes, have to decrement high byte
LF796       sty     MON_A2H
            sbc     LINNUM+1
            cmp     STREND+1        ;running into variables?
            bcc     LF7A0           ;yes, out of memory
            bne     LF7A3           ;no, still room
LF7A0       jmp     MEMERR          ;mem full err

LF7A3       sta     MEMSIZE+1
            sta     FRETOP+1        ;clear string space
            sta     MON_A1H         ;(but names are still in VARTBL!)
            sta     HGR_SHAPE_PTR+1
            pla
            sta     HGR_SHAPE_PTR
            sta     MEMSIZE
            sta     FRETOP
            sta     MON_A1L
            jsr     MON_RD2BIT      ;read to tape transitions
            lda     #$03            ;short delay for intermediate header
            jmp     MON_READ2       ;read shapes

; Called from STORE and RECALL.
TAPEPNT     clc
            lda     LOWTR
            adc     LINNUM
            sta     MON_A2L
            lda     LOWTR+1
            adc     LINNUM+1
            sta     MON_A2H
            ldy     #$04
            lda     (LOWTR),y
            jsr     GETARY2
            lda     HIGHDS
            sta     MON_A1L
            lda     HIGHDS+1
            sta     MON_A1H
            rts

; Called from STORE and RECALL.
GETARYPT    lda     #$40
            sta     SUBFLG
            jsr     PTRGET
            lda     #$00
            sta     SUBFLG
            jmp     VARTIO

;********************************************************************************
;* HTAB statement                                                               *
;*                                                                              *
;* Note that if WNDLEFT is not 0, HTAB can print outside the screen (e.g. in    *
;* the program).                                                                *
;********************************************************************************
HTAB        jsr     GETBYT
            dex
            txa
LF7EC       cmp     #40
            bcc     LF7FA
            sbc     #40
            pha
            jsr     CRDO
            pla
            jmp     LF7EC

LF7FA       sta     MON_CH
            rts

            byte    "K"+$80
            byte    "R"+$80,"W"+$80            ;Richard Weiland?
