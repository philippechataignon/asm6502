;********************************************************************************
;* Apple II                                                                     *
;* System Monitor                                                               *
;*                                                                              *
;* Copyright 1977 by Apple Computer, Inc.                                       *
;* All Rights Reserved                                                          *
;*                                                                              *
;* S. Wozniak                                                                   *
;* A. Baum                                                                      *
;********************************************************************************
;* This is a disassembly of the original Apple II monitor.  The labels and      *
;* comments come from "Monitor ROM Listing" in the Apple II Reference Manual    *
;* (starts on page 155).  This is a fairly direct translation -- operands are   *
;* generally formatted as they appear in the original listing.  Comments have   *
;* been converted to mixed-case, but are otherwise unchanged (typographical     *
;* errors and all).                                                             *
;********************************************************************************
;* Project created by Andy McFadden, using 6502bench SourceGen v1.4.            *
;* Last updated 2019/10/29                                                      *
;********************************************************************************
LOC0        =     $00
LOC1        =     $01
WNDLFT      =     $20
WNDWDTH     =     $21
WNDTOP      =     $22
WNDBTM      =     $23
CH          =     $24
CV          =     $25
GBASL       =     $26
GBASH       =     $27
BASL        =     $28
BASH        =     $29
BAS2L       =     $2a
BAS2H       =     $2b
H2          =     $2c
LMNEM       =     $2c
RTNL        =     $2c
RMNEM       =     $2d
RTNH        =     $2d
V2          =     $2d
CHKSUM      =     $2e
FORMAT      =     $2e
MASK        =     $2e
LASTIN      =     $2f
LENGTH      =     $2f
SIGN        =     $2f
COLOR       =     $30
MODE        =     $31
INVFLG      =     $32
PROMPT      =     $33
YSAV        =     $34
YSAV1       =     $35
CSWL        =     $36
KSWL        =     $38
PCL         =     $3a
PCH         =     $3b
A1L         =     $3c
XQT         =     $3c
A1H         =     $3d
A2L         =     $3e
A2H         =     $3f
A3L         =     $40
A3H         =     $41
A4L         =     $42
A4H         =     $43
A5L         =     $44
ACC         =     $45
XREG        =     $46
YREG        =     $47
STATUS      =     $48
SPNT        =     $49
RNDL        =     $4e
RNDH        =     $4f
ACL         =     $50
ACH         =     $51
XTNDL       =     $52
XTNDH       =     $53
AUXL        =     $54
AUXH        =     $55
PICK        =     $95
IN          =     $0200
USRADR      =     $03f8
NMI         =     $03fb
IRQLOC      =     $03fe
IOADR       =     $c000
KBDSTRB     =     $c010           ;RW keyboard strobe
TAPEOUT     =     $c020           ;RW toggle caseette tape output
SPKR        =     $c030           ;RW toggle speaker
TXTCLR      =     $c050           ;RW display graphics
TXTSET      =     $c051           ;RW display text
MIXSET      =     $c053           ;RW display split screen
TXTPAGE1    =     $c054           ;RW display page 1
LORES       =     $c056           ;RW display lo-res graphics
TAPEIN      =     $c060           ;R read cassette input
PADDL0      =     $c064           ;R analog input 0
PTRIG       =     $c070           ;RW analog input reset
BASIC       =     $e000
BASIC2      =     $e003

.include "apple_enc.inc"
.enc "apple"

* =  $f800

PLOT        lsr     A               ;Y-coord/2
            php                     ;save LSB in carry
            jsr     GBASCALC        ;calc base adr in GBASL,H
            plp                     ;restore LSB from carry
            lda     #$0f            ;mask $0F if even
            bcc     RTMASK
            adc     #$e0            ;mask $F0 if odd
RTMASK      sta     MASK
PLOT1       lda     (GBASL),y       ;data
            eor     COLOR           ; xor color
            and     MASK            ;  and mask
            eor     (GBASL),y       ;   xor data
            sta     (GBASL),y       ;    to data
            rts

HLINE       jsr     PLOT            ;plot square
HLINE1      cpy     H2              ;done?
            bcs     RTS1            ; yes, return
            iny                     ; no, incr index (X-coord)
            jsr     PLOT1           ;plot next square
            bcc     HLINE1          ;always taken
VLINEZ      adc     #$01            ;next Y-coord
VLINE       pha                     ; save on stack
            jsr     PLOT            ; plot square
            pla
            cmp     V2              ;done?
            bcc     VLINEZ          ; no, loop
RTS1        rts

CLRSCR      ldy     #$2f            ;max Y, full scrn clr
            bne     CLRSC2          ;always taken

CLRTOP      ldy     #$27            ;max Y, top scrn clr
CLRSC2      sty     V2              ;store as bottom coord for VLINE calls
            ldy     #$27            ;rightmost X-coord (column)
CLRSC3      lda     #$00            ;top coord for VLINE calls
            sta     COLOR           ;clear color (black)
            jsr     VLINE           ;draw vline
            dey                     ;next leftmost X-coord
            bpl     CLRSC3          ;loop until done.
            rts

GBASCALC    pha                     ;for input 000DEFGH
            lsr     A
            and     #$03
            ora     #$04            ;generate GBASH=000001FG
            sta     GBASH
            pla                     ;and GBASL=HDEDE000
            and     #$18
            bcc     GBCALC
            adc     #$7f
GBCALC      sta     GBASL
            asl     A
            asl     A
            ora     GBASL
            sta     GBASL
            rts

NXTCOL      lda     COLOR           ;increment color by 3
            clc
            adc     #$03
SETCOL      and     #$0f            ;sets COLOR=17*A mod 16
            sta     COLOR
            asl     A               ;both half .bytes of COLOR =al
            asl     A
            asl     A
            asl     A
            ora     COLOR
            sta     COLOR
            rts

SCRN        lsr     A               ;read screen Y-coord/2
            php                     ;save LSB (carry)
            jsr     GBASCALC        ;calc base address
            lda     (GBASL),y       ;get .byte
            plp                     ;restore LSB from carry
SCRN2       bcc     RTMSKZ          ;if even, use lo H
            lsr     A
            lsr     A
            lsr     A               ;shift high half .byte down
            lsr     A
RTMSKZ      and     #$0f            ;mask 4-bits
            rts

INSDS1      ldx     PCL             ;print PCL,H
            ldy     PCH
            jsr     PRYX2
            jsr     PRBLNK          ;followed by a blank
            lda     (PCL,x)         ;get op code
INSDS2      tay
            lsr     A               ;even/odd test
            bcc     IEVEN
            ror     A               ;bit 1 test
            bcs     ERR             ;XXXXXX11 invalid op
            cmp     #$a2
            beq     ERR             ;opcode $89 invalid
            and     #$87            ;mask bits
IEVEN       lsr     A               ;LSB into carry for L/R test
            tax
            lda     FMT1,x          ;get format index .byte
            jsr     SCRN2           ;R/L H-.byte on carry
            bne     GETFMT
ERR         ldy     #$80            ;substitute $80 for invalid ops
            lda     #$00            ;set print format index to 0
GETFMT      tax
            lda     FMT2,x          ;index into print format table
            sta     FORMAT          ;save for adr field formatting
            and     #$03            ;mask for 2-bit length (P=1 .byte, 1=2 byte, 2=3 byte)
            sta     LENGTH
            tya                     ;opcode
            and     #$8f            ;mask for 1XXX1010 test
            tax                     ; save it
            tya                     ;opcode to A again
            ldy     #$03
            cpx     #$8a
            beq     MNNDX3
MNNDX1      lsr     A
            bcc     MNNDX3          ;form index into mnemonic table
            lsr     A
MNNDX2      lsr     A               ;1) 1XXX1010=>00101XXX
            ora     #$20            ;2) XXXYYY01=>00111XXX
            dey                     ;3) XXXYYY10=>00110XXX
            bne     MNNDX2          ;4) XXXYY100=>00100XXX
            iny                     ;5) XXXXX000=>000XXXXX
MNNDX3      dey
            bne     MNNDX1
            rts

            .byte   $ff,$ff,$ff

INSTDSP     jsr     INSDS1          ;gen fmt, len .bytes
            pha                     ;save mnemonic table index
PRNTOP      lda     (PCL),y
            jsr     PRBYTE
            ldx     #$01            ;print 2 blanks
PRNTBL      jsr     PRBL2
            cpy     LENGTH          ;print inst (1-3 .bytes)
            iny                     ;in a 12 chr field
            bcc     PRNTOP
            ldx     #$03            ;char count for mnemonic print
            cpy     #$04
            bcc     PRNTBL
            pla                     ;recover mnemonic index
            tay
            lda     MNEML,y
            sta     LMNEM           ;fech 3-char mnemonic
            lda     MNEMR,y         ;  (packed in 2-.bytes)
            sta     RMNEM
PRMN1       lda     #$00
            ldy     #$05
PRMN2       asl     RMNEM           ;shift 5 bits of
            rol     LMNEM           ;  character into A
            rol     A               ;    (clear carry)
            dey
            bne     PRMN2
            adc     #$bf            ;add "?" offset
            jsr     COUT            ;output a char of mnem
            dex
            bne     PRMN1
            jsr     PRBLNK          ;output 3 blanks
            ldy     LENGTH
            ldx     #$06            ;cnt for 6 format bits
PRADR1      cpx     #$03
            beq     PRADR5          ;if X=3 then addr.
PRADR2      asl     FORMAT
            bcc     PRADR3
            lda     CHAR1-1,x
            jsr     COUT
            lda     CHAR2-1,x
            beq     PRADR3
            jsr     COUT
PRADR3      dex
            bne     PRADR1
            rts

PRADR4      dey
            bmi     PRADR2
            jsr     PRBYTE
PRADR5      lda     FORMAT
            cmp     #$e8            ;handle rel adr mode
            lda     (PCL),y         ;special (print target,
            bcc     PRADR4          ;  not offset)
RELADR      jsr     PCADJ3
            tax                     ;PCL,PCH+OFFSET+1 to A,Y
            inx
            bne     PRNTYX          ;+1 to Y,X
            iny
PRNTYX      tya
PRNTAX      jsr     PRBYTE          ;output target adr
PRNTX       txa                     ;  of branch and return
            jmp     PRBYTE

PRBLNK      ldx     #$03            ;blank count
PRBL2       lda     #$a0            ;load a space
PRBL3       jsr     COUT            ;output a blank
            dex
            bne     PRBL2           ;loop until count=0
            rts

PCADJ       sec                     ;0=1-.byte,1=2-byte,
PCADJ2      lda     LASTIN          ;  2=3-.byte
PCADJ3      ldy     PCH
            tax                     ;test displacement sign
            bpl     PCADJ4          ;  (for rel branch)
            dey                     ;extend neg by decr PCH
PCADJ4      adc     PCL
            bcc     RTS2            ;PCL+LENGTH(or DISPL)+1 to A
            iny                     ;  carry into Y (PCH)
RTS2        rts

; FMT1 .bytes:  XXXXXXY0 instrs
; if Y=0       then left half .byte
; if Y=1       then right half .byte
;                   (x=index)
FMT1        .byte   $04,$20,$54,$30,$0d,$80,$04,$90,$03,$22,$54,$33,$0d,$80,$04,$90
            .byte        $04,$20,$54,$33,$0d,$80,$04,$90,$04,$20,$54,$3b,$0d,$80,$04,$90
            .byte        $00,$22,$44,$33,$0d,$c8,$44,$00,$11,$22,$44,$33,$0d,$c8,$44,$a9
            .byte        $01,$22,$44,$33,$0d,$80,$04,$90,$01,$22,$44,$33,$0d,$80,$04,$90
            .byte        $26,$31,$87,$9a
; ZZXXXY01 instr's
FMT2        .byte    $00             ;err
            .byte    $21             ;imm
            .byte    $81             ;z-page
            .byte    $82             ;abs
            .byte    $00             ;implied
            .byte    $00             ;accumulator
            .byte    $59             ;(zpag,x)
            .byte    $4d             ;(zpag),y
            .byte    $91             ;zpag,x
            .byte    $92             ;abs,x
            .byte    $86             ;abs,y
            .byte    $4a             ;(abs)
            .byte    $85             ;zpag,y
            .byte    $9d             ;relative
CHAR1       .text    ",),#($"
CHAR2       .text    "Y"
            .byte    $00
            .text    "X","$","$"
            .byte    $00
; MNEML is of form:
; (A) XXXXX000
; (B) XXXYY100
; (C) 1XXX1010
; (D) XXXYYY10
; (E) XXXYYY01
;     (X=index)
MNEML       .byte   $1c,$8a,$1c,$23,$5d,$8b,$1b,$a1,$9d,$8a,$1d,$23,$9d,$8b,$1d,$a1
            .byte      $00,$29,$19,$ae,$69,$a8,$19,$23,$24,$53,$1b,$23,$24,$53,$19,$a1
            .byte      $00,$1a,$5b,$5b,$a5,$69,$24,$24,$ae,$ae,$a8,$ad,$29,$00,$7c,$00
            .byte      $15,$9c,$6d,$9c,$a5,$69,$29,$53,$84,$13,$34,$11,$a5,$69,$23,$a0
MNEMR       .byte   $d8,$62,$5a,$48,$26,$62,$94,$88,$54,$44,$c8,$54,$68,$44,$e8,$94
            .byte      $00,$b4,$08,$84,$74,$b4,$28,$6e,$74,$f4,$cc,$4a,$72,$f2,$a4,$8a
            .byte      $00,$aa,$a2,$a2,$74,$74,$74,$72,$44,$68,$b2,$32,$b2,$00,$22,$00
            .byte      $1a,$1a,$26,$26,$72,$72,$88,$c8,$c4,$ca,$26,$48,$44,$44,$a2,$c8
            .byte      $ff,$ff,$ff

STEP        jsr     INSTDSP         ;disassemble one inst
            pla                     ;  at (PCL,H)
            sta     RTNL            ;adjust to user
            pla                     ;  stack. Save
            sta     RTNH            ;  rtn adr.
            ldx     #$08
XQINIT      lda     INITBL-1,x      ;init XEQ area
            sta     XQT,x
            dex
            bne     XQINIT
            lda     (PCL,x)         ;user opcode .byte
            beq     XBRK            ;special if break
            ldy     LENGTH          ;len from disassembly
            cmp     #$20
            beq     XJSR            ;handle JSR, RTS, JMP,
            cmp     #$60            ;  JMP ( ), RTI special
            beq     XRTS
            cmp     #$4c
            beq     XJMP
            cmp     #$6c
            beq     XJMPAT
            cmp     #$40
            beq     XRTI
            and     #$1f
            eor     #$14
            cmp     #$04            ;copy user inst to XEQ area
            beq     XQ2             ;  with trailing NOPs
XQ1         lda     (PCL),y         ;change rel branch
XQ2         sta     XQT,y           ;  disp to 4 for
            dey                     ;  jmp to branch or
            bpl     XQ1             ;  nbranch from XEQ.
            jsr     RESTORE         ;restore user reg contents.
            jmp     XQT             ;XEQ user op from RAM (return to NBRANCH)

IRQ         sta     ACC
            pla
            pha                     ;**IRQ handler
            asl     A
            asl     A
            asl     A
            bmi     BREAK           ;test for break
            jmp     (IRQLOC)        ;user routine vector in RAM

BREAK       plp
            jsr     SAV1            ;ave reg's on break
            pla                     ;  including PC
            sta     PCL
            pla
            sta     PCH
XBRK        jsr     INSDS1          ;print user PC.
            jsr     RGDSP1          ;  and reg's
            jmp     MON             ;go to monitor

XRTI        clc
            pla                     ;simulate RTI by expecting
            sta     STATUS          ;  status from stack, then RTS
XRTS        pla                     ;RTS simulation
            sta     PCL             ;  extract PC from stack
            pla                     ;  and update PC by 1 (len=0)
PCINC2      sta     PCH
PCINC3      lda     LENGTH          ;update PC by len
            jsr     PCADJ3
            sty     PCH
            clc
            bcc     NEWPCL

XJSR        clc
            jsr     PCADJ2          ;update PC and push
            tax                     ;  onto stack for
            tya                     ;  JSR simulate
            pha
            txa
            pha
            ldy     #$02
XJMP        clc
XJMPAT      lda     (PCL),y
            tax                     ;load PC for JMP,
            dey                     ;  (JMP) simulate.
            lda     (PCL),y
            stx     PCH
NEWPCL      sta     PCL
            bcs     XJMP
RTNJMP      lda     RMNEM
            pha
            lda     H2
            pha
REGDSP      jsr     CROUT           ;display user reg
RGDSP1      lda     #$45            ;  contents with
            sta     A3L             ;  labels
            lda     #$00
            sta     A3H
            ldx     #$fb
RDSP1       lda     #$a0
            jsr     COUT
            lda     MNEMR+30,x
            jsr     COUT
            lda     #$bd
            jsr     COUT
            lda     $4a,x
            jsr     PRBYTE
            inx
            bmi     RDSP1
            rts

BRANCH      clc                     ;branch taken,
            ldy     #$01            ;  add len+2 to PC
            lda     (PCL),y
            jsr     PCADJ3
            sta     PCL
            tya
            sec
            bcs     PCINC2

NBRNCH      jsr     SAVE            ;normal return after
            sec                     ;  XEQ user of
            bcs     PCINC3          ;go update PC

INITBL      nop
            nop                     ;dummy fill for
            jmp     NBRNCH          ;  XEQ area

            jmp     BRANCH

            .text   "AXYPS"

PREAD       lda     PTRIG           ;trigger paddles
            ldy     #$00            ;init count
            nop                     ;compensate for 1st count
            nop
PREAD2      lda     PADDL0,x        ;count Y-reg every
            bpl     RTS2D           ;  12 usec [actually 11]
            iny
            bne     PREAD2          ;  exit at 255 max
            dey
RTS2D       rts

INIT        lda     #$00            ;clr status for debug
            sta     STATUS          ;  software
            lda     LORES
            lda     TXTPAGE1        ;init video mode
SETTXT      lda     TXTSET          ;set for text mode
            lda     #$00            ;  full screen window
            beq     SETWND

SETGR       lda     TXTCLR          ;set for graphics mode
            lda     MIXSET          ;  lower 4 lines as
            jsr     CLRTOP          ;  text window
            lda     #$14
SETWND      sta     WNDTOP          ;set for 40 col window
            lda     #$00            ;  top in A-reg
            sta     WNDLFT          ;  bttm at line 24
            lda     #$28
            sta     WNDWDTH
            lda     #$18
            sta     WNDBTM          ;  vtab to row 23
            lda     #$17
TABV        sta     CV              ;vtabs to row in A-reg
            jmp     VTAB

MULPM       jsr     MD1             ;abs val of AC AUX
MUL         ldy     #$10            ;index for 16 bits
MUL2        lda     ACL             ;ACX * AUX + XTND
            lsr     A               ;  to AC, XTND
            bcc     MUL4            ;if no carry,
            clc                     ;  no partial prod.
            ldx     #$fe
MUL3        lda     XTNDL+2,x       ;add mplcnd (AUX)
            adc     AUXL+2,x        ; to partial prod
            sta     XTNDL+2,x       ;    (XTND).
            inx
            bne     MUL3
MUL4        ldx     #$03
MUL5        ror     ACL,x           ;(original src: DFB #$76, DFB #$50)
            dex
            bpl     MUL5
            dey
            bne     MUL2
            rts

DIVPM       jsr     MD1             ;abs val of AC, AUX.
DIV         ldy     #$10            ;index for 16 bits
DIV2        asl     ACL
            rol     ACH
            rol     XTNDL           ;XTND/AUX
            rol     XTNDH           ;  to AC.
            sec
            lda     XTNDL
            sbc     AUXL            ;mod to XTND.
            tax
            lda     XTNDH
            sbc     AUXH
            bcc     DIV3
            stx     XTNDL
            sta     XTNDH
            inc     ACL
DIV3        dey
            bne     DIV2
            rts

MD1         ldy     #$00            ;abs val of AC, AUX
            sty     LASTIN          ;  with result sign
            ldx     #AUXL           ;  in LSB of SIGN.
            jsr     MD2
            ldx     #ACL
MD2         lda     LOC1,x          ;X specifies AC or AUX
            bpl     MDRTS
            sec
MD3         tya
            sbc     LOC0,x          ;compl specified reg
            sta     LOC0,x          ;  if neg.
            tya
            sbc     LOC1,x
            sta     LOC1,x
            inc     SIGN
MDRTS       rts

BASCALC     pha                     ;calc base adr in BASL,H
            lsr     A               ;  for given line no.
            and     #$03            ;  0<=line no.<=$17
            ora     #$04            ;ARG=000ABCDE, generate
            sta     BASH            ;  BASH=000001CD
            pla                     ;  and
            and     #$18            ;  BASL=EABAB000
            bcc     BSCLC2
            adc     #$7f
BSCLC2      sta     BASL
            asl     A
            asl     A
            ora     BASL
            sta     BASL
            rts

BELL1       cmp     #$87            ;bell char? (cntrl-G)
            bne     RTS2B           ;  no, return
            lda     #$40            ;delay .01 seconds
            jsr     WAIT
            ldy     #$c0
BELL2       lda     #$0c            ;toggle speaker at
            jsr     WAIT            ;  1 KHz for .1 sec.
            lda     SPKR
            dey
            bne     BELL2
RTS2B       rts

STOADV      ldy     CH              ;curser H index to Y-reg
            sta     (BASL),y        ;stor char in line
ADVANCE     inc     CH              ;increment curser H index
            lda     CH              ;  (move right)
            cmp     WNDWDTH         ;beyond window width?
            bcs     CR              ;  yes CR to next line
RTS3        rts                     ;  no,return

VIDOUT      cmp     #$a0            ;control char?
            bcs     STOADV          ;  no,output it
            tay                     ;inverse video?
            bpl     STOADV          ;  yes, output it.
            cmp     #$8d            ;CR?
            beq     CR              ;  yes.
            cmp     #$8a            ;line feed?
            beq     LF              ;  if so, do it.
            cmp     #$88            ;back space? (cntrl-H)
            bne     BELL1           ;  no, check for bell.
BS          dec     CH              ;decrement curser H index
            bpl     RTS3            ;if pos, ok. Else move up
            lda     WNDWDTH         ;set CH to WNDWDTH-1
            sta     CH
            dec     CH              ;(rightmost screen pos)
UP          lda     WNDTOP          ;curser V index
            cmp     CV
            bcs     RTS4            ;if top line then return
            dec     CV              ;decr curser V-index
VTAB        lda     CV              ;get curser V-index
VTABZ       jsr     BASCALC         ;generate base addr
            adc     WNDLFT          ;and window left index
            sta     BASL            ;to BASL
RTS4        rts

ESC1        eor     #$c0            ;esc?
            beq     HOME            ;  if so, do home and clear
            adc     #$fd            ;esc-A or B check
            bcc     ADVANCE         ;  A, advance
            beq     BS              ;  B, backspace
            adc     #$fd            ;esc-C or D check
            bcc     LF              ;  C,down
            beq     UP              ;  D, go up
            adc     #$fd            ;esc-E or F check
            bcc     CLREOL          ;  E, clear to end of line
            bne     RTS4            ;  not F, return
CLREOP      ldy     CH              ;cursor H to Y index
            lda     CV              ;cursor V to A-register
CLEOP1      pha                     ;save current line on stk
            jsr     VTABZ           ;calc base address
            jsr     CLEOLZ          ;clear to EOL, set carry
            ldy     #$00            ;clear from H index=0 for rest
            pla                     ;increment current line
            adc     #$00            ;(carry is set)
            cmp     WNDBTM          ;done to bottom of window?
            bcc     CLEOP1          ;  no, keep clearing lines
            bcs     VTAB            ;  yes, tab to current line

HOME        lda     WNDTOP          ;init cursor V
            sta     CV              ;  and H-indices
            ldy     #$00
            sty     CH              ;then clear to end of page
            beq     CLEOP1

CR          lda     #$00            ;cursor to left of index
            sta     CH              ;(ret cursor H=0)
LF          inc     CV              ;incr cursor V(down 1 line)
            lda     CV
            cmp     WNDBTM          ;off screen?
            bcc     VTABZ           ;  no, set base addr
            dec     CV              ;decr cursor V(back to bottom)
SCROLL      lda     WNDTOP          ;start at top of scrl wndw
            pha
            jsr     VTABZ           ;generate base address
SCRL1       lda     BASL            ;copy BASL,H
            sta     BAS2L           ;  to BAS2L,H
            lda     BASH
            sta     BAS2H
            ldy     WNDWDTH         ;init Y to rightmost index
            dey                     ;  of scrolling window
            pla
            adc     #$01            ;incr line number
            cmp     WNDBTM          ;done?
            bcs     SCRL3           ;  yes, finish
            pha
            jsr     VTABZ           ;form BASL,H (base addr)
SCRL2       lda     (BASL),y        ;move a chr up on line
            sta     (BAS2L),y
            dey                     ;next char of line
            bpl     SCRL2
            bmi     SCRL1           ;next line

SCRL3       ldy     #$00            ;clear bottom line
            jsr     CLEOLZ          ;get base addr for bottom line
            bcs     VTAB            ;carry is set
CLREOL      ldy     CH              ;cursor H index
CLEOLZ      lda     #$a0
CLEOL2      sta     (BASL),y        ;store blanks from 'here'
            iny                     ;  to end of lines (WNDWDTH)
            cpy     WNDWDTH
            bcc     CLEOL2
            rts

WAIT        sec
WAIT2       pha
WAIT3       sbc     #$01            ;1.0204 usec [wrong]
            bne     WAIT3           ;(13+2712*A+512*A*A) [wrong]
            pla
            sbc     #$01
            bne     WAIT2
            rts

NXTA4       inc     A4L             ;incr 2-.byte A4
            bne     NXTA1           ;  and A1
            inc     A4H
NXTA1       lda     A1L             ;incr 2-.byte A1.
            cmp     A2L
            lda     A1H             ;  and compare to A2
            sbc     A2H
            inc     A1L             ;  (carry set if >=)
            bne     RTS4B
            inc     A1H
RTS4B       rts

HEADR       ldy     #$4b            ;write A*256 'long 1'
            jsr     ZERDLY          ;  half cycles
            bne     HEADR           ;  (650 usec each)
            adc     #$fe
            bcs     HEADR           ;then a 'short 0'
            ldy     #$21            ;  (400 usec)
WRBIT       jsr     ZERDLY          ;write two half cycles
            iny                     ;  of 250 usec ('0')
            iny                     ;  or 500 usec ('0')
ZERDLY      dey
            bne     ZERDLY
            bcc     WRTAPE          ;Y is count for
            ldy     #$32            ;  timing loop
ONEDLY      dey
            bne     ONEDLY
WRTAPE      ldy     TAPEOUT
            ldy     #$2c
            dex
            rts

RDBYTE      ldx     #$08            ;8 bits to read
RDBYT2      pha                     ;read two transitions
            jsr     RD2BIT          ;  (find edge)
            pla
            rol     A               ;next bit
            ldy     #$3a            ;count for samples
            dex
            bne     RDBYT2
            rts

RD2BIT      jsr     RDBIT
RDBIT       dey                     ;decr Y until
            lda     TAPEIN          ;  tape transition
            eor     LASTIN
            bpl     RDBIT
            eor     LASTIN
            sta     LASTIN
            cpy     #$80            ;set carry on Y-reg.
            rts

RDKEY       ldy     CH
            lda     (BASL),y        ;set screen to flash
            pha
            and     #$3f
            ora     #$40
            sta     (BASL),y
            pla
            jmp     (KSWL)          ;go to user key-in

KEYIN       inc     RNDL
            bne     KEYIN2          ;incr rnd number
            inc     RNDH
KEYIN2      bit     IOADR           ;key down?
            bpl     KEYIN           ;  loop
            sta     (BASL),y        ;replace flashing screen
            lda     IOADR           ;get keycode
            bit     KBDSTRB         ;clr key strobe
            rts

ESC         jsr     RDKEY           ;get keycode
            jsr     ESC1            ;  handle esc func.
RDCHAR      jsr     RDKEY           ;read key
            cmp     #$9b            ;esc?
            beq     ESC             ;  yes, don't return
            rts

NOTCR       lda     INVFLG
            pha
            lda     #$ff
            sta     INVFLG          ;echo user line
            lda     IN,x            ;  non inverse
            jsr     COUT
            pla
            sta     INVFLG
            lda     IN,x
            cmp     #$88            ;check for edit keys
            beq     BCKSPC          ;  BS, ctrl-X
            cmp     #$98
            beq     CANCEL
            cpx     #$f8            ;margin?
            bcc     NOTCR1
            jsr     BELL            ;yes, sound bell
NOTCR1      inx                     ;advance input index
            bne     NXTCHAR
CANCEL      lda     #$dc            ;backslash after cancelled lin
            jsr     COUT
GETLNZ      jsr     CROUT           ;output CR
GETLN       lda     PROMPT
            jsr     COUT            ;output prompt char
            ldx     #$01            ;init input index
BCKSPC      txa                     ;  will backspace to 0
            beq     GETLNZ
            dex
NXTCHAR     jsr     RDCHAR
            cmp     #PICK           ;use screen char
            bne     CAPTST          ;  for ctrl-U
            lda     (BASL),y
CAPTST      cmp     #$e0
            bcc     ADDINP          ;convert to caps
            and     #$df
ADDINP      sta     IN,x            ;add to input buf
            cmp     #$8d
            bne     NOTCR
            jsr     CLREOL          ;clr to EOL if CR
CROUT       lda     #$8d
            bne     COUT

PRA1        ldy     A1H             ;print CR,A1 in hex
            ldx     A1L
PRYX2       jsr     CROUT
            jsr     PRNTYX
            ldy     #$00
            lda     #$ad            ;print '-'
            jmp     COUT

XAM8        lda     A1L
            ora     #$07            ;set to finish at
            sta     A2L             ;  mod 8=7
            lda     A1H
            sta     A2H
MOD8CHK     lda     A1L
            and     #$07
            bne     DATACUT
XAM         jsr     PRA1
DATACUT     lda     #$a0
            jsr     COUT            ;output blank
            lda     (A1L),y
            jsr     PRBYTE          ;output .byte in hex
            jsr     NXTA1
            bcc     MOD8CHK         ;check if time to,
RTS4C       rts                     ;  print addr

XAMPM       lsr     A               ;determine if mon
            bcc     XAM             ;  mode is xam
            lsr     A               ;  add, or sub
            lsr     A
            lda     A2L
            bcc     ADD
            eor     #$ff            ;sub: form 2's complement
ADD         adc     A1L
            pha
            lda     #$bd
            jsr     COUT            ;print '=', then result
            pla
PRBYTE      pha                     ;print .byte as 2 hex
            lsr     A               ;  digits, destroys A-reg
            lsr     A
            lsr     A
            lsr     A
            jsr     PRHEXZ
            pla
PRHEX       and     #$0f            ;print hex dig in A-reg
PRHEXZ      ora     #$b0            ;  LSB's
            cmp     #$ba
            bcc     COUT
            adc     #$06
COUT        jmp     (CSWL)          ;vector to user output routine

COUT1       cmp     #$a0
            bcc     COUTZ           ;don't output ctrl's inverse
            and     INVFLG          ;mask with inverse flag
COUTZ       sty     YSAV1           ;sav Y-reg
            pha                     ;sav A-reg
            jsr     VIDOUT          ;output A-reg as ASCII
            pla                     ;restore A-reg
            ldy     YSAV1           ;  and Y-reg
            rts                     ;  then return

BL1         dec     YSAV
            beq     XAM8
BLANK       dex                     ;blank to mon
            bne     SETMDZ          ;after blank
            cmp     #$ba            ;data store mode?
            bne     XAMPM           ;  no, xam, add or sub
STOR        sta     MODE            ;keep in store mode
            lda     A2L
            sta     (A3L),y         ;store as lwo .byte as (A3)
            inc     A3L
            bne     RTS5            ;incr A3, return
            inc     A3H
RTS5        rts

SETMODE     ldy     YSAV            ;save converted ':', '+',
            lda     IN-1,y          ;  '-', '.' as mode.
SETMDZ      sta     MODE
            rts

LT          ldx     #$01
LT2         lda     A2L,x           ;copy A2 (2 .bytes) to
            sta     A4L,x           ;  A4 and A5
            sta     A5L,x
            dex
            bpl     LT2
            rts

MOVE        lda     (A1L),y         ;move (A1 to A2) to
            sta     (A4L),y         ;  (A4)
            jsr     NXTA4
            bcc     MOVE
            rts

VFY         lda     (A1L),y         ;verify (A1 to A2) with
            cmp     (A4L),y         ;  (A4)
            beq     VFYOK
            jsr     PRA1
            lda     (A1L),y
            jsr     PRBYTE
            lda     #$a0
            jsr     COUT
            lda     #$a8
            jsr     COUT
            lda     (A4L),y
            jsr     PRBYTE
            lda     #$a9
            jsr     COUT
VFYOK       jsr     NXTA4
            bcc     VFY
            rts

LIST        jsr     A1PC            ;move A1 (2 .bytes) to
            lda     #$14            ;  PC if spec'd and
LIST2       pha                     ;  dissemble 20 instrs
            jsr     INSTDSP
            jsr     PCADJ           ;adjust PC each instr
            sta     PCL
            sty     PCH
            pla
            sec
            sbc     #$01            ;next of 20 instrs
            bne     LIST2
            rts

A1PC        txa                     ;if user spec'd adr
            beq     A1PCRTS         ;  copy from A1 to PC
A1PCLP      lda     A1L,x
            sta     PCL,x
            dex
            bpl     A1PCLP
A1PCRTS     rts

SETINV      ldy     #$3f            ;set for inverse vid
            bne     SETIFLG         ;  via COUT1

SETNORM     ldy     #$ff            ;set for normal vid
SETIFLG     sty     INVFLG
            rts

SETKBD      lda     #$00            ;simulate port #0 input
INPORT      sta     A2L             ;  specified (KEYIN routine)
INPRT       ldx     #KSWL
            ldy     #<KEYIN
            bne     IOPRT

SETVID      lda     #$00            ;simulate port #0 output
OUTPORT     sta     A2L             ;  specified (COUT1 routine)
OUTPRT      ldx     #CSWL
            ldy     #<COUT1
IOPRT       lda     A2L             ;set RAM in/out vectors
            and     #$0f
            beq     IOPRT1
            ora     #>IOADR
            ldy     #$00
            beq     IOPRT2

IOPRT1      lda     #>COUT1
IOPRT2      sty     LOC0,x
            sta     LOC1,x
            rts

            nop
            nop
XBASIC      jmp     BASIC           ;to BASIC with scratch

BASCONT     jmp     BASIC2          ;continue BASIC

GO          jsr     A1PC            ;adr to PC if spec'd
            jsr     RESTORE         ;restore meta regs
            jmp     (PCL)           ;go to user subr

REGZ        jmp     REGDSP          ;to reg display

TRACE       dec     YSAV
STEPZ       jsr     A1PC            ;adr to PC if spec'd
            jmp     STEP            ;take one step

USR         jmp     USRADR          ;to usr subr at USRADR

WRITE       lda     #$40
            jsr     HEADR           ;write 10-sec header
            ldy     #$27
WR1         ldx     #$00
            eor     (A1L,x)
            pha
            lda     (A1L,x)
            jsr     WRBYTE
            jsr     NXTA1
            ldy     #$1d
            pla
            bcc     WR1
            ldy     #$22
            jsr     WRBYTE
            beq     BELL
WRBYTE      ldx     #$10
WRBYT2      asl     A
            jsr     WRBIT
            bne     WRBYT2
            rts

CRMON       jsr     BL1             ;handle CR as blank
            pla                     ;  then pop stack
            pla                     ;  and rtn to mon
            bne     MONZ

READ        jsr     RD2BIT          ;find tapein edge
            lda     #$16
            jsr     HEADR           ;delay 3.5 seconds
            sta     CHKSUM          ;init CHKSUM=$ff
            jsr     RD2BIT          ;find tapein edge
RD2         ldy     #$24            ;look for sync bit
            jsr     RDBIT           ;  (short 0)
            bcs     RD2             ;  loop until found
            jsr     RDBIT           ;skip second sync H-cycle
            ldy     #$3b            ;index for 0/1 test
RD3         jsr     RDBYTE          ;read a .byte
            sta     (A1L,x)         ;store at (A1)
            eor     CHKSUM
            sta     CHKSUM          ;update running chksum
            jsr     NXTA1           ;incr A1, compare to A2
            ldy     #$35            ;compenstate 0/1 index
            bcc     RD3             ;loop until done
            jsr     RDBYTE          ;read chksum .byte
            cmp     CHKSUM
            beq     BELL            ;good, sound bell and return
PRERR       lda     #$c5
            jsr     COUT            ;print "ERR", then bell
            lda     #$d2
            jsr     COUT
            jsr     COUT
BELL        lda     #$87            ;output bell and return
            jmp     COUT

RESTORE     lda     STATUS          ;restore 6502 reg contents
            pha                     ;  used by debug software
            lda     ACC
RESTR1      ldx     XREG
            ldy     YREG
            plp
            rts

SAVE        sta     ACC             ;save 6502 reg contents
SAV1        stx     XREG
            sty     YREG
            php
            pla
            sta     STATUS
            tsx
            stx     SPNT
            cld
            rts

RESET       jsr     SETNORM         ;set screen mode
            jsr     INIT            ;  and init kbd/screen
            jsr     SETVID          ;  as I/O dev's
            jsr     SETKBD
MON         cld                     ;must set hex mode!
            jsr     BELL
MONZ        lda     #$aa            ;'*' prompt for mon
            sta     PROMPT
            jsr     GETLNZ          ;read a line
            jsr     ZMODE           ;clear mon mode, scan idx
NXTITM      jsr     GETNUM          ;get item, non-hex
            sty     YSAV            ;char in A-reg
            ldy     #$17            ;  X-reg=0 if no hex input
CHRSRCH     dey
            bmi     MON             ;not found, go to mon
            cmp     CHRTBL,y        ;find cmnd char in tbl
            bne     CHRSRCH
            jsr     TOSUB           ;found, call corresponding
            ldy     YSAV            ;  subroutine
            jmp     NXTITM

DIG         ldx     #$03
            asl     A
            asl     A               ;got hex dig,
            asl     A               ;  shift into A2
            asl     A
NXTBIT      asl     A
            rol     A2L
            rol     A2H
            dex                     ;leave X=$ff if dig
            bpl     NXTBIT
NXTBAS      lda     MODE
            bne     NXTBS2          ;if mode is zero
            lda     A2H,x           ;  then copy A2 to
            sta     A1H,x           ;  A1 and A3
            sta     A3H,x
NXTBS2      inx
            beq     NXTBAS
            bne     NXTCHR

GETNUM      ldx     #$00            ;clear A2
            stx     A2L
            stx     A2H
NXTCHR      lda     IN,y            ;get char
            iny
            eor     #$b0
            cmp     #$0a
            bcc     DIG             ;if hex dig, then
            adc     #$88
            cmp     #$fa
            bcs     DIG
            rts

TOSUB       lda     #>GO            ;push high-order
            pha                     ;  subr adr on stk
            lda     SUBTBL,y        ;push low order
            pha                     ;  subr adr on stk
            lda     MODE
ZMODE       ldy     #$00            ;clr mode, old mode
            sty     MODE            ;  to A-reg
            rts                     ;go to subr via RTS

CHRTBL      .byte    $bc             ;F("Ctrl+C")
            .byte    $b2             ;F("Ctrl+Y")
            .byte    $be             ;F("Ctrl+E")
            .byte    $ed             ;F("T")
            .byte    $ef             ;F("V")
            .byte    $c4             ;F("Ctrl+K")
            .byte    $ec             ;F("S")
            .byte    $a9             ;F("Ctrl+P")
            .byte    $bb             ;F("Ctrl+B")
            .byte    $a6             ;F("-")
            .byte    $a4             ;F("+")
            .byte    $06             ;F("M")  (F=EX-OR $B0+$89)
            .byte    $95             ;F("<")
            .byte    $07             ;F("N")
            .byte    $02             ;F("I")
            .byte    $05             ;F("L")
            .byte    $f0             ;F("W")
            .byte    $00             ;F("G")
            .byte    $eb             ;G("R")
            .byte    $93             ;F(":")
            .byte    $a7             ;F(".")
            .byte    $c6             ;F("CR")
            .byte    $99             ;F(BLANK)
SUBTBL      .byte    <BASCONT-1
            .byte    <USR-1
            .byte    <REGZ-1
            .byte    <TRACE-1
            .byte    <VFY-1
            .byte    <INPRT-1
            .byte    <STEPZ-1
            .byte    <OUTPRT-1
            .byte    <XBASIC-1
            .byte    <SETMODE-1
            .byte    <SETMODE-1
            .byte    <MOVE-1
            .byte    <LT-1
            .byte    <SETNORM-1
            .byte    <SETINV-1
            .byte    <LIST-1
            .byte    <WRITE-1
            .byte    <GO-1
            .byte    <READ-1
            .byte    <SETMODE-1
            .byte    <SETMODE-1
            .byte    <CRMON-1
            .byte    <BLANK-1
            .word    NMI             ;NMI vector
            .word    RESET           ;reset vector
            .word    IRQ             ;IRQ vector
