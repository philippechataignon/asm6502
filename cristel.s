;*******************************
;                              *
; CRISTEL V 1.14  (C) Ch.QUEST *
; --------------    16/10/85   *
;                              *
;  LOGICIEL DE COMMUNICATION   *
;                              *
;   POUR LA CARTE APPLE-TELL   *
;                              *
;    (ET LA CARTE TIME ][)     *
;                              *
;*******************************
;
;
;
;
;
;
* = $9000
;
; ADRESSES ROM ET PAGE ZERO UTILISEES
;
SYNTERR   = $DEC9        ;AFFICHE SYNTAX ERROR
LOMEM     = $6D
HIMEM     = $6F
FREE      = $73
PTRVAR    = $83          ;ADRESSE DU PRTGET
FAC       = $9D
SPEED     = $F1
CHRGET    = $B1          ;LECTURE D'UN CHAR DU PRGM
CHRGOT    = $B8          ;RELECTURE DERNIER CARAC
ONERR     = $D8
BUFFER    = $200
AMPER     = $3F5
HORL      = $C0F2        ;$C082+s0
V1ON      = $C059
V1OFF     = $C058
V2ON      = $C05B
V2OFF     = $C05A
V3ON      = $C05D
V3OFF     = $C05C
DOSLEN    = $AA60        ;L$ (BLOAD,BSAVE)
DOSADD    = $AA72        ;A$
CSW       = $AA53        ;ADRESSES DES ROUTINES
KSW       = $AA55        ;NORMAL D'AFFICHAGE (DOS)
COUT      = $FDED        ;AFFICHAGE
BASRUN    = $D566        ;RUN (ROM)
BASERR    = $D412        ;GESTION DES ERREURS
CHKCOM    = $DEBE        ;ATTEND UNE VIRGULE
FINDVAR   = $DFE3        ;RECHERCHE UN VARIABLE
WAITROM   = $FCA8        ;ROUTINE TEMPO (ROM)
HOMEROM   = $FC58        ;HOME (ROM)
PTRGET    = $DFE3        ;RECHERCHE VARIABLE
GARBAGE   = $E484
MOVFM     = $EAF9        ;TRANSFERT FAC>VARIABLE
SETVAR    = $EB27        ; '' ''
FLOAT     = $EB93        ;A => FAC
PRINORM   = $FDF0
NORINPUT  = $FD1B        ;ENTREE NORMALE D'UN CARACT
PRINTROM  = $DAD5        ;PRINT (ROM)
;
; SCRATCHPAD
;
SLOT16    = $6
SLOTCARD  = $7
STACKLVL  = $8
TEMPOL    = $9
TEMPOH    = $18
TEMPO     = $19
TEMP      = $1A          ;STOCKAGE TEMPORAIRE
MINUS     = $1A
TEMP2     = $1B
XCOOR     = $1C
MINUS2    = $1C
YCOOR     = $1D
LENGTH    = $1E
CARINP    = $FE
LEN2      = $CE
;
; TOKENS UTILISES
;
CLEAR_T    = 189
COLOR_T    = 160
DRAW_T     = 148
END_T      = 128
FLASH_T    = 159
GET_T      = 190
GR_T       = 136
HCOLOR_T   = 146
HOME_T     = 151
POS_T      = $D9
INPUT_T    = 132
INVERSE_T  = 158
NORMAL_T   = 157
NOTRACE_T  = $9C
PLOT_T     = $8D
PRINT_T    = 186
READ_T     = 135
SCALE_T    = 153
STEP_T     = 199
TEXT_T     = 137
TRACE_T    = $9B
VTAB_T     = 162
WAIT_T     = 181
XDRAW_T    = 149
LOAD_T     = $B6
DEL_T      = $85
IF_T       = $AD
ON_T       = $B4
;
; COMMANDES ROM
;
INITUART  = 0
MODESLCT  = 1
DTAFORMT  = 2
LINEREL   = 3
VIDEOREL  = 4
SETRTS    = 5
EMITSTAT  = 6
SENDCHAR  = 7
RCVSTAT   = 8
READCHAR  = 9
ASCDIAL   = 10
RINGTEST  = 11
ANSWER    = 12
SMARTCD   = 13
CLEANUP   = 14
BIGBELL   = 15
;
; CODES VIDEOTEX
;
BELL      = $7
BS        = $8
HT        = $9
LF        = $A
VT        = $B
FF        = $C
CR        = $D
SO        = $E
SI        = $F
CSRON     = $11
REP       = $12
SEP       = $13
CSROFF    = $14
SS2       = $16
CAN       = $18
SUB       = $1A
ESC       = $1B
RS        = $1E
US        = $1F
LINE      = $5A
NOLINE    = $59
ENVOI     = $41
RETOUR    = $42
REPETITI  = $43
GUIDE     = $44
ANNULATI  = $45
SOMMAIRE  = $46
CORRECTI  = $47
SUITE     = $48
CONFIN    = $49
;
; CONSTANTES
;
JOUR      = $26

.include "apple_enc.inc"
.enc "none"

;
; MISE EN PLACE...
;
INIT      lda #<DEBUT
          sta AMPER+1
          lda #>DEBUT
          sta AMPER+2
          lda #$4C
          sta AMPER
          lda #32
          sta CARINP
          lda #<INIT
          sta FREE
          lda #>INIT
          sta FREE+1
          lda #0
          sta XCOOR
          bit V1OFF
          bit V2OFF
          bit V3OFF
          rts
CARSEND   .byte 0
CAREP     .byte 0
NBREP     .byte 0
NORPRINT  .word 0
NOPAGE    .byte 0
;
; PRGM
;
DEBUT     pha
          jsr CHRGET
          pla
          ldx #CMDADDR-CMDNAME
NEXTCMD   cmp CMDNAME-1,x
          beq CMDFOUND
          dex
          bne NEXTCMD
          jmp SYNTERR
CMDFOUND  dex
          txa
          asl
          tax
          lda CMDADDR+1,x
          pha
          lda CMDADDR,x
          pha
          tsx
          inx
          inx
          stx STACKLVL
          rts            ;RETOUR PAR LA COMMANDE
CMDNAME   .byte CLEAR_T,COLOR_T,DRAW_T,END_T,FLASH_T,GET_T,GR_T
          .byte HCOLOR_T,HOME_T,POS_T,INPUT_T,INVERSE_T,NORMAL_T
          .byte TRACE_T,NOTRACE_T,PLOT_T
          .byte PRINT_T,READ_T,SCALE_T,STEP_T,TEXT_T,VTAB_T,WAIT_T,XDRAW_T,LOAD_T
          .byte DEL_T,IF_T,ON_T
CMDADDR   .word CLEAR-1,COLOR-1,DRAW-1,END-1,FLASH-1,GET-1,GR-1
          .word HCOLOR-1,HOME-1,POS-1,INPUT-1,INVERSE-1,NORMAL-1
          .word TRACE-1,NOTRACE-1,PLOT-1
          .word PRINT-1,READ-1,SCALE-1,STEP-1,TEXT-1,VTAB-1,WAIT-1,XDRAW-1
          .word LOAD-1,DEL-1,IF-1,ON-1
;
; POINT D'ENTREE ROM
;
ROM       jmp $C211
;
; ROUTINE MSGOUT
;
MSGOUT    pla
          sta TEMP
          pla
          sta TEMP+1
          ldy #0
MSG2      inc TEMP
          bne MSG3
          inc TEMP+1
MSG3      lda (TEMP),y
          beq MSG4
          ora #$80
          jsr COUT
          jmp MSG2
MSG4      lda TEMP+1
          pha
          lda TEMP
          pha
          rts
;
;
;
INTERUPT  pha
          txa
          pha
          tya
          pha
          jsr INTL
          pla
          tay
          pla
          tax
          pla
          inc CHRGOT
          bne INTRTS
          inc CHRGOT+1
INTRTS    rts
INTL      lda FINBUF
          sec
          sbc DEBUTBUF
          cmp #$FF
          beq INTRTS
          ldy #RCVSTAT
          jsr ROM
          and #1
          beq INTRTS
          ldy #READCHAR
          bit V1ON
          jsr ROM
          jsr SCROLLR
          and #$7F
          pha
          bit V1OFF
          pla
          cmp #CR
          beq ISRESET
RETINT    jsr STABUF
          inc FINBUF
          bne INTL
;
STABUF    bit $C089
          bit $C089
          .byte $8D
FINBUF    .byte $00
          .byte $D0
          bit $C08A
          rts
;
LDABUF    bit $C088
          bit $C088
          .byte $AD
DEBUTBUF  .byte $00
          .byte $D0
          bit $C08A
          rts
;
;

ISRESET   ldx KSW
          cpx #<NORINPUT
          beq RETINT
RESET     jmp $3D0       ;INTDOS !!!
;
;
;
BFRON     ldx #5
LOOPON    lda BFR1,x
          sta CHRGET,x
          dex
          bpl LOOPON
          rts
BFR1      .byte $20             ; JSR
          .word INTERUPT        ; INTERRUPT addr
          .byte $EA,$EA,$EA     ; 3 NOP
          rts
;
BFROFF    ldx #5
LOOPOFF   lda BFR2,x
          sta CHRGET,x
          dex
          bpl LOOPOFF
          rts
BFR2      inc CHRGOT
          bne WAIT2PT
          inc CHRGOT+1
;
;
;
WAIT2PT   ldy #0
          lda (CHRGOT),y
          beq END2PT
          cmp #':'
END2PT    rts
;
;
;
SENDESC   sta TEMP
          lda #ESC
          jsr SEND5
          lda TEMP
          jmp SEND5
SENDIT    sta CARSEND
          lda XCOOR
          cmp #$FF
          beq SEND1
          jsr PORTEUSE
SEND1     lda CARSEND
          cmp CAREP
          beq REP1
SEND6     lda NBREP
          beq SEND2
          lda NBREP
          cmp #1
          beq SEND4
REPEND    lda #REP
          jsr SEND3
          lda NBREP
          clc
          adc #$40
          jsr SEND3
          jmp SEND2
REP1      lda CARSEND
          cmp #$20
          bmi SEND2
          lda NBREP
          cmp #63
          beq REPEND
          inc NBREP
          rts
SEND4     lda CAREP
          jsr SEND3
SEND2     lda #0
          sta NBREP
          lda CARSEND
          sta CAREP
SEND3     pha
          jsr SCROLLS
          bit V2ON
          pla
          ldy #SENDCHAR
          jsr ROM
          bit V2OFF
          jmp INTL
SEND5     sta CARSEND
          jsr SEND6
          lda #0
          sta CAREP
          rts
;
;
;
SCROLLS   pha
          ldy #0
SCLOOP2   lda $481,y
          sta $480,y
          iny
          cpy #39
          bne SCLOOP2
          pla
          jsr CONTROL
          sta $4A7
          rts
;
;
;
CONTROL   and #$7F
          cmp #$1F
          bmi CONRTS
          ora #$80
CONRTS    rts
;
;
;
PORTEUSE  ldy #RCVSTAT
          jsr ROM
          and #%00001000
          beq LOST
          rts
;
;
;
READIT    jsr READIT2
          ldx #READIT2-FILTBL-1
READIT3   cmp FILTBL,x
          beq READIT
          dex
          bpl READIT3
          rts
FILTBL    .byte $1B,$07,$03,$00
READIT2   lda FINBUF
          cmp DEBUTBUF
          beq READOLD
          jsr LDABUF
          inc DEBUTBUF
          rts
READOLD   lda #0
          sta TEMPOL
          sta TEMPOH
          lda #-11
          sta SLOT16
WAITCHAR  ldy #RCVSTAT
          jsr ROM
          sta TEMPO
          and #%00001000
          beq LOST
          lda TEMPO
          and #%00000001
          bne RECEIVED
          inc TEMPOL
          bne WAITCHAR
          inc TEMPOH
          bne WAITCHAR
          inc SLOT16
          bne WAITCHAR
LOST      jsr END
          ldx STACKLVL
          txs
          lda KSW
          cmp #<NORINPUT
          beq LOST2
          jmp $C600
LOST2     bit ONERR
          bmi ERROR
          jmp BASRUN
RECEIVED  bit V1ON
          ldy #READCHAR
          jsr ROM
          pha
          bit V1OFF
          pla
          jsr SCROLLR
          and #$7F
          rts
ERROR     lda #17
          jmp BASERR
;
;
;
SCROLLR   ora #$80
          pha
          ldy #0
SCLOOP    lda $401,y
          sta $400,y
          iny
          cpy #39
          bne SCLOOP
          pla
          jsr CONTROL
          sta $427
          rts
VALEUR    jsr $E6F8
          lda $A1
          rts
;
;
;
OUTMEM    lda #77
          jmp BASERR
;
; PRISE DE MAIN PAR LE MINITEL...
;
ON        jsr VALEUR
          beq OFF
          lda #<SAVPRINT
          sta CSW
          lda #>SAVPRINT
          sta CSW+1
          lda #<ON2
          sta KSW
          lda #>ON2
          sta KSW+1
          jsr HOME
          jsr CLEAR
          lda #1
          jsr STEPIN
          lda #17
          jmp SENDIT
OFF       lda #<PRINORM
          sta CSW
          lda #>PRINORM
          sta CSW+1
          lda #<NORINPUT
          sta KSW
          lda #>NORINPUT
          sta KSW+1
          rts
ONVAR     .word 0
ON2       stx ONVAR
          sty ONVAR+1
ONIN      lda #17
          jsr SEND5
          jsr READIT
          cmp #SEP
          bne ONRTS
ON3       jsr READIT
          cmp #SEP
          beq ON3
          and #$0F
          tax
          lda ONTBL,x
          bmi ON4
ONRTS     ldx ONVAR
          ldy ONVAR+1
          ora #$80
          rts
ONTBL     .text x"200D2080201880082020202020202020"
ON4       cpx #3
          bne ON5
          jsr HOME
          jmp ONIN
ON5       jsr READIT
          and #$1F
          jmp ONRTS
;
; WAIT: INITIALISATION ET ATTENTE
;
WAIT      jsr MSGOUT
          .null 'ATTENTE',$0d
          bit V3OFF
          jsr FINDSLOT   ;CHERCHE LA CARTE
          bcs WAIT2      ;TROUVEE ??
          jsr MSGOUT     ;MSG ERREUR
          .enc "apple"
          .null $0d,"PAS DE CARTE !",$0d
          .enc "none"
          jmp SYNTERR
WAIT2     lda SLOTCARD
          sta ROM+2
          lda #0
          sta DOSLEN
          sta DOSLEN+1
          sta DOSADD
          sta DOSADD+1
          ldy #INITUART  ;RAZ CARTE
          lda #$07
          jsr ROM
          ldy #LINEREL
          lda #0
          jsr ROM        ;RACCROCHE
          bit $C010
          ldy #BIGBELL
          lda #0
          jsr ROM        ;WAITS FOR RING
          ldy #LINEREL
          lda #$FF
          jsr ROM        ;DECROCHE
          bit V3ON
          ldy #DTAFORMT  ;SETS DATA FORMAT
          lda #%00100111 ;TO 7 BITS 1 STOP
          jsr ROM        ;EVEN PARITY
          ldy #ANSWER
          jsr ROM        ;CONNEXION TYPE CCITT V23
          ldy #SETRTS
          lda #1
          jsr ROM        ;SEND CARRIER
          bit $C010
          ldy #SMARTCD
          lda #76        ;CHERCHE PORTEUSE
          ldx #170       ;PENDANT 30s
          jsr ROM        ;PORTEUSE: 1,7s
          bne WAITOK
          jsr MSGOUT
          .enc "apple"
          .null $0d,$0d,"PORTEUSE ?",$0d
          .enc "none"
          jmp WAIT
WAITOK    jsr CLEAR
          jsr BFRON
          lda #0
          jsr STEPIN
          jmp HOME       ;CONNEXION OK
;
;
;
CLEAR     lda #0
          sta FINBUF
          sta DEBUTBUF
          ldy #CLEANUP
          jmp ROM
;
;
;
HOME      lda #FF        ;EFFACE ECRAN MINITEL
          jsr SENDIT
          lda #0
          sta TEMP
          jsr POS1
          jsr DEL2
          lda #RS        ;RETOUR 'HOME'
          jmp SEND5
;
;
;
VTAB      jsr VALEUR
          sta TEMP
POS1      lda #1
POSMNTL   sta TEMP2
          lda #US
          jsr SEND5
          lda TEMP
          clc
          adc #$40
          jsr SEND5
          lda TEMP2
          clc
          adc #$40
          jmp SEND5
;
;
;
POS       jsr VALEUR
          sta TEMP
          jsr WAIT2PT
          beq POS2
          jsr CHKCOM
          jsr VALEUR
          jmp POSMNTL
POS2      lda #1
          jmp POSMNTL
;
;
;
;
;
DEL       jsr POS
DEL2      lda #CAN
          jmp SEND5
;
;
;
LOAD      jsr VALEUR
          sta NOPAGE
          asl
          asl
          pha
          jsr CHKCOM
          jsr BLOAD
          pla
          tax
          lda DOSADD
          sta DOSTABL,x
          inx
          lda DOSADD+1
          sta DOSTABL,x
          inx
          lda DOSLEN
          sta DOSTABL,x
          inx
          lda DOSLEN+1
          sta DOSTABL,x
          rts
DOSTABL   .fill 32
;
;
;
BLOAD     jsr MSGOUT
          .byte $0D,$04
          .text 'BLOAD'
          .byte $00
          ldx #1
          jsr BANKSEL
          lda #14
          jsr PRINTROM
          ldx #2
          jmp BANKSEL
;
;
;
BANKSEL   txa
          clc
          adc #$80
          sta RAM+1
          tax
          ldy NOPAGE
          cpy #6
          bmi RAM2
          txa
          clc
          adc #8
          sta RAM+1
RAM2      jsr RAM
RAM       lda $C080
          rts
;
;
;
FLASH     lda #'H'
          jmp SENDESC
;
;
;
INVERSE   lda #']'
          jmp SENDESC
;
;
;
NORMAL    lda #'I'
          jsr SENDESC
          lda #'\'
          jmp SENDESC
;
;
;
COLOR     jsr VALEUR
          clc
          adc #$40
          jmp SENDESC
;
;
;
HCOLOR    jsr VALEUR
          clc
          adc #$50
          jmp SENDESC
;
;
;
TEXT      lda #SI
          jmp SENDIT
;
;
;
GR        lda #SO
          jmp SENDIT
;
;
;
READ      ldy #19
READLOOP  lda TABHORL,y
          bmi READ2
          jsr READHORL
          sta BUFFER,y
READNEXT  dey
          bpl READLOOP
          lda #JOUR
          jsr READHORL
          and #$07
          asl
          tay
          lda TABJOUR,y
          sta BUFFER+9
          lda TABJOUR+1,y
          sta BUFFER+10
          lda #20
          sta LEN2
          lda #0
          sta TEMP
          jmp CARSEP2
READ2     and #3
          tax
          lda TABCAR,x
          sta BUFFER,y
          bpl READNEXT   ; JMP READNEXT
READHORL  sta HORL
          lda HORL
          sta TEMP
          cpy #0
          bne HORL2
          and #$3
HORL2     cpy #12
          bne HORL3
          and #3
HORL3     clc
          adc #$30
          rts
TABCAR    .text '- /'
TABHORL   .text x"2524802322802120818181812827822A29822C2B"
TABJOUR   .text 'DILUMAMEJEVESA'
;
;
;
PRINT     lda #$FF
          sta XCOOR
          jsr WAIT2PT
          beq PRINT3
          lda #0
          sta TEMP
          lda CSW
          sta NORPRINT
          lda CSW+1
          sta NORPRINT+1
          lda #<SAVPRINT
          sta CSW
          lda #>SAVPRINT
          sta CSW+1
          jsr PRINTROM
PRINERR   lda NORPRINT
          sta CSW
          lda NORPRINT+1
          sta CSW+1
          lda #0
          sta XCOOR
          rts
SAVPRINT  and #%01111111
          pha
          jsr PRINTIT
          pla
          cmp #CR
          beq CROUT
          rts
CROUT     lda #LF
PRINTIT   stx LENGTH
          sty TEMP2
          jsr SENDIT
          ldy TEMP2
          ldx LENGTH
          rts
PRINT3    lda #CR
          jsr SENDIT
          lda #LF
          jmp SENDIT
;
;
;
SCALE     jsr VALEUR
          clc
          adc #76
          jmp SENDESC
;
;
;
END       jsr BFROFF
          bit V3OFF
          bit V2OFF
          bit V1OFF
          ldy #LINEREL
          lda #0
          jmp ROM        ;RACCROCHE
;
;
;
TRACE     lda #LINE
          jmp SENDESC
;
;
;
NOTRACE   lda #NOLINE
          jmp SENDESC
;
;
;
PLOT      jsr VALEUR
          jsr SENDIT
          jsr CHKCOM
          lda #REP
          jsr SENDIT
          jsr VALEUR
          adc #$3E
          jmp SENDIT
;
;
;
XDRAW     lda #0
          sta NOPAGE
          jsr BLOAD
          jmp DRAWBIS
;
;
;
INPUT     jsr VALEUR
          sta XCOOR
          jsr CHKCOM
          jsr VALEUR
          sta YCOOR
          jsr CHKCOM
          jsr VALEUR
          sta LENGTH
          jsr CHKCOM
          jsr POSINP
          lda CARINP
INPUTIN   jsr SEND5
          lda LENGTH
          cmp #1
          beq ONLY1
          lda #REP
          jsr SEND5
          lda LENGTH
          clc
          adc #$3F
          jsr SEND5
ONLY1     jsr POSINP     ;CURSOR = ON
          lda #CSRON
          jsr SEND5
          lda #0
          sta LEN2
WAITCAR   jsr READIT
          sta TEMP
          cmp #SEP
          beq CARSEP
          lda TEMP
          ldx LEN2
          sta $200,x
          ldx LEN2
          cpx LENGTH
          beq FULL
          lda SLOTCARD
          bpl INPUT1
          lda TEMP
INPUT1    jsr SEND3
          inc LEN2
          jmp WAITCAR
FULL      lda #BELL
          jsr SEND5
          jmp WAITCAR
CARSEP    jsr READIT
          sta TEMP
          cmp #CORRECTI
          beq CORRECT
          lda TEMP
          cmp #ANNULATI
          bne CARSEP3
          jsr POSINP
          lda CARINP
          jmp INPUTIN
CORRECT   lda LEN2
          beq WAITCAR
          lda #BS
          jsr SEND5
          lda CARINP
          jsr SEND5
          lda #BS
          jsr SEND5
          dec LEN2
          jmp WAITCAR
CARSEP3   lda #20
          jsr SEND5
CARSEP2   lda #0
          sta LENGTH
          lda TEMP
          and #$F
          sta $FF
LOOP2     lda HIMEM
          sec
          sbc LOMEM
          sec
          sbc LEN2
          sta TEMP
          lda HIMEM+1
          sbc LOMEM+1
          sta TEMP2
          bcs LOOPOK
          lda LENGTH
          cmp #1
          bne LOOP5
          jmp OUTMEM
LOOP5     inc LENGTH
          jsr GARBAGE
          jmp LOOP2
LOOPOK    lda HIMEM
          sec
          sbc LEN2
          sta HIMEM
          lda HIMEM+1
          sbc #0
          sta HIMEM+1
          jsr PTRGET
          ldy #0
          lda LEN2
          sta (PTRVAR),y
          iny
          lda HIMEM
          sta (PTRVAR),y
          iny
          lda HIMEM+1
          sta (PTRVAR),y
LOOP3     iny
          lda #0
          sta (PTRVAR),y
          cpy #4
          bne LOOP3
          ldy #0
LOOP4     cpy LEN2
          beq INPUTEND
          lda $200,y
          sta (HIMEM),y
          iny
          bne LOOP4
INPUTEND  rts
POSINP    lda XCOOR
          sta TEMP
          lda YCOOR
          jmp POSMNTL
;
;
;
IF        lda FINBUF
          cmp DEBUTBUF
          bne GET
          ldy #RCVSTAT
          jsr ROM
          and #1
          bne GET
          lda #0
          sta BUFFER
          beq GET3
GET       jsr READIT
          sta BUFFER
          cmp #SEP
          bne GET3
          jsr READIT
          and #$F
          sta BUFFER
GET3      lda #1
          sta LEN2
          lda #0
          sta TEMP
          jmp CARSEP2
;
;
;
DRAW      jsr WAIT2PT
          beq DRAWBIS
          jsr VALEUR
          sta NOPAGE
          asl
          asl
          tax
          lda DOSTABL,x
          sta DOSADD
          inx
          lda DOSTABL,x
          sta DOSADD+1
          inx
          lda DOSTABL,x
          sta DOSLEN
          inx
          lda DOSTABL,x
          sta DOSLEN+1
DRAWBIS   lda DOSADD
          sta TEMP
          clc
          adc DOSLEN
          sta LENGTH
          lda DOSADD+1
          sta TEMP2
          adc DOSLEN+1
          sta LEN2
DRAW1     ldx #0
          jsr BANKSEL
          ldy #0
          lda (TEMP),y
          pha
          ldx #2
          jsr BANKSEL
          pla
          jsr SENDIT
          inc TEMP
          bne DRAW2
          inc TEMP2
DRAW2     lda TEMP
          cmp LENGTH
          bne DRAW1
          lda TEMP2
          cmp LEN2
          bne DRAW1
          rts
;
;
;
STEP      jsr VALEUR
STEPIN    sta TEMP2
          lda #':'
          jsr SENDESC
          lda #'j'
          sec
          sbc TEMP2
          jsr SENDIT
          lda #'C'
          jmp SENDIT

;
; FINDSLOT DE L'APPLE-TELL
;
POINTL    = TEMP
POINTH    = POINTL+1
                         ;
FINDSLOT  lda #<$C700
          sta POINTL
          ldx #>$C700      ;commencer en slot 7
FIND1     stx POINTH
          ldy #$0B        ;tester l'octet de signature generique
          lda #$01        ;signature generique des cartes avec firmware
          cmp (POINTL),y
          bne FIND3       ;pas bon
          cmp (POINTL),y
          bne FIND3       ;pas bon la deuxieme fois...
          iny             ;tester notre signature de peripherique
          lda #$49        ;signature de l'APPLE-TELL
          cmp (POINTL),y
          bne FIND3       ;pas bonne
          cmp (POINTL),y  ;deuxieme essai...
FIND3     sta $CFFF       ;debrancher la ROM d'extension
          beq FIND2       ;on a trouve...
          dex
          cpx #>$C100      ;en est-on plus loin que le slot 1 ?
          bcs FIND1       ;non, essayer encore
          rts
FIND2     txa
          sta SLOTCARD
          asl
          asl
          asl
          asl
          sta SLOT16
          sec
          rts             ;carte trouvee, retour avec C=1
