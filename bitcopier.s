;;****************************
;                            *
; BITCOPIER I                *
; COPYRIGHT J._A.L.B. 1982   *
; LISTING : P.CHATAIGNON     *
;                            *
;*****************************

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
A1H         =     $3d
A2L         =     $3e
A2H         =     $3f
PREG        =     $48

DRVOFF = $C088
DRVON = $C089
INBYT = $C08C
OUTBYT = $C08D
DRVCTL1 = $C08E
DRVCTL2 = $C08F
RDKEY = $FD0C
HOME = $FC58
VTAB = $FC22
COUT = $FDED
CROUT = $FD8E
WAIT = $FCA8
TEST = $7000
GETIOB = $03E3
RWTS = $03D9
XAM = $FDB3
SCREEN = $0583

ptr1 = $FA
ptr2 = $FC
trkstart = $02
trkend = $03

dlm1 = $F9
dlm2 = $05
dlm3 = $F8
dlm4 = $F7

ptr0 = $00

var1 = $FF
var2 = $FE

.include "apple_enc.inc"
.include "macros.inc"

.enc "apple"

* = $2B00
            jmp START

;***********************
;                      *
; TABLE CARACTERES     *
;                      *
;***********************


TABLE1      .text "INTRODUISEZ VOS DISQUETTES S.V.P:"
            .byte 0
TABLE2      .text "APPLE BITCOPIER (C) 1982         "
            .text 'JALB '
            .text " "
            .byte 0

TIRET       ldx #40
            lda #"="
-           jsr COUT
            dex
            bne -
            rts


;***********************
;                      *
; DEBUT PROGRAMME      *
;                      *
;***********************

START       jsr HOME
            ldx #0
            sta CH
            lda #6
            sta CH+1
            jsr VTAB
            jsr TIRET
            ldx #0
-           lda TABLE2,x
            beq +
            jsr COUT
            inx
            bne -
+           jsr CROUT
            jsr TIRET
            lda #4
            sta CH
            lda #12
            sta CV
            jsr VTAB
            ldx #0
-           lda TABLE1,x
            beq +
            jsr COUT
            inx
            bne -
+           jsr RDKEY

;***********************
;                      *
; INITIALISATION       *
;                      *
;***********************

INIT        lda #0 ;PISTE DEPART
            sta trkstart
            lda #35 ;PISTE FIN
            sta trkend
            lda #$D5 ;MARQUEURS
            sta dlm1
            lda #$AA
            sta dlm2
            lda #$96
            sta dlm3
            lda #$B5
            sta dlm4
            jsr HOME
            jsr AFFTRK
            jsr PISTE0
            jmp PGM01

;***********************
;                      *
; ECRITURE             *
;                      *
;***********************

ECRIT       lda #2
            jsr SEEK0
            lda DRVON,x
            ldy #5
-           lda #$FF
            jsr WAIT
            dey
            bne -
            ldy #$FF
            lda OUTBYT,x
            lda DRVCTL1,x
            lda #$FF
            sta DRVCTL2,x
            ora INBYT,x
            clc
            clc
            clc
            clc
            clc
            lda #$FF
            jsr WAIT12
ECR2        pha
            pla
            sta OUTBYT,x
            ora INBYT,x
            dey
            beq ECR3
            jsr WAIT12
            lda #$FF
            stx $60
            jmp ECR2
ECR3        lda TEST,y
            clc
            clc
            iny
            pha
            pla
            jmp ECR5
ECR4        lda TEST,y
PTR = *-1
            iny
            beq ECR7
            cmp #0
            beq ECR8
            clc
            clc
            clc
ECR5        clc
ECR6        sta OUTBYT,x
            ora INBYT,x
            jmp ECR4
ECR7        inc PTR
            clc
            jmp ECR6
ECR8        lda DRVCTL1,x
            lda DRVOFF,x
            lda #$70
            sta PTR
WAIT12      rts

;***********************
;                      *
; REMET DRIVE PISTE 0  *
;                      *
;***********************

SEEK0       pha
            jsr GETIOB
            sty ptr0
            sta ptr0+1
            pla
            ldy #$02
            sta (ptr0),y
            lda trkstart
            ldy #$04
            sta (ptr0),y
            lda #$00
            ldy #$0C
            sta (ptr0),y
            jsr GETIOB
            jsr RWTS
            lda #0
            sta PREG
            ldy #$01
            lda (ptr0),y
            tax
            rts

;***********************
;                      *
; LECTURE              *
;                      *
;***********************

; read $3000-$6EFF

LECTURE     lda #$01
            jsr SEEK0
LECT1       ldx #$60
            lda DRVON,x
            lda DRVCTL1,x
            lda #$00
            sta ptr0
            lda #$30
            sta ptr0+1
            ldy #0
-           lda INBYT,x
            bpl -
            sta (ptr0),y
            inc ptr0
            bne -
            inc ptr0+1
            lda ptr0+1
            cmp #$6F
            bcc -
            lda DRVOFF,x
            rts

;***********************
;                      *
; BOUCLE PRINCIPALE    *
;                      *
;***********************

PGM01       lda #'R'
            jsr AFFICH
            lda #0
            sta var1
PGM02       jsr LECTURE
            lda #'A'
            jsr AFFICH
            jsr ANALYSE
            bcs PGMCS       ; si carry
            ldy #$00        ; ptr2 = $7000
            sty ptr2
            lda #$70
            sta ptr2+1
-           lda (ptr1),y     ; copie depuis (ptr1) -> (ptr2 = $7000-$8FFF)
            sta (ptr2),y     ; ptr1 computed in ANALYSE
            iny
            bne -
            inc ptr1+1
            inc ptr2+1
            lda ptr2+1
            cmp #$90        ; max $9000
            bne -
            lda #5
            sta CV
            lda #0
            sta CH
            sta A1L
            lda #$70
            sta A1H
            sta A2H
            lda #$5F
            sta A2L
            jsr XAM         ; IMPRIME $7000-$705F
            move #$7000,ptr1
            move #$7500,ptr2
            ldy #$00
            ldx #$00
            stx var2
PGM04       lda (ptr2),y
            cmp TEST,x
            beq PGM06
PGM05       iny
            bne PGM04
            inc ptr2+1
            inc var2
            lda var2
            cmp #$20
            bne PGM04
            jmp PGM13
PGM06       tya
            pha
            lda ptr2+1
            pha
PGM07       inx
            cpx #$0A
            beq PGM15
            iny
            bne PGM08
            inc ptr2+1
PGM08       lda (ptr2),y
            cmp TEST,x
            beq PGM07
            pla
            sta ptr2+1
            pla
            tay
            ldx #$00
            jmp PGM05
PGMCS       lda var1
            cmp #$04
            beq PGM10
            inc var1
            lda #'R'
            jsr AFFICH
            jmp PGM02
PGM10       lda #'1'
            jsr AFFICH
            jmp PGM21
PGM11       lda var1
            cmp #$04
            beq PGM12
            inc var1
            jmp PGM16
PGM12       lda #'2'
            jsr AFFICH
            jmp PGM21
PGM13       lda var1
            cmp #$04
            beq PGM14
            inc var1
            lda #'R'
            jsr AFFICH
            jmp PGM02
PGM14       lda #'3'
            jsr AFFICH
            jmp PGM21
PGM15       pla
            sta ptr2+1
            pla
            tay
            lda #$00
            sta (ptr2),y
PGM16       lda #'W'
            jsr AFFICH
            jsr ECRIT
            lda #'V'
            jsr AFFICH
            jsr LECT1
            lda #'A'
            jsr AFFICH
            lda #$30
            sta ptr2+1
            ldy #$00
            sta ptr2
            ldx #$00
PGM17       lda (ptr2),y
            cmp TEST,x
            bne PGM18
            inx
            cpx #$10
            beq PGM20
            bne PGM19
PGM18       ldx #$00
PGM19       iny
            bne PGM17
            inc ptr2+1
            lda ptr2+1
            cmp #$4F
            bne PGM17
            jmp PGM11
PGM20       lda #$B0
            jsr AFFICH
PGM21       lda trkstart
            cmp trkend
            beq +
            inc trkstart
            jmp PGM01
+           brk

;***********************
;                      *
; ANALYSE              *
;                      *
;***********************

ANALYSE     lda #$00            ; init ptr1 = $3000
            tay
            sta ptr1
            lda #$30
            sta ptr1+1
ANA01       lda (ptr1),y
            cmp dlm1
            beq ANA03
ANA02       iny
            bne ANA01
            inc ptr1+1
            lda ptr1+1
            cmp #$4F            ; max $4EFF
            bne ANA01
            jmp ANA08
ANA03       tya
            pha
            lda ptr1+1
            pha
            iny
            bne ANA04
            inc ptr1+1
ANA04       lda (ptr1),y
            cmp dlm2
            bne ANA07
            iny
            bne ANA05
            inc ptr1+1
ANA05       lda (ptr1),y
            cmp dlm3
            beq ANA06
            cmp dlm4
            bne ANA07
ANA06       pla
            sta ptr1+1
            pla
            tay
            clc
            adc ptr1
            sta ptr1
            lda #$00
            adc ptr1+1
            sta ptr1+1
            clc
            rts
ANA07       pla
            sta ptr1+1
            pla
            tay
            jmp ANA02
ANA08       lda #$00
            tay
            sta ptr1
            lda #$30
            sta ptr1+1
ANA09       jsr SYNCR
            bcs ANA12
            lda (ptr1),y
            cmp dlm1
            bne ANA09
            lda ptr1+1
            pha
            iny
            bne ANA10
            inc ptr1+1
ANA10       lda (ptr1),y
            cmp dlm2
            bne ANA11
            pla
            sta ptr1+1
            clc
            rts
ANA11       pla
            sta ptr1+1
            jmp ANA09
ANA12       lda #$00
            tay
            sta ptr1
            lda #$30
            sta ptr1+1
ANA13       jsr SYNCR
            bcs ANA14
            lda (ptr1),y
            cmp dlm1
            bne ANA13
            clc
            rts
ANA14       lda #$00
            tay
            sta ptr1
            lda #$30
            sta ptr1+1
            jsr SYNCR
            rts

;***********************
;                      *
; ANALYSE NON STANDARD *
;                      *
;***********************

SYNCR       ldx #$00
SYN01       lda (ptr1),y
            cmp #$FF
            bne SYN03
            inx
            cpx #dlm2
            beq SYN05
SYN02       iny
            bne SYN01
            inc ptr1+1
            lda ptr1+1
            cmp #$4F
            beq SYN04
            bne SYN01
SYN03       ldx #$00
            jmp SYN02
SYN04       sec
            rts
SYN05       iny
            bne SYN06
            inc ptr1+1
            lda ptr1+1
            cmp #$4F
            beq SYN04
SYN06       lda (ptr1),y
            cmp #$FF
            beq SYN05
            clc
            tya
            adc ptr1
            sta ptr1
            lda #$00
            adc ptr1+1
            sta ptr1+1
            ldy #$00
            clc
            rts

;***********************
;                      *
; AFFICHAGE            *
;                      *
;***********************

AFFICH      ldx trkstart
            sta SCREEN,x
            rts
AFFTRK      ldx #$00
AFF1        lda TEXTE,x
            cmp #$00
            beq AFFRTS
            jsr COUT
            inx
            jmp AFF1
AFFRTS      rts

;***********************
;                      *
; REMET 2 DRIVES SUR 0 *
;                      *
;***********************

PISTE0      lda trkstart
            pha
            lda #$FF
            sta trkstart
            lda #1
            jsr SEEK0
            lda #2
            jsr SEEK0
            pla
            sta trkstart
            rts

;***********************
;                      *
; AFFICHAGE ECRAN      *
;                      *
;***********************

TEXTE
            .text "TR 000000000000000011111111111111112222"
            .byte $8D
            .text "AC 0123456789ABCDEF0123456789ABCDEF0123"
            .byte $8D
            .text "---------------------------------------"
            .byte $8D
            .text "ER"
            .byte $8D
            .text "---------------------------------------"
            .byte $8D
            .byte 0
