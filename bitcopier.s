;;****************************
;                            *
; BITCOPIER I                *
; COPYRIGHT J._A.L.B. 1982   *
; LISTING : P.CHATAIGNON     *
;                            *
;*****************************

CH = $24
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


* = $2B00
            JMP START

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

TIRET       ldx #$28
            lda #$BD
TIR1        jsr COUT
            dex
            bne TIR1
            rts


;***********************
;                      *
; DEBUT PROGRAMME      *
;                      *
;***********************

START       jsr HOME
            ldx #$00
            sta CH
            lda #$06
            sta CH+1
            jsr VTAB
            jsr TIRET
            ldx #$00
ST1         lda TABLE2,X
            beq ST2
            jsr COUT
            inx
            bne ST1
ST2         jsr CROUT
            jsr TIRET
            lda #$04
            sta $24
            lda #$0C
            sta $25
            jsr VTAB
            ldx #$00
ST3         lda TABLE1,X
            beq ST4
            jsr COUT
            inx
            bne ST3
ST4         jsr RDKEY

;***********************
;                      *
; INITIALISATION       *
;                      *
;***********************

INIT        lda #$00 ;PISTE DEPART
            sta $02
            lda #$23 ;PISTE FIN
            sta $03
            lda #$D5 ;MARQUEURS
            sta $F9
            lda #$AA
            sta $05
            lda #$96
            sta $F8
            lda #$B5
            sta $F7
            jsr HOME
            jsr AFFTRK
            jsr PISTE0
            jmp PGM01

;***********************
;                      *
; ECRITURE             *
;                      *
;***********************

ECRIT       lda #$02
            jsr SEEK0
            lda DRVON,X
            ldy #$05
ECR1        lda #$FF
            jsr WAIT
            dey
            bne ECR1
            ldy #$FF
            lda OUTBYT,X
            lda DRVCTL1,X
            lda #$FF
            sta DRVCTL2,X
            ora INBYT,X
            clc
            clc
            clc
            clc
            clc
            lda #$FF
            jsr WAIT12
ECR2        pha
            pla
            sta OUTBYT,X
            ora INBYT,X
            dey
            beq ECR3
            jsr WAIT12
            lda #$FF
            stx $60
            jmp ECR2
ECR3        lda TEST,Y
            clc
            clc
            iny
            pha
            pla
            jmp ECR5
ECR4        lda TEST,Y
PTR = *-1
            iny
            beq ECR7
            cmp #$00
            beq ECR8
            clc
            clc
            clc
ECR5        clc
ECR6        sta OUTBYT,X
            ora INBYT,X
            jmp ECR4
ECR7        inc PTR
            clc
            jmp ECR6
ECR8        lda DRVCTL1,X
            lda DRVOFF,X
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
            sty $00
            sta $01
            pla
            ldy #$02
            sta ($00),Y
            lda $02
            ldy #$04
            sta ($00),Y
            lda #$00
            ldy #$0C
            sta ($00),Y
            jsr GETIOB
            jsr RWTS
            lda #$00
            sta $48
            ldy #$01
            lda ($00),Y
            tax
            rts

;***********************
;                      *
; LECTURE              *
;                      *
;***********************

LECTURE     lda #$01
            jsr SEEK0
LECT1       ldx #$60
            lda DRVON,X
            lda DRVCTL1,X
            lda #$00
            sta $00
            lda #$30
            sta $01
            ldy #$00
LECT2       lda INBYT,X
            bpl LECT2
            sta ($00),Y
            inc $00
            bne LECT2
            inc $01
            lda $01
            cmp #$6F
            bcc LECT2
            lda DRVOFF,X
            rts

;***********************
;                      *
; BOUCLE PRINCIPALE    *
;                      *
;***********************

PGM01       lda #'R'
            jsr AFFICH
            lda #$00
            sta $FF
PGM02       jsr LECTURE
            lda #'A'
            jsr AFFICH
            jsr ANALYSE
            bcs PGM09
            ldy #$00
            sty $FC
            lda #$70
            sta $FD
PGM03       lda ($FA),Y
            sta ($FC),Y
            iny
            bne PGM03
            inc $FB
            inc $FD
            lda $FD
            cmp #$90
            bne PGM03
            lda #$05
            sta $25
            lda #$00
            sta $24
            sta $3C
            lda #$70
            sta $3D
            sta $3F
            lda #$5F
            sta $3E
            jsr XAM ;IMPRIME A1 ($3C,$3D)
            lda #$70
            sta $FB
            ldy #$00
            sty $FA
            sty $FC
            lda #$75
            sta $FD
            ldy #$00
            ldx #$00
            stx $FE
PGM04       lda ($FC),Y
            cmp TEST,X
            beq PGM06
PGM05       iny
            bne PGM04
            inc $FD
            inc $FE
            lda $FE
            cmp #$20
            bne PGM04
            jmp PGM13
PGM06       tya
            pha
            lda $FD
            pha
PGM07       inx
            cpx #$0A
            beq PGM15
            iny
            bne PGM08
            inc $FD
PGM08       lda ($FC),Y
            cmp TEST,X
            beq PGM07
            pla
            sta $FD
            pla
            tay
            ldx #$00
            jmp PGM05
PGM09       lda $FF
            cmp #$04
            beq PGM10
            inc $FF
            lda #'R'
            jsr AFFICH
            jmp PGM02
PGM10       lda #'1'
            jsr AFFICH
            jmp PGM21
PGM11       lda $FF
            cmp #$04
            beq PGM12
            inc $FF
            jmp PGM16
PGM12       lda #'2'
            jsr AFFICH
            jmp PGM21
PGM13       lda $FF
            cmp #$04
            beq PGM14
            inc $FF
            lda #'R'
            jsr AFFICH
            jmp PGM02
PGM14       lda #'3'
            jsr AFFICH
            jmp PGM21
PGM15       pla
            sta $FD
            pla
            tay
            lda #$00
            sta ($FC),Y
PGM16       lda #'W'
            jsr AFFICH
            jsr ECRIT
            lda #'V'
            jsr AFFICH
            jsr LECT1
            lda #'A'
            jsr AFFICH
            lda #$30
            sta $FD
            ldy #$00
            sta $FC
            ldx #$00
PGM17       lda ($FC),Y
            cmp TEST,X
            bne PGM18
            inx
            cpx #$10
            beq PGM20
            bne PGM19
PGM18       ldx #$00
PGM19       iny
            bne PGM17
            inc $FD
            lda $FD
            cmp #$4F
            bne PGM17
            jmp PGM11
PGM20       lda #$B0
            jsr AFFICH
PGM21       lda $02
            cmp $03
            beq PGM22
            inc $02
            jmp PGM01
PGM22       brk

;***********************
;                      *
; ANALYSE              *
;                      *
;***********************

ANALYSE     lda #$00
            tay
            sta $FA
            lda #$30
            sta $FB
ANA01       lda ($FA),Y
            cmp $F9
            beq ANA03
ANA02       iny
            bne ANA01
            inc $FB
            lda $FB
            cmp #$4F
            bne ANA01
            jmp ANA08
ANA03       tya
            pha
            lda $FB
            pha
            iny
            bne ANA04
            inc $FB
ANA04       lda ($FA),Y
            cmp $05
            bne ANA07
            iny
            bne ANA05
            inc $FB
ANA05       lda ($FA),Y
            cmp $F8
            beq ANA06
            cmp $F7
            bne ANA07
ANA06       pla
            sta $FB
            pla
            tay
            clc
            adc $FA
            sta $FA
            lda #$00
            adc $FB
            sta $FB
            clc
            rts
ANA07       pla
            sta $FB
            pla
            tay
            jmp ANA02
ANA08       lda #$00
            tay
            sta $FA
            lda #$30
            sta $FB
ANA09       jsr SYNCR
            bcs ANA12
            lda ($FA),Y
            cmp $F9
            bne ANA09
            lda $FB
            pha
            iny
            bne ANA10
            inc $FB
ANA10       lda ($FA),Y
            cmp $05
            bne ANA11
            pla
            sta $FB
            clc
            rts
ANA11       pla
            sta $FB
            jmp ANA09
ANA12       lda #$00
            tay
            sta $FA
            lda #$30
            sta $FB
ANA13       jsr SYNCR
            bcs ANA14
            lda ($FA),Y
            cmp $F9
            bne ANA13
            clc
            rts
ANA14       lda #$00
            tay
            sta $FA
            lda #$30
            sta $FB
            jsr SYNCR
            rts

;***********************
;                      *
; ANALYSE NON STANDARD *
;                      *
;***********************

SYNCR       ldx #$00
SYN01       lda ($FA),Y
            cmp #$FF
            bne SYN03
            inx
            cpx #$05
            beq SYN05
SYN02       iny
            bne SYN01
            inc $FB
            lda $FB
            cmp #$4F
            beq SYN04
            bne SYN01
SYN03       ldx #$00
            jmp SYN02
SYN04       sec
            rts
SYN05       iny
            bne SYN06
            inc $FB
            lda $FB
            cmp #$4F
            beq SYN04
SYN06       lda ($FA),Y
            cmp #$FF
            beq SYN05
            clc
            tya
            adc $FA
            sta $FA
            lda #$00
            adc $FB
            sta $FB
            ldy #$00
            clc
            rts

;***********************
;                      *
; AFFICHAGE            *
;                      *
;***********************

AFFICH      ldx $02
            sta SCREEN,X
            rts
AFFTRK      ldx #$00
AFF1        lda TEXTE,X
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

PISTE0      lda $02
            pha
            lda #$FF
            sta $02
            lda #$01
            jsr SEEK0
            lda #$02
            jsr SEEK0
            pla
            sta $02
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
