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
GETIOB = $03E3
RWTS = $03D9
XAM = $FDB3
SCREEN = $0583

ptr1 = $FA
ptr2 = $FC
trkcurr = $02
trkend = $03

dlm1 = $F9
dlm2 = $05
dlm3 = $F8
dlm4 = $F7

ptr0 = $00

var1 = $FF
var2 = $FE

buff1 = $3000
buff1end = $4f00
buff2 = $7000
buff2end = $9000
addr1 = $7500

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
            sta trkcurr
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
ECR3        lda buff2,y
            clc
            clc
            iny
            pha
            pla
            jmp ECR5
ECR4        lda buff2,y
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
            lda trkcurr
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
            lda #0              ; init retry counter
            sta var1
PGM02       jsr LECTURE
            lda #'A'
            jsr AFFICH
            jsr ANALYSE
            bcs PGMCS           ; carry set = fail
            move buff2,ptr2   ; ptr2 = $7000
-           lda (ptr1),y        ; copy from buff1/ptr1 computed in ANALYSE
            sta (ptr2),y        ; in buff2
            iny
            bne -
            inc ptr1+1
            inc ptr2+1
            lda ptr2+1
            cmp #<buff2end      ; until end of buff2
            bne -
            lda #5
            sta CV
            lda #0
            sta CH
            sta A1L
            lda #>buff2
            sta A1H
            sta A2H
            lda #$5F
            sta A2L
            jsr XAM             ; display start ($5F bytes) of buff2
            move #buff2,ptr1    ; find 10 identical values in buffer
            move #addr1,ptr2
            ldy #0
            ldx #0
            stx var2
-           lda (ptr2),y        ; compare (ptr2),y et buff2
            cmp buff2,x
            beq +               ; match
PGM05       iny
            bne -
            inc ptr2+1
            inc var2
            lda var2
            cmp #$20
            bne -
            jmp PGM13
+           tya                 ; store ptr2H,Y on stack
            pha
            lda ptr2+1
            pha
-           inx
            cpx #10
            beq PGM15           ; 10 common values -> PGM15
            iny
            bne -
            inc ptr2+1
-           lda (ptr2),y
            cmp buff2,x
            beq -
            pla
            sta ptr2+1
            pla
            tay
            ldx #$00
            jmp PGM05

PGMCS       lda var1        ; test if #retry < 4
            cmp #4
            beq ERR1        ; fail, next track if any
            inc var1        ; incr var1 and read again
            lda #'R'
            jsr AFFICH
            jmp PGM02
ERR1        lda #'1'
            jmp ERR
ERR2        lda #'2'
            jmp ERR
ERR3        lda #'3'
ERR         jsr AFFICH
            jmp PGNTRK

PGM11       lda var1
            cmp #4
            beq ERR2
            inc var1
            jmp PGM16
PGM13       lda var1
            cmp #4
            beq ERR3
            inc var1
            lda #'R'
            jsr AFFICH
            jmp PGM02
PGM15       pla             ; 10 common values
            sta ptr2+1      ; unstack ptr2H,Y
            pla
            tay
            lda #$00
            sta (ptr2),y    ; write 0 at buffer end

PGM16       lda #'W'
            jsr AFFICH
            jsr ECRIT
            lda #'V'
            jsr AFFICH
            jsr LECT1
            lda #'A'
            jsr AFFICH
            move #buff1,ptr2
            ldx #$00
PGM17       lda (ptr2),y
            cmp buff2,x
            bne PGM18
            inx
            cpx #$10
            beq PGMOK
            bne PGM19
PGM18       ldx #$00
PGM19       iny
            bne PGM17
            inc ptr2+1
            lda ptr2+1
            cmp #>buff1end
            bne PGM17
            jmp PGM11
PGMOK       lda #"0"
            jsr AFFICH
PGNTRK      lda trkcurr
            cmp trkend
            beq EXIT
            inc trkcurr
            jmp PGM01
EXIT        brk

;***********************
;                      *
; ANALYSE              *
;                      *
;***********************

ANALYSE     move #buff1,ptr1    ; init ptr1 = $3000
            tay
-           lda (ptr1),y        ; search dlm1, found -> ANA03
            cmp dlm1
            beq ANA03           ; found dlm1
ANA02       iny                 ; next nibble
            bne -
            inc ptr1+1
            lda ptr1+1
            cmp #>buff1end      ; max $4EFF
            bne -
            jmp NONSTD          ; not found at end of buffer, NONSTD
ANA03       tya                 ; found dlm1, store Y | ptr1H on stack
            pha                 ; push addr
            lda ptr1+1
            pha
            iny
            bne +
            inc ptr1+1
+           lda (ptr1),y        ; search dlm2
            cmp dlm2
            bne NOTDLM          ; not dlm2, NOTDLM
            iny
            bne ANA05
            inc ptr1+1
ANA05       lda (ptr1),y        ; search dlm3
            cmp dlm3
            beq BINGO           ; bingo !
            cmp dlm4
            bne NOTDLM
BINGO       pla                 ; store stacked address in ptr1
            sta ptr1+1          ; ptr1H
            pla
            tay
            clc                 ; add Y to ptr1
            adc ptr1            ; if Y + ptr1L > $FF
            sta ptr1            ; increment ptr1H
            lda #0
            adc ptr1+1
            sta ptr1+1
            clc                 ; success = carry clear
            rts
NOTDLM      pla                 ; not dlm2/3, restore ptr1
            sta ptr1+1
            pla
            tay
            jmp ANA02           ; and next nibble
NONSTD      move #buff1,ptr1    ; restore ptr1 = $3000
            tay
ANA09       jsr SYNCR           ; call non standard analyse
            bcs ANA12           ; if carry set, fail -> ANA12
            lda (ptr1),y
            cmp dlm1            ; dlm1 after synchro ?
            bne ANA09           ; no, test another synchro
            lda ptr1+1          ; store ptr1,Y on stack
            pha                 ; push ptrH
            iny
            bne ANA10
            inc ptr1+1
ANA10       lda (ptr1),y        ; dlm2 ?
            cmp dlm2
            bne ANA11
            pla                 ; success !
            sta ptr1+1          ; restore and store ptr1H
            clc                 ; carry clear
            rts
ANA11       pla                 ; restore ptr1H
            sta ptr1+1
            jmp ANA09           ; next synchro
ANA12       move #buff1,ptr1    ; first analyse fail, retry from $3000
            tay
ANA13       jsr SYNCR
            bcs ANA14           ; fail again, ANA14
            lda (ptr1),y
            cmp dlm1
            bne ANA13
            clc                 ; found dlm1 = success
            rts
ANA14       move buff1,ptr1     ; last attempt
            tay
            jsr SYNCR
            rts

;***********************
;                      *
; ANALYSE NON STANDARD *
;                      *
;***********************

SYNCR       ldx #0
-           lda (ptr1),y
            cmp #$FF        ; search synchro
            bne SYN03
            inx             ; inx if $ff found
            cpx #5
            beq SYN05       ; found 5 $FF -> SYN05
SYN02       iny
            bne -
            inc ptr1+1
            lda ptr1+1
            cmp #>buff1end
            beq SYNFAIL
            bne -
SYN03       ldx #0
            jmp SYN02
SYNFAIL     sec             ; fail -> carry set
            rts
SYN05       iny             ; 5 $FF, incr ptr1,Y
            bne +
            inc ptr1+1
            lda ptr1+1
            cmp #>buff1end
            beq SYNFAIL
+           lda (ptr1),y
            cmp #$FF        ; while in synchro, incr ptr1,Y
            beq SYN05
            clc             ; end of synchro = success
            tya             ; store ptr1 + Y in ptr1
            adc ptr1        ; ptr1 = addr of first nibble of
            sta ptr1        ; min 5 $FF
            lda #$00
            adc ptr1+1
            sta ptr1+1
            ldy #$00
            clc             ; success = carry clear
            rts

;***********************
;                      *
; AFFICHAGE            *
;                      *
;***********************

AFFICH      ldx trkcurr
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

PISTE0      lda trkcurr
            pha
            lda #$FF
            sta trkcurr
            lda #1
            jsr SEEK0
            lda #2
            jsr SEEK0
            pla
            sta trkcurr
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
