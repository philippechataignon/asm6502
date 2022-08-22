; diskcopier

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
buff1read = $6f00
buff2 = $7000
buff2end = $9000
addr1 = $7500

slot = $60

rpliob = 0
rplslt = 1
rpldrv = 2
rplvol = 3
rpltrk = 4
rplsec = 5
rpldct = 6
rplbuf = 8                      ; (2 bytes)
rplsiz = $b
rplcmd = $c
rplret = $e

cmdseek = 0
cmdread = 1
cmdwrite = 2
cmdformat= 4

rwts        = $3D9
locrpl      = $3E3

ack        = $06
nak        = $15
esc        = $9b

.include "apple_enc.inc"
.include "macros.inc"

.enc "apple"

* = $900

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
            print TABLE2
            jsr TIRET
            lda #4
            sta CH
            lda #12
            sta CV
            jsr VTAB
            print TABLE1
            jsr RDKEY

;***********************
;                      *
; INITIALISATION       *
;                      *
;***********************

INIT        lda #0          ; init track
            sta trkcurr
            lda #35         ; last track
            sta trkend
            lda #$D5        ; track/sector delimiters
            sta dlm1
            lda #$AA
            sta dlm2
            lda #$96
            sta dlm3
            lda #$B5
            sta dlm4
            jsr HOME
            print TEXTE
            jsr SEEK0
            jsr xm.ssc.init     ; init ssc
            jmp PGM01

;***********************
;                      *
; ENVOI                *
;                      *
;***********************

ENVOI
            lda #>buff2
            sta xm.start
            ldy ptr2+1          ; get ptr2H
            iny
            sty xm.end
            lda #esc            ; send ESC to prevent send
            jsr xm.ssc.putc
            jsr xm.ssc.getc     ; and wait ack
            cmp #ack
            bne +
            jmp xm.XModemSend   ; send buffer using xmodem
+           jmp ERR4            ; ssc error

;***********************
;                      *
; LECTURE              *
;                      *
;***********************

; read $3000-$6EFF

LECTURE     jsr SEEK0
LECT1       ldx #slot
            lda DRVON,x
            lda DRVCTL1,x
            move buff1,ptr0
-           lda INBYT,x
            bpl -
            sta (ptr0),y
            inc ptr0
            bne -
            inc ptr0+1
            lda ptr0+1
            cmp #>buff1read
            blt -
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
            move buff2,ptr2     ; ptr2 = $7000
-           lda (ptr1),y        ; copy from ptr1 computed in ANALYSE
            sta (ptr2),y        ; in buff2
            iny
            bne -
            inc ptr1+1
            inc ptr2+1
            lda ptr2+1
            cmp #<buff2end      ; until end of buff2
            blt -
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
            move #addr1,ptr2    ; addr1 = start of search addr
            ldy #0
            ldx #0
            stx var2
-           lda (ptr2),y        ; compare (ptr2),y et buff2
            cmp buff2,x
            beq +               ; match
PGMNXTCMP   iny                 ; incr ptr2H,Y
            bne -
            inc ptr2+1          ; incr var2 when new page
            inc var2
            lda var2
            cmp #$20            ; $20 pages ?
            bne -
            jmp PGMENDBUF
+           tya                 ; store ptr2H,Y = delta on stack
            pha
            lda ptr2+1
            pha
-           inx
            cpx #10
            beq PGMCMPOK        ; 10 common values -> PGMCMPOK
            iny
            bne +
            inc ptr2+1
+           lda (ptr2),y
            cmp buff2,x
            beq -               ; again =, next X
            pla
            sta ptr2+1
            pla
            tay
            ldx #0
            jmp PGMNXTCMP

PGMCS       lda var1        ; test if #retry < 4
            cmp #4
            beq ERR1        ; fail, next track if any
            inc var1        ; incr var1 and read again
            lda #'R'
            jsr AFFICH
            jmp PGM02


PGMENDBUF   lda var1
            cmp #4          ; try to reread 4 times
            beq ERR3        ; else exit with error 3
            inc var1
            lda #'R'
            jsr AFFICH
            jmp PGM02

PGMCMPOK    pla             ; 10 common values
            sta ptr2+1      ; unstack ptr2H,Y
            pla
            tay
            lda #$00
            sta (ptr2),y    ; write 0 at buffer end

PGMWRITE    lda #'S'
            jsr AFFICH
            jsr ENVOI       ; SEND (buff2)->(ptr2)

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
; ERREURS              *
;                      *
;***********************

ERR1        lda #'1'
            jmp ERR
ERR2        lda #'2'
            jmp ERR
ERR3        lda #'3'
            jmp ERR
ERR4        lda #'4'
ERR         jsr AFFICH
            jmp PGNTRK

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

;***********************
;                      *
; REMET DRIVE PISTE 0  *
;                      *
;***********************

SEEK0
            jsr locrpl
            sty ptr0
            sta ptr0+1
            st_rwts ptr0,1,rpldrv           ; drive 1
            st_rwts ptr0,#$FF,rpltrk        ; track max
            st_rwts ptr0,#cmdseek,rplcmd    ; seek
            jsr locrpl
            jsr rwts
            lda #0
            sta PREG
            ldy #$01
            lda (ptr0),y
            tax
            rts

;***********************
;                      *
; AFFICHAGE ECRAN      *
;                      *
;***********************

TIRET       ldx #40
            lda #"="
-           jsr COUT
            dex
            bne -
            rts

xm          .binclude "xmodem_send.s"

TEXTE       .text "TR 000000000000000011111111111111112222\n"
            .text "AC 0123456789ABCDEF0123456789ABCDEF0123\n"
            .text "---------------------------------------\n"
            .text "ER\n"
            .text "---------------------------------------\n"
            .byte 0

TABLE1      .text "INSERT DISK                      "
            .byte 0
TABLE2      .text "DISKCOPIER                       "
            .text 'PHC  '
            .text " \n"
            .byte 0
