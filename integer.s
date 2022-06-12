ORG         :?=     $BD00

            .if ORG > 0
* =  ORG
            .fi

NUM         = $FA             ; 4 bytes
FLAG        = $E3             ; 1 byte
SAVE        = $EB             ; 4 bytes
PTR         = $EB             ; 2 bytes
TEMP        = $D7             ; 1 byte

CH          = $24
BASL        = $28
BUFF        = $BF00
IOADR       = $C000
KBDSTRB     = $C010
COUT1       = $FDF0

INPUTNUM
            ldx #$0
_L1         dex                 ; $FF at first iteration
_L2         bit IOADR           ; key down?
            bpl _L2             ; wait loop
            lda IOADR           ; get keycode
            bit KBDSTRB         ; clr key strobe
            cmp #$8D            ; return ?
            beq EXIT            ; yes, exit input loop
            cmp #$88            ; backspace ?
            bne +               ; no, continue
            cpx #$FF            ; yes, is first char ?
            beq _L2             ; yes, do nothing
            dec CH              ; no, point to previous char
            inx
            jmp _L2
+           cmp #'0'+$80        ; test if num else pass
            blt _L2             ; if < '0', pass
            cmp #'9'+$80+1
            bge _L2             ; if > '9', pass
            sta BUFF,x          ; else store in BUFF
            jsr COUT1
            jmp _L1
EXIT        txa                 ; copy X to A to compute nb digits in Y
            eor #$FF            ; inverse all bits
            sta TEMP
            inx                 ; X = ptr to last digit (= unity)
            stx PTR             ; PTR -> last char
            lda #>BUFF          ; init high PTR with high BUFF
            sta PTR+1           ;
            ldy #0              ; Y = digit/char counter
            sty NUM             ; init sum
            sty NUM+1
            sty NUM+2
            sty NUM+3
_L0         lda (PTR),y         ; get char (PTR is fixed)
            and #$0F            ; keep low nibble
            tax                 ; X = index loop
            clc
-           beq +               ; if X > 0, add POWER to NUM (32 bits)
            lda NUM
            adc POWER0,y
            sta NUM
            lda NUM+1
            adc POWER1,y
            sta NUM+1
            lda NUM+2
            adc POWER2,y
            sta NUM+2
            lda NUM+3
            adc POWER3,y
            sta NUM+3
            dex
            jmp -               ; next add iteration
+           iny
            cpy TEMP
            bcc _L0             ; next digit/char
            rts

powers = (1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000)

POWER0      .byte powers & $ff
POWER1      .byte powers >> 8 & $ff
POWER2      .byte powers >> 16 & $ff
POWER3      .byte powers >> 24 & $ff

            .align $100

PRINTNUM
            lda FLAG                ; set P bit to 0
            and #%10111111
            sta FLAG
            and #%00000111          ; get 8/16/32 bits (1/2/4)
            tax
            dex
            stx TEMP                ; 0,1,3 for 8/16/32 bits

-           lda NUM,x               ; saves NUM in SAVE
            sta SAVE,x
            dex
            bpl -
            bit FLAG                ; test bit 7 = S
            bpl pr_unsigned         ; bpl -> N = bit7 = S = 0
            ldx TEMP
            lda NUM,x               ; high order byte
            bpl pr_unsigned         ; with bit7 = 0 -> unsigned
-           lda NUM,x               ; negate x and output '-'
            eor #$ff
            sta NUM,x
            dex
            bpl -
            inc NUM
            bne +
            inc NUM+1
            bne +
            inc NUM+2
            bne +
            inc NUM+3
+           lda #'-'+$C0            ; '-' screen code
            jsr COUT1
pr_unsigned
            ldx TEMP
            ldy NBLOOP,x            ; Offset to nb loop
            lda #%00000001
            bit FLAG
            bne PRINT8
            lda #%00000010
            bit FLAG
            bne PRINT16
            lda #%00000100
            bit FLAG
            bne PRINT32
            jmp RESTORE

PRINT8
            ldx #$FF
            sec                     ; Start with digit=-1

-           lda NUM
            sbc POWER0,y
            sta NUM                 ; Subtract current tens
            inx
            bcs -                   ; Loop until <0
            lda NUM
            adc POWER0,y
            sta NUM                 ; Add current tens back in
            txa
            bne +                   ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc pr8_next            ; if V==0 -> initial 0
+           jsr PRXDIGIT            ; Print this digit
pr8_next    dey
            bpl PRINT8                 ; Loop for next digit
            jmp RESTORE

PRINT16
            ldx #$FF
            sec                     ; Start with digit=-1
-           lda NUM
            sbc POWER0,y
            sta NUM                 ; Subtract current tens
            lda NUM+1
            sbc POWER1,y
            sta NUM+1
            inx
            bcs -                   ; Loop until <0
            lda NUM
            adc POWER0,y
            sta NUM                 ; Add current tens back in
            lda NUM+1
            adc POWER1,y
            sta NUM+1
            txa
            bne +                   ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc pr16_next           ; if V==0 -> initial 0
+           jsr PRXDIGIT            ; Print this digit
pr16_next   dey
            bpl PRINT16             ; Loop for next digit
            jmp RESTORE

PRINT32
            ldx #$FF
            sec                     ; Start with digit=-1
-           lda NUM
            sbc POWER0,y
            sta NUM                 ; Subtract current tens
            lda NUM+1
            sbc POWER1,y
            sta NUM+1
            lda NUM+2
            sbc POWER2,y
            sta NUM+2
            lda NUM+3
            sbc POWER3,y
            sta NUM+3
            inx
            bcs -                   ; Loop until <0
            lda NUM
            adc POWER0,y
            sta NUM                 ; Add current tens back in
            lda NUM+1
            adc POWER1,y
            sta NUM+1
            lda NUM+2
            adc POWER2,y
            sta NUM+2
            lda NUM+3
            adc POWER3,y
            sta NUM+3
            txa
            bne +                   ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc pr32_next           ; if V==0 -> initial 0
+           jsr PRXDIGIT            ; Print this digit
pr32_next   dey
            bpl PRINT32             ; Loop for next digit
RESTORE
            ldx TEMP
-           lda SAVE,x
            sta NUM,x
            dex
            bpl -
            rts

PRXDIGIT                            ; output digit in X
            txa                     ; Save A, pass digit to A
            ora #$B0                ; convert to ASCII for NUMber
            jsr COUT1               ; Print it
            lda FLAG                ; set P bit to 1
            ora #%01000000
            sta FLAG
            rts                     ; Restore A and return

NBLOOP      .byte 2,4,0,9
