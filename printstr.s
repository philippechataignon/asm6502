PRINTPTR    = $CE
COUT        = $FDED

PRINTSTR:
            sta PRINTPTR+1         ; store A=MSB
            sty PRINTPTR           ; store Y=LSB
            ldy #0
.L1         lda (PRINTPTR),y       ;
            beq .L2              ; return if 0 = end of string
            jsr COUT
            iny
            jmp .L1
.L2         rts

