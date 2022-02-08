PRINTPTR    = $CE
COUT        = $FDED

PRINTSTR:
            sta PRINTPTR+1         ; store A=MSB
            sty PRINTPTR           ; store Y=LSB
            ldy #0
_L1         lda (PRINTPTR),y       ;
            beq _L2              ; return if 0 = end of string
            jsr COUT
            iny
            jmp _L1
_L2         rts

