* = $803
COUT = $FDED
TEMP = $CE                          ; temp LSB

.include "apple_enc.inc"
.enc "apple"

            jsr msgout              ; PC + 2 is pushed
            .null "HELLO WORLD!\n"
            jsr msgout
            .null "HELLO WORLD!2\n"
            rts

msgout      pla                     ; get calling addr in TEMP
            sta TEMP
            pla
            sta TEMP+1
            ldy #0                  ; always 0
-           inc TEMP
            bne +
            inc TEMP+1
+           lda (TEMP),Y
            beq _exit
            jsr COUT
            jmp -
_exit       lda TEMP+1              ; TEMP points on next instr
            pha
            lda TEMP
            pha
            rts                     ; use TEMP as return addr
