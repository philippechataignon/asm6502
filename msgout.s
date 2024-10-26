
msgout
_COUT = $FDED
_TEMP = $CE                         ; temp LSB
            pla                     ; get calling addr in TEMP
            sta _TEMP
            pla
            sta _TEMP+1
            ldy #0                  ; always 0
-           inc _TEMP
            bne +
            inc _TEMP+1
+           lda (_TEMP),y
            beq _exit
            jsr _COUT
            jmp -
_exit       lda _TEMP+1              ; TEMP points on next instr
            pha
            lda _TEMP
            pha
            rts                     ; use TEMP as return addr
