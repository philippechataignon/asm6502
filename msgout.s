
msgout
_COUT = $FDED
            pla                     ; get calling addr in TEMP
            sta _PTR
            pla
            sta _PTR+1
            ldy #0                  ; always 0
-           inc _PTR
            bne +
            inc _PTR+1
+           lda $1234,y
_PTR = * -2
            beq _exit
            jsr _COUT
            jmp -
_exit       lda _PTR+1              ; TEMP points on next instr
            pha
            lda _PTR
            pha
            rts                     ; use TEMP as return addr
