msgout
_COUT       = $FDED
            pla                     ; get calling addr in _PTR
            sta _PTR
            pla
            sta _PTR+1
-           inc _PTR
            bne +
+           inc _PTR+1
            lda _PTR
_PTR        = * - 2
            beq +
            jsr _COUT
            jmp -
+           lda _PTR+1              ; _PTR point to next instr
            pha
            lda _PTR
            pha
            rts                     ; use _PTR as return addr
