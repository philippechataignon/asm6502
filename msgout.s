msgout
_COUT = $FDED
            pla                     ; get calling addr
            sta _PTR                ; in _PTR
            pla
            sta _PTR+1
            ldy #0
-           inc _PTR
            bne +
            inc _PTR+1
+           lda $1234,y             ; load char
_PTR = * -2
            beq _exit               ; if NULL, exit
            jsr _COUT               ; display
            jmp -                   ; and next
_exit
            inc _PTR                ; increment return addr
            bne +
            inc _PTR+1
+           jmp (_PTR)              ; and jump
