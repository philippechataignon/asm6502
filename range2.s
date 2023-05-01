START = $fa
END   = $fc
CUR   = $fe
TMP   = $ce


* = $900
            lda START+1     ; init CUR high byte with START high byte
            sta CUR+1
            ldy #0          ; Y = 0
            sty CUR         ; CUR = HH00
            ldy START
-           lda (CUR),y     ; current = HH00 + Y
            iny             ; increment (CUR),y
            bne +
            inc CUR+1       ; incr high byte
            beq _exit       ; if $0000, exit
+           sty TMP         ; if END >= CURH||Y=TMP
            lda END
            cmp TMP
            lda END+1
            sbc CUR+1
            bge -           ; continue
_exit       rts


