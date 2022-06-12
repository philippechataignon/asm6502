param = $bf00
paramsize = 6
src = param
end = param + 2
dst = param + 4
ptr = $ce

.if DIRECT
* = $300
.fi


inline      pla              ; get calling addr + 2 in ptr
            sta ptr
            pla
            sta ptr+1
            ldx #0
            ldy #0           ; always 0
-           inc ptr
            bne +
            inc ptr+1
+           lda (ptr),y      ; get param
            sta param,x
            inx
            cpx #paramsize
            bne -
            lda ptr+1        ; use ptr as return addr
            pha
            lda ptr
            pha
            rts
