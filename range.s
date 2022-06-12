start = $FA
end = $FC
tmp = $19

* = $803

        ldy start       ; get real start
        lda #0          ; start LSB = 0
        sta start
-       lda #$A5        ; constant
        sta (start),y   ; store
        iny
        bne +
        inc start+1
        beq exit        ; exit on $0000

;+       cpy end         ; y,start < end
;        lda start+1
;        sbc end+1
;        blt -
                        ; y=tmp,start <= end
+       sty tmp
        lda end
        cmp tmp
        lda end+1
        sbc start+1
        bge -
exit    rts

