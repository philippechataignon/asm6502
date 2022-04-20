start = $FA
end = $FC

* = $900

        lda start
        sta mod
        lda start+1
        sta mod+1

loop    lda $3412
mod =   * - 2
        inc mod
        bne +
        inc mod+1
+       lda mod+1
        cmp end+1
        bne loop
        lda mod
        cmp end
        bne loop
        rts
