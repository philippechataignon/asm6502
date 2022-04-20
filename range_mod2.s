start = $1234
end = $5678

* = $900

loop    lda start
mod1l =  * - 2
mod1h =  * - 1
        inc mod1l
        bne +
        inc mod1h
+       lda mod1h
        cmp #>end
mod2h = * - 1
        bne loop
        lda mod1l
        cmp #<end
mod2l = * - 2
        bne loop
        rts
