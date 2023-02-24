; store cte in [ptr; end[

ptr = $FA
end = $FC

* = $300

        ldy #0          ; always 0
; if ptr >= end, exit
loop    lda ptr
        cmp end
        lda ptr+1
        sbc end+1
        bge exit
        lda #$A2        ; constant
        sta (ptr),y   ; store
; incr ptr
        inc ptr
        bne loop
        inc ptr+1
        bne loop
exit    rts

