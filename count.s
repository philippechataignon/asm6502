ptr     = $FA
last    = $FC
start   = $D0
end     = $0
countp  = $10
automod = $1234

; count on 2 pages = 16 bits

*       = $803

init    lda #0
        sta ptr
        sta dataptr
        lda #start
        sta dataptr+1
        lda #end
        sta last
        lda #countp
        sta ptr+1

; init count array to 0
        ldy #0
        lda #0
-       sta (ptr),y
        inc ptr+1   ; page+1
        sta (ptr),y
        dec ptr+1
        iny
        bne -

        ldx #0
loop    ldy automod,x
dataptr = * - 2
        lda (ptr),y
        clc
        adc #1
        sta (ptr),y
        inc ptr+1   ; page+1
        lda (ptr),y
        adc #0
        sta (ptr),y
        dec ptr+1   ; restore page
        inx
        bne loop
        inc dataptr+1
        lda dataptr+1
        cmp last
        bne loop
        rts
