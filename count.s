ptr     = $FA
data    = $1000
count   = $1100

*       = $803

        lda #<count
        sta ptr
        lda #>count
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

; count each #byte
; starts at 1 because max # is 255
        ldx #0
loop    ldy data,x
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
exit    rts
