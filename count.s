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
        iny
        bne -

; count each #byte
; starts at 1 because max # is 255
        ldx #1
loop    ldy data,x
        lda (ptr),y
        clc
        adc #1
        sta (ptr),y
        inx
        bne loop
exit    rts
