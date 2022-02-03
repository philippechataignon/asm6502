XL = $FA      ; X and result as XL..XL+3
YL = $FC      ; Y and 16 high bits of result
TL = $EB      ; T contains X initial value

    org $803

    lda XL    ; Put X in T
    ldy XL+1
    sta TL
    sty TL+1
    ldy #0
    sty XL    ; Zero-out 2 LSB of result
    sty XL+1

    ldy #16   ; We'll loop 16 times over Y bits
.L1 asl XL    ; Shift the entire 32 bits over one bit position.
    rol XL+1
    rol XL+2  ; equivalent of YL
    rol XL+3  ; equivalent of YH
    bcc .L2   ; Skip the adding-in to the result if
              ; the high bit shifted out was 0.
    clc       ; Else, add with carry to intermediate result.
    lda TL    ; Add X (0,0,TH,TL) with carry to intermediate result
    adc XL
    sta XL
    lda TL+1
    adc XL+1
    sta XL+1
    lda #0    ; If C=1, incr XL+2 = third byte
    adc XL+2
    sta XL+2

.L2 dey       ; If we haven't done 16 iterations yet,
    bne .L1   ; then go around again.
    rts
