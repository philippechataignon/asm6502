XL = $FA      ; X and result as XL..XL+3
YL = $FC      ; Y and 16 high bits of result
TL = $EB      ; T contains X initial value

* = $803

    lda XL    ; Put X in T
    ldy XL+1
    sta TL
    sty TL+1
    ldy #0
    sty XL    ; Zero-out 2 LSB of result
    sty XL+1

    ldy #16   ; We'll loop 16 times over Y bits
L1  asl XL    ; 5 Shift the entire 32 bits over one bit position.
    rol XL+1  ; 5
    rol XL+2  ; 5 equivalent of YL
    rol XL+3  ; 5 equivalent of YH
    bcc L2    ; 2/3 Skip the adding-in to the result if
              ; the high bit shifted out was 0.
    clc       ; 2 Else, add with carry to intermediate result.
    lda TL    ; 3 Add X (0,0,TH,TL) with carry to intermediate result
    adc XL    ; 3
    sta XL    ; 3
    lda TL+1  ; 3
    adc XL+1  ; 3
    sta XL+1  ; 3
    lda #0    ; 3 If C=1, incr XL+2 = third byte
    adc XL+2  ; 3
    sta XL+2  ; 3  = 29 µs
              ; total = 16 * 20 + 16 * 0.5 * 30 = 560 µs = 0.56 ms 
L2  dey       ; If we haven't done 16 iterations yet,
    bne L1    ; then go around again.
    rts
