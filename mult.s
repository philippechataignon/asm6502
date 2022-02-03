XL = $FC
YL = $FA
TL = $EB

    org $803

    lda YL    ; Get the multiplicand and
    ldy YL+1
    sta TL    ; put it in the scratchpad.
    sty TL+1
    ldy #0
    sty YL    ; Zero-out the original multiplicand area.
    sty YL+1

    ldy #16   ; We'll loop 16 times.
.L1 asl YL    ; Shift the entire 32 bits over one bit position.
    rol YL+1
    rol XL
    rol XL+1
    bcc .L2    ; Skip the adding-in to the result if
              ; the high bit shifted out was 0.
    clc       ; Else, add multiplier to intermediate result.
    lda TL
    adc YL
    sta YL
    lda TL+1
    adc YL+1
    sta YL+1

    lda #0    ; If C=1, incr lo byte of hi cell.
    adc XL
    sta XL

.L2 dey       ; If we haven't done 16 iterations yet,
    bne .L1    ; then go around again.
    rts
