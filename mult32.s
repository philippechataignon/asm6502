X = $F0         ; X
Y = $F4         ; Y
R = $FA         ; R result
T = $EB


* = $803

    lda #0      ; clear result
    sta R
    sta R+1
    sta R+2
    sta R+3
    lda Y       ; copy Y to T
    sta T
    lda Y+1
    sta T+1
    lda Y+2
    sta T+2
    lda Y+3
    sta T+3
    ldy #32     ; loop 32 times over Y bits
L1  asl R       ; shift 32 bits R
    rol R+1
    rol R+2
    rol R+3
    asl T       ; shift 32 bits Y to get high bit
    rol T+1
    rol T+2
    rol T+3     ; get Y high bit in C
    bcc L2      ; skip if C=0
    clc         ; else, add X to R
    lda R
    adc X
    sta R
    lda R+1
    adc X+1
    sta R+1
    lda R+2
    adc X+2
    sta R+2
    lda R+3
    adc X+3
    sta R+3
L2  dey       ; 32 iterations ?
    bne L1    ; next Y bit
    rts
