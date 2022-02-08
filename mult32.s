ORG :?= $BC00

X = $6          ; X
Y = $EB         ; Y
M = $FA         ; R result
T :?= $19         ; T temp

    .if ORG > 0
* = ORG
    .fi

    lda #0      ; clear result
    sta M
    sta M+1
    sta M+2
    sta M+3
    lda Y       ; copy Y to T
    sta T
    lda Y+1
    sta T+1
    lda Y+2
    sta T+2
    lda Y+3
    sta T+3
    ldy #32     ; loop 32 times over Y bits
L1  asl M       ; shift 32 bits R
    rol M+1
    rol M+2
    rol M+3
    asl T       ; shift 32 bits Y to get high bit
    rol T+1
    rol T+2
    rol T+3     ; get Y high bit in C
    bcc L2      ; skip if C=0
    clc         ; else, add X to M
    lda M
    adc X
    sta M
    lda M+1
    adc X+1
    sta M+1
    lda M+2
    adc X+2
    sta M+2
    lda M+3
    adc X+3
    sta M+3
L2  dey       ; 32 iterations ?
    bne L1    ; next Y bit
    rts
