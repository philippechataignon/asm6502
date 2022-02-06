; div32.s
SIZE    = 4

; N and R must be adjacent
; Zero-Page   ; INPUT   ; OUPUT
T       = $19 ;         ;
D       = $EB ; / d     ;
N       = $F8 ; n       ; q = n/d
R       = $FC ; 0       ; r = q - (n/d)*d

* = $BB00

        ; init R = 0
        ldx #SIZE-1
        ldy #0
.L1     sty R,X
        dex
        bpl .L1

        ; main loop: 32 iterations
        ldx #8*SIZE
LOOP    asl N+0         ; Shift high bit of N into R
        rol N+1
        rol N+2
        rol N+3         ; N high bit enters in R low
        rol R+0
        rol R+1
        rol R+2
        rol R+3
        sec
        lda R+0         ; T = R - D
        sbc D+0
        sta T+0
        lda R+1
        sbc D+1
        sta T+1
        lda R+2
        sbc D+2
        sta T+2
        lda R+3
        sbc D+3
        sta T+3
        blt NEXT        ; R < D -> next
        inc N           ; N LSB 0->1
        ; Update R <- T
        ldy #SIZE-1
.L2     lda T,Y
        sta R,Y
        dey
        bpl .L2
NEXT    dex        ; next bit
        bne LOOP   ; if any
        rts
