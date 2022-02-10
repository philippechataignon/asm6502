ORG     :?= $BC80
BYTE    = 4

; N and R must be adjacent
; Zero-Page   ; INPUT   ; OUPUT
T       :?= $19
D       :?= $EB ; / d     ;
N       :?= $F8 ; n       ; q = n/d
R       :?= $FC ; 0       ; r = q - (n/d)*d

        .if ORG > 0
*       = ORG
        .fi

        ; init R = 0
        ldy #0
.for i in range(BYTE)
        sty R+i
.next

        ; main loop: 32 iterations
        ldx #8*BYTE
        ; Shift high bit of N into R
        ; N LSB = 0 (clc) and will be 1
        ; if INC N below               
        ; N high bit enters in R low   
LOOP    clc
.for i in range(2*BYTE)
        rol N+i
.next

        ; T = R - D on 32 bits
        sec
.for i in range(BYTE)
        lda R+i         
        sbc D+i
        sta T+i
.next
        blt NEXT        ; R < D -> next
        inc N           ; N LSB 0->1

        ; Update R <- T
.for i in range(BYTE)
        lda T+i
        sta R+i
.next
NEXT    dex        ; next bit
        bne LOOP   ; if any
        rts
