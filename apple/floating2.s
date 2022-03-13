;**********************
;
; APPLE-II FLOATING
;  POINT ROUTINES
;
; COPYRIGHT 1977 BY
; APPLE COMPUTER INC.
;
; ALL RIGHTS RESERVED
;     S. WOZNIAK
;
;**********************
SIGN    = $F3
X2      = $F4
M2      = $F5
X1      = $F8
M1      = $F9
E       = $FC
OVLOC   = $3F5

RTAR    = $F47D

* = $F63D
FIX1    JSR RTAR
FIX     LDA X1
        BPL UNDFL
        CMP #$8E
        BNE FIX1
        BIT M1
        BPL FIXRTS
        LDA M1+2
        BEQ FIXRTS
        INC M1+1
        BNE FIXRTS
        INC M1
FIXRTS  RTS
UNDFL   LDA #$0
        STA M1
        STA M1+1
        RTS

        .fill 8,$FF   ; FILL WITH 8 FF
