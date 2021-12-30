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
SIGN    EQU $F3
X2      EQU $F4
M2      EQU $F5
X1      EQU $F8
M1      EQU $F9
E       EQU $FC
OVLOC   EQU $3F5

RTAR    EQU $F47A

        ORG $F63D
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
