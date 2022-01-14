; ---------------------------
; Print 32-bit decimal number
; ---------------------------
; On entry, num=number to print
;           pad=0 or pad character (eg '0' or ' ')
; On entry at PrDec32Lp1,
;           Y=(number of digits)*4-4, eg 36 for 10 digits
; On exit,  A,X,Y,num,pad corrupted
; Size      129 bytes
; -----------------------------------------------------------------


num     EQU $FA
save    EQU $EB
COUT1   EQU $FDF0

        ORG $280

        LDA num
        STA save
        LDA num+1
        STA save+1
        LDA num+2
        STA save+2
        LDA num+3
        STA save+3

PrDec32
        LDY #36                                  ; Offset to powers of ten
PrDec32Lp1
        LDX #$FF
        SEC                             ; Start with digit=-1
PrDec32Lp2
        LDA num+0
        SBC PrDec32Tens+0,Y
        STA num+0  ; Subtract current tens
        LDA num+1
        SBC PrDec32Tens+1,Y
        STA num+1
        LDA num+2
        SBC PrDec32Tens+2,Y
        STA num+2
        LDA num+3
        SBC PrDec32Tens+3,Y
        STA num+3
        INX
        BCS PrDec32Lp2                       ; Loop until <0
        LDA num+0
        ADC PrDec32Tens+0,Y
        STA num+0  ; Add current tens back in
        LDA num+1
        ADC PrDec32Tens+1,Y
        STA num+1
        LDA num+2
        ADC PrDec32Tens+2,Y
        STA num+2
        LDA num+3
        ADC PrDec32Tens+3,Y
        STA num+3
        TXA
        ORA #$B0                              ; Print this digit
        JSR COUT1
PrDec32Next
        DEY
        DEY
        DEY
        DEY
        BPL PrDec32Lp1           ; Loop for next digit
        LDA save
        STA num
        LDA save+1
        STA num+1
        LDA save+2
        STA num+2
        LDA save+3
        STA num+3
        RTS


PrDec32Tens
        DEFL 1
        DEFL 10
        DEFL 100
        DEFL 1000
        DEFL 10000
        DEFL 100000
        DEFL 1000000
        DEFL 10000000
        DEFL 100000000
        DEFL 1000000000
