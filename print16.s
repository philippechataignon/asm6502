; ---------------------------
; Print 16-bit decimal number
; ---------------------------
; On entry, num=number to print
; -----------------------------------------------------------------

num     EQU $FA
save    EQU $EB
COUT1   EQU $FDF0

        ORG $280

        LDA num
        STA save
        LDA num+1
        STA save+1
PrDec16
        LDY #8                          ; Offset to powers of ten
PrDec16Lp1
        LDX #$FF
        SEC                             ; Start with digit=-1
PrDec16Lp2
        LDA num
        SBC PrDec16Tens,Y
        STA num  ; Subtract current tens
        LDA num+1
        SBC PrDec16Tens+1,Y
        STA num+1
        INX
        BCS PrDec16Lp2                       ; Loop until <0
        LDA num
        ADC PrDec16Tens,Y
        STA num  ; Add current tens back in
        LDA num+1
        ADC PrDec16Tens+1,Y
        STA num+1
        TXA
        ORA #$B0                              ; Print this digit
        JSR COUT1
        DEY
        DEY
        BPL PrDec16Lp1                   ; Loop for next digit
        LDA save
        STA num
        LDA save+1
        STA num+1
        RTS

PrDec16Tens
        DEFW 1
        DEFW 10
        DEFW 100
        DEFW 1000
        DEFW 10000
; -----------------------------------------------------------------
