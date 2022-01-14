; ---------------------------
; Print 16-bit decimal number
; ---------------------------
; On entry, num=number to print
;           pad=0 or pad character (eg '0' or ' ')
; On entry at PrDec16Lp1,
;           Y=(number of digits)*2-2, eg 8 for 5 digits
; On exit,  A,X,Y,num,pad corrupted
; Size      69 bytes
; -----------------------------------------------------------------

num     EQU $FA
pad     EQU $FC
COUT1   EQU $FDF0

        ORG $280

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
        RTS

PrDec16Tens
        WORD 1
        WORD 10
        WORD 100
        WORD 1000
        WORD 10000
; -----------------------------------------------------------------
