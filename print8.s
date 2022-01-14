; On entry, A=value to print
; On exit,  A corrupted

NUM         equ     $FA
COUT1       equ     $FDF0

    ORG $280

    LDA NUM
.PrDec
    LDX #$FF
    SEC               ; Prepare for subtraction
.PrDec100
    INX               ; X = 0
    SBC #100
    BCS .PrDec100     ; if >= 100, inx
    ADC #100
    JSR PrDecDigit   ; Print the 100s
    LDX #$FF
    SEC               ; Prepare for subtraction
.PrDec10
    INX
    SBC #10
    BCS .PrDec10      ; Count how many 10s
    ADC #10
    JSR PrDecDigit    ; Print the 10s
    TAX               ; Pass 1s into X
PrDecDigit            ; output digit in X
    PHA
    TXA               ; Save A, pass digit to A
    ORA #$B0          ; convert to ASCII for number
    JSR COUT1         ; Print it
    PLA
    RTS               ; Restore A and return
