; ---------------------------
; Print 8-16-32-bit decimal number
; ---------------------------
; On entry, num=number to print
; flag = |S|P| | | |32|16|8|
; -----------------------------------------------------------------


num     equ $FA
flag    equ $FE
save    equ $EB
temp    equ $EF
COUT1   equ $FDF0

        org $BE00

printnum
        lda flag                ; set P bit to 0
        and #%10111111
        sta flag
        and #%00000111          ; get 8/16/32 bits (1/2/4)
        tax
        dex
        stx temp                ; 0,1,3 for 8/16/32 bits
Save
.L0     lda num,x
        sta save,x
        dex
        bpl .L0
        bit flag                ; test bit 7 = S
        bpl .Unsigned           ; bpl -> N = bit7 = S = 0
        ldx temp
        lda num,x               ; high order byte
        bpl .Unsigned           ; with bit7 = 0 -> unsigned
.L1     lda num,x               ; negate x and output '-'
        eor #$ff
        sta num,x
        dex
        bpl .L1
        inc num
        bne .L2
        inc num+1
        bne .L2
        inc num+2
        bne .L2
        inc num+3
.L2     lda #'-'+$C0            ; '-' screen code
        jsr COUT1
.Unsigned
        ldx temp
        ldy NbLoop,X            ; Offset to nb loop
        lda #%00000001
        bit flag
        bne print8
        lda #%00000010
        bit flag
        bne print16
        lda #%00000100
        bit flag
        bne print32
        jmp Restore

print8
.Loop1
        ldx #$FF
        sec                     ; Start with digit=-1
.Loop2
        lda num
        sbc Tens0,Y
        sta num                 ; Subtract current tens
        inx
        bcs .Loop2              ; Loop until <0
        lda num
        adc Tens0,Y
        sta num                 ; Add current tens back in
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bvc .Next               ; if V==0 -> initial 0
.Print
        jsr PrXDigit            ; Print this digit
.Next
        dey
        bpl .Loop1              ; Loop for next digit
        jmp Restore

print16
.Loop1
        ldx #$FF
        sec                     ; Start with digit=-1
.Loop2
        lda num
        sbc Tens0,Y
        sta num                 ; Subtract current tens
        lda num+1
        sbc Tens1,Y
        sta num+1
        inx
        bcs .Loop2              ; Loop until <0
        lda num
        adc Tens0,Y
        sta num                 ; Add current tens back in
        lda num+1
        adc Tens1,Y
        sta num+1
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bvc .Next               ; if V==0 -> initial 0
.Print
        jsr PrXDigit            ; Print this digit
.Next
        dey
        bpl .Loop1              ; Loop for next digit
        jmp Restore

print32
.Loop1
        ldx #$FF
        sec                     ; Start with digit=-1
.Loop2
        lda num
        sbc Tens0,Y
        sta num                 ; Subtract current tens
        lda num+1
        sbc Tens1,Y
        sta num+1
        lda num+2
        sbc Tens2,Y
        sta num+2
        lda num+3
        sbc Tens3,Y
        sta num+3
        inx
        bcs .Loop2              ; Loop until <0
        lda num
        adc Tens0,Y
        sta num                 ; Add current tens back in
        lda num+1
        adc Tens1,Y
        sta num+1
        lda num+2
        adc Tens2,Y
        sta num+2
        lda num+3
        adc Tens3,Y
        sta num+3
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bvc .Next               ; if V==0 -> initial 0
.Print
        jsr PrXDigit            ; Print this digit
.Next
        dey
        bpl .Loop1              ; Loop for next digit
Restore
        ldx temp
.L0     lda save,x
        sta num,x
        dex
        bpl .L0
        rts

PrXDigit                        ; output digit in X
        txa                     ; Save A, pass digit to A
        ora #$B0                ; convert to ASCII for number
        jsr COUT1               ; Print it
        lda flag                ; set P bit to 1
        ora #%01000000
        sta flag
        rts                     ; Restore A and return

Tens0   defb 1 & $ff
        defb 10 & $ff
        defb 100 & $ff
        defb 1000 & $ff
        defb 10000 & $ff
        defb 100000 & $ff
        defb 1000000 & $ff
        defb 10000000 & $ff
        defb 100000000 & $ff
        defb 1000000000 & $ff
Tens1   defb 1 >> 8 & $ff
        defb 10 >> 8 & $ff
        defb 100 >> 8 & $ff
        defb 1000 >> 8 & $ff
        defb 10000 >> 8 & $ff
        defb 100000 >> 8 & $ff
        defb 1000000 >> 8 & $ff
        defb 10000000 >> 8 & $ff
        defb 100000000 >> 8 & $ff
        defb 1000000000 >> 8 & $ff
Tens2   defb 1 >> 16 & $ff
        defb 10 >> 16 & $ff
        defb 100 >> 16 & $ff
        defb 1000 >> 16 & $ff
        defb 10000 >> 16 & $ff
        defb 100000 >> 16 & $ff
        defb 1000000 >> 16 & $ff
        defb 10000000 >> 16 & $ff
        defb 100000000 >> 16 & $ff
        defb 1000000000 >> 16 & $ff
Tens3   defb 1 >> 24 & $ff
        defb 10 >> 24 & $ff
        defb 100 >> 24 & $ff
        defb 1000 >> 24 & $ff
        defb 10000 >> 24 & $ff
        defb 100000 >> 24 & $ff
        defb 1000000 >> 24 & $ff
        defb 10000000 >> 24 & $ff
        defb 100000000 >> 24 & $ff
        defb 1000000000 >> 24 & $ff

NbLoop defb 2,4,0,9
