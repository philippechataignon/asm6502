; ---------------------------
; Print 8-16-32-bit decimal number
; ---------------------------
; On entry, num=number to print
; flag = |0|S| | | |32|16|8|
; -----------------------------------------------------------------


num     equ $FA
flag    equ $FE
save    equ $EB
temp    equ $EF
COUT1   equ $FDF0

        org $BE00

printnum
        lda flag
        and #%00000111          ; get 8/16/32 bits (1/2/4)
        tax
        dex
        stx temp                ; 0,1,3 for 8/16/32 bits
Save
.L0     lda num,x
        sta save,x
        dex
        bpl .L0
        bit flag                ; test bit 6 = S
        bvc .Unsigned           ; unsigned
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
        ldy NbLoop,X           ; Offset to nb loop
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
        lda num+0
        sbc TensB+0,Y
        sta num+0               ; Subtract current tens
        inx
        bcs .Loop2              ; Loop until <0
        lda num+0
        adc TensB+0,Y
        sta num+0               ; Add current tens back in
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bpl .Next               ; if N==0 -> initial 0
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
        lda num+0
        sbc TensW+0,Y
        sta num+0               ; Subtract current tens
        lda num+1
        sbc TensW+1,Y
        sta num+1
        inx
        bcs .Loop2              ; Loop until <0
        lda num+0
        adc TensW+0,Y
        sta num+0               ; Add current tens back in
        lda num+1
        adc TensW+1,Y
        sta num+1
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bpl .Next               ; if N==0 -> initial 0
.Print
        jsr PrXDigit            ; Print this digit
.Next
        dey
        dey
        bpl .Loop1              ; Loop for next digit
        jmp Restore

print32
.Loop1
        ldx #$FF
        sec                     ; Start with digit=-1
.Loop2
        lda num+0
        sbc TensL+0,Y
        sta num+0               ; Subtract current tens
        lda num+1
        sbc TensL+1,Y
        sta num+1
        lda num+2
        sbc TensL+2,Y
        sta num+2
        lda num+3
        sbc TensL+3,Y
        sta num+3
        inx
        bcs .Loop2              ; Loop until <0
        lda num+0
        adc TensL+0,Y
        sta num+0               ; Add current tens back in
        lda num+1
        adc TensL+1,Y
        sta num+1
        lda num+2
        adc TensL+2,Y
        sta num+2
        lda num+3
        adc TensL+3,Y
        sta num+3
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bpl .Next               ; if N==0 -> initial 0
.Print
        jsr PrXDigit            ; Print this digit
.Next
        dey
        dey
        dey
        dey
        bpl .Loop1              ; Loop for next digit
Restore
        ldx temp
.L0     lda save,x
        sta num,x
        dex
        bpl .L0
        rts

PrXDigit                  ; output digit in X
        TXA               ; Save A, pass digit to A
        ORA #$B0          ; convert to ASCII for number
        JSR COUT1         ; Print it
        LDA flag          ; set P bit to 1
        ORA #%10000000
        STA flag
        RTS               ; Restore A and return

Fill    defb 0

TensB
        defb 1
        defb 10
        defb 100

TensW
        defw 1
        defw 10
        defw 100
        defw 1000
        defw 10000

TensL
        defl 1
        defl 10
        defl 100
        defl 1000
        defl 10000
        defl 100000
        defl 1000000
        defl 10000000
        defl 100000000
        defl 1000000000

NbLoop defb 2,8,0,36
