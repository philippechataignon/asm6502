; ---------------------------
; Print 32-bit decimal number
; ---------------------------
; On entry, num=number to print
;           pad=0 or pad character (eg '0' or ' ')
; On entry at .Loop1,
;           Y=(number of digits)*4-4, eg 36 for 10 digits
; On exit,  A,X,Y,num,pad corrupted
; Size      129 bytes
; -----------------------------------------------------------------


num     equ $FA
flag    equ $FE
save    equ $EB
COUT1   equ $FDF0

        org $280

print16s
        lda num
        sta save
        lda num+1
        sta save+1
        bpl .Positive
        lda num+1
        eor #$ff
        sta num+1
        lda num
        eor #$ff
        sta num
        inc num
        lda #'-'+$C0            ; '-' screen code
        jsr COUT1
.Positive
        ldy #8                  ; Offset to powers of ten
        sty flag                ; %00001000 bit7=0
.Loop1
        ldx #$FF
        sec                     ; Start with digit=-1
.Loop2
        lda num+0
        sbc .Tens+0,Y
        sta num+0               ; Subtract current tens
        lda num+1
        sbc .Tens+1,Y
        sta num+1
        inx
        bcs .Loop2              ; Loop until <0
        lda num+0
        adc .Tens+0,Y
        sta num+0               ; Add current tens back in
        lda num+1
        adc .Tens+1,Y
        sta num+1
        txa
        bne .Print              ; >0 -> print
        bit flag                ; bit 7 = N
        bpl .Next               ; if N==0 -> initial 0
.Print
        ora #$B0                ; Print this digit
        sta flag                ; B0 = 10110000 -> bit7=1
        jsr COUT1
.Next
        dey
        dey
        bpl .Loop1              ; Loop for next digit
        lda save
        sta num
        lda save+1
        sta num+1
        rts


.Tens
        defw 1
        defw 10
        defw 100
        defw 1000
        defw 10000
