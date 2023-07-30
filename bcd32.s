; Convert an 32 bit binary value to BCD
;
; This function converts a 32 bit binary value into a 40 bit BCD. It
; works by transferring one bit a time from the source and adding it
; into a BCD value that is being doubled on each iteration. As all the
; arithmetic is being done in BCD the result is a binary to decimal
; conversion.

* = $300
        jmp BINBCD32

; 1234567890 -> BCD: $12 $34 $56 $78 $90
BIN        .dword  1234567890
BCD        .fill  5

BINBCD32:
        sed             ; Switch to decimal mode
        ldy #8
        lda #0          ; Ensure the result is clear
-       sta BCD,y
        dey
        bpl -

        ldx #32         ; The number of source bits
CNVBIT: asl BIN         ; Shift out one bit
        rol BIN+1
        rol BIN+2
        rol BIN+3

        ldy #5
-       lda BCD-1,y       ; And add into result
        adc BCD-1,y       ; propagating any carry
        sta BCD-1,y       ; thru whole result
        dey
        bne -
        dex             ; And repeat for next bit
        bne CNVBIT
        cld             ; Back to binary
        brk
