; Convert an 16/32 bit binary value to BCD
;
; This function converts a 16/32 bit binary value into a 24/40 bit BCD. It
; works by transferring one bit a time from the source and adding it
; into a BCD value that is being doubled on each iteration. As all the
; arithmetic is being done in BCD the result is a binary to decimal
; conversion.

; 1234567890 -> BCD: $12 $34 $56 $78 $90

BITS := 32

PRBYTE      = $FDDA
PRHEX       = $FDE3
COUT        = $FDED

F0ADDR      = $EB
BIN         = $EC
BCD         = BIN + BITS / 8
BCD_POS     = BITS / 8 + 1

.include "apple_enc.inc"
.enc "apple"

.if DIRECT
* = $300
.fi

BINBCD:
        lda #$F0        ; bit instr has no immediate mode
        sta F0ADDR
        sed             ; Switch to decimal mode
        ldx #BCD_POS
        lda #0          ; Ensure the result is clear
-       sta BCD-1,x
        dex
        bne -

        ldy #BITS       ; The number of source bits
CNVBIT: asl BIN         ; Shift out one bit
        rol BIN+1
.if BITS == 32
        rol BIN+2
        rol BIN+3
.endif
        ldx #BCD_POS
-       lda BCD-1,x     ; And add into result
        adc BCD-1,x     ; propagating any carry
        sta BCD-1,x     ; thru whole result
        dex
        bne -
        dey             ; And repeat for next bit
        bne CNVBIT
        cld             ; Back to binary
        ldx #0          ; skip when byte = 0
-       lda BCD,x       ; avoid printing "00"
        bne NIBBLE      ; test if "0x"
        inx
        cpx #BCD_POS
        blt -
        lda #"0"        ; all zeros, print "0"
        jsr COUT
        rts
NIBBLE:
        bit F0ADDR      ; "and" #$F0
        bne PRINT       ; si XY, normal print
        jsr PRHEX       ; else 0Y, PRHEX
        jmp +           ; next byte
PRINT:
-       lda BCD,x
        jsr PRBYTE
+       inx
        cpx #BCD_POS
        blt -
        rts
