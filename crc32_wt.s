.if DIRECT
* = $803
.fi
            ; Most reference books give the CRC-32 poly
            ; as $04C11DB7. This is actually the same if
            ; you write it in binary and read it right-
            ; to-left instead of left-to-right. Doing it
            ; this way means we won't have to explicitly
            ; reverse things afterwards
POLY        = $EDB88320
COUT1       = $FDF0
PRBYTE      = $FDDA
CUR = MAINLOOP + 1

ZP = true   ; if ZP if set, CRC, WORK and TMP are in page zero
            ; on D000-FFFF, time goes from 6s20 to 5s30 for $1D05C1B1

INIT:
            jmp ENTRY

START       .word 1
END         .word 2

ENTRY       ldy     START+1     ; init CUR high byte with START high byte
            sty     CUR+1
            ldy     #$ff        ; init CRC with $ffffffff
            sty     CRC
            sty     CRC+1
            sty     CRC+2
            sty     CRC+3
            iny                 ; Y = 0
            sty     CUR         ; CUR = HH00
            ldy     START
MAINLOOP    lda     $ffff,Y     ; load current byte
            eor     CRC         ; Quick CRC computation with lookup tables
            jsr     CRCVALUE
            lda     CRC+1
            eor     WORK
            sta     CRC
            lda     CRC+2
            eor     WORK+1
            sta     CRC+1
            lda     CRC+3
            eor     WORK+2
            sta     CRC+2
            lda     WORK+3
            sta     CRC+3
            iny                 ; increment (CUR),Y
            bne     +
            inc     CUR+1
            beq     EXIT        ; if $0000, exit
+           sty     TMP         ; END>=CUR,Y=TMP
            lda     END
            cmp     TMP
            lda     END+1
            sbc     CUR+1
            bge     MAINLOOP    ; yes, continue
EXIT        ldy     #3          ; eor $FFFFFFFF for CRC at the end
-           lda     CRC,Y
            eor     #$FF
            sta     CRC,Y
            jsr     PRBYTE      ; and display
            dey
            bpl     -
            rts

CRCVALUE    sta     WORK
            lda     #0           ; A contains the high byte of the CRC-32
            sta     WORK+2       ; The other three bytes are in memory
            sta     WORK+1
            ldx     #8           ; X counts bits in a byte
-           lsr                  ; The CRC-32 algorithm is similar to CRC-16
            ror     WORK+2       ; except that it is reversed (originally for
            ror     WORK+1       ; hardware reasons). This is why we shift
            ror     WORK         ; right instead of left here.
            bcc     +            ; Do nothing if no overflow
            eor     #POLY >> 24  ; else add CRC-32 reverse polynomial POLY
            pha                  ; Save high byte while we do others
            lda     WORK+2
            eor     #POLY >> 16 & $ff
            sta     WORK+2
            lda     WORK+1
            eor     #POLY >> 8 & $ff
            sta     WORK+1
            lda     WORK
            eor     #POLY & $ff
            sta     WORK
            pla                 ; Restore high byte
+           dex
            bne     -          ; Do next bit
            sta     WORK+3     ; Return CRC value in WORK
            rts


.align $100
CRCT0       .fill 256,? ; Four 256-byte tables
CRCT1       .fill 256,? ; (should be page-aligned for speed)
CRCT2       .fill 256,?
CRCT3       .fill 256,?

.if ZP
* = $6
.fi
CRC         .dword ?
.if ZP
* = $19
.fi
WORK        .dword ?
TMP         .byte ?
