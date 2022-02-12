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

CRC         = $EB
START       = $FA         ; 2 bytes
END         = $FC         ; 2 bytes
CUR         = $FE         ; 2 bytes
TMP         = $19         ; 1 byte
COUT1       = $FDF0
CRCT0       = $8C00       ; Four 256-byte tables
CRCT1       = $8D00       ; (should be page-aligned for speed)
CRCT2       = $8E00
CRCT3       = $8F00

INIT:
            jsr     MAKECRCTABLE
            ldy     START+1     ; init CUR high byte with START high byte
            sty     CUR+1
            ldy     #$ff        ; init CRC with $ffffffff
            sty     CRC
            sty     CRC+1
            sty     CRC+2
            sty     CRC+3
            iny                 ; Y = 0
            sty     CUR         ; CUR = HH00
            ldy     START
MAINLOOP    lda     (CUR),Y     ; current = HH00 + Y
            jsr     UPDCRC      ; update CRC
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
            jsr     COUTBYTE    ; and display
            dey
            bpl     -
            rts
COUTNIB:                         ; output a nibble (0-F)
            ora     #$B0        ; convert to ASCII for number
            cmp     #$BA        ; >= BA (3A|80) -> not number but [A-F]
            blt     +
            adc     #6          ; need to add 6 : BA + C + 6 = C1 = 'A'
+           jsr     COUT1
            rts
COUTBYTE:
            pha                 ; push A for low nibble
            lsr                 ; >> 4
            lsr
            lsr
            lsr
            jsr     COUTNIB     ; display high nibble
            pla
            and     #$0F
            jsr     COUTNIB     ; display low nibble
            rts

MAKECRCTABLE:
            ldx     #0          ; X counts from 0 to 255
_BYTELOOP:  lda     #0          ; A contains the high byte of the CRC-32
            sta     CRC+2       ; The other three bytes are in memory
            sta     CRC+1
            stx     CRC         ; X ($00-$FF) in CRC LSB
            ldy     #8          ; Y counts bits in a byte
_BITLOOP:   lsr                 ; The CRC-32 algorithm is similar to CRC-16
            ror     CRC+2       ; except that it is reversed (originally for
            ror     CRC+1       ; hardware reasons). This is why we shift
            ror     CRC         ; right instead of left here.
            bcc     _NOADD      ; Do nothing if no overflow
                                ; else add CRC-32 reverse polynomial POLY
            eor     #POLY >> 24
            pha                 ; Save high byte while we do others
            lda     CRC+2
            eor     #POLY >> 16 & $ff
            sta     CRC+2
            lda     CRC+1
            eor     #POLY >> 8 & $ff
            sta     CRC+1
            lda     CRC
            eor     #POLY & $ff
            sta     CRC
            pla                 ; Restore high byte
_NOADD:     dey
            bne     _BITLOOP    ; Do next bit
            sta     CRCT3,X     ; Save CRC into table, high to low bytes
            lda     CRC+2
            sta     CRCT2,X
            lda     CRC+1
            sta     CRCT1,X
            lda     CRC
            sta     CRCT0,X
            inx
            bne     _BYTELOOP    ; Do next byte
            rts

UPDCRC:
            eor     CRC         ; Quick CRC computation with lookup tables
            tax
            lda     CRC+1
            eor     CRCT0,X
            sta     CRC
            lda     CRC+2
            eor     CRCT1,X
            sta     CRC+1
            lda     CRC+3
            eor     CRCT2,X
            sta     CRC+2
            lda     CRCT3,X
            sta     CRC+3
            rts
