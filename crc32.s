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

CUR         = $FE         ; 2 bytes
COUT1       = $FDF0
PRBYTE      = $FDDA

INIT:
            jmp ENTRY

START       .word $d000
END         .word $ffff

ENTRY       jsr     MAKECRCTABLE
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
MAINLOOP    lda     (CUR),y     ; current = HH00 + Y
            eor     CRC         ; Quick CRC computation with lookup tables
            tax                 ; update CRC
            lda     CRC+1
            eor     CRCT0,x
            sta     CRC
            lda     CRC+2
            eor     CRCT1,x
            sta     CRC+1
            lda     CRC+3
            eor     CRCT2,x
            sta     CRC+2
            lda     CRCT3,x
            sta     CRC+3
            iny                 ; increment (CUR),y
            bne     +
            inc     CUR+1
            beq     EXIT        ; if $0000, exit
+           sty     TMP         ; END>=CUR,y=TMP
            lda     END
            cmp     TMP
            lda     END+1
            sbc     CUR+1
            bge     MAINLOOP    ; yes, continue
EXIT        ldy     #3          ; eor $FFFFFFFF for CRC at the end
-           lda     CRC,y
            eor     #$FF
            sta     CRC,y
            jsr     PRBYTE      ; and display
            dey
            bpl     -
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
            sta     CRCT3,x     ; Save CRC into table, high to low bytes
            lda     CRC+2
            sta     CRCT2,x
            lda     CRC+1
            sta     CRCT1,x
            lda     CRC
            sta     CRCT0,x
            inx
            bne     _BYTELOOP    ; Do next byte
            rts

CRC         .dword ?
TMP         .byte ?

.align $100
CRCT0       .fill 256,? ; Four 256-byte tables
CRCT1       .fill 256,? ; (should be page-aligned for speed)
CRCT2       .fill 256,?
CRCT3       .fill 256,?
