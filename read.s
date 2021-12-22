CHKSUM      equ     $2E
LASTIN      equ     $2F
COUT        equ     $FDED
TAPEOUT     equ     $c020
TAPEIN      equ     $c060
A1L         equ     $fa
A1H         equ     $fb
A2L         equ     $fc
A2H         equ     $fd

            org     $280

READ        jsr     RD2BIT
            lda     #$16
            jsr     HEADR
            sta     CHKSUM
            jsr     RD2BIT
RD2         ldy     #$24
            jsr     RDBIT
            bcs     RD2
            jsr     RDBIT
            ldy     #$3b
RD3         jsr     RDBYTE
            sta     (A1L,x)
            eor     CHKSUM
            sta     CHKSUM
            jsr     NXTA1
            ldy     #$35
            bcc     RD3
            jsr     RDBYTE
            cmp     CHKSUM
            beq     BELL
PRERR       lda     #$c5
            jsr     COUT
            lda     #$d2
            jsr     COUT
            jsr     COUT
BELL        lda     #$87
            jmp     COUT

RDBYTE      ldx     #$08
RDBYT2      pha
            jsr     RD2BIT
            pla
            rol
            ldy     #$3a
            dex
            bne     RDBYT2
            rts


RD2BIT      jsr     RDBIT
RDBIT       dey
            lda     TAPEIN
            eor     LASTIN
            bpl     RDBIT
            eor     LASTIN
            sta     LASTIN
            cpy     #$80
            rts


HEADR       ldy     #$4b
            jsr     ZERDLY
            bne     HEADR
            adc     #$fe
            bcs     HEADR
            ldy     #$21
WRBIT       jsr     ZERDLY
            iny
            iny
ZERDLY      dey
            bne     ZERDLY
            bcc     WRTAPE
            ldy     #$32
ONEDLY      dey
            bne     ONEDLY
WRTAPE      ldy     TAPEOUT
            ldy     #$2c
            dex
            rts


NXTA1       lda     A1L
            cmp     A2L
            lda     A1H
            sbc     A2H
            inc     A1L
            bne     RTS
            inc     A1H
RTS         rts
