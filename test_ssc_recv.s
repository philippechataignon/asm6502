DIRECT := false

automod = $1234

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged

home = $fc58
endofline1 = $427

ssc.exitkbd := exit

.include "apple_enc.inc"
.enc "none"

.include "macros.inc"

*       =  $803

safe_getc       .macro
                jsr ssc.getc3s
                bcc exit        ; if timeout, exit
                sta endofline1
                .endm

entry
                jsr home
                jsr ssc.init
                jsr ssc.flush
                lda #NAK
                jsr ssc.putc

                ldy #0
-               safe_getc
                sta $1000,y
                iny
                cpy #132
                bne -

                lda #ACK
                jsr ssc.putc

                ldy #0
-               safe_getc
                sta $1100,y
                iny
                cpy #132
                bne -
exit            rts

ssc             .binclude "ssc.s"

                .enc "apple"
msg_norecv      .null "NO RECV\n"
