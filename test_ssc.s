DIRECT := false

automod = $1234

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged

ssc.exitkbd := norecv

.include "apple_enc.inc"
.enc "none"

.include "macros.inc"

*       =  $900


                jsr ssc.init
                jsr ssc.flush
                jsr ssc.getc3s
-               lda #SOH
                jsr ssc.putc
                lda #$01
                jsr ssc.putc
                eor #$FF
                jsr ssc.putc
                jsr ssc.getc3s
                bcc norecv
                cmp #ACK                ; Chr received... is it:
                beq getack
                print msg_retry
                jmp -
norecv          print msg_norecv
                rts
getack          print getack
                rts

ssc             .binclude "ssc.s"

; print subroutine
printstr        .binclude "printstr.s"

                .enc "apple"
msg_getack      .null "GET ACK OK\n"
msg_retry       .null "NO ACK RESEND\n"
msg_norecv      .null "NO RECV\n"
