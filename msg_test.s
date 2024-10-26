* = $803
COUT = $FDED

.include "macros.inc"
.include "apple_enc.inc"
.enc "apple"
            jsr m.msgout
            .null "HELLO WORLD5!\n"
            jsr m.msgout
            .null "HELLO WORLD6!\n"
            .prp "HELLO WORLD3!"
            .prp "HELLO WORLD4!"

            rts

m           .binclude "msgout.s"
