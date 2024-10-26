* = $803
COUT = $FDED

.include "macros.inc"
.include "apple_enc.inc"
.enc "apple"
            .prc "HELLO WORLD1!\n"
            .prc "HELLO WORLD2!\n"
            .prp "HELLO WORLD3!"
            .prp "HELLO WORLD4!"

            jsr m.msgout
            .null "HELLO WORLD5!\n"
            jsr m.msgout
            .null "HELLO WORLD6!\n"
            .prc "HELLO WORLD7!\n"
            .prc "HELLO WORLD8!\n"
            rts

m           .binclude "msgout.s"
