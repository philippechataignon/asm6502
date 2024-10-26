* = $803
COUT = $FDED

.include "macros.inc"
.include "apple_enc.inc"
.enc "apple"
            .prc "HELLO WORLD1!"
            .prc "HELLO WORLD2!"
            .prp "HELLO WORLD3!"
            .prp "HELLO WORLD4!"

            jsr m.msgout
            .null "HELLO WORLD5!"
            jsr m.msgout
            .null "HELLO WORLD6!"
            rts

m           .binclude "msgout.s"
