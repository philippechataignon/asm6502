* = $803
COUT = $FDED

.include "macros.inc"
.include "apple_enc.inc"
.enc "apple"
            .prc "HELLO WORLD!\n"
            .prc "HELLO WORLD!2\n"
            .prp "HELLO WORLD!\n"
            .prp "HELLO WORLD!2\n"
.enc "apple_inv"
            .prc "HELLO WORLD!\n"
            .prc "HELLO WORLD!2\n"
            .prp "HELLO WORLD!\n"
            .prp "HELLO WORLD!2\n"
.enc "none"
            .pra "HELLO WORLD!\n"
            .pra "HELLO WORLD!2\n"
            rts
