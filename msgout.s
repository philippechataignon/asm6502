* = $803
COUT = $FDED

.include "macros.inc"
.include "apple_enc.inc"
.enc "apple"

            jsr msgout              ; PC + 2 is pushed
            .null "HELLO WORLD!\n"
            jsr msgout
            .null "HELLO WORLD!2\n"
            rts

msgout      pla                     ; get calling addr in PTR
            sta PTR
            pla
            sta PTR+1
-           incr PTR
            lda PTR
PTR         = * - 2
            beq _exit
            jsr COUT
            jmp -
_exit       lda PTR+1               ; PTR point to next instr
            pha
            lda PTR
            pha
            rts                     ; use PTR as return addr
