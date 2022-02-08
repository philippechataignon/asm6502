*       = $1000

;add     .sfunction _x, (_adder=$80, _x + _adder)

defstr  .macro
        abyte +$80,\1
        byte 0
        .endm

print   .macro
        lda #>\1
        ldy #<\1
        jsr PRINTSTR
        .endm

        print MSG
        print MSG2
        print MSG3
        print MSG4
        rts

        .include "printstr.s"

        .enc  "apple"
        .cdef " _",$B0
MSG     .null " !#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_",$8D
MSG2    .null "HELLO WORLD!"
MSG3    .null "HELLO WORLD3!"
MSG4    .null "HELLO WORLD4!"
