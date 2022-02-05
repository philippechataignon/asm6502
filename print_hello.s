*       = $1000

defstr  macro
        abyte +$80,\1
        byte 0
        endm

print   macro
        lda #>\1
        ldy #<\1
        jsr PRINTSTR
        endm

        print MSG
        print MSG2
        print MSG3
        print MSG4
        rts

        include "printstr.s"

MSG     defstr "HELLO WORLD!"
MSG2    defstr "HELLO WORLD2!\r"
MSG3    defstr "HELLO WORLD3!\r"
MSG4    defstr "HELLO WORLD4!\r"
