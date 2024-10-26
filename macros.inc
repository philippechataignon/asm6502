; prt string at \1, ended with null char
; A, Y modified
prt         .macro
_cout      = $fded
            ldy #0
-           lda \1,y
            beq +
            jsr _cout
            iny
            bne -
+
            .endm

prc         .macro
_cout      = $fded
            ldy #0
-           lda _str,y
            beq +
            jsr _cout
            iny
            bne -
_str        .null \1
+
            .endm

prp         .macro
_cout      = $fded
            ldx _str
-           lda _str,x
            jsr _cout
            dex
            bne -
            beq +
_str        .ptext \1[::-1]
+
            .endm

; move \1 to \2, # is OK and used for pointers
; after A = \1 MSB, Y = \1 LSB
mov        .macro
            ldy <\1
            lda >\1
            sty \2
            sta \2+1
            .endm

st_rwts     .macro
            lda \2
            ldy #\3
            sta (\1),y
            .endm

; increment 16 bits
incr        .macro
            inc \1
            bne +
            inc \1+1
+
            .endm
