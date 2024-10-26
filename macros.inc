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

prp         .macro
_cout      = $fded
            jmp +
_str        .ptext $8d,\1[::-1]
+           ldx _str
-           lda _str,x
            jsr _cout
            dex
            bne -
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
