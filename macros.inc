; prt string at \1, ended with null char
; A, Y modified
prt         .macro
_coutz      = $fdf6
            ldy #0
-           lda \1,y
            beq +
            jsr _coutz
            iny
            bne -
+
            .endm

prc         .macro
_coutz      = $fdf6
            ldy #0
-           lda _str,y
            beq +
            jsr _coutz
            iny
            bne -
_str        .null \1
+
            .endm

pra         .macro
.edef "\n",$d
_coutz      = $fdf6
            ldy #0
-           lda _str,y
            bmi +
            ora #$80
            jsr _coutz
            iny
            jmp -
_str        .shift \1
+           jsr _coutz
            .endm

prp         .macro
_coutz      = $fdf6
            ldx _str
            ldy #1
-           lda _str,y
            jsr _coutz
            iny
            dex
            bne -
            beq +
_str        .ptext \1
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
