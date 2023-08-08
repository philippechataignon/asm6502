; prt string at \1, ended with null char
; A, Y modified
prt         .macro
_cout       = $fded
            ldy #0
-           lda \1,y
            beq +
            jsr _cout
            iny
            bne -
+
            .endm

; prt string at \1, ended with null char
; no modified register
prs        .macro
            pha
            tya
            pha
            prt \1
            pla
            tay
            pla
            .endm

; move \1 to \2, # is OK and used for pointers
; no modified register
mvs         .macro
            pha
            lda <\1
            sta \2
            lda >\1
            sta \2+1
            pla
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
