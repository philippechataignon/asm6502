print       .macro
_cout       = $fded
            ldy #0
-           lda \1,y
            beq +
            jsr _cout
            iny
            bne -
+
            .endm

printsafe   .macro
            pha
            tya
            pha
            print \1
            pla
            tay
            pla
            .endm

move        .macro
            lda >\1
            sta \2+1
            lda <\1
            sta \2
            .endm

moveay      .macro
            lda >\1
            sta \2+1
            ldy <\1
            sty \2
            .endm

move3       .macro
            lda \2
            sta \3
            lda \1
            sta \3+1
            .endm

st_rwts     .macro
            lda \2
            ldy #\3
            sta (\1),y
            .endm
