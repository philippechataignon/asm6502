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

prints      .macro
            pha
            tya
            pha
            print \1
            pla
            tay
            pla
            .endm

move        .macro
            lda <\1
            sta \2
            lda >\1
            sta \2+1
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
