print       .macro
            ldy #<\1
            lda #>\1
            jsr printstr.main
            .endm

printl      .macro
            print \1
            jsr $fd8e   ; crout
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
