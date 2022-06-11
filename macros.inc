print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

move            .macro
                lda <\1
                sta \2
                lda >\1
                sta \2+1
                .endm
