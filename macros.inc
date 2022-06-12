print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr.main
                .endm

move            .macro
                lda <\1
                sta \2
                lda >\1
                sta \2+1
                .endm

move3           .macro
                lda \2
                sta \3
                lda \1
                sta \3+1
                .endm
