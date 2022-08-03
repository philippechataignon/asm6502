line21      = $6D0

clrstatus
            lda #" "            ; space
            ldx #linewidth-1    ; clear line
-           sta line21,x
            dex
            bpl -
            lda #statusline      ; vert
            jsr bascalc          ; move cursor to status line
            lda #0
            sta ch               ; horiz
            rts
draw
            clc
            lda secnum          ; num line = sector + 4
            adc #4
            jsr bascalc
            clc
            lda trknum          ; num col = track + 4
            adc #4
            tay
            txa
            sta (basl),y        ; store char in screen ram
-           rts

screen      .text "TRACK\n"
            .text "    00000000000000001111111111111111222\n"
            .text "    0123456789ABCDEF0123456789ABCDEF012\n"
            .text "    -----------------------------------\n"
            .text "  0:\n"
            .text "  1:\n"
            .text "  2:\n"
            .text "  3:\n"
            .text "  4:\n"
            .text "S 5:\n"
            .text "E 6:\n"
            .text "C 7:\n"
            .text "T 8:\n"
            .text "O 9:\n"
            .text "R A:\n"
            .text "  B:\n"
            .text "  C:\n"
            .text "  D:\n"
            .text "  E:\n"
            .text "  F:\n"
            .byte 0
