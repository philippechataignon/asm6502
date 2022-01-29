            ifndef ORG
ORG         =     $300
            endif

            if ORG > 0
            org     ORG
            fi

N           = $ef

            ldy #0
            ldx #0
.loop1      dex             ;2 cycles
            bne .loop1      ;3c => 5c * 256 = 1280 µs = 1.28 ms
            dey
            bne .loop1      ;256 * 1.28 ms + 256 * 5 = 257 * 1.28 = 330 ms ~ 1/3 s
            dec N
            bne .loop1
            rts
