NDELAY      = $ef

            ldy #$80
            ldx #0
_loop1      dex             ;2 cycles
            bne _loop1      ;3c => 5c * 256 = 1280 µs = 1_28 ms
            dey
            bne _loop1      ;256 * 1_28 ms + 256 * 5 = 257 * 1_28 = 330 ms ~ 1/3 s
            dec NDELAY
            bne _loop1
            rts
