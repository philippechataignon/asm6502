.if DIRECT
* = $300
.fi

NB = $E3

            sta NB
            ldx #0
_loop0      ldy #10
_loop1      dex             ;2 cycles
            bne _loop1      ;3c => 5c * 256 = 1280 us = 1.28 ms
            dey
            bne _loop1      ;256 * 1_28 ms + 256 * 5 = 257 * 1_28 = 330 ms ~ 1/3 s
            dec NB
            bne _loop0
            rts
