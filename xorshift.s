rndl=$ce
rndh=rndl + 1

.if DIRECT
seed=12345
        lda #<seed
        sta rndl
        lda #>seed
        sta rndh
.fi
        ; You can get 8-bit random numbers in A or 16-bit numbers
        ; in rnd.
        ; X/Y unchanged.
xorshift:
        lda rndh
        lsr
        lda rndl
        ror
        eor rndh
        sta rndh    ; high part of x ^= x << 7 done
        ror         ; A has now x >> 9 and high bit comes from low byte
        eor rndl
        sta rndl    ; x ^= x >> 9 and the low part of x ^= x << 7 done
        eor rndh
        sta rndh    ; x ^= x << 8 done
        rts
