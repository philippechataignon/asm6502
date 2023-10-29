rngl=$CE
rngh=rngl + 1

seed=12345
        lda #<seed
        sta rngl
        lda #>seed
        sta rngh
        ; You can get 8-bit random numbers in A or 16-bit numbers
        ; in rng. 
        ; X/Y unchanged.
random  lda rngh
        lsr
        lda rngl
        ror
        eor rngh
        sta rngh    ; high part of x ^= x << 7 done
        ror         ; A has now x >> 9 and high bit comes from low byte
        eor rngl
        sta rngl    ; x ^= x >> 9 and the low part of x ^= x << 7 done
        eor rngh 
        sta rngh    ; x ^= x << 8 done
        rts
