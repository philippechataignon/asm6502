i       = $FA
data    = $1000

*       = $803

loop0   ldx #0
loop1   lda data+1,x
        cmp data,x
        blt swap
        bge next
swap    lda data+1,x
        ldy data,x
        sta data,x
        tya
        sta data+1,x
next    inx
        cpx i
        blt loop1
        ldx i
        beq exit
        dex
        stx i
        jmp loop0
exit    rts
