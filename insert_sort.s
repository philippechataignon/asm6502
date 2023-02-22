i       = $FA
data    = $1000

*       = $803

loop0   ldy #0
loop1   lda data+1,y
        cmp data,y
        bpl +
        bvs next
        bvc swap
+       bvc next
swap    lda data+1,y
        ldx data,y
        sta data,y
        txa
        sta data+1,y
next    iny
        cpy i
        blt loop1
        ldy i
        beq exit
        dey
        sty i
        jmp loop0
exit    rts
