automod = $1234
cout = $fded
crout = $fd8e

; print subroutine
main
        sty addr_mod
        sta addr_mod+1
        pha
        tya
        pha
        ldy #0
-       lda automod,y
addr_mod = * - 2
        beq +
        jsr cout
        iny
        jmp -
+       pla
        tay
        pla
        rts
