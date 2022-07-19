automod = $1234
cout = $fded
crout = $fd8e

; print subroutine
main    pha
        tya
        pha
        sty addr_mod
        sta addr_mod+1
        ldy #0
-       lda automod,y
addr_mod = * - 2
        beq +
        jsr cout
        iny
        jmp -
+       pla
        tya
        pla
        rts
