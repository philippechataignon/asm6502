automod = $1234

; monitor
cout = $fded
crout = $fd8e

; print subroutine
main            sty addr_mod
                sta addr_mod+1
                ldy #0
-               lda automod,Y
addr_mod = * - 2
                beq +                ; return if 0 = end of string
                jsr cout
                iny
                jmp -
+               jsr crout
                rts
