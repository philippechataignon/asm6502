automod = $1234

.if DIRECT
* = $900
ssc.exitkbd := exit
.fi


send        lda automod
send1l = * - 2
send1h = * - 1
            jsr ssc.putc
            inc send1l
            bne +
            inc send1h
+           lda send1h
            cmp #>automod
send2h = * - 1
            bne send
            lda send1l
            cmp #<automod
send2l = * - 1
            bne send
            rts

recv        jsr ssc.getc
            sta automod
recv1l = * - 2
recv1h = * - 1
            inc recv1l
            bne +
            inc recv1h
+           lda recv1h
            cmp #>automod
recv2h = * - 1
            bne recv
            lda recv1l
            cmp #<automod
recv2l = * - 1
            bne recv
exit        rts

.if DIRECT
ssc         .binclude "ssc.s"
.fi
