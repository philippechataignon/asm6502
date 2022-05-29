k_start = $1234
k_end = $5678

.if DIRECT
* = $900
.fi

send        lda k_start
            jsr ssc.putc
send1l = * - 2
send1h = * - 1
            inc send1l
            bne +
            inc send1h
+           lda send1h
            cmp #>k_end
send2h = * - 1
            bne send
            lda send1l
            cmp #<k_end
send2l = * - 2
            bne send
            rts

recv        jsr ssc.getc
            sta k_start
recv1l = * - 2
recv1h = * - 1
            inc recv1l
            bne +
            inc recv1h
+           lda recv1h
            cmp #>k_end
recv2h = * - 1
            bne recv
            lda recv1l
            cmp #<k_end
recv2l = * - 2
            bne recv
            rts

.if DIRECT
ssc         .binclude "ssc.s"
.fi
