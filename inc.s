DIRECT := false

* = $800

    lda #0
    sta inc1.var
    sta inc2.var
    sta inc1.param1
    sta inc1.param2
    sta inc1.param3
    sta inc1.param4
    rts

inc1    .binclude "inc1.s"
inc2    .binclude "inc2.s"
