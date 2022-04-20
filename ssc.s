kbd         = $c000
kbdstrobe   = $c010

esc         = $9B

sscslot     = $30               ; slot 3
sscreg      = $C088+sscslot
sscstatus   = $C089+sscslot
ssccommand  = $C08A+sscslot
ssccontrol  = $C08B+sscslot

k_start = $1234
k_end = $5678

.if DIRECT
* = $900
.fi

init        bit sscreg          ; reset ssc
            lda #$0B            ; no parity, rts on, dtr on, intr
            sta ssccommand
            lda #$1F            ; 19200, 8bits, no parity
            sta ssccontrol
            rts


putc        pha                     ; Push A onto the stack
-           lda sscstatus           ; Check status bits
            and #%00010000          ; Test bit 4 = transmit register empty if 1
            beq -                   ; Output register is full, so loop
            pla
            sta sscreg              ; Put character
            rts

getc
            lda kbd
            cmp #esc
            bne +
            bit kbdstrobe
            jmp exitkbd
+           lda sscstatus           ; Check status bits
            and #%00001000          ; Test bit3 = receive register full if 1
            beq getc                ; Input register empty, loop
            lda sscreg              ; Get character
            rts

send        lda k_start
            jsr putc
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

recv        jsr getc
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
exitkbd     .fill 1
.fi
