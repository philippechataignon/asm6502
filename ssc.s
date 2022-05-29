kbd         = $c000
kbdstrobe   = $c010

esc         = $9B

sscslot     = $30               ; slot 3
sscreg      = $C088+sscslot
sscstatus   = $C089+sscslot
ssccommand  = $C08A+sscslot
ssccontrol  = $C08B+sscslot

.if DIRECT
* = $900
; exit_kbd must be assigned to exit jsr when escape
exitkbd = $3D0
.fi

init        bit sscreg          ; reset ssc
            lda #%00001011            ; 7-5 no parity, 4 no echo, 3-2 disable transmit intr
            sta ssccommand            ; rts low, 1 irq disabled, 0dtr enable
            lda #%00011111            ; 7 1 stop bit, 6-5 8bits, 4 baud rate gen, 3-0 19200
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
