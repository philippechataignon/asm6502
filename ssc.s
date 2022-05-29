kbd         = $c000
kbdstrobe   = $c010

esc         = $9B

sscslot     = $30               ; slot 3
sscreg      = $C088+sscslot
sscstatus   = $C089+sscslot
ssccommand  = $C08A+sscslot
ssccontrol  = $C08B+sscslot

retry   = $ce                ; retry counter

.if DIRECT
* = $900
; exit_kbd must be assigned to exit jsr when escape
exitkbd = $3D0
.fi

init        bit sscreg              ; reset ssc
            lda #%00001011          ; 7-5 no parity, 4 no echo, 3-2 disable transmit intr
            sta ssccommand          ; rts low, 1 irq disabled, 0dtr enable
            lda #%00011111          ; 7 1 stop bit, 6-5 8bits, 4 baud rate gen, 3-0 19200
            sta ssccontrol
            rts

putc        pha                     ; Push A onto the stack
-           lda sscstatus           ; Check status bits
            and #%00010000          ; Test bit 4 = transmit register empty if 1
            beq -                   ; Output register is full, so loop
            pla
            sta sscreg              ; Put character
            rts

; non blocking get routine
; carry set if char present

getc_nb
            lda kbd
            cmp #esc
            bne +
            bit kbdstrobe
            jmp exitkbd
+           clc                     ; no chr present
            lda sscstatus           ; get Serial port status
            and #%00001000          ; mask rcvr full bit
            beq +                   ; if not chr, done
            lda sscreg              ; else get chr
            sec                     ; and set the Carry Flag
+           rts                     ; done

getc        jsr getc_nb
            bcc getc
            rts

getc3s      lda #$ff              ; 3 seconds
            sta retry
getcwait    ldx #0                ; wait for chr input and cycle timing loop
            sta retry             ; set low value of timing loop
-           jsr getc_nb           ; get chr from serial port, don't wait
            bcs +                 ; got one, so exit
            dex                   ; no character received, so dec counter
            bne -
            dec retry             ; dec counter
            bne -
            clc                   ; if loop times out, CLC, else SEC and return
+           rts                   ; with character in A

flush       lda #$ff/3            ; flush receive buffer
            sta retry             ; flush until empty for ~1 sec.
            jsr getcwait          ; read the port
            bcs Flush             ; if chr recvd, wait for another
            rts                   ; else done
