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
exitkbd := $3D0
.fi

; ssc init routine
init        bit sscreg              ; reset ssc
            lda #%00001011          ; 7-5 no parity, 4 no echo, 3-2 disable transmit intr
            sta ssccommand          ; rts low, 1 irq disabled, 0dtr enable
            lda #%00011111          ; 7 1 stop bit, 6-5 8bits, 4 baud rate gen, 3-0 19200
            sta ssccontrol
            rts

; blocking put routine : send A
putc        pha                     ; Push A onto the stack
-           lda sscstatus           ; Check status bits
            and #%00010000          ; Test bit 4 transmit register empty
            beq -                   ; Output register is full, so loop
            pla
            sta sscreg              ; Put character
            rts

; non blocking get routine
; carry set if char received in A
getc_nb
            clc                     ; no chr present
            lda sscstatus           ; get Serial port status
            and #%00001000          ; Test bit 3 recv full
            beq +                   ; if no char, done
            lda sscreg              ; else get char
            sec                     ; and set the Carry Flag
+           rts                     ; done

; blocking get routine
; char received in A
getc        jsr getc_nb             ; blocking get routine
            bcc getc
            rts

; non blocking get routine timeout = 3s
; carry set if char received in A
getc3s      ldx #$0               ; 3 seconds
            stx retry             ; set low value of timing loop
                                  ; internal loop ~ 11.7 ms
-           jsr getc_nb           ; get chr from serial port, don't wait
            bcs +                 ; got one, so exit with SEC
            dex                   ; no character received, so dec counter
            bne -
            dec retry             ; dec counter
            bne -
            clc                   ; if loop times out, CLC and return
+           rts                   ; with character in A

; flush buffer
flush
-           jsr getc_nb
            bcs -                 ; get char, get next
            rts                   ; else done
