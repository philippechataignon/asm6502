kbd         = $c000
kbdstrobe   = $c010

esc         = $9B

slot        = 3                 ; slot 3

sscslot     = slot << 4
sscreg      = $C088+sscslot
sscstatus   = $C089+sscslot
ssccommand  = $C08A+sscslot
ssccontrol  = $C08B+sscslot


; ssc init routine
init        bit sscreg              ; reset ssc
            lda #%00001011          ; 7-5 no parity, 4 no echo, 3-2 disable transmit intr
            sta ssccommand          ; rts low, 1 irq disabled, 0dtr enable
            lda #%00011111          ; 7 1 stop bit, 6-5 8bits, 4 baud rate gen, 3-0 19200
            sta ssccontrol
            rts

; blocking put routine : send A
putc        pha                     ; Push A onto the stack
            lda #%00010000          ; Load mask for bit 4 transmit register empty
-           bit sscstatus           ; Check status register
            beq -                   ; Output register is full, so loop
            pla                     ; Restore A from stack
            sta sscreg              ; Put character
            rts

; non blocking get routine
; carry set if char received in A
getc_nb
            clc                     ; no chr present
            lda #%00001000          ; Load mask for bit 3 recv full
            bit sscstatus           ; Check status register
            beq +                   ; if no char, done
            sec                     ; else set the Carry Flag
            lda sscreg              ; and get char
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

retry       .byte 0               ; retry counter
