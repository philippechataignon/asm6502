; XMODEM Sender for the 6502

; transfer 256 bytes pages over serial using xmodem protocol
; exemple : 1000-17FF
; FA:10 18 B00G

; zero page variables
blknum  = $06                ; block number
errcnt  = $08                ; error counter 10 is the limit
blksum  = $e3                ; chksum

start   = $fa                ; data pointer (two byte variable)
end     = $fb

cout = $fded
prbyte = $fdda

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged

.include "apple_enc.inc"
.enc "none"
.include "macros.inc"

.if DIRECT
*       =  $B00
.fi

XModemSend
                jsr ssc.init            ; init serial card 19200 8n1
                jsr ssc.flush           ; flush ssc buffer
.if DIRECT
                prt TitleMsg
.fi
                lda #0
                sta blknum              ; set block counter to 0
-               jsr ssc.getc3s
                bcc -                   ; wait for something to come in...
                cmp #NAK                ; is it the NAK to start a chksum xfer?
                beq +
.if DIRECT
                jmp Abort               ; not NAK, abort
.else
                jmp -
.fi
+
.if DIRECT
                prt StartMsg
.fi
                lda #0                  ; write start to ptr
                sta ptr
                lda start
                sta ptr+1
NextBlk         inc blknum              ; inc block counter
                lda #10                 ; error counter set to
                sta errcnt              ; 10 max retries
StartBlk
.if DIRECT
                lda #'.'+$80
                jsr cout
.fi
                lda #SOH
                jsr ssc.putc            ; send SOH = start of header
                lda blknum
                jsr ssc.putc            ; send count
                eor #$FF
                jsr ssc.putc            ; send neg count
                lda #0                  ; Y =  0
                sta blksum              ; init blksum
-               lda $1234,y             ; send 128 bytes of data
ptr             = * - 2
                jsr ssc.putc            ; send current byte
                clc
                adc blksum              ; add mod 256 to blksum
                sta blksum
                iny
                beq EndLoop             ; end of loop2
                bpl -                   ; in loop1
                cpy #$80                ; end of loop1 == start of loop2
                bne -                   ; in loop2
EndLoop         lda blksum              ; end of loop1 (Y==$80) or loop2 (Y==$0)
                jsr ssc.putc            ; send chksum
                jsr ssc.getc3s          ; Wait for Ack/Nack
                bcc SetError            ; No chr received after 3 seconds, resend
                cmp #ACK                ; Chr received... is it:
                bne SetError            ; No ACK => error
                cpy #128                ; if end of loop1, next block
                beq NextBlk             ; after blksum sending
                inc ptr+1               ; next page
                lda ptr+1
                cmp end                 ; if < end, Loop again
                bne NextBlk
ExitSend        lda #EOT                ; send final EOT
                jsr ssc.putc
.if DIRECT
                prt GoodMsg
.fi
                rts

SetError        dec errcnt              ; decr error counter
                jsr prbyte
                prt RetryMsg
                lda errcnt
                jsr prbyte
                bne StartBlk            ; if not null, resend block
Abort           jsr ssc.flush           ; yes, too many errors, flush buffer,
Exit_Err        prt ErrMsg
                rts

ssc             .binclude "ssc.s"

                .enc "apple"
.if DIRECT
TitleMsg        .null "WAIT NAK\n"
StartMsg        .null "GET NAK\n"
GoodMsg         .null "\nOK\n"
.fi
ErrMsg          .null "\nERROR!\n"
RetryMsg        .null "\nRETRY "
