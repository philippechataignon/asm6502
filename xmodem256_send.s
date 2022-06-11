; XMODEM/chksum Sender/Receiver for the 6502

; By Daryl Rictor Aug 2002

; A simple file transfer program to allow transfers between the SBC and a
; console device utilizing the x-modem/chksum transfer protocol.  Requires
; ~1200 bytes of either RAM or ROM, 132 bytes of RAM for the receive buffer,
; and 12 bytes of zero page RAM for variable storage.

; zero page variables
blknum  = $06                ; block number
errcnt  = $08                ; error counter 10 is the limit
blksum  = $19                ; chksum

start   = $fa                ; data pointer (two byte variable)
end     = $fb

automod = $1234

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged

.include "apple_enc.inc"
.enc "none"

.include "macros.inc"

*       =  $900

XModemSend      jsr ssc.flush           ; flush ssc buffer
                print SendMsg
                lda #0
                sta blknum              ; set block counter to 0
-               jsr ssc.getc3s
                bcc -                   ; wait for something to come in...
                cmp #NAK                ; is it the NAK to start a chksum xfer?
                bne Abort               ; not NAK, abort

                lda start
                sta ptr+1               ; ptrH = start
                lda #0
                sta ptr                 ; ptrL = 0 (always)

StartBlk        lda #10                 ; error counter set to
                sta errcnt              ; 10 max retries
                inc blknum              ; inc block counter
                lda #SOH
                jsr ssc.putc            ; send SOH = start of header
                lda blknum
                jsr ssc.putc            ; send count
                eor #$FF
                jsr ssc.putc            ; send neg count
                ldy #0                  ; Y =  0
                sty blksum              ; init blksum

Loop            lda automod,Y           ; send 128 bytes of data
ptr             = * - 2
                jsr ssc.putc            ; send current byte
                clc
                adc blksum              ; add mod 256 to blksum
                sta blksum
                iny
                beq EndLoop             ; if end of loop2, EndLoop
                bpl Loop                ; if in loop1, then Loop
                cpy #$80                ; end of loop1 == start of loop2
                bne Loop                ; if in loop2, then Loop
EndLoop         lda blksum              ; end of loop1 (y==$80) or loop2 (y==$0)
                jsr ssc.putc            ; send chksum
                jsr ssc.getc3s          ; Wait for Ack/Nack
                bcc Seterror            ; No chr received after 3 seconds, resend
                cmp #ACK                ; Chr received... is it:
                bne SetError            ; No ACK => error
                cpy #$80                ; if end of loop1, return to Loop
                beq Loop                ; after blksum sending
                inc ptr+1               ; next page
                lda ptr+1
                cmp end                 ; if < end, Loop again
                blt Loop
ExitSend        lda #EOT                ; send final EOT
                jsr ssc.putc
                print GoodMsg
                rts

Seterror        dec errcnt              ; decr error counter
                bne StartBlk            ; if not null, resend block
Abort           jsr ssc.flush           ; yes, too many errors, flush buffer,
Exit_Err        print ErrMsg
                rts

ssc             .binclude "ssc.s"

; print subroutine
printstr .binclude "printstr.s"

                .enc "apple"
GoodMsg         .null "TRANSFER OK"
ErrMsg          .null "TRANSFER ABORTED!"
SendMsg         .null "XMODEM256 SEND"
