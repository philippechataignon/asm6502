; XMODEM/chksum Sender/Receiver for the 6502

DIRECT := false

; By Daryl Rictor Aug 2002

; A simple file transfer program to allow transfers between the SBC and a
; console device utilizing the x-modem/chksum transfer protocol.  Requires
; ~1200 bytes of either RAM or ROM, 132 bytes of RAM for the receive buffer,
; and 12 bytes of zero page RAM for variable storage.

; zero page variables
lastblk = $06                ; flag for last block
blkno   = $07                ; block number
errcnt  = $08                ; error counter 10 is the limit
chksum  = $19                ; chksum

ptr     = $fa                ; data pointer (two byte variable)
ptrh    = $fb

eofp    = $fc                ; end of file address pointer (2 bytes)
eofph   = $fd

ssc.exitkbd := Abort


temp = $e3

; non-zero page variables and buffers
Rbuff   = $240               ; 128 bytes buffer
automod = $1200              ; fake automodified address

; monitor
cout = $fded
crout = $fd8e

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged

.include "apple_enc.inc"
.enc "none"

.include "macros.inc"

*       =  $900

                jmp XModemSend
;;; recv
XModemRecv      jsr ssc.init
                jsr ssc.flush
                print RecvMsg
                lda #0                ; init chksum
                sta chksum
                lda #1                ; set block # to 1
                sta blkno
-               lda #NAK              ; NAK start with chksum mode
                jsr ssc.putc           ; send it
                jsr ssc.getc3s         ; wait for input
                bcs GotByte           ; byte received, process it
                bcc -                 ; resend NAK

StartBlk        lda #0
                sta chksum
-               jsr ssc.getc3s         ; get first byte of block
                bcc -                 ; timed out, keep waiting...
GotByte         cmp #SOH              ; start of block?
                beq GetBlk            ; yes
                cmp #EOT
                bne BadRecv           ; Not SOH or EOT, so flush buffer & send NAK
                jmp RDone             ; EOT - all done!

GetBlk          lda blkno
                eor #$ff
                sta temp              ; store expected blkno 1's compl
                jsr ssc.getc3s         ; get blkno
                bcc BadRecv           ; chr rcv error, flush and send NAK
                cmp blkno             ; compare to expected block #
                beq +                 ; matched!
                jmp Exit_Err          ; Unexpected block number - abort
+               jsr ssc.getc3s         ; get blkno 1's compl
                bcc BadRecv           ; chr rcv error, flush and send NAK
                cmp temp              ; compare to expected
                beq +                 ; matched!
                jmp Exit_Err          ; Unexpected block number - abort
Recvloop
+               ldx #0                ;
-               jsr ssc.getc3s         ; get next byte
                bcc BadRecv           ; chr rcv error, flush and send NAK
                sta Rbuff,x           ; good char, save it in the rcv buffer
                clc                   ; update chksum
                adc chksum
                inx                   ; inc buffer pointer
                cpx #128              ; <no> <-no> <128 bytes> <chksum>
                bne -                 ; get 128 characters
                jsr ssc.getc3s         ; get chksum
                bcc BadRecv           ; chr rcv error, flush and send NAK
                cmp chksum            ; compare to calculated checksum
                beq GoodChksum        ; good chksum
BadRecv         jsr ssc.flush             ; flush the input port
                lda #NAK
                jsr ssc.putc           ; send NAK to resend block
                jmp StartBlk          ; start over, get the block again
GoodChksum      ldx #0
                ldy #0                ; set offset to zero
-               lda Rbuff,x           ; get data byte from buffer
                sta (ptr),y           ; save to target
                inx                   ; point to next data byte
                iny                   ; point to next address
                cpx #128              ; is it the last byte
                bne -                 ; no, get the next one
                inc blkno             ; done.  Inc the block #
                lda #ACK              ; send ACK
                jsr ssc.putc
                jmp StartBlk          ; get next block

RDone           lda #ACK              ; last block, send ACK and exit.
                jsr ssc.putc
                jsr ssc.flush             ; get leftover characters, if any
                jmp Exit_Good

;;; send
XModemSend      jsr ssc.flush
                print SendMsg
                lda #0
                sta lastblk           ; set flag to false
                sta blkno             ; set block # to 1
-               jsr ssc.getc3s
                bcc -                 ; wait for something to come in...
                cmp #NAK              ; is it the NAK to start a chksum xfer?
                bne Abort             ; not NAK, print abort msg and exit

LdBuffer                              ; start block
                ldx #0                ; init pointers
                stx chksum
                ldy #0                ; Y always = 0
                inc blkno             ; inc block counter

LdBuff          lda (ptr),y           ; save 128 bytes of data
                sta Rbuff,x
                clc
                adc chksum
                lda eofp
                cmp ptr               ; Are we at the last address?
                bne +                 ; no, inc pointer and continue
                lda eofph
                cmp ptrh
                bne +                 ; No last byte, continue
                inc lastblk           ; Yes, Set last byte flag
-               inx
                cpx #128              ; Are we at the end of the 128 byte block?
                beq SendBlock         ; Yes, send the block
                lda #0                ; Fill rest of 128 bytes with $00
                sta Rbuff,x
                beq -                 ; Branch always
+               inc ptr               ; Inc address pointer
                bne +
                inc ptrh
+               inx
                cpx #128              ; last byte in block?
                bne LdBuff            ; no, get the next

                lda #10               ; error counter set to
                sta errcnt            ; 10 max retries
SendBlock       ldx #0
                lda #SOH
                jsr ssc.putc          ; send SOH = start of header
                lda blkno             ; send block number
                jsr ssc.putc
                eor #$FF              ; send block number 1's complement
                jsr ssc.putc
-               lda Rbuff,x           ; send 128 bytes in buffer
                jsr ssc.putc
                inx
                cpx #128              ; last byte?
                blt -                 ; no, get next
                lda chksum
                jsr ssc.putc          ; send chksum
                jsr ssc.getc3s        ; Wait for Ack/Nack
                bcc SetError          ; No chr received after 3 seconds, resend
                cmp #ACK              ; Chr received... is it:
                bne SetError          ; No ACK => error
                                      ; ACK, send next bloc
                lda lastblk           ; Was the last block sent?
                bne LdBuffer          ; no, send the next one
                jmp Exit_Good         ; yes, we're done
SetError        dec errcnt            ; decr error counter
                bne SendBlock         ; if not null, resend block
Abort           jsr ssc.flush             ; yes, too many errors, flush buffer,
                jmp Exit_Err          ; print error msg and exit

ssc             .binclude "ssc.s"

; exits
Exit_Err        print ErrMsg
                rts

Exit_Good       lda #EOT
                jsr ssc.putc
                print GoodMsg
                rts

; print subroutine
printstr        .binclude "printstr.s"

                .enc "apple"
GoodMsg         .null "TRANSFER SUCCESSFUL!"
ErrMsg          .null "TRANSFER ERROR!"
SendMsg         .null "XMODEM SEND"
RecvMsg         .null "XMODEM RECV"
