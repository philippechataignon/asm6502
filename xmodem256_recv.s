; XMODEM Receiver for the 6502

; zero page variables
blknum  = $06                ; block number
errcnt  = $08                ; error counter 10 is the limit
blksum  = $19                ; blksum

; start addr of buffer
start   = $1000
limit = $96                 ; don't write after $9600

temp = $e3

automod = $1234              ; fake automodified address

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

; macros
print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

*       =  $900

XModemRecv      jsr ssc.init
                jsr ssc.flush
                print RecvMsg
                lda #>start
                sta ptr+1               ; set ptr to start
                lda #<start
                sta ptr
                inx                     ; set block # to 1
                stx blknum
-               lda #NAK                ; NAK start with blksum mode
                jsr ssc.putc            ; send it
                jsr ssc.getc3s          ; wait for input
                bcc -                   ; resend NAK
                bcs +
StartBlk        lda #0                  ; init blksum
                sta blksum
-               jsr ssc.getc3s          ; wait for input
                bcc -                   ; timed out, keep waiting...
+               cmp #SOH                ; start of block?
                beq GetBlk              ; yes
                cmp #EOT
                bne BadRecv             ; Not SOH or EOT, so flush buffer & send NAK
                jmp RDone               ; EOT - all done!

GetBlk          lda blknum
                eor #$ff
                sta temp                ; store expected blknum 1's compl
                jsr ssc.getc3s          ; get blknum
                bcc BadRecv             ; chr rcv error, flush and send NAK
                cmp blknum              ; compare to expected block #
                beq +                   ; matched!
                jmp PrtAbort            ; Unexpected block number - abort
+               jsr ssc.getc3s          ; get blknum 1's compl
                bcc BadRecv             ; chr rcv error, flush and send NAK
                cmp temp                ; compare to expected
                beq +                   ; matched!
                jmp PrtAbort            ; Unexpected block number - abort
Recvloop
+               ldx #0
-               jsr ssc.getc3s          ; get next byte
                bcc BadRecv             ; chr rcv error, flush and send NAK
                sta automod,X           ; good char, save it in the rcv buffer
ptr = * - 2
                clc                     ; update blksum
                adc blksum
                inx                     ; inc buffer pointer
                bpl -                   ; continue until $80
                jsr ssc.getc3s          ; get blksum
                bcc BadRecv             ; chr rcv error, flush and send NAK
                cmp blksum              ; compare to calculated checksum
                beq EndBlock            ; good blksum

BadRecv         jsr ssc.flush           ; flush the input port
                lda #NAK
                jsr ssc.putc            ; send NAK to resend block
                jmp StartBlk            ; start over, get the block again

EndBlock        inc blknum              ; done.  Inc the block #
                lda ptr                 ; ptrL $0 <-> $80
                ora #$80
                sta ptr
                bne +                   ; if $0, next page so
                inc ptr+1               ; increment ptrH
                lda ptr+1               ; test if ptr >= limit
                cmp #limit
                bge PrtAbort            ; yes, abort
+               lda #ACK                ; send ACK
                jsr ssc.putc
                jmp StartBlk            ; get next block

RDone           lda #ACK                ; last block, send ACK and exit.
                jsr ssc.putc
                jsr ssc.flush           ; get leftover characters, if any
                print GoodMsg
                rts

PrtAbort        jsr ssc.flush           ; yes, too many errors, flush buffer,
                print ErrMsg
                rts

ssc             .binclude "ssc.s"

; exits


; print subroutine
printstr
                sty printstr_mod
                sta printstr_mod+1
                ldy #0
-               lda automod,Y
printstr_mod = * - 2
                beq +                   ; return if 0 = end of string
                jsr cout
                iny
                jmp -
+               jsr crout
                rts

                .enc "apple"
GoodMsg         .null "TRANSFER SUCCESSFUL!"
ErrMsg          .null "TRANSFER ERROR!"
RecvMsg         .null "XMODEM RECV"
