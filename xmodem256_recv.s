; XMODEM Receiver for the 6502
DIRECT := false

; zero page variables
blknum  = $06                   ; block number
chksum  = $19                   ; blksum

; buffer: $1000 -> max $95FF
start   = $1000
limit = $96                     ; don't write after $9600


automod = $1234                 ; fake automodified address

; monitor
cout = $fded
crout = $fd8e
home = $fc58

endofline1 = $427

; XMODEM Control Character Constants
SOH = $01                       ; start block
EOT = $04                       ; end of text marker
ACK = $06                       ; good block acknowledged
NAK = $15                       ; bad block acknowledged

.include "apple_enc.inc"
.enc "none"

.include "macros.inc"

; macros
safe_getc       .macro
                jsr ssc.getc3s          ; get chksum
                bcc TimeoutAbort        ; chr recv error, flush and send NAK
                sta endofline1
                .endm

*       =  $900

XModemRecv      jsr ssc.init
                jsr ssc.flush
                jsr home
                print RecvMsg
                move #start,ptr_mod     ; set ptr_mod to start
                lda #1                  ; set block # to 1
                sta blknum
                lda #0                  ; init chksum
                sta chksum
-               lda #NAK                ; send NAK to start transfer
                jsr ssc.putc
                jsr ssc.getc3s          ; wait for input
                bcc -                   ; no reply, resend NAK
                bcs +                   ; receive a byte
StartRecv       lda #0                  ; reinit chksum
                sta chksum
-               jsr ssc.getc3s          ; wait for input
                bcc -                   ; timed out, keep waiting...
+               cmp #SOH                ; start of block?
                beq StartBlk            ; yes
                cmp #EOT                ; end of transmission ?
                bne ProcAbort           ; Not SOH or EOT, so flush buffer & send NAK
EndRecv         lda #ACK                ; last block, send ACK and exit.
                jsr ssc.putc
                jsr ssc.flush           ; get leftover characters, if any
                print GoodMsg
                rts

SendNak         jsr ssc.flush           ; flush the input port
                lda #NAK
                jsr ssc.putc            ; send NAK to resend block
                jmp StartRecv           ; start over, get the block again

Blk1Abort       ldy #$11
                bne Abort
Blk2Abort       ldy #$12
                bne Abort
ChkAbort        ldy #$13
                bne Abort
TimeoutAbort    ldy #$14
                bne Abort
ProcAbort       ldy #$15
                bne Abort
LimAbort        ldy #$16
Abort           brk
                ;jsr ssc.flush           ; yes, too many errors, flush buffer,
                ;print AbortMsg
                ;rts

StartBlk        safe_getc               ; get byte and send nak if timeout
                cmp blknum              ; compare to expected block #
                bne Blk1Abort
+               safe_getc
                eor #$ff                ; neg block number
                cmp blknum              ; compare to expected
                bne Blk2Abort
+               ldy #0
-               safe_getc
                sta automod,y           ; good char, save it in the recv buffer
ptr_mod = * - 2
                clc                     ; update chksum
                adc chksum
                sta chksum
                iny                     ; inc buffer pointer
                bpl -                   ; continue until $80=128 bits
                safe_getc
                cmp chksum              ; compare to calculated checksum
                bne ChkAbort            ; uncorrect chksum, abort
                inc blknum              ; done.  Inc the block #
                lda ptr_mod             ; ptr_modL $0 <-> $80
                eor #$80
                sta ptr_mod
                bne +                   ; if $0, next page so
                inc ptr_mod+1           ; increment ptr_modH
                lda ptr_mod+1           ; test if ptr_mod >= limit
                cmp #limit
                beq LimAbort            ; yes, abort
.enc "apple"
                lda #'.'
                jsr cout
+               lda #ACK                ; send ACK
                jsr ssc.putc
                jmp StartRecv           ; get next block

ssc             .binclude "ssc.s"
printstr        .binclude "printstr.s"

GoodMsg         .null "\nOK\n"
AbortMsg        .null "ABORTED!\n"
RecvMsg         .null "XMODEM256 RECV\n"
