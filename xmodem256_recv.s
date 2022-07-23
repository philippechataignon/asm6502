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
prbyte = $fdda
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
getc_nak       .macro
                jsr ssc.getc3s          ; get char, timeout 3s
                bcc SendNak             ; timeout, flush and send NAK
                sta endofline1
                .endm

*       =  $900

XModemRecv      jsr ssc.init
                jsr ssc.flush
.if DIRECT
                jsr home
                print RecvMsg
.fi
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
.if DIRECT
                print GoodMsg
.fi
                rts

SendNak         jsr ssc.flush           ; flush the input port
                lda #NAK
                jsr ssc.putc            ; send NAK to resend block
                jmp StartRecv           ; start over, get the block again

ProcAbort       lda #$14
                bne Abort
LimAbort        lda #$15
Abort           
.if DIRECT
                jsr prbyte
                print ErrorMsg
.fi
                jmp ssc.flush

StartBlk        getc_nak                ; get byte and send nak if timeout
                cmp blknum              ; compare to expected block #
                bne SendNak             ; error, send NAK
+               getc_nak
                eor #$ff                ; neg block number
                cmp blknum              ; compare to expected
                bne SendNak             ; error, send NAK
+               ldy #0
-               getc_nak
                sta automod,y           ; good char, save it in the recv buffer
ptr_mod = * - 2
                clc                     ; update chksum
                adc chksum
                sta chksum
                iny                     ; inc buffer pointer
                bpl -                   ; continue until $80=128 bits
                getc_nak
                cmp chksum              ; compare to calculated checksum
                bne SendNak             ; error, send NAK
                ; block is well received, done
                inc blknum              ; inc the block #
                lda ptr_mod             ; ptr_modL $0 <-> $80
                eor #$80
                sta ptr_mod
                bne +                   ; if $0, next page so
                inc ptr_mod+1           ; increment ptr_modH
                lda ptr_mod+1           ; test if ptr_mod = limit
                cmp #limit
                beq LimAbort            ; yes, abort
.enc "apple"
                lda #'.'
                jsr cout
+               lda #ACK                ; send ACK
                jsr ssc.putc
                jmp StartRecv           ; get next block

ssc             .binclude "ssc.s"

.if DIRECT
GoodMsg         .null "\nOK\n"
RecvMsg         .null "XMODEM256 RECV\n"
ErrorMsg        .null " ERROR CODE\n"
.fi
