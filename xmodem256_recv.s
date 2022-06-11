; XMODEM Receiver for the 6502

; zero page variables
blknum  = $06                   ; block number
nblknum = $e3                   ; negative block number
blksum  = $19                   ; blksum

; buffer: $1000 -> max $95FF
start   = $1000
limit = $96                     ; don't write after $9600


automod = $1234                 ; fake automodified address

; monitor
cout = $fded
crout = $fd8e

; XMODEM Control Character Constants
SOH = $01                       ; start block
EOT = $04                       ; end of text marker
ACK = $06                       ; good block acknowledged
NAK = $15                       ; bad block acknowledged

.include "apple_enc.inc"
.enc "none"

; macros
print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

move            .macro
                lda <\1
                sta \2
                lda >\1
                sta \2+1
                .endm

safe_getc        .macro
                jsr ssc.getc3s          ; get blksum
                bcc SendNack            ; chr recv error, flush and send NAK
                .endm

*       =  $900

XModemRecv      jsr ssc.init
                jsr ssc.flush
                print RecvMsg
                move #start,ptr         ; set ptr to start
                lda #1                  ; set block # to 1
                sta blknum
-               lda #NAK                ; NAK start with blksum mode
                jsr ssc.putc            ; send it
                jsr ssc.getc3s          ; wait for input
                bcc -                   ; resend NAK
                bcs +                   ; receive a byte !
StartBlk        lda #0                  ; init blksum
                sta blksum
-               jsr ssc.getc3s          ; wait for input
                bcc -                   ; timed out, keep waiting...
+               cmp #SOH                ; start of block?
                beq GetBlk              ; yes
                cmp #EOT                ; end of transmission ?
                bne SendNack            ; Not SOH or EOT, so flush buffer & send NAK
EndRecv         lda #ACK                ; last block, send ACK and exit.
                jsr ssc.putc
                jsr ssc.flush           ; get leftover characters, if any
                print GoodMsg
                rts

GetBlk          lda blknum
                eor #$ff
                sta nblknum             ; store expected blknum 1's compl
                safe_getc
                cmp blknum              ; compare to expected block #
                beq +                   ; matched!
                jmp PrtAbort            ; Unexpected block number - abort
                safe_getc
                cmp nblknum             ; compare to expected
                beq +                   ; matched!
                jmp PrtAbort            ; Unexpected block number - abort
Recvloop
+               ldx #0
                safe_getc
                sta automod,X           ; good char, save it in the recv buffer
ptr = * - 2
                clc                     ; update blksum
                adc blksum
                inx                     ; inc buffer pointer
                bpl -                   ; continue until $80=128 bits
                safe_getc
                cmp blksum              ; compare to calculated checksum
                beq EndBlock            ; good blksum

SendNack        jsr ssc.flush           ; flush the input port
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
