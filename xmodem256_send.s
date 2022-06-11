; XMODEM/chksum Sender/Receiver for the 6502

; By Daryl Rictor Aug 2002

; A simple file transfer program to allow transfers between the SBC and a
; console device utilizing the x-modem/chksum transfer protocol.  Requires
; ~1200 bytes of either RAM or ROM, 132 bytes of RAM for the receive buffer,
; and 12 bytes of zero page RAM for variable storage.

; zero page variables
blknum  = $06                ; block number
nblknum = $07                ; 255 - block number
errcnt  = $08                ; error counter 10 is the limit
blksum  = $19                ; chksum

start   = $fa                ; data pointer (two byte variable)
end     = $fb
temp = $e3

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

; macros
print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

*       =  $900

XModemSend      jsr ssc.flush           ; flush ssc buffer
                print SendMsg
                lda #0
                sta blknum              ; set block counter to 0
                lda #255                ; blknum + nblknum always 255
                sta nblknum             ; set neg block counter to 255
-               jsr ssc.getc3s
                bcc -                   ; wait for something to come in...
                cmp #NAK                ; is it the NAK to start a chksum xfer?
                bne PrtAbort            ; not NAK, print abort msg and exit

                lda start
                sta ptr+1               ; ptrH = start
                lda #0
                sta ptr                 ; ptrL = 0

StartBlk        lda #10                 ; error counter set to
                sta errcnt              ; 10 max retries
                inc blknum              ; inc block counter
                dec nblknum             ; dec neg block counter
                lda #SOH
                jsr ssc.putc            ; send SOH = start of header
                lda blknum
                jsr ssc.putc            ; send count
                lda nblknum
                jsr ssc.putc            ; send neg count
                ldy #0                  ; Y =  0
                sty blksum              ; init blksum

Loop            lda automod,Y           ; send 128 bytes of data
ptr             = * - 2
                jsr ssc.putc            ; send current byte
                clc
                adc blksum
                iny
                beq EndLoop
                bpl Loop                ; Y : 0 -> 7F
                cpy #$80                ; end of loop1 -> send blksum
                bne Loop
EndLoop         lda blksum
                jsr ssc.putc            ; send chksum
                jsr ssc.getc3s          ; Wait for Ack/Nack
                bcc Seterror            ; No chr received after 3 seconds, resend
                cmp #ACK                ; Chr received... is it:
                bne SetError            ; No ACK => error
                inc ptr+1               ; next page
                lda ptr+1
                cmp end                 ; if < end, Loop again
                blt Loop
                jmp Exit_Good           ; yes, we're done

Seterror        dec errcnt              ; decr error counter
                bne StartBlk            ; if not null, resend block
PrtAbort        jsr ssc.flush           ; yes, too many errors, flush buffer,
                jmp Exit_Err            ; print error msg and exit

ssc             .binclude "ssc.s"

; exits
Exit_Err        print ErrMsg
                rts

Exit_Good       lda #EOT
                jsr ssc.putc
                print GoodMsg
                rts

; print subroutine
printstr
                sty printstr_mod
                sta printstr_mod+1
                ldy #0
-               lda automod,Y
printstr_mod = * - 2
                beq +                ; return if 0 = end of string
                jsr cout
                iny
                jmp -
+               jsr crout
                rts

                .enc "apple"
GoodMsg         .null "TRANSFER SUCCESSFUL!"
ErrMsg          .null "TRANSFER ERROR!"
SendMsg         .null "XMODEM SEND"
RecvMsg         .null "XMODEM RECV"
