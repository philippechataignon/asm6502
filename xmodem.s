; XMODEM/chksum Sender/Receiver for the 6502

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

retry   = $ce                ; retry counter
retry2  = $cf                ; 2nd counter

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

; ACIA variables
slot = 3
ACIA_Data    = $c088 + slot * $10
ACIA_Status  = $c089 + slot * $10
ACIA_Command = $c08a + slot * $10
ACIA_Control = $c08b + slot * $10

.include "apple_enc.inc"
.enc "none"

; macros
print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

*       =  $900

                jmp XModemSend
;;; recv
XModemRecv      jsr ACIA_Init
                jsr Flush
                print RecvMsg
                lda #0                ; init chksum
                sta chksum
                lda #1                ; set block # to 1
                sta blkno             
-               lda #NAK              ; NAK start with chksum mode
                jsr Put_Chr           ; send it
                jsr GetByte3s         ; wait for input
                bcs GotByte           ; byte received, process it
                bcc -                 ; resend NAK

StartBlk        lda #0
                sta chksum
-               jsr GetByte3s         ; get first byte of block
                bcc -                 ; timed out, keep waiting...
GotByte         cmp #SOH              ; start of block?
                beq GetBlk            ; yes
                cmp #EOT
                bne BadRecv           ; Not SOH or EOT, so flush buffer & send NAK
                jmp RDone             ; EOT - all done!

GetBlk          lda blkno
                eor #$ff
                sta temp              ; store expected blkno 1's compl
                jsr GetByte3s         ; get blkno
                bcc BadRecv           ; chr rcv error, flush and send NAK
                cmp blkno             ; compare to expected block #
                beq +                 ; matched!
                jmp Exit_Err          ; Unexpected block number - abort
+               jsr GetByte3s         ; get blkno 1's compl
                bcc BadRecv           ; chr rcv error, flush and send NAK
                cmp temp              ; compare to expected
                beq +                 ; matched!
                jmp Exit_Err          ; Unexpected block number - abort
Recvloop
+               ldx #0                ; 
-               jsr GetByte3s         ; get next byte
                bcc BadRecv           ; chr rcv error, flush and send NAK
                sta Rbuff,X           ; good char, save it in the rcv buffer
                clc                   ; update chksum
                adc chksum            
                inx                   ; inc buffer pointer
                cpx #128              ; <no> <-no> <128 bytes> <chksum>
                bne -                 ; get 128 characters
                jsr GetByte3s         ; get chksum
                bcc BadRecv           ; chr rcv error, flush and send NAK
                cmp chksum            ; compare to calculated checksum
                beq GoodChksum        ; good chksum
BadRecv         jsr Flush             ; flush the input port
                lda #NAK
                jsr Put_Chr           ; send NAK to resend block
                jmp StartBlk          ; start over, get the block again
GoodChksum      ldx #0
                ldy #0                ; set offset to zero
-               lda Rbuff,X           ; get data byte from buffer
                sta (ptr),Y           ; save to target
                inc ptr               ; point to next address
                bne +                 ; did it step over page boundary?
                inc ptr+1             ; adjust high address for page crossing
+               inx                   ; point to next data byte
                cpx #128              ; is it the last byte
                bne -                 ; no, get the next one
                inc blkno             ; done.  Inc the block #
                lda #ACK              ; send ACK
                jsr Put_Chr
                jmp StartBlk          ; get next block

RDone           lda #ACK              ; last block, send ACK and exit.
                jsr Put_Chr
                jsr Flush             ; get leftover characters, if any
                jmp Exit_Good

;;; send
XModemSend      jsr ACIA_Init
-               jsr GetByte           ; flush the port
                bcs -                 ; if chr recvd, wait for another
                print SendMsg
                lda #0
                sta lastblk           ; set flag to false
                sta blkno             ; set block # to 1
                jsr GetByte3s
                bcc -                 ; wait for something to come in...
                cmp #NAK              ; is it the NAK to start a chksum xfer?
                bne PrtAbort          ; not NAK, print abort msg and exit

LdBuffer                              ; start block
                ldx #0                ; init pointers
                stx chksum
                ldy #0                ; Y always = 0
                inc blkno             ; inc block counter

LdBuff          lda (ptr),Y           ; save 128 bytes of data
                sta Rbuff,X
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
                sta Rbuff,X
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
                jsr Put_chr           ; send SOH = start of header
                lda blkno             ; send block number
                jsr Put_chr
                eor #$FF              ; send block number 1's complement
                jsr Put_chr
-               lda Rbuff,X           ; send 128 bytes in buffer
                jsr Put_chr
                inx
                cpx #128              ; last byte?
                blt -                 ; no, get next
                lda chksum
                jsr Put_chr           ; send chksum
                jsr GetByte3s           ; Wait for Ack/Nack
                bcc Seterror          ; No chr received after 3 seconds, resend
                cmp #ACK              ; Chr received... is it:
                bne SetError          ; No ACK => error
                                      ; ACK, send next bloc
                lda lastblk           ; Was the last block sent?
                bne LdBuffer          ; no, send the next one
                jmp Exit_Good         ; yes, we're done
Seterror        dec errcnt            ; decr error counter
                bne SendBlock         ; if not null, resend block
PrtAbort        jsr Flush             ; yes, too many errors, flush buffer,
                jmp Exit_Err          ; print error msg and exit

; SSC routines
ACIA_Init       lda #$1F              ; 19.2K/8/1
                sta ACIA_Control      ; control reg
                lda #$0B              ; N parity/echo off/rx int off/ dtr active low
                sta ACIA_Command      ; command reg
                rts                   ; done
                                      ; input chr from ACIA (no waiting)
Get_Chr         clc                   ; no chr present
                lda ACIA_Status       ; get Serial port status
                and #%00001000              ; mask rcvr full bit
                beq +                 ; if not chr, done
                lda ACIA_Data         ; else get chr
                sec                   ; and set the Carry Flag
+               rts                   ; done
                                      ; output to OutPut Port
Put_Chr         pha                   ; save registers
-               lda ACIA_Status       ; serial port status
                and #%00010000              ; is tx buffer empty
                beq -                 ; no, go back and test it again
                pla                   ; yes, get chr to send
                sta ACIA_Data         ; put character to Port
                rts                   ; done

GetByte3s       lda #$ff              ; 3 seconds
                sta retry2
GetByte         lda #$00              ; wait for chr input and cycle timing loop
                sta retry             ; set low value of timing loop
-               jsr Get_chr           ; get chr from serial port, don't wait
                bcs +                 ; got one, so exit
                dec retry             ; no character received, so dec counter
                bne -
                dec retry2            ; dec hi byte of counter
                bne -
                clc                   ; if loop times out, CLC, else SEC and return
+               rts                   ; with character in A

Flush           lda #$ff/3            ; flush receive buffer
                sta retry2            ; flush until empty for ~1 sec.
                jsr GetByte           ; read the port
                bcs Flush             ; if chr recvd, wait for another
                rts                   ; else done

; exits
Exit_Err        print ErrMsg
                rts

Exit_Good       lda #EOT
                jsr Put_Chr
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
