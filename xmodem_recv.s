; XMODEM/chksum Sender/Receiver for the 65C02

; By Daryl Rictor Aug 2002

; A simple file transfer program to allow transfers between the SBC and a
; console device utilizing the x-modem/chksum transfer protocol.  Requires
; ~1200 bytes of either RAM or ROM, 132 bytes of RAM for the receive buffer,
; and 12 bytes of zero page RAM for variable storage.

;**************************************************************************
; This implementation of XMODEM/chksum does NOT conform strictly to the
; XMODEM protocol standard in that it (1) does not accurately time character
; reception or (2) fall back to the Checksum mode.

; (1) For timing, it uses a crude timing loop to provide approximate
; delays.  These have been calibrated against a 1MHz CPU clock.  I have
; found that CPU clock speed of up to 5MHz also work but may not in
; every case.  Windows HyperTerminal worked quite well at both speeds!

; (2) Most modern terminal programs support XMODEM/chksum which can detect a
; wider range of transmission errors so the fallback to the simple checksum
; calculation was not implemented to save space.
;**************************************************************************

; Files transferred via XMODEM-chksum will have the load address contained in
; the first two bytes in little-endian format:
;  FIRST BLOCK
;     offset(0) = lo(load start address),
;     offset(1) = hi(load start address)
;     offset(2) = data byte (0)
;     offset(n) = data byte (n-2)

; Subsequent blocks
;     offset(n) = data byte (n)

; One note, XMODEM send 128 byte blocks.  If the block of memory that
; you wish to save is smaller than the 128 byte block boundary, then
; the last block will be padded with zeros.  Upon reloading, the
; data will be written back to the original location.  In addition, the
; padded zeros WILL also be written into RAM, which could overwrite other
; data.

;-------------------------- The Code ----------------------------

; zero page variables (adjust these to suit your needs)

lastblk = $06                ; flag for last block
blkno   = $07                ; block number
errcnt  = $08                ; error counter 10 is the limit

chksum  = $19                ; chksum lo byte  (two byte variable)

ptr     = $fa                ; data pointer (two byte variable)
ptrh    = $fb

eofp    = $fc                ; end of file address pointer (2 bytes)
eofph   = $fd

retry   = $ce                ; retry counter
retry2  = $cf                ; 2nd counter

temp = $e3

; non-zero page variables and buffers

Rbuff = $240                 ; 128 bytes receive buffer
mod = $1200                  ; fake automodified address

; monitor

cout = $fded
crout = $fd8e

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged

; ACIA variables
slot = $3
ACIA_Data    = $c088 + slot * $10
ACIA_Status  = $c089 + slot * $10
ACIA_Command = $c08a + slot * $10
ACIA_Control = $c08b + slot * $10

.include "apple_enc.inc"

; macros

print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

; Xmodem/chksum transfer routines
; By Daryl Rictor, August 8, 2002

; v1.0  released on Aug 8, 2002.

*       =  $900                         ; Start of program

; Enter this routine with the beginning address stored in the zero page address
; pointed to by ptr & ptrh and the ending address stored in the zero page address
; pointed to by eofp & eofph.

.enc "none"

XModemRcv       jsr ACIA_Init
                jsr Flush
                print RecvMsg
                lda #0
                sta chksum
                lda #1
                sta blkno               ; set block # to 1
-               lda #NAK                ; NAK start with chksum mode
                jsr Put_Chr             ; send it
                jsr GetByte3s           ; wait for input
                bcs GotByte             ; byte received, process it
                bcc -                   ; resend NAK

StartBlk        lda #0
                sta chksum
-               jsr GetByte3s           ; get first byte of block
                bcc -                   ; timed out, keep waiting...
GotByte         cmp #SOH                ; start of block?
                beq GetBlk              ; yes
                cmp #EOT
                bne BadRecv             ; Not SOH or EOT, so flush buffer & send NAK
                jmp RDone               ; EOT - all done!

GetBlk          lda blkno
                eor #$ff
                sta temp                ; store expected blkno 1's compl
                jsr GetByte3s           ; get blkno
                bcc BadRecv             ; chr rcv error, flush and send NAK
                cmp blkno               ; compare to expected block #
                beq +                   ; matched!
                jmp Exit_Err            ; Unexpected block number - abort
+               jsr GetByte3s           ; get blkno 1's compl
                bcc BadRecv             ; chr rcv error, flush and send NAK
                cmp temp                ; compare to expected
                beq +                   ; matched!
                jmp Exit_Err            ; Unexpected block number - abort
Recvloop
+               ldx #0                  ; 
-               jsr GetByte3s           ; get next byte
                bcc BadRecv             ; chr rcv error, flush and send NAK
                sta Rbuff,X             ; good char, save it in the rcv buffer
                clc                     ; update chksum
                adc chksum              
                inx                     ; inc buffer pointer
                cpx #128                ; <no> <-no> <128 bytes> <chksum>
                bne -                   ; get 128 characters
                jsr GetByte3s           ; get chksum
                bcc BadRecv             ; chr rcv error, flush and send NAK
                cmp chksum              ; compare to calculated checksum
                beq GoodChksum          ; good chksum
BadRecv         jsr Flush               ; flush the input port
                lda #NAK
                jsr Put_Chr             ; send NAK to resend block
                jmp StartBlk            ; start over, get the block again
GoodChksum      ldx #0
                ldy #0                  ; set offset to zero
-               lda Rbuff,X             ; get data byte from buffer
                sta (ptr),Y             ; save to target
                inc ptr                 ; point to next address
                bne +                   ; did it step over page boundary?
                inc ptr+1               ; adjust high address for page crossing
+               inx                     ; point to next data byte
                cpx #128                ; is it the last byte
                bne -                   ; no, get the next one
                inc blkno               ; done.  Inc the block #
                lda #ACK                ; send ACK
                jsr Put_Chr
                jmp StartBlk            ; get next block

RDone           lda #ACK                ; last block, send ACK and exit.
                jsr Put_Chr
                jsr Flush               ; get leftover characters, if any
                jmp Exit_Good

ACIA_Init       lda        #$1F              ; 19.2K/8/1
                sta        ACIA_Control      ; control reg
                lda        #$0B              ; N parity/echo off/rx int off/ dtr active low
                sta        ACIA_Command      ; command reg
                rts                          ; done
                                             ; input chr from ACIA (no waiting)
Get_Chr         clc                          ; no chr present
                lda        ACIA_Status       ; get Serial port status
                and        #%00001000              ; mask rcvr full bit
                beq        +                 ; if not chr, done
                lda        ACIA_Data         ; else get chr
                sec                          ; and set the Carry Flag
+               rts                          ; done
                                             ; output to OutPut Port
Put_Chr         pha                          ; save registers
-               lda        ACIA_Status       ; serial port status
                and        #%00010000              ; is tx buffer empty
                beq        -                 ; no, go back and test it again
                pla                          ; yes, get chr to send
                sta        ACIA_Data         ; put character to Port
                rts                          ; done

GetByte3s       lda #$ff             ; wait 3s
                sta retry2
GetByte         lda #$00             ; wait for chr input and cycle timing loop
                sta retry            ; set low value of timing loop
-               jsr Get_chr          ; get chr from serial port, don't wait
                bcs +                ; got one, so exit
                dec retry            ; no character received, so dec counter
                bne -
                dec retry2           ; dec hi byte of counter
                bne -
                clc                  ; if loop times out, CLC, else SEC and return
+               rts                  ; with character in A

Flush           lda #$70             ; flush receive buffer
                sta retry2           ; flush until empty for ~1 sec.
                jsr GetByte          ; read the port
                bcs Flush            ; if chr recvd, wait for another
                rts                  ; else done

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
-               lda mod,Y
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
RecvMsg         .null "XMODEM RECV"
