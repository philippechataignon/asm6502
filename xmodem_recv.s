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

chksum     = $19                ; chksum lo byte  (two byte variable)

ptr     = $fa                ; data pointer (two byte variable)
ptrh    = $fb

eofp    = $fc                ; end of file address pointer (2 bytes)
eofph   = $fd

retry   = $ce                ; retry counter
retry2  = $cf                ; 2nd counter

;temp    = $e3

; non-zero page variables and buffers

Rbuff   =        $8f00              ; temp 132 byte receive buffer
mod = $1200                         ; fake automodified address

; monitor

cout = $fded
crout = $fd8e

; XMODEM Control Character Constants
SOH = $01                ; start block
EOT = $04                ; end of text marker
ACK = $06                ; good block acknowledged
NAK = $15                ; bad block acknowledged
CAN = $18                ; cancel (not standard, not supported)
CR  = $0d                ; carriage return
LF  = $0a                ; line feed
ESC = $1b                ; ESC to exit

; ACIA variables
slot = 3
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
                lda #1
                sta blkno               ; set block # to 1
StartCrc        lda #NAK                ; NAK start with chksum mode
                jsr Put_Chr             ; send it
                lda #0
                sta chksum
                jsr GetByte3s           ; wait for input
                bcs GotByte             ; byte received, process it
                bcc StartCrc            ; resend NAK

StartBlk        jsr GetByte3s           ; get first byte of block
                bcc StartBlk            ; timed out, keep waiting...
GotByte         cmp #SOH                ; start of block?
                beq +                   ; yes
                cmp #EOT
                bne BadChksum           ; Not SOH or EOT, so flush buffer & send NAK
                jmp RDone               ; EOT - all done!
+               ldx #0
GetBlk          jsr GetByte3s           ; get next character
                bcc BadChksum           ; chr rcv error, flush and send NAK
                sta Rbuff,X             ; good char, save it in the rcv buffer
                inx                     ; inc buffer pointer
                cpx #131                ; <no> <-no> <128 bytes> <chksum>
                bne GetBlk              ; get 131 characters
                ldx #0
                lda Rbuff,X             ; get block # from buffer
                cmp blkno               ; compare to expected block #
                beq +                   ; matched!
                jsr Exit_Err            ; Unexpected block number - abort
                jsr Flush               ; mismatched - flush buffer and then do BRK
                lda #-3                 ; put error code in "A"
                brk                     ; err -3 = unexpected block # - fatal error - BRK or RTS
+               eor #$ff                ; 1's comp of block #
                inx
                cmp Rbuff,X             ; compare with expected 1's comp of block #
                beq +                   ; matched!
                jsr Exit_Err            ; Unexpected block number - abort
                jsr Flush               ; mismatched - flush buffer and then do BRK
                lda #-4                 ; put error code in "A"
                brk                     ; err -4 = bad 1's comp of block#
+               lda Rbuff,Y             ; get hi chksum from buffer
                cmp chksum              ; compare to calculated checksum
                beq GoodCrc             ; good chksum
BadChksum       jsr Flush               ; flush the input port
                lda #NAK
                jsr Put_Chr             ; send NAK to resend block
                jmp StartBlk            ; start over, get the block again
GoodCrc         ldx #$02
                lda blkno               ; get the block number
CopyBlk         ldy #$00                ; set offset to zero
-               lda Rbuff,X             ; get data byte from buffer
                sta (ptr),Y             ; save to target
                inc ptr                 ; point to next address
                bne +                   ; did it step over page boundary?
                inc ptr+1               ; adjust high address for page crossing
+               inx                     ; point to next data byte
                cpx #130                ; is it the last byte
                bne -                   ; no, get the next one
                inc blkno               ; done.  Inc the block #
                lda #ACK                ; send ACK
                jsr Put_Chr
                jmp StartBlk            ; get next block

RDone           lda #ACK                ; last block, send ACK and exit.
                jsr Put_Chr
                jsr Flush               ; get leftover characters, if any
                jmp Exit_Good

;  I/O Device Specific Routines

;  Two routines are used to communicate with the I/O device.

; "Get_Chr" routine will scan the input port for a character.  It will
; return without waiting with the Carry flag CLEAR if no character is
; present or return with the Carry flag SET and the character in the "A"
; register if one was present.

; "Put_Chr" routine will write one byte to the output port.  Its alright
; if this routine waits for the port to be ready.  its assumed that the
; character was send upon return from this routine.

; Here is an example of the routines used for a standard 6551 ACIA.
; You would call the ACIA_Init prior to running the xmodem transfer
; routine.


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
