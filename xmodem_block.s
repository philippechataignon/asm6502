; XMODEM/CRC Sender/Receiver for the 65C02

; By Daryl Rictor Aug 2002

; A simple file transfer program to allow transfers between the SBC and a
; console device utilizing the x-modem/CRC transfer protocol.  Requires
; ~1200 bytes of either RAM or ROM, 132 bytes of RAM for the receive buffer,
; and 12 bytes of zero page RAM for variable storage.

;**************************************************************************
; This implementation of XMODEM/CRC does NOT conform strictly to the
; XMODEM protocol standard in that it (1) does not accurately time character
; reception or (2) fall back to the Checksum mode.

; (1) For timing, it uses a crude timing loop to provide approximate
; delays.  These have been calibrated against a 1MHz CPU clock.  I have
; found that CPU clock speed of up to 5MHz also work but may not in
; every case.  Windows HyperTerminal worked quite well at both speeds!

; (2) Most modern terminal programs support XMODEM/CRC which can detect a
; wider range of transmission errors so the fallback to the simple checksum
; calculation was not implemented to save space.
;**************************************************************************

; Files transferred via XMODEM-CRC will have the load address contained in
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

crc     = $19                ; CRC lo byte  (two byte variable)
crch    = $1a                ; CRC hi byte

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

.include "apple_enc.inc"

; macros

print           .macro
                ldy #<\1
                lda #>\1
                jsr printstr
                .endm

; Xmodem/CRC transfer routines
; By Daryl Rictor, August 8, 2002

; v1.0  released on Aug 8, 2002.

*       =  $900                         ; Start of program

; Enter this routine with the beginning address stored in the zero page address
; pointed to by ptr & ptrh and the ending address stored in the zero page address
; pointed to by eofp & eofph.

.enc "none"

                jmp XmodemRcv           ; quick jmp table

;;; Send

XModemSend      jsr ACIA_Init
-               jsr GetByte             ; flush the port
                bcs -                   ; if chr recvd, wait for another
                print SendMsg
                lda #0
                sta errcnt              ; error counter set to 0
                sta lastblk             ; set flag to false
                sta blkno               ; set block # to 1
                ldy #0                  ; init data block offset to 0
-               lda #$ff                ; 3 seconds
                sta retry2
                jsr GetByte
                bcc -                   ; wait for something to come in...
                cmp #"C"                ; is it the "C" to start a CRC xfer?
                bne PrtAbort            ; not "C", print abort msg and exit

LdBuffer        ;lda lastblk             ; Was the last block sent?
                ;beq +                   ; no, send the next one
                ;jmp Exit_Good           ; yes, we're done
+               ldx #$02                ; init pointers
                ldy #$00
                inc blkno               ; inc block counter
                lda blkno               ;
                sta Rbuff               ; save in 1st byte of buffer
                eor #$FF                ;
                sta Rbuff+1             ; save 1's comp of blkno next

LdBuff          lda (ptr),Y             ; save 128 bytes of data
                sta Rbuff,X
                sec
                lda eofp
                sbc ptr                 ; Are we at the last address?
                bne +                   ; no, inc pointer and continue
                lda eofph
                sbc ptrh
                bne +                   ; No last byte, continue
                inc lastBlk             ; Yes, Set last byte flag
-               inx
                cpx #130                ; Are we at the end of the 128 byte block?
                beq SCalcCRC            ; Yes, calc CRC
                lda #$00                ; Fill rest of 128 bytes with $00
                sta Rbuff,X
                beq -                   ; Branch always
+               inc ptr                 ; Inc address pointer
                bne +
                inc ptrh
+               inx
                cpx #130                ; last byte in block?
                bne LdBuff              ; no, get the next

SCalcCRC        jsr CalcCRC
                lda crch                ; save Hi byte of CRC to buffer
                sta Rbuff,Y
                iny
                lda crc                 ; save lo byte of CRC to buffer
                sta Rbuff,Y
Resend          ldx #0
                lda #SOH
                jsr Put_chr             ; send SOH = start of header
SendBlk         lda Rbuff,X             ; Send 132 bytes in buffer to the console
                jsr Put_chr
                inx
                cpx #132                ; last byte?
                bne SendBlk             ; no, get next
                lda #$FF                ; yes, set 3 second delay
                sta retry2              ; and
                jsr GetByte             ; Wait for Ack/Nack
                bcc Seterror            ; No chr received after 3 seconds, resend
                cmp #ACK                ; Chr received... is it:
                beq LdBuffer            ; ACK, send next block
                cmp #NAK                ;
                beq Seterror            ; NAK, inc errors and resend
                cmp #ESC
                beq PrtAbort            ; Esc pressed to abort
                                        ; fall through to error counter
Seterror        inc errcnt              ; Inc error counter
                lda errcnt              ;
                cmp #10                 ; are there 10 errors? (Xmodem spec for failure)
                bne Resend              ; no, resend block
PrtAbort        jsr Flush               ; yes, too many errors, flush buffer,
                jmp Exit_Err            ; print error msg and exit

;;; Receive

XModemRcv
                jsr ACIA_Init
                jsr Flush
                print RecvMsg
                lda #1
                sta blkno               ; set block # to 1
StartCrc        lda #"C"                ; "C" start with CRC mode
                jsr Put_Chr             ; send it
                lda #$FF
                sta retry2              ; set loop counter for ~3 sec delay
                lda #$00
                sta crc
                sta crch                ; init CRC value
                jsr GetByte             ; wait for input
                bcs GotByte             ; byte received, process it
                bcc StartCrc            ; resend "C"

StartBlk        lda #$FF                ;
                sta retry2              ; set loop counter for ~3 sec delay
                jsr GetByte             ; get first byte of block
                bcc StartBlk            ; timed out, keep waiting...
GotByte         cmp #SOH                ; start of block?
                beq +                   ; yes
                cmp #EOT
                bne BadCrc              ; Not SOH or EOT, so flush buffer & send NAK
                jmp RDone               ; EOT - all done!
+               ldx #$00
GetBlk          lda #$ff                ; 3 sec window to receive characters
                sta retry2
                jsr GetByte             ; get next character
                bcc BadCrc              ; chr rcv error, flush and send NAK
                sta Rbuff,X             ; good char, save it in the rcv buffer
                inx                     ; inc buffer pointer
                cpx #132                ; <01> <FE> <128 bytes> <CRCH> <CRCL>
                bne GetBlk              ; get 132 characters
                ldx #$00
                lda Rbuff,X             ; get block # from buffer
                cmp blkno               ; compare to expected block #
                beq +                   ; matched!
                jsr Exit_Err            ; Unexpected block number - abort
                jsr Flush               ; mismatched - flush buffer and then do BRK
                lda #-3                 ; put error code in "A" if desired
                brk                     ; unexpected block # - fatal error - BRK or RTS
+               eor #$ff                ; 1's comp of block #
                inx
                cmp Rbuff,X             ; compare with expected 1's comp of block #
                beq +                   ; matched!
                jsr Exit_Err            ; Unexpected block number - abort
                jsr Flush               ; mismatched - flush buffer and then do BRK
;               lda #$-4                ; put error code in "A" if desired
                brk                     ; bad 1's comp of block#
+               jsr CalcCRC             ; calc CRC
                lda Rbuff,Y             ; get hi CRC from buffer
                cmp crch                ; compare to calculated hi CRC
                bne BadCrc              ; bad crc, send NAK
                iny
                lda Rbuff,Y             ; get lo CRC from buffer
                cmp crc                 ; compare to calculated lo CRC
                beq GoodCrc             ; good CRC
BadCrc          jsr Flush               ; flush the input port
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

slot = 3
ACIA_Data    = $c088 + slot * $10
ACIA_Status  = $c089 + slot * $10
ACIA_Command = $c08a + slot * $10
ACIA_Control = $c08b + slot * $10

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


Exit_Err       print ErrMsg
                rts

Exit_Good      
-               lda #EOT
                jsr Put_Chr
                print GoodMsg
                rts
                .enc "apple"
GoodMsg         .null "TRANSFER SUCCESSFUL!"
ErrMsg          .null "TRANSFER ERROR!"
SendMsg         .null "SEND"
RecvMsg         .null "RECV"
                .enc "none"

;  CRC subroutines
CalcCRC         lda #$00             ; yes, calculate the CRC for the 128 bytes
                sta crc
                sta crch
                ldy #$02
CalcCRC1        lda Rbuff,Y
                eor crc+1            ; Quick CRC computation with lookup tables
                tax                  ; updates the two bytes at crc & crc+1
                lda crc              ; with the byte send in the "A" register
                eor crchi,X
                sta crc+1
                lda crclo,X
                sta crc
                iny
                cpy #130             ; done yet?
                bne CalcCRC1         ; no, get next
                rts                  ; y=82 on exit

; low byte CRC lookup table (should be page aligned)
.align $100
crclo
 .byte $00,$21,$42,$63,$84,$A5,$C6,$E7,$08,$29,$4A,$6B,$8C,$AD,$CE,$EF
 .byte $31,$10,$73,$52,$B5,$94,$F7,$D6,$39,$18,$7B,$5A,$BD,$9C,$FF,$DE
 .byte $62,$43,$20,$01,$E6,$C7,$A4,$85,$6A,$4B,$28,$09,$EE,$CF,$AC,$8D
 .byte $53,$72,$11,$30,$D7,$F6,$95,$B4,$5B,$7A,$19,$38,$DF,$FE,$9D,$BC
 .byte $C4,$E5,$86,$A7,$40,$61,$02,$23,$CC,$ED,$8E,$AF,$48,$69,$0A,$2B
 .byte $F5,$D4,$B7,$96,$71,$50,$33,$12,$FD,$DC,$BF,$9E,$79,$58,$3B,$1A
 .byte $A6,$87,$E4,$C5,$22,$03,$60,$41,$AE,$8F,$EC,$CD,$2A,$0B,$68,$49
 .byte $97,$B6,$D5,$F4,$13,$32,$51,$70,$9F,$BE,$DD,$FC,$1B,$3A,$59,$78
 .byte $88,$A9,$CA,$EB,$0C,$2D,$4E,$6F,$80,$A1,$C2,$E3,$04,$25,$46,$67
 .byte $B9,$98,$FB,$DA,$3D,$1C,$7F,$5E,$B1,$90,$F3,$D2,$35,$14,$77,$56
 .byte $EA,$CB,$A8,$89,$6E,$4F,$2C,$0D,$E2,$C3,$A0,$81,$66,$47,$24,$05
 .byte $DB,$FA,$99,$B8,$5F,$7E,$1D,$3C,$D3,$F2,$91,$B0,$57,$76,$15,$34
 .byte $4C,$6D,$0E,$2F,$C8,$E9,$8A,$AB,$44,$65,$06,$27,$C0,$E1,$82,$A3
 .byte $7D,$5C,$3F,$1E,$F9,$D8,$BB,$9A,$75,$54,$37,$16,$F1,$D0,$B3,$92
 .byte $2E,$0F,$6C,$4D,$AA,$8B,$E8,$C9,$26,$07,$64,$45,$A2,$83,$E0,$C1
 .byte $1F,$3E,$5D,$7C,$9B,$BA,$D9,$F8,$17,$36,$55,$74,$93,$B2,$D1,$F0

; hi byte CRC lookup table (should be page aligned)
.align $100
crchi
 .byte $00,$10,$20,$30,$40,$50,$60,$70,$81,$91,$A1,$B1,$C1,$D1,$E1,$F1
 .byte $12,$02,$32,$22,$52,$42,$72,$62,$93,$83,$B3,$A3,$D3,$C3,$F3,$E3
 .byte $24,$34,$04,$14,$64,$74,$44,$54,$A5,$B5,$85,$95,$E5,$F5,$C5,$D5
 .byte $36,$26,$16,$06,$76,$66,$56,$46,$B7,$A7,$97,$87,$F7,$E7,$D7,$C7
 .byte $48,$58,$68,$78,$08,$18,$28,$38,$C9,$D9,$E9,$F9,$89,$99,$A9,$B9
 .byte $5A,$4A,$7A,$6A,$1A,$0A,$3A,$2A,$DB,$CB,$FB,$EB,$9B,$8B,$BB,$AB
 .byte $6C,$7C,$4C,$5C,$2C,$3C,$0C,$1C,$ED,$FD,$CD,$DD,$AD,$BD,$8D,$9D
 .byte $7E,$6E,$5E,$4E,$3E,$2E,$1E,$0E,$FF,$EF,$DF,$CF,$BF,$AF,$9F,$8F
 .byte $91,$81,$B1,$A1,$D1,$C1,$F1,$E1,$10,$00,$30,$20,$50,$40,$70,$60
 .byte $83,$93,$A3,$B3,$C3,$D3,$E3,$F3,$02,$12,$22,$32,$42,$52,$62,$72
 .byte $B5,$A5,$95,$85,$F5,$E5,$D5,$C5,$34,$24,$14,$04,$74,$64,$54,$44
 .byte $A7,$B7,$87,$97,$E7,$F7,$C7,$D7,$26,$36,$06,$16,$66,$76,$46,$56
 .byte $D9,$C9,$F9,$E9,$99,$89,$B9,$A9,$58,$48,$78,$68,$18,$08,$38,$28
 .byte $CB,$DB,$EB,$FB,$8B,$9B,$AB,$BB,$4A,$5A,$6A,$7A,$0A,$1A,$2A,$3A
 .byte $FD,$ED,$DD,$CD,$BD,$AD,$9D,$8D,$7C,$6C,$5C,$4C,$3C,$2C,$1C,$0C
 .byte $EF,$FF,$CF,$DF,$AF,$BF,$8F,$9F,$6E,$7E,$4E,$5E,$2E,$3E,$0E,$1E
