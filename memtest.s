; RAM tester
; test a range of pages
; print PASS if OK, else ERR
; example:
;
; * FA:5 C0
; * 2A0G
;
; will test $0500-$BFFF
; note : program location page can't be tested

CHR2SCR     macro
            ora #$B0        ; normal
            cmp #$BA        ; if > 9
            bcc .L1
            sbc #$39        ; substract $40 in character table
.L1                         ; = $39 + 1 (C is set)
            sta SCREEN + \1 ; pos = param
            endm

START       equ $FA
END         equ $FB
CURL        equ $FC
CURH        equ $FD
SCREEN      equ $400            ; line 0
COUT1       equ $FDF0
CR          equ $FC62
HOME        equ $FC58

            org $280
INIT
            jsr HOME
            ldy #0
            sty CURL
            lda START
            sta CURH

LOOP1
            cpy #0
            bne .NOBYTEOUT
            lda CURH
            jsr BYTEOUT
.NOBYTEOUT
            tya
            sta (CURL),y    ; and store
            iny             ; next byte
            bne LOOP1
            inc CURH        ; next page
            lda CURH
            cmp END         ; last page ?
            bcc LOOP1      ; no, continue

            lda START
            sta CURH

LOOP2
            cpy #0
            bne .NOBYTEOUT
            lda CURH
            jsr BYTEOUT
.NOBYTEOUT
            tya
            cmp (CURL),y    ; cmp y and addr
            bne ERROR
            iny             ; next byte
            bne LOOP2
            inc CURH        ; next page
            lda CURH
            cmp END         ; last page ?
            bcc LOOP2      ; no, continue

OK
            ldx #LEN_PASS
.MSG
            lda MSG_PASS-1,X
            sta SCREEN,X
            dex
            bne .MSG
            rts
ERROR
            ldx #LEN_ERR
.MSG
            lda MSG_ERR-1,X
            sta SCREEN,X
            dex
            bne .MSG
            rts

BYTEOUT
            pha             ; push A for processing high nibble
            lsr             ; >> 4
            lsr
            lsr
            lsr
CHAR1       CHR2SCR 38      ; output pos 38
            pla             ; get stacked A
            and #$0F        ; low nibble
CHAR2       CHR2SCR 39      ; output pos 39
            rts

                    ; inverse
MSG_PASS    abyte   (0b10000000 | ._),"PASS"  ; set bit 7 = 1
LEN_PASS    equ     * - MSG_PASS
                    ; inverse & blink
MSG_ERR     abyte   (0b01111111 & ._),"ERR"
LEN_ERR     equ     * - MSG_ERR
