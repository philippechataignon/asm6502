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

            .include "apple_enc.inc"

CHR2SCR     .macro
            ora #$B0        ; normal 0-9 -> B0 B9, A -> BA
            cmp #$BA        ; if > "9"
            blt +
            adc #6          ; "9"=B9 BA+C+6=C1="A"
+           sta LINE0 + \1  ; pos = param
            .endm

PRINTMSG    .macro
            ldx #size(\1)-1
-           lda \1,X
            sta LINE0,X
            dex
            bpl -
            .endm

START       = $FA
END         = $FB
CUR         = $FC
LINE0       = $400            ; line 0
COUT1       = $FDF0
CR          = $FC62
HOME        = $FC58

*           = $280

INIT        jsr HOME
            ldy #0
            sty CUR
            lda START
            sta CUR+1

STORE       cpy #0
            bne +           ;BYTEOUT if Y=0
            lda CUR+1
            jsr BYTEOUT
+           tya
            sta (CUR),y     ; and store
            iny             ; next byte
            bne STORE
            inc CUR+1       ; next page
            lda CUR+1
            cmp END         ; last page ?
            bcc STORE       ; no, continue

            lda START
            sta CUR+1
COMPARE     cpy #0          ;BYTEOUT if Y=0
            bne +
            lda CUR+1
            jsr BYTEOUT
+           tya
            cmp (CUR),y     ; cmp y and addr
            bne ERROR       ; if !=, exit with error
            iny             ; next byte
            bne COMPARE
            inc CUR+1       ; next page
            lda CUR+1
            cmp END         ; last page ?
            bcc COMPARE     ; no, continue

OK          PRINTMSG MSG_PASS
            rts
ERROR       PRINTMSG MSG_ERR
            rts

BYTEOUT     pha             ; push A for processing high nibble
            lsr             ; >> 4
            lsr
            lsr
            lsr
CHAR1       CHR2SCR 38      ; output pos 38
            pla             ; get stacked A
            and #$0F        ; low nibble
CHAR2       CHR2SCR 39      ; output pos 39
            rts

            .enc "apple"
MSG_PASS    .text   "PASS"
            .enc "apple_inv"
MSG_ERR     .text   "ERR"
