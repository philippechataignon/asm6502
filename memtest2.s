; RAM tester
; test a range of pages
; print PASS if OK, else ERR
; example:
;
; * FA:4 C0
; * 2A0G
;
; will test $0400-$BFFF
; note : program location page can't be tested

START       equ $FA
END         equ $FB
CURL        equ $FC
CURH        equ $FD
COUT1       equ $FDF0
CR          equ $FC62
HOME        equ $FC58

            org $2A0
INIT
            ldy #0
            sty CURL
            lda START
            sta CURH

LOOP1
            tya
            sta     (CURL),y    ; and store
            iny                 ; next byte
            bne     LOOP1       ;
            inc     CURH        ; next page
            lda     CURH
            cmp     END         ; last page ?
            bcc     LOOP1       ; no, continue

            lda START
            sta CURH

LOOP2
            tya
            cmp     (CURL),y    ; cmp y and addr
            bne     ERROR
            iny                 ; next byte
            bne     LOOP2       ;
            inc     CURH        ; next page
            lda     CURH
            cmp     END         ; last page ?
            bcc     LOOP2       ; no, continue

OK            
            jsr     HOME
            lda     #'P'+$80
            jsr     COUT1
            lda     #'A'+$80
            jsr     COUT1
            lda     #'S'+$80
            jsr     COUT1
            lda     #'S'+$80
            jsr     COUT1
            lda     #' '+$80
            jsr     COUT1
            lda     #'@'+$80
            jsr     COUT1
            lda     START
            jsr     COUTBYTE
            lda     #$00
            jsr     COUTBYTE
            lda     #'-'+$80
            jsr     COUT1
            lda     CURH
            jsr     COUTBYTE
            lda     CURL
            jsr     COUTBYTE
            jsr     CR
            rts
ERROR            
            jsr     HOME
            lda     #'E'+$80
            jsr     COUT1
            lda     #'R'+$80
            jsr     COUT1
            lda     #'R'+$80
            jsr     COUT1
            lda     #' '+$80
            jsr     COUT1
            lda     #'@'+$80
            jsr     COUT1
            lda     CURH
            jsr     COUTBYTE
            lda     CURL
            jsr     COUTBYTE
            jsr     CR
            rts

COUT4
            and     #$0F
            ora     #$B0        ; convert to ASCII for number
            cmp     #$BA        ; > BA (3A|80) -> not number but [A-F], need to add 6
            bcc     COUTN
            adc     #$06
COUTN
            jsr     COUT1
            rts

COUTBYTE
            pha
            lsr
            lsr
            lsr
            lsr
            jsr     COUT4
            pla
            jsr     COUT4
            rts
