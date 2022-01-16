CH          equ $24
BASL        equ $28
PTR         equ $EB             ; 2 bytes
RES         equ $FA             ; 4 bytes
BUFF        equ $200
IOADR       equ $C000
KBDSTRB     equ $C010
COUT1       equ $FDF0

            org $900

            ldx #$0
.L1         dex                 ; $FF at first iteration
.L2         bit IOADR           ; key down?
            bpl .L2             ; wait loop
            lda IOADR           ; get keycode
            bit KBDSTRB         ; clr key strobe
            cmp #$8D            ; return ?
            beq EXIT            ; yes, exit input loop
            cmp #$88            ; backspace ?
            bne .L3             ; no, continue
            cpx #$FF            ; yes, is first char ?
            beq .L2             ; yes, do nothing
            dec CH              ; no, point to previous char
            inx
            jmp .L2
.L3         cmp #'0'+$80        ; test if num else pass
            bcc .L1             ; if < '0', pass
            cmp #'9'+$80+1
            bcs .L1             ; if > '9', pass
            sta BUFF,X          ; else store in BUFF
            jsr COUT1
            jmp .L1
EXIT        inx                 ; X = unit digit ptr
            stx PTR             ; PTR -> last char
            lda #>BUFF          ; init high PTR with high BUFF
            sta PTR+1           ;
            ldy #0              ; Y = digit/char counter
            sty RES             ; init sum
            sty RES+1
            sty RES+2
            sty RES+3
.M0         lda (PTR),Y         ; get char (PTR is fixed)
            and #$0F            ; keep low nibble
            tax                 ; X = index loop
            clc
.M1         beq .M2             ; if X > 0, add POWER to RES (32 bits)
            lda RES
            adc POWER0,Y
            sta RES
            lda RES+1
            adc POWER1,Y
            sta RES+1
            lda RES+2
            adc POWER2,Y
            sta RES+2
            lda RES+3
            adc POWER3,Y
            sta RES+3
            dex
            jmp .M1             ; next add iteration
.M2         iny
            cpy #10             ; max 9 loops
            bcc .M0             ; next digit/char
            rts

POWER0      defb 1 & $ff
            defb 10 & $ff
            defb 100 & $ff
            defb 1000 & $ff
            defb 10000 & $ff
            defb 100000 & $ff
            defb 1000000 & $ff
            defb 10000000 & $ff
            defb 100000000 & $ff
            defb 1000000000 & $ff
POWER1      defb 1 >> 8 & $ff
            defb 10 >> 8 & $ff
            defb 100 >> 8 & $ff
            defb 1000 >> 8 & $ff
            defb 10000 >> 8 & $ff
            defb 100000 >> 8 & $ff
            defb 1000000 >> 8 & $ff
            defb 10000000 >> 8 & $ff
            defb 100000000 >> 8 & $ff
            defb 1000000000 >> 8 & $ff
POWER2      defb 1 >> 16 & $ff
            defb 10 >> 16 & $ff
            defb 100 >> 16 & $ff
            defb 1000 >> 16 & $ff
            defb 10000 >> 16 & $ff
            defb 100000 >> 16 & $ff
            defb 1000000 >> 16 & $ff
            defb 10000000 >> 16 & $ff
            defb 100000000 >> 16 & $ff
            defb 1000000000 >> 16 & $ff
POWER3      defb 1 >> 24 & $ff
            defb 10 >> 24 & $ff
            defb 100 >> 24 & $ff
            defb 1000 >> 24 & $ff
            defb 10000 >> 24 & $ff
            defb 100000 >> 24 & $ff
            defb 1000000 >> 24 & $ff
            defb 10000000 >> 24 & $ff
            defb 100000000 >> 24 & $ff
            defb 1000000000 >> 24 & $ff
