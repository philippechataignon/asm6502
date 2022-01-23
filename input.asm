
INPUTNUM
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
            bcc .L2             ; if < '0', pass
            cmp #'9'+$80+1
            bcs .L2             ; if > '9', pass
            sta BUFF,X          ; else store in BUFF
            jsr COUT1
            jmp .L1
EXIT        txa                 ; copy X to A to compute nb digits in Y
            eor #$FF            ; inverse all bits
            sta TEMP
            inx                 ; X = ptr to last digit (= unity)
            stx PTR             ; PTR -> last char
            lda #>BUFF          ; init high PTR with high BUFF
            sta PTR+1           ;
            ldy #0              ; Y = digit/char counter
            sty NUM             ; init sum
            sty NUM+1
            sty NUM+2
            sty NUM+3
.M0         lda (PTR),Y         ; get char (PTR is fixed)
            and #$0F            ; keep low nibble
            tax                 ; X = index loop
            clc
.M1         beq .M2             ; if X > 0, add POWER to NUM (32 bits)
            lda NUM
            adc POWER0,Y
            sta NUM
            lda NUM+1
            adc POWER1,Y
            sta NUM+1
            lda NUM+2
            adc POWER2,Y
            sta NUM+2
            lda NUM+3
            adc POWER3,Y
            sta NUM+3
            dex
            jmp .M1             ; next add iteration
.M2         iny
            cpy TEMP
            bcc .M0             ; next digit/char
            rts
