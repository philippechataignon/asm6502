; Print 8-16-32-bit decimal NUMber
; ---------------------------
; On entry, NUM=number to print
; FLAG = |S|P| | | |32|16|8|
; -----------------------------------------------------------------


NUM         equ $FA             ; 4 bytes
FLAG        equ $FE             ; 1 byte
SAVE        equ $EB             ; 4 bytes
PTR         equ $EB             ; 2 bytes
TEMP        equ $EF             ; 1 byte

CH          equ $24
BASL        equ $28
BUFF        equ $200
IOADR       equ $C000
KBDSTRB     equ $C010
COUT1       equ $FDF0

            org $BD00

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
            bcc .L1             ; if < '0', pass
            cmp #'9'+$80+1
            bcs .L1             ; if > '9', pass
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

            org $BE00

PRINTNUM
            lda FLAG                ; set P bit to 0
            and #%10111111
            sta FLAG
            and #%00000111          ; get 8/16/32 bits (1/2/4)
            tax
            dex
            stx TEMP                ; 0,1,3 for 8/16/32 bits

.L0         lda NUM,X               ; saves NUM in SAVE
            sta SAVE,X
            dex
            bpl .L0
            bit FLAG                ; test bit 7 = S
            bpl .L3                 ; bpl -> N = bit7 = S = 0
            ldx TEMP
            lda NUM,X               ; high order byte
            bpl .L3                 ; with bit7 = 0 -> unsigned
.L1         lda NUM,X               ; negate x and output '-'
            eor #$ff
            sta NUM,X
            dex
            bpl .L1
            inc NUM
            bne .L2
            inc NUM+1
            bne .L2
            inc NUM+2
            bne .L2
            inc NUM+3
.L2         lda #'-'+$C0            ; '-' screen code
            jsr COUT1
.L3
            ldx TEMP
            ldy NBLOOP,X            ; Offset to nb loop
            lda #%00000001
            bit FLAG
            bne PRINT8
            lda #%00000010
            bit FLAG
            bne PRINT16
            lda #%00000100
            bit FLAG
            bne PRINT32
            jmp RESTORE

PRINT8
.L1
            ldx #$FF
            sec                     ; Start with digit=-1
.L2
            lda NUM
            sbc POWER0,Y
            sta NUM                 ; Subtract current tens
            inx
            bcs .L2                 ; Loop until <0
            lda NUM
            adc POWER0,Y
            sta NUM                 ; Add current tens back in
            txa
            bne .L3                 ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc .L4                 ; if V==0 -> initial 0
.L3
            jsr PRXDIGIT            ; Print this digit
.L4
            dey
            bpl .L1                 ; Loop for next digit
            jmp RESTORE

PRINT16
.L1
            ldx #$FF
            sec                     ; Start with digit=-1
.L2
            lda NUM
            sbc POWER0,Y
            sta NUM                 ; Subtract current tens
            lda NUM+1
            sbc POWER1,Y
            sta NUM+1
            inx
            bcs .L2                 ; Loop until <0
            lda NUM
            adc POWER0,Y
            sta NUM                 ; Add current tens back in
            lda NUM+1
            adc POWER1,Y
            sta NUM+1
            txa
            bne .L3                 ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc .L4                 ; if V==0 -> initial 0
.L3
            jsr PRXDIGIT            ; Print this digit
.L4
            dey
            bpl .L1                 ; Loop for next digit
            jmp RESTORE

PRINT32
.L1
            ldx #$FF
            sec                     ; Start with digit=-1
.L2
            lda NUM
            sbc POWER0,Y
            sta NUM                 ; Subtract current tens
            lda NUM+1
            sbc POWER1,Y
            sta NUM+1
            lda NUM+2
            sbc POWER2,Y
            sta NUM+2
            lda NUM+3
            sbc POWER3,Y
            sta NUM+3
            inx
            bcs .L2                 ; Loop until <0
            lda NUM
            adc POWER0,Y
            sta NUM                 ; Add current tens back in
            lda NUM+1
            adc POWER1,Y
            sta NUM+1
            lda NUM+2
            adc POWER2,Y
            sta NUM+2
            lda NUM+3
            adc POWER3,Y
            sta NUM+3
            txa
            bne .L3                 ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc .L4                 ; if V==0 -> initial 0
.L3
            jsr PRXDIGIT            ; Print this digit
.L4
            dey
            bpl .L1                 ; Loop for next digit
RESTORE
            ldx TEMP
.L0         lda SAVE,X
            sta NUM,X
            dex
            bpl .L0
            rts

PRXDIGIT                            ; output digit in X
            txa                     ; Save A, pass digit to A
            ora #$B0                ; convert to ASCII for NUMber
            jsr COUT1               ; Print it
            lda FLAG                ; set P bit to 1
            ora #%01000000
            sta FLAG
            rts                     ; Restore A and return

NBLOOP      defb 2,4,0,9
