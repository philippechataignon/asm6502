ORG         :?=     $BD00

            .if ORG > 0
* =  ORG
            .fi

NUM         = $FA             ; 4 bytes
FLAG        = $E3             ; 1 byte
SAVE        = $EB             ; 4 bytes
PTR         = $EB             ; 2 bytes
TEMP        = $D7             ; 1 byte

CH          = $24
BASL        = $28
BUFF        = $BF00
IOADR       = $C000
KBDSTRB     = $C010
COUT1       = $FDF0

INPUTNUM
            ldx #$0
_L1         dex                 ; $FF at first iteration
_L2         bit IOADR           ; key down?
            bpl _L2             ; wait loop
            lda IOADR           ; get keycode
            bit KBDSTRB         ; clr key strobe
            cmp #$8D            ; return ?
            beq EXIT            ; yes, exit input loop
            cmp #$88            ; backspace ?
            bne _L3             ; no, continue
            cpx #$FF            ; yes, is first char ?
            beq _L2             ; yes, do nothing
            dec CH              ; no, point to previous char
            inx
            jmp _L2
_L3         cmp #'0'+$80        ; test if num else pass
            blt _L2             ; if < '0', pass
            cmp #'9'+$80+1
            bge _L2             ; if > '9', pass
            sta BUFF,X          ; else store in BUFF
            jsr COUT1
            jmp _L1
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
_L0         lda (PTR),Y         ; get char (PTR is fixed)
            and #$0F            ; keep low nibble
            tax                 ; X = index loop
            clc
_L1         beq _L2             ; if X > 0, add POWER to NUM (32 bits)
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
            jmp _L1             ; next add iteration
_L2         iny
            cpy TEMP
            bcc _L0             ; next digit/char
            rts

powers = (1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000)

POWER0      .byte powers & $ff
POWER1      .byte powers >> 8 & $ff
POWER2      .byte powers >> 16 & $ff
POWER3      .byte powers >> 24 & $ff

            .align $100

PRINTNUM
            lda FLAG                ; set P bit to 0
            and #%10111111
            sta FLAG
            and #%00000111          ; get 8/16/32 bits (1/2/4)
            tax
            dex
            stx TEMP                ; 0,1,3 for 8/16/32 bits

_L0         lda NUM,X               ; saves NUM in SAVE
            sta SAVE,X
            dex
            bpl _L0
            bit FLAG                ; test bit 7 = S
            bpl _L3                 ; bpl -> N = bit7 = S = 0
            ldx TEMP
            lda NUM,X               ; high order byte
            bpl _L3                 ; with bit7 = 0 -> unsigned
_L1         lda NUM,X               ; negate x and output '-'
            eor #$ff
            sta NUM,X
            dex
            bpl _L1
            inc NUM
            bne _L2
            inc NUM+1
            bne _L2
            inc NUM+2
            bne _L2
            inc NUM+3
_L2         lda #'-'+$C0            ; '-' screen code
            jsr COUT1
_L3
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
_L1
            ldx #$FF
            sec                     ; Start with digit=-1
_L2
            lda NUM
            sbc POWER0,Y
            sta NUM                 ; Subtract current tens
            inx
            bcs _L2                 ; Loop until <0
            lda NUM
            adc POWER0,Y
            sta NUM                 ; Add current tens back in
            txa
            bne _L3                 ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc _L4                 ; if V==0 -> initial 0
_L3
            jsr PRXDIGIT            ; Print this digit
_L4
            dey
            bpl _L1                 ; Loop for next digit
            jmp RESTORE

PRINT16
_L1
            ldx #$FF
            sec                     ; Start with digit=-1
_L2
            lda NUM
            sbc POWER0,Y
            sta NUM                 ; Subtract current tens
            lda NUM+1
            sbc POWER1,Y
            sta NUM+1
            inx
            bcs _L2                 ; Loop until <0
            lda NUM
            adc POWER0,Y
            sta NUM                 ; Add current tens back in
            lda NUM+1
            adc POWER1,Y
            sta NUM+1
            txa
            bne _L3                 ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc _L4                 ; if V==0 -> initial 0
_L3
            jsr PRXDIGIT            ; Print this digit
_L4
            dey
            bpl _L1                 ; Loop for next digit
            jmp RESTORE

PRINT32
_L1
            ldx #$FF
            sec                     ; Start with digit=-1
_L2
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
            bcs _L2                 ; Loop until <0
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
            bne _L3                 ; >0 -> print
            bit FLAG                ; bit 7 = N
            bvc _L4                 ; if V==0 -> initial 0
_L3
            jsr PRXDIGIT            ; Print this digit
_L4
            dey
            bpl _L1                 ; Loop for next digit
RESTORE
            ldx TEMP
_L0         lda SAVE,X
            sta NUM,X
            dex
            bpl _L0
            rts

PRXDIGIT                            ; output digit in X
            txa                     ; Save A, pass digit to A
            ora #$B0                ; convert to ASCII for NUMber
            jsr COUT1               ; Print it
            lda FLAG                ; set P bit to 1
            ora #%01000000
            sta FLAG
            rts                     ; Restore A and return

NBLOOP      .byte 2,4,0,9
