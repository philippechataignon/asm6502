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
