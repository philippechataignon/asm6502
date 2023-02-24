varpnt  = $83
chkcom  = $debe
ptrget  = $dfe3
getarypt = $f7d9
arytab = $6b
charget = $b1
lowtr = $9b

start = $fa
end = $fc

*       = $9000

        jsr chkcom
        jsr getarypt

        lda lowtr
        sta start
        lda lowtr+1
        sta start+1

; addr first value = start = lowtr + 6
        clc
        lda lowtr
        adc #6
        sta start
        lda lowtr+1
        adc #0
        sta start+1

; end = lowtr + (lowtr)2
        ldy #2
        clc
        lda (lowtr),y
        adc lowtr
        sta end
        iny
        lda (lowtr),y
        adc lowtr+1
        sta end+1

        jsr chkcom
        jsr ptrget
        lda #>12345
        ldx #<12345
        ldy #2
        sta (lowtr),y
        iny
        txa
        sta (lowtr),y
        rts
