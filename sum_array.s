varpnt  = $83
chkcom  = $debe
ptrget  = $dfe3
getarypt = $f7d9
arytab = $6b
charget = $b1
lowtr = $9b

ptr = $fa
end = $fc
sum = $fe

*       = $9000

        jsr chkcom
        jsr getarypt

; addr first value = ptr = lowtr + 7
        clc
        lda lowtr
        adc #7
        sta ptr
        lda lowtr+1
        adc #0
        sta ptr+1

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

; init sum = 0
        lda #0
        sta sum
        sta sum+1

; ptr >= end, exit
loop    lda ptr
        cmp end
        lda ptr+1
        sbc end+1
        bge exit
; add (ptr) to sum
        ldy #1
        clc
        lda (ptr),y
        adc sum
        sta sum
        dey
        lda (ptr),y
        adc sum+1
        sta sum+1
; ptr = ptr + 2
        clc
        lda ptr
        adc #2
        sta ptr
        lda ptr+1
        adc #0
        sta ptr+1
        jmp loop

; store in n%
exit    jsr chkcom
        jsr ptrget
        ldy #2
        lda sum+1
        sta (lowtr),y
        iny
        lda sum
        sta (lowtr),y
        rts
