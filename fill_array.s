varpnt  = $83
chkcom  = $debe
ptrget  = $dfe3
getarypt = $f7d9
arytab = $6b
charget = $b1
lowtr = $9b

ptr = $fa
end = $fc
fill = $fe

*       = $9100

; get n% -> fill
        jsr chkcom
        jsr ptrget
        ldy #2
        lda (lowtr),y
        sta fill+1
        iny
        lda (lowtr),y
        sta fill

; get array

        jsr chkcom
        jsr getarypt
        lda lowtr
        sta ptr
        lda lowtr+1
        sta ptr+1

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

loop
; ptr >= end, exit
        lda ptr
        cmp end
        lda ptr+1
        sbc end+1
        bge exit
; add (ptr) to sum
        ldy #0
        lda fill+1
        sta (ptr),y
        iny
        lda fill
        sta (ptr),y
; ptr = ptr + 2
        clc
        lda ptr
        adc #2
        sta ptr
        lda ptr+1
        adc #0
        sta ptr+1
        jmp loop

exit    rts
