varpnt  = $83       ; pointer to entry buffer
chkcom  = $debe     ; wait a comma
ptrget  = $dfe3     ; find a variable
getarypt = $f7d9    ; get array pointer
arytab = $6b
charget = $b1
lowtr = $9b

ptr = $fa
end = $fc
fill = $fe

*       = $9100

; get n% -> fill
        jsr chkcom      ; get comma
        jsr ptrget      ; get addr var in lowtr
        ldy #2          ; value in addr + 2
        lda (lowtr),y   ; MSB
        sta fill+1
        iny             ; addr +3
        lda (lowtr),y   ; LSB
        sta fill        ; store in fill

; get array
        jsr chkcom      ; get comma
        jsr getarypt    ; get array ptr

; first value = ptr = lowtr + 7
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

; ptr >= end, exit
loop    lda ptr
        cmp end
        lda ptr+1
        sbc end+1
        bge exit    ; if ptr == end, exit
        ldy #0
        lda fill+1  ; store fill MSB
        sta (ptr),y
        iny
        lda fill    ; store fill LSB
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
