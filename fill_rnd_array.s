varpnt  = $83       ; pointer to entry buffer
chkcom  = $debe     ; wait a comma
ptrget  = $dfe3     ; find a variable
getarypt = $f7d9    ; get array pointer
arytab = $6b
charget = $b1
lowtr = $9b

ptr = $fa
count = $fc
fill = $fe

.include "macros.inc"

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
        jsr chkcom      ; check if ptr is on comma
        jsr getarypt    ; get array ptr

; first value = ptr = lowtr + 7
        clc 
        lda lowtr
        adc #7
        sta ptr
        lda lowtr+1
        adc #0
        sta ptr+1

; get count = (lowtr)2
        ldy #2
        lda (lowtr),y
        sta count
        iny
        lda (lowtr),y
        sta count+1

; main loop
loop:
        ldy #0
        lda fill+1  ; store fill MSB
        sta (ptr),y
        iny
        lda fill    ; store fill LSB
        sta (ptr),y
; next item, ptr = ptr + 2
        incr ptr
        incr ptr
; decrement counter
        lda count
        bne +
        lda count+1
        beq exit    ; if count == $0000, exit
        dec count+1
+       dec count
        jmp loop
exit    rts
