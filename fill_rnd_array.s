varpnt  = $83       ; pointer to entry buffer
chkcom  = $debe     ; wait a comma
ptrget  = $dfe3     ; find a variable
getarypt = $f7d9    ; get array pointer
arytab = $6b
charget = $b1
lowtr = $9b

ptr = $fa
count = $fc

.include "macros.inc"

*       = $9100

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
        jsr rnd.xorshift
        ldy #0
        lda rnd.rndl+1  ; store rnd MSB
        sta (ptr),y
        iny
        lda rnd.rndl    ; store rnd LSB
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

rnd     .binclude "xorshift.s"
