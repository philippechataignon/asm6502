CRC   = $FE
start = $FA
end = $FC
tmp = $19

* = $803

        ldy start
        lda #0
        sta start
        sta CRC
-       lda (start),Y

CRC8    eor CRC         ; A contained the data
        sta CRC         ; XOR it with the byte
        asl             ; current contents of A will become x^2 term
        bcc +           ; if b7 = 1
        eor #$07        ; then apply polynomial with feedback
+       eor CRC         ; apply x^1
        asl             ; C contains b7 ^ b6
        bcc +
        eor #$07
+       eor CRC         ; apply unity term
        sta CRC         ; save result

        iny             ; next byte
        bne +
        inc start+1
        beq ++          ; if 0 after $ffff, exit
+       sty tmp
        lda end
        cmp tmp
        lda end+1
        sbc start+1
        bge -           ; end >= cur, continue
+       rts
