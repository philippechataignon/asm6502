PRBYTE = $FDDA
PTR = LOOP + 1          ; self-modifying addr

* = $803

        jmp ENTRY

START   .word 1
END     .word 2

ENTRY   ldy START
        lda START+1
        sta PTR+1
        lda #0
        sta PTR
        sta CRC

LOOP    lda $FFFF,Y

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
        inc PTR+1
        beq ++          ; if 0 after $ffff, exit
+       sty TMP
        lda END
        cmp TMP
        lda END+1
        sbc PTR+1
        bge LOOP        ; end >= cur, continue
+       lda CRC
        jsr PRBYTE
        rts

CRC     .byte ?
TMP     .byte ?
