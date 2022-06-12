; crc16 : 1s50 for D000-FFFF = $E792

prbyte = $fdda
ptr = loop + 1          ; auto-modifying addr

* = $803

        jmp ENTRY

start   .word 1
end     .word 2

ENTRY   ldy start
        lda start+1
        sta ptr+1
        ldx #$FF
        stx crcl
        stx crch
        inx             ; X = 0
        stx ptr
loop    lda $ffff,y
crc16   eor crch        ; A contained the data
        sta crch        ; XOR it into high byte
        lsr             ; right shift A 4 bits
        lsr             ; to make top of x^12 term
        lsr             ; ($1...)
        lsr
        tax             ; save it
        asl             ; then make top of x^5 term
        eor crcl        ; and XOR that with low byte
        sta crcl        ; and save
        txa             ; restore partial term
        eor crch        ; and update high byte
        sta crch        ; and save
        asl             ; left shift three
        asl             ; the rest of the terms
        asl             ; have feedback from x^12
        tax             ; save bottom of x^12
        asl             ; left shift two more
        asl             ; watch the carry flag
        eor crch        ; bottom of x^5 ($..2.)
        sta crch        ; save high byte
        txa             ; fetch temp value
        rol             ; bottom of x^12, middle of x^5!
        eor crcl        ; finally update low byte
        ldx crch        ; then swap high and low bytes
        sta crch
        stx crcl        ; end CRC16_F
        iny             ; next byte
        bne +
        inc ptr+1
        beq ++          ; if 0 after $ffff, exit
+       sty tmp
        lda end
        cmp tmp
        lda end+1
        sbc ptr+1
        bge loop        ; end >= cur, continue
+       lda crch
        jsr prbyte
        lda crcl
        jsr prbyte
        rts

crcl    .byte ?         ; current value of CRC
crch    .byte ?         ; not necessarily contiguous
tmp     .byte ?

