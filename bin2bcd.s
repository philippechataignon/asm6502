NUM = 131
FORMAT_BUF = $4000

* = $2000

        ;123456789045 = $1c $be $99 $1a $35

        lda #$35
        sta NUM
        lda #$1a
        sta NUM+1
        lda #$99
        sta NUM+2
        lda #$be
        sta NUM+3
        lda #$1c
        sta NUM+4
        lda #5
        jsr BinToBcd
        ;a = 5  buf = $45 $90 $78 $56 $34 $12

        lda #99
        sta NUM
        lda #1
        jsr BinToBcd
        ;a = 1 buf = $99
        rts

BinToBcd
;Convert binary number to BCD.
;Arbitrary number sizes are supported.
;In:
;   NUM    buffer with number to convert
;   a      number of bytes in the number
;Out:
;   FORMAT_BUF   output buffer
;  a            number of BCD bytes (at least 1)
;Uses:
;   x,y, t1,t1_h, t2


bcd_size     = 128
num_size     = 129
b           = 130

        ldx #0         ; initial result is 0, 1 byte size
        stx FORMAT_BUF
        inx
        stx bcd_size

    ;---Skip leading zeroes in the number (this may be removed, it we need the routine smaller)
        tay
        iny
skip   dey
        beq done         ;the number is zero, we are done
        lda NUM-1,y
        beq   skip

        sty num_size
        sed

    ;--- Process one byte at a time
next_byte
        ldy num_size
        lda NUM-1,y
        sta b
        sec            ;set top bit of the mask to 1
        bcs loop

shift_byte
    ;--- BCD = BCD * 2 + CARRY
        ldy #1
        ldx bcd_size
mul2
        lda FORMAT_BUF-1,y
        adc   FORMAT_BUF-1,y
        sta FORMAT_BUF-1,y
        iny
        dex
        bne mul2

        bcc loop

    ;--- BCD must be one byte bigger (we need to store our extra 1 in CARRY there)
        lda #1
        sta FORMAT_BUF-1,y
        sty bcd_size         ;as the x is 1 based, we can directly store is as new bcd_size
                                ;we cound use INC here, it would be slower, however.
        clc
loop   rol b               ;Divide by two, if result is 0, end. As we initially set the 0 bit to 1, this is in fact loop to 8.
        bne shift_byte

    ;--- Repeat for all bytes in the number
        dec num_size
        bne next_byte

        cld
done
        lda bcd_size
        rts
