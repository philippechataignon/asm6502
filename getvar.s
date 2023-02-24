varpnt  = $83
chkcom  = $debe
ptrget  = $dfe3
data    = $8000
data2   = $8100
getarypt = $f7d9
arytab = $6b
charget = $b1
lowtr = $9B

*       = $9000

        jsr chkcom
        jsr getarypt
        ldy #0
-       lda (lowtr),y
        sta data,y
        iny
        bne -
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
