;LZ4 data decompressor for Apple II
;use LZ4 legacy format:
; lz4 -l file

*=$803

;unpacker variables, no need to change these
src =   $FA
end =   $FC
dst =   $FE
count = $CE
delta = $D6

DEST =  $6000

init
        lda #<(pakoff+8)        ; 8 = skip legacy lz4 header
        sta src
        lda #>(pakoff+8)
        sta src+1
        lda #<(pakoff+paksize)
        sta end
        lda #>(pakoff+paksize)
        sta end+1
        lda #<DEST
        sta dst
        lda #>DEST
        sta dst+1
unpack ;unpacker entrypoint
        ldy #0          ; Y is always 0
parsetoken
        jsr getsrc      ; get token
        pha             ; push it
        lsr             ; get # litterals
        lsr
        lsr
        lsr
        beq copymatches ; if 0, no litterals
        jsr buildcount
        tax
        jsr docopy
        lda src
        cmp end
        lda src+1
        sbc end+1
        bcs done

copymatches
        jsr getsrc
        sta delta
        jsr getsrc
        sta delta+1
        pla
        and #$0f
        jsr buildcount
        clc
        adc #4
        tax
        bcc +
        inc count+1
+       lda src+1
        pha
        lda src
        pha
        sec
        lda dst
        sbc delta
        sta src
        lda dst+1
        sbc delta+1
        sta src+1
        jsr docopy
        pla
        sta src
        pla
        sta src+1
        jmp parsetoken

done
        pla
        rts

docopy
        jsr getput
        dex
        bne docopy
        dec count+1
        bne docopy
        rts

buildcount              ; build count from token nibble
        ldx #1          ;
        stx count+1
        cmp #$0f        ; if == $f
        bne _no_f
-       sta count       ; store in LSB count
        jsr getsrc      ; get next byte
        tax
        clc
        adc count       ; and add to count
        bcc +
        inc count+1
+       inx
        beq -
_no_f   rts

getput
        jsr getsrc

putdst
        sta (dst), y
        inc dst
        bne +
        inc dst+1
+       rts

getsrc
        lda (src), y
        inc src
        bne +
        inc src+1
+       rts

pakoff
.binary "integer.s.lz4"
paksize = * - pakoff - 1

