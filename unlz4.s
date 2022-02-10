;LZ4 data decompressor for Apple II
;use LZ4 legacy format:
; lz4 -l file

*=$803

;unpacker variables, no need to change these
src =   $FA
end =   $FC
dst =   $FE
count = $CE
offset = $D6

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

unpack
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
        lda src         ; src >= end ?
        cmp end
        lda src+1
        sbc end+1
        bge done        ; yes, done

copymatches             ; else copymatch phase
        jsr getsrc      ; get offset (2 bytes)
        sta offset
        jsr getsrc
        sta offset+1
        pla             ; get token
        and #$0f        ;
        jsr buildcount
        clc             ; add 4 to get matchlength
        adc #4
        tax             ;
        bcc +           ; if more than $FF
        inc count+1     ; inc count+1
+       lda src+1
        pha
        lda src
        pha             ; push src on stack
        sec
        lda dst
        sbc offset      ; src = dst - offset
        sta src
        lda dst+1
        sbc offset+1
        sta src+1
        jsr docopy      ;
        pla             ; restore src
        sta src
        pla
        sta src+1
        jmp parsetoken  ; end of block -> next token

docopy                  ; copy X + 256 * [count+1] litterals
        jsr getput      ; get value
        dex
        bne docopy
        dec count+1
        bpl docopy
        rts

; example1: 4x
; at enter, A = $04, at exit A = 4 and count MSB = 0, LSB untouched
; example2: Fx FF F3 for #litterals
; at enter, A = $0F
; count = |0F|0|, then get next A = X = $FF
; $FF + $F > 1 -> count = |0F|1|, A = $0E
; count = |0E|1|, then get next A = X = $F3
; F3+E=101, count=|0E|2| and A = $01
; # = 256 * [count+1] + A (count is useless)

buildcount              ; build count from token nibble
        ldx #0          ;
        stx count+1
        cmp #$0f        ; if nibble == $f, read next else no_f
        bne no_f
-       sta count       ; store $0F in LSB count
        jsr getsrc      ; get next byte in A
        tax
        clc             ; update count+1
        adc count       ; if A + $F > $FF
        bcc +
        inc count+1     ; if carry, incr count MSB
+       inx             ; if $FF read, tax and inx => X=0 -> continue
        beq -
no_f    rts             ; A contains nibble if < $0f, else last byte

done
        pla             ; restore stack (parsetoken pha)
        rts

getput
        jsr getsrc      ; (dst) <-  (src)

putdst                  ; store to (dst++)
        sta (dst), y
        inc dst
        bne +
        inc dst+1
+       rts

getsrc                  ; get from (src++)
        lda (src), y
        inc src
        bne +
        inc src+1
+       rts

pakoff
.binary "integer.s.lz4"
paksize = * - pakoff - 1
