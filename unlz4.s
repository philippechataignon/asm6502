;LZ4 data decompressor for Apple II
;use LZ4 legacy format:
; lz4 -l file

INCLUDE :?= false

.if !INCLUDE
* = $300
.fi

;unpacker variables, no need to change these
src =   $FA
end =   $FC
dst =   $FE
count = $CE
tmp =   $19
offset= $D6

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
        jsr buildcount  ; A = LSB #, count = MSB #
        tax
        jsr docopy      ; use X LSB / count MSB
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
        bcc +           ; if more than $FF, MSB++
        inc count
+       lda src         ; push src on stack
        pha
        lda src+1
        pha
        sec             ; src = dst - offset
        lda dst
        sbc offset
        sta src
        lda dst+1
        sbc offset+1
        sta src+1
        jsr docopy      ; copy temporarily from dest - offset to dest
        pla             ; restore src
        sta src+1
        pla
        sta src
        jmp parsetoken  ; end of block -> next token

docopy  jsr getput      ; copy X + 256 * [count] litterals/
        dex             ; get value
        bne docopy
        dec count
        bpl docopy
        rts

; example1: 4x
; at enter, A = $04, at exit A = 4 and count = 0
;
; example2: Fx FF F3 for #litterals
; at enter, A = $0F
; tmp only matters to get C and increment count
; count = 0, then get next A = $FF
; $FF + $F = $10E > $100 -> count = 1, A = $0E
; count = 1, tmp = $0E then get next A = $F3
; $F3 + $E = $101, count=2 and A = $01
; # = 256 * [count] + A (X is useless)

buildcount              ; build count from token nibble
        ldx #0          ;
        stx count
        cmp #$0f        ; if nibble == $f, read next byte
        bne bc_rts      ; else exit
-       sta tmp         ; store A=LSB in tmp
        jsr getsrc      ; get next byte in A
        tax             ; then inx,bne to test ==$FF
        clc             ; tmp ~ count LSB
        adc tmp         ; if A + tmp > $FF, C is set
        bcc +           ;
        inc count       ; if carry, incr count=MSB
+       inx             ; if $FF read, X=0 -> continue
        beq -
bc_rts  rts

done    pla             ; restore stack (parsetoken pha)
        rts

getput  jsr getsrc      ; (dst++) <- (src++)

putdst  sta (dst), y    ; store to (dst++)
        inc dst
        bne +
        inc dst+1
+       rts

getsrc  lda (src), y    ; get from (src++)
        inc src
        bne +
        inc src+1
+       rts
