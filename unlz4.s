;LZ4 data decompressor for Apple II
;
; http://fastcompression.blogspot.com/2011/05/lz4-explained.html

;   token   | literal length | literals  | offset  | match length
;  1 byte   | 0-n bytes      | 0-L bytes | 2 bytes | 0-n bytes

; token aaaabbbb
; aaaa = length of literals 0 = no literal, $f need more bytes
; bbbb = match length 0 = 4 $f = 19 after offset

; Example
; 00000000: *b4*56 6f69 7475   7265 2075 6e0a 0b00 *45*64  .Voiture un...Ed
; 00000010: 6575   780d 00*55* 7472 6f69 730e 0070 *71*75  eux..Utrois..pqu
; 00000020: 6174   7265 0a                                 atre.

; token b4: b = 11 = length of literals
; output Voiture un@
;        01234567890
; offset 000b -> -11 before offset, match = 4 + 4 = 8 -> Voiture(space)
; output Voiture un@Voiture
;        0123456789012345678
; token 45: 4 literals = deux
; output Voiture un@Voiture deux
;        01234567890123456789012
; offset 000d -> -13 (@ position), match length = 5+4 = 9
; output Voiture un@Voiture deux@Voiture
;        01234567890123456789012345678901
; token 55
; output Voiture un@Voiture deux@Voiture trois
;        0123456789012345678901234567890123456
; offset 000e, length 5+4=9
; output Voiture un@Voiture deux@Voiture trois@Voiture
;        0123456789012345678901234567890123456789012345
; token 71
; output Voiture un@Voiture deux@Voiture trois@Voiture quatre@
;        0123456789012345678901234567890123456789012345

src = $06
dst = $08

.if DIRECT
* = $300
.fi

inflate
        ldy #0          ; Y is always 0

parsetoken
        jsr getsrc      ; get token
        pha             ; push it
        lsr             ; get # litterals
        lsr
        lsr
        lsr
        beq copymatches ; if 0, no litterals
        jsr buildcount  ; after, A = LSB #, count = MSB #
        tax
        jsr docopy      ; use X LSB / count MSB
        lda src         ; src >= end ?
        cmp end
        lda src+1
        sbc end+1
        bge lzdone      ; yes, done

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

docopy  jsr getput      ; copy X + 256 * [count] litterals
        dex             ; get value
        bne docopy
        dec count
        bpl docopy
        rts

; example1: 4x
; at enter, A = $04, at exit A = 4 and count = 0
;
; example2: Fx FF F3 for literals length
; at enter, A = $0F
; tmp only matters to get carry C and increment count
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
        tax             ; copy in X for $FF test
        clc             ; tmp ~ count LSB
        adc tmp         ; if A + tmp > $FF, C is set
        bcc +           ;
        inc count       ; if carry, incr count=MSB
+       inx             ; if $FF read, X=0 -> continue
        beq -
bc_rts  rts

lzdone  pla             ; restore stack (parsetoken pha)
        rts

; get byte from src and put to dest
getput  jsr getsrc      ; (dst++) <- (src++)

; put byte from A to dest
putdst  sta (dst), y    ; store to (dst++)
        inc dst
        bne +
        inc dst+1
+       rts

; get byte in A from src
getsrc  lda (src), y    ; get from (src++)
        inc src
        bne +
        inc src+1
+       rts

end     .word 0
offset  .word 0
count   .byte 0
tmp     .byte 0
