; LZ4 data decompressor for 6502
;
; http://fastcompression.blogspot.com/2011/05/lz4-explained.html

;  token    | literal length | literals  | offset  | match length
;  1 byte   | 0-n bytes      | 0-L bytes | 2 bytes | 0-n bytes

; token aaaabbbb
; aaaa = length of literals 0 = no literal, $f need more bytes
; bbbb = match length 0 = 4 $f = 19 after offset

.if DIRECT
* = $300
.fi

; -- entry

unlz4
;literal length
                jsr    get_byte         ; get token
                sta    token            ; store for future use (match length)
                lsr    a                ; get high nibble
                lsr    a
                lsr    a
                lsr    a
                beq    read_offset      ; if 0, no literals, read offset
                cmp    #$0f             ; Z set if $0f, tested in gen_length
                jsr    gen_length       ; get literal length in len(hl)
; literals loop
literals        jsr    get_byte         ; read byte
                jsr    store_byte       ; and store
                bne    literals         ; until len
                lda    src              ; if src >= end
                cmp    end
                lda    src+1
                sbc    end+1
                bcs    unlz4_exit       ; then exit
; get offset and calc source address
read_offset     jsr    get_byte         ; get LSB offset
                sec                     ; put (dst - offset) in tmp_src
                eor    #$ff             ; add -offset -1 (eor) + 1(carry)
                adc    dst              ; to dst
                sta    tmp_src          ; and store in tmp_src
                jsr    get_byte         ; idem with MSB offset
                eor    #$ff
                adc    dst+1
                sta    tmp_src+1        ; C is set if offset < dst, always

; calc match length from saved token
+               lda    #$ff             ; get token
token           =      *-1
                and    #$0f             ; get low nibble
                adc    #3               ; add 4, C is set from read_offset
                cmp    #$F+4            ; equivalent to cmp $0f before +4
                jsr    gen_length
; copy matches loop
-               lda    $1234
tmp_src         =      *-2
                inc    tmp_src
                bne    +
                inc    tmp_src+1
+               jsr    store_byte
                bne    -
                beq    unlz4            ; next byte (token)
unlz4_exit      rts

; -- store byte and decr len
store_byte      sta    $1234
dst             =      *-2
                inc    dst
                bne    +
                inc    dst+1
+               dec    lenl
                bne    +
                dec    lenh
+               rts

; -- get byte and incr src
get_byte        lda   $1234
src             =     *-2
                inc   src
			    bne   +
			    inc   src+1
+               rts

; -- calc length, store in lenHL
-               jsr    get_byte
                tay
                clc
                adc    #$00             ; $00 replaced by LSB length
lenl            =      *-1
                bcc    +                ; 16 bits length increment
                inc    lenh
+               iny                     ; test if $ff with beq
gen_length      sta    lenl             ; do not modify flags, uses in adc #
                beq    -                ; Z from "cmp $0f" when initial call
                                        ; else from "iny"
                tay
                beq    +
                inc    lenh
+               rts

lenh            .byte   $00
end             .word   0

