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
                cmp    #$0f             ; position Z if $0f
                jsr    getLength        ; get literal length
; literals loop
literals        jsr    get_byte         ; read byte
                jsr    store_byte       ; and store
                bne    literals         ; until len
; offset
read_offset     jsr    get_byte         ; get LSB offset
                tay                     ; put (dest - offset) in src
                sec                     ;
                eor    #$ff             ; eor 1 = FE = -2 = -1 - 1
                adc    dest             ;
                sta    src              ; idem with MSB offset
                jsr    get_byte
                tax                     ; save to X
                eor    #$ff
                adc    dest+1
                sta    src+1
                tya
                bne    +
                txa
                bne    +
                rts                     ; exit if offset = 0

; calc match length from saved token
+               lda    #$ff             ; get token
token           =      *-1
                and    #$0f             ; get low nibble
                adc    #$03             ; add 4 (C set ?)
                cmp    #$13             ; equivalent to cmp $0f
                jsr    getLength
; copy matches loop
-               lda    $1234
src             =      *-2
                inc    src
                bne    +
                inc    src+1
+               jsr    store_byte
                bne    -
                beq    unlz4            ; next byte (token)

; -- store byte and decr len
store_byte      sta    $1234
dest            =      *-2
                inc    dest
                bne    +
                inc    dest+1
+               dec    lenL
                bne    +
                dec    lenH
+               rts

; -- get byte and incr source
get_byte        lda   $1234
source          =     *-2
                inc   source
			    bne   +
			    inc   source+1
+               rts

; -- calc length, store in lenHL
-               jsr    get_byte
                tay
                clc
                adc    #$00                 ; $00 replaced by LSB length
lenL            =      *-1
                bcc    +                    ; 16 bits length increment
                inc    lenH
+               iny                         ; test if $ff with beq
getLength       sta    lenL                 ; do not modify flags, uses in adc #
                beq    -                    ; Z from "cmp $0f" when initial call
                                            ; else from "iny"
                tay                         ;
                beq    +
                inc    lenH
+               rts

lenH            .byte   $00

