;LZ4 data decompressor for Apple II
;use LZ4 legacy format:
; lz4 -l file

INCLUDE := 1

* = $803

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

.include "unlz4.s"

pakoff
.binary "integer.s.lz4"
paksize = * - pakoff - 1
