;LZ4 data decompressor for Apple II
;use LZ4 legacy format:
; lz4 -l file

INCLUDE := true

* = $803

DEST =  $6000

init
        lda #<(pakoff)
        sta src
        lda #>(pakoff)
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
