; LZ4 data decompressor for Apple II
; use LZ4 legacy format:
; lz4 -l file

* = $803
buffer_dest =  $6000

init
        lda #<(buffer)
        sta unlz4.src
        lda #>(buffer)
        sta unlz4.src+1
        lda #<(buffer+size(buffer))
        sta unlz4.end
        lda #>(buffer+size(buffer))
        sta unlz4.end+1
        lda #<buffer_dest
        sta unlz4.dest
        lda #>buffer_dest
        sta unlz4.dest+1

DIRECT := false
unlz4  .binclude "unlz4b.s"

.align $100
buffer .binary "test.bin.lz4"
