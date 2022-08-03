*           = $803
DIRECT := false

;            other vars
data        = $1000             ; max is $3848 from LZ4_compressBound
zdata       = $4900             ; unlz buffer

segtotal = 10
secbyseg = 560 / segtotal       ; # of sector by segment

.include "apple_enc.inc"
.enc "apple"

.include "macros.inc"

status      .macro
            jsr clrstatus
            print \1
            .endm

start
            jsr init            ; init screen
            jsr home            ; clear screen
            print title
            lda #19             ; col 20
            sta ch
            lda #0              ; row 0
            jsr bascalc
            print screen
setupiob
            jsr locrpl         ; locate rwts paramlist
            sty rwtsptr         ; and save rwtsptr
            sta rwtsptr+1

getparam    status paramm
            jsr xm.XModemRecv   ; receive param in $1000
            ldx #0              ; copy end address of segments
            ldy #segtotal-1
-           lda data,x          ; in segl/segh
            sta segl,y
            lda data+1,x
            sta segh,y
            inx
            inx
            dey
            bpl -
initmain
            ;;; init main loop
            st_rwts rwtsptr,#0,rplbuf   ; buffer LSB is 0 ($4800)
            st_rwts rwtsptr,#0,rplvol   ; every volume number
            lda #0
            sta trknum          ; track 0
            sta secnum          ; sector 0
            lda #segtotal-1     ; segment number
            sta segcnt

segloop     ; main loop
            ldx #'R'-$C0
            jsr draw
            status loadm

            ldx segcnt          ; get #segment
            lda segh,x          ; get load end MSB
            sta inflate.end+1
            jsr prbyte
            lda segl,x          ; get load end LSB
            sta inflate.end
            jsr prbyte

            lda #0              ; prepare loading
            sta inflate.src
            sta inflate.dst
            lda #>data          ; store start loc MSB
            sta inflate.src+1
            lda #>zdata
            sta inflate.dst+1

            jsr xm.XModemRecv   ; get segment
            ldx #'I'-$C0

            jsr draw
            status inflatem
            jsr inflate

            status writem

            lda #>zdata         ; init with zdata buffer
            sta buffer
            lda #secbyseg
            sta seccnt          ; do 7 tracks/segment
trkloop
            ldx #'W'-$C0
            jsr draw

            st_rwts rwtsptr,trknum,rpltrk  ; track number
            st_rwts rwtsptr,secnum,rplsec  ; sector number
            st_rwts rwtsptr,buffer,rplbuf+1  ; buffer MSB
            st_rwts rwtsptr,#cmdwrite,rplcmd  ; write cmd
            jsr locrpl          ; locate rwts paramlist
            jsr rwts            ; do it!

            lda #0
            sta preg
            bcc nodiskerr
            ldx #"E"
            jmp +
nodiskerr   ldx #"."
+           jsr draw            ; write dot
            inc buffer          ; next page to write
            inc secnum
            lda secnum
            cmp #secmax         ; more than sector max ?
            blt +               ; yes, next track
            inc trknum
            lda #0              ; init sector number
            sta secnum
+           dec seccnt          ; decr sector number
            bne trkloop         ; if >= 0, next sector
            dec segcnt
            bmi done            ; 0, all done with segments
            jmp segloop
done
            status donem
            jsr crout
            rts

sscerr      status sscerrorm
            rts


xm          .binclude "xmodem_recv.s"
inflate     .binclude "unlz4.s"

.include    "disk.inc"

            .enc "apple_inv"
title       .null "DISKLOAD"
            .enc "apple"
paramm      .null "READ PARAM"
loadm       .null "RECEIVE: $1000-$"
inflatem    .null "INFLATE $4900-$80FF"
formatm     .null "FORMAT"
writem      .null "WRITE"
donem       .null "DONE"
sscerrorm   .null "SSC ERROR"
segl        .fill segtotal,?
segh        .fill segtotal,?
