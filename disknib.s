*           = $900
DIRECT := false

;            other vars
data        = $1000
zdata       = $3000

temp = $CE

trktotal = 35

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
            ldx #0              ; copy end address of tracks
            ldy #0
-           lda data,x          ; in trkl/trkh
            sta trkl,y
            lda data+1,x
            sta trkh,y
            inx
            inx
            iny
            cpy #trktotal
            blt -
initmain
            ;;; init main loop
            st_rwts rwtsptr,#0,rplvol   ; every volume number
            st_rwts rwtsptr,#0,rplsec  ; sector number
            st_rwts rwtsptr,#0,rplbuf   ; buffer LSB is 0 ($4800)
            st_rwts rwtsptr,#0,rplbuf+1  ; buffer MSB
            lda #0
            sta trknum          ; track 0
            sta secnum          ; sector 0
trkloop     ; main loop
            ldx #'R'-$C0
            jsr draw
            status loadm

            ldx trknum          ; get #track
            lda trkh,x          ; get load end MSB
            sta inflate.end+1
            jsr prbyte
            lda trkl,x          ; get load end LSB
            sta inflate.end
            jsr prbyte

            lda #0              ; prepare loading
            sta inflate.src
            sta inflate.dst
            lda #>data          ; store start loc MSB
            sta inflate.src+1
            lda #>zdata
            sta inflate.dst+1

            ldx #diskslot
            lda drvon,x

            jsr xm.XModemRecv   ; get segment
            ldx #'I'-$C0

            jsr draw
            status inflatem

            ldx #diskslot
            lda drvon,x

            jsr inflate

            status writem

            lda #>zdata         ; init with zdata buffer
            sta buffer

            ldx #'W'-$C0
            jsr draw

            ; seek to track trknum
            st_rwts rwtsptr,trknum,rpltrk
            st_rwts rwtsptr,#cmdseek,rplcmd
            jsr locrpl
            jsr rwts
            lda #0
            sta preg

            jsr writenib

            ldx #"."
            jsr draw            ; write dot
            inc trknum
            lda trknum
            cmp #trktotal
            bge +               ; >= last track, end
            jmp trkloop         ; next track

+           ldx #diskslot
            lda drvoff,x
            status donem
            rts

writenib                        ; timing are very important in this routine
            ;ldy #$ff            ; write synchro
            ;lda drvoutbyt,x
            ;lda drvctl1,x
            ;lda #$ff
            ;sta drvctl2,x
            ;ora drvinbyt,x
            ;clc
            ;clc
            ;clc
            ;clc
            ;clc
            ;lda #$ff
            ;jsr wait12
-           ;pha
            ;pla
            ;sta drvoutbyt,x
            ;ora drvinbyt,x
            ;dey
            ;beq +               ; end of synchro
            ;jsr wait12
            ;lda #$ff
            ;stx temp
            ;jmp -
            ldy #0
wrwrite     lda zdata,y
ptr = * - 1
            iny
            beq wrnpage
            cmp #0              ; exit when 0 in write buffer
            beq wrexit          ; 0 can't be a valid value for nibble
            clc
            clc
            clc
+           clc
-           sta drvoutbyt,x     ; write byte on disk
            ora drvinbyt,x
            jmp wrwrite
wrnpage     inc ptr
            clc
            jmp -
wrexit      lda drvctl1,x
            lda #>zdata
            sta ptr
wait12      rts

xm          .binclude "xmodem_recv.s"
inflate     .binclude "unlz4.s"

.include    "disk.inc"

            .enc "apple_inv"
title       .null "DISKNIB"
            .enc "apple"
paramm      .null "READ PARAM"
loadm       .null "RECEIVE: $1000-$"
inflatem    .null "INFLATE $4900-$80FF"
formatm     .null "FORMAT"
writem      .null "WRITE"
donem       .null "DONE\n"
.align      $100
trkl        .fill trktotal,?
trkh        .fill trktotal,?
end         = *
