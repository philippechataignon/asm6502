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
            ldy #trktotal-1
-           lda data,x          ; in trkl/trkh
            sta trkl,y
            lda data+1,x
            sta trkh,y
            inx
            inx
            dey
            bpl -
initmain
            ;;; init main loop
            st_rwts rwtsptr,#0,rplvol   ; every volume number
            st_rwts rwtsptr,#0,rplsec  ; sector number
            st_rwts rwtsptr,#0,rplbuf   ; buffer LSB is 0 ($4800)
            st_rwts rwtsptr,#0,rplbuf+1  ; buffer MSB
            jsr locrpl          ; locate rwts paramlist
            jsr rwts            ; do it!
            lda #0
            sta trknum          ; track 0
            sta secnum          ; sector 0
initdrive
            ldx #diskslot       ; timing are very important in this routine
            lda drvon,x
            ldy #5
-           lda #$ff            ; wait ~ 1s
            jsr wait            ; to reach disk speed
            dey
            bne -

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

            jsr xm.XModemRecv   ; get segment
            ldx #'I'-$C0

            jsr draw
            status inflatem
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
            cmp #trktotal
            bge +               ; >= last track, end
            jmp trkloop         ; next track

+           ldx #diskslot
            lda drvoff,x
            status donem
            rts

writenib                        ; timing are very important in this routine
            ldy #$ff
            lda drvoutbyt,x
            lda drvctl1,x
            lda #$ff
            sta drvctl2,x
            ora drvinbyt,x
            clc
            clc
            clc
            clc
            clc
            lda #$ff
            jsr wait12
-           pha
            pla
            sta drvoutbyt,x
            ora drvinbyt,x
            dey
            beq +
            jsr wait12
            lda #$ff
            stx temp
            jmp -
+           lda zdata,y
            clc
            clc
            iny
            pha
            pla
            jmp +
-           lda zdata,y
ptr = * - 1
            iny
            beq wrnpage
            cmp #0              ; exit when 0 in write buffer
            beq wrexit          ; 0 can't be a valid value for nibble
            clc
            clc
            clc
+           clc
-           sta drvoutbyt,x
            ora drvinbyt,x
            jmp -
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
trkl        .fill trktotal,?
trkh        .fill trktotal,?
