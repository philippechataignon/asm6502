*           = $900
DIRECT := false

segtotal = 5
secbyseg = 560 / segtotal       ; # of sector by segment

.include "apple_enc.inc"
.enc "apple"

.include "macros.inc"

status      .macro
            jsr clrstatus
            print \1
            .endm

start       jsr init            ; init screen
            jsr home            ; clear screen
            print title

            jsr xm.ssc.init     ; init ssc
            lda #start_page     ; init xmodem send addr
            sta xm.start
            lda #end_page
            sta xm.end

            lda #19             ; col 20
            sta ch
            lda #0              ; row 0
            jsr bascalc
            print screen
setupiob
            jsr locrpl         ; locate rwts paramlist
            sty rwtsptr         ; and save rwtsptr
            sta rwtsptr+1
            status waitm        ; send magic header
            jsr kbdwait
initmain
            ; init main loop
            st_rwts rwtsptr,#0,rplbuf
            st_rwts rwtsptr,#0,rplvol       ; every volume
            lda #0
            sta trknum       ; track 0
            sta secnum       ; sector 0
            lda #segtotal       ; segment number
            sta segcnt

            ; main loop
segloop     status readm
            lda #start_page     ; init with data buffer
            sta buffer
            lda #secbyseg
            sta seccnt          ; 560 / 5 sectors
trkloop
            ldx #'R'-$C0
            jsr draw
            st_rwts rwtsptr,trknum,rpltrk
            st_rwts rwtsptr,secnum,rplsec
            st_rwts rwtsptr,buffer,rplbuf+1
            st_rwts rwtsptr,#cmdread,rplcmd

            jsr locrpl         ; locate rwts paramlist
            jsr rwts            ; do it!
            lda #0
            sta preg
            bcc +               ; no disk error
            ldx #"E"
            jmp ++
+           ldx #"."
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
            status sendm
            ldx #'S'-$C0
            jsr draw
            lda #esc            ; send ESC to prevent send
            jsr xm.ssc.putc
            jsr xm.ssc.getc     ; and wait ack
            cmp #ack
            bne error
            jsr xm.XModemSend   ; send buffer using xmodem
            dec segcnt
            beq done            ; 0, all done with segments
            jmp segloop
error       status errorm
            jmp final
done
            ldx #' '
            jsr draw
            status donem
final       jsr crout
            bit kbdstrb
            jmp dos             ; exit to dos


xm          .binclude "xmodem_send.s"

.include    "disk.inc"

            .enc "apple_inv"
title       .null "DISKSAVE"
            .enc "apple"
readm       .null "READ"
rwtsm       .null "RWTS "
donem       .null "DONE"
errorm      .null "ERROR!"
sendm       .null "SEND"
waitm       .null "INSERT DISK"
