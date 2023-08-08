*           = $900
DIRECT := false

ptr = $ce

buffstart = $1000
buffend   = $3000

trknb = 35

.include "apple_enc.inc"
.enc "apple"

.include "macros.inc"

status      .macro
            jsr clrstatus
            prt \1
            .endm

start       jsr init            ; init screen
            jsr home            ; clear screen
            prt title
            lda #19             ; col 20
            sta ch
            lda #0              ; row 0
            jsr bascalc
            prt screen

            jsr xm.ssc.init     ; init ssc
            lda #>buffstart     ; init xmodem send addr
            sta xm.start
            lda #>buffend
            sta xm.end
setupiob
            jsr locrpl          ; locate rwts paramlist
            sty rwtsptr         ; and save rwtsptr
            sta rwtsptr+1
            status waitm        ; send magic header
            jsr kbdwait
initmain
            st_rwts rwtsptr,#0,rplvol       ; every volume
            lda #0
            sta trknum          ; track 0
            sta secnum          ; sector 0 for display
trkloop     status readm
            ldx #'R'-$C0
            jsr draw
            ; seek to cur_track
            st_rwts rwtsptr,trknum,rpltrk
            st_rwts rwtsptr,#cmdseek,rplcmd
            jsr locrpl
            jsr rwts
            lda #0
            sta preg
lect1       ldx #diskslot
            lda drvon,x
            lda drvctl1,x
            ldy #0
            sty ptr
            lda #>buffstart
            sta ptr+1
waitff      lda drvinbyt,x
            bpl waitff          ; wait until byte ready
            cmp #$ff
            bne waitff
-           lda drvinbyt,x
            bpl -
            cmp #$ff
            bne waitff
-           lda drvinbyt,x
            bpl -
            cmp #$ff
            bne +
            beq -
loop1       lda drvinbyt,x
            bpl loop1
+           sta (ptr),y
            iny
            bne loop1
            inc ptr+1
            lda ptr+1
            cmp #>buffend
            blt loop1
            lda drvon,x
            ldx #"."
            jsr draw            ; write dot
            status sendm
            ldx #'S'-$C0
            jsr draw
            lda #esc            ; send ESC to prevent send
            jsr xm.ssc.putc
            jsr xm.ssc.getc     ; and wait ack
            cmp #ack
            bne error
            jsr xm.XModemSend   ; send buffer using xmodem
            inc trknum
            lda trknum
            cmp #trknb
            bge done
            jmp trkloop

error       status errorm
            jmp final
done
            ldx #' '
            jsr draw
            status donem
final
            ldx #diskslot
            lda drvoff,x
            jsr crout
            bit kbdstrb
            jmp dos             ; exit to dos

xm          .binclude "xmodem_send.s"

.include    "disk.inc"

            .enc "apple_inv"
title       .null "DISKCOPIER"
            .enc "apple"
readm       .null "READ"
rwtsm       .null "RWTS "
donem       .null "DONE"
errorm      .null "ERROR!"
sendm       .null "SEND"
waitm       .null "INSERT DISK"
