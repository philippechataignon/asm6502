*           = $9000
DIRECT := false

; apple vectors
tapein      = $C060             ; read tape interface
motoroff    = $C088             ; Turn drive motor off
motoron     = $C089             ; Turn drive motor on
kbd         = $C000
kbdstrobe   = $C010
bascalc     = $FBC1             ; calc line addr
clear       = $FC58             ; clear screen
crout       = $FD8E             ; CR out sub
prbyte      = $FDDA             ; print byte in hex
cout        = $FDED             ; character out sub
save        = $FECD             ; write to tape

;            dos routines
rwts        = $3D9              ; RWTS jsr (tmp = delay)
locrpl      = $3E3              ; locate RWTS paramlist jsr

;           rwts
rpliob = 0
rplslt = 1
rpldrv = 2
rplvol = 3
rpltrk = 4
rplsec = 5
rpldct = 6
rplbufl = 8
rplbufh = 9
rplsiz = $b
rplcmd = $c
rplret = $e

cmdseek = 0
cmdread = 1
cmdwrite = 2
cmdformat= 4

;            zero page parameters
secnum      = $19               ; sector num ($0-$f)
trknum      = $1A               ; track num (0-34)
segcnt      = $1B               ; segment 0-9
buffer      = $1C               ; MSB of RWTS buffer
seccnt      = $1D               ; sector count 0-55
rwtsptr     = $1E               ; rwtsptr LSB

;             monitor vars
ch          = $24               ; cursor horizontal
basl        = $28               ; line addr form bascalc LSB
a1          = $3c               ; for save
a2          = $3e               ; for save
preg        = $48               ; monitor status register

;            other vars
diskslot    = $60               ; slot 6 * 16
start_page  = $10
end_page    = $80

line21      = $6D0

linewidth = 40
statusline = 21
secmax = 16                     ; 16 sectors by track
segtotal = 5
secbyseg = 560 / segtotal       ; # of sector by segment
                                ; (560 sectors / 5 segments)
mult = 15                       ; delay multiplier

esc        = $9b        ; ESCAPE KEY
ack        = $06        ; ACKNOWLEDGE
nak        = $15        ; NEGATIVE ACKNOWLEDGE


.include "apple_enc.inc"
.enc "apple"

.include "macros.inc"

status      .macro
            jsr clrstatus
            print \1
            .endm

start       jsr xm.ssc.init     ; init ssc
            jsr clear           ; clear screen
            print title

            lda #start_page     ; init xmodem send addr
            sta xm.start
            lda #end_page
            sta xm.end
                                ; TRACK
            lda #19             ; col 20
            sta ch
            lda #0              ; row 0
            jsr bascalc
            print track

            print header
            ldx #linewidth-5    ; length of line
            lda #'-'
-           jsr cout
            dex
            bne -
            jsr crout

            print left

setupiob
            jsr locrpl         ; locate rwts paramlist
            sty rwtsptr         ; and save rwtsptr
            sta rwtsptr+1

            status waitm        ; send magic header
initmain
            ; init main loop
            st_rwts rwtsptr,#0,rplbufl
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
            st_rwts rwtsptr,buffer,rplbufh
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
done
            ldx #' '
            jsr draw
            status donem
            jmp final
error       status errorm
final       jsr crout
            bit $c010
            rts

clrstatus
            lda #" "            ; space
            ldx #linewidth-1    ; clear line
-           sta line21,x
            dex
            bpl -
            lda #statusline      ; vert
            jsr bascalc          ; move cursor to status line
            lda #0
            sta ch               ; horiz
            rts
draw
            clc
            lda #4
            adc secnum          ; num line
            jsr bascalc
            ldy trknum
            iny                 ; add 4 to get col
            iny
            iny
            iny
            txa
            sta (basl),y        ; store char in screen ram
-           rts

xm          .binclude "xmodem_send.s"

            .enc "apple_inv"
title       .null "DISKSAVE"
            .enc "apple"
readm       .null "READ"
rwtsm       .null "RWTS "
donem       .null "DONE"
errorm      .null "ERROR!"
sendm       .null "SEND"
waitm       .null "WAIT SSC"
track       .null "TRACK\n"
header      .text "    00000000000000001111111111111111222\n"
            .text "    0123456789ABCDEF0123456789ABCDEF012\n"
            .null "    "
left        .text "  0:\n"
            .text "  1:\n"
            .text "  2:\n"
            .text "  3:\n"
            .text "  4:\n"
            .text "S 5:\n"
            .text "E 6:\n"
            .text "C 7:\n"
            .text "T 8:\n"
            .text "O 9:\n"
            .text "R A:\n"
            .text "  B:\n"
            .text "  C:\n"
            .text "  D:\n"
            .text "  E:\n"
            .null "  F:\n"
rwts_iob    .byte 17
