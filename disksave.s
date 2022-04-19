*           = $9000
DIRECT := false
REAL := true

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
.if REAL
rwts        = $3D9             ; RWTS jsr (tmp = delay)
.fi
locrpl      = $3E3              ; locate RWTS paramlist jsr

;           rwts
rpliob = 0
rplslt = 1
rpldrv = 2
rplvol = 3
rpltrk = 4
rplsec = 5
rpldct = 6
rplbuf = 8
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
prtptr      = $CE               ; pointer LSB

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

sscslot     = $30               ; slot 3
sscreg      = $C088+sscslot
sscstatus   = $C089+sscslot
ssccommand  = $C08A+sscslot
ssccontrol  = $C08B+sscslot

.include "apple_enc.inc"
.enc "apple"

status      .macro
            jsr clrstatus
            ldy #<\1
            lda #>\1
            jsr print
            .endm

start
            ; init ssc

initssc     bit sscreg          ; reset ssc
            lda #$0B            ; no parity, rts on, dtr on, intr
            sta ssccommand
            lda #$1F            ; 19200, 8bits, no parity
            sta ssccontrol

            jsr clear           ; clear screen
            ldy #<title
            lda #>title         ; print title
            jsr print
                                ; TRACK
            lda #19             ; col 20
            sta ch
            lda #0              ; row 0
            jsr bascalc
            ldy #<track
            lda #>track         ; print track
            jsr print

            ldy #<header
            lda #>header        ; print header
            jsr print
            ldx #linewidth-5    ; length of line
            lda #'-'
-           jsr cout
            dex
            bne -
            jsr crout

            ldy #<left
            lda #>left          ; print left side of grid
            jsr print

setupiob
.if REAL
            jsr locrpl         ; locate rwts paramlist
.else
            ldy #<rwts_iob      ; simul locrpl
            lda #>rwts_iob
.fi
            sty rwtsptr         ; and save rwtsptr
            sta rwtsptr+1

            status waitm        ; send magic header
            lda #$19
            jsr sscput
            lda #$64
            jsr sscput
            lda #$0
            jsr sscput
            jsr sscget          ; and wait ack
            cmp #ack
            beq initmain
            jmp sscerr

initmain
            ; init main loop
            lda #0
            ldy #rplbuf         ; buffer LSB is 0
            sta (rwtsptr),y     ; write it to RWTS
            ldy #rplvol         ; every volume
            sta (rwtsptr),y     ; write it to RWTS
            sta trknum          ; track 0
            sta secnum          ; sector 0
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

            lda trknum          ; track number
            ldy #rpltrk         ; offset in RWTS
            sta (rwtsptr),y     ; write it to RWTS
            lda secnum          ; sector number
            ldy #rplsec         ; offset in RWTS
            sta (rwtsptr),y     ; write it to RWTS
            lda buffer          ; buffer MSB
            ldy #rplbuf+1       ; offset in RWTS
            sta (rwtsptr),y     ; write it to RWTS
            lda #cmdread        ; read command
            ldy #rplcmd         ; offset in RWTS
            sta (rwtsptr),y     ; write it to RWTS

.if REAL
            jsr locrpl         ; locate rwts paramlist
.else
            ldy #<rwts_iob      ; simul locrpl
            lda #>rwts_iob
.fi
            jsr rwts            ; do it!
            lda #0
            sta preg
            bcs diskerror
            ldx #"."
            jsr draw            ; write dot
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
.if REAL
            jsr send
.else
            lda #MULT
            jsr delay
.fi
            ; wait ack
            status waitm
            jsr sscget
            cmp #ack
            bne sscerr

            dec segcnt
            beq done            ; 0, all done with segments
            jmp segloop
done
            lda #ack            ; send final ack
            jsr sscput
            ldx #' '
            jsr draw
            status donem
final       jsr crout
            bit $c010
            rts

sscerr      status sscerrorm
            jmp final

diskerror   status diskerrorm
            jmp final

clrstatus
            lda #" "            ; space
            ldx #linewidth-1    ; clear line
-           sta line21,X
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
print
            sta prtptr+1         ; store A=MSB
            sty prtptr           ; store Y=LSB
            ldy #0
_L1         lda (prtptr),y       ;
            beq -                ; return if 0 = end of string
            jsr cout
            iny
            jmp _L1

sscput      pha                     ; Push A onto the stack
-           lda sscstatus            ; Check status bits
            and #%00010000          ; Test bit 4 = transmit register empty if 1
            beq -                   ; Output register is full, so loop
            pla
            sta sscreg              ; Put character
            rts

sscget      lda kbd
            cmp #esc
            beq done
            lda sscstatus          ; Check status bits
            and #%00001000          ; Test bit3 = receive register full if 1
            beq sscget              ; Input register empty, loop
            lda sscreg              ; Get character
            rts

send        lda #start_page
            sta mod1+2
            ldx #$0
loop 
mod1        lda $ffff,X
            jsr sscput
            inx
            bne loop
            inc mod1+2
            lda mod1+2
            cmp #end_page
            blt loop
            rts

.if !REAL
rwts
            tya                 ;saves Y on stack
            pha
            lda #22
            jsr bascalc
            lda #0
            sta ch
            ldy #0
-           lda (rwtsptr),Y
            jsr prbyte
            lda #" "
            jsr cout
            iny
            cpy #$D
            blt -
            clc
            pla                 ; restore Y
            tay
            lda #1
            jsr delay
            rts

delay       .binclude "delay.s"
.fi

            .enc "apple_inv"
title       .null "DISKSAVE"
            .enc "apple_flash"
diskerrorm  .null "DISK ERROR"
sscerrorm   .null "SSC ERROR"
            .enc "apple"
readm       .null "READ"
rwtsm       .null "RWTS "
donem       .null "DONE"
sendm       .null "SEND"
waitm       .null "WAIT SSC"
track       .null "TRACK\n"
header      .text "    00000000001111111111222222222233333\n"
            .text "    01234567890123456789012345678901234\n"
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
