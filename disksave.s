*           = $9000
DIRECT := false
REAL := false

; apple vectors
dos         = $9D84
asrom       = $9D72
init        = $A54F
tapein      = $C060             ; read tape interface
motoroff    = $C088             ; Turn drive motor off
motoron     = $C089             ; Turn drive motor on
reboot      = $FAA6             ; reboot machine
tabv        = $FB5B             ; move cursor to ch,a
bascalc     = $FBC1             ; calc line addr
cleos       = $FC42             ; clear to end of screen
clear       = $FC58             ; clear screen
rdkey       = $FD0C             ; read key
crout       = $FD8E             ; CR out sub
prbyte      = $FDDA             ; print byte in hex
cout        = $FDED             ; character out sub
save        = $FECD             ; write to tape

;            dos routines
.if REAL
rwts        = $3D9             ; RWTS jsr (tmp = delay)
.fi
locrpl      = $3E3              ; locate RWTS paramlist jsr

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

;            other vars
data        = $1000             ; 7 track loaded in $1000-$8000
dataend     = $7FFF
slot        = $60               ; slot 6 * 16

line21      = $6D0

linewidth = 40
statusline = 21
secmax = 16                     ; 16 sectors by track
secbyseg = 112                  ; # of sector by segment
                                ; (560 sectors / 5 segments)
mult = 15                       ; delay multiplier


.include "apple_enc.inc"
.enc "apple"

status      .macro
            jsr clrstatus
            ldy #<\1
            lda #>\1
            jsr print
            .endm

start
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

            ldy #size(rwts_param)-1 ; copy default rwts param
-           lda rwts_param,y
            sta (rwtsptr),y     ; write it to RWTS
            dey
            bpl -

initmain
            ;;; init main loop
            lda #0
            ldy #rplbuf         ; buffer LSB is 0 ($4800)
            sta (rwtsptr),y     ; write it to RWTS
            sta trknum          ; track 0
            sta secnum          ; sector 0
            lda #4              ; segment number
            sta segcnt

segloop     ; main loop
            status readm
            ldx #slot           ; slot #6
            lda motoron,x       ; turn it on
            lda #>data          ; init with data buffer
            sta buffer
            lda #secbyseg
            sta seccnt          ; do 7 tracks/segment
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
            lda #cmdread        ; read(1)/write(2) command
            ldy #rplcmd         ; offset in RWTS
            sta (rwtsptr),y     ; write it to RWTS

.if REAL
            jsr locrpl         ; locate rwts paramlist
.else
            ldy #<rwts_iob      ; simul locrpl
            lda #>rwts_iob
.fi
            jsr rwts            ; do it!
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


            status savem
            ldx #'S'-$C0
            jsr draw
            lda #<data
            sta a1
            lda #>data
            sta a1+1
            lda #<dataend
            sta a2
            lda #>dataend
            sta a2+1
.if REAL
            jsr save
.else
            lda #MULT
            jsr delay
.fi
            dec segcnt
            bmi done            ; 0, all done with segments
            jmp segloop
done
            ldx #' '
            jsr draw
            status donem
            jsr crout
            rts

diskerror
            ldx #slot            ; slot #6
            lda motoroff,x       ; turn it off
            status diskerrorm     ; print error
            rts

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
.fi

delay       .binclude "delay.s"
load8000    .binclude "load8000.s"
inflate     .binclude "unlz4.s"

            .enc "apple_inv"
title       .null "DISKSAVE"
            .enc "apple_flash"
diskerrorm  .null "DISK ERROR"
            .enc "apple"
readm       .null "READ"
rwtsm       .null "RWTS "
donem       .null "DONE"
savem       .null "SAVE"
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
rwts_param  .byte 1,slot,1,0,0,0,0,0,0,0,0,0,0,0,0,slot,1
rwts_iob    .byte 17
