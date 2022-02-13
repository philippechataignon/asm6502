*           = $9000
DIRECT := false

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
read        = $FEFD             ; read from tape

;            dos routines
;rwts        = $3D9             ; RWTS jsr (tmp = delay)
locrpl      = $3E3              ; locate RWTS paramlist jsr

;            zero page parameters
secnum      = $19               ; loop var
trknum      = $1A               ; loop var
segcnt      = $1B               ; sgement 0-9
buffer      = $1C               ; MSB of RWTS buffer
seccnt      = $1D               ; sector count 0-55
pointer     = $1E               ; pointer LSB/MSB
prtptr      = $CE               ; pointer LSB/MSB

;             monitor vars
ch          = $24               ; cursor horizontal
basl        = $28               ; line addr form bascalc L
bash        = $29               ; line addr form bascalc H
A1          = $3C               ; for read
A2          = $3E               ; for read

;            other vars
data        = $1000             ; 7 track loaded in $1000-$8000
zdata       = $4800             ; unlz buffer
slot        = $60               ; slot 6 * 16

line21      = $6D0

MULT = 5                        ; delay multiplier

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
            ldx #35             ; length of line
            lda #'-'
-           jsr cout
            dex
            bne -
            jsr crout

            ldy #<left
            lda #>left          ; print left side of grid
            jsr print

setupiob
            ;jsr locrpl         ; locate rwts paramlist
            sty pointer         ; and save pointer
            sta pointer+1

            ldy #3              ; offset in RWTS
-           lda rwts_param,y
            sta (pointer),y     ; write it to RWTS
            dey
            bpl -

;format                         ; format the diskette
;            status formatm
;            lda #4              ; read(1)/write(2) command
;            ldy #$c             ; offset in RWTS
;            sta (pointer),y     ; write it to RWTS
;
;            ;jsr locrpl         ; locate rwts paramlist
;            jsr rwts            ; do it!
;            lda #MULT
;            jsr delay
;            bcc initmain
;            jmp diskerror

getparam    status paramm
            lda #<segl
            sta a1
            lda #>segl
            sta a1+1
            lda #<segend
            sta a2
            lda #>segend
            sta a2+1
            jsr read

initmain
            ;;; init main loop
            lda #0              ; 256 bytes/sector
            ldy #$b             ; offset in RWTS
            sta (pointer),y     ; write it to RWTS
            ldy #8              ; buffer LSB offset in RWTS
            sta (pointer),y     ; write it to RWTS
            sta trknum          ; track 0
            sta secnum          ; sector 0
            lda #9              ; segment number
            sta segcnt

segloop     ; main loop
            ldx #'R'-$C0
            jsr draw
            status loadm

            ldx segcnt          ; get #segment
            lda segh,X          ; get load end MSB
            sta load8000.endload+1
            sta inflate.end+1
            jsr prbyte
            lda segl,X          ; get load end LSB
            sta load8000.endload
            sta inflate.end
            jsr prbyte

            lda #0              ; prepare loading
            sta load8000.begload
            sta inflate.src
            sta inflate.dst
            lda #>data          ; store start loc MSB
            sta load8000.begload+1
            sta inflate.src+1
            lda #>zdata
            sta inflate.dst+1

            jsr load8000
            ;lda #MULT
            ;jsr delay

            ldx #'I'-$C0
            jsr draw
            status inflatem
            jsr inflate

            ldx #slot           ; slot #6
            lda motoron,x       ; turn it on

            status writem

            lda #>data
            sta buffer
            lda #56
            sta seccnt          ; do 7 tracks/segment
trkloop
            lda trknum          ; track number
            ldy #4              ; offset in RWTS
            sta (pointer),y     ; write it to RWTS
            ldx #'W'-$C0
            jsr draw
            lda secnum          ; sector number
            ldy #5              ; offset in RWTS
            sta (pointer),y     ; write it to RWTS
            lda buffer          ; buffer MSB
            ldy #9              ; offset in RWTS
            sta (pointer),y     ; write it to RWTS
            lda #2              ; read(1)/write(2) command
            ldy #$c             ; offset in RWTS
            sta (pointer),y     ; write it to RWTS

            ;jsr locrpl          ; locate rwts paramlist
            jsr rwts            ; do it!
            bcs diskerror
            ldx #"."
            jsr draw            ; write dot
            inc secnum
            lda secnum
            cmp #$0F+1          ; more than sector F ?
            blt +               ; yes, next track
            inc trknum
            lda #0              ; init sector number
            sta secnum
            inc buffer          ; next page to write
+           dec seccnt          ; decr sector number
            bne trkloop         ; if >= 0, next sector
            dec segcnt
            bmi done            ; 0, all done with segments
            jmp segloop
done
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
            ldx #24             ; clear 24 spaces
-           sta line21,X
            dex
            bpl -
            lda #21              ; vert
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
rwts        lda #1
delay       .binclude "delay.s"
load8000    .binclude "load8000.s"
inflate     .binclude "unlz4.s"
            
            .enc "apple_inv"
title       .null "DISKLOAD"
            .enc "apple_flash"
diskerrorm  .null "DISK ERROR"
            .enc "apple"
paramm      .null "READ PARAM"
donem       .null "DONE"
loadm       .null "LOAD: $1000-$"
inflatem    .null "INFLATE $4800-$7FFF"
formatm     .null "FORMAT"
writem      .null "WRITE"
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
rwts_param  .byte  1,slot,1,0             ; table type,slot,drive,volume
segl        .fill 10,?
segh        .fill 10,?
segend      = * -1
