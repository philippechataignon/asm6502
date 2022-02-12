
load8000    = $280

*           = $9000
; apple vectors

dos         = $9D84
asrom       = $9D72
init        = $A54F
tapein      = $C060        ; read tape interface
motoroff    = $C088        ; Turn drive motor off
motoron     = $C089        ; Turn drive motor on
reboot      = $FAA6        ; reboot machine
tabv        = $FB5B        ; move cursor to ch,a
bascalc     = $FBC1        ; calc line addr
cleos       = $FC42        ; clear to end of screen
clear       = $FC58        ; clear screen
rdkey       = $FD0C        ; read key
crout       = $FD8E        ; CR out sub
prbyte      = $FDDA        ; print byte in hex
cout        = $FDED        ; character out sub
warm        = $FF69        ; back to monitor

;            dos routines

fm          = $3D6         ; file manager entry
;rwts       = $3D9         ; RWTS jsr
locfpl      = $3DC         ; locate file manager paramlist jsr
locrpl      = $3E3         ; locate RWTS paramlist jsr

;            zero page parameters

begload     = $D0          ; begin load location LSB/MSB
endload     = $D2          ; end load location LSB/MSB
chksum      = $D4          ; checksum location
secnum      = $D5          ; loop var
trknum      = $D6          ; loop var
segcnt      = $D7          ; sgement 0-9
buffer      = $D8          ; MSB of RWTS buffer
seccnt      = $D9          ; sector count 0-55
pointer     = $DA          ; pointer LSB/MSB
prtptr      = $DC          ; pointer LSB/MSB
fmptr       = $DE          ; file manager pointer
flag        = $E1          ; 0 = format, $80 = no format

;             monitor vars

ch          = $24          ; cursor horizontal
basl        = $28          ; line addr form bascalc L
bash        = $29          ; line addr form bascalc H
preg        = $48          ; mon p reg

;            other vars

data        = $1000        ; 7 track loaded in $1000-$8000
enddata     = $8000        ;
slot        = $60          ; slot 6 * 16

line21      = $6D0

MULT = 5                        ; delay multiplier

.include "apple_enc.inc"
.enc "apple"

start:
            jsr clear           ; clear screen
            lda #>title         ; print title
            ldy #<title
            jsr print
                                ; TRACK
            lda #19             ; col 20
            sta ch
            lda #0              ; row 0
            jsr tabv
            lda #>track         ; print track
            ldy #<track
            jsr print

            lda #>header        ; print header
            ldy #<header
            jsr print
            ldx #35             ; length of line
            lda #'-'
-           jsr cout
            dex
            bne -
            jsr crout

            lda #>left          ; print left side of grid
            ldy #<left
            jsr print

setupiob:
            ;jsr locrpl         ; locate rwts paramlist
            sty pointer         ; and save pointer
            sta pointer+1

            ldy #3              ; offset in RWTS
-           lda rwts_param,y
            sta (pointer),y     ; write it to RWTS
            dey
            bpl -

format:                         ; format the diskette
            lda flag
            bmi endformat       ; if bit7=1, no format

            jsr clrstatus
            lda #>formatm       ; print formatting
            ldy #<formatm
            jsr print
            lda #4              ; read(1)/write(2) command
            ldy #$c             ; offset in RWTS
            sta (pointer),y     ; write it to RWTS

            ;jsr locrpl         ; locate rwts paramlist
            jsr rwts            ; do it!
            lda #MULT
            jsr delay
            bcc endformat
            jmp diskerror

endformat:
            lda #0              ; 256 bytes/sector
            ldy #$b             ; offset in RWTS
            sta (pointer),y     ; write it to RWTS

            ;lda #0             ; buffer LSB
            ldy #8              ; offset in RWTS
            sta (pointer),y     ; write it to RWTS

            ;lda #0
            sta trknum          ; start with track 0
            lda #10
            sta segcnt
            lda #0
            sta secnum

            ; main loop

segloop:
            ldx #'R'-$C0
            jsr draw
            jsr clrstatus
            lda #>loadm
            ldy #<loadm
            jsr print

            lda #0              ; prepare loading data.enddata
            sta begload
            sta endload
            lda #>data          ; store start loc MSB
            sta begload+1
            lda #>enddata       ; store end location MSB
            sta endload+1
            ; jsr load8000
            lda #MULT
            jsr delay

            jsr clrstatus
            lda #>inflatem
            ldy #<inflatem
            jsr print
            ; jsr inflate
            ldx #'I'-$C0
            jsr draw
            ;jsr load8000
            lda #MULT
            jsr delay

            ldx #slot           ; slot #6
            lda motoron,x       ; turn it on

            jsr clrstatus
            lda #>writem        ; print writing
            ldy #<writem
            jsr print

            lda #>data
            sta buffer
            lda #56
            sta seccnt          ; do 7 tracks/segment
trkloop:
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
            lda #0
            sta preg            ; fix p reg so mon is happy

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
            beq done            ; 0, all done with segments
            jmp segloop
done:
            jsr clrstatus
            lda #>donem         ; print done
            ldy #<donem
            jsr print
            jsr rdkey
            rts
;;          jmp reboot

diskerror:
            lda #0
            sta preg             ; fix p reg so mon is happy
            ldx #slot            ; slot #6
            lda motoroff,x       ; turn it off
            jsr clrstatus
            lda #>diskerrorm     ; print error
            ldy #<diskerrorm
            jsr print
;            jmp warm
            rts

clrstatus:
            lda #" "            ; space
            ldx #16             ; clear 16 spaces
-           sta line21,X
            dex
            bpl -
            lda #21              ; vert
            jsr bascalc          ; move cursor to status line
            lda #0
            sta ch               ; horiz
            rts
draw:
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
rts:        rts
print:
            sta prtptr+1         ; store A=MSB
            sty prtptr           ; store Y=LSB
            ldy #0
_L1         lda (prtptr),y       ;
            beq rts              ; return if 0 = end of string
            jsr cout
            iny
            jmp _L1

rwts:
            clc
            lda #1

DIRECT := false
delay:      .binclude "delay.s"

            .enc "apple_inv"
title:      .null "DISKLOAD"

diskerrorm:
            .enc "apple_flash"
            .null "DISK ERROR"

            .enc "apple"
donem:
            .null "DONE"
loadm:
            .null "LOAD"
inflatem:
            .null "INFLATE"
formatm:
            .null "FORMAT"
writem:
            .null "WRITE"
track:
            .null "TRACK\n"
header:
            .text "    00000000001111111111222222222233333\n"
            .text "    01234567890123456789012345678901234\n"
            .null "    "
left:
            .text "  0:\n"
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
