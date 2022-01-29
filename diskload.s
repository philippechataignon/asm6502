            ifndef ORG
ORG         =   $9000
            endif

            if ORG > 0
            org     ORG
            fi

load8000    =   $280

; apple vectors

dos         =   $9D84
asrom       =   $9D72
init        =   $A54F
tapein      =   $C060        ; read tape interface
motoroff    =   $C088        ; Turn drive motor off
motoron     =   $C089        ; Turn drive motor on
reboot      =   $FAA6        ; reboot machine
tabv        =   $FB5B        ; move cursor to ch,a
bascalc     =   $FBC1        ; calc line addr
bell        =   $FBDD        ; ding
cleos       =   $FC42        ; clear to end of screen
clear       =   $FC58        ; clear screen
rdkey       =   $FD0C        ; read key
crout       =   $FD8E        ; CR out sub
prbyte      =   $FDDA         ; print byte in hex
cout        =   $FDED        ; character out sub
warm        =   $FF69        ; back to monitor

;            dos routines

fm          =   $3D6        ; file manager entry
;rwts       =   $3D9        ; RWTS jsr
locfpl      =   $3DC        ; locate file manager paramlist jsr
locrpl      =   $3E3        ; locate RWTS paramlist jsr

;            zero page parameters

begload     =   $D0        ; begin load location LSB/MSB
endload     =   $D2        ; end load location LSB/MSB
chksum      =   $D4        ; checksum location
secnum      =   $D5        ; loop var
trknum      =   $D6        ; loop var
segcnt      =   $D7        ; loop var
buffer      =   $D8        ; MSB of RWTS buffer
trkcnt      =   $D9        ; track counter (0-6)
pointer     =   $DA        ; pointer LSB/MSB
prtptr      =   $DC        ; pointer LSB/MSB
fmptr       =   $DE        ; file manager pointer
temp        =   $E0        ; temp var

;             monitor vars

ch          =   $24        ; cursor horizontal
basl        =   $28        ; line addr form bascalc L
bash        =   $29        ; line addr form bascalc H
preg        =   $48        ; mon p reg
line3       =   $580

;            other vars

data        =   $1000         ; 7 track loaded in $1000-$8000
enddata     =   $8000
;boot1o      =    $96D0        ; tape loaded boot 1 location
;boot1       =    $3D0         ; target boot 1 location
;cmpbuf      =    $9200        ; buffer for sector check
;count       =    $900

            org    $9000

start:
            jsr clear            ; clear screen
            lda #<title          ; print title
            ldy #>title
            jsr print
                                 ; TRACK
            lda #19              ; col 20
            sta ch
            lda #0               ; row 0
            jsr tabv
            lda #<track          ; print track
            ldy #>track
            jsr print

            lda #<header         ; print header
            ldy #>header
            jsr print
            ldx #35              ; length of line
            jsr line

            lda #<left           ; print left side of grid
            ldy #>left
            jsr print

;;setupiob:
;;            jsr locrpl           ; locate rwts paramlist
;;            sty pointer          ; and save pointer
;;            sta pointer+1
;;
;;            lda #1               ; table type
;;            ldy #0               ; offset in RWTS
;;            sta (pointer),y      ; write it to RWTS
;;
;;            lda #6 * 16          ; slot 6
;;            ldy #1               ; offset in RWTS
;;            sta (pointer),y      ; write it to RWTS
;;
;;            lda #1               ; drive number
;;            ldy #2               ; offset in RWTS
;;            sta (pointer),y      ; write it to RWTS
;;
;;            lda #254             ; volume number
;;            ldy #3               ; offset in RWTS
;;            sta (pointer),y      ; write it to RWTS
;;
;;format:                          ; format the diskette
;;            lda infdata+20       ; check noformat flag
;;            bne endformat        ; if not 0 jump to endformat

            jsr status
            lda #<formatm        ; print formatting
            ldy #>formatm
            jsr print
            lda #5
            sta NDELAY
            jsr delay

                                 ;;; file manager format
;;            jsr locfpl           ; load up Y and A
;;            sty fmptr
;;            sta fmptr+1
;;
;;            lda #$0B             ; init command
;;            ldy #0
;;            sta (fmptr),y
;;
;;            lda #$9D             ; DOS location
;;            ldy #1
;;            sta (fmptr),y
;;
;;            lda #254             ; volume number
;;            ldy #4
;;            sta (fmptr),y
;;
;;            lda #$01             ; drive number
;;            ldy #5
;;            sta (fmptr),y
;;
;;            lda #$06             ; slot number
;;            ldy #6
;;            sta (fmptr),y
;;
;;            lda #$00             ; scratch area LSB
;;            ldy #$0C
;;            sta (fmptr),y
;;
;;            lda #$92             ; scratch area MSB
;;            ldy #$0D
;;            sta (fmptr),y
;;
;;            jsr fm               ; call file manager
;;
;;            ldy #$0A             ; return code
;;            lda (fmptr),y
;;            beq endformat
;;formaterror:
;;            jmp diskerror
;;endformat:
                                 ;;;begin segment loop (5)
            lda #0               ; 256 bytes/sector
            ldy #$0b             ; offset in RWTS
            sta (pointer),y      ; write it to RWTS

            lda #0               ; buffer LSB
            ldy #8               ; offset in RWTS
            sta (pointer),y      ; write it to RWTS

            lda #0
            sta trknum           ; start with track 0
            lda #5
            sta segcnt
segloop:
            lda #$0
            sta secnum


load:
            jsr draw_r
            jsr status
            lda #<loadm          ; print loading data
            ldy #>loadm
            jsr print            ; flash

            sec
            lda #5
            sbc segcnt
            asl
            asl
            tax
            stx temp

            ldx temp
            lda #$00
            sta begload
            sta endload
            lda #>data
            sta begload+1
            lda #>enddata         ; store end location MSB
            sta endload+1

            lda #5
            sta NDELAY
            jsr delay
            ;jsr load8000        ; get 7 tracks data in $1000-$8000
                                 ; turn motor on to save 1-2 sec
            ldx #$60             ; slot #6
            lda motoron,x        ; turn it on

            jsr status
                                 ;;;begin track loop (7)
            jsr status
            lda #<writem         ; print writing
            ldy #>writem
            jsr print

            lda #>data
            sta buffer
            lda #7
            sta trkcnt           ; do 7 tracks/segment
trkloop:
            lda trknum           ; track number
            ldy #4               ; offset in RWTS
            sta (pointer),y      ; write it to RWTS


                                 ;;;begin sector loop (16), backwards is faster, much faster
            lda #$F
            sta secnum
secloop:
            jsr draw_w           ; write sector from buffer to disk
            lda secnum           ; sector number
            ldy #5               ; offset in RWTS
            sta (pointer),y      ; write it to RWTS

            lda buffer           ; buffer MSB
            clc
            adc secnum
            ldy #9               ; offset in RWTS
            sta (pointer),y      ; write it to RWTS

            lda #2               ; read(1)/write(2) command
            ldy #$c              ; offset in RWTS
            sta (pointer),y      ; write it to RWTS

            ;jsr locrpl           ; locate rwts paramlist
            jsr rwts             ; do it!
            ;bcs diskerror
            jsr draw_s           ; write space
            lda #0
            sta preg             ; fix p reg so mon is happy

            dec secnum
            bpl secloop

            lda buffer           ; buffer += $10
            clc                  ; next page to write
            adc #$10
            sta buffer

            inc trknum           ; next track
            dec trkcnt           ;
            bne trkloop          ; 0, all done with 7 tracks
                                         ;;;end track loop

            dec segcnt
            beq done             ; 0, all done with 5 segments
            jmp segloop
                                         ;;;end segment loop

                                         ;;; prompt for data only load?
done:
            jsr status
            lda #<donem          ; print done
            ldy #>donem
            jsr print
            jsr bell
            jsr rdkey
            rts
;;            jmp reboot
error:
                                 ; turn motor off, just in case left on
            ldx #$60             ; slot #6
            lda motoroff,x       ; turn it off

            lda #<errorm         ; print error
            ldy #>errorm
            jsr print
;            jmp warm
            rts
diskerror:
            lda #0
            sta preg             ; fix p reg so mon is happy
            jsr status
            lda #<diskerrorm     ; print error
            ldy #>diskerrorm
            jsr print
;            jmp warm
            rts
status:
            lda #0
            sta $24              ; horiz
            lda #22              ; vert
            jsr tabv          ; move cursor to $24,a; 0 base
            jmp cleos
draw_w:                          ; print a 'W' in the grid
            ldx #'W'-$40
            jmp draw
draw_r:                          ; print a 'R' in the grid
            ldx #'R'+$80
            jmp draw
draw_s:                          ; print a ' ' in the grid
            ldx #$80+"."
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
            rts
line:
            lda #'-' | $80
loop0:
            jsr cout
            dex
            bne loop0
            jsr crout
rts:        rts
print:
            sta prtptr
            sty prtptr+1
            ldy #$0
print_loop: lda (prtptr),y
            beq rts              ; return if 0 = end of string
            jsr cout
            iny
            jmp print_loop

rwts:
            lda #1
            sta NDELAY
delay:
            include "delay.s"

title:
            abyte -$40,"DISKLOAD" ; inverse
            byte  0
diskerrorm:
            abyte +$80,"DISK "
errorm:
            abyte +$80,"ERROR"
            byte  0
donem:
            abyte +$80,"DONE. PRESS [RETURN] TO REBOOT."
            byte  0
loadm:
            abyte +$80,"LOADING DATA"
            byte  0
formatm:
            abyte +$80,"FORMATTING DISK "
            byte  0
writem:
            abyte +$80,"WRITING DATA "
            byte  0
track:
            abyte +$80,"TRACK",$0D
            byte  0
header:
            abyte +$80,"    00000000001111111111222222222233333",$0D
            abyte +$80,"    01234567890123456789012345678901234",$0D
            abyte +$80,"    "
            byte  0
left:
            abyte +$80,"  0:",$0D
            abyte +$80,"  1:",$0D
            abyte +$80,"  2:",$0D
            abyte +$80,"  3:",$0D
            abyte +$80,"  4:",$0D
            abyte +$80,"S 5:",$0D
            abyte +$80,"E 6:",$0D
            abyte +$80,"C 7:",$0D
            abyte +$80,"T 8:",$0D
            abyte +$80,"O 9:",$0D
            abyte +$80,"R A:",$0D
            abyte +$80,"  B:",$0D
            abyte +$80,"  C:",$0D
            abyte +$80,"  D:",$0D
            abyte +$80,"  E:",$0D
            abyte +$80,"  F:",$0D
            byte  0
