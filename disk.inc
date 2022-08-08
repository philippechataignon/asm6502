; apple vectors
kbdstrb     = $C010
init        = $FB2F             ; init screen
kbdwait     = $FB88             ; wait keypress
bascalc     = $FBC1             ; calc line addr
home        = $FC58             ; clear screen
crout       = $FD8E             ; CR out sub
prbyte      = $FDDA             ; print byte in hex
cout        = $FDED             ; character out sub

;            dos routines
dos         = $3D0
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
rplbuf = 8                      ; (2 bytes)
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
rwtsptr     = $1E               ; ptr to rwts addr (2 bytes)

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

linewidth = 40
statusline = 21
secmax = 16                     ; 16 sectors by track
line21      = $6D0

ack        = $06
nak        = $15
esc        = $9b

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
            lda secnum          ; num line = sector + 4
            adc #4
            jsr bascalc
            clc
            lda trknum          ; num col = track + 4
            adc #4
            tay
            txa
            sta (basl),y        ; store char in screen ram
-           rts

screen      .text "TRACK\n"
            .text "    00000000000000001111111111111111222\n"
            .text "    0123456789ABCDEF0123456789ABCDEF012\n"
            .text "    -----------------------------------\n"
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
            .text "  F:\n"
            .byte 0
