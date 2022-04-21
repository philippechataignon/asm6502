*           = $900
rwts        = $3D9             ; RWTS jsr (tmp = delay)
locrpl      = $3E3              ; locate RWTS paramlist jsr
ptr = $10

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

;             monitor vars
preg        = $48               ; monitor status register

;            other vars
diskslot    = $60               ; slot 6 * 16
esc        = $9b        ; ESCAPE KEY
ack        = $06        ; ACKNOWLEDGE
nak        = $15        ; NEGATIVE ACKNOWLEDGE

sscslot     = $30               ; slot 3
sscreg      = $C088+sscslot
sscstatus   = $C089+sscslot
ssccommand  = $C08A+sscslot
ssccontrol  = $C08B+sscslot

track = 1
sector = 2
adr = $1000

            jsr locrpl         ; locate rwts paramlist
            sty ptr         ; and save ptr
            sta ptr+1

            lda #0
            ldy #rplvol         ; every volume
            sta (ptr),y     
            lda #track          ; track number
            ldy #rpltrk
            sta (ptr),y    
            lda #sector         ; sector number
            ldy #rplsec
            sta (ptr),y   
            lda #<adr
            ldy #rplbuf         ; buffer LSB
            sta (ptr),y  
            lda #>adr           ; buffer MSB
            ldy #rplbuf+1       ; offset in RWTS
            sta (ptr),y  
            lda #cmdread        ; read command
            ldy #rplcmd         ; offset in RWTS
            sta (ptr),y 
                                
            jsr locrpl          ; locate rwts paramlist
            jsr rwts            ; do it!
            lda #0
            sta preg
            rts
