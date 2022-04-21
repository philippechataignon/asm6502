*           = $900
rwts        = $3D9             ; RWTS jsr (tmp = delay)
locrpl      = $3E3              ; locate RWTS paramlist jsr

rpliob = $0
rplslt = $1
rpldrv = $2
rplvol = $3
rpltrk = $4
rplsec = $5
rpldct = $6
rplbuf = $8
rplsiz = $b
rplcmd = $c
rplret = $e

cmdseek = 0
cmdread = 1
cmdwrite = 2
cmdformat= 4

preg = $48               ; monitor status register
ptr = $19

track = 1
sector = 2
adr = $1000

store       .macro
            lda \2
            ldy #\1
            sta (ptr),y     
            .endm

            jsr locrpl
            sty ptr
            sta ptr+1

            store rpltrk,#track
            store rplsec,#sector
            store rplvol,#0
            store rplbuf,<adr
            store rplbuf+1,>adr
            store rplcmd,#cmdread
                                
            jsr locrpl
            jsr rwts
            lda #0
            sta preg
            rts
