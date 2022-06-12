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

st_rwts     .macro
            lda \2
            ldy #\3
            sta (\1),y
            .endm

            jsr locrpl
            sty ptr
            sta ptr+1

            st_rwts ptr,#track,rpltrk
            st_rwts ptr,#sector,rplsec
            st_rwts ptr,#0,rplvol
            st_rwts ptr,<adr,rplbuf
            st_rwts ptr,>adr,rplbuf+1
            st_rwts ptr,#cmdread,rplcmd

            jsr locrpl
            jsr rwts
            lda #0
            sta preg
            rts
