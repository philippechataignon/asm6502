rwts        = $3D9
locrpl      = $3E3

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


.include "macros.inc"

*           = $280
            jsr locrpl
            sty ptr
            sta ptr+1

            st_rwts ptr,#0,rplvol
            st_rwts ptr,#cmdformat,rplcmd

            jsr locrpl
            jsr rwts
            lda #0
            sta preg
            rts
