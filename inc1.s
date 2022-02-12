var = 1

.if DIRECT
* = $300
.fi

start1  lda #1
        sta var
        rts

param1  .word ?
param2  .byte ?
param3  .dword ?
param4  .byte ?

