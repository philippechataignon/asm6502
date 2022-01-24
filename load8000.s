            org $280

cout        equ $FDED           ; character out sub
prbyte      equ $FDDA           ; print byte in hex
tapein      equ $C060           ; read tape interface

; zero page parameters

begload     equ $FA             ; begin load location LSB/MSB
endload     equ $FC             ; end load location LSB/MSB
chksum      equ $FE             ; checksum location
pointer     equ $EB             ; LSB/MSB pointer

readtape:
            lda begload         ; load begin LSB location
            sta store+1         ; store it for automodified location
            sta sumloop+1       ; store it for automodified location
            lda begload+1       ; load begin MSB location
            sta store+2         ; store it for automodified location
            sta sumloop+2       ; store it for automodified location

            ldx #0              ; X is used in ROL instr at store:

nsync:
            bit tapein          ; wait for high level
            bpl nsync

next_byte:
            lda #1              ; A = bit counter
next_bit:
            ldy #0              ; Y = #cycles at low level after high

psync:
            bit tapein          ; wait loop for low level
            bmi psync

ploop:
            iny                 ; 2 cycles
            bit tapein          ; 4 cycles
            bpl ploop           ; high level found, Y gets #cycles at low level
                                ; 2 +1 if branch, +1 if in another page

                                ; total ~9 cycles

            cpy #64             ; 2 cycles if Y >= 64 (<= 770Hz) = ending detect
            bge endcode         ; 2(3)

            cpy #20             ; 2 cycles if Y >= 20 (<= 2000Hz) ignore and next bit
            bge next_bit        ; 2(3)
                                ; between 7 and 20 (2000-6000 Hz) -> 1
                                ; between 0 and 7  (6000-12000 Hz) -> 0
            cpy #7              ; 2, if Y<, then clear carry, if Y>= set carry
                                ; in next line, C will enter in current address


store:                          ; warning: automodified code in store+1/store+2
            rol >0,X            ; 7, roll carry bit into store
            ldy #0              ; 2
            asl                 ; 2 at bit 7, A = 0
            bne next_bit            ; 2(3)
            inx                 ; 2 cycles
            bne next_byte       ; 2(3)
            inc store+2         ; 6 cycles
            jmp next_byte       ; 3 cycles
                                ; 37/42 subtotal max
endcode:
            lda #$ff            ; init checksum
sumloop:                        ; warning: automodified code in sumloop+1/sumloop+2
            eor >0
            tax                 ; saves checksum in X
            inc sumloop+1       ; incr LSB
            bne nexteor
            inc sumloop+2       ; incr MSB
nexteor:
            lda sumloop+1       ; 16 bits compare sumloop+1 < endload
            cmp endload
            lda sumloop+2
            sbc endload+1
            txa                 ; restore checksum
            blt sumloop
            beq exit            ; checksum OK, exit
error:
            lda #<errm
            ldy #>errm
print:
            sta pointer
            sty pointer+1
            ldy #$FF
print1:
            iny
            lda (pointer),y
            beq exit
            ora #$80
            jsr cout
            jmp print1
exit:
            rts

errm:       asciiz    "KO"
