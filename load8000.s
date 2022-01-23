            org $260

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
            lda begload+1       ; load begin MSB location
            sta store+2         ; store it for automodified location

            ldx #0              ; X is used in ROL instr at store:

nsync:
            bit tapein          ; wait for high level
            bpl nsync

main0:
            lda #1              ; A = bit counter
main:
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

            cpy #64             ; 2 cycles if Y >= 64 endcode (770Hz)
            bge endcode         ; 2(3)

            cpy #20             ; 2 cycles if Y >= 20 0 main (2000Hz)
            bge main            ; 2(3)

            cpy #7              ; 2, if Y<, then clear carry, if Y>= set carry
                                ; in next line, C will enter in current address


store:                          ; warning: automodified code in store+1/store+2
            rol >0,X            ; 7, roll carry bit into store
            ldy #0              ; 2
            asl                 ; 2 at bit 7, A = 0
            bne main            ; 2(3)
            inx                 ; 2 cycles
            bne main0           ; 2(3)
            inc store+2         ; 6 cycles
            jmp main0           ; 3 cycles
                                ; 37/42 subtotal max
endcode:
            txa                 ; write end of file location + 1
            clc
            adc store+1
            sta store+1
            bcc endcheck        ; LSB didn't roll over to zero
            inc store+2         ; did roll over to zero, inc MSB
endcheck:                       ; checksum control
            lda #0
            sta pointer
            lda begload+1
            sta pointer+1
            lda #$ff            ; init checksum
            ldy begload
sumloop:
            eor (pointer),y
            ldx pointer+1
            cpx endload+1       ; last page?
            bcs last
            iny
            bne sumloop
            inc pointer+1
            jmp sumloop
last:
            iny
            beq exit
            cpy endload
            bcc sumloop         ; <
            beq sumloop         ; =
exit:
            sta chksum
            ldy #1
            eor (endload),y
            bne error
            jmp ok
error:
            lda #<errm
            ldy #>errm
            jmp print
ok:
            lda #<okm
            ldy #>okm
print:
            sta pointer
            sty pointer+1
            ldy #$FF
print1:
            iny
            lda (pointer),y
            beq exitprint
            ora #$80
            jsr cout
            jmp print1
exitprint:
            rts

okm:        asciiz    "OK"
errm:       asciiz    "KO"
