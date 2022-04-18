adr=$2000
slot = $30

* = $1000

	        lda #$0B	; COMMAND: NO PARITY, RTS ON,
	        sta $C08A+slot	; DTR ON, NO INTERRUPTS
	        lda #$1F    ; 19200, 8bits, no
	        sta $C08B+slot	; PSPEED

            ldx #$0
loop:       dex
            lda adr,X
            jsr sscput
            bne loop
            rts

sscput:     pha		; Push A onto the stack
-           lda $c089+slot	; Check status bits
            and #$50	; Mask for DSR (must ignore for Laser 128)
	        cmp #$10
	        bne -     	; Output register is full, so loop
	        pla
     	    sta $c088+slot	; Put character
	        rts
