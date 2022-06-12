adr=$1000
slot = $30
start_page = $10
end_page = $20

* = $300

	        lda #$0B	; COMMAND: NO PARITY, RTS ON,
	        sta $C08A+slot	; DTR ON, NO INTERRUPTS
	        lda #$1F    ; 19200, 8bits, no
	        sta $C08B+slot	; PSPEED
            lda #start_page
            sta mod1+2


loop0:      ldx #$0
loop:
mod1:       lda adr,x
            jsr sscput
            inx
            bne loop
            inc mod1+2
            lda mod1+2
            cmp #end_page
            blt loop0
            rts

sscput:     pha		; Push A onto the stack
-           lda $c089+slot	; Check status bits
	        and #$10    ; check bit 4 = transmit register empty
	        beq -     	; Output register is full, so loop
	        pla
     	    sta $c088+slot	; Put character
	        rts
