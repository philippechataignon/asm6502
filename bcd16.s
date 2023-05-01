; Convert an 16 bit binary value to BCD
;
; This function converts a 16 bit binary value into a 24 bit BCD. It
; works by transferring one bit a time from the source and adding it
; into a BCD value that is being doubled on each iteration. As all the
; arithmetic is being done in BCD the result is a binary to decimal
; conversion. All conversions take 915 clock cycles.
;
; See BINBCD8 for more details of its operation.
;
; Andrew Jacobs, 28-Feb-2004

* = $300
        jmp BINBCD16

; 12345 -> $45 $23 $01
BIN		.word  12345
BCD		.fill  3

BINBCD16:
        sed		    ; Switch to decimal mode
		lda #0		; Ensure the result is clear
		sta BCD+0
		sta BCD+1
		sta BCD+2
		ldx #16		; The number of source bits

CNVBIT:	asl BIN 	; Shift out one bit
		rol BIN+1
		lda BCD  	; And add into result
		adc BCD
		sta BCD
		lda BCD+1	; propagating any carry
		adc BCD+1
		sta BCD+1
		lda BCD+2	; ... thru whole result
		adc BCD+2
		sta BCD+2
		dex		; And repeat for next bit
		bne CNVBIT
		cld		; Back to binary

		brk		; All Done.

; A test value to be converted
