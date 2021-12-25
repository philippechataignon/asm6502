;diskload8000.s

org	=	$280		; should be $9000
cout	=	$FDED		; character out sub
crout	=	$FD8E		; CR out sub
prbyte	=	$FDDA 		; print byte in hex
tapein	=	$C060		; read tape interface

; zero page parameters

begload	=	$FA		; begin load location LSB/MSB
endload	=	$FC		; end load location LSB/MSB
chksum	=	$FE		; checksum location
pointer	=	$EB		; LSB/MSB pointer

	org	org		
readtape:
	lda	begload		; load begin LSB location
	sta	store+1		; store it
	lda	begload+1	; load begin MSB location
	sta	store+2		; store it

	ldx	#0		; set X to 0
	lda	#1		; set A to 0

nsync:
    bit	tapein		; 4 cycles, sync bit ; first pulse
	bpl	nsync		; 2 + 1 cycles

main:
    ldy	#0		; 2 set Y to 0

psync:
    bit	tapein		;
	bmi	psync

ploop:
    iny			; 2 cycles
	bit	tapein		; 4 cycles
	bpl	ploop		; 2 +1 if branch, +1 if in another page
				; total ~9 cycles

	cpy	#$40		; 2 cycles if Y - $40 > 0 endcode (770Hz)
	bpl	endcode		; 2(3)

	cpy	#$15		; 2 cycles if Y - $15 > 0 main (2000Hz)
	bpl	main		; 2(3)

	cpy	#$07		; 2, if Y<, then clear carry, if Y>= set carry
store:
    rol	store+1,x	; 7, roll carry bit into store
	ldy	#0		; 2
	asl			; 2 A*=2
	bne	main		; 2(3)
	lda	#1		; 2
	inx			; 2 cycles
	bne	main		; 2(3)
	inc	store+2		; 6 cycles
	jmp	main		; 3 cycles
				; 34 subtotal max
				; 36 subtotal max
endcode:
	txa			; write end of file location + 1
	clc
	adc	store+1
	sta	store+1
	bcc	endcheck	; LSB didn't roll over to zero
	inc	store+2		; did roll over to zero, inc MSB
endcheck:			; check for match of expected length
	;lda	endload
	;cmp	store+1
	;bne	error
	;lda	endload+1
	;cmp	store+2
	;bne	error
	jsr	ok
sumcheck:
	jsr	crout
	lda	#<chkm
	ldy	#>chkm
	jsr	print
	lda	#0
	sta	pointer
	lda	begload+1
	sta	pointer+1
	lda	#$ff		; init checksum
	ldy	begload
sumloop:
	eor	(pointer),y
	ldx	pointer+1 
	cpx	endload+1   ;last page?
	bcs	last
	iny
	bne	sumloop
	inc	pointer+1
	jmp	sumloop
last:
	iny
    beq exit
	cpy	endload
	bcc	sumloop
    beq sumloop     ; <=
exit:
    sta chksum
	ldy	#1
	eor	(endload),y
	bne	error
	jmp	ok
error:
	lda	#<errm
	ldy	#>errm
	jsr	print
	rts
ok:
	lda	#<okm
	ldy	#>okm
print:
	sta	pointer
	sty	pointer+1
	ldy	#0
	lda	(pointer),y	; load initial char
print1:
    ora	#$80
	jsr	cout
	iny
	lda	(pointer),y
	bne	print1
	rts

chkm:	asciiz	"CHKSUM "
okm:	asciiz	"OK"
errm:	asciiz	"ERROR"
