;LZ4 data decompressor for Apple II
;Peter Ferrie (peter.ferrie@gmail.com)
;assemble using ACME
;src<dst

    org $280

;unpacker variables, no need to change these
src	    equ	$FA
dst	    equ	$FC
count	equ	$FE
delta	equ	$EB

unpack ;unpacker entrypoint
	ldy	#0

parsetoken
	jsr	getsrc
	pha
	lsr
	lsr
	lsr
	lsr
	beq	copymatches
	jsr	buildcount
	tax
	jsr	docopy
	lda	src
	cmp	#<(pakoff+1)
	lda	src+1
	sbc	#>(pakoff+1)
	bcc	done

copymatches
	jsr	getsrc
	sta	delta
	jsr	getsrc
	sta	delta+1
	pla
	and	#$0f
	jsr	buildcount
	clc
	adc	#4
	tax
	bcc	.L0
	inc	count+1
.L0	lda	src+1
	pha
	lda	src
	pha
	clc
	lda	dst
	adc	delta
	sta	src
	lda	dst+1
	adc	delta+1
	sta	src+1
	jsr	docopy
	pla
	sta	src
	pla
	sta	src+1
	jmp	parsetoken

done
	pla
	rts

docopy
	jsr	getput
	dex
	bne	docopy
	dec	count+1
	bne	docopy
	rts

buildcount
	ldx	#1
	stx	count+1
	cmp	#$0f
	bne	.L2
.L0 sta	count
	jsr	getsrc
	tax
	clc
	adc	count
	bcc	.L1
	inc	count+1
.L1	inx
	beq	.L0
.L2	rts

getput
	jsr	getsrc

putdst
	cpy	dst
	bne	.L0
	dec	dst+1
.L0	dec	dst
	sta	(dst), y
	rts

getsrc
	lda	src
	bne	.L0
	dec	src+1
.L0	dec	src
	lda	(src), y
	rts

pakoff
	;place packed data here
