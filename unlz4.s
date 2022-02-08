;LZ4 data decompressor for Apple II
;Peter Ferrie (peter.ferrie@gmail.com)
;assemble using ACME
;src<dst

* = $280

;unpacker variables, no need to change these
src	    =	$FA
dst	    =	$FC
count	=	$FE
delta	=	$EB

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
	bcc	+
	inc	count+1
+ 	lda	src+1
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
	bne	_L2
_L0 sta	count
	jsr	getsrc
	tax
	clc
	adc	count
	bcc	_L1
	inc	count+1
_L1	inx
	beq	_L0
_L2	rts

getput
	jsr	getsrc

putdst
	cpy	dst
	bne	+
	dec	dst+1
+	dec	dst
	sta	(dst), y
	rts

getsrc
	lda	src
	bne	+
	dec	src+1
+	dec	src
	lda	(src), y
	rts

pakoff
	;place packed data here
