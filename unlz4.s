;LZ4 data decompressor for Apple II
;Peter Ferrie (peter.ferrie@gmail.com)
;dst<src

*=$803

;unpacker variables, no need to change these
src	=	$0
dst	=	$2
end	=	$4
count	=	$6
delta	=	$8
A1L	=	$3c
A1H	=	$3d
A2L	=	$3e
A2H	=	$3f
A4L	=	$42
A4H	=	$43

DEST = $6000

init
    lda #<(pakoff+8)        ; 8 = skip lz4 header
    sta src
    lda #>(pakoff+8)
    sta src+1
    lda #<(pakoff+paksize)
    sta end
    lda #>(pakoff+paksize)
    sta end+1
    lda #<DEST
    sta dst
    lda #>DEST
    sta dst+1

unpack ;unpacker entrypoint
	ldy	#0
	jmp	parsetoken

pakoff
;place packed data here for low memory unpacking
.binary "integer.s.lz4"
paksize = * - pakoff - 1

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
	cmp	end
	lda	src+1
	sbc	end+1
	bcs	done

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
+	lda	src+1
	pha
	lda	src
	pha
	sec
	lda	dst
	sbc	delta
	sta	src
	lda	dst+1
	sbc	delta+1
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
	bne	++
-	sta	count
	jsr	getsrc
	tax
	clc
	adc	count
	bcc	+
	inc	count+1
+	inx
	beq	-
+	rts

getput
	jsr	getsrc

putdst
	sta 	(dst), y
	inc	dst
	bne	+
	inc	dst+1
+	rts

getsrc
	lda 	(src), y
	inc	src
	bne	+
	inc	src+1
+	rts
