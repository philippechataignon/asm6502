INCLUDE :?= 0

M1 = $6          ; M1
M2 = $EB         ; M2
RES = $FA         ; RES result
T :?= $19         ; T temp

.if INCLUDE != 0
* = $BC00
.fi

    ; clear result
    lda #0      
.for i in range(4)
    sta RES+i
.next

    ; copy M2 to T
.for i in range(4)
    lda M2+i
    sta T+i
.next

    ; main loop
    ldy #32     ; loop 32 times over M2 bits
-   asl RES       ; shift 32 bits RES
    rol RES+1
    rol RES+2
    rol RES+3
    asl T       ; shift 32 bits T(=M2) to get high bit
    rol T+1
    rol T+2
    rol T+3     ; get M2 high bit in C
    bcc +       ; skip if C=0
    clc         ; else, add M1 to RES
    ; RES <- RES+M1
.for i in range(4)
    lda RES+i
    adc M1+i
    sta RES+i
.next
+   dey       ; 32 iterations ?
    bne -    ; next M2 bit
    rts
