;;****************************
;                            *
; BITCOPIER I                *
; COPYRIGHT J._A.L.B. 1982   *
; LISTING : P.CHATAIGNON     *
;                            *
;*****************************

CH = $24
DRVOFF = $C088
DRVON = $C089
INBYT = $C08C
OUTBYT = $C08D
DRVCTL1 = $C08E
DRVCTL2 = $C08F
RDKEY = $FD0C
HOME = $FC58
VTAB = $FC22
COUT = $FDED
CROUT = $FD8E
WAIT = $FCA8
TEST = $7000
GETIOB = $03E3
RWTS = $03D9
XAM = $FDB3
SCREEN = $0583


* = $2B00
 JMP START

;***********************
;                      *
; TABLE CARACTERES     *
;                      *
;***********************


TABLE1 .text "INTRODUISEZ VOS DISQUETTES S.V.P:"
 .byte 0
TABLE2 .text "APPLE BITCOPIER (C) 1982         "
 .text 'JALB '
 .text " "
 .byte 0

TIRET LDX #$28
 LDA #$BD
TIR1 JSR COUT
 DEX
 BNE TIR1
 RTS


;***********************
;                      *
; DEBUT PROGRAMME      *
;                      *
;***********************

START JSR HOME
 LDX #$00
 STA CH
 LDA #$06
 STA CH+1
 JSR VTAB
 JSR TIRET
 LDX #$00
ST1 LDA TABLE2,X
 BEQ ST2
 JSR COUT
 INX
 BNE ST1
ST2 JSR CROUT
 JSR TIRET
 LDA #$04
 STA $24
 LDA #$0C
 STA $25
 JSR VTAB
 LDX #$00
ST3 LDA TABLE1,X
 BEQ ST4
 JSR COUT
 INX
 BNE ST3
ST4 JSR RDKEY

;***********************
;                      *
; INITIALISATION       *
;                      *
;***********************

INIT LDA #$00 ;PISTE DEPART
 STA $02
 LDA #$23 ;PISTE FIN
 STA $03
 LDA #$D5 ;MARQUEURS
 STA $F9
 LDA #$AA
 STA $05
 LDA #$96
 STA $F8
 LDA #$B5
 STA $F7
 JSR HOME
 JSR AFFTRK
 JSR PISTE0
 JMP PGM01

;***********************
;                      *
; ECRITURE             *
;                      *
;***********************

ECRIT LDA #$02
 JSR SEEK0
 LDA DRVON,X
 LDY #$05
ECR1 LDA #$FF
 JSR WAIT
 DEY
 BNE ECR1
 LDY #$FF
 LDA OUTBYT,X
 LDA DRVCTL1,X
 LDA #$FF
 STA DRVCTL2,X
 ORA INBYT,X
 CLC
 CLC
 CLC
 CLC
 CLC
 LDA #$FF
 JSR WAIT12
ECR2 PHA
 PLA
 STA OUTBYT,X
 ORA INBYT,X
 DEY
 BEQ ECR3
 JSR WAIT12
 LDA #$FF
 STX $60
 JMP ECR2
ECR3 LDA TEST,Y
 CLC
 CLC
 INY
 PHA
 PLA
 JMP ECR5
ECR4 LDA TEST,Y
PTR = *-1
 INY
 BEQ ECR7
 CMP #$00
 BEQ ECR8
 CLC
 CLC
 CLC
ECR5 CLC
ECR6 STA OUTBYT,X
 ORA INBYT,X
 JMP ECR4
ECR7 INC PTR
 CLC
 JMP ECR6
ECR8 LDA DRVCTL1,X
 LDA DRVOFF,X
 LDA #$70
 STA PTR
WAIT12 RTS

;***********************
;                      *
; REMET DRIVE PISTE 0  *
;                      *
;***********************

SEEK0 PHA
 JSR GETIOB
 STY $00
 STA $01
 PLA
 LDY #$02
 STA ($00),Y
 LDA $02
 LDY #$04
 STA ($00),Y
 LDA #$00
 LDY #$0C
 STA ($00),Y
 JSR GETIOB
 JSR RWTS
 LDA #$00
 STA $48
 LDY #$01
 LDA ($00),Y
 TAX
 RTS

;***********************
;                      *
; LECTURE              *
;                      *
;***********************

LECTURE LDA #$01
 JSR SEEK0
LECT1 LDX #$60
 LDA DRVON,X
 LDA DRVCTL1,X
 LDA #$00
 STA $00
 LDA #$30
 STA $01
 LDY #$00
LECT2 LDA INBYT,X
 BPL LECT2
 STA ($00),Y
 INC $00
 BNE LECT2
 INC $01
 LDA $01
 CMP #$6F
 BCC LECT2
 LDA DRVOFF,X
 RTS

;***********************
;                      *
; BOUCLE PRINCIPALE    *
;                      *
;***********************

PGM01 LDA #'R'
 JSR AFFICH
 LDA #$00
 STA $FF
PGM02 JSR LECTURE
 LDA #'A'
 JSR AFFICH
 JSR ANALYSE
 BCS PGM09
 LDY #$00
 STY $FC
 LDA #$70
 STA $FD
PGM03 LDA ($FA),Y
 STA ($FC),Y
 INY
 BNE PGM03
 INC $FB
 INC $FD
 LDA $FD
 CMP #$90
 BNE PGM03
 LDA #$05
 STA $25
 LDA #$00
 STA $24
 STA $3C
 LDA #$70
 STA $3D
 STA $3F
 LDA #$5F
 STA $3E
 JSR XAM ;IMPRIME A1 ($3C,$3D)
 LDA #$70
 STA $FB
 LDY #$00
 STY $FA
 STY $FC
 LDA #$75
 STA $FD
 LDY #$00
 LDX #$00
 STX $FE
PGM04 LDA ($FC),Y
 CMP TEST,X
 BEQ PGM06
PGM05 INY
 BNE PGM04
 INC $FD
 INC $FE
 LDA $FE
 CMP #$20
 BNE PGM04
 JMP PGM13
PGM06 TYA
 PHA
 LDA $FD
 PHA
PGM07 INX
 CPX #$0A
 BEQ PGM15
 INY
 BNE PGM08
 INC $FD
PGM08 LDA ($FC),Y
 CMP TEST,X
 BEQ PGM07
 PLA
 STA $FD
 PLA
 TAY
 LDX #$00
 JMP PGM05
PGM09 LDA $FF
 CMP #$04
 BEQ PGM10
 INC $FF
 LDA #'R'
 JSR AFFICH
 JMP PGM02
PGM10 LDA #'1'
 JSR AFFICH
 JMP PGM21
PGM11 LDA $FF
 CMP #$04
 BEQ PGM12
 INC $FF
 JMP PGM16
PGM12 LDA #'2'
 JSR AFFICH
 JMP PGM21
PGM13 LDA $FF
 CMP #$04
 BEQ PGM14
 INC $FF
 LDA #'R'
 JSR AFFICH
 JMP PGM02
PGM14 LDA #'3'
 JSR AFFICH
 JMP PGM21
PGM15 PLA
 STA $FD
 PLA
 TAY
 LDA #$00
 STA ($FC),Y
PGM16 LDA #'W'
 JSR AFFICH
 JSR ECRIT
 LDA #'V'
 JSR AFFICH
 JSR LECT1
 LDA #'A'
 JSR AFFICH
 LDA #$30
 STA $FD
 LDY #$00
 STA $FC
 LDX #$00
PGM17 LDA ($FC),Y
 CMP TEST,X
 BNE PGM18
 INX
 CPX #$10
 BEQ PGM20
 BNE PGM19
PGM18 LDX #$00
PGM19 INY
 BNE PGM17
 INC $FD
 LDA $FD
 CMP #$4F
 BNE PGM17
 JMP PGM11
PGM20 LDA #$B0
 JSR AFFICH
PGM21 LDA $02
 CMP $03
 BEQ PGM22
 INC $02
 JMP PGM01
PGM22 BRK

;***********************
;                      *
; ANALYSE              *
;                      *
;***********************

ANALYSE LDA #$00
 TAY
 STA $FA
 LDA #$30
 STA $FB
ANA01 LDA ($FA),Y
 CMP $F9
 BEQ ANA03
ANA02 INY
 BNE ANA01
 INC $FB
 LDA $FB
 CMP #$4F
 BNE ANA01
 JMP ANA08
ANA03 TYA
 PHA
 LDA $FB
 PHA
 INY
 BNE ANA04
 INC $FB
ANA04 LDA ($FA),Y
 CMP $05
 BNE ANA07
 INY
 BNE ANA05
 INC $FB
ANA05 LDA ($FA),Y
 CMP $F8
 BEQ ANA06
 CMP $F7
 BNE ANA07
ANA06 PLA
 STA $FB
 PLA
 TAY
 CLC
 ADC $FA
 STA $FA
 LDA #$00
 ADC $FB
 STA $FB
 CLC
 RTS
ANA07 PLA
 STA $FB
 PLA
 TAY
 JMP ANA02
ANA08 LDA #$00
 TAY
 STA $FA
 LDA #$30
 STA $FB
ANA09 JSR SYNCR
 BCS ANA12
 LDA ($FA),Y
 CMP $F9
 BNE ANA09
 LDA $FB
 PHA
 INY
 BNE ANA10
 INC $FB
ANA10 LDA ($FA),Y
 CMP $05
 BNE ANA11
 PLA
 STA $FB
 CLC
 RTS
ANA11 PLA
 STA $FB
 JMP ANA09
ANA12 LDA #$00
 TAY
 STA $FA
 LDA #$30
 STA $FB
ANA13 JSR SYNCR
 BCS ANA14
 LDA ($FA),Y
 CMP $F9
 BNE ANA13
 CLC
 RTS
ANA14 LDA #$00
 TAY
 STA $FA
 LDA #$30
 STA $FB
 JSR SYNCR
 RTS

;***********************
;                      *
; ANALYSE NON STANDARD *
;                      *
;***********************

SYNCR LDX #$00
SYN01 LDA ($FA),Y
 CMP #$FF
 BNE SYN03
 INX
 CPX #$05
 BEQ SYN05
SYN02 INY
 BNE SYN01
 INC $FB
 LDA $FB
 CMP #$4F
 BEQ SYN04
 BNE SYN01
SYN03 LDX #$00
 JMP SYN02
SYN04 SEC
 RTS
SYN05 INY
 BNE SYN06
 INC $FB
 LDA $FB
 CMP #$4F
 BEQ SYN04
SYN06 LDA ($FA),Y
 CMP #$FF
 BEQ SYN05
 CLC
 TYA
 ADC $FA
 STA $FA
 LDA #$00
 ADC $FB
 STA $FB
 LDY #$00
 CLC
 RTS

;***********************
;                      *
; AFFICHAGE            *
;                      *
;***********************

AFFICH LDX $02
 STA SCREEN,X
 RTS
AFFTRK LDX #$00
AFF1 LDA TEXTE,X
 CMP #$00
 BEQ AFFRTS
 JSR COUT
 INX
 JMP AFF1
AFFRTS RTS

;***********************
;                      *
; REMET 2 DRIVES SUR 0 *
;                      *
;***********************

PISTE0 LDA $02
 PHA
 LDA #$FF
 STA $02
 LDA #$01
 JSR SEEK0
 LDA #$02
 JSR SEEK0
 PLA
 STA $02
 RTS

;***********************
;                      *
; AFFICHAGE ECRAN      *
;                      *
;***********************

TEXTE
 .text "TR 000000000000000011111111111111112222"
 .byte $8D
 .text "AC 0123456789ABCDEF0123456789ABCDEF0123"
 .byte $8D
 .text "---------------------------------------"
 .byte $8D
 .text "ER"
 .byte $8D
 .text "---------------------------------------"
 .byte $8D
 .byte 0
