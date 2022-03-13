  * =    $F669
  sty    $58
  stx    $57
  sta    $56
  php
  pla
  sta    $59
  tsx
  inx
  inx
  lda    $0100,x
  asl    a
  asl    a
  asl    a
  asl    a
  rts
  ldy    $58
  ldx    $57
  lda    $59
  pha
  lda    $56
  plp
  rts
