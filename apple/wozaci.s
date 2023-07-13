;-------------------------------------------------------------------------
;
;  The WOZ Apple Cassette Interface for the Apple 1
;  Written by Steve Wozniak somewhere around 1976
;
;-------------------------------------------------------------------------

* = $C100

;-------------------------------------------------------------------------
;  Memory declaration
;-------------------------------------------------------------------------

HEX1L           =     $24             ; End address of dump block
HEX1H           =     $25
HEX2L           =     $26             ; Begin address of dump block
HEX2H           =     $27
SAVEINDEX       =     $28             ; Save index in input buffer
LASTSTATE       =     $29             ; Last input state

IN              =     $0200           ; Input buffer
FLIP            =     $C000           ; Output flip-flop
TAPEIN          =     $C081           ; Tape input
KBD             =     $D010           ; PIA.A keyboard input
KBDCR           =     $D011           ; PIA.A keyboard control register
ESCAPE          =     $FF1A           ; Escape back to monitor
ECHO            =     $FFEF           ; Echo character to terminal

;-------------------------------------------------------------------------
;  Constants
;-------------------------------------------------------------------------

CR              =     $8D             ; Carriage Return
ESC             =     $9B             ; ASCII ESC

.include "apple_enc.inc"
.enc "apple"

;-------------------------------------------------------------------------
;  Let's get started
;-------------------------------------------------------------------------

WOZACI          lda     #"*"            ; Print the Tape prompt
                jsr     ECHO
                lda     #CR             ; And drop the cursor one line
                jsr     ECHO

                ldy     #-1             ; Reset the input buffer index
NEXTCHAR        iny
KBDWAIT         lda     KBDCR           ; Wait for a key
                bpl     KBDWAIT         ; Still no key!

                lda     KBD             ; Read key from keyboard
                sta     IN,Y            ; Save it into buffer
                jsr     ECHO            ; And type it on the screen
                cmp     #ESC
                beq     WOZACI          ; Start from scratch if ESC!
                cmp     #CR
                bne     NEXTCHAR        ; Read keys until CR

                ldx     #-1             ; Initialize parse buffer pointer

;-------------------------------------------------------------------------
; Start parsing first or a new tape command
;-------------------------------------------------------------------------

NEXTCMD         lda     #0              ; Clear begin and end values
                sta     HEX1L
                sta     HEX1H
                sta     HEX2L
                sta     HEX2H

NEXTCHR         inx                     ; Increment input pointer
                lda     IN,X            ; Get next char from input line
                cmp     #"R"            ; Read command?
                beq     READ            ; Yes!
                cmp     #"W"            ; Write command?
                beq     WRITE           ; Yes! (note: CY=1)
                cmp     #"."            ; Separator?
                beq     SEP             ; Yes!
                cmp     #CR             ; End of line?
                beq     GOESC           ; Escape to monitor! We're done
                cmp     #" "            ; Ignore spaces
                beq     NEXTCHR
                eor     #"0"            ; Map digits to 0-9
                cmp     #9+1            ; Is it a decimal digit?
                bcc     DIG             ; Yes!
                adc     #$88            ; Map letter "A"-"F" to $FA-$FF
                cmp     #$FA            ; Hex letter?
                bcc     WOZACI          ; No! Character not hex!

DIG             asl                     ; Hex digit to MSD of A
                asl
                asl
                asl

                ldy     #4              ; Shift count
HEXSHIFT        asl                     ; Hex digit left, MSB to carry
                rol     HEX1L           ; Rotate into LSD
                rol     HEX1H           ; Rotate into MSD
                dey                     ; Done 4 shifts?
                bne     HEXSHIFT        ; No! Loop
                beq     NEXTCHR         ; Handle next character

;-------------------------------------------------------------------------
; Return to monitor, prints \ first
;-------------------------------------------------------------------------

GOESC           jmp     ESCAPE          ; Escape back to monitor

;-------------------------------------------------------------------------
; Separating . found. Copy HEX1 to Hex2. Doesn't clear HEX1!!!
;-------------------------------------------------------------------------

SEP             lda     HEX1L           ; Copy hex value 1 to hex value 2
                sta     HEX2L
                lda     HEX1H
                sta     HEX2H
                bcs     NEXTCHR         ; Always taken!

;-------------------------------------------------------------------------
; Write a block of memory to tape
;-------------------------------------------------------------------------

WRITE           lda     #64             ; Write 10 second header
                jsr     WHEADER

WRNEXT          dey                     ; Compensate timing for extra work
                ldx     #0              ; Get next byte to write
                lda     (HEX2L,X)

                ldx     #8*2            ; Shift 8 bits (decremented twice)
WBITLOOP        asl                     ; Shift MSB to carry
                jsr     WRITEBIT        ; Write this bit
                bne     WBITLOOP        ; Do all 8 bits!

                jsr     INCADDR         ; Increment address
                ldy     #30             ; Compensate timer for extra work
                bcc     WRNEXT          ; Not done yet! Write next byte

RESTIDX         ldx     SAVEINDEX       ; Restore index in input line
                bcs     NEXTCMD         ; Always taken!

;-----------------------------------------------------------------------
; Read from tape
;-----------------------------------------------------------------------

READ            jsr     FULLCYCLE       ; Wait until full cycle is detected
                lda     #22             ; Introduce some delay to allow
                jsr     WHEADER         ;  the tape speed to stabilize
                jsr     FULLCYCLE       ; Synchronize with full cycle

NOTSTART        ldy     #31             ; Try to detect the much shorter
                jsr     CMPLEVEL        ;   start bit
                bcs     NOTSTART        ; Start bit not detected yet!

                jsr     CMPLEVEL        ; Wait for 2nd phase of start bit

                ldy     #58             ; Set threshold value in middle
RDBYTE          ldx     #8              ; Receiver 8 bits
RDBIT           pha
                jsr     FULLCYCLE       ; Detect a full cycle
                pla
                rol                     ; Roll new bit into result
                ldy     #57             ; Set threshold value in middle
                dex                     ; Decrement bit counter
                bne     RDBIT           ; Read next bit!
                sta     (HEX2L,X)       ; Save new byte

                jsr     INCADDR         ; Increment address
                ldy     #53             ; Compensate threshold with workload
                bcc     RDBYTE          ; Do next byte if not done yet!
                bcs     RESTIDX         ; Always taken! Restore parse index

FULLCYCLE       jsr     CMPLEVEL        ; Wait for two level changes
CMPLEVEL        dey                     ; Decrement time counter
                lda     TAPEIN          ; Get Tape In data
                cmp     LASTSTATE       ; Same as before?
                beq     CMPLEVEL        ; Yes!
                sta     LASTSTATE       ; Save new data

                cpy     #128            ; Compare threshold
                rts

;-------------------------------------------------------------------------
; Write header to tape
;
; The header consists of an asymmetric cycle, starting with one phase of
; approximately (66+47)x5=565us, followed by a second phase of
; approximately (44+47)x5=455us.
; Total cycle duration is approximately 1020us ~ 1kHz. The actual
; frequencywill be a bit lower because of the additional workload between
; the twoloops.
; The header ends with a short phase of (30+47)x5=385us and a normal
; phase of (44+47)x5=455us. This start bit must be detected by the read
; routine to trigger the reading of the actual data.
;-------------------------------------------------------------------------

WHEADER         stx     SAVEINDEX       ; Save index in input line
HCOUNT          ldy     #66             ; Extra long delay
                jsr     WDELAY          ; CY is constantly 1, writing a 1
                bne     HCOUNT          ; Do this 64 * 256 time!
                adc     #-2             ; Decrement A (CY=1 all the time)
                bcs     HCOUNT          ; Not all done!
                ldy     #30             ; Write a final short bit (start)

;-------------------------------------------------------------------------
; Write a full bit cycle
;
; Upon entry Y contains a compensated value for the first phase of 0
; bit length. All subsequent loops don't have to be time compensated.
;-------------------------------------------------------------------------

WRITEBIT        jsr     WDELAY          ; Do two equal phases
                ldy     #44             ; Load 250us counter - compensation

WDELAY          dey                     ; Delay 250us (one phase of 2kHz)
                bne     WDELAY
                bcc     WRITE1          ; Write a '1' (2kHz)

                ldy     #47             ; Additional delay for '0' (1kHz)
WDELAY0         dey                     ;  (delay 250us)
                bne     WDELAY0

WRITE1          ldy     FLIP,X          ; Flip the output bit
                ldy     #41             ; Reload 250us cntr (compensation)
                dex                     ; Decrement bit counter
                rts

;-------------------------------------------------------------------------
; Increment current address and compare with last address
;-------------------------------------------------------------------------

INCADDR         lda     HEX2L           ; Compare current address with
                cmp     HEX1L           ;  end address
                lda     HEX2H
                sbc     HEX1H
                inc     HEX2L           ; And increment current address
                bne     NOCARRY         ; No carry to MSB!
                inc     HEX2H
NOCARRY         rts
