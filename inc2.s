var = 2

.if DIRECT
* = $300
.fi

start2  lda #2
        sta var + range($10)
        rts

my_list = (5, 20, 37, 129)

.byte   my_list
.byte   my_list-5

addrs = ($D000, $FDED, $FA62)

.byte <addrs
.byte >addrs

Cwhite = 1
Cred = 2
Cblue = 3
screen_param := ((1, 2, Cblue, "Texte1"), (3, 1, Cred, "Texte2"), (5, 3, Cblue, "Texte3"), (7, 0, Cwhite, "Texte4"))

* = $1000
txtptrs := [screen_text]   ; first offset

screen_text
.for text in screen_param[:, 3]
    .null text  ; define string
    txtptrs := txtptrs .. [txtptrs[-1] + len(text) + 1]
.next

screen_data .block
    x .byte screen_param[:, 0]
    y .byte screen_param[:, 1]
    color .byte screen_param[:, 2]
    text .block
        lo .byte <(txtptrs)
        hi .byte >(txtptrs)
    .bend
.bend

* = $1000
screen_data2 .block
    x .byte screen_param[:, 0]
    y .byte screen_param[:, 1]
    color .byte screen_param[:, 2]
    text .block
        lo .byte <(textinst)
        hi .byte >(textinst)
    .bend
    textinst .bfor ptext in screen_param[:,3]
        .null ptext
    .next
.bend

* = $1000
screen_data3 .block
    x .byte screen_param[:, 0]
    y .byte screen_param[:, 1]
    color .byte screen_param[:, 2]
    text .block
        lo .byte <(textinst)
        hi .byte >(textinst)
    .bend
    textinst .bfor ptext in screen_param[:,3]
        .text ptext
        test .byte 0
    .next
    textnull .block
        lo .byte <textinst.test
        hi .byte >textinst.test
    .bend
.bend

