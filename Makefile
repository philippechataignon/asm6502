TOPT=--intel-hex -C -B -q -m --tab-size=4 --line-numbers -Wall -Wlong-branch -Wno-implied-reg -Wno-shadow -D DIRECT:=true

%.bin: %.s
	64tass $(TOPT) -b -L $(<:.s=.lst) -o $@ $<

%.hex: %.s
	64tass $(TOPT) -L $(<:.s=.lst) -o $@ $<

target_hex = $(patsubst %.s,%.hex,$(wildcard *.s)) $(patsubst %.S,%.hex,$(wildcard *.S))

all:	hex apple write_example test.bin.lz4
hex:	$(target_hex)

write_example: write_example.c
	gcc -o write_example $<

test.bin.lz4: write_example
	./write_example
	lz4 -c -l test.bin | tail -c+9 > $@

$(target_hex): apple_enc.inc macros.inc

disksave.hex: xmodem_send.hex disk.inc
diskload.hex: xmodem_recv.hex disk.inc unlz4.hex
diskcopier.hex: xmodem_send.hex disk.inc
libint.hex: mult32.hex div32.hex integer.hex
loadlz.hex: load8000.s unlz4.s
unlz4_example.hex: unlz4.hex test.bin.lz4
xmodem_recv.hex xmodem_send.hex: ssc.hex
ssc_sendrec.hex: ssc.hex
msg_test.hex: msgout.hex

apple:
	make -C $@

.PHONY: all apple

clean:
	-rm -f $(target_bin) $(target_hex) $(patsubst %.s,%.lst,$(wildcard *.s)) *.bin *.hex *.lst test.bin.lz4 write_example
	make -C apple clean
