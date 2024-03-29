TOPT=--intel-hex -C -B -q -m --tab-size=4 --line-numbers -Wall -Wlong-branch -Wno-implied-reg -Wno-shadow -D DIRECT:=true

%.bin: %.s
	64tass $(TOPT) -b -L $(<:.s=.lst) -o $@ $<

%.hex: %.s
	64tass $(TOPT) -L $(<:.s=.lst) -o $@ $<

target_hex = $(patsubst %.s,%.hex,$(wildcard *.s)) $(patsubst %.S,%.hex,$(wildcard *.S))
target_bin = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))


all:	hex apple
bin:	$(target_bin)
hex:	$(target_hex)

integer.s.lz4: integer.s
	lz4 -f $< $@
	touch $@


$(target_hex): apple_enc.inc macros.inc

diskload.hex: unlz4.s xmodem_recv.s disk.inc
libint.hex: mult32.s div32.s integer.s
loadlz.hex: load8000.s unlz4.s
unlz4_example.hex: unlz4.s integer.s.lz4
inc.hex: inc1.s inc2.s
disksave.hex: xmodem_send.s disk.inc
diskcopier.hex: xmodem_send.s disk.inc
xmodem.hex xmodem256_recv.hex xmodem256_send.hex ssc_sendrec.hex: ssc.s

apple:
	make -C $@

.PHONY: all apple

clean:
	-rm -f $(target_bin) $(target_hex) $(patsubst %.s,%.lst,$(wildcard *.s)) *.bin *.hex *.lst integer.s.lz4
	make -C apple clean
