TOPT=--intel-hex -B -q -m --tab-size=4 --line-numbers -Wall -Wlong-branch -Wno-implied-reg

%.bin: %.s
	64tass $(TOPT) -b -L $(<:.s=.lst) -o "$@" "$<"

%.hex: %.s
	64tass $(TOPT) -L $(<:.s=.lst) -o "$@" "$<"

target_hex = $(patsubst %.s,%.hex,$(wildcard *.s)) $(patsubst %.S,%.hex,$(wildcard *.S))
target_bin = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))

all:	hex
bin:	$(target_bin)
hex:	$(target_hex)

$(target_hex): apple_enc.inc

diskload.hex: delay.s
libint.hex: mult32.s div32.s integer.s
print_hello.s: printstr.s
loadlz.hex: load8000.s unlz4.s
unlz4_example.hex: unlz4.s integer.s.lz4

clean:
	-rm -f $(target_bin) $(target_hex) $(patsubst %.s,%.lst,$(wildcard *.s)) *.bin *.hex *.lst
