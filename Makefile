OPT=--intel-hex

%.bin: %.s
	# vasm6502_oldstyle $(OPT) -maxerrors=50 -ast -esc -quiet -L $(<:.s=.lst) -Fbin  -o "$@" "$<"
	64tass $(OPT) -o "$@" "$<"

%.hex: %.s
	# vasm6502_oldstyle $(OPT) -maxerrors=50 -esc -quiet -L $(<:.s=.lst) -Fihex -o "$@" "$<"
	64tass $(OPT) -L $(<:.s=.lst) -o "$@" "$<"

%.bin: %.S
	vasm6502_oldstyle $(OPT) -maxerrors=50 -esc -quiet -L $(<:.s=.lst) -Fbin  -dotdir -o "$@" "$<"

%.hex: %.S
	vasm6502_oldstyle $(OPT) -maxerrors=50 -esc -quiet -L $(<:.s=.lst) -Fihex -dotdir -o "$@" "$<"


target_hex = $(patsubst %.s,%.hex,$(wildcard *.s)) $(patsubst %.S,%.hex,$(wildcard *.S))
target_bin = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))

all:	hex apple2.rom apple2plus.rom
bin:	$(target_bin)
hex:	$(target_hex)

apple2plus.rom: applesoft.bin mon2.bin
	cat applesoft.bin mon2.bin > apple2plus.rom

apple2.rom:	intbasic.bin floating.bin miniasm.bin floating2.bin miniasm_jmp.bin misc_f669.bin sweet16.bin mon.bin
	cat intbasic.bin floating.bin miniasm.bin floating2.bin miniasm_jmp.bin misc_f669.bin sweet16.bin mon.bin> apple2.rom

diskload.hex: delay.s

clean:
	-rm -f $(target_bin) $(target_hex) $(patsubst %.s,%.lst,$(wildcard *.s))
