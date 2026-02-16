TOPT=--intel-hex -B -q -m --tab-size=4 --line-numbers -Wall -Wlong-branch -Wno-implied-reg -Wno-shadow

%.bin: %.s
	64tass $(TOPT) -b -L $(<:.s=.lst) -o "$@" "$<"

%.hex: %.s
	64tass $(TOPT) -L $(<:.s=.lst) -o "$@" "$<"

target_hex = $(patsubst %.s,%.hex,$(wildcard *.s)) $(patsubst %.S,%.hex,$(wildcard *.S))
target_bin = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))

all:	hex apple2.rom apple2plus.rom phc.rom
bin:	$(target_bin)
hex:	$(target_hex)

apple2plus.rom: applesoft.bin mon2.bin
	cat applesoft.bin mon2.bin > apple2plus.rom
	objcopy --input-target=binary --output-target=ihex --change-addresses 53248 apple2plus.rom apple2plus.hex

apple2.rom:	intbasic.bin floating.bin miniasm.bin floating2.bin miniasm_jmp.bin misc_f669.bin sweet16.bin mon.bin
	cat intbasic.bin floating.bin miniasm.bin floating2.bin miniasm_jmp.bin misc_f669.bin sweet16.bin mon.bin> apple2.rom
	objcopy --input-target=binary --output-target=ihex --change-addresses 57344 apple2.rom apple2.hex

phc.rom: applesoft.bin mon_phc.bin
	cat applesoft.bin mon_phc.bin > phc.rom
	objcopy --input-target=binary --output-target=ihex --change-addresses 53248 phc.rom phc.hex

clean:
	-rm -f $(target_bin) $(target_hex) $(patsubst %.s,%.lst,$(wildcard *.s)) *.bin *.hex *.lst
