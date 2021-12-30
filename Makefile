OPT=

%.bin: %.s
	vasm6502_oldstyle $(OPT) -maxerrors=50 -quiet -L $(<:.s=.lst) -Fbin -o "$@" "$<"

%.hex: %.s
	vasm6502_oldstyle $(OPT) -L $(<:.s=.lst) -Fihex -o "$@" "$<"

%.bin: %.S
	vasm6502_oldstyle $(OPT) -quiet -L $(<:.S=.lst) -dotdir -Fbin -o "$@" "$<"

%.hex: %.S
	vasm6502_oldstyle $(OPT) -L $(<:.S=.lst) -dotdir -Fihex -o "$@" "$<"

target_hex = $(patsubst %.s,%.hex,$(wildcard *.s)) $(patsubst %.S,%.hex,$(wildcard *.S))
target_bin = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))

all:	$(target_bin)
hex:	$(target_hex)

clean:
	-rm -f $(target_bin) $(target_hex)
