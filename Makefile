OPT=

%.bin: %.s
	vasm6502_oldstyle $(OPT) -quiet -L $(<:.s=.lst) -Fbin -o "$@" "$<"

%.bin: %.S
	vasm6502_oldstyle $(OPT) -quiet -L $(<:.S=.lst) -dotdir -Fbin -o "$@" "$<"

target = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))

all:	$(target)

clean:
	-rm -f $(target)
