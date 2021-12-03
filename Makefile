OPT=-L /dev/stdout

%.bin: %.s
	vasm6502_oldstyle $(OPT) -quiet -Fbin -o "$@" "$<"

%.bin: %.S
	vasm6502_oldstyle $(OPT) -quiet -dotdir -Fbin -o "$@" "$<"

target = $(patsubst %.s,%.bin,$(wildcard *.s)) $(patsubst %.S,%.bin,$(wildcard *.S))

all:	$(target)

clean:
	-rm -f $(target)
