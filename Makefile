AS      = as
LD      = ld
ASFLAGS = --fatal-warnings

lowio: lowio.o
	$(LD) -o $@ $<

lowio.o: lowio.s
	$(AS) $(ASFLAGS) -o $@ $<

.PHONY: test run clean
test: lowio
	python3 test_lowio.py

run: lowio
	./lowio

clean:
	rm -f lowio lowio.o
