
executables := $(patsubst %.idr,build/exec/%,$(wildcard *.idr))

all: $(executables)

test: build/exec/Test 
	build/exec/Test

build/exec/%: %.idr
	idris2 $< -o $*

.PHONY: clean
clean:
	rm -rf build