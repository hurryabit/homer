
all: test.wasm


%.o : %.c
	wasm32-unknown-wasi-clang -Wall -nostdlib --target=wasm32 -Oz $< -c -o $@

%.o : %.wat
	wat2wasm --relocatable $< -o $@

test.wasm: runtime.o test.o
	wasm32-unknown-wasi-wasm-ld --no-entry \
	  --export=test_stack \
	  --export=test_closure \
	  --export=test_add \
	  --export=test_gc_i32 \
	  --export=test_gc_closure \
  	  runtime.o test.o -o test.wasm

.PHONY: test
test: test.wasm
	wasm-interp --run-all-exports test.wasm
