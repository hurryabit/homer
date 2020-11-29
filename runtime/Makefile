
all: test.wasm


%.o : %.c
	wasm32-unknown-wasi-clang -Wall -nostdlib --target=wasm32 -Oz $< -c -o $@

%.o : %.wat
	wat2wasm --relocatable $< -o $@

runtime.wasm:
	wasm32-unknown-wasi-wasm-ld --no-entry \
	  --export-all \
  	  runtime.o -o runtime.wasm


test.wasm: runtime.o test.o
	wasm32-unknown-wasi-wasm-ld --no-entry --verbose \
	  --export=test_stack \
	  --export=test_closure \
	  --export=test_add \
	  --export=test_gc_i32 \
	  --export=test_gc_closure \
  	  test.o runtime.o -o test.wasm

.PHONY: test
test: test.wasm
	wasm-interp --run-all-exports test.wasm


.PHONY: dump-macros
dump-macros:
	 wasm32-unknown-wasi-clang -dM -E -x c /dev/null
