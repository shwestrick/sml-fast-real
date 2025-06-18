all:
	mlton -default-ann 'allowFFI true' -export-header export.h  SIMDParse.sml digit_parse_neon.c
obj-stdout:
	clang -O3 -c -target arm64-apple-macos -isysroot $(shell xcrun --show-sdk-path) digit_parse_neon.c -o digit_parse_neon.o
obj:
	clang -O3 -c -target arm64-apple-macos digit_parse_neon.c -o digit_parse_neon.o

clean:
	rm -f digit_parse_neon.o SIMDParse