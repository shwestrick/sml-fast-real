all:
	mlton -default-ann 'allowFFI true' -export-header export.h  digit_parse.sml digit_parse_neon.c
obj:
	clang -O3 -c -target arm64-apple-macos digit_parse_neon.c -o digit_parse_neon.o
clean:
	rm digit_parse