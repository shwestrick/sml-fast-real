SML_FAST_REAL_SOURCES=../lib/github.com/shwestrick/sml-fast-real/**.mlb ../lib/github.com/shwestrick/sml-fast-real/**.sml
SML_FAST_REAL_DIR=../lib/github.com/shwestrick/sml-fast-real
C_SOURCES=$(SML_FAST_REAL_DIR)/digit_parse_neon.c
C_FLAGS=-O3 -c -target arm64-apple-macos -fPIC


default: test

lib:
	smlpkg sync

$(SML_FAST_REAL_DIR)/digit_parse_neon.o: $(C_SOURCES)
	clang $(C_FLAGS) $(C_SOURCES) -o $@

test: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb
	mlton -default-type int64 -default-type word64 test.mlb 

test.mpl: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb
	mpl -default-type int64 -default-type word64 test.mpl.mlb

test.ffi: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb $(SML_FAST_REAL_DIR)/digit_parse_neon.o
	mlton -default-type int64 -default-type word64 \
	-default-ann 'allowFFI true' \
	-link-opt $(SML_FAST_REAL_DIR)/digit_parse_neon.o \
	test.mlb

test.mpl.ffi: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb $(SML_FAST_REAL_DIR)/digit_parse_neon.o
	mpl -default-type int64 -default-type word64 \
	-default-ann 'allowFFI true' \
	-link-opt $(SML_FAST_REAL_DIR)/digit_parse_neon.o \
	test.mlb

clean:
	rm -f $(SML_FAST_REAL_DIR)/digit_parse_neon.o
	rm test