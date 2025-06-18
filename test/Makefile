SML_FAST_REAL_SOURCES=../lib/github.com/shwestrick/sml-fast-real/**.mlb ../lib/github.com/shwestrick/sml-fast-real/**.sml
SML_FAST_REAL_DIR=../lib/github.com/shwestrick/sml-fast-real
C_SOURCES=$(SML_FAST_REAL_DIR)/digit_parse_neon.c
C_FLAGS=-O3 -c -target arm64-apple-macos -fPIC


default: test

all: test test.mpl parse_arg parse_arg.mpl

lib:
	smlpkg sync

$(SML_FAST_REAL_DIR)/digit_parse_neon.o: $(C_SOURCES)
	clang $(C_FLAGS) $(C_SOURCES) -o $@

test: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb
	mlton -default-type int64 -default-type word64 test.mlb

throughput_test.mpl: $(SML_FAST_REAL_SOURCES) lib throughput_test.sml throughput_test.mlb
	mpl $(COMPILE_FLAGS) throughput_test.mpl.mlb

parse_arg: $(SML_FAST_REAL_SOURCES) parse_arg.sml parse_arg.mlb
	mlton $(COMPILE_FLAGS) parse_arg.mlb

parse_arg.mpl: $(SML_FAST_REAL_SOURCES) parse_arg.sml parse_arg.mlb
	mpl $(COMPILE_FLAGS) -output parse_arg.mpl parse_arg.mlb

edge: $(SML_FAST_REAL_SOURCES) lib edge_cases.sml edge_cases.mlb
	mlton $(COMPILE_FLAGS) edge_cases.mlb

edge.mpl: $(SML_FAST_REAL_SOURCES) edge_cases.sml edge_cases.mlb
	mpl $(COMPILE_FLAGS) -output edge.mpl edge_cases.mlb

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
	rm -f test test.mpl parse_arg parse_arg.mpl edge_cases edge_cases.mpl edge.mpl throughput_test throughput_test.mpl