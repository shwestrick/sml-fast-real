SML_FAST_REAL_SOURCES=../lib/github.com/shwestrick/sml-fast-real/**.mlb ../lib/github.com/shwestrick/sml-fast-real/**.sml

COMPILE_FLAGS=-default-type int64 -default-type word64

default: test

all: test test.mpl parse_arg parse_arg.mpl

lib:
	smlpkg sync

test: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb
	mlton $(COMPILE_FLAGS) test.mlb

test.mpl: $(SML_FAST_REAL_SOURCES) lib test.sml test.mpl.mlb
	mpl $(COMPILE_FLAGS) test.mpl.mlb

throughput_test: $(SML_FAST_REAL_SOURCES) lib throughput_test.sml throughput_test.mlb
	mlton $(COMPILE_FLAGS) throughput_test.mlb

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

clean:
	rm -f test test.mpl parse_arg parse_arg.mpl edge_cases edge_cases.mpl edge.mpl throughput_test throughput_test.mpl