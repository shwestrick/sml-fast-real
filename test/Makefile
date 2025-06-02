SML_FAST_REAL_SOURCES=../lib/github.com/shwestrick/sml-fast-real/**.mlb ../lib/github.com/shwestrick/sml-fast-real/**.sml

default: test

lib:
	smlpkg sync

test: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb
	mlton -default-type int64 -default-type word64 test.mlb

test.mpl: $(SML_FAST_REAL_SOURCES) lib test.sml test.mlb
	mpl -default-type int64 -default-type word64 test.mpl.mlb