SOURCES=../lib/github.com/shwestrick/sml-fast-real/**.mlb ../lib/github.com/shwestrick/sml-fast-real/**.sml

test: $(SOURCES) test.sml test.mlb
	mlton -default-type int64 -default-type word64 test.mlb