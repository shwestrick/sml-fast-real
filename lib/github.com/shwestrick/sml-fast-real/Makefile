CC = clang
CFLAGS = -O3 -target arm64-apple-macos -isysroot $(shell xcrun --show-sdk-path)
LDFLAGS = -framework System

all: libdigitparse.dylib

libdigitparse.dylib: digit_parse_neon.c
	$(CC) $(CFLAGS) -shared -o $@ $< $(LDFLAGS)

clean:
	rm -f libdigitparse.dylib