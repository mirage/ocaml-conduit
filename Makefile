.PHONY: all install clean

all:
	./build.sh

install:
	./build.sh true

clean:
	rm -rf _build
