.PHONY: build test info clean lint init

info:
	eask info

init:
	eask install-deps
	eask recompile
	eask install
	eask test activate

build: init
	eask package ./dist
	eask reinstall

test: build
	eask run command test

lint:
	eask lint checkdoc

clean:
	eask clean elc
