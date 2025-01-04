.PHONY: init build test info clean lint

info:
	eask info

init:
	eask install-deps
	eask recompile
	eask install
	eask test activate

build:
	eask install-deps
	eask recompile
	eask package ./dist
	eask reinstall

test:
	make clean
	eask recompile
	eask package ./dist
	eask reinstall
	eask run command test

lint:
	make clean
	eask lint checkdoc

clean:
	eask clean elc
