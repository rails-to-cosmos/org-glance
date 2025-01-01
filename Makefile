.PHONY: init build test info clean

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
	eask test ert org-glance-test.el

clean:
	eask clean elc
