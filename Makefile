.PHONY: init build test info

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
	eask recompile
	eask package ./dist
	eask reinstall
	eask test --verbose 5 ert org-glance-test.el
