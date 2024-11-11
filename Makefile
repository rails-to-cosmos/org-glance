.PHONY: build test info

info:
	eask info

build:
	eask package ./dist
	eask reinstall

test:
	eask install
	eask recompile
	eask test activate
	eask test --debug --verbose 5 ert tests/*.el
