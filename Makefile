.PHONY: build test info

info:
	eask info

build:
	eask package ./build

test:
	eask install
	eask recompile
	eask test activate
	eask test --debug --verbose 5 ert org-glance-test.el
