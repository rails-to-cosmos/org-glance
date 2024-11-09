.PHONY: test

test:
	eask install && eask test ert org-glance-test.el
