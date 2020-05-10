CASK ?= cask
EMACS ?= emacs

.PHONY: all test clean
all: init
init: ${CASK} install
test:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ert-runner -L . -L test
clean: rm -rf *.elc
