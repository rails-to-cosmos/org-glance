CASK ?= cask
EMACS ?= emacs

.PHONY: all test clean init
all: init build
init: ${CASK} install
test:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ert-runner -L . -L test
clean: ${CASK} clean-elc
