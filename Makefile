CASK ?= cask
EMACS ?= emacs

.PHONY: all test testsrc clean init
all: init build
init:
	${CASK} install
build:
	${CASK} clean-elc
	${CASK} build
test:
	${CASK} clean-elc
	${CASK} exec ecukes
	${CASK} build
	${CASK} exec ecukes
	${CASK} clean-elc
clean:
	${CASK} clean-elc
