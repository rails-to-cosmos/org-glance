CASK ?= cask
EMACS ?= emacs

.PHONY: all test testsrc clean init
all: init build
init:
	${CASK} install
build:
	${CASK} clean-elc
	${CASK} build
test_src:
	${CASK} clean-elc
	${CASK} exec ecukes
test:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ecukes
	${CASK} clean-elc
it:  # run integration tests
	./script/it.sh
clean:
	${CASK} clean-elc
