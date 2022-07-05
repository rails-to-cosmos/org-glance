CASK ?= cask
EMACS ?= emacs

.PHONY: all test clean init

all: init build test

init:
	${CASK} install

build:
	${CASK} clean-elc
	${CASK} build

test:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ecukes
	${CASK} clean-elc

test-unstable:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ecukes --tags @unstable
	${CASK} clean-elc

clean:
	${CASK} clean-elc
