CASK ?= cask
EMACS ?= emacs
MAKEM ?= ./makem.sh

.PHONY: all test clean init

all: init build test

init:
	${CASK} install
	${MAKEM} -s.sandbox --install-deps --install-linters

build:
	${CASK} clean-elc
	${CASK} build

test:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ecukes
	${CASK} clean-elc

debug:
	${CASK} clean-elc
	${CASK} build
	${CASK} exec ecukes --tags @debug --debug
	${CASK} clean-elc

clean:
	${CASK} clean-elc

lint:
	${MAKEM} -s.sandbox lint-elsa
