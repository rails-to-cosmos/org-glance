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
	${CASK} exec ecukes --debug
	${CASK} clean-elc

it:  # run integration tests
	./script/it.sh

clean:
	${CASK} clean-elc
