CASK ?= cask
EMACS ?= emacs

.PHONY: all test clean

all: init

init:
	${CASK} install
	${CASK} exec ert-runner init

test:
	${CASK} exec ert-runner -L . -L test

clean:
	  rm -rf *.elc
