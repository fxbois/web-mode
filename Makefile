CASK ?= cask
EMACS ?= emacs

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile web-mode.el

clean-elc:
	rm -f web-mode.elc

.PHONY:	all test unit
