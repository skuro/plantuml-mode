CASK ?= cask
EMACS ?= emacs

all: test

test: unit ecukes

unit:
	${CASK} exec ert-runner

# TODO: add BDD style tests
#ecukes:
#	${CASK} exec ecukes

install:
	${CASK} install

.PHONY:	all test unit ecukes install
