CASK ?= cask
EMACS ?= emacs
CASK_DIR ?= `${CASK} package-directory`

all: test

test: unit ecukes

unit:
	${CASK} exec ert-runner

# TODO: add BDD style tests
#ecukes:
#	${CASK} exec ecukes

install:
	${CASK} install

clean:
	rm -Rf .emacs.d
	rm -Rf .cask

.PHONY:	all test unit ecukes install
