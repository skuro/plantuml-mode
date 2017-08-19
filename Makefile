CASK ?= cask
EMACS ?= emacs
<<<<<<< HEAD
=======
CASK_DIR ?= `${CASK} package-directory`
>>>>>>> develop

all: test

test: unit ecukes

unit:
	${CASK} exec ert-runner

# TODO: add BDD style tests
#ecukes:
#	${CASK} exec ecukes

install:
	${CASK} install

<<<<<<< HEAD
=======
clean:
	rm -Rf .emacs.d
	rm -Rf .cask

>>>>>>> develop
.PHONY:	all test unit ecukes install
