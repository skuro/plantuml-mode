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

clean:
	rm -Rf .emacs.d
	eval $(foo := $(CASK package-directory))
	echo $(foo)
	#rm -Rf ${dir}/../testing

.PHONY:	all test unit ecukes install
