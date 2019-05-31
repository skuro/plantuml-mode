#
# Programs used in the make goals
#
export CASK  ?= cask
export EMACS ?= emacs

#
# General configuration
#
export CASK_DIR            ?= `${CASK} package-directory`
export BATCH                = --batch -q -l .emacs/init.el
export COVERALLS_REPO_TOKEN = 61YFD7RG1RRJTs0kEuj1aZX7VBgUPiMNO

all: version test

version:
	$(EMACS) $(BATCH) --version

test: install unit

unit:
	${CASK} exec ert-runner

install:
	${CASK} install

clean:
	rm -Rf .emacs.d
	rm -Rf .cask

.PHONY: all test unit install clean
