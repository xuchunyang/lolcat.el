EMACS ?= emacs

all: compile ert

compile:
	${EMACS} -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile lolcat.el

ert:
	${EMACS} -Q --batch -L . -l *-tests.el -f ert-run-tests-batch-and-exit
