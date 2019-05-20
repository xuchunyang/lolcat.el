EMACS ?= emacs

all: compile

compile:
	${EMACS} -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile lolcat.el
