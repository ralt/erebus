LISP ?= sbcl

all: erebus

erebus: $(wildcard src/*.lisp) erebus.asd
	$(LISP) --eval '(ql:quickload :erebus)' \
		--eval '(asdf:make :erebus)' \
		--eval '(quit)'
