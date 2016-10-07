QUICKLISP = $(HOME)/quicklisp/setup.lisp

melisma2ly: melisma.lisp web.lisp
	sbcl --load $(QUICKLISP) --load web.lisp
