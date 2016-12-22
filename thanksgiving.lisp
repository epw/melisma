(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:thanksgiving
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:thanksgiving)

(defun phrase (major-p &optional (key /C))
  (let ((major-or-minor (if major-p :major :minor))
	(*base-pitch* key))
    (dolist (pitch (list @E @G @E @D @C @B))
      (n ((lambda (p) (if (eq pitch @B) p (octave p)))
	  (typed-degree major-or-minor pitch))
	 1))
    (n (octave (typed-degree major-or-minor @C)) 2)))

(defun main ()
  (produce-mp3 120 (melody)
    (phrase t)
    (phrase nil (octave /A -1))
    (phrase t /G)
    (phrase t)))

(main)

