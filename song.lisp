(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:piece
  (:use #:cl #:melisma))

(in-package #:piece)

(defun high-scale ()
  (list (make-note :beats 1 :pitch 24)
	(make-note :beats 1 :pitch 27)
	(make-note :beats 1 :pitch 31)
	(make-note :beats 1 :pitch 36)))

(defun main ()
  (make-music 120 (melody bass)
    (n 
