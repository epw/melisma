(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:song
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:song)

(defun main ()
  (make-music 120 ((melody (make-voice :instrument "flute"))
		   bass)
    (dolist (note (melisma::major-chord 24))
      (n note 1))
    (n 36 1)

    (n 0 2 bass)
    (n 4 2 bass)))

(main)
