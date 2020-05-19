(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:tabs
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:tabs)

(defun tabs ()
  (arrange-music 120 ((m (make-voice :instrument +acoustic-guitar-nylon+)))
    (n m 0)))

(lilypond-main (tabs))
