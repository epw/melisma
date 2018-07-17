(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:understanding
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:understanding)

(defun understanding ()
  (arrange-music 120 ((m (make-voice :instrument +acoustic-grand+ :octave 1))
  		      (b (make-voice :instrument +acoustic-grand+)))
    (voice-set-key m /A)
    (n m (diatonic-chord 1) 4)))

(lilypond-main (understanding))
