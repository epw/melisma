(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:wizards-walk
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:wizards-walk)

(defun wizards-walk ()
  (arrange-music 60 ((m (make-voice :key "g \\major" :time-sig "2/4" :offset-note (octaves 2))))
    (n m /E 1/2)
    (n m /D 1/4)
    (n m /C 1/4)
    (octave -1
      (n m /B 1/4)
      (n m /A 1/4)
      (n m /G 1/4)
      (n m (sharp /F) 1/4)

      (n m /G 1/4)
      (n m /A 1/4)
      (n m (sharp /F) 1/4)
      (n m /G 1/4)
      (n m /E 1/2)
      (n m /B 1/2))))

(lilypond-main (wizards-walk))
