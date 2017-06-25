(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:with-drums
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:with-drums)

(defun with-drums (action)
  (melisma::arrange-music action 60
      ((melody (make-voice :key "e \\major" :time-sig "4/4"))
       (drums (make-voice :instrument "drums" :time-sig "4/4")))
    (n /C 4)
    (n "bd" 4)))

(defun main ()
  (with-drums #'melisma::play-lilypond))

(main)
