(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:with-drums
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:with-drums)

(defun with-drums ()
  (melisma::arrange-music 60
      ((melody (make-voice :key "e \\major" :time-sig "4/4"))
       (drums (make-voice :instrument "drums" :time-sig "4/4")))
    (n melody /C 4)
    (n drums "bd" 4)))

(lilypond-main (with-drums))
