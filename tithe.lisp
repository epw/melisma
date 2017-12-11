(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:tithe
  (:use #:cl #:melisma)
  (:export #:tithe))

(in-package #:tithe)

(defun tithe ()
  (arrange-music 120 ((melody (make-voice :instrument "acoustic grand" :offset-note (octaves 1)))
		     drums)
    (declare (ignore drums))
    (n melody i-first) ; when
    (n melody i-second) ; the
    (n melody i-third 2) ; tithe
    (n melody i-fourth 2) ; came
    (n melody i-fifth) ; call-
    (n melody i-first) ; -ing
    (n melody i-first) ; where
    (n melody i-second) ; were
    (n melody i-first) ; you
    ))
;; (n melody i-sixth)
    ;; (n melody i-fourth)
    ;; (n melody (octaves -1 i-seventh))
    ;; (n melody i-second)
    ;; (n melody i-first 2)))

(defun main ()
  (play-lilypond (tithe)))

(main)

