;; This package is to test out the idea of using special variables to have
;; "fixed" note attributes and let the rest be fluid.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:fixed
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:fixed)

(defun pitch (&key (base *base-pitch*) (degree 1) (mode :major))
  (+ base (funcall (if (eq mode :major) #'major-degree #'minor-degree) degree)))


(defun main ()
  (make-music 130 ((melody (make-voice :instrument "violin"))
		   bass)
    (let ((*base-pitch* 12))
      (n (pitch) 1)
      (n (pitch :degree 3) 1)
      (n (pitch :degree 3 :mode :minor) 1))))
    
(main)
