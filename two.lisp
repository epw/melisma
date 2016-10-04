(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:two
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:two)

(defun main ()
  (make-music 180 ((piano (make-voice :instrument "acoustic grand"))
		   (violin (make-voice :instrument "violin"))
		   bass)
    (let ((*base-pitch* (list piano (+ /C 0))))
      (n (major-chord /C) 1)
;;      (n '(0 12) 1)
      )))

(main)
