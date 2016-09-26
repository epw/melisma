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
    (let ((*base-pitch* (list piano (+ /C 12))))
      (repeat ()
	(repeat () (n '(0 4) 1))
	(repeat () (n -5 1)))
      (repeat ()
	(repeat () (n '(4 7) 1))
	(repeat () (n -5 1)))

      )))

(main)
