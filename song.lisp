(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:song
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:song)

(defun x (num)
  (list num
	   (case num
	     (0 4)
	     (4 7)
	     (7 12))))

(defun main ()
  (make-music 120 ((melody (make-voice :instrument "acoustic grand"))
		   bass)
    (let ((*base-pitch* (list melody /C)))
      (n (x 0) 2)
      (n (x 4) 2)
      (n (x 7) 2)
      (n (x 4) 2)
      (n (x 7) 2)

      (let ((*base-pitch* (copy-base-pitch melody 12)))
	(n 0 1)
	(n 4 1)
	(n 7 1)
	(n 12 1))

      (n (x 7) 2)
      (n (x 4) 2)
      (n 4 2)
      (n 0 4)
      )))

(main)
