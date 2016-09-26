(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:song
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:song)

(defun main ()
  (make-music 200 ((melody (make-voice :instrument "violin"))
		   bass)
    (let ((*base-pitch* (list melody (+ /C 24))))
      (n (major-chord 0) 4)
      )))

(main)
