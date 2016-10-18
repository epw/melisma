(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:moonlight
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:moonlight)

(defun main ()
  (make-music 60 ((melody (make-voice :key "e \\major" :time-sig "4/4"))
		  (bass (make-voice :key "e \\major" :clef "bass" :time-sig "4/4")))
    (repeat (8) (triplet (- (1+ /G) 12) (- (1+ /C) 0) /E 1))
    (n (list (- (1+ /C) 24) (- (1+ /C) 12)) 4 bass)
    (n (list (- /B 24) (- /B 12)) 4 bass)))

(main)
