(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:moonlight
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:moonlight)

(defun moonlight (action)
  (melisma::arrange-music action 60
      ((melody (make-voice :key "e \\major" :time-sig "4/4"))
       (bass (make-voice :key "e \\major" :clef "bass" :time-sig "4/4")))
    (repeat (8) (triplet (- (1+ /G) 12) (- (1+ /C) 0) /E 1))
    (n (list (- (1+ /C) 24) (- (1+ /C) 12)) 4 bass)
    (n (list (- /B 24) (- /B 12)) 4 bass)

    (repeat (2) (triplet (- /A 12) (- (1+ /C) 0) /E 1))
    (repeat (2) (triplet (- /A 12) (- /D 0) (1+ /F) 1))
    (n (list (- /A 24) (- /A 12)) 2 bass)
    (n (list (- (1+ /F) 24) (- (1+ /F) 12)) 2 bass)

    (triplet (- (1+ /G) 12) (- (1+ /C) 0) (1+ /F) 1)
    (triplet (- (1+ /G) 12) (- (1+ /C) 0) /E 1)
    (triplet (- (1+ /G) 12) (- (1+ /C) 0) (1+ /D) 1)
    (triplet (- (1+ /F) 12) (- /B 12) (1+ /D) 1)
    (repeat (2) (n (list (- (1+ /G) 24) (- (1+ /G) 12)) 2 bass))))
    

(defun main ()
  (moonlight #'melisma::play-lilypond))

(main)
