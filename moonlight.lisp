(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:moonlight
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:moonlight)

(defun moonlight ()
  (arrange-music 60 ((melody (make-voice :key "e \\major" :time-sig "4/4" :middle-c 1))
		     (bass (make-voice :key "e \\major" :clef "bass" :time-sig "4/4")))
    (dotimes (i 8) (triplet melody
			    (sharp (octaves -1 /G))
			    (sharp /C)
			    /E))
    (n bass (list (sharp (octaves -1 /C)) (sharp /C)) 4)
    (n bass (list (octaves -2 /B) (octaves -1 /B)) 4)

    (dotimes (i 2) (triplet melody
			    (octaves -1 /A)
			    (sharp /C)
			    /E))
    (dotimes (i 2) (triplet melody
			    (octaves -1 /A)
			    /D
			    (sharp /F)))
    (n bass (list (octaves -2 /A) (octaves -1 /A)) 2)
    (n bass (list (sharp (octaves -2 /F)) (sharp (octaves -1 /F))) 2)

    (triplet melody
	     (sharp (octaves -1 /G))
	     /C
	     (sharp /F))
    (triplet melody
	     (sharp (octaves -1 /G))
	     (sharp /C)
	     /E)
    (triplet melody
	     (sharp (octaves -1 /G))
	     (sharp /C)
	     (sharp /D))
    (triplet melody
	     (sharp (octaves -1 /F))
	     (octaves -1 /B)
	     (sharp /D))
    (dotimes (i 2) (n bass (list (sharp (octaves -2 /G)) (sharp (octaves -1 /G))) 2))))

(defun main ()
  (play-lilypond (moonlight)))

(main)
