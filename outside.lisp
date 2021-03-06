(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:outside
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:outside)

(defun outside ()
  (arrange-music 125 ((m (make-voice :instrument +acoustic-guitar-nylon+ :key "f \\major" :octave 2))
		      (b (make-voice :instrument +acoustic-guitar-nylon+ :key "f \\major")))
    (n m (octaves -1 (flat /b)) 1/2)
    (n m /f 1/2)
    (n m /f 1/2)
    (n m /d 1/2)
    (n m /d 1)
    (n m /d 1)
    (n b (list (flat /b) (octaves 1 /f)) 4)
    
    (n m nil 1/2)
    (n m /d 1/2)
    (n m /d 1/2)
    (n m /c 1/2)
    (n m /d 1/2)
    (n m (flat /b) 1/2)
    (n m /a 1)
    (n b (list /g (octaves 1 /d) (octaves 1 /g)) 4)

    (n m /d 1/2)
    (n m /a 1/2)
    (n m /a 1/2)
    (n m /f 1/2)
    (n m /f 1)
    (n m /f 1)
    (n b (list (octaves 1 /d) (octaves 1 /a)) 4)

    (n m /c 1/2)
    (n m /g 1/2)
    (n m /g 1/2)
    (n m /e 1/2)
    (n m /e 1)
    (n m /e 1)
    (n b (list (octaves 1 /c) (octaves 1 /g)) 4)
    ))

(lilypond-main (outside))
