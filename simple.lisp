(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:simple
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:simple)

(defun sequ (voice offset-degree &optional chords-p )
  (voice-offset-degree (voice offset-degree)
    (n voice (if chords-p (list a-first a-third) a-first) 3/2)
    (n voice (if chords-p (list a-third a-fifth) a-third))
    (n voice (if chords-p (list a-fifth (octaves 1 a-first)) a-fifth) 3/2)))

(defun simple ()
  (arrange-music 120 ((melody (make-voice :instrument "electric piano 2" :offset-note (octaves 1)))
		      (bass (make-voice :instrument "electric piano 2"))
		      drums)

    (sequ melody 0)
    (sequ melody 3)
    (sequ melody 0)
    (n melody a-fourth 2)
    (voice-catch-up drums melody)
    (n melody a-second 2)
    (n drums nil 1)
    (n drums +drum-snare+ 1/2)
    (triplet drums +drum-snare+ +drum-snare+ +drum-snare+ 1/2)
    
    (voice-catch-up bass melody)
    (sequ melody 0 t)
    (sequ melody 3 t)
    (sequ melody 0 t)
    (n melody (major-chord a-fourth) 2)
    (n melody (major-chord a-fifth) 2)
    (n melody (major-chord a-sixth) 2)
    (n bass (degree-chord :major 1) 4)
    (n bass (degree-chord :major 4) 4)
    (n bass (degree-chord :major 1) 4)))

(defun main ()
  (play-lilypond (simple)))

(main)

