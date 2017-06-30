(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:chords
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:chords)

(defun i-get-off (voice &optional (beats 1))
  (n voice i-first beats)
  (n voice i-third beats)
  (n voice i-fifth beats))

(defun euphoria (voice)
  (n voice i-fifth 3/2)
  (n voice i-third)
  (n voice i-first 3/2)
  (n voice i-third 3/2)
  (n voice i-fifth)
  (n voice i-fifth 3/2))

(defun islander (voice beats)
  (let ((notes (list i-first i-third i-fifth (octaves 1 i-first))))
    (triplet voice (first notes) (second notes) (third notes) beats)
    (n voice (fourth notes) beats)
    (n voice (third notes) beats)
    (n voice (second notes) beats)
    (n voice (first notes) beats)

    (triplet voice (second notes) (second notes) (second notes) beats)
    (n voice (first notes) beats)
    (n voice (octaves -1 (third notes)) beats)))
    

(defun chords ()
  (arrange-music 150 ((melody (make-voice :instrument "dulcimer" :offset-note (octaves 2 a-first))))
    (euphoria melody)
    (r melody 1)
    (i-get-off melody 1)
    (i-get-off melody 1)
    (i-get-off melody 1/2)
    (i-get-off melody 1/2)
    ))

(defun main ()
  (play-lilypond (chords)))

(main)

