(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:chords
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:chords)

(defun opening (melody bass notes &optional (followthrough-p t))
  (dolist (n notes)
    (n melody n
       (if followthrough-p 1
	   (if (eq n (car (last notes))) 2 1))))
  (if followthrough-p (n melody (first notes)))
  (n bass notes 4))

(defun rising (melody bass degree)
  (octave -1 (n bass (degree-chord :major degree) 4))
  (voice-offset-inc (melody (major-degree degree))
    (let ((notes (append (degree-chord :major 1))))
      (n melody (pop notes) 1/2)
      (n melody (pop notes) 1/2)
      (n melody (pop notes)))
    (n melody (octaves 1 a-first))
    (n melody a-fifth)))

(defun chords ()
  (arrange-music 140 ((melody (make-voice :instrument "acoustic guitar (nylon)" :offset-note (octaves 1)
					  :midi-range (cons 0.3 0.5)))
		      (bass (make-voice :instrument "acoustic grand" :offset-note 0)))
    (opening melody bass (degree-chord :major 1))
    (opening melody bass (degree-chord :major 4))
    (opening melody bass (degree-chord :major 6) nil)
    (opening melody bass (degree-chord :major 5))

    (rising melody bass 1)
    (rising melody bass 2)
    (rising melody bass 3)

    (voice-offset-inc (melody (major-degree 3))
      (n melody (octaves 1 a-first))
      (n melody a-fifth)
      (n melody (octaves 1 a-fifth)))))


(defun main ()
  (play-lilypond (chords)))

(main)

