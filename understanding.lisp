(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:understanding
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:understanding)

(defun melody-phrase (m &key (chord-modifier #'identity) (triplet-modifier #'identity))
  (let ((chord (funcall chord-modifier (diatonic-chord 1))))
    (n m (first chord) 1)
    (n m (second chord) 1/2)
    (dotimes (_ 3)
      (n m (funcall triplet-modifier (third chord)) 1/2))))

(defun jumping (m &optional (chord-modifier #'identity) (shift -1))
  (let ((chord (funcall chord-modifier (diatonic-chord 1))))
    (n m (first chord) 1)
    (n m (second chord) 1/2)
    (s m (octaves shift (third chord)) 1/2)))

(defun understanding ()
  (arrange-music 120 ((m (make-voice :instrument +acoustic-grand+ :octave 2))
  		      (b (make-voice :instrument +acoustic-grand+)))
    (jumping m)
    (n b (first (diatonic-chord 1)) 3)
    (voice-catch-up m b)
    
    (jumping m #'raise 0)
    (n b (second (diatonic-chord 1)) 3)
    (voice-catch-up m b)

    (let ((chord (raise (diatonic-chord 1))))
      (n m (third chord) 1)
      (n m (second chord) 1/2)
      (s m (third chord) 1/2)
      (n b (third (diatonic-chord 1)) 3)
      (voice-catch-up m b)
      
      (n m (octaves 1 (second chord)) 2))

    ))
					;    (voice-set-key m /A)
;    (n m (lower (diatonic-chord 1) 2) 2)))

(lilypond-main (understanding))
