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

(defun understanding ()
  (arrange-music 120 ((m (make-voice :instrument +acoustic-grand+ :octave 2))
  		      (b (make-voice :instrument +acoustic-grand+)))

    (melody-phrase m :triplet-modifier (lambda (note) (octaves -1 note)))
    (n b (first (diatonic-chord 1)) 3)
    
    (melody-phrase m :chord-modifier #'raise)
    (n b (second (diatonic-chord 1)) 3)

    (n b (third (diatonic-chord 1)) 3)

    ;; (n b (first (diatonic-chord 1)) 3)

    ))
					;    (voice-set-key m /A)
;    (n m (lower (diatonic-chord 1) 2) 2)))

(lilypond-main (understanding))
