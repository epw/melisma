(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:four-chord
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:four-chord)


;; (defun four-chord-seq (voice i &optional (v nil) (vi nil) (iv nil))
;;   (let* ((v-dur (if (null v) i v))
;; 	 (vi-dur (if (null vi) v-dur vi))
;; 	 (iv-dur (if (null iv) vi-dur iv)))
;;     (n voice a-first i)
;;     (n voice a-fifth v-dur)
;;     (n voice a-sixth vi-dur)
;;     (n voice a-fourth iv-dur)))

(defun main ()
  (arrange-music 140 ((m (make-voice :instrument +violin+ :octave 1))
		      (b (make-voice :instrument +cello+ :octave 0)))
    (r b 4)
;    (triplet m a-first a-first a-first 1/2)
    (n m a-first 1/4)
    (n m a-first 1/4)
    (n m a-first 1/4)
    (r m 1/4)
    (n m a-first 1)
    (n m a-first 1)
    (n m (octaves -1 a-fifth) 1)
    (n m a-first 3/2)
    ))

(lilypond-main (main))
