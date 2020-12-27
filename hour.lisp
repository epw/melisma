(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:hour
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:hour)


;; (defun four-chord-seq (voice i &optional (v nil) (vi nil) (iv nil))
;;   (let* ((v-dur (if (null v) i v))
;; 	 (vi-dur (if (null vi) v-dur vi))
;; 	 (iv-dur (if (null iv) vi-dur iv)))
;;     (n voice a-first i)
;;     (n voice a-fifth v-dur)
;;     (n voice a-sixth vi-dur)
;;     (n voice a-fourth iv-dur)))

(defun main ()
  (arrange-music 200 ((m (make-voice :instrument +acoustic-grand+ :octave 2))
		      (b (make-voice :instrument +acoustic-grand+ :octave 0)))
    (n m a-first)
    (n m a-first)
    (n m a-first)
    (n m a-first)
    (n m a-third)
    (n m a-first 1/2)
    (n m a-first 1/2)
    ))

(lilypond-main (main))
