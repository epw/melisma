(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))
    (load (compile-file "/home/eric/projects/melisma/melisma-instruments.lisp"))))

(defpackage #:palette
  (:use #:cl #:melisma #:melisma-instruments)
  (:export #:main))

(in-package #:palette)

(defun mapcat (list function &optional count)
  (append list (subseq (mapcar function list) 0 count)))

(defun wrap (chord)
  (mapcat chord (lambda (n) (octaves 1 n)) 1))

(defun transform (list function)
  (loop for e in list
     for i = 0 then (1+ i)
     collect
       (funcall function e i)))

(defun palette ()
  (arrange-music 120 ((m (make-voice :instrument +acoustic-grand+ :offset-note (octaves 1)))
		     (b (make-voice :instrument +acoustic-grand+)))
    (let* ((seq (wrap (degree-chord :major 1)))
	   (melody (append seq seq seq seq
			   '(1 4 8 13) seq
			   seq seq)))
      (dolist (note seq)
	(n b note 4))
      (dolist (note melody)
	(n m note 1/2)))))

(lilypond-main (palette))


