(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:new
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:new)

(defun sequ (voice offset-degree &optional chords-p )
  (voice-offset-degree (voice offset-degree)
    (n voice (if chords-p (list a-first a-third) a-first) 3/2)
    (n voice (if chords-p (list a-third a-fifth) a-third))
    (n voice (if chords-p (list a-fifth (octaves 1 a-first)) a-fifth) 3/2)))

(defun comb (list)
	   (if (= (length list) 1)
	       (list list)
	       (loop for element in list append
		    (loop for combination in (comb (remove-if (lambda (e) (eq element e)) list))
		       collect (cons element combination)))))

(defun new ()
  (arrange-music 60 ((melody (make-voice :instrument "pan flute" :offset-note (octaves 2)))
		     (bass (make-voice :instrument "orchestral harp" :offset-note (octaves 1))))

    (dolist (order (subseq (comb '(1 4 5 6)) 0 4))
      (print order)
      (dolist (degree order)
	(n bass (degree-chord :major degree) 1)))

    (dotimes (_ 2)
      (r melody 4)
      (n melody (first (degree-chord :major 1)))
      (n melody (second (degree-chord :major 1)))
      (n melody (+ 2 (first (degree-chord :major 1))))
      (n melody (octaves -1 (third (degree-chord :major 1)))))

    ))
    
    ;; (n melody (first (degree-chord :major 1)) 1)
    ;; (n melody (second (degree-chord :major 1)) 1)
    ;; (n melody (third (degree-chord :major 1)) 1)
    ;; (n melody (octaves 1 (first (degree-chord :major 1))) 1)

    ;; (n melody (octaves 1 (first (degree-chord :major 1))) 1)
    ;; (n melody (third (degree-chord :major 1)) 1)
    ;; (n melody (second (degree-chord :major 1)) 1)
    ;; (n melody (first (degree-chord :major 1)) 1)
    ;; ))

    ;; (sequ melody 0)
    ;; (sequ melody 3)
    ;; (sequ melody 0)
    ;; (n melody a-fourth 2)
    ;; (voice-catch-up drums melody)
    ;; (n melody a-second 2)
    ;; (n drums nil 1)
    ;; (n drums +drum-snare+ 1/2)
    ;; (triplet drums +drum-snare+ +drum-snare+ +drum-snare+ 1/2)
    
    ;; (voice-catch-up bass melody)
    ;; (sequ melody 0 t)
    ;; (sequ melody 3 t)
    ;; (sequ melody 0 t)
    ;; (n melody (major-chord a-fourth) 2)
    ;; (n melody (major-chord a-fifth) 2)
    ;; (n melody (major-chord a-sixth) 2)
    ;; (n bass (degree-chord :major 1) 4)
    ;; (n bass (degree-chord :major 4) 4)
    ;; (n bass (degree-chord :major 1) 4)))

(lilypond-main (new))
