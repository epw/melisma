(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:moonlight
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:moonlight)

;; Sounds wrong, don't know why. Looks right. See ~/moonlight-*

(defun moonlight ()
  (arrange-music 60 ((melody (make-voice :key "cis \\minor" :time-sig "4/4" :middle-c 1))
		     (bass (make-voice :key "cis \\minor" :clef "bass" :time-sig "4/4")))
    (dotimes (i 8) (triplet melody (sharp (octaves -1 :g)) :c :e))
    (n bass (list (octaves -1 :c) :c) 4)
    (n bass (list (octaves -2 :b) (octaves -1 :b)) 4)

    (dotimes (i 2) (triplet melody (octaves -1 :a) :c :e))
    (dotimes (i 2) (triplet melody (octaves -1 :a) (flat :d) :f))
    (n bass (list (octaves -2 :a) (octaves -1 :a)) 2)
    (n bass (list (octaves -2 :f) (octaves -1 :f)) 2)

    (triplet melody (octaves -1 :g) (octaves -1 (sharp :b)) :f)
    (triplet melody (octaves -1 :g) :c :e)
    (triplet melody (octaves -1 :g) :c :d)
    (triplet melody (octaves -1 :f) (octaves -1 (sharp :b)) :d)
    (dotimes (i 2) (n bass (list (octaves -2 :g) (octaves -1 :g)) 2))))

(lilypond-main (moonlight))
