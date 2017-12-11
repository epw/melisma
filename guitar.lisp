(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:guitar
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:guitar)

(defun opening (voice end-beats)
  (n voice (major-chord (major-degree 8)) 2)
  (n voice (major-chord (major-degree 4)) 2)
  (n voice (major-chord (major-degree 8)) 1/2)
  (n voice (major-chord (major-degree 5)) 1/2)
  (n voice (major-chord (major-degree 1)) 1/2)
  (n voice (major-chord (major-degree 4)) end-beats))

(defun theme (voice)
  (opening voice 1)
  (n voice (major-chord (major-degree 1)) 1/2)
  (n voice (major-chord (- (major-degree 5) 12)) 1))

(defun first-pass (melody bass)
  (dotimes (_ 2)
    (theme melody)
    (n bass (major-chord (major-degree 1)) 8))
  (n bass (major-chord (major-degree 1)) 4)
  (n bass (major-chord (major-degree 1)) 4))

(defun main ()
  (arrange-music 180 ((melody (make-voice :instrument "acoustic guitar (nylon)" :key "g \\major" :offset-note /G)))
    (opening melody 1)
    (n melody (major-chord (major-degree 1)) 1/2)
    (n melody (major-chord (- (major-degree 5) 12)) 1)
    (opening melody 2)
    (n melody (major-chord (- (major-degree 5) 12)) 1/2)
    (n melody (major-chord (major-degree 1)) 1/2)
    (n melody (major-chord (- (major-degree 5) 12)) 1/2)
    (n melody (major-chord (- (major-degree 4) 12)) 1/2)
    (n melody (major-chord (major-degree 1)) 1/2)
    (opening melody 1)
    (n melody (major-chord (major-degree 1)) 1/2)
    (n melody (major-chord (- (major-degree 5) 12)) 1)
      
;;      (voice-catch-up bass)

      ))

(lilypond-main (main))

