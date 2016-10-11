(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:guitar
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:guitar)

(defun opening (end-beats)
  (n (major-chord (major-degree 8)) 2)
  (n (major-chord (major-degree 4)) 2)
  (n (major-chord (major-degree 8)) 1/2)
  (n (major-chord (major-degree 5)) 1/2)
  (n (major-chord (major-degree 1)) 1/2)
  (n (major-chord (major-degree 4)) end-beats))

(defun theme ()
  (opening 1)
  (n (major-chord (major-degree 1)) 1/2)
  (n (major-chord (- (major-degree 5) 12)) 1))

(defun first-pass (bass)
  (repeat ()
    (theme)
    (n (major-chord (major-degree 1)) 8 bass))
  (n (major-chord (major-degree 1)) 4)
  (n (major-chord (major-degree 1)) 4 bass))

(defun main ()
  (make-music 180 ((melody (make-voice :instrument "acoustic guitar (nylon)" :key "g \\major"))
		   bass)
    (let ((*base-pitch* (+ /G 0)))
      (opening 1)
      (n (major-chord (major-degree 1)) 1/2)
      (n (major-chord (- (major-degree 5) 12)) 1)
      (opening 2)
      (n (major-chord (- (major-degree 5) 12)) 1/2)
      (n (major-chord (major-degree 1)) 1/2)
      (n (major-chord (- (major-degree 5) 12)) 1/2)
      (n (major-chord (- (major-degree 4) 12)) 1/2)
      (n (major-chord (major-degree 1)) 1/2)
      (opening 1)
      (n (major-chord (major-degree 1)) 1/2)
      (n (major-chord (- (major-degree 5) 12)) 1)
      
;;      (voice-catch-up bass)

      )))

(main)
