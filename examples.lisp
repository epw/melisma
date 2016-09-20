(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:examples
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:examples)

(defun up ()
  (dolist (note (major-chord 0))
    (n note 1/4))
  (n nil 1/4))

(defun up-fill (voice-to-fill voice-at-point beats)
  (declare (ignore voice-at-point beats))
  (dolist (note (major-chord 0))
    (n note 1/4 voice-to-fill))
  (n nil 1/4 voice-to-fill))

(defun bass-up (bass)
  (dolist (note (major-chord 0))
    (n note 2 bass)))

(defun main ()
  (make-music 120 ((melody (make-voice :instrument "acoustic grand"))
		   bass)
    (let ((*base-pitch* (list melody (+ /C 12))))
      (up)
      (voice-catch-up bass)
;;      (repeat (6) (up))
      (bass-up bass)
      (voice-catch-up melody bass #'up-fill)
      (n 12 2 bass)
      )))

(main)
