(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:light
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:light)

(defun theme (key &optional version)
  (let ((*base-pitch* (copy-base-pitch *default-voice* (+ (base-pitch-for-voice *default-voice*) key))))
    (ecase version
      ((nil)
       (n (minor-degree 1) 1))
      ((t)
       (n (minor-degree 1) 1/2)
       (n (minor-degree 1) 1/2)))
    (n (- (minor-degree 5) 12) 1)
    (n (minor-degree 1) 1)
    (n (minor-degree 1) 1)
    (n (minor-degree 1) 1/2)
    (n (minor-degree 1) 1/2)
    (n (- (minor-degree 5) 12) 1/2)
    (n (minor-degree 1) 1/2)
    (n (minor-degree 3) 1/2)
    (n (minor-degree 5) 1/2)
    (ecase version
      ((nil)
       (n (minor-degree 1) 1))
      ((t)
       (n (minor-degree 8) 1)))))

(defun main ()
  (make-music 180 ((melody (make-voice :instrument "sitar"))
		   bass)
    (let ((*base-pitch* (list melody (+ /C 12))))
      (theme /C)
      (theme /C t))))

(main)
