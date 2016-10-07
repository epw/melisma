(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:melisma-web
  (:use #:cl #:melisma))

(defun main ()
  (in-package #:melisma-web)
  (eval
   (read-from-string
    (with-output-to-string (s)
      (loop for line = (read-line *standard-input* nil nil)
	 while line
	 do (format s "~a~%" line))))))

(sb-ext:save-lisp-and-die "melisma2ly" :toplevel #'main :executable t)
