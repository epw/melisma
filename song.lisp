(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:piece
  (:use #:cl #:melisma))

(in-package #:piece)

(play-lilypond (render-lilypond "acoustic grand" "c \\major" 240
				(make-note :voice :v :duration 1 :pitch 0)))

(sb-ext:quit)
