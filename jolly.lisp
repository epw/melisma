(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:jolly
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:jolly)

(defun main ()
  (make-music 180 ((melody (make-voice :instrument "acoustic grand"))
		   (bass (make-voice :instrument "acoustic grand")))
    (let ((*base-pitch* (list melody (+ /C 24))))
      (repeat ()
	(repeat (4)
	  (n /E 1))
	(n /D 1)
	(n /D 1)
	(n /D 2)
	(repeat (4)
	  (n /C 1))
	(n /E 4)

	(let ((*base-pitch* (list melody (+ /C 12))))
	  (repeat (4)
	    (n /A 1))
	  (n /G 1)
	  (n /G 1))
	(n /C 2)

	(n /D 1)
	(n /C 1)
	(n /D 1)
	(n /E 1)
	(ecase %repeat-index
	  (0 (n /D 4))
	  (1 (n /C 4)))
	))))

(main)
