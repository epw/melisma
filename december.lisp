(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:december
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:december)

(defun phrase (major-p &optional (key /C))
  (let ((major-or-minor (if major-p :major :minor))
	(*base-pitch* key))
    (dolist (pitch (list @E @G @E @D @C @B))
      (n ((lambda (p) (if (eq pitch @B) p (octave p)))
	  (typed-degree major-or-minor pitch))
	 1))
    (n (octave (typed-degree major-or-minor @C)) 2)))

(defun main ()
  (make-music 140 (melody bass)
    (let ((*base-pitch* (list melody (octave /C))))
      (repeat ()
	(n /E 1)
	(repeat (3)
	  (n /C 1/2)
	  (n /E 1))
	(n /C 1/2)
	(repeat (3)
	  (n /E 1/2)
	  (n /C 1/2)
	  (n /C 1/2))
	(n /E 2)
	(ecase %repeat-index
	  (0 (voice-catch-up bass melody))
	  (1 (n /C 3 bass)
	     (n /E 3 bass))))
      )))
    
(main)

