(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:piece
  (:use #:cl #:melisma))

(in-package #:piece)

(defun high-scale ()
  (list (make-note :beats 1 :pitch 24)
	(make-note :beats 1 :pitch 27)
	(make-note :beats 1 :pitch 31)
	(make-note :beats 1 :pitch 36)))

(play-lilypond
 (let* ((piano (make-voice :instrument "acoustic grand" :key "c \\major" :time-sig "4/4"))
	(bass (make-voice))
	(*default-voice* piano))
   (render-lilypond 240
		    
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :beats 2)
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    ;; (make-note :beats 1 :pitch 15)
		    ;; (make-note :beats 1 :pitch 15)
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    ;; (make-note :beats 2)
		    ;; (make-note :beats 1 :pitch 12)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :beats 1 :pitch 14)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    ;; (make-note :beats 4 :pitch 12)
		    ;; (make-note :voice bass :beats 4 :pitch 0)
		    (high-scale)
		    (list (make-note :beats 1 :pitch 24)
			  (make-note :beats 1 :pitch 27)
			  (make-note :beats 1 :pitch 30)
			  (make-note :beats 1 :pitch 39))
		    '(:ctrl (voice-catch-up bass))
		    (make-note :voice bass :beats 4 :pitch 0)
		    )))



(play-lilypond
 (render-lilypond
  (dotimes (_ 2)
    (declare (ignore _))
    (add-note :beats 1 :pitch 12)
    (add-note :beats 1 :pitch 15)
    (add-note :beats 1 :pitch 19)
    (add-note :beats 1 :pitch 24)
    (add-note :voice bass :beats 4 :pitch 0))))
