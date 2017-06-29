(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :eric)
    (ql:quickload "eric"))
  (unless (find-package :melisma)
    (load (compile-file "/home/eric/projects/melisma/melisma.lisp"))))

(defpackage #:song
  (:use #:cl #:melisma)
  (:export #:main))

(in-package #:song)

(defun theme (voice offset)
  (voice-offset-inc (voice offset)
    (n voice (octaves 1 a-first) 1)
    (n voice a-fifth 1/2)
    (n voice (octaves 1 a-first) 1/2)
    (n voice a-fifth 1)
    (n voice a-third 1)))

(defun speedy (voice)
  (octave 2
    (n voice (major-degree 1) 1/4)
    (n voice (major-degree 1) 1/4)
    (n voice (major-degree 5) 1/2)
    (n voice (major-degree 1) 1/2)
    (n voice (major-degree 5) 1)))

(defun falling (voice key)
  (voice-offset-inc (voice key)
    (n voice (major-degree 8) 1)
    (n voice (major-degree 5) 1)
    (n voice (major-degree 3) 1)
    (n voice (major-degree 1) 1)))

(defun rising (voice key &optional (offset 0))
  (voice-offset-inc (voice key)
    (let ((pitches (major-chord 0))
	  (extensions 0))
      (macrolet ((pop-extend (list)
		   `(progn (when (zerop (length ,list))
			     (incf extensions)
			     (setf ,list (major-chord (* extensions 12))))
			   (pop ,list))))
	(dotimes (i offset)
	  (pop-extend pitches))
	(dotimes (i 4)
	  (n voice (pop-extend pitches) 1/2))))))

(defun song ()
  (arrange-music 160 ((melody (make-voice :instrument "acoustic grand" :offset-note (octaves 1 /C)))
		      bass)
      (theme melody a-first)
      (theme melody a-first)
      (theme melody a-second)
      (theme melody a-second)
      (voice-catch-up bass melody)
      (theme melody a-first)
      (n bass (major-chord a-first))
      (r bass 3)

      (theme melody a-first)
      (n bass (major-chord a-first))
      (r bass)
      (n bass (major-chord a-first))
      (r bass 1)
      (n bass (major-chord a-first) 4)
      (octave 1 (theme melody a-first))

      (speedy melody)))

(defun mini-song ()
  (arrange-music 130 ((melody (make-voice :instrument "acoustic grand" :offset-note (octaves 1 /C))))
      (theme melody a-seventh)))

(defun main ()
  (play-lilypond (song)))

(main)

