(defpackage :melisma
  (:use :cl :eric))

(in-package :melisma)

;; A single note or chord.
(defstruct note pitch duration)

;; The note which a note-pitch is relative to. (tone octave)
(defvar *pitch-zero* '(c 1))

(defvar *pitches* '(c d e f g a b))

(defun output-pitch (pitch)
  (let* ((absolute-pitch (+ (position (first *pitch-zero*) *pitches*) pitch))
	 (letter (nth (mod absolute-pitch (length *pitches*))
		      *pitches*))
	 (octave (+ (second *pitch-zero*) (int/ absolute-pitch (length *pitches*)))))
    (format nil "~(~a~)~{~a~}" letter
	    (rest (if (> octave 0)
		      (loop for i from 0 to octave collect "'")
		      (loop for i from octave to 0 collect ","))))))

(defun output-note (note)
  (format nil "~a~d" (output-pitch (note-pitch note)) duration))

(provide :melisma)
