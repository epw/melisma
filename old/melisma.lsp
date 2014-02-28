(in-package :melisma)

(defstruct note-or-rest
  "Either a note or a rest. Rests have :pitch NIL"
  pitch
  duration)

(defstruct phrase
  "A basic unit of music, where one thing happens."
  (notes () :type list))

(defun note (pitch duration)
  (make-note-or-rest :pitch pitch :duration duration))

(defun music-rest (duration)
  (make-note-or-rest :pitch NIL :duration duration))

