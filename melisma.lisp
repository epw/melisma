(defpackage #:melisma
  (:use #:cl #:eric))

(in-package #:melisma)

(defun something ()
  (format t "Do something, not really sure what yet.~%")
  'something)

(defvar *notes*)

(defmacro defphrase (name args &body body)
  `(defun ,name ,args
     ,@body))

(defstruct note
  duration
  pitch
  (octave 0))

(defun note (duration pitch &optional octave)
  (push (make-note :duration duration :pitch pitch :octave (if octave octave 0))
	*notes*))


(defphrase a (&optional variation)
  (note 4 'g 1)
  (note 4 'b 1)
  (note 4 'd 1)
  (if (eq variation 'recap)
      (note 4 'r)
      (note 4 'g 2)))

(defphrase b ()
  (note 1 'b 0))

(defmacro repeat (&body body)
  `(progn
     ,@body
     ,@body))

(defmacro defpiece (name args &body body)
  `(defun ,name ,args
     (let ((*notes* (list)))
       ,@body
       (reverse *notes*))))

(defpiece piece ()
  (repeat (a))
  (repeat
   (b)
   (a 'recap))
  (note 4 'd 1)
  (note 4 'b 1)
  (note 2 'g 1))

(defun octave-marks (note)
  (let ((octave (note-octave note)))
    (format nil "~v@{~a~:*~}" (abs octave) (if (< octave 0) "," "'"))))

(defun render (f notes)
  (dolist (note notes)
    (format f "    ~(~a~)~a~a~%" (note-pitch note) (octave-marks note)
	    (note-duration note))))

(defun write-lilypond (filename piece instrument)
  (eric:fopen (f filename :w)
    (format f "\\version \"2.16.0\"
\\score {
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = 120~%" instrument "g \\major")
    (render f (funcall piece))
    (format f "  }
  \\layout { }
  \\midi { }
}~%")))

(defun synth ()
  (write-lilypond "piece.ly" #'piece "acoustic grand"))

