(defpackage #:melisma
  (:use #:cl #:eric)
  (:export #:*notes*
	   #:*base-octave*
	   #:*current-voice*
	   #:note
	   #:note-duration
	   #:note-pitch
	   #:note-octave
	   #:note-voice
	   #:defphrase
	   #:phrase-length
	   #:repeat
	   #:simul
	   #:defpiece
	   #:synth))

(in-package #:melisma)

(defun something ()
  (format t "Do something, not really sure what yet.~%")
  'something)

(defvar *notes*)
(defvar *base-octave* 0)
(defvar *current-voice* 0)

(defstruct note
  duration
  pitch
  (octave 0)
  (voice 0))

(defun note (duration pitch &optional (octave 0))
  (push (make-note :duration duration :pitch pitch :octave (+ *base-octave* octave)
		   :voice *current-voice*)
	*notes*))

(defmacro defphrase (name args &body body)
  `(defun ,name ,args
     ,@body))

(defun phrase-length (piece-fun &rest args)
  (let ((*notes* (list)))
    (apply piece-fun args)
    (loop for note in *notes* summing (/ 4 (note-duration note)))))

(defmacro repeat (&body body)
  `(progn
     ,@body
     ,@body))

(defun simul (&rest phrases)
  (let ((main-length (phrase-length (first phrases))))
    (assert (every (lambda (p) (= main-length (phrase-length p))) (rest phrases))))
  (loop for phrase in phrases for i = 0 then (1+ i) do
       (let ((*current-voice* i))
	 (funcall phrase))))

(defmacro defpiece (name args &body body)
  `(defun ,name ,args
     (let ((*notes* (list)))
       ,@body
       (reverse *notes*))))

(defun octave-marks (note)
  (if (eq (note-pitch note) :r)
      ""
      (let ((octave (note-octave note)))
	(format nil "~v@{~a~:*~}" (abs octave) (if (< octave 0) "," "'")))))

(defun render (f notes)
  (let ((voices (remove-duplicates (mapcar #'note-voice notes))))
    (if (> (length voices) 1)
	(format f "      <<~%"))
    (dolist (voice voices)
      (if (> (length voices) 1)
	  (format f "        {~%"))
      (dolist (note (remove-if-not (lambda (n) (eq (note-voice n) voice)) notes))
	(format f "    ~(~a~)~a~a~%" (note-pitch note) (octave-marks note)
		(note-duration note)))
      (when (> (length voices) 1)
	(format f "        }~%")
	(if (not (eq voice (car (last voices))))
	    (format f "      \\\\~%"))))
    (if (> (length voices) 1)
	(format f "      >>~%"))))


(defun write-lilypond (filename piece instrument)
  (eric:fopen (f filename :w)
    (format f "\\version \"2.16.0\"
\\score {
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = 240~%" instrument "g \\major")
    (render f (funcall piece))
    (format f "  }
  \\layout { }
  \\midi { }
}~%")))

(defun shell (command &rest args)
  (let* ((output (make-string-output-stream))
	 (process (sb-ext:run-program "/usr/bin/env" (cons command args)
				      :output output
				      :error output)))
    (values (sb-ext:process-exit-code process)
	    (get-output-stream-string output))))

(defun shell-show-errors (command &rest args)
  (multiple-value-bind (return-code output) (apply #'shell command args)
    (if (not (zerop return-code))
	(format t "~a" output))
    return-code))

(defun file-ly (name)
  (format nil "~a.ly" name))

(defun file-midi (name)
  (format nil "~a.midi" name))

(defun synth (basename piece)
  (write-lilypond (file-ly basename) piece "acoustic grand")
  (eric:fopen (f (file-ly basename))
    (let ((s (make-string (file-length f))))
      (read-sequence s f)
      (format t "~a" s)))
  (format t "Lilypond~%")
  (if (not (zerop (shell-show-errors "lilypond" (file-ly basename))))
      (return-from synth))
  (format t "Timidity~%")
  (if (not (zerop (shell-show-errors "timidity" (file-midi basename))))
      (return-from synth)))

