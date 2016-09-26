(defpackage #:melisma
  (:use #:cl #:eric)
  (:export #:*input*
	   #:*default-voice*
	   #:*base-pitch*
	   #:make-voice
	   #:make-note
	   #:show-sheet-music
	   #:base-pitch-for-voice
	   #:copy-base-pitch
	   #:voice-catch-up
	   #:make-music
	   #:music-beats
	   #:n
	   #:r
	   #:/A
	   #:/B
	   #:/C
	   #:/D
	   #:/E
	   #:/F
	   #:/G
	   #:major-chord
	   #:minor-chord
	   #:augmented-chord
	   #:diminished-chord
	   #:repeat))

(in-package #:melisma)

(defstruct voice
  (instrument "acoustic grand")
  (key "c \\major")
  (time-sig "4/4")
  (timeline ())
  current-position)
(defun time-sig-lower (time-sig)
  (parse-integer (subseq time-sig (1+ (position #\/ time-sig)))))
(defun voice-time-sig-lower (voice)
  (time-sig-lower (voice-time-sig voice)))
(defun time-sig-upper (time-sig)
  (parse-integer (subseq time-sig 0 (position #\/ time-sig))))
(defun voice-time-sig-upper (voice)
  (time-sig-upper (voice-time-sig voice)))
(defun voice-measure-beats (voice)
  (voice-time-sig-upper voice))
(defun voice-position (voice)
  (apply #'+ (mapcar #'count-beats (subseq (voice-timeline voice) 0 (voice-current-position voice)))))
(defun beats-remaining-in-measure (voice)
  (- (voice-measure-beats voice)
     (mod (voice-position voice) (voice-time-sig-upper voice))))

(defvar *default-voice* (make-voice))

(defstruct note
  beats
  pitch
  tied-p)

(defstruct tuplet
  "Multiple notes taking up a different number of beats, spaced evenly. Most commonly triplet."
  notes
  beats)

(defun count-beats (element)
  (typecase element
    (note (note-beats element))
    (list (apply #'+ (mapcar #'count-beats element)))
    (tuplet (tuplet-beats element))
    (t 0)))

(defun ensure-list (atom-or-list)
  (if (listp atom-or-list) atom-or-list
      (list atom-or-list)))

(defun octave-marks (pitch)
  (if pitch
      (let ((octave (floor pitch 12)))
	(format nil "~v@{~a~:*~}" (abs octave) (if (< octave 0) "," "'")))
      ""))

(defun render-pitch (pitch)
  (if pitch
      (nth (mod pitch 12) (list 'c 'cis 'd 'dis 'e 'f 'fis 'g 'gis 'a 'ais 'b))
      'r))

(defun render-note-value (beats time-sig-lower)
  (multiple-value-bind (quotient remainder) (floor time-sig-lower beats)
    (if (zerop remainder)
	(list (format nil "~d" quotient))
	(if (and (= time-sig-lower 8)
		 (>= beats 6))
	    (cons "2."
		  (if (= beats 7) "8" nil))
	    (let ((quotient (floor time-sig-lower (1- beats)))
		  (remainder (first (render-note-value 1 time-sig-lower))))
	      (if (and (every #'digit-char-p remainder)
		       (= (parse-integer remainder) (2* quotient)))
		  (list (format nil "~d." quotient))
		  (list (format nil "~d" quotient) remainder)))))))

(defun render-duration (beats voice)
  (cond ((< beats (beats-remaining-in-measure voice))
	 (render-note-value beats (voice-time-sig-lower voice)))
	(t
	 (loop while (> beats 0)
	    with original-position = (voice-current-position voice)
	    nconc
	      (progn
		(render-note-value (min beats (beats-remaining-in-measure voice))
				   (voice-time-sig-lower voice)))
	    do
	      (decf beats (beats-remaining-in-measure voice))
	      (incf (voice-current-position voice))
	    finally (setf (voice-current-position voice) original-position)))))

(defun render-note (note voice)
  (let ((note-value (if (and (note-pitch note) (listp (note-pitch note)))
			(mapcar (lambda (p)
				  (format nil "~(~a~)~a" (render-pitch p) (octave-marks p)))
				(ensure-list (note-pitch note)))
			(list (format nil "~(~a~)~a" (render-pitch (note-pitch note)) (octave-marks (note-pitch note))))))
	(durations (render-duration (note-beats note) voice)))
    (with-output-to-string (s)
      (dolist (duration durations)
	(format s "~a~{~(~a~)~^ ~}~a~a"
		(if (> (length note-value) 1) "<" "")
		note-value
		(if (> (length note-value) 1) ">" "")
		duration)
	(unless (eq duration (car (last durations)))
	  (format s "~~")))
      (when (note-tied-p note)
	(format s "~~")))))

(defun render-pitch-of-size (pitch size)
  (let ((note-value (if (and pitch (listp pitch))
			(mapcar (lambda (p)
				  (format nil "~(~a~)~a" (render-pitch p) (octave-marks p)))
				(ensure-list pitch))
			(list (format nil "~(~a~)~a" (render-pitch pitch) (octave-marks pitch))))))
    (with-output-to-string (s)
      (format s "~a~{~(~a~)~^ ~}~a~a"
	      (if (> (length note-value) 1) "<" "")
	      note-value
	      (if (> (length note-value) 1) ">" "")
	      size))))

(defgeneric render-element (stream element voice))

(defmethod render-element (stream element voice)
  (declare (ignorable stream element voice))
  nil)

(defmethod render-element (stream (element note) voice)
  (format stream "    ~a~%" (render-note element voice)))

(defmethod render-element (stream (element tuplet) voice)
  (format stream "    \tuplet ~a/~a ~{~a ~}~%" (length (tuplet-notes element)) (tuplet-beats element)
	  ;; Note: '4' is definitely wrong here, need to calculate the correct "shape" of the note.
	  (mapcar (lambda (tuplet-note) (render-pitch-of-size tuplet-note 4)) (tuplet-notes element))))

(defun render (f voice)
  (format f "        {~%")
  (setf (voice-current-position voice) 0)
  (dolist (element (voice-timeline voice))
    (render-element f element voice)
    (incf (voice-current-position voice)))
  (format f "        }~%"))

(defun render-lilypond (tempo voices)
  (with-output-to-string (s)
      (format s "\\version \"2.16.0\"
\\score {
  <<")
      (dolist (voice voices)
	(setf (voice-timeline voice) (nreverse (voice-timeline voice)))
	(format s "
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = ~d~%" (voice-instrument voice) (voice-key voice) tempo)
	(render s voice)
	(format s "  }~%"))
      (format s "  >>
  \\layout { }
  \\midi { }
}~%")))

(defun play-lilypond (lilypond-string &optional (filename "/tmp/melisma"))
  (with-output-to-string (output)
    (if (zerop (sb-ext:process-exit-code
		(sb-ext:run-program "/usr/bin/env" (list "lilypond" "-o" filename "-")
				:input (make-string-input-stream lilypond-string)
				:output output
				:error output)))
	(shell-show-errors "timidity" (file-ext filename :midi)))))

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

(defun file-ext (name ext)
  (format nil "~a.~(~a~)" name ext))

(defun show-sheet-music (&optional (filename "/tmp/melisma"))
  (shell-show-errors "evince" (file-ext filename :pdf)))

(defvar *base-pitch* nil)

(defun base-pitch-for-voice (voice &optional (base-pitch *base-pitch*))
  (typecase base-pitch
    (list
     (getf base-pitch voice 0))
    (t base-pitch)))

(defun (setf base-pitch-for-voice) (voice new-value &optional (base-pitch *base-pitch*))
  (setf (getf base-pitch voice) new-value))

(defun copy-base-pitch (&rest voice-pitch-pairs)
  (typecase *base-pitch*
    (list
     (let ((base-pitch (copy-list *base-pitch*)))
       (loop for voice-pitch-pair on voice-pitch-pairs by #'cddr
	  do
	    (setf (getf base-pitch (first voice-pitch-pair)) (second voice-pitch-pair)))
       base-pitch))
    (t *base-pitch*)))

(defun voice-catch-up (voice-to-rest &optional (voice-at-point *default-voice*) fill-fn)
  (loop while (< (voice-position voice-to-rest) (voice-position voice-at-point))
     for beats = (min (- (voice-position voice-at-point) (voice-position voice-to-rest))
		       (voice-measure-beats voice-to-rest))
     do
       (if fill-fn
	   (funcall fill-fn voice-to-rest voice-at-point beats)
	   (push (make-note :beats beats :pitch nil) (voice-timeline voice-to-rest)))))

;; Syntactic sugar
(defmacro arrange-music (action tempo voices &body body)
  (let ((just-voices (loop for voice in voices
			collect
			  (if (listp voice) (first voice)
			      voice))))
    `(,action
      (render-lilypond ,tempo
		       (let (,@(loop for voice in voices
				  collect
				    (if (listp voice) voice
					(list voice '(make-voice)))))
			 (let ((*default-voice* ,(first just-voices)))
			   ,@body
			   (list ,@just-voices)))))))

(defmacro make-music (tempo voices &body body)
  `(arrange-music play-lilypond ,tempo ,voices ,@body))

(defmacro music-beats (&body body)
  "Given a body of music that pushes onto *default-voice*, return the number of beats used."
  `(let ((melody (make-voice)))
     (arrange-music (lambda (_)
		      (declare (ignore _))
		      (voice-position melody))
	 120 ((*default-voice* melody)) ,@body)))

(defmacro play (&body body)
  "Most simple macro for trying things out."
  `(make-music 120 (melody)
     ,@body))

(defun get-relative-pitches (pitch voice)
  (if *base-pitch*
      (typecase pitch
	(list (mapcar (lambda (p) (get-relative-pitches p voice)) pitch))
	(t (+ (base-pitch-for-voice voice *base-pitch*) pitch)))
      pitch))

(defun n (pitch duration &optional (voice *default-voice*) tied-p)
  (push (make-note :beats duration :pitch (get-relative-pitches pitch voice) :tied-p tied-p)
	(voice-timeline voice)))

(defun r (duration &optional (voice *default-voice*) tied-p)
    (n nil duration voice tied-p))

(defvar /C 0)
(defvar /D 2)
(defvar /E 4)
(defvar /F 5)
(defvar /G 7)
(defvar /A 9)
(defvar /B 11)

(defun major-chord (root)
  (list root (+ root 4) (+ root 7)))
(defun minor-chord (root)
  (list root (+ root 3) (+ root 7)))
(defun augmented-chord (root)
  (list root (+ root 4) (+ root 8)))
(defun diminished-chord (root)
  (list root (+ root 3) (+ root 6)))

(defmacro repeat ((&optional (times 2)) &body body)
  (cons 'progn (loop for i from 1 to times append body)))

;; Example music, rather than structure

(defun experiment ()
  (make-music 120 (melody bass)
    (push (make-note :beats 1 :pitch 12) (voice-timeline melody))
    (push (make-note :beats 1 :pitch 16) (voice-timeline melody))
    (push (make-note :beats 1 :pitch 19) (voice-timeline melody))
    (push (make-note :beats 2 :pitch 24) (voice-timeline melody))

    (voice-catch-up bass melody)

    (n 12 1)
    (n 16 1)
    (n 19 1)
    (n 24 1)

    (n 0 4 bass)))
