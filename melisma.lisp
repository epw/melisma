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
	   #:do-music
	   #:produce-mp3
	   #:music-beats
	   #:n
	   #:s
	   #:r
	   #:triplet
	   #:/A
	   #:/B
	   #:/C
	   #:/D
	   #:/E
	   #:/F
	   #:/G
	   #:@A
	   #:@B
	   #:@C
	   #:@D
	   #:@E
	   #:@F
	   #:@G
	   #:major-degree
	   #:minor-degree
	   #:typed-degree
	   #:major-chord
	   #:minor-chord
	   #:augmented-chord
	   #:diminished-chord
	   #:diatonic-pitch
	   #:diatonic-chord
	   #:typed-chord
	   #:octave
	   #:repeat
	   #:%repeat-index))

(in-package #:melisma)

(defstruct voice
  (instrument "acoustic grand")
  (key "c \\major")
  (time-sig "4/4")
  clef
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
  tied-p
  articulation)

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
  (typecase pitch
    (string pitch)
    (integer (nth (mod pitch 12) (list 'c 'cis 'd 'dis 'e 'f 'fis 'g 'gis 'a 'ais 'b)))
    (null 'r)))

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
	(format s "~~"))
      (when (note-articulation note)
	(format s "\\~(~a~)" (note-articulation note))))))

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
  (format stream "    \\tuplet ~a/~a { ~{~a ~}}~%" (length (tuplet-notes element)) 2
	  (mapcar (lambda (tuplet-note)
		    (render-pitch-of-size tuplet-note
					  (floor (voice-time-sig-lower voice)
						 (2/ (tuplet-beats element)))))
		  (tuplet-notes element))))

(defun render (f voice)
  (format f "        {~%")
  (setf (voice-current-position voice) 0)
  (dolist (element (voice-timeline voice))
    (render-element f element voice)
    (incf (voice-current-position voice)))
  (format f "        }~%"))

(defun render-voice (stream voice tempo)
  (setf (voice-timeline voice) (nreverse (voice-timeline voice)))
  (if (string= (voice-instrument voice) "drums")
      (format stream "
  \\drums
  {
    \\tempo 4 = ~d~%" tempo)
      (format stream "
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = ~d~%" (voice-instrument voice) (voice-key voice) tempo))
  (when (voice-clef voice)
    (format stream "    \\clef ~(~a~)~%" (voice-clef voice)))
  (render stream voice)
  (format stream "  }~%")
  (setf (voice-timeline voice) (nreverse (voice-timeline voice))))

(defvar *last-lilypond*)

(defvar *articulate-p* t)

(defun render-lilypond (tempo voices &optional (articulate-p *articulate-p*))
  (setf *last-lilypond* (list tempo voices))
  (with-output-to-string (s)
      (format s "\\version \"2.16.0\"
~a

\\score {
~a
  <<" (if articulate-p "\\include \"articulate.ly\"" "")
  (if articulate-p "\\unfoldRepeats \\articulate" ""))
      (dolist (voice voices)
	(render-voice s voice tempo))
      (format s "  >>
  \\layout { }
  \\midi { }
}~%")))

(defun consume-lilypond (lilypond-string command extension &optional (filename "/tmp/melisma") (other-args ()))
  (with-output-to-string (output)
    (if (zerop (sb-ext:process-exit-code
		(sb-ext:run-program "/usr/bin/env" (list "lilypond" "-o" filename "-")
				:input (make-string-input-stream lilypond-string)
				:output output
				:error output)))
	(apply #'shell-show-errors command (if extension (file-ext filename extension) filename) other-args))))

(defun play-lilypond (lilypond-string &optional (filename "/tmp/melisma"))
  (consume-lilypond lilypond-string "timidity" :midi filename))

(defun show-lilypond (lilypond-string &optional (filename "/tmp/melisma"))
  (consume-lilypond lilypond-string "evince" :pdf filename))

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

(defun show-sheet-music (&key clean-run-p c (filename "/tmp/melisma"))
  (if (or clean-run-p c)
      (show-lilypond (render-lilypond (first *last-lilypond*) (second *last-lilypond*) nil))
      (shell-show-errors "evince" (file-ext filename :pdf))))

(defun replay ()
  (play-lilypond (render-lilypond (first *last-lilypond*) (second *last-lilypond*))))

(defvar *base-pitch* 0)

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

(defmacro arrange-music (action tempo voices &body body)
  (let ((just-voices (loop for voice in voices
			collect
			  (if (listp voice) (first voice)
			      voice))))
    `(funcall ,action
      (render-lilypond ,tempo
		       (let (,@(loop for voice in voices
				  collect
				    (if (listp voice) voice
					(list voice '(make-voice)))))
			 (let ((*default-voice* ,(first just-voices)))
			   ,@body
			   (list ,@just-voices)))))))

(defmacro make-music (tempo voices &body body)
  `(arrange-music #'play-lilypond ,tempo ,voices ,@body))

(defmacro do-music (tempo voices &body body)
  `(arrange-music (lambda (s) (format t "~a~%" s)) ,tempo ,voices ,@body))

(defmacro music-beats (&body body)
  "Given a body of music that pushes onto *default-voice*, return the number of beats used."
  `(let ((melody (make-voice)))
     (arrange-music (lambda (_)
		      (declare (ignore _))
		      (voice-position melody))
	 120 ((*default-voice* melody)) ,@body)))

(defmacro play ((&optional (instrument "acoustic grand")) &body body)
  "Most simple macro for trying things out."
  `(make-music 120 ((melody (make-voice :instrument ,instrument)))
     (let ((*default-voice* melody))
       ,@body)))

(defmacro show (&body body)
  "Simple macro to see Lilypond output."
  `(arrange-music (lambda (s) (format t "~a" s)) 120 (melody) ,@body))

(defun get-relative-pitches (pitch voice)
  (if *base-pitch*
      (typecase pitch
	(string nil)
	(list (mapcar (lambda (p) (get-relative-pitches p voice)) pitch))
	(t (+ (base-pitch-for-voice voice *base-pitch*) pitch)))
      pitch))

(defvar *mp3-file* "/home/eric/www/melisma-output.mp3")
(defmacro produce-mp3 (tempo voices &body body)
  (let ((lilypond-string (gensym)))
    `(let ((,lilypond-string (arrange-music #'identity ,tempo ,voices ,@body)))
       (format t "~a~%" (consume-lilypond ,lilypond-string "timidity" :midi "/tmp/produce-mp3" (list "-o" *mp3-file* "-Ov")))
       (let* ((*articulate-p* nil)
	      (,lilypond-string (arrange-music #'identity ,tempo ,voices ,@body)))
	 (consume-lilypond ,lilypond-string "ls" :pdf "/tmp/produce-mp3-clean")
	 (format t "Copy: ~a~%" (shell-show-errors "/bin/cp" "/tmp/produce-mp3-clean.pdf" "/home/eric/www/melisma-output.pdf"))))))

;; Syntactic sugar

(defun n (pitch duration &optional (voice *default-voice*) tied-p articulation)
  (push (make-note :beats duration :pitch (get-relative-pitches pitch voice) :tied-p tied-p :articulation articulation)
	(voice-timeline voice)))

(defun s (pitch duration &optional (voice *default-voice*))
  (push (make-note :beats duration :pitch (get-relative-pitches pitch voice) :articulation "staccato")
	(voice-timeline voice)))

(defun r (duration &optional (voice *default-voice*) tied-p)
    (n nil duration voice tied-p))

;; (play () (tuplet 1 /C /E /G))

(defun triplet (note1 note2 note3 duration &optional (voice *default-voice*))
  (push (make-tuplet :notes (mapcar (lambda (n) (get-relative-pitches n voice)) (list note1 note2 note3)) :beats duration)
	(voice-timeline voice)))

(defvar /C 0)
(defvar /D 2)
(defvar /E 4)
(defvar /F 5)
(defvar /G 7)
(defvar /A 9)
(defvar /B 11)

(defvar @C 1)
(defvar @D 2)
(defvar @E 3)
(defvar @F 4)
(defvar @G 5)
(defvar @A 6)
(defvar @B 7)


(defun major-degree (n)
  (ecase n
    (1 0)
    (2 2)
    (3 4)
    (4 5)
    (5 7)
    (6 9)
    (7 11)
    (8 12)))

(defun minor-degree (n)
  (ecase n
    (1 0)
    (2 2)
    (3 3)
    (4 5)
    (5 7)
    (6 8)
    (7 10)
    (8 12)))

(defun typed-degree (major-or-minor n)
  (ecase major-or-minor
    (:major (major-degree n))
    (:minor (minor-degree n))))

(defun octave (pitch &optional (shift 1))
  (+ pitch (* shift 12)))

(defun major-chord (root)
  (list root (+ root 4) (+ root 7)))
(defun minor-chord (root)
  (list root (+ root 3) (+ root 7)))
(defun augmented-chord (root)
  (list root (+ root 4) (+ root 8)))
(defun diminished-chord (root)
  (list root (+ root 3) (+ root 6)))

(defun diatonic-pitch (root degree)
  (+ root (funcall (case degree
		     ((1 4 5) #'major-degree)
		     (t #'minor-degree))
		   degree)))

;; We need some kind of way to easily add to some or all of a list, so
;; make this shorter:
;; (let ((chord (diatonic-chord 1)))
;;   (list (second chord) (+ (first chord) 12) (+ (third chord) 12)))

(defun diatonic-chord (degree &optional (key *base-pitch*))
  (funcall (case degree
	     ((1 4 5) #'major-chord)
	     (7 #'diminished-chord)
	     (t #'minor-chord))
	   (+ (major-degree degree) key)))

(defun typed-chord (type root)
  (ecase type
    (:major (major-chord root))
    (:minor (minor-chord root))
    (:augmented (augmented-chord root))
    (:diminished (diminished-chord root))))

(defmacro repeat ((&optional (times 2)) &body body)
  (append '(let ((%repeat-index 0)))
	  (loop for i from 1 to times append (append body '((incf %repeat-index))))))

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
