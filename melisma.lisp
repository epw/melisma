(defpackage #:melisma
  (:use #:cl #:eric)
  (:export #:*input*
	   #:make-voice
	   #:make-note
	   #:show-sheet-music
	   #:voice-catch-up
	   #:arrange-music
	   #:play-lilypond
	   #:make-music
	   #:do-music
	   #:produce-mp3
	   #:music-beats
	   #:voice-offset-set
	   #:voice-offset-inc
	   #:voice-catch-up
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
	   #:a-first
	   #:a-second
	   #:a-third
	   #:a-fourth
	   #:a-fifth
	   #:a-sixth
	   #:a-seventh
	   #:i-first
	   #:i-second
	   #:i-third
	   #:i-fourth
	   #:i-fifth
	   #:i-sixth
	   #:i-seventh
	   #:major-degree
	   #:minor-degree
	   #:typed-degree
	   #:voice-offset-degree
	   #:major-chord
	   #:minor-chord
	   #:augmented-chord
	   #:diminished-chord
	   #:diatonic-pitch
	   #:diatonic-chord
	   #:typed-chord
	   #:octave
	   #:octaves
	   #:flat
	   #:sharp
	   #:repeat
	   #:%repeat-index))

(in-package #:melisma)

(defstruct voice
  (instrument "acoustic grand")
  (offset-note 0)
  (key "c \\major")
  (time-sig "4/4")
  clef
  middle-c
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

(defun make-drums (&key (time-sig "4/4"))
  (make-voice :instrument "drums" :time-sig time-sig))

(defvar *octave-offset* 0)

(defstruct note
  beats
  pitch
  (offset 0)
  tied-p
  articulation)

(defstruct tuplet
  "Multiple notes taking up a different number of beats, spaced evenly. Most commonly triplet."
  notes
  offset
  beats)

(defstruct control
  "Control functions to be executed at particular points in the playback."
  fn)

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

(alexandria:define-constant +circle-of-fifths-sharps+ '(:f :c :g :d :a :e)
  :test 'equal)
(alexandria:define-constant +circle-of-fifths-flats+ '(:b :e :a :d :g :c)
  :test 'equal)
(alexandria:define-constant +circle-of-fifths-major-right+ '(c g d a e b fis)
  :test 'equal)
(alexandria:define-constant +circle-of-fifths-major-left+ '(nil f bes ees aes des ges)
  :test 'equal)
(alexandria:define-constant +circle-of-fifths-minor-right+ '(a e b fis cis gis dis)
  :test 'equal)
(alexandria:define-constant +circle-of-fifths-minor-left+ '(nil d g c f bes ees)
  :test 'equal)

(defun key-intonation (pitch key)
  (multiple-value-bind (key-sym pos) (read-from-string key)
    (let* ((mode (read-from-string (subseq key (1+ pos))))
	   (right (case mode
		    (major +circle-of-fifths-major-right+)
		    (minor +circle-of-fifths-minor-right+)))
	   (left (case mode
		   (major +circle-of-fifths-major-left+)
		   (minor +circle-of-fifths-minor-left+)))
	   (count (position key-sym right)))
      (if count
	  (if (member pitch (subseq +circle-of-fifths-sharps+ 0 count))
	      (format nil "~ais" pitch)
	      (format nil "~a" pitch))
	  (if (member pitch (subseq +circle-of-fifths-flats+ 0 (position key-sym left)))
	      (format nil "~aes" pitch)
	      (format nil "~a" pitch))))))

(defun render-pitch (pitch &optional (offset 0) key)
  (typecase pitch
    (string pitch)
    (keyword
     (format nil "~(~a~)~a" (key-intonation pitch key) (octave-marks offset)))
    (integer
     (let ((actual-pitch (+ pitch offset)))
       (format nil "~(~a~)~a"
	       (nth (mod actual-pitch 12) (list 'c 'cis 'd 'dis 'e 'f 'fis 'g 'gis 'a 'ais 'b))
	       (octave-marks actual-pitch))))
    (null "r")))

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

(defun note-value (pitch offset key)
  (if (and pitch (listp pitch))
      (mapcar (lambda (p) (render-pitch p offset key)) (ensure-list pitch))
      (list (render-pitch pitch offset key))))

(defun render-note (note voice)
  (let ((note-value (note-value (note-pitch note) (note-offset note) (voice-key voice)))
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

(defun render-pitch-of-size (pitch offset size key)
  (let ((note-value (note-value pitch offset key)))
    (with-output-to-string (s)
      (format s "~a~{~(~a~)~^ ~}~a~a"
	      (if (> (length note-value) 1) "<" "")
	      note-value
	      (if (> (length note-value) 1) ">" "")
	      size))))

(defun render (f voice)
  (format f "        {~%")
  (setf (voice-current-position voice) 0)
  (dolist (element (voice-timeline voice))
    (etypecase element
      (control (funcall (control-fn element) voice))
      (note (format f "    ~a~%" (render-note element voice)))
      (tuplet
       (format f "    \\tuplet ~a/~a { ~{~a ~}}~%" (length (tuplet-notes element)) 2
	       (mapcar (lambda (tuplet-note)
			 (render-pitch-of-size tuplet-note (tuplet-offset element)
					       (floor (voice-time-sig-lower voice)
						      (2/ (tuplet-beats element)))
					       (voice-key voice)))
		       (tuplet-notes element)))))
					;    (render-element f element voice)
    (incf (voice-current-position voice)))
  (format f "        }~%"))

(defun render-voice-attrs (voice)
  (with-output-to-string (s)
    (when (voice-middle-c voice)
      (format s "    \\set Staff.middleCPosition = #~d~%" (voice-middle-c voice)))))

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
    \\time ~a
    \\tempo 4 = ~d~%~a" (voice-instrument voice) (voice-key voice) (voice-time-sig voice) tempo (render-voice-attrs voice)))
  (when (voice-clef voice)
    (format stream "    \\clef ~(~a~)~%" (voice-clef voice)))
  (render stream voice)
  (format stream "  }~%")
  (setf (voice-timeline voice) (nreverse (voice-timeline voice))))

(defvar *last-lilypond*)

; Controls "articulation", which makes fancier note attributes like trills work in MIDI, but makes the output ugly
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

(defun show-sheet-music (&key (clean-run-p t) (c t) (filename "/tmp/melisma"))
  (if (and clean-run-p c)
      (show-lilypond (render-lilypond (first *last-lilypond*) (second *last-lilypond*) nil))
      (shell-show-errors "evince" (file-ext filename :pdf))))

(defun replay ()
  (play-lilypond (render-lilypond (first *last-lilypond*) (second *last-lilypond*))))

(defmacro voice-offset-set ((voice shift) &body body)
  (let ((shift-amount (gensym))
	(original (gensym)))
    `(let ((,shift-amount ,shift)
	   (,original (voice-offset-note ,voice)))
       (setf (voice-offset-note ,voice) ,shift-amount)
       ,@body
       (setf (voice-offset-note ,voice) ,original))))

(defmacro voice-offset-inc ((voice shift) &body body)
  `(voice-offset-set (,voice (+ (voice-offset-note ,voice) ,shift)) ,@body))

(defun voice-catch-up (voice-to-rest voice-at-point &optional fill-fn)
  (loop while (< (voice-position voice-to-rest) (voice-position voice-at-point))
     for beats = (min (- (voice-position voice-at-point) (voice-position voice-to-rest))
		       (voice-measure-beats voice-to-rest))
     do
       (if fill-fn
	   (funcall fill-fn voice-to-rest voice-at-point beats)
	   (push (make-note :beats beats :pitch nil) (voice-timeline voice-to-rest)))))

(defun n (voice pitch &optional (duration 1) tied-p articulation)
  (push (make-note :beats duration
		   :pitch pitch
		   :offset (+ *octave-offset* (voice-offset-note voice))
		   :tied-p tied-p :articulation articulation)
	(voice-timeline voice)))

(defun s (voice pitch &optional (duration 1))
  (n voice pitch duration nil "staccato"))

(defun r (voice &optional (duration 1))
  (n voice nil duration))

(defun triplet (voice pitch1 pitch2 pitch3 &optional (duration 1))
  (push (make-tuplet :notes (list pitch1 pitch2 pitch3) :offset (+ *octave-offset* (voice-offset-note voice))
		     :beats duration)
	(voice-timeline voice)))

(defmacro arrange-music (tempo voices &body body)
  "This macro implements the main Melisma DSL."
  `(render-lilypond ,tempo
		    (let* ((voices (list
				    ,@(loop for voice in voices
					 collect
					   (if (listp voice) (list 'cons (list 'quote (car voice)) (cadr voice))
					       (list 'cons (list 'quote voice)
						     (if (eq voice 'drums)
							 '(make-drums)
							 '(make-voice)))))))
			   ,@(loop for voice in voices
				for voice-name = (if (listp voice) (first voice) voice)
				collect
				  `(,voice-name (cdr (assoc ',voice-name voices)))))
		      ,@body
		      (mapcar #'cdr voices))))

;; Following two should be unnecessary, not yet deleted just in case

;; (defmacro make-music (tempo voices &body body)
;;   `(arrange-music #'play-lilypond ,tempo ,voices ,@body))

;; (defmacro do-music (tempo voices &body body)
;;   `(arrange-music (lambda (s) (format t "~a~%" s)) ,tempo ,voices ,@body))

;; (defmacro music-beats (&body body)
;;   "Given a body of music that pushes onto *default-voice*, return the number of beats used."
;;   `(let ((melody (make-voice)))
;;      (arrange-music (lambda (_)
;; 		      (declare (ignore _))
;; 		      (voice-position melody))
;; 	 120 ((*default-voice* melody)) ,@body)))

(defmacro play ((&rest voices) &body body)
  `(play-lilypond (arrange-music 120 ,voices ,@body)))

(defmacro show ((&rest voices) &body body)
  `(let ((*articulate-p* nil))
     (show-lilypond (arrange-music 120 ,voices ,@body))))

(defvar *mp3-file* "/home/eric/www/melisma-output.mp3")
(defmacro produce-mp3 (tempo voices &body body)
  (let ((lilypond-string (gensym)))
    `(let ((,lilypond-string (arrange-music #'identity ,tempo ,voices ,@body)))
       (format t "~a~%" (consume-lilypond ,lilypond-string "timidity" :midi "/tmp/produce-mp3" (list "-o" *mp3-file* "-Ov")))
       (let* ((*articulate-p* nil)
	      (,lilypond-string (arrange-music #'identity ,tempo ,voices ,@body)))
	 (consume-lilypond ,lilypond-string "ls" :pdf "/tmp/produce-mp3-clean")
	 (format t "Copy: ~a~%" (shell-show-errors "/bin/cp" "/tmp/produce-mp3-clean.pdf" "/home/eric/www/melisma-output.pdf"))))))

(defconstant /C 0)
(defconstant /D 2)
(defconstant /E 4)
(defconstant /F 5)
(defconstant /G 7)
(defconstant /A 9)
(defconstant /B 11)

(defconstant @C 1)
(defconstant @D 2)
(defconstant @E 3)
(defconstant @F 4)
(defconstant @G 5)
(defconstant @A 6)
(defconstant @B 7)

(alexandria:define-constant +drum-snare+ "sn" :test 'equal)
(alexandria:define-constant +drum-bass+ "bd" :test 'equal)
(alexandria:define-constant +drum-cym+ "crashcymbal" :test 'equal)

(alexandria:define-constant +major-degrees+
  '((1 . 0)
    (2 . 2)
    (3 . 4)
    (4 . 5)
    (5 . 7)
    (6 . 9)
    (7 . 11)
    (8 . 12))
  :test 'equal)

(defun major-degree (n)
  (cdr (assoc n +major-degrees+)))

(defconstant a-first (major-degree 1))
(defconstant a-second (major-degree 2))
(defconstant a-third (major-degree 3))
(defconstant a-fourth (major-degree 4))
(defconstant a-fifth (major-degree 5))
(defconstant a-sixth (major-degree 6))
(defconstant a-seventh (major-degree 7))

(alexandria:define-constant +minor-degrees+
  '((1 . 0)
    (2 . 2)
    (3 . 3)
    (4 . 5)
    (5 . 7)
    (6 . 8)
    (7 . 10)
    (8 . 12))
  :test 'equal)

(defun minor-degree (n)
  (cdr (assoc n +minor-degrees+)))

(defconstant i-first (minor-degree 1))
(defconstant i-second (minor-degree 2))
(defconstant i-third (minor-degree 3))
(defconstant i-fourth (minor-degree 4))
(defconstant i-fifth (minor-degree 5))
(defconstant i-sixth (minor-degree 6))
(defconstant i-seventh (minor-degree 7))

(defun typed-degree (major-or-minor n)
  (ecase major-or-minor
    (:major (major-degree n))
    (:minor (minor-degree n))))

(defun degree-from-pitch (raw-pitch major-or-minor)
  (let* ((pitch (mod raw-pitch 12))
	 (degree (car (rassoc pitch (ecase major-or-minor
				      (:major +major-degrees+)
				      (:minor +minor-degrees+))))))
    (if degree degree
	(values (car (rassoc (1- pitch) (ecase major-or-minor
					  (:major +major-degrees+)
					  (:minor +minor-degrees+))))
		:sharp))))

(defmacro voice-offset-degree ((voice degree-shift &optional (major-or-minor :major)) &body body)
  `(voice-offset-set (,voice (+ (typed-degree ,major-or-minor
					      (+ ,degree-shift
						 (degree-from-pitch (voice-offset-note ,voice) ,major-or-minor)))
				(* 12 (floor (voice-offset-note ,voice) 12))))
     ,@body))

(defmacro octave (shift &body body)
  `(let ((*octave-offset* (+ *octave-offset* ,(* shift 12))))
    ,@body))

(defun octaves (count &optional (pitch 0))
  (+ pitch (* count 12)))

(defun sharp (pitch)
  (typecase pitch
    (keyword
     (read-from-string (format nil "~sis" pitch)))
    (number
     (+ pitch 1))))

(defun flat (pitch)
  (typecase pitch
    (keyword
     (read-from-string (format nil "~ses" pitch)))
    (number
     (- pitch 1))))

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

;; (defun diatonic-chord (degree &optional (key *base-pitch*))
;;   (funcall (case degree
;; 	     ((1 4 5) #'major-chord)
;; 	     (7 #'diminished-chord)
;; 	     (t #'minor-chord))
;; 	   (+ (major-degree degree) key)))

(defun typed-chord (type root)
  (ecase type
    (:major (major-chord root))
    (:minor (minor-chord root))
    (:augmented (augmented-chord root))
    (:diminished (diminished-chord root))))

;; Example music, rather than structure
