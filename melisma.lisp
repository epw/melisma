(defpackage #:melisma
  (:use #:cl #:eric)
  (:export #:*input*
	   #:make-voice
	   #:make-note
	   #:note
	   #:render-lilypond
	   #:play-lilypond))

(in-package #:melisma)

(defvar *base-pitch*)
(defvar *voice*)

(defstruct voice
  (instrument "piano")
  (key "c \\major")
  (time-sig "4/4")
  (position 0)
  (notes ()))
(defun time-sig-lower (time-sig)
  (parse-integer (subseq time-sig (1+ (position #\/ time-sig)))))
(defun voice-time-sig-lower (voice)
  (time-sig-lower (voice-time-sig voice)))

(defstruct note
  (voice *voice*)
  beats
  pitch)

(defun ensure-list (atom-or-list)
  (if (listp atom-or-list) atom-or-list
      (list atom-or-list)))

(defun relative-note (pitch beats &optional (voice *voice*))
  (make-note :pitch (typecase pitch
		      (nil nil)
		      (list (mapcar (lambda (p) (+ p *base-pitch*)) pitch))
		      (t (+ pitch *base-pitch*)))
	     :beats beats
	     :voice voice))

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
	(format nil "~d" quotient)

;; Make this second part handle (6 8) and (7 8). Be smarter than subtracting 1.
	(let ((quotient (floor time-sig-lower (1- beats)))
	      (remainder (render-note-value 1 time-sig-lower)))
	  (format t "Remainder: ~a, every digit char: ~a, parsed: ~d, = quotient/2: ~a~%"
		  remainder (every #'digit-char-p remainder) (parse-integer remainder)
		  (= (parse-integer remainder) (2/ quotient)))
	  (if (and (every #'digit-char-p remainder)
		   (= (parse-integer remainder) (2* quotient)))
	      (format nil "~d." quotient)
	      (list (format nil "~d" quotient) remainder))))))
    

(defun render-duration (beats voice)
  (if (< beats (beats-remaining-in-measure voice))
      (render-note-value beats (voice-time-sig-lower voice))

		   (render-note-value (beats-remaining-in-measure voice)
				      (voice-time-sig-lower voice))
		   ...)))
      
(defun render-note (note)
   (let ((note-value (if (and (note-pitch note) (listp (note-pitch note)))
			 (mapcar (lambda (p)
				   (format nil "~(~a~)~a" (render-pitch p) (octave-marks p)))
				 (ensure-list (note-pitch note)))
			 (list (render-pitch (note-pitch note)))))
	 (durations (render-duration (note-beats note) (note-voice note))))
     (with-output-to-string (s)
       (dolist (duration durations)
	 (format s "~a~{~a~^ ~}~a~a" (if (> (length note-value) 1) "<" "") note-value (if (> (length note-value) 1) "<" "")
		 duration)))))

(defun render (f notes)
  (format f "        {~%")
  (dolist (note (reverse notes))
    (format f "    ~a~%" (render-note note)))
  (format f "        }~%"))

(defun render-lilypond (tempo &rest piece)
  (let ((piece-flattened (reverse (loop for note-or-list in piece nconc (if (listp note-or-list) note-or-list (list note-or-list))))))
    (with-output-to-string (s)
      (format s "\\version \"2.16.0\"
\\score {")
      (let ((voices (remove-duplicates (mapcar #'note-voice piece-flattened))))
	(dolist (voice voices)
	  (format s "
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = ~d~%" (voice-instrument voice) (voice-key voice) tempo)
	  (format s "      <<~%")

	  (render s (remove-if-not (lambda (note) (eq (note-voice note) voice)) piece-flattened))
	  (unless (equal voice (car (last voices)))
	    (format s "      \\\\~%"))))
      (format s "      >>~%")
      (format s "  }
  \\layout { }
  \\midi { }
}~%"))))

(defun play-lilypond (lilypond-string &optional (filename "/tmp/melisma"))
  (with-output-to-string (output)
    (if (zerop (sb-ext:process-exit-code
		(sb-ext:run-program "/usr/bin/env" (list "lilypond" "-o" filename "-")
				:input (make-string-input-stream lilypond-string)
				:output output
				:error output)))
	(shell-show-errors "timidity" (file-midi filename)))))

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

;; (defun synth (basename instrument piece)
;; ;  (write-lilypond (file-ly basename) piece instrument)
;;   (eric:fopen (f (file-ly basename))
;;     (let ((s (make-string (file-length f))))
;;       (read-sequence s f)
;;       (format t "~a" s)))
;;   (format t "Lilypond~%")
;;   (if (not (zerop (shell-show-errors "lilypond" (file-ly basename))))
;;       (return-from synth))
;;   (format t "Timidity~%")
;;   (if (not (zerop (shell-show-errors "timidity" (file-midi basename))))
;;       (return-from synth)))

(defvar *input* nil "Set this to the list of notes in music files.")

;; Example music, rather than structure

(defun major-chord (root)
  (list root (+ root 4) (+ root 7)))
(defun minor-chord (root)
  (list root (+ root 3) (+ root 7)))
(defun augmented-chord (root)
  (list root (+ root 4) (+ root 8)))
(defun diminished-chord (root)
  (list root (+ root 3) (+ root 6)))

;(defun wholes-chord (voice pitches)
;  (phrase voice 0 (relative-note pitches 1)))

;(defun chord-dur (voice pitches duration)
;  (phrase voice 0 (relative-note pitches duration)))

;; (defun experiment ()
;;   (synth "music" "acoustic grand"
;; 	 (piece (v bass)
;; 	   (chord-dur v (major-chord 12) 4)
;; 	   (chord-dur v (minor-chord 13) 4)
;; 	   (chord-dur v (minor-chord 14) 4)
;; 	   (chord-dur v (minor-chord 13) 4)
;; 	   (chord-dur v (major-chord 12) 4)

;; 	   (wholes-chord bass 24)
;; 	   (wholes-chord bass 24))))

;(experiment)


;; 	 (piece (v bass)
;; ;		   (motive v 12)
;; ;		   (motive v 14)
;; ;		   (motive v 19)
;; ;		   (motive v )
;; ;		   (motive v 12)
;; 	   (wholes-chord v (major-chord 0))
;; 		   (wholes-chord v (minor-chord 1))
;; 		   (wholes-chord v (minor-chord 2))
;; 		   (wholes-chord v (minor-chord 1))
;; 		   (wholes-chord v (major-chord 0))


;; 		   ;; (phrase v 0 (relative-note 0 4)
;; 		   ;; 	   (relative-note 0 4)
;; 		   ;; 	   (relative-note 12 4)
;; 		   ;; 	   (relative-note 12 4))
;; 		   ;; (chord-dur v (major-chord 7) 1)
;; 		   ;; (chord-dur v (major-chord 0) 1)
;; 		   ;; (chord-dur v (major-chord 5) 1)

;; 		   ;; (chord-dur v (major-chord 7) 1)
;; 		   ;; (chord-dur v (major-chord 5) 1)
;; 		   ;; (chord-dur v (major-chord 0) 1)
;; 		   ;; (format t "~s~%" v)
;; ;;		   (bass-line bass -12)
;; ;;		   (bass-line bass -10)
;; ;;		   (bass-line bass -12)
;; ;;		   (bass-line bass -12))))
;; 		   )
