(defpackage #:melisma
  (:use #:cl #:eric)
  (:export #:*input*
	   #:make-note
	   #:note
	   #:render-lilypond
	   #:render-lilypond*
	   #:play-lilypond))

(in-package #:melisma)

(defvar *base-pitch*)

(defstruct note
  voice
  duration
  pitch)

(defun relative-note (pitch duration)
  (make-note :pitch (typecase pitch
		      (nil nil)
		      (list (mapcar (lambda (p) (+ p *base-pitch*)) pitch))
		      (t (+ pitch *base-pitch*)))
	     :duration duration))
	     
(defun octave-marks (pitch)
  (if pitch
      (let ((octave (floor pitch 12)))
	(format nil "~v@{~a~:*~}" (abs octave) (if (< octave 0) "," "'")))
      ""))

(defun render-pitch (pitch)
  (if pitch
      (nth (mod pitch 12) (list 'c 'cis 'd 'dis 'e 'f 'fis 'g 'gis 'a 'ais 'b))
      'r))

(defun render-note (note)
  (if (and (note-pitch note) (listp (note-pitch note)))
      (format nil "<~{~a~^ ~}>~a"
	      (mapcar (lambda (p)
			(format nil "~(~a~)~a" (render-pitch p) (octave-marks p)))
		      (note-pitch note))
	      (note-duration note))
      (format nil "~(~a~)~a~a" (render-pitch (note-pitch note)) (octave-marks (note-pitch note))
	      (note-duration note))))

(defun render (f notes)
  (format f "        {~%")
  (dolist (note (reverse notes))
    (format f "    ~a~%" (render-note note)))
  (format f "        }~%"))

(defun render-lilypond (instrument key tempo &rest piece)
  (with-output-to-string (s)
    (format s "\\version \"2.16.0\"
\\score {")
    (format s "
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = ~d~%" instrument key tempo)
    (format s "      <<~%")
    (let ((voices (remove-duplicates (mapcar #'note-voice piece))))
      (dolist (voice voices)
	(render s (remove-if-not (lambda (note) (eq (note-voice note) voice)) (reverse piece)))
	(unless (equal voice (car (last voices)))
	  (format s "      \\\\~%"))))
    (format s "      >>~%")
    (format s "  }
  \\layout { }
  \\midi { }
}~%")))

(defun render-lilypond* (instrument key tempo piece)
  (apply #'render-lilypond instrument key tempo piece))

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
