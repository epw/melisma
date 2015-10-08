(defpackage #:melisma
  (:use #:cl #:eric))

(in-package #:melisma)

(defvar *base-pitch*)

(defstruct note
  duration
  pitch)

(defstruct voice
  notes)

(defmacro piece ((&rest voices) &body body)
  (let ((voice-definitions (mapcar (lambda (v) (list v '(make-voice))) voices)))
    `(let ,voice-definitions
       ,@body
       (list ,@voices))))

(defmacro phrase (voice base &rest notes)
  `(let ((*base-pitch* ,base))
     (dolist (note (list ,@notes))
       (push note (voice-notes ,voice)))))

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

(defun write-lilypond (filename piece instrument)
  (eric:fopen (f filename :w)
    (format f "\\version \"2.16.0\"
\\score {
  \\new Staff \\with {midiInstrument = #~s}
  {
    \\key ~(~a~)
    \\tempo 4 = 240~%" instrument "g \\major")
    (format f "      <<~%")
    (dolist (voice piece)
      (render f (voice-notes voice))
      (unless (equal voice (car (last piece)))
	(format f "      \\\\~%")))
    (format f "      >>~%")
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


;; Example music, rather than structure

(defun motive (voice base-pitch)
  (phrase voice base-pitch (relative-note 0 4)
	  (relative-note 4 4); (relative-note 7 4)
	  (relative-note (list 0 4 7) 4)
	  (relative-note nil 4)))

(defun bass-line (voice base-pitch)
  (phrase voice base-pitch (relative-note nil 4) (relative-note 0 2) (relative-note 0 4)))

(defun major-chord (root)
  (list root (+ root 4) (+ root 7)))
(defun minor-chord (root)
  (list root (+ root 3) (+ root 7)))
(defun augmented-chord (root)
  (list root (+ root 4) (+ root 8)))
(defun diminished-chord (root)
  (list root (+ root 3) (+ root 6)))

(defun wholes-chord (voice pitches)
  (phrase voice 0 (relative-note pitches 1)))

(defun chord-dur (voice pitches duration)
  (phrase voice 0 (relative-note pitches duration)))

(defun experiment ()
  (synth "music" (piece (v bass)
;		   (motive v 12)
;		   (motive v 14)
;		   (motive v 19)
;		   (motive v )
;		   (motive v 12)

		   (wholes-chord v (major-chord 0))
		   (wholes-chord v (minor-chord 1))
		   (wholes-chord v (minor-chord 2))
		   (wholes-chord v (minor-chord 1))
		   (wholes-chord v (major-chord 0))


		   ;; (phrase v 0 (relative-note 0 4)
		   ;; 	   (relative-note 0 4)
		   ;; 	   (relative-note 12 4)
		   ;; 	   (relative-note 12 4))
		   ;; (chord-dur v (major-chord 7) 1)
		   ;; (chord-dur v (major-chord 0) 1)
		   ;; (chord-dur v (major-chord 5) 1)

		   ;; (chord-dur v (major-chord 7) 1)
		   ;; (chord-dur v (major-chord 5) 1)
		   ;; (chord-dur v (major-chord 0) 1)
		   ;; (format t "~s~%" v)
;;		   (bass-line bass -12)
;;		   (bass-line bass -10)
;;		   (bass-line bass -12)
;;		   (bass-line bass -12))))
		   )))

(experiment)
