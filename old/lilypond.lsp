(in-package :melisma/lilypond)

(defvar *tabbed-amount* 0)
(defvar *tab-by* 2)

(defun indent ()
  (incf *tabbed-amount* *tab-by*))

(defun unindent ()
  (decf *tabbed-amount* *tab-by*))

(defun clear-indent ()
  (setf *tabbed-amount* 0))

(defun tabs ()
  (let ((s (make-string-output-stream)))
    (dotimes (i *tabbed-amount*)
      (princ #\Space s))
    (get-output-stream-string s)))

(defun tabbed-fmt (stream format &rest args)
  (apply #'format stream (format nil "~a~a" (tabs) format) args))

(defun open-bracket (stream)
  (tabbed-fmt stream "{~%")
  (indent))

(defun close-bracket (stream)
  (unindent)
  (tabbed-fmt stream "}~%"))

(defun lilypond-output (music &key (stream *standard-output*) midi-p)
  (clear-indent)
  (tabbed-fmt stream "\\version \"2.12.3\"~%")
  (tabbed-fmt stream "\\score ")
  (open-bracket stream)
  (tabbed-fmt stream "\\new Staff \\with {midiInstrument = #\"acoustic grand\"}~%")
  (open-bracket stream)
  (tabbed-fmt stream "\\tempo 4 = 120~%")

  (open-bracket stream)
  (tabbed-fmt stream music)
  (close-bracket stream)

  (close-bracket stream)
  (if midi-p
      (tabbed-fmt stream "\\midi { }~%"))
  (close-bracket stream))

(defun generate (music midi-p)
  (fopen (f "/tmp/melisma.ly" :w)
    (lilypond-output music :stream f :midi-p midi-p))
  (popen "lilypond -o /tmp/melisma /tmp/melisma.ly"))

(defun play (music)
  (generate music t)
  (popen "timidity /tmp/melisma.midi"))

(defun show (music)
  (generate music nil)
  (popen "evince /tmp/melisma.pdf"))
