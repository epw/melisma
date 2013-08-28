(defpackage :melisma
  (:use :cl :eric))

(in-package :melisma)

(defvar *test-cases* nil)

(defmacro deftest (name &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (setf *test-cases* (adjoin ',name *test-cases*))))

(defmacro defassert (name form value &key (test 'equals))
  `(deftest ,name
     (let ((result (funcall #',test ,form ,value)))
     (if result
	 t
	 (format t "~%Error: ~s should return ~s, got ~s~%" ',form ,value result)))))

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

(deftest test-open-bracket
  (clear-indent)
  (let ((s (make-string-output-stream)))
    (open-bracket s)
    (and (string= (get-output-stream-string s) (format nil "{~%"))
	 (= *tabbed-amount* *tab-by*))))

(defun close-bracket (stream)
  (unindent)
  (tabbed-fmt stream "}~%"))

(defun output-note (note)
  (typecase note
    (symbol (format nil "~(~a~)" note))
    (list (if (= (length note) 1) (output-note (first note))
	      (case (second note)
		(up (format nil "~(~a~)'" (output-note (second note))))
		(down (format nil "~(~a~)," (output-note (second note))))))))

(defassert output-note-a (output-note 'a) "a")
(defassert output-note-a-high (output-note '(up a)) "a'")

(defun output-notes (music stream)
  (tabbed-fmt stream "")
  (loop for note in music do (format stream "~a " (output-note note)))
  (format stream "~%"))

(defun lilypond-output (music &key (stream *standard-output*) midi-p)
  (clear-indent)
  (tabbed-fmt stream "\\version \"2.12.3\"~%")
  (tabbed-fmt stream "\\score ")
  (open-bracket stream)
  (tabbed-fmt stream "\\new Staff \\with {midiInstrument = #\"acoustic grand\"}~%")
  (open-bracket stream)
  (tabbed-fmt stream "\\tempo 4 = 120~%")

  (open-bracket stream)
  (output-notes music stream)
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

(defun run-tests ()
  (let ((results
	 (loop for test in (reverse *test-cases*) collect
	      (list test (funcall test)))))
    (format t "~s~%" results)
    (if (not (= (count t (mapcar #'second results))
		(length results)))
	(format t "~%SOME TESTS FAILED!~%")
	(format t "~%All tests passed~%"))))

(provide :melisma)
