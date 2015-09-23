(defpackage #:piece
  (:use #:cl #:melisma))

(in-package #:piece)

;; It would be great to find a way to unify "phrases" and "pieces". Maybe some kind
;; of argument that lets you indicate whether to append to an existing binding of
;; melisma:*notes*?

;; As always, we've got to figure out multiple notes.

(defun quarter-notes (&rest notes)
  (dolist (note notes)
    (note 4 note)))

(defun a (&optional variation)
  (quarter-notes :g :b :d)
  (if (eq variation 'recap)
      (note 4 :r)
      (note 4 :g 1)))

(defun b ()
  (note 1 :b -1))

;; (defpiece piece ()
;;   (let ((*base-octave* 1))
;;     (repeat (a))
;;     (repeat
;;       (b)
;;       (a 'recap))
;;     (note 4 :d)
;;     (note 4 :b)
;;     (note 2 :g)))

(defun treble ()
  (dotimes (i 32)
    (note 4 :c)))

(defun bass ()
  (dotimes (i 8)
    (note 1 :e -1)))

(defun piece ()
  (simul #'treble #'bass))

(defun render ()
  (synth "piece" #'piece))
