(defpackage #:piece
  (:use #:cl #:melisma))

(in-package #:piece)

;; It would be great to find a way to unify "phrases" and "pieces". Maybe some kind
;; of argument that lets you indicate whether to append to an existing binding of
;; melisma:*notes*?

;; As always, we've got to figure out multiple notes.

(defphrase a (&optional variation)
  (note 4 :g)
  (note 4 :b)
  (note 4 :d)
  (if (eq variation 'recap)
      (note 4 :r)
      (note 4 :g 1)))

(defphrase b ()
  (note 1 :b -1))

(defpiece piece ()
  (let ((*base-octave* 1))
    (repeat (a))
    (repeat
      (b)
      (a 'recap))
    (note 4 :d)
    (note 4 :b)
    (note 2 :g)))

(defun render ()
  (synth "piece" #'piece))
