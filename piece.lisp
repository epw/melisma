(defpackage #:piece
  (:use #:cl #:melisma))

(in-package #:piece)

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
