;;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;    Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;    -- Gerald Jay Sussman
;;;;
;;;;    Name:               generators.lisp
;;;;
;;;;    Started:            Thu Mar 12 10:48:46 2026
;;;;    Modifications:
;;;;
;;;;    Purpose:
;;;;
;;;;
;;;;
;;;;    Calling Sequence:
;;;;
;;;;
;;;;    Inputs:
;;;;
;;;;    Outputs:
;;;;
;;;;    Example:
;;;;
;;;;    Notes:
;;;;
;;;;
(defpackage :generators
  (:use :common-lisp)
  (:export :generator :make-generator :exhaustedp :current :next))

(in-package :generators)

;; current
;; terminate
;; next

;; persistent:
;; state


(defclass generator () ())
(defclass list-generator (generator)
  ((contents :initarg :contents)))
(defclass vector-generator (generator)
  ((contents :initarg :contents)
   (index :initform 0 :initarg :index)))

(defun make-generator (seq)
  (etypecase seq
    (list (make-instance 'list-generator :contents seq))
    (vector (make-instance 'vector-generator :contents seq))))

(defgeneric exhaustedp (generator)
  (:documentation "Has every element of a generator's sequence been consumed?"))
(defmethod exhaustedp ((g list-generator))
  (with-slots (contents) g
    (null contents)))
(defmethod exhaustedp ((g vector-generator))
  (with-slots (contents index) g
    (= index (length contents))))

(defgeneric current (generator)
  (:documentation "Retrieve current element of the generator's sequence."))
(defmethod current :around ((g generator))
  (if (exhaustedp g)
      (error "The generator has been exhausted.")
      (call-next-method)))
(defmethod current ((g list-generator))
  (with-slots (contents) g
    (first contents)))
(defmethod current ((g vector-generator))
  (with-slots (contents index) g
    (elt contents index)))

(defgeneric next (generator)
  (:documentation "Return new generator advanced to next element."))
(defmethod next :around ((g generator))
  (if (exhaustedp g)
      (error "The generator has been exhausted.")
      (call-next-method)))
(defmethod next ((g list-generator))
  (with-slots (contents) g
    (make-instance 'list-generator :contents (rest contents))))
(defmethod next ((g vector-generator))
  (with-slots (contents index) g
    (make-instance 'vector-generator :contents contents :index (1+ index))))

