;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               arrays.lisp
;;;;
;;;;   Started:            Tue May 19 05:54:06 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :arrays (:use :common-lisp :test) (:shadow :+ :- :* :/)) ; This shadows REPL too!!! * ** ...

(in-package :arrays)

;;;
;;;    We lose 0-arg version...Can't dispatch on nothing...
;;;    
(defgeneric + (x &rest xs))
(defmethod + ((a array) &rest arrays)
  (if (null arrays)
      a
      (destructuring-bind (b . more) arrays
        (assert (equal (array-dimensions a) (array-dimensions b)) (a b) "Incompatible dimensions: ~A ~A" (array-dimensions a) (array-dimensions b))
        (let ((result (make-array (array-dimensions a))))
          (dotimes (i (array-total-size a))
            (setf (row-major-aref result i) (+ (row-major-aref a i) (row-major-aref b i))))
          (apply #'+ result more)))) )

(defmethod + ((x number) &rest xs)
  (if (null xs)
      x
      (destructuring-bind (y . more) xs
        (apply #'+ (cl:+ x y) more))))

(defgeneric - (x &rest xs))
(defmethod - ((a array) &rest arrays)
  (if (null arrays)
      (* -1 a)
      (destructuring-bind (b . more) arrays
        (assert (equal (array-dimensions a) (array-dimensions b)) (a b) "Incompatible dimensions: ~A ~A" (array-dimensions a) (array-dimensions b))
        (let ((result (make-array (array-dimensions a))))
          (dotimes (i (array-total-size a))
            (setf (row-major-aref result i) (- (row-major-aref a i) (row-major-aref b i))))
          (apply #'- result more)))) )

(defmethod - ((x number) &rest xs)
  (if (null xs)
      (cl:- x)
      (destructuring-bind (y . more) xs
        (apply #'- (cl:- x y) more))))

(defgeneric * (x &rest xs))
(defmethod * ((a array) &rest arrays)
  (if (null arrays)
      a
      (destructuring-bind (b . more) arrays
        (assert (and (= (array-rank a) (array-rank b) 2)
                     (= (array-dimension a 1) (array-dimension b 0)))
                (a b)
                "Cannot multiply ~S by ~S." a b)
        (let ((result (make-array (array-dimensions a))))
          (dotimes (i (array-total-size a))
            (setf (row-major-aref result i) (* (row-major-aref a i) (row-major-aref b i))))
          (apply #'* result more)))) )

(defmethod * ((x number) &rest xs)
  (if (null xs)
      x
      (destructuring-bind (y . more) xs
        (apply #'* (cl:* x y) more))))

