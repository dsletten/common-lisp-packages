;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               fp.lisp
;;;;
;;;;   Started:            Tue Feb 23 19:53:23 2021
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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :fp (:use :common-lisp :test) (:shadow :map))

(in-package :fp)

;;;
;;;    Only handles single LIST, not multiple SEQUENCES!
;;;    
(defun map (f list)
  (reduce #'(lambda (elt rest)
              (cons (funcall f elt) rest))
          list
          :initial-value '()
          :from-end t))

(deftest test-map ()
  (check
   (equal (map #'1+ (loop for i from 0 to 10 collect i)) '(1 2 3 4 5 6 7 8 9 10 11))
   (equal (map #'length (list "Is" "this" "not" "pung?")) '(2 4 3 5))))

(defun filter (f list)
  (reduce #'(lambda (elt rest)
              (if (funcall f elt)
                  (cons elt rest)
                  rest))
          list
          :initial-value '()
          :from-end t))

(deftest test-filter ()
  (check
   (equal (filter #'evenp (loop for i from 0 to 10 collect i)) '(0 2 4 6 8 10))
   (equal (filter #'oddp (loop for i from 0 to 10 collect i)) '(1 3 5 7 9))
   (equal (filter #'(lambda (elt) (< elt 6)) (loop for i from 0 to 10 collect i)) '(0 1 2 3 4 5))
   (equal (filter #'(lambda (elt) (evenp (length elt))) (list "Is" "this" "not" "pung?")) '("Is" "this"))))

;;;
;;;    Initial input is the "empty" function IDENTITY. Thus, all functions can only take one argument (All Curried?)
;;;    
(defun compose (&rest fs)
  (reduce #'(lambda (f g)
              #'(lambda (x)
                  (funcall f (funcall g x))))
          fs
          :initial-value #'identity
          :from-end t))

;(funcall (compose #'sin #'(lambda (degrees) (* degrees (/ pi 180)))) 210)
