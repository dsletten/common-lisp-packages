;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               twos-complement.lisp
;;;;
;;;;   Started:            Tue May 12 00:06:43 2020
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
;;;;   From Slade ch. 16
;;;;
;;;;

(defpackage :twos-complement
  (:use :common-lisp)
  (:export :twos-complement :binary-to-bits))

(in-package :twos-complement)

(defun twos-complement (n)
  (1+ (lognot n)))

(defun binary-to-bits (n &optional (width 8))
;; (defun binary-to-bits (n &optional (width (ceiling (log (abs n) 2))))
  (if (minusp n)
      (flip-bits (binary-to-bits (lognot n) width))
      (format nil "~V,'0B" width n)))

(defun flip-bits (s)
  (map 'string #'flip-bit s))

(defun flip-bit (ch)
  (ecase ch
    (#\0 #\1)
    (#\1 #\0)))
