;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               command-line.lisp
;;;;
;;;;   Started:            Tue Aug 23 23:53:56 2011
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
(load "/Users/dsletten/lisp/packages/shell")

(defpackage :command-line
  (:use :common-lisp :shell)
  (:export :defscript :enforce-args :read-command-line-args))

(in-package :command-line)

(defun enforce-args (name args &rest param-names)
  (if (check-args args param-names)
      t
      (print-usage name param-names)))

(defun check-args (args param-names)
  (= (length args) (length param-names)))

(defun print-usage (name param-names)
  (format *error-output* "Usage:~%~A~{ <~A>~}~%" name param-names))

(defun read-command-line-args ()
  (mapcar #'read-from-string (get-args)))

(defmacro defscript (script (&rest args) &body body)
  (let ((quoted-args (mapcar #'(lambda (arg) (list 'quote arg)) args)))
  `(when (enforce-args ',script (get-args) ,@quoted-args)
     (destructuring-bind (,@args) (read-command-line-args)
       ,@body))))
