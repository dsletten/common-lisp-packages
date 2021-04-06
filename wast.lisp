;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               wast.lisp
;;;;
;;;;   Started:            Wed Mar 31 17:36:02 2021
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

(defpackage :wast (:use :common-lisp :test))

(in-package :wast)

(define-symbol-macro i32 (unsigned-byte 32))

(defmacro module ((func . body) (export . table))
  (let ((functions '()))
    (destructuring-bind (name &rest code) body
      (multiple-value-bind (params code) (get-params code)
        (multiple-value-bind (result code) (get-result code)
          (acons name `#'(lambda ,params ,code) functions)))) ))

(defun get-params (code)
  (labels ((params (code result)
             (if (paramp (first code))
                 (params (rest code) (cons (rest (first code)) result))
                 (values (nreverse result) code))))
    (params code '())))

(defun paramp (expr)
  (and (consp expr)
       (symbolp (first expr))
       (string= (symbol-name (first expr)) (symbol-name 'param))))

(defun get-result (code)
  (labels ((result (code result)
             (if (resultp (first code))
                 (result (rest code) (cons (rest (first code)) result))
                 (values (nreverse result) code))))
    (result code '())))

(defun resultp (expr)
  (and (consp expr)
       (symbolp (first expr))
       (string= (symbol-name (first expr)) (symbol-name 'result))))




;; (module
;;  (func $how_old (param $year_now i32) (param $year_born i32) (result i32)
;;        get_local $year_now
;;        get_local $year_born
;;        i32.sub)
;;  (export "how_old" (func $how_old)))
    
