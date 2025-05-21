;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               sql.lisp
;;;;
;;;;   Started:            Mon Apr 14 19:32:24 2025
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/strings" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/strings.lisp" :verbose nil))

(defpackage :sql
  (:use :common-lisp :strings)
  (:export :default
           :format-field :format-fields
           :not-null
           :null)
  (:shadow :null))

(in-package :sql)

(defconstant null 'null)

(defgeneric format-field (field)
  (:documentation "Format Lisp value as string for SQL value."))
(defmethod format-field ((value string)) (format nil "'~A'" (string-substitute "\\'" "'" value)))
(defmethod format-field ((value double-float))
  (let ((*read-default-float-format* 'double-float))
    (write-to-string value)))
(defmethod format-field ((value float))
  (format-field (coerce value 'double-float)))
(defmethod format-field ((value number)) (write-to-string value))
(defmethod format-field ((value (eql t))) "true")
(defmethod format-field ((value (eql nil))) "false")
(defmethod format-field ((value (eql null))) "null")

(defun format-fields (row)
  (join (mapcar #'format-field row) ", "))

(defun not-null (s)
  (join (list s "not null") " "))

(defun default (field default)
  (join (list field "default" (write-to-string default)) " "))

