;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               text-region.lisp
;;;;
;;;;   Started:            Sat Sep 12 10:42:08 2020
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
;;;;   Fix this to use displaced arrays for children!
;;;;   Child overwrites parent region that overlaps it...
;;;;
;(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :text-region (:use :common-lisp) (:shadow :write-char :write-string :write-line))

(in-package :text-region)

; UPDATEDP slot? Child needs to be redrawn...
(defclass text-region ()
  ((parent :initform nil)
   (children :initform (make-hash-table))
   (min-row :initarg :min-row)
   (min-column :initarg :min-column)
   (max-row :initarg :max-row)
   (max-column :initarg :max-column)
   (field)
   (cursor)
   (borderless :initform nil)
   (left-margin :initform 0)
   (right-margin :initform 0)
   (top-margin :initform 0)
   (bottom-margin :initform 0)))

(defmethod initialize-instance :after ((tr text-region) &rest initargs)
  (declare (ignore initargs))
  (with-slots (field cursor min-row min-column max-row max-column) tr
    (setf field (make-array (list max-row max-column) :element-type 'character :initial-element #\space)
          cursor (make-instance 'cursor :text-region tr))))

(defun make-text-region (&key (min-row 0) (min-column 0) max-row max-column)
  (make-instance 'text-region :min-row min-row :min-column min-column :max-row max-row :max-column max-column))

(defun add-child-region (p c row column)
  (validate-row p row)
  (validate-column p column)
  (with-slots (children) p
    (with-slots (parent) c
      (setf parent p
            (gethash c children) (list row column)))) )

(defun draw (text-region)
  (with-slots (field parent children min-row min-column max-row max-column) text-region
    (cond ((null parent) ; Compare CLEAR...
           (loop for child being the hash-keys in children using (hash-value offsets)
                 when (state-changed-p child)
                 do (draw-child-region text-region child))
           (loop for i from min-row below max-row
                 do (loop for j from min-column below max-column
                          do (cl:write-char (aref field i j)))
                 (terpri)))
          (t (draw-child-region parent text-region)))) )

(defun state-changed-p (text-region)
  t) ; !!!!

(defun draw-child-region (p c)
  (with-slots ((parent-field field) children) p
    (destructuring-bind (row-offset column-offset) (gethash c children)
      (with-slots ((child-field field)) c
        (destructuring-bind (rows columns) (array-dimensions child-field)
          (loop for i below rows
                do (loop for j below columns
                         do (setf (aref parent-field (+ i row-offset) (+ j column-offset)) (aref child-field i j)))) )))) )

; Write newline char -> move cursor
(defun write-char (text-region ch)
  (with-slots (field cursor) text-region
    (with-slots (row column) cursor
      (case ch
        (#\newline (newline cursor))
        (otherwise (setf (aref field row column) ch) (advance cursor)))) ))

(defun write-string (text-region s)
  (loop for ch across s
        do (write-char text-region ch)))

(defun write-line (text-region s)
  (write-string text-region s)
  (write-char text-region #\newline))

(defun validate-row (text-region row)
  (with-slots (min-row max-row) text-region
    (assert (<= min-row row (1- max-row)))) )

(defun validate-column (text-region column)
  (with-slots (min-column max-column) text-region
    (assert (<= min-column column (1- max-column)))) )

(defun fill-row (text-region row ch)
  (validate-row text-region row)
  (with-slots (min-row max-row min-column max-column) text-region
    (fill-region text-region row min-column 1 max-column ch)))

(defun fill-column (text-region column ch)
  (validate-column text-region column)
  (with-slots (min-row max-row min-column max-column) text-region
    (fill-region text-region min-row column max-row 1 ch)))

(defun fill-region (text-region i j height width ch)
  (validate-row text-region i)
  (validate-column text-region j)
  (with-slots (field min-row min-column max-row max-column) text-region
    (loop for row from i below (+ i height)
          do (loop for column from j below (+ j width)
                   do (setf (aref field row column) ch)))) )

(defun draw-border (text-region &optional (horizontal #\-) (vertical #\|))
  (with-slots (min-row min-column max-row max-column) text-region
    (fill-row text-region min-row horizontal)
    (fill-row text-region (1- max-row) horizontal)
    (fill-column text-region min-column vertical)
    (fill-column text-region (1- max-column) vertical)))

(defun clear (text-region)
  (with-slots (children min-row max-row) text-region
    (loop for child being the hash-keys in children
          do (clear child))
    (loop for row from min-row below max-row
          do (fill-row text-region row #\space))))

; min/max are inclusive ????????????
(defclass cursor ()
  ((text-region :initarg :text-region)
   (row :reader row)
   (column :reader column)))

(defmethod initialize-instance :after ((c cursor) &rest initargs)
  (declare (ignore initargs))
  (with-slots (text-region row column) c
    (with-slots (min-row min-column) text-region
      (setf row min-row
            column min-column))))

(defmethod print-object ((c cursor) stream)
  (with-slots (row column) c
    (print-unreadable-object (c stream :type t)
      (format stream "(~D ~D)" row column))))

;; (defun make-cursor (&key (min-row 0) (min-column 0) max-row max-column)
;;   (make-instance 'cursor :min-row min-row :min-column min-column :max-row max-row :max-column max-column))

(defun advance (cursor)
  (with-slots (text-region row column) cursor
    (with-slots (min-row min-column max-row max-column) text-region
      (incf column)
      (when (>= column max-column)
        (setf column min-column)
        (incf row)
        (when (>= row max-row)
          (setf row min-row)))) ))

(defun newline (cursor)
  (with-slots (text-region row column) cursor
    (with-slots (min-row min-column max-row max-column) text-region
      (setf column min-column)
      (incf row)
      (when (>= row max-row)
        (setf row min-row)))) )

(defun set-position (cursor i j)
  (with-slots (text-region row column) cursor
    (with-slots (min-row min-column max-row max-column) text-region
      (if (and (<= min-row i (1- max-row))
               (<= min-column j (1- max-column)))
          (setf row i
                column j)
          (error "Position out of range: (~D ~D)" i j)))) )

