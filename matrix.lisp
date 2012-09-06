;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               matrix.lisp
;;;;
;;;;   Started:            Fri Jan  7 23:34:58 2011
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
;(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/lang.lisp")

(defpackage matrix
  (:use :common-lisp)
  (:export :list-to-array :print-2d-array :seq-to-array))

(in-package matrix)

(defun array-to-vectors (a)
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1)))
    (do ((result (make-array m))
         (i 0 (1+ i)))
        ((= i m) result)
      (setf (aref result i) (copy-seq (make-array n :displaced-to a :displaced-index-offset (* i n)))) )))

(defun vectors-to-array (v)
  (make-array (list (length v) (length (aref v 0))) :initial-contents v))

(defun sparse-vectors-to-array (v)
  (let* ((m (length v))
         (n (apply #'max (map 'list #'length v)))
         (result (make-array (list m n))))
    (dotimes (i m result)
      (dotimes (j (length (aref v i)))
        (setf (aref result i j) (aref (aref v i) j)))) ))

;;;
;;;    sequence -> array
;;;    See ~/programs/matrix.lisp
;;;    
(defun add-elt-to-result (elt result)
  (cons (cons elt (first result)) (rest result)))

(defun start-row (result)
  (cons '() result))

;;;
;;;    Transform list L to a list of rows of length COLUMNS. Complete as many rows as possible. Each row, except perhaps
;;;    the last, will have COLUMNS elements. The number of rows depends on the length of L.
;;;    
(defun list-to-rows-fill-rows (l columns)
  "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
  (labels ((list-to-rows-fill-rows-aux (l n)
             (cond ((endp l) (list '()))
                   ((zerop n) (start-row (list-to-rows-fill-rows-aux l columns)))
                   (t (add-elt-to-result (first l) (list-to-rows-fill-rows-aux (rest l) (1- n)))) )))
    (list-to-rows-fill-rows-aux l columns)) )

;; (defun list-to-rows-fill-rows (l columns)
;;   (do ((result (make-linked-queue))
;;        (row (make-linked-queue))
;;        (list l (rest list))
;;        (i 1 (1+ i)))
;;       ((endp list) (unless (emptyp row) (enqueue result row)) (mapcar #'elements (elements result)))
;;     (enqueue row (first list))
;;     (when (zerop (mod i columns))
;;       (enqueue result row)
;;       (setf row (make-linked-queue)))) )

;;;
;;;    Broken in CLISP
;;;    
(defun list-to-rows-fill-rows-broken? (l columns)
  (loop for take-drop = (multiple-value-list (lang:take-drop columns l)) then (multiple-value-list (lang:take-drop columns (second take-drop)))
        until (null (first take-drop))
        collect (first take-drop)))

(defun list-to-rows-fill-rows (l columns)
  (labels ((list-to-rows-fill-rows-aux (l result)
             (multiple-value-bind (take drop) (lang:take-drop columns l)
               (if (null take)
                   (nreverse result)
                   (list-to-rows-fill-rows-aux drop (cons take result)))) ))
    (list-to-rows-fill-rows-aux l '())))
;;;
;;;    Transform list L to a list of ROWS rows. Complete as many columns as possible. Each column, except perhaps
;;;    the last, will have ROWS elements. The number of columns depends on the length of L.
;;;    
(defun list-to-rows-fill-columns (l rows)
  (loop with length = (length l)
        with cols = (ceiling length rows)
        with long-rows = (if (zerop (rem length rows)) rows (rem length rows))
        for col from 1 to rows
        for start = 0 then end
        for end = (if (zerop long-rows) (1- cols) cols) then (if (> long-rows 0) (+ end cols) (+ end (1- cols)))
        collect (subseq l start end)
        when (> long-rows 0)
        do (decf long-rows)))

(defun transpose (matrix)
  (if (or (null matrix) (null (first matrix)))
      '()
      (loop for row in matrix
            until (null row)
            collect (first row) into firsts
            collect (rest row) into rests
            finally (return (cons firsts (transpose rests)))) ))

;;;
;;;    Transform list L to a list of columns of length ROWS. Complete as many columns as possible. Each column, except perhaps
;;;    the last, will have ROWS elements. The number of columns depends on the length of L. Finally transpose this list of
;;;    columns into a list of rows where the ith elements of each row constitutes the ith column.
;;;    
(defun list-to-columns-fill-columns (l rows)
  (transpose (list-to-rows-fill-rows l rows)))

;;;
;;;    Transform list L to a list of COLUMNS columns. Complete as many rows as possible. Each row, except perhaps
;;;    the last, will have COLUMNS elements. The number of rows depends on the length of L. Finally transpose this list of
;;;    columns into a list of rows where the ith elements of each row constitutes the ith column.
;;;    
(defun list-to-columns-fill-rows (l columns)
  (transpose (list-to-rows-fill-columns l columns)))

(defun seq-to-array (seq &rest keys)
  (etypecase seq
    (list (apply #'list-to-array seq keys))
    (vector (apply #'list-to-array (coerce seq 'list) keys))))

(defun list-to-array (list &key (direction :by-row) (columns 1 columns-supplied-p) (rows 1 rows-supplied-p) fill-elt)
  (let ((length (length list)))
    (cond ((not (or columns-supplied-p rows-supplied-p))
           (error "Must supply either row or column length."))
          ((and columns-supplied-p rows-supplied-p (/= length (* rows columns)))
           (error "Incompatible dimensions."))
          (t
           (ecase direction
             (:by-row (if rows-supplied-p
                          (list-of-lists-to-array (list-to-rows-fill-columns list rows) rows (ceiling length rows) fill-elt)
                          (list-of-lists-to-array (list-to-rows-fill-rows list columns) (ceiling length columns) columns fill-elt)))
             (:by-column (if columns-supplied-p
                             (list-of-lists-to-array (list-to-columns-fill-rows list columns) (ceiling length columns) columns fill-elt)
                             (list-of-lists-to-array (list-to-columns-fill-columns list rows) rows (ceiling length rows) fill-elt)))) ))))

(defun list-of-lists-to-array (lol rows cols fill-elt)
  (let ((result (make-array (list rows cols) :initial-element fill-elt)))
    (loop for i from 0 below rows
          for row in lol
          do (loop for j from 0 below cols
                   for elt in row
                   do (setf (aref result i j) elt)))
    result))

(defun print-2d-array (a &key print-nil (width 1) (align :left) (indent 0))
  (destructuring-bind (m n) (array-dimensions a)
    (let* ((elts (loop for i from 0 below (* m n)
                       collect (if (and (null (row-major-aref a i))
                                        (not print-nil))
                                   ""
                                   (format nil "~A" (row-major-aref a i)))) )
           (width (max width (+ (apply #'max (mapcar #'length elts)) 1)))
           (format-control (if (eq align :left) "~VA" "~V@A"))
           (indentation (make-string indent :initial-element #\space)))
      (loop for i from 1 upto (* m n)
            for elt in elts
            when (= (mod i n) 1)
            do (write-string indentation)
            end
            do (format t format-control width elt)
            when (zerop (mod i n))
            do (terpri)))) )
