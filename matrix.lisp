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
;;;;       This code is distilled from ~/lisp/programs/matrix.lisp, which in turn evolved from ~/lisp/books/Slade/ch07/old/matrix.lisp
;;;;
;;;;
;(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :matrix
  (:use :common-lisp :test)
  (:export :array-to-seq :array-to-vectors :list-to-array :lists-to-array :lists-to-ragged-array :print-2d-array :seq-to-array :vectors-to-array :vectors-to-ragged-array))

(in-package :matrix)

(defun array-to-vectors (a)
  "Convert a two-dimensional array of M rows and N columns into an M-element vector of vectors of length N."
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1)))
;    (do ((result (make-array m))
    (do ((result (make-sequence 'vector m))
         (i 0 (1+ i)))
        ((= i m) result)
      (setf (aref result i) (copy-seq (make-array n :displaced-to a :displaced-index-offset (* i n)))) )))

(deftest test-array-to-vectors ()
  (check
   (equalp (array-to-vectors (seq-to-array (loop for i from 1 to 12 collect i) :rows 6 :direction :by-column)) #(#(1 7) #(2 8) #(3 9) #(4 10) #(5 11) #(6 12)))
   (equalp (array-to-vectors (seq-to-array (loop for i from 1 to 12 collect i) :rows 3)) #(#(1 2 3 4) #(5 6 7 8) #(9 10 11 12)))) )

(defun array-to-lists (a)
  "Convert a two-dimensional array of M rows and N columns into an M-element list of lists of length N."
  (let ((m (array-dimension a 0))
        (n (array-dimension a 1)))
    (loop for i from 0 below m collect (loop for j from 0 below n collect (aref a i j)))) )

(deftest test-array-to-lists ()
  (check
   (equal (array-to-lists (seq-to-array (loop for i from 1 to 12 collect i) :rows 6 :direction :by-column)) '((1 7) (2 8) (3 9) (4 10) (5 11) (6 12)))
   (equal (array-to-lists (seq-to-array (loop for i from 1 to 12 collect i) :rows 3)) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))) )

(defun vectors-to-array (v)
  "Convert an M-element vector of vectors of length N into an M X N array."
  (make-array (list (length v) (length (aref v 0))) :initial-contents v))

(deftest test-vectors-to-array ()
  (check
   (equalp (vectors-to-array (array-to-vectors (seq-to-array (loop for i from 1 to 12 collect i) :rows 3))) #2A((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equalp (vectors-to-array (array-to-vectors (seq-to-array (loop for i from 1 to 12 collect i) :rows 3 :direction :by-column))) #2A((1 4 7 10) (2 5 8 11) (3 6 9 12)))) )

(defun lists-to-array (l)
  "Convert an M-element list of lists of length N into an M X N array."
  (make-array (list (length l) (length (first l))) :initial-contents l))

(deftest test-lists-to-array ()
  (check
   (equalp (lists-to-array (array-to-lists (seq-to-array (loop for i from 1 to 12 collect i) :rows 3 :direction :by-column))) #2A((1 4 7 10) (2 5 8 11) (3 6 9 12)))
   (equalp (lists-to-array (array-to-lists (seq-to-array (loop for i from 1 to 12 collect i) :rows 3))) #2A((1 2 3 4) (5 6 7 8) (9 10 11 12)))) )

(defun vectors-to-ragged-array (v &key fill-elt)
  "Convert a vector of vectors of uneven length to a ragged array."
  (let* ((m (length v))
         (n (apply #'max (map 'list #'length v)))
         (result (make-array (list m n) :initial-element fill-elt)))
    (dotimes (i m result)
      (dotimes (j (length (aref v i)))
        (setf (aref result i j) (aref (aref v i) j)))) ))

(deftest test-vectors-to-ragged-array ()
  (check
   (equalp (vectors-to-ragged-array #(#(1 2 3 4) #(5 6) #(7 8 9)) :fill-elt #\x) #2A((1 2 3 4) (5 6 #\x #\x) (7 8 9 #\x)))
   (equalp (vectors-to-ragged-array #(#(1 2 3 4) #() #(7 8 9 10 11)) :fill-elt #\x) #2A((1 2 3 4 #\x) (#\x #\x #\x #\x #\x) (7 8 9 10 11)))))

(defun lists-to-ragged-array (l &key fill-elt)
  "Convert a list of lists of uneven length to a ragged array."
  (let* ((m (length l))
         (n (apply #'max (mapcar #'length l)))
         (result (make-array (list m n) :initial-element fill-elt)))
    (loop for i from 0 below m
          for list in l
          do (loop for j from 0 below n
                   for elt in list
                   do (setf (aref result i j) elt))
          finally (return result))))

(deftest test-lists-to-ragged-array ()
  (check
   (equalp (lists-to-ragged-array '((1 2 3 4) (5 6) (7 8 9)) :fill-elt #\x) #2A((1 2 3 4) (5 6 #\x #\x) (7 8 9 #\x)))
   (equalp (lists-to-ragged-array '((1 2 3 4) () (7 8 9 10 11)) :fill-elt #\x) #2A((1 2 3 4 #\x) (#\x #\x #\x #\x #\x) (7 8 9 10 11)))) )

;;;
;;;    sequence -> array
;;;    See ~/programs/matrix.lisp
;;;    
;; (defun add-elt-to-result (elt result)
;;   (cons (cons elt (first result)) (rest result)))

;; (defun start-row (result)
;;   (cons '() result))

;;;
;;;    Transform list L to a list of rows of length COLUMNS. Complete as many rows as possible. Each row, except perhaps
;;;    the last, will have COLUMNS elements. The number of rows depends on the length of L.
;;;    
;; (defun list-to-rows-fill-rows (l columns)
;;   "Transform a one-dimensional list into a two-dimensional list of lists each containing the specified number of elements."
;;   (labels ((list-to-rows-fill-rows-aux (l n)
;;              (cond ((endp l) (list '()))
;;                    ((zerop n) (start-row (list-to-rows-fill-rows-aux l columns)))
;;                    (t (add-elt-to-result (first l) (list-to-rows-fill-rows-aux (rest l) (1- n)))) )))
;;     (list-to-rows-fill-rows-aux l columns)) )

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
;; (defun list-to-rows-fill-rows-broken? (l columns)
;;   (loop for take-drop = (multiple-value-list (lang:take-drop columns l)) then (multiple-value-list (lang:take-drop columns (second take-drop)))
;;         until (null (first take-drop))
;;         collect (first take-drop)))

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
                       for elt = (row-major-aref a i)
                       collect (if (and (null elt) (not print-nil))
                                   ""
                                   (format nil "~A" elt))))
           (width (max width (+ (apply #'max (mapcar #'length elts)) 1)))
           (format-control (ccase align
                             (:left "~VA")
                             (:right "~V@A")
                             (:center "~V<~;~A~;~>")))
           (indentation (make-string indent :initial-element #\space)))
      (loop for i from 1 upto (* m n)
            for elt in elts
            when (= (mod i n) 1)
            do (write-string indentation)
            end
            do (format t format-control width elt)
            when (zerop (mod i n))
            do (terpri)))) )

(defun array-to-seq (type a &key (direction :by-row) (key #'identity))
  (ecase direction
    (:by-row (ecase type
               (list (loop for i from 0 below (array-total-size a)
                           collect (funcall key (row-major-aref a i))))
               (vector (loop for i from 0 below (array-total-size a)
                             with result = (make-sequence 'vector (array-total-size a))
                             do (setf (aref result i) (funcall key (row-major-aref a i)))
                             finally (return result)))
               (string ;(assert (eq (array-element-type a) 'character) () "Must be an array of characters.")
                       (loop for i from 0 below (array-total-size a)
                             with result = (make-string (array-total-size a))
                             do (setf (char result i) (funcall key (row-major-aref a i)))
                             finally (return result)))) )
    (:by-column (ecase type
                  (list (loop for j from 0 below (array-dimension a 1)
                              nconc (loop for i from 0 below (array-dimension a 0)
                                          collect (funcall key (aref a i j)))) )
                  (vector (let ((result (make-sequence 'vector (array-total-size a)))
                                (k 0))
                            (dotimes (j (array-dimension a 1) result)
                              (dotimes (i (array-dimension a 0))
                                (setf (aref result k) (funcall key (aref a i j)))
                                (incf k)))) )
                  (string ;(assert (eq (array-element-type a) 'character) () "Must be an array of characters.")
                          (let ((result (make-string (array-total-size a)))
                                (k 0))
                            (dotimes (j (array-dimension a 1) result)
                              (dotimes (i (array-dimension a 0))
                                (setf (char result k) (funcall key (aref a i j)))
                                (incf k)))) )))) )

(deftest test-list-to-array ()
  (check
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :rows 5 :direction :by-row :fill-elt 0)
           #2A((1 2 3) (4 5 6) (7 8 0) (9 10 0) (11 12 0)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :columns 5 :direction :by-row :fill-elt 0)
           #2A((1 2 3 4 5) (6 7 8 9 10) (11 12 0 0 0)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :rows 5 :direction :by-column :fill-elt 0)
           #2A((1 6 11) (2 7 12) (3 8 0) (4 9 0) (5 10 0)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :columns 5 :direction :by-column :fill-elt 0)
           #2A((1 4 7 9 11) (2 5 8 10 12) (3 6 0 0 0)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :rows 4 :direction :by-row :fill-elt 0)
           #2A((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :columns 3 :direction :by-row :fill-elt 0)
           #2A((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :rows 4 :direction :by-column :fill-elt 0)
           #2A((1 5 9) (2 6 10) (3 7 11) (4 8 12)))
   (equalp (list-to-array (loop for i from 1 to 12 collect i) :columns 3 :direction :by-column :fill-elt 0)
           #2A((1 5 9) (2 6 10) (3 7 11) (4 8 12)))) )

(deftest test-seq-to-array ()
  (check
   (equalp (seq-to-array "ABCDEFGHIJKL" :rows 5 :direction :by-row :fill-elt #\space)
           #2A((#\A #\B #\C) (#\D #\E #\F) (#\G #\H #\ ) (#\I #\J #\ ) (#\K #\L #\ )))
   (equalp (seq-to-array "ABCDEFGHIJKL" :columns 5 :direction :by-row :fill-elt #\space)
           #2A((#\A #\B #\C #\D #\E) (#\F #\G #\H #\I #\J) (#\K #\L #\  #\  #\ )))
   (equalp (seq-to-array "ABCDEFGHIJKL" :rows 5 :direction :by-column :fill-elt #\space)
           #2A((#\A #\F #\K) (#\B #\G #\L) (#\C #\H #\ ) (#\D #\I #\ ) (#\E #\J #\ )))
   (equalp (seq-to-array "ABCDEFGHIJKL" :columns 5 :direction :by-column :fill-elt #\space)
           #2A((#\A #\D #\G #\I #\K) (#\B #\E #\H #\J #\L) (#\C #\F #\  #\  #\ )))
   (equalp (seq-to-array "ABCDEFGHIJKL" :rows 4 :direction :by-row :fill-elt #\space)
           #2A((#\A #\B #\C) (#\D #\E #\F) (#\G #\H #\I) (#\J #\K #\L)))
   (equalp (seq-to-array "ABCDEFGHIJKL" :columns 3 :direction :by-row :fill-elt #\space)
           #2A((#\A #\B #\C) (#\D #\E #\F) (#\G #\H #\I) (#\J #\K #\L)))
   (equalp (seq-to-array "ABCDEFGHIJKL" :rows 4 :direction :by-column :fill-elt #\space)
           #2A((#\A #\E #\I) (#\B #\F #\J) (#\C #\G #\K) (#\D #\H #\L)))
   (equalp (seq-to-array "ABCDEFGHIJKL" :columns 3 :direction :by-column :fill-elt #\space)
           #2A((#\A #\E #\I) (#\B #\F #\J) (#\C #\G #\K) (#\D #\H #\L)))) )

(defun make-random-word ()
  (let ((s (make-string (+ (random 10) 3))))
    (dotimes (i (length s))
      (setf (char s i) (random-letter)))
    (if (zerop (random 2))
        s
        (string-capitalize s))))

(defun random-letter ()
  (code-char (+ (char-code #\a) (random 26))))

;(print-2d-array (list-to-array (sort (loop for i from 1 to 33 collect (make-random-word)) #'string<) :direction :by-column :columns 8 :fill-elt ""))
;; (print-2d-array (list-to-array (sort (loop for i from 1 to 33 collect (make-random-word)) #'string<) :direction :by-column :columns 6) :width 20 :align :center)
;; (print-2d-array (list-to-array (sort (loop for i from 1 to 33 collect (make-random-word)) #'string<) :direction :by-column :columns 6) :width 20 :align :left)
;; (print-2d-array (list-to-array (sort (loop for i from 1 to 33 collect (make-random-word)) #'string<) :direction :by-column :columns 6) :width 20 :align :right)
  
(deftest test-array-to-seq ()
  (check
   (equal (array-to-seq 'list (seq-to-array "abcdefghijkl" :rows 3)) '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l))
   (equal (array-to-seq 'list (seq-to-array "abcdefghijkl" :rows 3) :key #'char-upcase) '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L))
   (equal (array-to-seq 'list (seq-to-array "abcdefghijkl" :rows 3) :direction :by-column :key #'char-upcase) '(#\A #\E #\I #\B #\F #\J #\C #\G #\K #\D #\H #\L))
   (equalp (array-to-seq 'vector (seq-to-array "abcdefghijkl" :rows 3)) #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l))
   (string= (array-to-seq 'string (seq-to-array "abcdefghijkl" :rows 3)) "abcdefghijkl")
   (string= (array-to-seq 'string (seq-to-array "abcdefghijkl" :rows 5 :fill-elt #\_)) "abcdefgh_ij_kl_")
   (equal (array-to-seq 'list (seq-to-array (loop for i from 1 to 12 collect i) :rows 3 :direction :by-column) :direction :by-column) '(1 2 3 4 5 6 7 8 9 10 11 12))
   (equalp (array-to-seq 'vector (seq-to-array (loop for i from 1 to 12 collect i) :rows 3 :direction :by-column) :direction :by-column) #(1 2 3 4 5 6 7 8 9 10 11 12))
   (equalp (array-to-seq 'vector (seq-to-array (loop for i from 1 to 12 collect i) :rows 6 :direction :by-column) :direction :by-column) #(1 2 3 4 5 6 7 8 9 10 11 12))))
