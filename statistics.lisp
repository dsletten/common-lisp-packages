;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               statistics.lisp
;;;;
;;;;   Started:            Sun Apr 26 00:16:18 2009
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
;;;;   Notes: See Knuth Vol. 2 pg. 232 regarding mean/standard deviation
;;;;
;;;;

(defpackage statistics
  (:use common-lisp)
  (:export))

(in-package statistics)

(defun riemann (x0 x1 &key (dx (/ (- x1 x0) 1d3)) f)
  (labels ((riemann-aux (x sum)
             (if (> x x1)
                 sum
                 (riemann-aux (+ x dx) (+ sum (* dx (funcall f x)))) )))
    (riemann-aux x0 0)))

(defun riemann (x0 x1 &key (dx (/ (- x1 x0) 1d3)) f)
  (do ((x x0 (+ x dx))
       (sum 0 (+ sum (* dx (funcall f x)))))
      ((> x x1) sum)))

(defun riemann (x0 x1 &key (dx (/ (- x1 x0) 1d3)) f)
  (loop for x = x0 then (+ x dx)
        while (<= x x1)
        sum (* dx (funcall f x))))

;;;
;;;    Compare the different implementations of MEAN
;;;
;;;(coerce (loop repeat 10000 collect (* (random 1.0) (expt 10 (- (random 200) 100)))) 'vector)
;;;
;;;    The Knuth versions all seem to agree. Only slightly different from the naive version.
;;;
(defun mean (vals)
  (/ (reduce #'+ vals) (length vals)))

(defun mean-knuth-reduce (vals)
  (let ((k 2))
    (reduce #'(lambda (mean val) (prog1 (+ mean (/ (- val mean) k)) (incf k)))
            vals
            :initial-value (aref vals 0)
            :start 1)))

(defun mean-knuth-loop (vals)
  (loop for k from 1
        for val across vals
        for mean = val then (+ mean (/ (- val mean) k))
        finally (return mean)))

(defun mean-knuth-do (vals)
  (do* ((i 0 (1+ i))
        (k 1 (1+ k))
        (mean (aref vals i) (+ mean (/ (- (aref vals i) mean) k)))
        (limit (1- (length vals))))
       ((= i limit) mean)))

(defun mean-knuth-recur (vals)
  (labels ((mean-knuth-aux (i k mean)
             (if (= i (length vals))
                 mean
                 (mean-knuth-aux (1+ i) (1+ k) (+ mean (/ (- (aref vals i) mean) k)))) ))
    (mean-knuth-aux 0 1 (aref vals 0))))

(defun mean-list (vals)
  (loop for val in vals
        sum val into sum
        count val into count
        finally (return (/ sum count))))

(defun mean-list (vals)
  (labels ((mean-list-aux (vals sum count)
             (if (endp vals)
                 (/ sum count)
                 (mean-list-aux (rest vals) (+ (first vals) sum) (1+ count)))) )
    (mean-list-aux vals 0 0)))

(defun harmonic-mean (vals)
  (/ (length vals) (reduce #'(lambda (sum val) (+ sum (/ val))) vals :initial-value 0)))

(defun geometric-mean (vals)
  (expt (reduce #'* vals) (/ (length vals))))

(defun median (vals)
  (aref (sort (copy-seq vals) #'<) (truncate (length vals) 2)))

(defun mode (vals)
  (do ((frequency-table (make-hash-table :test #'eql))
       (max-frequency 0)
       mode
       (i 0 (1+ i)))
      ((= i (length vals)) mode)
    (let* ((val (aref vals i))
           (frequency (1+ (gethash val frequency-table 0))))
;           (frequency (1+ (or (gethash val frequency-table) 0))))
      (setf (gethash val frequency-table) frequency)
      (when (> frequency max-frequency)
        (setf mode val
              max-frequency frequency)))) )

(defun variance (vals)
  (let ((mean (mean vals)))
    (/ (reduce #'(lambda (sum val)
                   (+ sum (expt (- val mean) 2)))
               vals
               :initial-value 0)
       (length vals))))

(defun sample-variance (vals)
  (let ((mean (mean vals)))
    (/ (reduce #'+ (map 'vector #'(lambda (val) (expt (- val mean) 2)) vals))
       (1- (length vals)))) )

(defun population-variance (vals)
  (let ((mean (mean vals)))
    (/ (reduce #'+ (map 'vector #'(lambda (val) (expt (- val mean) 2)) vals))
       (length vals))))

(defun sample-standard-deviation (vals)
  (sqrt (sample-variance vals)))

(defun population-standard-deviation (vals)
  (sqrt (population-variance vals)))

;;;
;;;    These standard deviation functions differ substantially more than the
;;;    MEAN functions do.
;;;    
(defun sigma (vals)
  (sqrt (variance vals)))

(defun sigma-knuth (vals)
  (loop for k from 1
        for val across vals
        for meank-1 = val then mean
        for mean = val then (+ mean (/ (- val mean) k))
        for s = 0 then (+ s (* (- val meank-1) (- val mean)))
        finally (return (sqrt (/ s (1- (length vals)))) )))

;; (defun sigma-knuth (vals)
;;   (do ((i 0 (1+ i))
;;        (k 1 (1+ k))
;;        (mean (aref vals 0) (+ mean (/ (- (aref vals i) mean) k)))
;;        (mean-1 0 mean)
;;        (sigma 0 (+ sigma (* (- (aref vals i) mean
;;       ((= i (length vals)) mean)))
  
(defun correlate (v1 v2)
  (let ((mean (/ (reduce #'+ (map 'vector #'* v1 v2)) (length v1)))
        (v1mean (mean v1))
        (v2mean (mean v2))
        (sv1 (sigma v1))
        (sv2 (sigma v2)))
    (/ (- mean (* v1mean v2mean)) (* sv1 sv2))))

(defun factorial (n)
  (case n
    ((0 1) 1)
    (otherwise (* n (factorial (1- n)))) ))

(defun r (x y)
  (let ((sum-x (reduce #'+ x))
        (sum-y (reduce #'+ y))
        (sum-xy (reduce #'+ (mapcar #'* x y)))
        (sum-xx (reduce #'+ (mapcar #'* x x)))
        (sum-yy (reduce #'+ (mapcar #'* y y)))
        (n (length x)))
    (print (list sum-x sum-y sum-xy sum-xx sum-yy))
    (/ (- (* n sum-xy) (* sum-x sum-y))
       (* (sqrt (- (* n sum-xx) (* sum-x sum-x)))
          (sqrt (- (* n sum-yy) (* sum-y sum-y)))) )))

(defun frequency-table (vals classes)
  (let* ((sorted-vals (sort (copy-seq vals) #'<))
         (min (elt sorted-vals 0))
         (max (elt sorted-vals (1- (length sorted-vals))))
         (range (- max min))
         (class-width (ceiling range classes)))
    (loop for class from min to max by class-width
          do (format t "[~D ~D]~%" class (+ class (1- class-width)))) ))



    