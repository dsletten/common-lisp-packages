;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-lisp.lisp
;;;;
;;;;   Started:            Sat Nov 16 10:05:56 2024
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
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :test-lisp (:use :common-lisp :core :test))

(in-package :test-lisp)

(deftest test-s-expression-vs-form ()
  (check
   (equalelts '((+ 1 2)
                (+ 1 . (2))
                (cl:+ 1 2)
                (COMMON-LISP:+ 1 2)
                (+ 1. 2.)
                (+ 4/4 2/1)
                (+ #.(/ 3 3) #.(/ 6 3))
                (+ . (1 . (2 . nil)))
                (+ #b1 #b10)))) )

(deftest test-quote ()
  (check
   (= 2 (length (read-from-string "'(+ 1 2)")))
   (= 3 (length (read-from-string "(+ 1 2)")))
   (equal 'quote (first (read-from-string "'(+ 1 2)")))
   (equal '(+ 1 2) (second (read-from-string "'(+ 1 2)")))
   (equal '(quote (+ 1 2)) (read-from-string "'(+ 1 2)"))
   (equal '((+ 1 2) quote) (reverse (read-from-string "'(+ 1 2)")))) )

;; (sdraw (read-from-string "'a"))

;; [*|*]--->[*|*]--->NIL
;;  |        |
;;  v        v
;; QUOTE     A

;; (sdraw (read-from-string "'(+ 1 2)"))

;; [*|*]--->[*|*]--->NIL
;;  |        |
;;  v        v
;; QUOTE    [*|*]--->[*|*]--->[*|*]--->NIL
;;           |        |        |
;;           v        v        v
;;           +        1        2

;;;
;;;    TODO: Fix LOOP
;;;
;;;    p       r
;;;    - = d + -
;;;    q       q
;;;    
;;;    p = qd + r <=> r = p - qd
;;;    
;;;    d = (truncate p q) => r = (rem p/q) C/C++/Java/JavaScript/Oz %
;;;    d = (floor p q) => r = (mod p/q)    Ruby %
;;;    
(deftest test-mod-rem ()
  (loop for a in '(10 -10)
        do (loop for b in '(3 -3)
                 do (check
                     (multiple-value-bind (_ mod) (floor a b)
                       (declare (ignore _))
                       (= mod (mod a b)))
                     (multiple-value-bind (_ mod) (floor (/ a b))
                       (declare (ignore _))
                       (= (* b mod) (mod a b)))
                     (multiple-value-bind (_ rem) (truncate a b)
                       (declare (ignore _))
                       (= rem (rem a b)))
                     (multiple-value-bind (_ rem) (truncate (/ a b))
                       (declare (ignore _))
                       (= (* b rem) (rem a b)))) )))

(deftest test-symbol-name ()
  (check
   (string= "PUNG" (symbol-name 'pung))
   (string= "pUNG" (symbol-name '\pung))
   (string= "pUNG" (symbol-name '|p|ung))
   (string= "pung" (symbol-name '|pung|))
   (string= "pungFOO" (symbol-name '|pung|foo))
   (string= "|a|" (symbol-name '|\|a\||))
   (string= "\\a\\" (symbol-name '|\\a\\|))
   (= 3 (length (symbol-name '|\\a\\|)))
   (string= "a;" (symbol-name '|a\;|))
   (eq (make-symbol "a;") '|a\;|)
   (= 2 (length (symbol-name '|a\;|)))
   (string= "a\\;" (symbol-name '|a\\;|))
   (eq (make-symbol "a\\;") '|a\\;|)
   (= 3 (length (symbol-name '|a\\;|)))
   (string= "" (symbol-name '||))
   (string= "" (symbol-name (make-symbol "")))) )

(deftest test-numberp ()
  (check
   (numberp 9)
   (numberp #xFF)
   (numberp 2/3)
   (numberp pi)
   (numberp (sqrt -1))
   (not (numberp 'x))
   (not (numberp :x))
   (not (numberp #\x))
   (not (numberp "x"))
   (not (numberp t))
   (not (numberp (list 1 2)))
   (not (numberp #'(lambda (x) (+ x 5))))
   (dolist (x '(0 0d0 3/4 :x #\x "x" (x)) t)
     (check
      (equal (typep x 'number) (numberp x)))) ))

(deftest test-realp ()
  (check
   (realp 9)
   (realp #xFF)
   (realp 2/3)
   (realp pi)
   (not (realp (sqrt -1)))
   (not (realp 'x))))

(deftest test-rationalp ()
  (check
   (rationalp 9)
   (rationalp 9.)
   (rationalp #xFF)
   (rationalp 2/3)
   (rationalp 4/1)
   (rationalp 4/4)
   (not (rationalp pi))
   (not (rationalp (sqrt -1)))
   (not (rationalp 'x))))

(deftest test-integerp ()
  (check
   (integerp 9)
   (integerp 9.)
   (integerp #xFF)
   (integerp 4/1)
   (integerp 4/4)
   (not (integerp 2/3))
   (not (integerp pi))
   (not (integerp (sqrt -1)))
   (not (integerp 'x))
   (dolist (x '(0 0. 0d0 3/4 4/4 :x #\x "x" (x)) t)
     (check
      (equal (typep x 'integer) (integerp x)))) ))

(deftest test-floatp ()
  (check
   (not (floatp 9))
   (not (floatp #xFF))
   (not (floatp 2/3))
   (floatp pi)
   (floatp 3.7)
   (floatp 3.7d0)
   (floatp 3.7e0)
   (floatp 3.7f0)
   (floatp 3.7l0)
   (floatp 3.7s0)
   (not (floatp (sqrt -1)))
   (not (floatp 'x))
   (dolist (x '(0 0d0 3.7 3.7d0 3/4 :x #\x "x" (x)) t)
     (check
      (equal (typep x 'float) (floatp x)))) ))

(deftest test-zerop ()
  (check
   (zerop 0)
   (zerop 0.)
   (zerop -0)
   (zerop 0d0)
   (zerop -0d0)
   (zerop 0/5)
   (zerop -0/5)
   (not (zerop least-positive-double-float))))

(deftest test-plusp ()
  (check
   (plusp 1)
   (plusp 1/2)
   (plusp 1.)
   (plusp 1.3)
   (plusp 1.3d0)
   (plusp least-positive-double-float)
   (not (plusp 0))
   (not (plusp -9))
   (not (plusp least-negative-double-float))))

(deftest test-minusp ()
  (check
   (minusp -1)
   (minusp -1/2)
   (minusp -1.)
   (minusp -1.3)
   (minusp -1.3d0)
   (not (minusp least-positive-double-float))
   (not (minusp 0))
   (not (minusp -0))
   (not (minusp -0d0))
   (minusp least-negative-double-float)))
   
(deftest test-oddp ()
  (check
   (oddp 3)
   (oddp 3.)
   (not (oddp 2))
   (not (oddp 2.))
   (handler-case (not (oddp 2/3))
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Only applicable to integers.")))
   (handler-case (not (oddp pi))
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Only applicable to integers.")))) )

(deftest test-evenp ()
  (check
   (evenp 2)
   (evenp 2.)
   (not (evenp 3))
   (not (evenp 3.))
   (handler-case (not (evenp 2/3))
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Only applicable to integers.")))
   (handler-case (not (evenp pi))
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Only applicable to integers.")))) )

(deftest test-ratios ()
  (check
   (= 7/2 14/4)
   (= -7/2 (/ -14 4))
   (= -7/2 (/ 14 -4))
   (= 1 4/4)
   (let ((r 9/15))
     (= 1 (gcd (numerator r) (denominator r)))) ; Relatively prime
   (= 7 (numerator 14/4))
   (= 2 (denominator 14/4))
   (= 8 (numerator 8))
   (= 1 (denominator 8))
   (handler-case (denominator 8.0)
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Only applicable to RATIONAL numbers.")))) ) ; Not specified in CLHS?

(deftest test-numerical-equality ()
  (check
   (= 2 2. 4/2 2.0 2d0 2e0 2f0 2l0 2s0)
   (not (eql 2 2d0))
   (= 3 6/2 9/3 300/100)
   (= 0 -0)
   (= 0.0 -0.0)
   (not (eql 0.0 -0.0))
   (not (= 0.1d0 0.1)) ; !!
   (> 0.1 0.1d0)
   (= 0.1d0 (rational 0.1d0))
   ;; (rational 0.1) => 13421773/134217728 = 3602879755583488/36028797018963968
   ;; (rational 0.1d0) => 3602879701896397/36028797018963968
   ;; (- (rational 0.1) (rational 0.1d0)) => 53687091/36028797018963968
   (not (= 1 2 3))
   (/= 1 2 3)
   (eq (and (/= 1 2) (/= 1 3) (/= 2 3))
       (/= 1 2 3))
   (not (= 1 2 3 1))
   (not (/= 1 2 3 1))
   (eq (and (/= 1 2) (/= 1 3) (/= 1 1) (/= 2 3) (/= 2 1) (/= 3 1))
       (/= 1 2 3 1))
   (< 1 2 3 4 5)
   (not (< 1 1 2 2 3 3 4 4 5 5))
   (<= 1 1 2 2 3 3 4 4 5 5)
   (<= 1 1d0 2e0 2 3f0 3l0 4 4s0 5.0 5)
   (> 0 -1 -2 -3 -4)
   (not (> 0 0 -1 -1 -2 -2 -3 -3 -4 -4))
   (>= 0 0 -1 -1 -2 -2 -3 -3 -4 -4)))

;; (sdraw '(= 3 6/2 9/3 300/100))

;; [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;  |        |        |        |        |
;;  v        v        v        v        v
;;  =        3        3        3        3

;; (sdraw '(= 0 -0))

;; [*|*]--->[*|*]--->[*|*]--->NIL
;;  |        |        |
;;  v        v        v
;;  =        0        0

;; (sdraw '(= 0.0 -0.0))

;; [*|*]--->[*|*]--->[*|*]--->NIL
;;  |        |        |
;;  v        v        v
;;  =       0.0      -0.0

