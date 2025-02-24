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
;;;;   Notes: (dolist (test (remove 'test (find-tests))) (funcall test))
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
;;;    - = q + -
;;;    d       d
;;;    
;;;    p = qd + r <=> r = p - qd
;;;    
;;;    d = (truncate p q) => r = (rem p q)
;;;    d = (floor p q) => r = (mod p q)
;;;    
;;;    C/C++/Java/JavaScript/Scala/Io % is REM! (Oz mod!!)
;;;        Test even: n % 2 == 0
;;;        Test odd:  n % 2 != 0 (Not n % 2 == 1) -3 % 2 => -1
;;;
;;;    Python/Ruby % is MOD
;;;    Common Lisp/Clojure/Haskell/Prolog: mod, rem
;;;    
(deftest test-mod-rem ()
  (loop for p in '(10 -10)
        do (loop for d in '(3 -3)
                 do (check
                     (multiple-value-bind (_ r) (floor p d)
                       (declare (ignore _))
                       (= r (mod p d)))
                     (multiple-value-bind (_ r) (floor (/ p d))
                       (declare (ignore _))
                       (= (* d r) (mod p d)))
                     (multiple-value-bind (_ r) (truncate p d)
                       (declare (ignore _))
                       (= r (rem p d)))
                     (multiple-value-bind (_ r) (truncate (/ p d))
                       (declare (ignore _))
                       (= (* d r) (rem p d)))) )))

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
   (eq (intern "a;") '|a\;|)
   (= 2 (length (symbol-name '|a\;|)))
   (string= "a\\;" (symbol-name '|a\\;|))
   (eq (intern "a\\;") '|a\\;|)
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
     (type-error () t)
     (:no-error () (error "Only applicable to integers.")))
   (handler-case (not (oddp pi))
     (type-error () t)
     (:no-error () (error "Only applicable to integers.")))) )

(deftest test-evenp ()
  (check
   (evenp 2)
   (evenp 2.)
   (not (evenp 3))
   (not (evenp 3.))
   (handler-case (not (evenp 2/3))
     (type-error () t)
     (:no-error () (error "Only applicable to integers.")))
   (handler-case (not (evenp pi))
     (type-error () t)
     (:no-error () (error "Only applicable to integers.")))) )

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
     (type-error () t)
     (:no-error () (error "Only applicable to RATIONAL numbers.")))) ) ; Not specified in CLHS?

;;;
;;;    Cannot haphazardly apply Boolean algebra:
;;;    (not (> a b)) ≡ (<= a b)
;;;      but
;;;    (not (> a b c d)) ≢(<= a b c d)
;;;    
(deftest test-compare-numbers ()
  (check
   ;;    Trivial cases with single arg
   (funcall (every-pred #'= #'/= #'< #'<= #'> #'>=) 1)

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

(deftest test-contagion ()
  (check
   (typep (+ 0.1f0 0.2f0) 'single-float)
   (typep (+ 0.1f0 0.2d0) 'double-float)
   (typep (+ 0.1d0 0.2f0) 'double-float)
   (typep (+ 0f0 0.2f0) 'single-float)
   (typep (+ 0f0 0.2d0) 'double-float)
   (typep (+ 0d0 0.2f0) 'double-float)
   (typep (+ 1/10 0.2f0) 'single-float)
   (typep (+ 1/10 0.2d0) 'double-float)
   (typep (+ 0.1f0 1/5) 'single-float)
   (typep (+ 0.1d0 1/5) 'double-float)
   (typep (+ 1 0.2f0) 'single-float)
   (typep (+ 1 0.2d0) 'double-float)
   (typep (+ 0.1f0 1) 'single-float)
   (typep (+ 0.1d0 1) 'double-float)
   (typep (+ 0 0.2f0) 'single-float)
   (typep (+ 0 0.2d0) 'double-float)
   (typep (+ 0.1f0 0) 'single-float)
   (typep (+ 0.1d0 0) 'double-float)
   (typep (+ 0.5 -0.5 1/2) 'single-float))) ; https://www.lispworks.com/documentation/HyperSpec/Body/12_adaa.htm

(deftest test-float ()
  (check
   (= 0.1f0 (float 1/10))
   (= 0.1f0 (float 1/10 1f0))
   (= (coerce 1/10 'single-float) (float 1/10))
   (= (coerce 1/10 'double-float) (float 1/10 1d0))
   (let ((x 0.1f0)) ; CLHS
     (= x (float (rational x) x)))
   (let ((x 0.1d0))
     (= x (float (rational x) x)))
   (let ((x 0.1f0))
     (= x (float (rationalize x) x)))
   (let ((x 0.1d0))
     (= x (float (rationalize x) x)))) )

;; RATIONAL
;; RATIONALIZE

(deftest test-coerce ()
  (check
   (= 0.1s0 (coerce 1/10 'short-float))
   (= 0.1f0 (coerce 1/10 'single-float))
   (= 0.1d0 (coerce 1/10 'double-float))
   (= 0.1l0 (coerce 1/10 'long-float))
   (handler-case (coerce 2 'bignum)
     (type-error () t)
     (:no-error () (error "Can't make integer something that it's not.")))
   (handler-case (coerce (expt 3 9999) 'fixnum) ; caught STYLE-WARNING: Lisp error during constant folding:
                                        ; 5437833951142086247677... can't be converted to type FIXNUM.
     (type-error () t)
     (:no-error () (error "Can't make integer something that it's not.")))) )

;;  TODO: Fix LOOP
(deftest test-integer-coercion ()
  (check
   (let ((p 9)
         (q 4))
     (and (equal (multiple-value-list (truncate p q))
                 (multiple-value-list (floor p q)))
          (equal (multiple-value-list (truncate (/ p q)))
                 (multiple-value-list (floor (/ p q)))) ))
   (let ((p -9)
         (q 4))
     (and (equal (multiple-value-list (truncate p q))
                 (multiple-value-list (ceiling p q)))
          (equal (multiple-value-list (truncate (/ p q)))
                 (multiple-value-list (ceiling (/ p q)))) ))
   (= 0 (round 0.1d0))
   (= 0 (round 0.5d0))
   (= 1 (round 0.8d0))
   (= 1 (round 1d0))
   (= 2 (round 1.5d0))
   (= 2 (round 2.5d0))
   (loop for p in '(10 -10)
         do (loop for d in '(3 -3)
                  do (loop for f in (list #'ceiling #'floor #'truncate #'round)
                           do (check
                               (multiple-value-bind (q r) (funcall f p d)
                                 (and (= p (+ (* q d) r))
                                      (integerp q)
                                      (integerp r)))
                               (multiple-value-bind (q r) (funcall f (/ p d))
                                 (and (= (/ p d) (+ q r))
                                      (integerp q)
                                      (typep r 'ratio)))) )))
   (loop for p in '(10d0 -10d0)
         do (loop for d in '(3d0 -3d0)
                  do (loop for f in (list #'ceiling #'floor #'truncate #'round)
                           do (check
                               (multiple-value-bind (q r) (funcall f p d)
                                 (and (= p (+ (* q d) r))
                                      (integerp q)
                                      (floatp r)))
                               (multiple-value-bind (q r) (funcall f (/ p d))
                                 (and (= (/ p d) (+ q r))
                                      (integerp q)
                                      (floatp r)))) )))
   (let ((xs (loop for x from -100 upto 100 by 0.1d0 collect x)))
     (every (every-pred #'(lambda (x) (= (- (floor x)) (ceiling (- x))))
                        #'(lambda (x) (zerop (+ (ceiling x) (floor (- x)))))
                        #'(lambda (x) (= (truncate x) (* (signum x) (floor (abs x)))) )
                        #'(lambda (x) (= (truncate (- x)) (- (truncate x))))
                        #'(lambda (x) (= (truncate x) (- x (rem x 1)))) )
            xs))
   (let ((xs (loop for x from -100 upto 100 by 0.1d0 collect x)))
     (every (every-pred #'(lambda (x) (= (floor x) (ffloor x)))
                        #'(lambda (x) (= (ceiling x) (fceiling x)))
                        #'(lambda (x) (= (truncate x) (ftruncate x)))
                        #'(lambda (x) (= (round x) (fround x))))
            xs))))

;;;
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_signum.htm
;;;    (signum x) ≡ (if (zerop x) x (/ x (abs x)))
;;;    
(deftest test-signum ()
  (check
   (eql 1 (signum 8))
   (eql 0 (signum 0))
   (eql -1 (signum -3))
   (eql 1d0 (signum 8d0))
   (eql 0d0 (signum 0d0))
   (eql -1d0 (signum -3d0))
   (= 0d0 (signum -0d0))
   (string= "-0.0" (format nil "~F" (signum -0d0)))) )

(deftest test-cond ()
  (check
   (not (cond))
   (equal '(:foo) (multiple-value-list (cond ((read-from-string "" nil :foo)))) )
   (equal '(:foo 0) (multiple-value-list (cond (t (read-from-string "" nil :foo)))) )
   (flet ((gtest (x y) ; Touretzky ex. 4.21
            (cond ((> x y) t)
                  ((zerop x) t)
                  ((zerop y) t)
                  (t nil))))
     (check
      (gtest 9 4)
      (gtest 9d0 4d0)
      (gtest 9 4d0)
      (not (gtest 4 9))
      (gtest 9 0)
      (gtest 0 4)
      (gtest 0d0 0d0)))
   (flet ((gtest (x y)
            (cond ((> x y)) ; Predicate is result
                  ((zerop x))
                  ((zerop y))
                  (t nil))))
     (check
      (gtest 9 4)
      (gtest 9d0 4d0)
      (gtest 9 4d0)
      (not (gtest 4 9))
      (gtest 9 0)
      (gtest 0 4)
      (gtest 0d0 0d0)))) )

(deftest test-and ()
  (check
   (and)
   (and t)
   (and t t)
   (not (and t nil))
   (not (and nil t))
   (not (and nil nil))
   ;;    Short-circuit evaluation
   (let ((pung "pung")
         (foo "foo"))
     (and (string= pung
                   (with-output-to-string (s)
                     (and (format s pung)
                          (format s foo))))
          (string= (concatenate 'string pung foo)
                   (with-output-to-string (s)
                     (and (progn (format s pung) t)
                          (format s foo)))) ))
   ;;    Multiple values only from final form
   (equal '(nil 0) (multiple-value-list (and (read-from-string "" nil nil))))
   (equal '(nil) (multiple-value-list (and (read-from-string "" nil nil) t)))) )

(deftest test-or ()
  (check
   (not (or))
   (or t)
   (or t t)
   (or t nil)
   (or nil t)
   (not (or nil nil))
   ;;    Short-circuit evaluation
   (let ((pung "pung")
         (foo "foo"))
     (and (string= pung
                   (with-output-to-string (s)
                     (or (progn (format s pung) t)
                         (format s foo))))
          (string= (concatenate 'string pung foo)
                   (with-output-to-string (s)
                     (or (format s pung)
                         (format s foo)))) ))
   ;;    Multiple values only from final form
   (equal '(2 2) (multiple-value-list (or (floor 8 3))))
   (equal '(2) (multiple-value-list (or (floor 8 3) t)))) )

;;;
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/v_most_p.htm
;;;    
(deftest test-bignum ()
  (check
   (typep most-positive-fixnum 'fixnum)
   (typep (1+ most-positive-fixnum) 'bignum)
   (typep most-negative-fixnum 'fixnum)
   (typep (1- most-negative-fixnum) 'bignum)
   (and (>= most-positive-fixnum (1- (expt 2 15)))
        (>= most-positive-fixnum array-dimension-limit))
   (<= most-negative-fixnum (- (expt 2 15)))) )

;;;
;;;    https://docs.oracle.com/cd/E60778_01/html/E60763/index.html
;;;    
(deftest test-float-overflow ()
  (check
   (handler-case (/ 3 0)
     (division-by-zero () t)
     (:no-error () (error "Since when is this allowed?")))
   (handler-case (/ 3d0 0d0)
     (division-by-zero () t)
     (:no-error () (error "Since when is this allowed?")))
   (handler-case (/ most-positive-double-float least-positive-double-float)
     (floating-point-overflow () t)
     (:no-error () (error "Less is more.")))
   (handler-case (* most-positive-double-float 10d0)
     (floating-point-overflow () t)
     (:no-error () (error "Less is more.")))
   (zerop (/ least-positive-double-float most-positive-double-float)) ; Underflow???
   (zerop (expt 10d0 -500))))

(deftest test-float-encoding ()
  (check
   (= 24 (float-digits 1f0)) ; Includes "hidden" bit. H.O. bit assumed to be 1
   (= 53 (float-digits 1d0))
   (let ((x 123.456f0)) ; CLHS
     (multiple-value-bind (significand exponent sign) (integer-decode-float x)
       (= x (* (scale-float (float significand x) exponent) sign))))
   (let ((x 123.456d0))
     (multiple-value-bind (significand exponent sign) (integer-decode-float x)
       (= x (* (scale-float (float significand x) exponent) sign))))
   (multiple-value-bind (significand exponent sign) (integer-decode-float most-positive-single-float)
     (let ((biased-exponent (+ exponent 23)))
       (and (= 1 sign)
            (= 127 biased-exponent)
            (= most-positive-single-float
               (coerce (/ (* significand (expt 2 biased-exponent)) (expt 2 23)) 'single-float)))) )
   (multiple-value-bind (significand exponent sign) (integer-decode-float most-positive-double-float)
     (let ((biased-exponent (+ exponent 52)))
       (and (= 1 sign)
            (= 1023 biased-exponent)
            (= most-positive-double-float
               (coerce (/ (* significand (expt 2 biased-exponent)) (expt 2 52)) 'double-float)))) )))

(deftest test-complex ()
  (check
   (not (complexp 2))
   (not (complexp (complex 2)))
   (not (complexp (complex 2 0)))
   (not (complexp (complex 2/3 0)))
   (complexp (complex 2d0))
   (complexp (complex 2d0 0))
   (complexp (sqrt -1))
   (complexp (sqrt (sqrt -1)))
   (= (complex 0 1) (sqrt -1))
   (complexp (* (sqrt -1) (sqrt -1)))
   (not (complexp (* (complex 0 1) (complex 0 1))))
   (let ((c (complex 1/2 1/3)))
     (and (rationalp (realpart c))
          (rationalp (imagpart c))))
   (let ((c (complex 0.5d0 1/3)))
     (and (floatp (realpart c))
          (floatp (imagpart c))))
   (= 8 (conjugate 8))
   (let* ((a 9)
          (b 10)
          (c (complex a b)))
     (= (complex (realpart c) (- (imagpart c))) (conjugate c)))
   (= 5 (abs #c(3 4)))
   (= 13 (abs #c(12 5)))) )

(deftest test-cons ()
  (check
   (let ((unique1 (cons 'a 'b))
         (unique2 (cons 'a 'b)))
     (and (eq unique1 (first (cons unique1 unique2)))
          (eq unique2 (rest (cons unique1 unique2)))) )
   (let ((l '(a b c d)))
     (equal l (cons (first l) (rest l))))
   (eq (car '()) (car '(())))
   (eq (cdr '()) (cdr '(())))
   (and (null '()) (consp '(()))) ))

(deftest test-identity ()
  (check
   (every #'(lambda (elt) (eql elt (identity elt)))
          '(a :bar 1 2d0 #\k "foo" (a b c) #(1 2)))
   (every #'(lambda (elt) (eq elt (identity elt)))
          '(a :bar "foo" (a b c) #(1 2)))) )

(deftest test-atom ()
  (check
   (let ((atoms '(x 1 #\g "foo" () #(1 2 3))))
     (every #'(lambda (obj) (and (atom obj)
                                 (typep obj 'atom)
                                 (not (consp obj))
                                 (not (typep obj 'cons))
                                 (typep obj '(not cons))))
            atoms)
     (notany #'(lambda (obj) (and (consp obj)
                                  (typep obj 'cons)
                                  (not (atom obj))
                                  (not (typep obj 'atom))
                                  (typep obj '(not atom))))
             atoms))))

(deftest test-consp ()
  (check
   (let ((conses '((a) (()) (a b c d) (a . b))))
     (every #'(lambda (obj) (and (consp obj)
                                 (typep obj 'cons)
                                 (not (atom obj))
                                 (not (typep obj 'atom))
                                 (typep obj '(not atom))))
            conses)
     (notany #'(lambda (obj) (and (atom obj)
                                  (typep obj 'atom)
                                  (not (consp obj))
                                  (not (typep obj 'cons))
                                  (typep obj '(not cons))))
             conses))))

(deftest test-listp ()
  (check
   (let ((lists '(() (a) (()) (a b c d) (a . b) (a b c . d))))
     (every #'(lambda (obj) (and (listp obj)
                                 (typep obj 'list)
                                 (typep obj '(or cons null))
                                 (or (consp obj) (null obj))))
            lists))))

(deftest test-symbolp ()
  (check
   (symbolp 'a)
   (symbolp '+)
   (symbolp (intern "FOO"))
   (symbolp (make-symbol "BAR"))
   (symbolp (gensym))
   (symbolp :pung)
   (not (symbolp "FOO"))
   (not (symbolp #\x))))

(deftest test-endp ()
  (check
   (endp '())
   (not (endp '(a)))
   (not (endp '(a b c d)))
   (not (endp '(a . b)))
   (handler-case (endp (cdr '(a . b)))
     (type-error () t)
     (:no-error () (error "Value is not a list.")))) )

;;;
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm#eq
;;;    An implementation is permitted to make ``copies'' of characters and numbers
;;;    at any time. The effect is that Common Lisp makes no guarantee that eq is true
;;;    even when both its arguments are ``the same thing'' if that thing is a character
;;;    or number.
;;;    
;; (deftest test-equality ()
;;   (check
;;    (not (eq 1d0 1d0))
;;    (eq :bar (if (eq 1d0 1d0) :foo :bar))
;;    (not (funcall #'eq 1d0 1d0))
;;    (not (apply #'eq '(1d0 1d0)))
;;    (not (funcall #'(lambda (a b) (eq a b)) 1d0 1d0))
;;    ((lambda (a b) (eq a b)) 1d0 1d0)
;;    (flet ((foo (a b) (eq a b))) (funcall #'(lambda (a b) (foo a b)) 1d0 1d0))
;;    (flet ((foo (a b) (eq a b))) (funcall #'foo 1d0 1d0))
;;    (equal '(t) (multiple-value-list (eq 1d0 1d0)))
;;    (equal '(t) (multiple-value-list (funcall #'eq 1d0 1d0)))
;;    (values-list (multiple-value-list (eq 1d0 1d0)))
;;    (values-list (multiple-value-list (funcall #'eq 1d0 1d0)))
;;    (equal '(1 2 3 4 5 6 7 8 9 10)
;;           (loop for i from 1 to 10 when (eq 1d0 1d0) collect i))
;;    (equal '(nil) (funcall (juxtapose #'eq #'eql #'equal #'equalp) 1d0 1d0))
;;    (not (and (eq 1d0 1d0) t))
;;    (or (eq 1d0 1d0) nil)
;;    (let ((boolean (eq 1d0 1d0)))
;;      boolean)))

;; (check (eq 1d0 1d0))

(deftest test-compare-characters ()
  (check
   ;;    Trivial cases with single arg
   (funcall (every-pred #'char=
                        #'char/=
                        #'char<
                        #'char<=
                        #'char>
                        #'char>=
                        #'char-equal
                        #'char-not-equal
                        #'char-lessp
                        #'char-greaterp
                        #'char-not-lessp
                        #'char-not-greaterp)
            #\a)   

   (char= #\a #\a)
   (char= #\a #\a #\a #\a)
   (not (char= #\a #\A))
   (char/= #\a #\A)

   (char-equal #\a #\A)
   (char-equal #\a #\A #\A #\a)
   (not (char-equal #\a #\B))
   (char-not-equal #\a #\B)

   (not (char= #\a #\b #\c))
   (char/= #\a #\b #\c)
   (eq (and (char/= #\a #\b) (char/= #\a #\c) (char/= #\b #\c))
       (char/= #\a #\b #\c))

   (not (char= #\a #\b #\c #\a))
   (not (char/= #\a #\b #\c #\a))
   (eq (and (char/= #\a #\b)
            (char/= #\a #\c)
            (char/= #\a #\a)
            (char/= #\b #\c)
            (char/= #\b #\a)
            (char/= #\c #\a))
       (char/= #\a #\b #\c #\a))

   (char< #\a #\b #\c #\d #\e)
   (not (char< #\a #\a #\b #\b #\c #\c #\d #\d #\e #\e))
   (char<= #\a #\a #\b #\b #\c #\c #\d #\d #\e #\e)
   (char> #\z #\y #\x #\w #\v)
   (not (char> #\z #\z #\y #\y #\x #\x #\w #\w #\v #\v))
   (char>= #\z #\z #\y #\y #\x #\x #\w #\w #\v #\v)

   (not (char-equal #\a #\B #\c))
   (char-not-equal #\a #\B #\c)
   (eq (and (char-not-equal #\a #\B) (char-not-equal #\a #\c) (char-not-equal #\B #\c))
       (char-not-equal #\a #\B #\c))

   (not (char-equal #\A #\b #\c #\a))
   (not (char-not-equal #\A #\b #\c #\a))
   (eq (and (char-not-equal #\A #\b)
            (char-not-equal #\A #\c)
            (char-not-equal #\A #\a)
            (char-not-equal #\b #\c)
            (char-not-equal #\b #\a)
            (char-not-equal #\c #\a))
       (char-not-equal #\A #\b #\c #\a))

   (char-lessp #\a #\b #\C #\d #\E)
   (not (char-lessp #\a #\a #\B #\b #\c #\C #\D #\D #\e #\e))
   (char-not-greaterp #\a #\a #\b #\b #\C #\c #\D #\D #\E #\e)
   (char-greaterp #\z #\y #\X #\W #\v)
   (not (char-greaterp #\Z #\z #\Y #\y #\X #\x #\w #\w #\v #\v))
   (char-not-lessp #\z #\z #\Y #\y #\x #\X #\w #\W #\v #\v)))

;;;
;;;    All binary predicates
;;;    
(deftest test-compare-strings ()
  (check
   (string= "pung" "pung")
   (string= "pung" "Is this not pung?" :start2 12 :end2 16)
   (not (string= "pung" "PUNG"))
   (string/= "pung" "PUNG")

   (string-equal "pung" "PUNG")
   (string-equal "Is this not pung?" "PUNG" :start1 12 :end1 16)
   (not (string-equal "pung" "foo"))
   (string-not-equal "pung" "foo")

   (string< "foo" "pung")
   (string> "pung" "foo")
   (not (string< "pung" "pung"))
   (string>= "pung" "pung")
   (not (string> "pung" "pung"))
   (string<= "pung" "pung")

   (string-lessp "FOO" "pung")
   (string-greaterp "PUNG" "Foo")
   (not (string-lessp "Pung" "pUNG"))
   (string-not-lessp "Pung" "pUNG"))
   (not (string-greaterp "Pung" "PUNG"))
   (string-not-greaterp "Pung" "PUNG"))

(deftest test-tailp ()
  (check
   (let* ((l1 (list 1 2 3 4 5))
          (l2 (copy-list l1)))
     (and (totally (maplist (partial* #'tailp l1) l1))
          (as-if (maplist (partial* #'tailp l1) l2))))
   (tailp 'c '(a b . c))))

(deftest test-ldiff ()
  (check
   (let* ((l1 (list 1 2 3 4 5))
          (l2 (copy-list l1)))
     (and (every (partial #'equal l1)
                 (maplist #'(lambda (l)
                              (append (ldiff l1 l) l))
                          l1))
          (notany (partial #'equal l1)
                  (maplist #'(lambda (l)
                               (append (ldiff l2 l) l))
                           l1))))
   (let ((l (list* 1 2 3)))
     (and (equal '(1 2) (ldiff l 3))
          (equal '(1) (ldiff l (cdr l)))
          (equal '() (ldiff l l))))
   (not (eq #1='(a b c d) (ldiff #1# '(x)))) ; LDIFF returns copy
   (equal #2='(a b c d) (ldiff #2# '(x)))) )

;;    I ain't typing this all myself...
;; (loop for i from 2 to 10 do (format t "(eq (~:R #1#) (nth ~D #1#))~%" i (1- i)))
;; (loop for i from 1 to 10 do (format t "(eq (nth ~D #1#) (elt #1# ~:*~D))~%" (1- i)))
(deftest test-indexed-list-access ()
  (check
   (eq '() (nth 0 '()))
   (eq 'a (nth 0 '(a b c d)))
   (eq 'b (nth 1 '(a b c d)))
   (eq 'd (nth 3 '(a b c d)))
   (eq '() (nth 10 '(a b c d)))

   (eq (first #1='(1 2 3 4 5 6 7 8 9 10)) (nth 0 #1#))
   (eq (second #1#) (nth 1 #1#))
   (eq (third #1#) (nth 2 #1#))
   (eq (fourth #1#) (nth 3 #1#))
   (eq (fifth #1#) (nth 4 #1#))
   (eq (sixth #1#) (nth 5 #1#))
   (eq (seventh #1#) (nth 6 #1#))
   (eq (eighth #1#) (nth 7 #1#))
   (eq (ninth #1#) (nth 8 #1#))
   (eq (tenth #1#) (nth 9 #1#))

   (eq (car #1#) (first #1#))

   (eq (car #1#) (nth 0 #1#))
   (eq (cadr #1#) (nth 1 #1#))
   (eq (caddr #1#) (nth 2 #1#))
   (eq (cadddr #1#) (nth 3 #1#))

   (eq (cadr #1#) (car (cdr #1#)))
   (eq (caddr #1#) (car (cdr (cdr #1#))))
   (eq (cadddr #1#) (car (cdr (cdr (cdr #1#)))) )

   (eq (nth 0 #1#) (elt #1# 0))
   (eq (nth 1 #1#) (elt #1# 1))
   (eq (nth 2 #1#) (elt #1# 2))
   (eq (nth 3 #1#) (elt #1# 3))
   (eq (nth 4 #1#) (elt #1# 4))
   (eq (nth 5 #1#) (elt #1# 5))
   (eq (nth 6 #1#) (elt #1# 6))
   (eq (nth 7 #1#) (elt #1# 7))
   (eq (nth 8 #1#) (elt #1# 8))
   (eq (nth 9 #1#) (elt #1# 9))

   (let ((l (list 1 2 3 4 5)))
     (setf (nth 0 l) :one)
     (setf (cadr l) :two)
     (setf (third l) :three)
     (setf (elt l 3) :four)
     (equal '(:one :two :three :four 5) l))))

(deftest test-tails ()
  (check
   (eq '() (cdr '()))
   (eq '() (rest '()))
   (eq '() (nthcdr 0 '()))
   (eq '() (nthcdr 1 '()))
   (eq '() (nthcdr 2 '()))

   (equal (rest #1='(a b c d e)) (cdr #1#))
   (equal (rest #1#) (nthcdr 1 #1#))
   (equal (cdr #1#) (nthcdr 1 #1#))
   (equal (cddr #1#) (nthcdr 2 #1#))
   (equal (cdddr #1#) (nthcdr 3 #1#))
   (equal (cddddr #1#) (nthcdr 4 #1#))
   (equal (cdr (cddddr #1#)) (nthcdr 5 #1#))

   (let* ((l (loop for i from 1 to 10 collect i))
          (tails1 (maplist #'identity l))
          (tails2 (loop for i below (length l) collect (nthcdr i l)))
          (tails3 (loop for cons on l collect cons)))
     (and (equal tails1 tails2)
          (equal tails2 tails3)))

   (let* ((l1 (list 1 2))
          (l2 (copy-list l1))
          (l3 (copy-list l1)))
     (setf (cdr l1) '(:two))
     (setf (rest l2) '(:two))
     (handler-case (setf (nthcdr 1 l3) '(:two))
       (undefined-function () t)
       (:no-error () (error "#'(SETF NTHCDR) is undefined.")))
     (equal l1 l2))))

(deftest test-last ()
  (check
   (equal '(c) (last #1='(a b c)))
   (equal '(y . z) (last #2='(x y . z)))
   (equal '() (last #1# 0))
   (equal 'z (last #2# 0))

   (equal (last #1# 1) (last #1#))

   (equal '(b c) (last #1# 2))
   (equal #1# (last #1# 3))
   (equal #1# (last #1# 4))

   (equal (nthcdr (1- (length #1#)) #1#) (last #1#))
   (equal (subseq #1# (1- (length #1#))) (last #1#))))

(deftest test-butlast ()
  (check
   (equal '(a b) (butlast #1='(a b c)))
   (equal '(x) (butlast #2='(x y . z)))

   (equal (butlast #1# 1) (butlast #1#))

   (equal '(a) (butlast #1# 2))
   (equal '() (butlast #1# 3))
   (equal '() (butlast #1# 4))

   (equal (ldiff #1# (last #1#)) (butlast #1#))
   (equal (ldiff #1# (last #1# 2)) (butlast #1# 2))

   (equal (subseq #1# 0 (1- (length #1#))) (butlast #1#))

   (equal (mapcar #'(lambda (elt _) (declare (ignore _)) elt) #1# (nthcdr 1 #1#))
          (butlast #1#))
   (equal (mapcar #'(lambda (elt _) (declare (ignore _)) elt) #1# (nthcdr 2 #1#))
          (butlast #1# 2))))

(deftest test-too-clever ()
  (check
   (null (setf))
   (null (cond))
   (null (or))
   (null (list))
   (null (progn))
   (null (append))))

(deftest test-list* ()
  (check
   ;; CLHS
   (dolist (x '(0 0d0 3.7 3.7d0 3/4 :x #\x "x" (x)) t)
     (check
      (eq x (list* x))))
   (equal (cons 1 (cons 2 (cons 3 (cons 4 '()))) ) (list* 1 2 3 4 '()))
   (equal (list 1 2 3 4) (list* 1 2 3 4 '()))
   (equal (list 1 2 3 4) (list* 1 2 3 '(4)))
   (equal (list 1 2 3 4) (list* 1 2 '(3 4)))
   (equal (list 1 2 3 4) (list* 1 '(2 3 4)))
   (equal (list 1 2 3 4) (list* '(1 2 3 4)))
   (equal (cons 3 4) (list* 3 4))
   (equal (cons 2 (cons 3 4)) (list* 2 3 4))
   (equal (cons 1 (cons 2 (cons 3 4))) (list* 1 2 3 4))))

(deftest test-append ()
  (check
   (null (append))
   (null (append '()))
   ;; CLHS
   (dolist (x '(0 0d0 3.7 3.7d0 3/4 :x #\x "x" (x)) t)
     (check
      (and (eq x (append x))
           (eq x (append '() '() '() '() x)))) )
   (equal '(a b c d e) (append '() '(a b c d e)))
   (equal '(a b c d e) (append '(a) '(b c d e)))
   (equal '(a b c d e) (append '(a b) '(c d e)))
   (equal '(a b c d e) (append '(a b c) '(d e)))
   (equal '(a b c d e) (append '(a b c d) '(e)))
   (equal '(a b c d e) (append '(a b c d e) '()))
   (equal '(a b c :d :e 7 2 3) (append '(a b c) '(:d :e) '(7 2 3)))
   (equal '(a b c . d) (append '(a) '(b) '(c) 'd))
   (let ((l1 '(1 2 3))
         (l2 '(4 5)))
     (eq l2 (nthcdr (length l1) (append l1 l2)))) ))

(deftest test-revappend ()
  (check
   (equal '(4 3 2 1 5 6 7 8) (revappend '(1 2 3 4) '(5 6 7 8)))
   (equal (reverse #1='(a b c d e f)) (revappend #1# '()))) )

(deftest test-make-list ()
  (check
   (null (make-list 0))
   (let ((n 3))
     (= n (length (make-list n))))
   (let ((obj 'pung))
     (every (partial #'eq obj) (make-list 5 :initial-element obj)))) )

(deftest test-copy-list ()
  (check
   (let ((l (list 1 2 3 4 5)))
     (and (equal l (copy-list l))
          (not (eq l (copy-list l))) ; Lists are different
          (every #'eq l (copy-list l)))) )) ; Elements are the same

(deftest test-length ()
  (check
   (equal 0 (length '()))
   (equal 1 (length '(a)))
   (equal 10 (length (loop for i from 1 to 10 collect i)))
   (equal 0 (length (vector)))
   (equal 1 (length (vector 1)))
   (equal 10 (length (apply #'vector (loop for i from 1 to 10 collect i))))
   (equal 0 (length ""))
   (equal 1 (length "M"))
   (equal 17 (length "Is this not pung?"))))

(deftest test-reverse ()
  (check
   (equal '() (reverse '()))
   (equal '(a) (reverse '(a)))
   (equal (loop for i from 10 downto 1 collect i)
          (reverse (loop for i from 1 to 10 collect i)))
   (equals #() (reverse (vector)))
   (equals #(1) (reverse (vector 1)))
   (equals (coerce (loop for i from 10 downto 1 collect i) 'vector)
           (reverse (apply #'vector (loop for i from 1 to 10 collect i))))
   (equal "" (reverse ""))
   (equal "M" (reverse "M"))
   (equal "?gnup ton siht sI"
          (reverse "Is this not pung?"))))

(defun foo (x)
  (+ x 9))

(deftest test-function-vs-symbol-function ()
  (flet ((foo (x) (* x 2)))
    (let ((foo #'(lambda (x) (- x 17))))
      (check
       (= 6 (foo 3))
       (= 6 (funcall #'foo 3))
       (= 12 (funcall 'foo 3))
       (= 12 (funcall (symbol-function 'foo) 3))
       (= -14 (funcall foo 3)))) ))

;;;
;;;    SOME and NOTEVERY (i.e., some not) are existence claims.
;;;    
;;;    Touretzky (?) 215 页:
;;;    (some <PRED> <SEQ>) ≈ (find-if <PRED> <SEQ>)
;;;    - FIND-IF always returns an elt of <SEQ> when true
;;;    "Notevery not"
;;;    
(deftest test-some ()
  (check
   (not (some #'evenp '()))
   (some #'evenp '(1 2 3 4 5))
   (some (complement #'oddp) '(1 2 3 4 5))
   (notevery (complement #'evenp) '(1 2 3 4 5))
   (some (complement #'evenp) '(1 2 3 4 5))
   (eq t (some #'evenp '(1 2 3 4 5)))
   (= 2 (find-if #'evenp '(1 2 3 4 5)))
   (flet ((f (l) (if (every #'evenp l) l nil)))
     (equal (find-if #'f #1='((1 2 3) (4 5 6) (6 8) (2 4)))
            (some #'f #1#)))
   (equal '(6 8) (find-if (partial #'every #'evenp) '((1 2 3) (4 5 6) (6 8) (2 4)))) ; FIND-IF preferable here
   (some #'char= "asdf" "abcd")
   (some #'< '(1 2 3) '(3 2 1))
   (some #'< '(1 2 3) '(3 2))
   (not (some #'< '(1 2 3) '()))
   (some #'< '(1 2) '(3 2 1))
   (not (some #'< '() '(3 2 1)))
   (some #'numberp '(x :a #\a "foo" 8 (error "Can't get here")))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidebar: compare SOME vs. FIND-IF
;; (defun some1 (fn l)
;;   (cond ((endp l) nil)
;; 	((funcall fn (first l)))
;; 	(t (some1 fn (rest l)))) )

;; (defun find-if1 (fn l)
;;   (cond ((endp l) nil)
;; 	((funcall fn (first l)) (first l))
;; 	(t (find-if1 fn (rest l)))) )

;; ;;;
;; ;;;    Abstraction
;; ;;;    
;; (defun traverse (l test xform)
;;   (cond ((endp l) nil)
;; 	((funcall test (first l)) (funcall xform (first l)))
;; 	(t (traverse (rest l) test xform))))

;; (defun some2 (fn l)
;;   (traverse l fn fn))

;; (defun find-if2 (fn l)
;;   (traverse l fn #'identity))


;; SOME -> FIND-IF:
;; (some f seq) -> (some #'(lambda (elt) (and (funcall f elt) elt)) seq)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;    (every <PRED> <SEQ>) ≈ (null (remove-if <PRED> <SEQ>))
;;;    "Notany not"
;;;    
(deftest test-every ()
  (check
   (every #'evenp '())
   (every #'evenp '(2 4 6 8))
   (every (complement #'oddp) '(2 4 6 8))
   (notany (complement #'evenp) '(2 4 6 8))
   (not (every #'evenp '(1 2 3 4 5)))
   (every #'symbolp #1='(x :a y :foo bar))
   (eq (null (remove-if #'symbolp #1#)) ; Touretzky ex. 7.27
       (every #'symbolp #1#))
   (every #'char-equal "asdf" "ASDF")
   (every #'< '(1 2 3) '(4 5 6) '(7 8 9))
   (every #'< '(1 2 3) '()) ; !! Not an issue for Clojure:
   (every #'< '() '(4 5 6)) ; !!   every only applied to 1 sequence.
   (every #'char= "asdf" "asdfg") ; !!
   (not (every #'symbolp '(x :a y 8 (error "Can't get here")))) ))

;;;
;;;    CLHS says (https://www.lispworks.com/documentation/HyperSpec/Body/f_everyc.htm):
;;;    (notany <PRED> <SEQ>+) ≡ (not (some <PRED> <SEQ>+))
;;;    
;;;                           ≡ (every (complement <PRED>) <SEQ>+)
;;;    (notany <PRED> <SEQ>) ≈ (null (remove-if-not <PRED> <SEQ>))
;;;    "Not some" "Every not"
;;;    
(deftest test-notany ()
  (check
   (notany #'evenp '())
   (notany #'evenp '(1 3 5 7 9))
   (notany (complement #'oddp) '(1 3 5 7 9))
   (every (complement #'evenp) '(1 3 5 7 9))
   (notany #'numberp #1='(x :a y :foo bar))
   (eq (null (remove-if-not #'numberp #1#))
       (notany #'numberp #1#))
   (notany #'char= "asdf" "ASDF")
   (notany #'< '(7 8 9) '(4 5 6) '(1 2 3))
   (notany #'< '(1 2 3) '()) ; !! Not an issue for Clojure:
   (notany #'< '() '(4 5 6)) ; !!   every only applied to 1 sequence.
   (not (notany #'numberp '(x :a y 8 (error "Can't get here")))) ))

;;;
;;;    CLHS says (https://www.lispworks.com/documentation/HyperSpec/Body/f_everyc.htm):
;;;    (notevery <PRED> <SEQ>+) ≡ (not (every <PRED> <SEQ>+))
;;;    
;;;                             ≡ (some (complement <PRED>) <SEQ>+)
;;;    The 2nd equivalence above is not strict. In other words, NOTEVERY cannot truly
;;;    be expressed in terms of SOME. In particular, NOTEVERY always returns T or NIL
;;;    whereas SOME may return some other non-nil value to represent true.
;;;    
;;;    Touretzky (?) 215 页:
;;;    (notevery <PRED> <SEQ>) ≈ (find-if (complement <PRED>) <SEQ>)
;;;    - FIND-IF-NOT always returns an elt of <SEQ> when true
;;;    "Some not"
;;;    
(deftest test-notevery ()
  (check
   (not (notevery #'evenp '()))
   (notevery #'evenp '(1 2 3 4 5))
   (notevery (complement #'oddp) '(1 2 3 4 5))
   (some (complement #'evenp) '(1 2 3 4 5))
   (notevery (complement #'evenp) '(1 2 3 4 5))
   (eq t (notevery #'evenp '(1 2 3 4 5)))
   (= 2 (find-if (complement #'oddp) '(1 2 3 4 5)))
   (= 2 (find-if-not #'oddp '(1 2 3 4 5)))
   (flet ((f (l) (if (some #'oddp l) l nil)))
     (and (find-if-not #'f #1='((1 2 3) (4 5 6) (6 8) (2 4)))
          (notevery #'f #1#))) ; Does not return element
   (equal '(6 8) (find-if-not (partial #'some #'oddp) '((1 2 3) (4 5 6) (6 8) (2 4))))
   (notevery #'char= "asdf" "abcd")
   (notevery #'< '(1 2 3) '(3 2 1))
   (notevery #'< '(1 2 3) '(3 2))
   (not (notevery #'< '(1 2 3) '()))
   (notevery #'< '(1 2) '(3 2 1))
   (not (notevery #'< '() '(3 2 1)))
   (notevery #'symbolp '(x :a #\a "foo" 8 (error "Can't get here")))) )

(deftest test-member ()
  (check
   (let ((list (loop for i from 1 to 20 collect i)))
     (totally (maplist #'(lambda (tail)
                           (eq tail ; EQ !!
                               (member (first tail) list)))
                       list)))
   (not (member 5 #1='(a b c d)))
   (equal #2='((1 . 2) (3 . 4)) (member 2 #2# :key #'cdr))
   (equal (rest #2#) (member 2 #2# :test-not #'= :key #'cdr)) ; CLHS
   (not (member '(b) #3='((a) (b) (c))))
   (equal (rest #3#) (member '(b) #3# :test #'equal))
   (not (member '#:c #1#))
   (equal '(c d) (member "C" #1# :key #'symbol-name :test #'equal))
   (equal '(c d) (member '#:c #1# :test #'(lambda (s1 s2) (equal (symbol-name s1) (symbol-name s2)))) )))

(deftest test-remove ()
  (check
   (equal '(1 2 3) (remove 4 #1='(1 2 3 4)))
   (equal #1# (remove 9 #1#))
   (equal '(a (b c) d) (remove 'c '(a (b c) c d)))
   (equal '(3 4 5) (remove 3 '(1 2 3 4 5) :test #'>)) ; (> 3 elt)
   (equal '(1 1 0 -1) (remove 1 #2='(1 2 3 2 1 0 -1) :test #'<)) ; (< 1 elt)
   ;; PAIP 61 页 argues that this is clearer here
   (equal '(1 1 0 -1) (remove-if #'(lambda (elt) (> elt 1)) #2#))
   (equal '(1 1 0 -1) (remove-if (partial* #'> 1) #2#))
   (equal '(1 1 0 -1) (loop for elt in #2# unless (> elt 1) collect elt))
   (let ((l #3='(a b c d e)))
     (remove 'c l)
     (equal #3# l))
   (let ((l #3#))
     (remove 'q l)
     (equal #3# l))
   (equal '("pung") (remove 3 '("pung" "foo" "bar" "baz") :key #'length))
   (equal '("pung") (remove-if #'(lambda (elt) (= (length elt) 3)) '("pung" "foo" "bar" "baz"))) ; Better?
   (equal '(2 3 4) (remove 1 #4='(1 2 1 3 1 4)))
   (equal '(2 1 3 1 4) (remove 1 #4# :count 1))
   (equal '(2 3 1 4) (remove 1 #4# :count 2))
   (equal '(1 2 1 3 4) (remove 1 #4# :from-end t :count 1))
   (equal '(1 2 3 4) (remove 1 #4# :from-end t :count 2))))
   
(deftest test-mapcar ()
  (check
   (equal '(2 4 6 8) (mapcar (partial #'* 2) '(1 2 3 4)))
   (equal '(t nil t nil) (mapcar #'< '(1 2 3 4) '(3 2 5 4)))
   (equal '(1111 2222 3333 4444) (mapcar #'+ '(1 2 3 4) '(10 20 30 40) '(100 200 300 400) '(1000 2000 3000 4000)))
   (equal '((a 1) (b 2) (c 3)) (mapcar #'list '(a b c) '(1 2 3)))
   (equal '((a . 1) (b . 2) (c . 3)) (mapcar #'cons '(a b c) '(1 2 3)))
   (equal #1='(1 2 3 4 5 6) (mapcar #'identity #1#))
   (= (length #1#) (length (mapcar #'1+ #1#)))) )

(deftest test-subst ()
  (check
   (equal '(b (b c (b))) (subst 'b 'a '(a (b c (a)))) )
   (equal '((is this . pung) not . pung) (subst 'pung 'nil '((is this) not)))
   (equal '((the hatter) (the hare) and (the dormouse)) ; Touretzky 193 页
          (subst 'the 'a '((a hatter) (a hare) and (a dormouse))))
   ;;
   ;;    Test must be applicable to _any_ subtree.
   ;;    Not: (subst 'x pi (list 2 3 (list pi)) :test #'=) <-- Error for NIL CDR
   (equal '(2 3 (x)) (subst 'x pi (list 2 3 (list pi)) :test #'(lambda (old elt) (and (numberp elt) (= old elt)))) )))

(deftest test-push ()
  (check
   (let* ((x (list 1 2 3))
          (y x))
     (push 0 x)
     (eq y (rest x)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Parameter passing
;;;    
;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/v_lambda.htm
;;;    
(deftest test-lambda-list-keywords ()
  (check
   (subsetp '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)
            lambda-list-keywords)
   (notany #'keywordp lambda-list-keywords)))

(deftest test-optional-parameters ()
  (flet ((f (a &optional b (c 3) (d 4 e))
           (list a b c d e)))
    (check
     (equal '(1 nil 3   4   nil) (f 1))
     (equal '(1 2   3   4   nil) (f 1 2))
     (equal '(1 2   3d0 4   nil) (f 1 2 3d0))
     (equal '(1 2   3d0 4d0 t)   (f 1 2 3d0 4d0)))) )

(deftest test-keyword-parameters ()
  (flet ((f (&key x (y 99) (z #\Z z-supplied-p))
           (list x y z z-supplied-p))
         (g (&key ((:x x)) ((:y y) 99) ((:z z) #\Z z-supplied-p)) ; Identical to F
           (list x y z z-supplied-p))
         (h (&key ((epsilon x) 8)) ; Invoked as EPSILON (Thus not really "keyword" parameter). Parameter is X.
           (list x)))
    (check
     (equal '(nil 99 #\Z nil) (f))
     (equal '(nil 12 #\Z nil) (f :y 12))
     (equal '(7 12 #\Z nil) (f :y 12 :x 7))
     (equal '(7 12 #\p t) (f :y 12 :z #\p :x 7))

     (equal '(nil 99 #\Z nil) (g))
     (equal '(nil 12 #\Z nil) (g :y 12))
     (equal '(7 12 #\Z nil) (g :y 12 :x 7))
     (equal '(7 12 #\p t) (g :y 12 :z #\p :x 7))

     (equal '(8) (h))
     (equal '(9) (h 'epsilon 9)))) ; Not :EPSILON!
  (flet ((f (&key ((:long-descriptive-external-name x)))
           (list x)))
    (check
     (equal '(nil) (f))
     (equal '(99) (f :long-descriptive-external-name 99))))
  ;; https://www.lispworks.com/documentation/HyperSpec/Body/03_dad.htm
  (flet ((f (&key x) (list x)))
    (check
     (equal '(3) (f :x 3))
     (equal '(2) (f :x 2 :x 3))
     (equal '(1) (f :x 1 :x 2 :x 3))))
  (check
   (string= "aaaa" (make-string 4 :initial-element #\a)) ; Keyword arg evaluated just like any other. Usually transparent since keyword is self-evaluating object.
   (let ((initial-element :initial-element))
     (string= "aaaa" (make-string 4 initial-element #\a))))
  ;; https://www.lispworks.com/documentation/HyperSpec/Body/03_dadaa.htm
  (check
   (= 1 ((lambda (&key x) x) :x 1 :y 2 :allow-other-keys t)) ; Caller asserts
   (= 1 ((lambda (&key x &allow-other-keys) x) :x 1 :y 2)) ; Callee allows
   ((lambda (&key) t) :allow-other-keys nil)
   (handler-case ((lambda (&key) t) :allow-other-keys nil :b 9)
     (error () t)
     (:no-error () (error "Unknown &KEY argument: :B")))) )

(deftest test-mix-optional-keyword-parameters ()
  (flet ((f (a &optional b (c 3) &rest d &key e)
           (list a b c d e)))
    (check
     (equal '(10 :e 13 () nil) (f 10 :e 13))
     (equal '(10 :e 13 (14 15 16 17 :allow-other-keys t) nil)
            (f 10 :e 13 14 15 16 17 :allow-other-keys t))
     (handler-case (equal '(10 :e 13 (14 15 16) nil) (f 10 :e 13 14 15 16))
       (error () t)
       (:no-error () (error "Odd number of &KEY arguments")))
     (equal '(10 :e 13 (:e 14) 14) (f 10 :e 13 :e 14)))) )

;;;
;;;    &REST parameter can capture keyword args too -> ad hoc property list.
;;;    But stray args can upset alignment of property list.
;;;    
(deftest test-&rest-property-list ()
  (flet ((pung (&rest args &key foo bar)
           (and (eq foo (getf args :foo))
                (eq bar (getf args :bar))))
         (foo (&rest args)
           (list (getf args :foo) (getf args :bar))))
    (check
     (pung :foo 8 :bar 9)

     (handler-case (pung 2 :foo 8 :bar 9)
       (error () t)
       (:no-error () (error "Odd number of &KEY arguments")))

     (equal '(8 nil) (foo :foo 8))

     (handler-case (equal '(8 nil) (foo 2 :foo 8))
       (error () t)
       (:no-error () (error "malformed property list")))) ))

(deftest test-keywordp ()
  (check
   (and (symbolp 'pung) (not (keywordp 'pung)))
   (and (symbolp '&optional) (not (keywordp '&optional)))
   (and (symbolp ':pung) (keywordp ':pung))
   (and (symbolp :pung) (keywordp :pung))))

(deftest test-setf ()
  (check
   (let ((x 8)
         (y 8))
     (setq x 9)
     (setf y 9)
     (= x y))
   (let ((x 8)
         (y 8))
     (declare (special x))
     (declare (special y))
     (set 'x 9)
     (setf (symbol-value 'y) 9)
     (= x y))
   (let ((l1 #1=(list 1 2 3))
         (l2 #1#))
     (rplaca l1 :foo)
     (setf (car l2) :foo)
     (equal l1 l2))
   (let ((x 1)
         (y 2)
         (z 3))
     (setf x :one y :two z :three)
     (and (eq x :one)
          (eq y :two)
          (eq z :three)))
   (let* ((x 10)
          (y x))
     (setf x 9)
     (= y 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Association lists (alists) / Property lists (plists)
;;;    
;;;    Both represent a map/table/dictionary, e.g.:
;;;
;;;    +-+-+
;;;    |a|1|
;;;    |-|-|
;;;    |b|2|
;;;    |-|-|
;;;    |c|3|
;;;    +-+-+

;;;
;;;    Alist:
;;;    ((a . 1) (b . 2) (c . 3)) ; Same number of CONS cells as plist
;;;    
;;;    [*|*]------->[*|*]------->[*|*]--->NIL 
;;;     |            |            |           
;;;     v            v            v           
;;;    [*|*]--->1   [*|*]--->2   [*|*]--->3   
;;;     |            |            |           
;;;     v            v            v           
;;;     A            B            C           

;;;
;;;    Alternatively:
;;;    ((a 1) (b 2) (c 3))
;;;                                                                         
;;;    [*|*]------------------>[*|*]------------------>[*|*]--->NIL         
;;;     |                       |                       |                   
;;;     v                       v                       v                   
;;;    [*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL
;;;     |        |              |        |              |        |          
;;;     v        v              v        v              v        v
;;;     A        1              B        2              C        3          

;;;
;;;    Plist:
;;;    (a 1 b 2 c 3)
;;;    
;;;    [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->NIL
;;;     |        |        |        |        |        |          
;;;     v        v        v        v        v        v          
;;;     A        1        B        2        C        3          
;;; 
;;;    Another way to visualize:
;;;    
;;;    [ * | * ]  >[ * | * ]  >[ * | * ]
;;;      |   |   /   |   |   /   |   |
;;;      v   V  /    v   V  /    v   V
;;;      A  [*|*]    B  [*|*]    C  [*|*]--->NIL    
;;;          |           |           |     
;;;          v           v           v     
;;;          1           2           3     
;;; 

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_assocc.htm
;;;    (assoc item alist :test fn) ≡ (find item alist :test fn :key #'car)
;;;    Except when `item' is NIL.
;;;    
(deftest test-assoc ()
  (check
   (equal '(jane 21) (assoc 'jane #1='((joe 22) (jane 21) (john 12))))
   (equal '(john 19) (assoc 'john (cons '(john 19) #1#)))
   (null (assoc 'tetsuo #1#))

   (equal '(jane . 21) (assoc 'jane #2='((joe . 22) (jane . 21) (john . 12))))
   (equal '(john . 19) (assoc 'john (acons 'john 19 #2#)))
   (null (assoc 'tetsuo #2#))

   (null (assoc 2d0 #3='((1 one) (2 two) (3 three))))
   (equal '(2 two) (assoc 2d0 #3# :test #'=))
   (null (assoc 1d0 #3# :key #'float))
   (equal '(1 one) (assoc 1d0 #3# :key (partial* #'float 1d0)))

   ;;
   ;;    Modify existing entry
   ;;    
   (let* ((keys '(a b c))
          (values '(1 2 3))
          (alist (pairlis keys values)))
     (check
      (handler-case (setf (assoc 'a alist) 5)
        (undefined-function () t)
        (:no-error () (error "The function (SETF ASSOC) is undefined.")))

      (= 3 (cdr (assoc 'c alist)))
      (progn
        (setf (cdr (assoc 'c alist)) 99)
        (= 99 (cdr (assoc 'c alist))))
        
      (= 1 (cdr (assoc 'a alist)))
      (progn
        (rplacd (assoc 'a alist) 12)
        (= 12 (cdr (assoc 'a alist)))) ))))
   
(deftest test-pairlis ()
  (check
   (let* ((keys '(a b c))
          (values '(1 2 3))
          (alist (pairlis keys values))) ; (... (a . 1) ...)
     (every #'(lambda (key) (assoc key alist)) keys))
   (let* ((keys '(a b c))
          (values '(1 2 3))
          (alist (pairlis keys (mapcar #'list values)))) ; (... (a 1) ...)
     (every #'(lambda (key) (assoc key alist)) keys))
   (let* ((alist '((a . 1) (b . 2) (c . 3)))
          (new-alist (pairlis '(d e) '(4 5) alist)))
     (tailp alist new-alist))))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_acons.htm
;;;    (acons key datum alist) ≡ (cons (cons key datum) alist)
;;;
;;;    Add new key/value or shadow existing key. Can't tell, doesn't detect.
;;;    
(deftest test-acons ()
  (check
   (let ((alist (pairlis '(a b c) '(1 2 3))))
     (and (null (assoc 'd alist))
          (equal '(d . 4) (assoc 'd (acons 'd 4 alist)))) )
   (let ((alist (pairlis '(a b c) '(1 2 3))))
     (and (null (assoc 'd alist))
          (equal '(d 4) (assoc 'd (acons 'd '(4) alist)))) )
   (let ((alist (pairlis '(a b c) '(1 2 3)))) ; Shadow
     (equal '(a . 4) (assoc 'a (acons 'a '4 alist)))) ))

;;;
;;;    No REMF/REMPROP for association lists as with property lists.
;;;    Instead:
;;;    - Remove all key/value pairs
;;;    (remove 'b (pairlis '(a b c b d b) '(1 2 3 4 5 6)) :key #'car)
;;;    - Remove first key/value pair
;;;    (let ((alist (pairlis '(a b c b d b) '(1 2 3 4 5 6))))
;;;      (remove (assoc 'b alist) alist))

(deftest test-copy-alist ()
  (let* ((a (pairlis '(a b c) '(1 2 3)))
         (b (copy-list a))
         (c (copy-alist a)))
    (check
     (equal a b)
     (equal a c))

    (setf (cdr (assoc 'b c)) 2d0)
    (check
     (not (equal a c))
     (eql (cdr (assoc 'b a)) 2)
     (eql (cdr (assoc 'b c)) 2d0))

    (setf (cdr (assoc 'a b)) 1d0)
    (check
     (equal a b)
     (eql (cdr (assoc 'a a)) 1d0)
     (eql (cdr (assoc 'a b)) 1d0)
     (eql (cdr (assoc 'a c)) 1))))

;; Alist A:
;;    [*|*]------->[*|*]------->[*|*]--->NIL
;;     |            |            |
;;     v            v            v
;;  +>[*|*]--->3 +>[*|*]--->2 +>[*|*]--->1d0
;;  |  |         |  |         |  |
;;  |  v         |  v         |  v
;;  |  C         |  B         |  A
;;  |            |            |
;; [*|*]------->[*|*]------->[*|*]--->NIL
;; Alist B:

;; Alist C:
;; [*|*]------->[*|*]------->[*|*]--->NIL
;;  |            |            |
;;  v            v            v
;; [*|*]--->3   [*|*]--->2d0 [*|*]--->1
;;  |            |            |
;;  v            v            v
;;  C            B            A

(deftest test-rassoc ()
  (let ((a (pairlis '(a b c) '(1 2 3))))
    (check
     (equal '(a . 1) (rassoc 1 a))
     (equal '(b . 2) (rassoc 2 a))
     (equal '(c . 3) (rassoc 3 a))))
  (let ((a '((a 1) (b 2) (c 3))))
    (check
     (null (rassoc 1 a))
     (equal '(a 1) (rassoc 1 a :key #'car))
     (equal '(b 2) (rassoc 2 a :key #'car))
     (equal '(c 3) (rassoc 3 a :key #'car)))) )

(deftest test-getf ()
  (check
   (let ((plist (list 'joe 22 'jane 21 'john 12)))
     (check
      (= 22 (getf plist 'joe))
      (null (getf plist 'bruno))
      (= 26 (getf plist 'bruno 26))))
   (let ((plist (list :joe 22 :jane 21 :john 12))) ; Practical Common Lisp 306 页
     (destructuring-bind (&key joe bruno &allow-other-keys) plist
       (check
        (= 22 joe)
        (null bruno))))
   (let ((plist (list 'joe 22 'jane 21 'john 12)))
     (setf (getf plist 'joe) 40
           (getf plist 'horkimer) 92)
     (check
      (= 40 (getf plist 'joe))
      (= 92 (getf plist 'horkimer))))
   (let ((plist '()))
     (incf (getf plist 'count 0))
     (check
      (equal '(count 1) plist)
      (= 1 (getf plist 'count))))
   (let ((plist1 '(a 1 b 2 c 3 d)) ; Odd number of elts "OK" as long as last "key" is not accessed
         (plist2 '(a 1 b 2 c 3)))
     (check
      (= (getf plist1 'a) (getf plist2 'a))
      (= (getf plist1 'b) (getf plist2 'b))
      (= (getf plist1 'c) (getf plist2 'c))
      (null (getf plist2 'd))
      (handler-case (getf plist1 'd) 
        (error () t)
        (:no-error () (error "malformed property list")))) )
   (let ((plist '(c 9 a 1 b 2 c 3))) ; Duplicate key shadowed
     (check
      (= 9 (getf plist 'c))
      (progn (setf (getf plist 'c) 12)
             (check
              (equal '(c 12 a 1 b 2 c 3) plist)
              (= 12 (getf plist 'c)))) ))
   (let* ((b #\b)
          (four 4d0)
          (five 5)
          (big (expt 3 99))
          (plist (list "a" 1 b 2 '(c) 3 four 4 five 5 big 6)))
     (check
      (null (getf plist "a")) ; Test is EQ. Use symbols for keys.
      ;;            (null (getf plist #\b)) ; Implementation-dependent
      (null (getf plist '(c)))
      ;;            (null (getf plist 4d0)) ; Implementation-dependent
      ;;            (null (getf plist 5)) ; Implementation-dependent
      ;;            (null (getf plist (expt 3 99))) ; Implementation-dependent
      ) )))

;;;
;;;    No RASSOC for property lists
;;;
;; (defun regetf (plist key)
;;   (getf (reverse plist) key))

(deftest test-get-properties ()
  (let ((plist (list 'joe 22 'jane 21 'john 12)))
    (check
     (multiple-value-bind (indicator value tail) (get-properties plist '(bruno sam joe))
       (and (eq 'joe indicator)
            (= 22 value)
            (equal '(joe 22 jane 21 john 12) tail)))
     (multiple-value-bind (indicator value tail) (get-properties plist '(bruno sam jane))
       (and (eq 'jane indicator)
            (= 21 value)
            (equal '(jane 21 john 12) tail)))
     (multiple-value-bind (indicator value tail) (get-properties plist '(bruno sam john))
       (and (eq 'john indicator)
            (= 12 value)
            (equal '(john 12) tail)))
     (multiple-value-bind (indicator value tail) (get-properties plist '(bruno sam))
       (and (null indicator)
            (null value)
            (null tail)))) ))

(deftest test-remf ()
  (check
   (let* ((plist1 (list 'joe 22 'jane 21 'john 12))
          (plist2 plist1))
     (check
      (remf plist1 'joe)
      (null (remf plist1 'joe)) ; Already gone
      (= 4 (length plist1))
      (= 6 (length plist2)) ; Implementation-dependent?
      (eq :foo (getf plist1 'joe :foo))
      (= 22 (getf plist2 'joe :foo)) ; Implementation-dependent?

      (remf plist1 'john)
      (= 2 (length plist1))
      (= 4 (length plist2)) ; Implementation-dependent?
      (/= 6 (length plist2)) ; Guaranteed? Tail is destructively modified.
      (eq :foo (getf plist1 'john :foo))
      (eq :foo (getf plist2 'john :foo))

      (null (remf plist1 'tony))))
   (let ((plist (list 'joe 27 'joe 22 'jane 21 'john 12)))
     (check
      (= 27 (getf plist 'joe))

      (remf plist 'joe)
      (= 22 (getf plist 'joe))

      (remf plist 'joe)
      (eq :foo (getf plist 'joe :foo))
      
      (null (remf plist 'joe)))) ; Already gone
   (let ((plist (list 'c 9 'a 1 'b 2 'c 3))) ; Duplicate key shadowed
     (check
      (= 9 (getf plist 'c)) ; Shadowed
      (remf plist 'c)
      (equal '(a 1 b 2 c 3) plist)
      (= 3 (getf plist 'c)) ; Unshadowed

      (remf plist 'c)
      (equal '(a 1 b 2) plist)
      (eq :foo (getf plist 'c :foo)) ; Removed

      (null (remf plist 'c)))) ))

(deftest test-symbol-plist ()
  (let ((symbol (make-symbol "KID-CHARLEMAGNE"))
        (plist (list :gas-in-the-car t 'people-down-the-hall-know-who-you-are 'maybe)))
    (check
     (null (symbol-plist symbol))
     (progn (setf (symbol-plist symbol) plist)
            (eq (symbol-plist symbol) plist))
     (eq t (get symbol :gas-in-the-car))
     (eq 'maybe (get symbol 'people-down-the-hall-know-who-you-are))
     (not (boundp symbol)))) ) ; Existence of symbol property list has nothing to do with whether symbol names a bound variable.

(deftest test-get ()
  (let ((symbol (make-symbol "BABS")))
    (check
     (null (get symbol 'spouse))
     (null (get symbol 'hotel))
     (null (get symbol 'divorce))

     (setf (get symbol 'spouse) 'clean-willy
           (get symbol 'hotel) 'bon-marché
           (get symbol 'divorce) 'haitian)
     (eq 'clean-willy (get symbol 'spouse))
     (eq 'bon-marché (get symbol 'hotel))
     (eq 'haitian (get symbol 'divorce))

     ;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_get.htm
     ;;    (get x y) ≡ (getf (symbol-plist x) y)
     (every #'(lambda (indicator)
                (eq (getf (symbol-plist symbol) indicator)
                    (get symbol indicator)))
            '(spouse hotel divorce)))) )

;;;
;;;    GETF:REMF::GET:REMPROP
;;;    (GET:GETF::REMPROP:REMF) ???
;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_rempro.htm
;;;    (remprop x y) ≡ (remf (symbol-plist x) y)
;;;    
(deftest test-remprop ()
  (let ((symbol (make-symbol "SANTA-ANA-WINDS"))
        (plist (list 'kid :will-live-and-learn 'i-am-the-only-one t 'you-gotta :shake-it-baby)))
    (setf (symbol-plist symbol) plist)
    (check
      (remprop symbol 'kid)
      (null (remprop symbol 'kid)) ; Already gone
      (= 4 (length (symbol-plist symbol)))
      (eq :foo (get symbol 'kid :foo)))
   (let ((symbol (make-symbol "GREEN-EARRINGS"))
         (plist (list 'flies-on-me :no 'flies-on-me :yes 'design 'rare)))
     (setf (symbol-plist symbol) plist)
     (check
      (eq :no (get symbol 'flies-on-me))

      (remprop symbol 'flies-on-me)
      (eq :yes (get symbol 'flies-on-me))

      (remprop symbol 'flies-on-me)
      (eq :foo (get symbol 'flies-on-me :foo))
      
      (null (remprop symbol 'flies-on-me)))) ; Already gone
   (let ((symbol (make-symbol "BODHISATTVA"))
         (plist (list 'sparkle :china 'shine :japan 'sell-my-house-in-town t 'sparkle :india))) ; Duplicate key shadowed
     (setf (symbol-plist symbol) plist)
     (check
      (eq :china (get symbol 'sparkle)) ; Shadowed
      (remprop symbol 'sparkle)
      (equal '(shine :japan sell-my-house-in-town t sparkle :india) (symbol-plist symbol))
      (eq :india (get symbol 'sparkle)) ; Unshadowed

      (remprop symbol 'sparkle)
      (equal '(shine :japan sell-my-house-in-town t) (symbol-plist symbol))
      (eq :foo (get symbol 'sparkle :foo)) ; Removed

      (null (remprop symbol 'sparkle)))) ))

;;;
;;;    Older versions of Lisp had PUT/PUTPROP functions to set values on plists.
;;;    Unnecessary in Common Lisp: (SETF (GET ...))
;;;
;; (defun put (object property value)
;;   (setf (get object property) value))

;;;
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_get.htm
;;;    There is no way using GET to distinguish an absent property from one whose value is default.
;;;    ?????
;; (defun hasprop (symbol prop)
;;   (let ((flag (list 'pung)))
;;     (not (eq (get symbol prop flag) flag)))) ; Retrieved property couldn't possibly be EQ to fresh CONS

;; (defun hasprop (symbol prop)
;;   (and (get-properties (symbol-plist symbol) (list prop)) t))

;;;
;;;    Application of alist
;;;    
(deftest test-sublis ()
  (check
   (equal '(john loves jane (who loves bill))
          (sublis '((mary . john) (john . jane) (jane . bill)) ; (<OLD> . <NEW>)
                  '(mary loves john (who loves jane))))
   (equal '(b c c) (sublis '((a . b) (b . c)) '(a b c))) ; Parallel
   (equal '(c c c) (subst 'c 'b (subst 'b 'a '(a b c)))) ; Sequential
   (string= "bac aabcb acb"
            (coerce (sublis '((#\a . #\b) (#\b . #\a))
                            (coerce "abc bbaca bca" 'list))
                    'string))))


;;;
;;;    :KEY is only applied to elements of <SEQUENCE> _not_ to <ITEM> with FIND
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm
;;;    
;;;    (find <ITEM> <SEQUENCE> &key from-end test test-not start end key)
;;;    
;; (find '(1) '(1 (1) 2 (2)))
;; NIL
;; (find '(1) '(1 (1) 2 (2)) :test #'equal)
;; (1)
;; (find '(1) '((1) (2)) :key #'car)
;; NIL
;; (find '(1) '((1) (1 2)) :key #'car)
;; NIL
;; (find '(1) '((1) ((1) 2)) :key #'car)
;; NIL
;; (find '(1) '((1) ((1) 2)) :key #'car :test #'equal)
;; ((1) 2)


;;;
;;;    17.2.1 Satisfying a Two-Argument Test
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/17_ba.htm
;;;
;;;    "The function designated by the :key argument is never called on O itself."
;;;                                                     ^^^^^^^^^^^^^^^^^^^^^^^^
;;;
;;;     (17.2.2 Satisfying a One-Argument Test
;;;      https://www.lispworks.com/documentation/HyperSpec/Body/17_bb.htm)


;;;
;;;    :KEY is applied to both <ITEM> and elt of <PLACE> for ADJOIN/PUSHNEW
;;;            ^^^^^^^^^^      ^^^^^^
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/m_pshnew.htm
;;;    If :key is supplied, it is used to extract the part to be tested from
;;;    both item and the list element, as for adjoin.
;;;
;;;    https://www.lispworks.com/documentation/HyperSpec/Body/f_adjoin.htm
;;;    (adjoin <ITEM> <LIST> &key key test test-not)
;;;    
;;;    But ADJOIN documentation is weaselly:
;;;    The test, test-not, and key affect how it is determined whether item is the same
;;;    as an element of list. For details, see
;;;    Section 17.2.1 (Satisfying a Two-Argument Test).
;;;
;;;    Obviously the CONS (b . 12) is not present in the alist. FIRST is applied to
;;;    both <ITEM> and each elt of <LIST>...
;; (adjoin (cons 'b 12) (pairlis '(a b c) '(1 2 3)) :key #'first)
;; ((C . 3) (B . 2) (A . 1))
;; (adjoin (cons 'd 12) (pairlis '(a b c) '(1 2 3)) :key #'first)
;; ((D . 12) (C . 3) (B . 2) (A . 1))

;;;
;;;     CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_adjoin.htm
;;;     (adjoin item list :key fn) ≡ (if (member (fn item) list :key fn) list (cons item list))
;;;     
(deftest test-adjoin ()
  (check
   (let ((l '(a b c)))
     (check
      (equal l (adjoin 'a l))
      (equal (cons 'd l) (adjoin 'd l))))
   (let ((l '("foo" "bar" "baz")))
     (check
      (equal (cons "baz" l) (adjoin "baz" l))
      (equal l (adjoin "baz" l :test #'string=))
      (equal (cons "BAZ" l) (adjoin "BAZ" l))
      (equal l (adjoin "BAZ" l :test #'string-equal))
      (equal l (adjoin "BAZ" l :test #'string= :key #'string-downcase))))
   (let ((l '((1 x) (2 y) (3d0 z))))
     (check
      (equal (cons '(2 :y) l) (adjoin '(2 :y) l))
      (equal l (adjoin '(2 :y) l :key #'first)) ; :KEY applied to <ITEM> _and_ each elt of <LIST>
      (equal (cons '(3 :z) l) (adjoin '(3 :z) l :key #'first))
      (equal l (adjoin '(3 :z) l :key #'first :test #'=)))) ))
