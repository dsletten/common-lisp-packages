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
                     (multiple-value-bind (_ r) (floor p d) ; FLOOR -> MOD
                       (declare (ignore _))
                       (= r (mod p d)))
                     (multiple-value-bind (_ r) (floor (/ p d))
                       (declare (ignore _))
                       (= (* d r) (mod p d)))
                     (multiple-value-bind (_ r) (truncate p d) ; TRUNCATE -> REM
                       (declare (ignore _))
                       (= r (rem p d)))
                     (multiple-value-bind (_ r) (truncate (/ p d))
                       (declare (ignore _))
                       (= (* d r) (rem p d)))) )))

(deftest test-symbol-name ()
  (check
   (string= "PUNG" (symbol-name 'pung))
   (string= "PUNG" 'pung) ; String designator
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

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_nump.htm
;;;    (numberp object) ≡ (typep object 'number)
;;;    
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
;;;    见 TEST-= below
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

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm
;;;    Returns true if its arguments are the same, identical object; otherwise, returns false.
;;;
;;;    An implementation is permitted to make ``copies'' of characters and numbers at any time.   
;;;    The effect is that Common Lisp makes no guarantee that eq is true even when both its       
;;;    arguments are ``the same thing'' if that thing is a character or number.                   
;;;                                                                                               
;;;    Most Common Lisp operators use eql rather than eq to compare objects,                      
;;;    or else they default to eql and only use eq if specifically requested to do so.            
;;;    However, the following operators are defined to use eq rather than eql                     
;;;    in a way that cannot be overridden by the code which employs them:                         
;;;                                                                                               
;;;    catch           getf     throw                                                             
;;;    get             remf                                                                       
;;;    get-properties  remprop                                                                    
;;;
(deftest test-eq ()
  (check
   (eq 'a 'a)
   (not (eq (make-symbol "FOO") (make-symbol "FOO")))

   (not (eq 3 3d0))
   (or (eq 3 3) (not (eq 3 3)))
   (let ((x 3))
     (or (eq x x) (not (eq x x)))) ; CLHS literally says this!!
   (or (eq 3d0 3d0) (not (eq 3d0 3d0)))
   (or (eq #c(3 -4) #c(3 -4)) (not (eq #c(3 -4) #c(3 -4))))

   (or (eq #\A #\A) (not (eq #\A #\A)))

   (not (eq (cons 'a 'b) (cons 'a 'b)))
   (or (eq '(a . b) '(a . b)) (not (eq '(a . b) '(a . b))))
   (let ((x '(a . b)))
     (eq x x))

   (not (eq "pung" (copy-seq "pung")))
   (or (eq "pung" "pung") (not (eq "pung" "pung")))
   (let ((x "pung"))
     (eq x x))))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm
;;;    The value of eql is true of two objects, x and y, in the folowing cases:                                        
;;;                                                                                                                    
;;;    1. If x and y are eq.                                                                                           
;;;    2. If x and y are both numbers of the same type and the same value.                                             
;;;    3. If they are both characters that represent the same character.                                               
;;;                                                                                                                    
;;;    Otherwise the value of eql is false.                                                                            
;;;                                                                                                                    
;;;    If an implementation supports positive and negative zeros as distinct values, then (eql 0.0 -0.0) returns false.
;;;    Otherwise, when the syntax -0.0 is read it is interpreted as the value 0.0, and so (eql 0.0 -0.0) returns true. 
;;;
;;;    eql is the same as eq, except that if the arguments are characters or numbers of the same type then their values are compared.       
;;;    Thus eql tells whether two objects are conceptually the same, whereas eq tells whether two objects are implementationally identical. 
;;;    It is for this reason that eql, not eq, is the default comparison predicate for operators that take sequences as arguments.          
;;;                                                                                                                                         
;;;    eql may not be true of two floats even when they represent the same value. = is used to compare mathematical values.                 
;;;                                                                                                                                         
;;;    Two complex numbers are considered to be eql if their real parts are eql and their imaginary parts are eql.                          
;;;    For example, (eql #C(4 5) #C(4 5)) is true and (eql #C(4 5) #C(4.0 5.0)) is false.                                                   
;;;    Note that while (eql #C(5.0 0.0) 5.0) is false, (eql #C(5 0) 5) is true.                                                             
;;;    In the case of (eql #C(5.0 0.0) 5.0) the two arguments are of different types, and so cannot satisfy eql.                            
;;;    In the case of (eql #C(5 0) 5), #C(5 0) is not a complex number, but is automatically reduced to the integer 5.                      
;;;
(deftest test-eql ()
  (check
   (eql 'a 'a)
   (not (eql (make-symbol "FOO") (make-symbol "FOO")))

   (eql 3 3)
   (let ((x 3))
     (eql x x))
   (not (eql 3 3.0))
   (eql 3.0 3.0)
   (eql 3d0 3d0)
   (or (eql 3d0 3s0) (not (eql 3d0 3s0)))
   (eql #c(3 -4) #c(3 -4))
   (not (eql #c(3 -4.0) #c(3 -4)))

   (multiple-value-bind (significand exponent sign) (integer-decode-float -0d0)
     (declare (ignore significand exponent))
     (if (minusp sign)
         (not (eql 0d0 -0d0))
         (eql 0d0 -0d0))) ; CLISP

   (not (eql (cons 'a 'b) (cons 'a 'b)))
   (or (eql '(a . b) '(a . b)) (not (eql '(a . b) '(a . b))))
   (let ((x (cons 'a 'b)))
     (eql x x))
   (let ((x '(a . b)))
     (eql x x))

   (eql #\A #\A)

   (or (eql "Foo" "Foo") (not (eql "Foo" "Foo")))
   (not (eql "Foo" (copy-seq "Foo")))
   (not (eql "FOO" "foo"))))

(defclass foo ()
  ((bar :initform "pung")))

;;;
;;;    EQUAL pathnames??
;;;    
(deftest test-equal ()
  (check
   (equal 'a 'a) ; EQ
   (not (equal (make-symbol "FOO") (make-symbol "FOO")))

   (equal 3 3) ; EQL
   (not (equal 3 3d0))
   (equal 3d0 3d0)
   (equal #c(3 -4) #c(3 -4))
   
   (equal #\a #\a) ; EQL
   (not (equal #\a #\A))

   (equal "Foo" "Foo") ; Elts compared by EQL
   (equal "Foo" (copy-seq "Foo"))
   (not (equal "FOO" "foo"))

   (equal (cons 'a 'b) (cons 'a 'b)) ; CARs EQUAL and CDRs EQUAL
   (equal '(a . b) '(a . b))
   (equal '(1 4/2 3d0) (copy-list '(1 4/2 3d0)))

   (not (equal (make-hash-table) (make-hash-table))) ; EQ
   (not (equal #(1 2 3) #(1 2 3)))
   (not (equal (make-instance 'foo) (make-instance 'foo)))) )

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm
;;;    Returns true if x and y are equal, or if they have components that are of the same type as each other
;;;    and if those components are equalp.
;;;
;;;    Type          Behavior                    
;;;    number        uses =                      
;;;    character     uses char-equal             
;;;    cons          descends                    
;;;    bit vector    descends                    
;;;    string        descends                    
;;;    pathname      same as equal               
;;;    structure     descends, as described above
;;;    Other array   descends                    
;;;    hash table    descends, as described above
;;;    Other object  uses eq                     
;;;
(deftest test-equalp ()
  (check
   (equalp 'a 'a) ; EQ
   (not (equalp (make-symbol "FOO") (make-symbol "FOO")))

   (equalp 3 3)
   (equalp 3 3d0)
   (equal 3d0 3d0)
   (equalp #c(3 -4) #c(3 -4))
   (equalp #c(3 -4d0) #c(3 -4))
   
   (equalp #\a #\a)
   (equalp #\a #\A)

   (equalp "Foo" "Foo")
   (equalp "Foo" (copy-seq "Foo"))
   (equalp "FOO" "foo")

   (equalp (cons 'a 'b) (cons 'a 'b))
   (equalp '(a . b) '(a . b))
   (equalp '(1 4/2 3d0) (copy-list '(1 4/2 3d0)))

   (let ((h1 (make-hash-table :test #'equal))
         (h2 (make-hash-table :test #'equal))
         (h3 (make-hash-table :test #'equal))
         (h4 (make-hash-table :test #'equal))
         (h5 (make-hash-table :test #'equalp)))
     (mapcar #'(lambda (k v)
                 (dolist (h (list h1 h2 h3 h4 h5))
                   (setf (gethash k h) v)))
             '("pung" "foo" "bar")
             '(1 2 3))
     (setf (gethash "baz" h3) 4)
     (setf (gethash "bar" h4) :three)
     (check
      (equalp h1 h2)
      (not (equalp h1 h3))
      (not (equalp h1 h4))
      (not (equalp h1 h5))))

   (not (equal #("pung" "foo" "bar") #("pung" "foo" "bar")))
   (equalp #("pung" "foo" "bar") #("pung" "foo" "bar"))
   (equalp #("pung" "FOO" "bar") #("Pung" "foo" "BAR"))
   (equals #("pung" "foo" "bar") #("pung" "foo" "bar")) ; CORE:EQUALS
   (not (equals #("pung" "FOO" "bar") #("Pung" "foo" "BAR")))

   (let ((v1 (make-array 6 :element-type 'integer :initial-contents '(1 1 1 3 5 7)))
         (v2 (make-array 8 :element-type 'integer :initial-contents '(1 1 1 3 5 7 2 6) :fill-pointer 6))
         (v3 (vector 1 1 1 3 5 7)))
     (check
      (equalp v1 v2)
      (equalp v1 v3)))

   (not (equalp (make-instance 'foo) (make-instance 'foo)))) )

(deftest test-equality ()
  (check
   (and (eq 'a 'a) (eql 'a 'a) (equal 'a 'a) (equalp 'a 'a))
   (and (eql 3 3) (equal 3 3) (equalp 3 3))
   (and (eql #\a #\a) (equal #\a #\a) (equalp #\a #\a))
   (and (equal "Foo" "Foo") (equalp "Foo" "Foo"))))

(deftest test-tree-equal ()
  (check
   (tree-equal #1='(1 (2) (3 (4 5) (6))) (copy-tree #1#))
   (not (tree-equal #1# #2='(1d0 (2d0) (3d0 (4d0 5d0) (6d0)))) )
   (handler-case (tree-equal #1# #2# :test #'=)
     (type-error () t)
     (:no-error () (error "TEST must apply to all leaves of tree!")))
   (tree-equal #1# #2# :test #'(lambda (a b) (or (and (null a) (null b)) (= a b))))
   (not (tree-equal #1# #3='(1d0 (2d0) ((3d0) (4d0 5d0) (6d0))) :test #'(lambda (a b) (or (and (null a) (null b)) (= a b)))) )
   ;;
   ;;  PAIP pg. 76 SAME-SHAPE-TREE-P
   ;;  "Do trees A and B have the same structure even if values are different?"
   ;;
   (tree-equal '((a) (b) (c d) ((e))) '((:a) (:b) (:c :d) ((:e))) :test (constantly t))
   (not (tree-equal '((a) (b) (c d) ((e))) '((:a) (:b) (:c (:d)) ((:e))) :test (constantly t)))) )

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm
;;;    The value of = is true if all numbers are the same in value; otherwise it is false.
;;;    Two complexes are considered equal by = if their real and imaginary parts are equal according to =.
;;;
;;;    The value of /= is true if no two numbers are the same in value; otherwise it is false.
;;;                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;;
;;;    见 TEST-COMPARE-NUMBERS above.
;;;    见 TEST-COMPARE-NUMBERS /= similar to CHAR/=, CHAR-NOT-EQUAL
;;;    
(deftest test-= ()
  (check
   (= 2)
   (= 2 2)
   (= 2 2 2)
   (= 2d0 2d0)
   (= 2 2d0)
   (= 4/2 #X2 #B10 #O2)
   (= 0d0 -0d0)
   (= #C(4 5) #C(4 5d0) #C(4d0 5d0) #C(4d0 5d0))
   (/= 1)
   (/= 1 2)
   (not (= 1 2 1 1))
   (not (/= 1 2 1 1)) ; Graham ACL 147 页 (/= w x y z) ≡ (and (/= w x) (/= w y) (/= w z) (/= x y) (/= x z) (/= y z)) !!!!
   (not (= (+ 0.1d0 0.1d0 0.1d0 0.1d0 0.1d0 0.1d0) (* 6 0.1d0) 0.6d0))
   (not (/= (+ 0.1d0 0.1d0 0.1d0 0.1d0 0.1d0 0.1d0) (* 6 0.1d0) 0.6d0))))

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

   ;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/13_af.htm
   (or (char> #\z #\A) (char< #\z #\A)) ; Implementation dependent
   (or (char> #\Z #\a) (char< #\Z #\a))
   (equal '(#\A #\a #\b #\B #\c #\C) (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp))

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

   (string= "PUNG" 'pung) ; String designators
   (string= 'foo :foo)
   (string= "x" #\x)
   (string= (make-symbol "PUNG") (make-symbol "PUNG"))
   (string= 'cl-user::foo 'test-lisp::foo)

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
   (eq (cadr #1#) (second #1#))
   (eq (caddr #1#) (third #1#))
   (eq (cadddr #1#) (fourth #1#))
   (eq (car (cddddr #1#)) (fifth #1#))

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
   (let ((l '(1 2 3 4 5)))
     (check
      (equal l (copy-list l))
      (not (eq l (copy-list l))) ; Lists are different
      (every #'eq l (copy-list l)))) ; Elements are the same
   (let ((l '(1 2d0 (a b) "pung" #\Q #(0 1 0))))
     (check
      (equal l (copy-list l))
      (not (eq l (copy-list l))) ; Lists are different
      (every #'eq l (copy-list l)))) )) ; Elements are the same

(deftest test-copy-tree ()
  (check
   (let ((l '(1 2 3 4 5)))
     (equal (copy-list l) (copy-tree l)))
   (let* ((l1 (list (list 1 2) 3 (list (list 4) 5)))
          (l2 (copy-list l1)) ; Shared structure
          (l3 (copy-tree l1))) ; No shared structure
     (check
      (equal l1 l2)
      (equal l1 l3)
      (eq (third l1) (third l2))
      (not (eq (third l1) (third l3)))
      (progn (setf (first (first l1)) :one)
             (equal l1 l2))
      (not (equal l1 l3))
      (equal '(:one 2) (first l1))
      (equal '(:one 2) (first l2))
      (equal '(1 2) (first l3))))
   (let* ((l1 (list (vector 1 2 3) (vector 4 5 6)))
          (l2 (copy-tree l1))) ; Only CONSes copied. Atoms still shared.
     (check
      (equal l1 l2)
      (progn (setf (aref (first l1) 2) 9)
             (equal l1 l2))
      (equals #(1 2 9) (first l1))
      (equals #(1 2 9) (first l2)))) ))

;; (defun copy-tree (tree)
;;   (if (atom tree)
;;       tree
;;       (cons (copy-tree (car tree))
;;             (copy-tree (cdr tree)))) )

;;;
;;;    Contrast LIST-LENGTH below
;;;    
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
   (equal 17 (length "Is this not pung?"))
   (let ((v (make-array 5 :fill-pointer 0)))
     (check
      (zerop (length v))
      (progn (vector-push 1 v)
             (vector-push 2 v)
             (= (length v) 2)))) ))

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
   (equal '(c d) (member '#:c #1# :test #'(lambda (s1 s2) (equal (symbol-name s1) (symbol-name s2)))) )
   (member #\a '(#\a #\e #\i #\o #\u))
   (find #\a "aeiou")))

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
   (let ((l '()))
     (push 1 l)
     (push 2 l)
     (push 3 l)
     (equal '(3 2 1) l))
   (let ((l (mapcar #'list '(1 2 3))))
     (push :one (first l))
     (push :two (second l))
     (push :three (third l))
     (equal '((:one 1) (:two 2) (:three 3)) l))
   (let* ((x (list 1 2 3))
          (y x))
     (push 0 x)
     (eq y (rest x)))) )

(deftest test-pop ()
  (check
   (let ((l (list 1 2 3)))
     (check
      (eql 1 (pop l))
      (eql 2 (pop l))
      (eql 3 (pop l))
      (null l)))
   (let ((l (mapcar #'list '(:one :two :three) '(1 2 3))))
     (check
      (eql :one (pop (first l)))
      (eql :two (pop (second l)))
      (eql :three (pop (third l)))
      (equal '((1) (2) (3)) l)))) )

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
;;      (equal (cons "BAZ" l) (adjoin "BAZ" l))
      (equal l (adjoin "BAZ" l :test #'string-equal))
      (equal l (adjoin "BAZ" l :test #'string= :key #'string-downcase))))
   (let ((l '((1 x) (2 y) (3d0 z))))
     (check
      (equal (cons '(2 :y) l) (adjoin '(2 :y) l))
      (equal l (adjoin '(2 :y) l :key #'first)) ; :KEY applied to <ITEM> _and_ each elt of <LIST>
      (equal (cons '(3 :z) l) (adjoin '(3 :z) l :key #'first))
      (equal l (adjoin '(3 :z) l :key #'first :test #'=)))) ))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/m_pshnew.htm
;;;    The effect of (pushnew item place :test p) is roughly equivalent to (setf place (adjoin item place :test p))
;;;
(deftest test-pushnew ()
  (check
   (let* ((l1 (list 'a 'b 'c))
          (l2 (copy-list l1)))
     (check
      (equal l2 (pushnew 'a l1))
      (equal (cons 'd l2) (pushnew 'd l1))))
   (let* ((l1 (list "foo" "bar" "baz"))
          (l2 (copy-list l1)))
     (check
      (equal (cons "baz" l2) (pushnew "baz" l1))))
   (let* ((l1 (list "foo" "bar" "baz"))
          (l2 (copy-list l1)))
     (check
      (equal l2 (pushnew "baz" l1 :test #'string=))
      (equal l2 (pushnew "BAZ" l1 :test #'string-equal))
      (equal l2 (pushnew "BAZ" l1 :test #'string= :key #'string-downcase))))
   (let* ((l1 (list '(1 x) '(2 y) '(3d0 z)))
          (l2 (copy-list l1)))
     (check
      (equal (cons '(2 :y) l2) (pushnew '(2 :y) l1))))
   (let* ((l1 (list '(1 x) '(2 y) '(3d0 z)))
          (l2 (copy-list l1)))
     (check
      (equal l2 (pushnew '(2 :y) l1 :key #'first)) ; :KEY applied to <ITEM> _and_ each elt of <LIST>
      (equal (cons '(3 :z) l2) (pushnew '(3 :z) l1 :key #'first))))
   (let* ((l1 (list '(1 x) '(2 y) '(3d0 z)))
          (l2 (copy-list l1)))
     (check
      (equal l2 (pushnew '(3 :z) l1 :key #'first :test #'=)))) ))

;;;
;;;    "Set" operations. Questionable for CONS-based sets.
;;;    A more sophisticated implementation should probably be used in most cases...
;;;
;;;    https://en.wikipedia.org/wiki/Algebra_of_sets
;;;    https://en.wikipedia.org/wiki/List_of_set_identities_and_relations
;;;    https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement
;;;
;;;    REMOVE-DUPLICATES (DELETE-DUPLICATES): Arbitrary list -> set
;;;    
(flet ((set-equal (a b &rest keys)
         (and (apply #'subsetp a b keys)
              (apply #'subsetp b a keys)))
       (random-subset (a)
         (loop for elt in a
               when (zerop (random 2)) collect elt))
       (∪ (a b &rest keys) (apply #'union a b keys))
       (∩ (a b &rest keys) (apply #'intersection a b keys))
       (∆ (a b &rest keys) (apply #'set-exclusive-or a b keys)))
  (deftest test-intersection ()
    (check
     (let* ((u (loop for i from 1 to 100 collect i))
            (a (random-subset u))
            (b (random-subset u))
            (c (random-subset u)))
       (check
        (every (compose #'null (partial #'∩ '())) #1=(list a b c u)) ; Empty set
        (every (compose #'null (partial* #'∩ '())) #1#)
        (every #'(lambda (set) (set-equal set (∩ set u))) #1#) ; Universal set
        (every #'(lambda (set) (set-equal set (∩ u set))) #1#)
        (every #'(lambda (set) (set-equal set (∩ set set))) #1#) ; Self
        (every #'(lambda (set1 set2) (set-equal (∩ set1 set2) (∩ set2 set1))) ; Commutative
               (list a a b)
               (list b c c))
        (set-equal (∩ a (∩ b c)) (∩ (∩ a b) c)))) ; Associative
     (let ((a '(a a a)) ; Pathological "sets"
           (b '(b b b b)))
       (check
        (null (∩ '() a))
        (set-equal a (∩ a a))
        (set-equal b (∩ b b))
        (set-equal (∩ a b) (∩ b a))))
     (let ((a '(1 3 5 7))
           (b '(3d0 6d0 8d0)))
       (check
        (null (∩ a b))
        (set-equal '(3) (∩ a b :test #'=) :test #'=)
        (set-equal '(3) (∩ a b :key (partial* #'coerce 'double-float)) :key (partial* #'coerce 'double-float))))
     (let ((a '("A" "B" "C" "D"))
           (b '("d" "c" "a" "b")))
       (check
        (null (∩ a b))
        (set-equal a (∩ a b :test #'string-equal) :test #'string-equal)
        (set-equal a (∩ a b :key #'string-downcase :test #'string=) :key #'string-downcase :test #'string=)
        (set-equal a (∩ a b :test #'equalp) :test #'equalp)
        (set-equal a (∩ a b :key #'string-downcase :test #'equal) :key #'string-downcase :test #'equal)))) )

  (deftest test-union ()
    (check
     (let* ((u (loop for i from 1 to 100 collect i))
            (a (random-subset u))
            (b (random-subset u))
            (c (random-subset u)))
       (check
        (every #'(lambda (set) (set-equal set (∪ '() set))) #1#) ; Empty set
        (every #'(lambda (set) (set-equal set (∪ set '()))) #1#)
        (every #'(lambda (set) (set-equal u (∪ set u))) #1#) ; Universal set
        (every #'(lambda (set) (set-equal u (∪ u set))) #1#)
        (every #'(lambda (set) (set-equal set (∪ set set))) #1#) ; Self
        (every #'(lambda (set1 set2) (set-equal (∪ set1 set2) (∪ set2 set1))) ; Commutative
               (list a a b)
               (list b c c))
        (set-equal (∪ a (∪ b c)) (∪ (∪ a b) c)))) ; Associative
     (let ((a '(a a a)) ; Pathological "sets"
           (b '(b b b b)))
       (check
        (set-equal a (∪ '() a))
        (set-equal (∪ a a) (∪ a a))
        (set-equal (∪ b b) (∪ b b))
        (set-equal (∪ a b) (∪ b a))))
     (let ((a '(1 3 5 7))
           (b '(3d0 6d0 8d0)))
       (check
        (set-equal (append a b) (∪ a b))
        (set-equal '(1 3 5 7 6 8) (∪ a b :test #'=) :test #'=)
        (set-equal '(1 3 5 7 6 8) (∪ a b :key (partial* #'coerce 'double-float)) :key (partial* #'coerce 'double-float))
        (not (set-equal (append a b) (∪ a b :test #'=)))
        (or (not (member 3 (∪ a b :test #'=)))
            (not (member 3d0 (∪ a b :test #'=)))) ))
     (let ((a '("A" "B" "C" "D"))
           (b '("d" "c" "a" "b")))
       (check
        (set-equal (append a b) (∪ a b))
        (set-equal (remove-duplicates (append a b) :test #'string-equal)
                   (∪ a b :test #'string-equal) :test #'string-equal)
        (set-equal a (∪ a b :test #'string-equal) :test #'string-equal)
        (set-equal a (∪ a b :key #'string-downcase :test #'string=) :key #'string-downcase :test #'string=)
        (set-equal a (∪ a b :test #'equalp) :test #'equalp)
        (set-equal a (∪ a b :key #'string-downcase :test #'equal) :key #'string-downcase :test #'equal)))
     (set-equal '((x 5) (y 6) (z 2)) (∪ '((x 5) (y 6)) '((z 2) (x 4)) :key #'car) :key #'car))) ; CLHS example

  ;;    TODO: Fix LOOP result value
  (deftest test-set-difference ()
    (check
     (loop repeat 10
           for u = (loop for i from 1 to 100 collect i)
           for a = (random-subset u)
           for b = (random-subset u)
           for c = (random-subset u)
           do (check
               (set-equal (set-difference c (∩ a b))
                          (∪ (set-difference c a) (set-difference c b)))
               (set-equal (set-difference c (∪ a b))
                          (∩ (set-difference c a) (set-difference c b)))
               (set-equal (set-difference c (set-difference b a))
                          (∪ (∩ c a) (set-difference c b)))
               (set-equal (set-difference c (set-difference c a))
                          (∩ c a))
               (set-equal (∩ (set-difference b a) c)
                          (set-difference (∩ b c) a))
               (set-equal (∩ (set-difference b a) c)
                          (∩ b (set-difference c a)))
               (set-equal (∪ (set-difference b a) c)
                          (set-difference (∪ b c) (set-difference a c)))
               (null (set-difference a a))
               (null (set-difference '() a))
               (set-equal a (set-difference a '()))
               (null (set-difference a u))
               (if (subsetp a b) (subsetp (set-difference c b) (set-difference c a)) t)))) )
  (deftest test-set-exclusive-or ()
    (check
     (loop repeat 10
           for u = (loop for i from 1 to 100 collect i)
           for a = (random-subset u)
           for b = (random-subset u)
           do (check
               (set-equal (set-difference (∪ a b) (∩ a b)) (∆ a b))
               (set-equal (∪ (set-difference a b) (set-difference b a)) (∆ a b)))) )))

(deftest test-subsetp ()
  (check
   (subsetp '() '())
   (subsetp '() #1='(a b c))
   (subsetp #1# #1#)
   (subsetp #1# (reverse #1#))
   (not (subsetp '("pung" "bar") #2='("pung" "foo" "bar" "baz")))
   (subsetp '("pung" "bar") #2# :test #'string=)
   (not (subsetp '("PUNG" "Bar") #2# :test #'string=))
   (subsetp '("PUNG" "Bar") #2# :test #'string-equal)
   (subsetp '("PUNG" "Bar") #2# :test #'string= :key #'string-downcase)
   (handler-case (subsetp '(d) '(a b . c))
     (type-error () t)
     (:no-error () (error "Arguments must be proper lists")))) )

;;;
;;;    Unusual :TEST functions can lead beyond normal set-theoretic concepts.
;;;    What does this mean?
;;;    (subsetp '(1 2 3) '(1 2 3 4 5) :test #'<) => T

;; (subsetp '(1d0 2d0 3d0) '(1 2 3 4 5) :test #'(lambda (a b) (print (list a b)) (< a b)))

;; (1.0d0 1) 
;; (1.0d0 2) 
;; (2.0d0 1) 
;; (2.0d0 2) 
;; (2.0d0 3) 
;; (3.0d0 1) 
;; (3.0d0 2) 
;; (3.0d0 3) 
;; (3.0d0 4) 
;; T

;;;    Evidently, each element of the first list is compared to see if the :TEST is true
;;;    for some element of the second list.

;; (subsetp '(1.1d0 2.1d0 3.1d0) '(1 2 3 4 5) :test #'(lambda (a b) (print (list a b)) (< a b)))

;; (1.1d0 1) 
;; (1.1d0 2) 
;; (2.1d0 1) 
;; (2.1d0 2) 
;; (2.1d0 3) 
;; (3.1d0 1) 
;; (3.1d0 2) 
;; (3.1d0 3) 
;; (3.1d0 4) 
;; T

;;;
;;;    Rare maps
;;;
;; (apropos 'map :cl)
;; MAP (fbound)
;; MAP-INTO (fbound)
;; MAPC (fbound)
;; MAPCAN (fbound)
;; MAPCAR (fbound)
;; MAPCON (fbound)
;; MAPHASH (fbound)
;; MAPL (fbound)
;; MAPLIST (fbound)

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm
;;;    (mapcon f x1 ... xn) ≡ (apply #'nconc (maplist f x1 ... xn))
;;;    (mapcan f x1 ... xn) ≡ (apply #'nconc (mapcar f x1 ... xn))
;;;
(deftest test-maps ()
  (check
   (let ((a (list 1 2 3))
         (b (list 4 5 6)))
     (check
      (equal '((1 4) (2 5) (3 6)) (mapcar #'list a b))
      (equal '(1 4 2 5 3 6) (mapcan #'list a b))
      (equal '((1 :P) (2 :Q) (3 :R) (4 :S) (5 :T))
             (mapcar #'list '(1 2 3 4 5) '(:p :q :r :s :t)))
      (equal '(1 :P 2 :Q 3 :R 4 :S 5 :T)
             (mapcan #'list '(1 2 3 4 5) '(:p :q :r :s :t)))

      (equal '(((1 2 3) (4 5 6)) ((2 3) (5 6)) ((3) (6)))
             (maplist #'list a b))
      (equal '((1 2 3) (4 5 6) (2 3) (5 6) (3) (6))
             (mapcon #'list a b))
      (equal '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))
             (maplist #'identity '(1 2 3 4 5)))
      (equal '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))
             (mapcon #'list '(1 2 3 4 5))) ; Too much CONSing
      (equal '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))
             (loop for cons on '(1 2 3 4 5)
                   collect cons))
      ;; (mapcon #'identity '(1 2 3 4 5)) ; Whoops! List surgery causes infinite loop...
      (equal '(1 2 3 4 5 2 3 4 5 3 4 5 4 5 5)
             (mapcon #'copy-list '(1 2 3 4 5)))) )
   (equal (remove-if-not #'numberp #1='(a 1 b c 3 4 d 5))
          (mapcan #'(lambda (elt) (if (numberp elt) (list elt) '())) #1#)))) ; Old school filter idiom!!

(deftest test-mapcar ()
  (check
   (equal '(2 3 4 5 6) (mapcar #'1+ '(1 2 3 4 5)))
   (equal '(#\A #\B #\C) (mapcar #'char-upcase '(#\a #\b #\c)))
   (equals '(#(1 :P) #(2 :Q) #(3 :R) #(4 :S) #(5 :T))
           (mapcar #'vector '(1 2 3 4 5) '(:p :q :r :s :t)))
   (let ((l '(a b c d e))) ; "Chain" together successive elements
     (equal '((a b) (b c) (c d) (d e))
            (mapcar #'list l (rest l)))) ))

(deftest test-maplist ()
  (check
   (equal '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))
          (maplist #'identity '(1 2 3 4 5)))
   (equal (loop for l on '(1 2 3 4 5) collect l)
          (maplist #'identity '(1 2 3 4 5)))
   (equal '(2 3 4 5 6)
          (maplist #'(lambda (l) (1+ (first l))) '(1 2 3 4 5)))
   (equal (loop for n in '(1 2 3 4 5) collect (1+ n))
          (maplist #'(lambda (l) (1+ (first l))) '(1 2 3 4 5)))
   (equal '(2 3 4 5 6)
          (maplist (compose #'1+ #'first) '(1 2 3 4 5)))
   (equal '(#\A #\B #\C)
          (maplist #'(lambda (l) (char-upcase (first l))) '(#\a #\b #\c)))
   (equal '(#\A #\B #\C)
          (maplist (compose #'char-upcase #'first) '(#\a #\b #\c)))) )

(deftest test-map ()
  (check
   (equals '#(1.4142135 2.0 2.4494898 2.828427)
           (map 'vector #'sqrt #(2 4 6 8)))
   (equal '(1.4142135 2.0 2.4494898 2.828427)
          (map 'list #'sqrt #(2 4 6 8)))
   (equals '#(1 3 3 5)
           (map 'vector #'+ '(1 2 3 4) #(0 1 0 1)))
   (equal '("CCC" "aa" "t")
          (map 'list #'(lambda (ch n) (make-string n :initial-element ch)) "Cat" '(3 2 1)))
   (flet ((scale (alpha x) (* alpha x)))
     (equals #(2.759386672047811d0 0.8635113660339563d0 2.81095652724814d0 0.9000423886947357d0 0.6632763486385608d0)
             (map 'vector (partial #'scale 3.4d0) #(0.8115843153081796d0 0.2539739311864577d0 0.8267519197788646d0 0.26471834961609875d0 0.19508127901134142d0))))
   (flet ((rot-13 (ch)
            (cond ((char<= #\A (char-upcase ch) #\M) (code-char (+ (char-code ch) 13)))
                  ((char<= #\N (char-upcase ch) #\Z) (code-char (- (char-code ch) 13)))
                  (t ch))))
     (string= "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
              (map 'string #'rot-13 "The quick brown fox jumps over the lazy dog.")))) )

(deftest test-side-effect-mapping ()
  (check
   (let ((dummy '())) ; CLHS example
     (mapl #'(lambda (x) (push x dummy)) '(1 2 3 4)) ; Push successive CONSes
     (equal '((4) (3 4) (2 3 4) (1 2 3 4)) dummy))
   (let* ((s (make-string-output-stream))
          (*standard-output* s))
     (equal '(#\I #\s #\  #\t #\h #\i #\s #\  #\n #\o #\t #\  #\p #\u #\n #\g #\?)
            (mapc (compose #'write-char #'char-upcase) (coerce "Is this not pung?" 'list)))
     (string= "IS THIS NOT PUNG?" (get-output-stream-string s)))
   ;;
   ;;    Two equivalents
   ;;    
   (let* ((s (make-string-output-stream))
          (*standard-output* s))
     (loop for ch across "Is this not pung?"
           do (write-char (char-upcase ch)))
     (string= "IS THIS NOT PUNG?" (get-output-stream-string s)))
   (let* ((s (make-string-output-stream))
          (*standard-output* s))
     (loop for ch in (coerce "Is this not pung?" 'list)
           do (write-char (char-upcase ch)))
     (string= "IS THIS NOT PUNG?" (get-output-stream-string s)))
   (let* ((s (make-string-output-stream))
          (*standard-output* s))
     (map nil (compose #'print #'sqrt) '(2 4 6 8))
     (string= "
1.4142135 
2.0 
2.4494898 
2.828427 "
              (get-output-stream-string s)))) )

;;;
;;;    NIL and () are "interchangeable" except in the sense that () is not
;;;    valid syntax for a symbol:
;;;    ✔ COMMON-LISP:NIL
;;;    vs.
;;;    ✘ COMMON-LISP:()
;;;    
(deftest test-nil ()
  (check
   ;;  NIL is a symbol.
   (symbolp 'nil)
   (string= "NIL" (symbol-name 'nil))
   (string= "NIL" (symbol-name '()))
   (string= "COMMON-LISP" (package-name (symbol-package 'nil)))
   ;;  NIL is a boolean value.
   (typep 'nil 'boolean)
   ;;  NIL is a list. The only object that is both a symbol and a list.
   (listp 'nil)
   (listp '())
   ;;  NIL represents the empty list: ()
   (zerop (length 'nil))
   (null 'nil)
   (null ())
   (null '())
   ;;  NIL marks the end of a CONS cell chain for a proper list.
   ;;  It is omitted from the printed representation.
   (null (last '(a b c d) 0))
   (null (rest (rest (rest (rest '(a b c d)))) ))
   (null (cddddr '(a b c d)))
   (equal '(a b c d) '(a b c d . ()))
   ;;  NIL is an atom. The only list that is not a CONS.
   (atom 'nil)
   (not (consp nil))
   (not (consp ()))
   ;;  NIL is a constant variable whose value is itself. See diagrams below.
   (eq nil nil)
   (eq 'nil nil)
   (eq nil 'nil)
   (eq 'nil 'nil)
   ;;  The CAR and CDR of NIL are both defined to be NIL. (Controversial in Common Lisp.)
   (eq nil (car nil))
   (eq nil (cdr nil))
   (eq nil (car (car nil)))
   (eq nil (cdr (cdr nil)))
   ;;  NIL is a value of type NULL
   ;;  CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/t_nil.htm
   ;;  The type containing the object nil is the type null, not the type nil.
   (typep nil 'null)
   ;;  NIL is a type (not a class).
   ;;  The type nil contains no objects and so is also called the empty type.
   ;;  The type nil is a subtype of every type. No object is of type nil. 
   (not (typep nil 'nil))
;;   (null (typep nil 'nil)) ;; !!
   (find-class 'null)
   (not (find-class 'nil nil))))

;; [*|*]--->[*|*]--->[*|*]--->NIL
;;  |        |        |
;;  v        v        v
;;  EQ      NIL      NIL

;; [*|*]--->[*|*]------------------>[*|*]--->NIL
;;  |        |                       |
;;  v        v                       v
;;  EQ      [*|*]--->[*|*]--->NIL   NIL
;;           |        |
;;           v        v
;;          QUOTE    NIL

;; [*|*]--->[*|*]--->[*|*]--->NIL
;;  |        |        |
;;  v        v        v
;;  EQ      NIL      [*|*]--->[*|*]--->NIL
;;                    |        |
;;                    v        v
;;                   QUOTE    NIL

;; [*|*]--->[*|*]------------------>[*|*]--->NIL
;;  |        |                       |
;;  v        v                       v
;;  EQ      [*|*]--->[*|*]--->NIL   [*|*]--->[*|*]--->NIL
;;           |        |              |        |
;;           v        v              v        v
;;          QUOTE    NIL            QUOTE    NIL
   
(deftest test-constants ()
  (check
   (constantp t)
   (constantp nil)
   (constantp pi)
   (constantp :k)
   (progn
     (defconstant syzygy 99)
     (and (constantp syzygy)
          (handler-case (makunbound 'syzygy)
            (error () t)
            (:no-error () (error "Maybe this is implementation-specific?")))) )))

;;;
;;;    (rplaca l obj) ≡ (setf (first l) obj)
;;;    
(deftest test-rplaca ()
  (check
   (let ((l (list 1 (list 2 5/2) 3 4)))
     (check
      (equal '(1d0 (2 5/2) 3 4) (rplaca l 1d0))
      (equal '(2d0 5/2) (rplaca (second l) 2d0))
      (equal '(1d0 (2d0 5/2) 3 4) l)
      (equal '(2.5d0) (rplaca (cdadr l) 2.5d0))
      (equal '(1d0 (2d0 2.5d0) 3 4) l)
      (equal '(3d0 4) (rplaca (cddr l) 3d0))
      (equal '(1d0 (2d0 2.5d0) 3d0 4) l)))
   (let ((l (list 1 (list 2 5/2) 3 4)))
     (check
      (progn (setf (first l) 1d0)
             (equal '(1d0 (2 5/2) 3 4) l))
      (progn (setf (first (second l)) 2d0)
             (equal '(1d0 (2d0 5/2) 3 4) l))
      (progn (setf (second (second l)) 2.5d0)
             (equal '(1d0 (2d0 2.5d0) 3 4) l))
      (progn (setf (third l) 3d0)
             (equal '(1d0 (2d0 2.5d0) 3d0 4) l))))
   (handler-case (let ((l (list)))
                   (rplaca l :foo))
     (error () t)
     (:no-error () (error "Argument to RPLACA must be CONS.")))) )

;;;
;;;    (rplacd l obj) ≡ (setf (rest l) obj)
;;;    
(deftest test-rplacd ()
  (check
   (let ((l (list 1 (list 2 5/2) 3 4)))
     (check
      (equal '(2 2.5d0) (rplacd (second l) (list 2.5d0)))
      (equal '(1 (2 2.5d0) 3 4) l)
      (equal '((2 2.5d0) 3d0 4) (rplacd (cdr l) (list 3d0 4)))
      (equal '(1 (2 2.5d0) 3d0 4) l)))
   (let ((l (list 1 (list 2 5/2) 3 4)))
     (check
      (progn (setf (rest (second l)) (list 2.5d0))
             (equal '(1 (2 2.5d0) 3 4) l))
      (progn (setf (rest (rest l)) (list 3d0 4))
             (equal '(1 (2 2.5d0) 3d0 4) l))))
   (handler-case (let ((l (list)))
                   (rplacd l :foo))
     (error () t)
     (:no-error () (error "Argument to RPLACD must be CONS.")))) )

(deftest test-incf ()
  (check
   (let ((x 8))
     (check
      (= (1+ 8) (incf x) x)
      (= 20 (incf x 11) x)
      (= 16 (incf x -4) x)))
   (let ((l (list 1d0 2d0 3d0)))
     (check
      (= 3d0 (incf (second l)) (second l))
      (= 3.1d0 (incf (third l) 0.1d0) (third l))))
   (let ((a (vector 1/2 1/3 1/4 1/5)))
     (check
      (= 6/5 (incf (aref a 3)) (aref a 3))
      (= 3/4 (incf (aref a 2) 1/2) (aref a 2)))) ))

(deftest test-decf ()
  (check
   (let ((x 8))
     (check
      (= (1- 8) (decf x) x)
      (= -4 (decf x 11) x)
      (= 0 (decf x -4) x)))
   (let ((l (list 1d0 2d0 3d0)))
     (check
      (= 1d0 (decf (second l)) (second l))
      (= 2.9d0 (decf (third l) 0.1d0) (third l))))
   (let ((a (vector 1/2 1/3 1/4 1/5)))
     (check
      (= -4/5 (decf (aref a 3)) (aref a 3))
      (= -1/4 (decf (aref a 2) 1/2) (aref a 2)))) ))

;;;
;;;    Ruby comparison:
;;;    (rotatef p q r)             vs. p, q, r = q, r, p
;;;    (rotatef a b) (rotatef b a) vs. a, b, c, d = b, a, d, c
;;;    Alternatively: (psetf a b b a c d d c)
;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/m_rotate.htm#rotatef
;;;    The effect of (rotatef place1 place2 ... placen) is roughly equivalent to
;;;    (psetf place1 place2
;;;           place2 place3
;;;           ...
;;;           placen place1)
;;;    except that the latter would evaluate any subforms of each place twice, whereas rotatef evaluates them once.
;;;    
(deftest test-rotatef ()
  (check
   (let ((x 1) ; Swap variables
         (y 2))
     (rotatef x y)
     (and (= x 2)
          (= y 1)))
   (let ((l (list 1 2 3 4)))
     (rotatef (first l) (second l) (third l) (fourth l))
     (equal '(2 3 4 1) l))
   (let ((l1 (list 'pung 'foo))
         (l2 (list 'bar 'baz)))
     (rotatef (first l1) (first l2))
     (and (equal '(bar foo) l1)
          (equal '(pung baz) l2)))
   ;;  All the same rotation
   (equalelts (list (let ((a 1) (b 2) (c 3)) (rotatef a b c) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (rotatef b c a) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (rotatef c a b) (list a b c))))
   ;;  A different equivalence
   (equalelts (list (let ((a 1) (b 2) (c 3)) (rotatef a c b) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (rotatef c b a) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (rotatef b a c) (list a b c))))
   ;;  Two separate rotations required
   (equalelts (list (let ((a 2) (b 2) (c 3) (d 4)) (psetf a b b a c d d c) (list a b c d))
                    (let ((a 2) (b 2) (c 3) (d 4)) (psetf b a a b d c c d) (list a b c d))
                    (let ((a 2) (b 2) (c 3) (d 4)) (rotatef a b) (rotatef c d) (list a b c d))))
   (let ((a (vector 1 2 3 4 5 6)))
     (rotatef (aref a 1) (aref a 3) (aref a 5)) ; Perl: @a[1, 3, 5] = @a[5, 1, 3];
     (equals #(1 4 3 6 5 2) a))
   (let* ((l1 (list 1 2 3 4))
          (l2 (cons 0 l1)))
     (rotatef (first l1) (second l1)) ; Destructive change affects shared structure
     (and (equal '(2 1 3 4) l1)
          (equal '(0 2 1 3 4) l2)))) )

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/m_shiftf.htm
;;;    The effect of (shiftf place1 place2 ... placen newvalue) is roughly equivalent to
;;;    (let ((var1 place1)   
;;;          (var2 place2)   
;;;          ...             
;;;          (varn placen)   
;;;          (var0 newvalue))
;;;      (setf place1 var2)  
;;;      (setf place2 var3)  
;;;      ...                 
;;;      (setf placen var0)  
;;;      var1)               
;;;    except that the latter would evaluate any subforms of each place twice, whereas shiftf evaluates them once.
;;;
;;;    (rotatef p1 p2 ... pn2) and (shiftf p1 p2 ... pn p1) have same side effects.
;;;    (Different return values.)
;;;    
(deftest test-shiftf ()
  (check
   (let ((x 1) ; Swap variables
         (y 2))
     (= 1 (shiftf x y x))
     (and (= x 2)
          (= y 1)))
   (let ((x 1)
         (y 2))
     (= 1 (shiftf x y 3))
     (and (= x 2)
          (= y 3)))
   (let ((l (list 1 2 3 4)))
     (= 1 (shiftf (first l) (second l) (third l) (fourth l) :one))
     (equal '(2 3 4 :one) l))
   (let ((l1 (list 'pung 'foo))
         (l2 (list 'bar 'baz)))
     (equal 'pung (shiftf (first l1) (first l2) :pung))
     (and (equal '(bar foo) l1)
          (equal '(:pung baz) l2)))
   ;;  All the same shift (Just use ROTATEF???)
   (equalelts (list (let ((a 1) (b 2) (c 3)) (shiftf a b c a) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (shiftf b c a b) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (shiftf c a b c) (list a b c))))
   ;;  A different equivalence
   (equalelts (list (let ((a 1) (b 2) (c 3)) (shiftf a c b a) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (shiftf c b a c) (list a b c))
                    (let ((a 1) (b 2) (c 3)) (shiftf b a c b) (list a b c))))
   ;;  Two separate shifts required
   (equalelts (list (let ((a 2) (b 2) (c 3) (d 4)) (psetf a b b a c d d c) (list a b c d))
                    (let ((a 2) (b 2) (c 3) (d 4)) (psetf b a a b d c c d) (list a b c d))
                    (let ((a 2) (b 2) (c 3) (d 4)) (shiftf a b a) (shiftf c d c) (list a b c d))))
   (let ((a (vector 1 2 3 4 5 6)))
     (= 2 (shiftf (aref a 1) (aref a 3) (aref a 5) :two))
     (equals #(1 4 3 6 5 :two) a))
   (let* ((l1 (list 1 2 3 4))
          (l2 (cons 0 l1)))
     (= 1 (shiftf (first l1) (second l1) :one)) ; Destructive change affects shared structure
     (and (equal '(2 :one 3 4) l1)
          (equal '(0 2 :one 3 4) l2)))
   (flet ((fibonacci (n) ; Cycle local variables
            (let ((current 0)
                  (next 1))
              (dotimes (i n current)
                (shiftf current next (+ current next)))) ))
     (check
         (= 0 (fibonacci 0))
         (= 1 (fibonacci 1))
         (= 1 (fibonacci 2))
         (= 5 (fibonacci 5)) 
         (= 55 (fibonacci 10)) 
         (= 610 (fibonacci 15)) 
         (= 6765 (fibonacci 20))
         (= 453973694165307953197296969697410619233826 (fibonacci 201)))) ))


;;;
;;;    Contrast LENGTH above. LIST-LENGTH deals with potentially circular lists.
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_list_l.htm
;;;
(deftest test-list-length ()
  (flet ((circular-list (&rest elements) ; CLHS example
           (let ((cycle (copy-list elements))) 
             (nconc cycle cycle))))
    (check
     (zerop (list-length '()))
     (= 4 (list-length '(a b c d)))
     (null (list-length (circular-list 'a)))
     (null (list-length (circular-list 'a 'b)))
     (null (list-length (circular-list 'a 'b 'c)))
     (null (list-length (circular-list 'a 'b 'c 'd)))
     (handler-case (list-length '(a b c . d))
       (type-error () t)
       (:no-error () (error "List must either be a proper list or a circular list.")))) ))
    
;;;
;;;    CLHS shows this possible implementation. Two pointers traverse the list.
;;;    For a proper list, the FAST pointer will eventually reach the terminal CONS.
;;;    For a circular list, FAST will "wrap around" and catch up to SLOW. In fact,
;;;    they will both coincide when pointing to the first element after SLOW has
;;;    wrapped around once and FAST has wrapped around twice.
;;;    
;; (defun list-length (x)  
;;   (do ((n 0 (+ n 2))
;;        (fast x (cddr fast))
;;        (slow x (cdr slow)))
;;       (nil)
;;     (when (endp fast) (return n))
;;     (when (endp (cdr fast)) (return (+ n 1)))
;;     (when (and (eq fast slow) (> n 0)) (return nil))))

;; (defun list-length* (x)  
;;   (do ((n 0 (+ n 2))
;;        (fast x (cddr fast))
;;        (slow x (cdr slow)))
;;       (nil)
;;     (print n)
;;     (when (endp fast) (return n))
;;     (when (endp (cdr fast)) (return (+ n 1)))
;;     (when (and (eq fast slow) (> n 0)) (return nil))))

;; (defun circular-list (&rest elements)
;;    (let ((cycle (copy-list elements))) 
;;      (nconc cycle cycle)))

;; (list-length* (circular-list 'a))

;; 0 
;; 2 
;; NIL
;; * (list-length* (circular-list 'a 'b))

;; 0 
;; 2 
;; 4 
;; NIL
;; * (list-length* (circular-list 'a 'b 'c))

;; 0 
;; 2 
;; 4 
;; 6 
;; NIL
;; * (list-length* (circular-list 'a 'b 'c 'd))

;; 0 
;; 2 
;; 4 
;; 6 
;; 8 
;; NIL

;;;
;;;    CLHS:
;;;    SET changes the contents of the value cell of SYMBOL to the given VALUE.
;;;    (set symbol value) ≡ (setf (symbol-value symbol) value)
;;;
;;;     The function SET is deprecated.
;;;     SET cannot change the value of a lexical variable.
;;;     
(deftest test-setters ()
  (check
   (let ((x 8))
     (check
      (progn (setq x 9)
             (= 9 x))
      (progn (setf x 10)
             (= 10 x)))) 
   (let ((x 8))
     (declare (special x))
     (check
      (progn (set 'x 9)
             (= 9 x))
      (progn (setf x 10)
             (= 10 x)))) 
   (let ((x 8)
         (y 'x))
     (declare (special x))
     (check
      (progn (set y 9)
             (= 9 x))))
   (let ((x 8)
         (y (list 'x)))
     (declare (special x))
     (check
      (progn (set (first y) 9)
             (and (= 9 x)
                  (equal '(x) y)))
      (progn (setf (first y) 10)
             (and (= 9 x)
                  (equal '(10) y)))) )))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/m_psetq.htm
;;;    See ROTATEF above.
;;;    
(deftest test-parallel-setters ()
  (check
   (let ((a 3) ; Failed swap
         (b 4))
     (setf a b b a)
     (= 4 a b))
   (let ((a 3) ; Swap
         (b 4))
     (psetq a b b a)
     (and (= 4 a) (= 3 b)))
   (let ((a 3) ; Swap
         (b 4))
     (psetf a b b a)
     (and (= 4 a) (= 3 b)))
   (let ((a 1) ; CLHS example
         (b 2)
         (c 3))
     (psetq a (1+ b) b (1+ a) c (+ a b))
     (and (= 3 a c) (= 2 b)))
   (let ((x (list 10 20 30))) ; CLHS - Use of PSETQ on a symbol macro.
     (symbol-macrolet ((y (car x)) (z (cadr x)))
       (psetq y (1+ z) z (1+ y))
       (equal '((21 11 30) 21 11) (list x y z))))
   (let ((l (list 'a 'b 'c))) ; PSETQ only works on symbols. PSETF can take generalized vars.
     (psetf (first l) (second l) (second l) (first l))
     (equal '(b a c) l))))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/m_defi_2.htm
;;;    Except for the issue of avoiding multiple evaluation (see below), the expansion of a define-modify-macro is equivalent to the following: 
;;;     (defmacro name (reference . lambda-list)                                                                                                
;;;       documentation                                                                                                                         
;;;       `(setf ,reference                                                                                                                     
;;;              (function ,reference ,arg1 ,arg2 ...)))                                                                                        
;;;

;;;
;;;    (timesf x 9) ≈ (setf x (funcall #'* x 9))
;;;    
(define-modify-macro timesf (multiplier) *)

;;;
;;;    (addf x 5 10) ≈ (setf x (funcall #'+ x 5 10))
;;;    
(define-modify-macro addf (&rest numbers) +)

;; (macroexpand-1 '(define-modify-macro addf (&rest numbers) +))
;; (DEFMACRO ADDF (#:PLACE &REST NUMBERS &ENVIRONMENT #:ENV)
;;   (SB-IMPL::EXPAND-RMW-MACRO '+ 'NIL #:PLACE (LIST* NUMBERS) T #:ENV 'NIL))
;; T

;;;
;;;    (reversef l) ≈ (setf l (funcall #'reverse l))
;;;    
(define-modify-macro reversef () reverse)

(deftest test-define-modify-macro ()
  (check
   (let ((x 7))
     (timesf x 2)
     (= 14 x))
   (let ((x 4))
     (addf x 1 2 3)
     (= x 10))
   (let* ((x (list 1 2 3))
          (y x)
          (z x))
     (reversef x)
     (and (equal '(3 2 1) x)
          (equal '(1 2 3) y)
          (not (eq x y))
          (eq y z)
          (progn (setf y (nreverse y))
                 (equal x y)))) ))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/12_acc.htm
;;;    12.1.3.3 Rule of Float Substitutability
;;;
;;;    When the arguments to an irrational mathematical function are all rational and the true mathematical result
;;;    is also (mathematically) rational, then unless otherwise noted an implementation is free to return either
;;;    an accurate rational result or a single float approximation.
;;;    If the arguments are all rational but the result cannot be expressed as a rational number,
;;;    then a single float approximation is always returned.
;;;
;;;    Function  Sample Results                                   
;;;    abs       (abs #c(3 4)) =>  5 or 5.0                       
;;;    acos      (acos 1) =>  0 or 0.0                            
;;;    acosh     (acosh 1) =>  0 or 0.0                           
;;;    asin      (asin 0) =>  0 or 0.0                            
;;;    asinh     (asinh 0) =>  0 or 0.0                           
;;;    atan      (atan 0) =>  0 or 0.0                            
;;;    atanh     (atanh 0) =>  0 or 0.0                           
;;;    cis       (cis 0) =>  1 or #c(1.0 0.0)                     
;;;    cos       (cos 0) =>  1 or 1.0                             
;;;    cosh      (cosh 0) =>  1 or 1.0                            
;;;    exp       (exp 0) =>  1 or 1.0                             
;;;    expt      (expt 8 1/3) =>  2 or 2.0                        
;;;    log       (log 1) =>  0 or 0.0                             
;;;              (log 8 2) =>  3 or 3.0                           
;;;    phase     (phase 7) =>  0 or 0.0                           
;;;    signum    (signum #c(3 4)) =>  #c(3/5 4/5) or #c(0.6 0.8)  
;;;    sin       (sin 0) =>  0 or 0.0                             
;;;    sinh      (sinh 0) =>  0 or 0.0                            
;;;    sqrt      (sqrt 4) =>  2 or 2.0                            
;;;              (sqrt 9/16) =>  3/4 or 0.75                      
;;;    tan       (tan 0) =>  0 or 0.0                             
;;;    tanh      (tanh 0) =>  0 or 0.0
;;;
(deftest test-float-substitutability ()
  (check
   (or (typep (exp 0) 'rational)
       (typep (exp 0) 'single-float))
   (typep (exp (coerce 0 'double-float)) 'double-float)
   (or (typep (expt 32 1/5) 'rational)
       (typep (expt 32 1/5) 'single-float))
   (typep (expt (coerce 32 'double-float) 1/5) 'double-float)
   (or (typep (log 1) 'rational)
       (typep (log 1) 'single-float))
   (typep (log (coerce 1 'double-float)) 'double-float)
   (or (typep (log 32 2) 'rational)
       (typep (log 32 2) 'single-float))
   (typep (log (coerce 32 'double-float) 2) 'double-float)
   (or (typep (sin 0) 'rational)
       (typep (sin 0) 'single-float))
   (typep (sin (coerce 0 'double-float)) 'double-float)
   (or (typep (sqrt 8) 'rational)
       (typep (sqrt 8) 'single-float))
   (typep (sqrt (coerce 8 'double-float)) 'double-float)
   (or (typep (tan 0) 'rational)
       (typep (tan 0) 'single-float))
   (typep (tan (coerce 0 'double-float)) 'double-float)))

(deftest test-make-string ()
  (check
   (string= "XXXXX" (make-string 5 :initial-element #\X))
   (equal (make-string 8) (make-string 8))
   (= 10 (length (make-string 10)))
   (or (string= "        " (make-string 8)) ; Implementation-dependent
       (string/= "        " (make-string 8)))
   (let ((s (make-string 5 :initial-element #\space))) ; Mutable
     (setf (subseq s 0) "Is this not pung?")
     (string= "Is th" s))
   (let ((s (make-string 17 :initial-element #\space)))
     (setf (subseq s 0) "Is this not pung?")
     (string= "Is this not pung?" s))))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_chp.htm
;;;    (characterp object) ≡ (typep object 'character)
;;;    
(deftest test-characterp ()
  (check
   (characterp #\a)
   (not (characterp 'a))
   (not (characterp :a))
   (not (characterp "a"))
   (characterp (code-char 65))
   (characterp (character 'a))
   (characterp (character :a))
   (characterp (character "a"))))
   
;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_stgp.htm
;;;    (stringp object) ≡ (typep object 'string)
;;;
(deftest test-stringp ()
  (check
   (stringp "pung")
   (not (stringp 'pung))
   (not (stringp :pung))
   (not (stringp #\p))
   (stringp (string 'pung))
   (stringp (string :pung))
   (stringp (string #\p))))

(deftest test-alpha-char-p ()
  (check
   (every #'alpha-char-p #[#\a #\z])
   (every #'alpha-char-p #[#\A #\Z])
   (notany #'alpha-char-p #[#\0 #\9])
   (notany #'alpha-char-p #[#\space #\/])))

;;;
;;;    DIGIT-CHAR-P           char->number
;;;    "Is this CHAR a DIGIT?"
;;;    
(deftest test-digit-char-p ()
  (check
   (not (digit-char-p #\?))
   (digit-char-p #\8)
   (not (digit-char-p #\a))
   (digit-char-p #\a 16)
   (digit-char-p #\A 16)
   (not (digit-char-p #\3 2))))

;;;
;;;    DIGIT-CHAR            number->char
;;;    (DIGIT->CHAR)
;;;
(deftest test-digit-char ()
  (check
   (digit-char 9)
   (char= #\9 (digit-char 9))
   (not (digit-char 10))
   (digit-char 10 16)
   (char= #\A (digit-char 10 16)))) ; Always uppercase.

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_alphan.htm
;;;    (alphanumericp x) ≡ (or (alpha-char-p x) (not (null (digit-char-p x))))
;;;
(deftest test-alphanumericp ()
  (check
   (every #'alphanumericp #[#\a #\z])
   (every #'alphanumericp #[#\A #\Z])
   (every #'alphanumericp #[#\0 #\9])
   (notany #'alphanumericp #[#\space #\/])))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/13_adca.htm
;;;
(deftest test-upper-case-p ()
  (check
   (every #'upper-case-p #[#\A #\Z])
   (notany #'upper-case-p #[#\a #\z])))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/13_adcb.htm
;;;
(deftest test-lower-case-p ()
  (check
   (every #'lower-case-p #[#\a #\z])
   (notany #'lower-case-p #[#\A #\Z])))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/13_adc.htm
;;;
(deftest test-both-case-p ()
  (check
   (every #'both-case-p #[#\a #\z])
   (every #'both-case-p #[#\A #\Z])))

(deftest test-standard-char-p ()
  (check
   (every #'standard-char-p #[#\! #\~])
   (every (partial* #'typep 'standard-char) #[#\! #\~])
   (standard-char-p #\space)
   (standard-char-p #\newline)
   (not (standard-char-p #\tab))
   (notany #'standard-char-p "中国的首都是北京")))

(deftest test-graphic-char-p ()
  (check
   (every #'graphic-char-p #[#\! #\~])
   (graphic-char-p #\space)
   (not (graphic-char-p #\newline))
   (not (graphic-char-p #\tab))
   (every #'graphic-char-p "中国的首都是北京")))

(deftest test-change-character-case ()
  (check
   (every #'char= #[#\a #\z] (mapcar #'char-downcase #[#\A #\Z]))
   (every #'char= #[#\A #\Z] (mapcar #'char-upcase #[#\a #\z]))))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_ch.htm
;;;    (character object) ≡ (coerce object 'character)
;;;    
(deftest test-character ()
  (check
   (char= #\A (character #\A))
   (char= #\A (character "A")) ; Character designators https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#character_designator
   (char= #\A (character 'a))
   (char= #\A (character :a))))

(deftest test-char-code-conversion ()
  (check
   (every #'char= #[#\a #\z] (mapcar #'code-char #[(char-code #\a) (char-code #\z)]))
   (every #'char= #[#\A #\Z] (mapcar #'code-char #[(char-code #\A) (char-code #\Z)]))
   (every #'= #[(char-code #\a) (char-code #\z)] (mapcar #'char-code #[#\a #\z]))
   (every #'= #[(char-code #\A) (char-code #\Z)] (mapcar #'char-code #[#\A #\Z]))))

;;;
;;;    CLHS: https://www.lispworks.com/documentation/HyperSpec/Body/f_char_.htm
;;;    (char s j) ≡ (aref (the string s) j)
;;;
(deftest test-char ()
  (check
   (char= #\n (char "pung" 2) (elt "pung" 2) (aref "pung" 2))
   (let ((s (copy-seq "pung")))
     (setf (char s 3) #\k)
     (string= "punk" s))))

(deftest test-concatenate ()
  (check
   (equal '(a b c d e) (concatenate 'list '(a b) '(c d e)))
   (equal "Is this not pung?" (concatenate 'string "Is this " "not" " pung?"))
   (equals #(1 2 3 4) (concatenate 'vector #(1 2) #(3 4)))
   (null (concatenate 'list '() '() '() '()))
   (equals '(#\A #\B #\C D E F 1 2 3 1 0 1 1)
           (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)) ; CLHS example
   (let ((tail '(b c d))) (eq (append tail) tail))
   (let ((tail '(b c d))) (not (eq (concatenate 'list tail) tail))) ; No shared structure
   (let ((tail '(b c d))) (eq (rest (append '(a) tail)) tail))
   (let ((tail '(b c d))) (not (eq (rest (concatenate 'list '(a) tail)) tail)))) )

(deftest test-subseq ()
  (check
   (string= "" (subseq "Is this not pung?" 3 3))
   (string= "pung?" (subseq "Is this not pung?" 12))
   (string= "Is" (subseq "Is this not pung?" 0 2))
   (let ((s (copy-seq "Is this not pung?")))
     (setf (subseq s 3 7) "that")
     (string= "Is that not pung?" s))
   (let ((s (copy-seq "Is this not pung?")))
     (setf (subseq s 3 7) "that thing over there under the couch")
     (string= "Is that not pung?" s))
   (equals #() (subseq (vector 1 2 3 4) 3 3))
   (equals #(2 3 4) (subseq (vector 1 2 3 4) 1))
   (equals #(2 3) (subseq (vector 1 2 3 4) 1 3))
   (let ((v (vector 1 2 3 4)))
     (setf (subseq v 1 3) (list :two :three))
     (equals #(1 :two :three 4) v))
   (let ((v (vector 1 2 3 4)))
     (setf (subseq v 1 3) (list :two))
     (equals #(1 :two 3 4) v))
   (null (subseq '(a b c d e) 3 3))
   (equal '(d e) (subseq '(a b c d e) 3))
   (equal '(b c d) (subseq '(a b c d e) 1 4))
   (let ((l (list 'a 'b 'c 'd 'e)))
     (setf (subseq l 0 3) "ABC")
     (equal '(#\A #\B #\C d e) l))
   (let ((l (list 'a 'b 'c 'd 'e)))
     (setf (subseq l 0 3) '("ABC"))
     (equal '("ABC" b c d e) l))))

(deftest test-position ()
  (check
   (= 4 (position #\Space "Pung Foo"))
   (null (position #\J "Pung Foo"))
   (= 8 (position #\n "Is this not pung?"))
   (= 14 (position #\n "Is this not pung?" :from-end t))
   (= 3 (position 'a '(b c d a)))
   (= 2 (position 'd #(b c d a)))

   (null (position "bar" #1='("pung" "foo" "bar" "baz")))
   (null (position-if #'(lambda (elt) (eql elt "bar")) #1#))
   (= 2 (position "bar" #1# :test #'string=))
   (= 2 (position-if #'(lambda (elt) (string= elt "bar")) #1#))
   (= 2 (position-if (partial #'string= "bar") #1#))

   (= 3 (position 'a #2='((a . 1) (b . 2) (c . 3) (a . 4)) :key #'first :from-end t))
   (= 3 (position-if #'(lambda (elt) (eql (first elt) 'a)) #2# :from-end t))

   (= 2 (position 3 #3='(1 2 3 4 5 6) :test #'(lambda (item elt) (zerop (mod elt item)))) )
   (= 5 (position 3 #3# :test #'(lambda (item elt) (zerop (mod elt item))) :start 3))
   (= 2 (position-if #'(lambda (elt) (zerop (mod elt 3))) #3#))
   (= 2 (position-if (compose #'zerop (partial* #'mod 3)) #3#))
   
   (= 4 (position-if-not #'minusp '(-3 -7 -1 -2 0 5)))
   (= 4 (position-if (complement #'minusp) '(-3 -7 -1 -2 0 5)))
   (null (position-if-not #'minusp '(-3 -7 -1 -2 0 5) :end 3))

   (= 3 (position-if #'(lambda (elt) (> elt 3)) #3#))
   (= 3 (position-if-not #'(lambda (elt) (<= elt 3)) #3#))
   (= 3 (position-if (complement #'(lambda (elt) (<= elt 3))) #3#))))

(deftest test-find ()
  (check
   (eql #\Space (find #\Space "Pung Foo"))
   (null (find #\J "Pung Foo"))
   (null (find #\N "Is this not pung?"))
   (eql #\N (find #\N "Is this Not pung?"))
   (eql #\n (find #\N "Is this Not pung?" :from-end t :test #'char-equal))
   (eq 'a (find 'a '(b c d a)))
   (eq 'd (find 'd #(b c d a)))

   (null (find "bar" #1='("pung" "foo" "bar" "baz")))
   (null (find-if #'(lambda (elt) (eql elt "bar")) #1#))
   (equal "bar" (find "bar" #1# :test #'string=))
   (equal "bar" (find-if #'(lambda (elt) (string= elt "bar")) #1#))
   (equal "bar"  (find-if (partial #'string= "bar") #1#))

   (equal '(a . 4) (find 'a #2='((a . 1) (b . 2) (c . 3) (a . 4)) :key #'first :from-end t))
   (equal '(a . 4) (find-if #'(lambda (elt) (eql (first elt) 'a)) #2# :from-end t))

   (= 3 (find 3 #3='(1 2 3 4 5 6) :test #'(lambda (item elt) (zerop (mod elt item)))) )
   (= 6 (find 3 #3# :test #'(lambda (item elt) (zerop (mod elt item))) :start 3))
   (= 3 (find-if #'(lambda (elt) (zerop (mod elt 3))) #3#))
   (= 3 (find-if (compose #'zerop (partial* #'mod 3)) #3#))
   
   (= 0 (find-if-not #'minusp '(-3 -7 -1 -2 0 5)))
   (= 0 (find-if (complement #'minusp) '(-3 -7 -1 -2 0 5)))
   (null (find-if-not #'minusp '(-3 -7 -1 -2 0 5) :end 3))

   (= 4 (find-if #'(lambda (elt) (> elt 3)) #3#))
   (= 4 (find-if-not #'(lambda (elt) (<= elt 3)) #3#))
   (= 4 (find-if (complement #'(lambda (elt) (<= elt 3))) #3#))

   (null (find nil '(a b nil c d)))
   (null (find nil '(a b c d)))) ) ; D'oh!
   
(deftest test-string-case ()
  (check
   (string= "A" (string-upcase #\a))
   (string= "A" (string-upcase #\A))
   (string= "FOO" (string-upcase 'foo))
   (string= "FOO" (string-upcase 'FOO))
   (string= "FOO" (string-upcase :foo))
   (string= "IS THIS NOT PUNG?" (string-upcase "Is this not pung?"))

   (string= "a" (string-downcase #\A))
   (string= "a" (string-downcase #\a))
   (string= "foo" (string-downcase 'foo))
   (string= "foo" (string-downcase 'FOO))
   (string= "foo" (string-downcase :foo))
   (string= "is this not pung?" (string-downcase "Is this not pung?"))

   (string= "A" (string-capitalize #\a))
   (string= "A" (string-capitalize #\A))
   (string= "Foo" (string-capitalize 'foo))
   (string= "Foo" (string-capitalize 'FOO))
   (string= "Foo" (string-capitalize :foo))
   (string= "Is This Not Pung?" (string-capitalize "Is this not pung?"))
   (string= "Is This Not Pung?" (string-capitalize "IS THIS NOT PUNG?"))
   (string= "D'Oh!" (string-capitalize "D'OH!"))

   (let ((s (copy-seq "Is this not pung?")))
     (nstring-upcase s)
     (string= s "IS THIS NOT PUNG?"))
   (let ((s (copy-seq "Is this not pung?")))
     (nstring-downcase s)
     (string= s "is this not pung?"))
   (let ((s (copy-seq "Is this not pung?")))
     (nstring-capitalize s)
     (string= s "Is This Not Pung?"))))

;;;
;;;    (string obj) ≢(coerce obj 'string)
;;;    
(deftest test-string-coercion ()
  (check
   (string= "a" (string #\a))
   (string= "a" (coerce '(#\a) 'string))
   (handler-case (coerce #\a 'string)
     (type-error () t)
     (:no-error () (error "Cannot coerce character to string.")))
   (equal '(#\p #\u #\n #\g) (coerce "pung" 'list))
   (string= "pung" (coerce '(#\p #\u #\n #\g) 'string))
   (string= "pung" (coerce #(#\p #\u #\n #\g) 'string))
   (string= "pung" (coerce (coerce "pung" 'list) 'string))
   (eq (symbol-name 'bolster) (string 'bolster))
   (eq 'foo (intern "FOO"))))

(deftest test-read-from-string ()
  (check
   (= 8 (read-from-string "8"))
   (eq 'foo (read-from-string "FOO"))
   (eq 'foo (read-from-string "foo"))

   (let ((s "a b"))
     (multiple-value-bind (obj index) (read-from-string s)
       (and (eq 'a obj)
            (eq 'b (read-from-string s nil nil :start index)))) )

   (eq 'pung (read-from-string "pung foo" :start 4)) ; Wrong! Mixing &optional/&key args
   (eq 'foo (read-from-string "pung foo" nil nil :start 4)) ; Correct

   (equal '(+ 2 3) (read-from-string "(+ 2 3)"))
   (consp (read-from-string "(+ 2 3)"))
   (null (read-from-string "(+ 2 3)" nil nil :start 7))

   (equal ''(+ 2 3) (read-from-string "'(+ 2 3)"))
   (= 2 (length (read-from-string "'(+ 2 3)"))) ; I.e., (quote (+ 2 3))
   (eq 'quote (first (read-from-string "'(+ 2 3)")))
   (equal '((+ 2 3) quote) (reverse (read-from-string "'(+ 2 3)")))

   (eq :oops (read-from-string "   " nil :oops))
   (handler-case (read-from-string "    ")
     (end-of-file () t)
     (:no-error () (error "End of file reached before object encountered. EOF-ERROR-P is true by default.")))
   (handler-case (read-from-string ")" nil nil)
     (reader-error () t)
     (:no-error () (error "Object cannot start with ).")))
   (handler-case (read-from-string "(" nil nil)
     (end-of-file () t)
     (:no-error () (error "End of file reached in middle of parsing object.")))

   (let ((*read-default-float-format* 'single-float))
     (eql 4f0 (read-from-string "4.0")))
   (let ((*read-default-float-format* 'double-float))
     (eql 4d0 (read-from-string "4.0")))

   (= 5 (read-from-string "#.(- 9 4)"))
   (let ((*read-eval* nil))
     (handler-case (read-from-string "#.(- 9 4)")
       (reader-error () t)
       (:no-error () (error "Can't read #. while *READ-EVAL* is NIL")))) ))





;; (defn reverse [coll]
;;   (reduce1 conj () coll))


