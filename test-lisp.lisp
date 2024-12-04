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

;;;
;;;    Cannot haphazardly apply Boolean algebra:
;;;    (not (> a b)) == (<= a b)
;;;      but
;;;    (not (> a b c d)) != (<= a b c d)
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
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't make integer something that it's not.")))
   (handler-case (coerce (expt 3 9999) 'fixnum) ; caught STYLE-WARNING: Lisp error during constant folding:
                                        ; 5437833951142086247677... can't be converted to type FIXNUM.
     (type-error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't make integer something that it's not.")))) )

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

(deftest test-cond ()
  (check
   (not (cond))
   (equal '(:foo) (multiple-value-list (cond ((read-from-string "" nil :foo)))) )
   (equal '(:foo 0) (multiple-value-list (cond (t (read-from-string "" nil :foo)))) )))

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
     (division-by-zero (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Since when is this allowed?")))
   (handler-case (/ 3d0 0d0)
     (division-by-zero (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Since when is this allowed?")))
   (handler-case (/ most-positive-double-float least-positive-double-float)
     (floating-point-overflow (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Less is more.")))
   (handler-case (* most-positive-double-float 10d0)
     (floating-point-overflow (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Less is more.")))
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
          '(a :bar 1 2d0 #\k "foo" (a b c) #(1 2)))) )

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
          (equal '() (ldiff l l)))) ))

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
   (eq (nth 9 #1#) (elt #1# 9))))


   
