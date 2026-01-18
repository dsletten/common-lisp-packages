;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               fp.lisp
;;;;
;;;;   Started:            Tue Feb 23 19:53:23 2021
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Define functions in terms of REDUCE...
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
;;;;   fold.pdf
;;;;   A tutorial on the universality and expressiveness of fold GRAHAM HUTTON
;;;;
;;;;   In functional programming, fold (also known as foldr) is a standard recursion
;;;;   operator that encapsulates a common pattern of recursion for processing lists.
;;;;   (Right fold!)
;;;;   fold :: (α → β → β) → β → ([α] → β)
;;;;   fold f v [] = v
;;;;   fold f v (x : xs) = f x (fold f v xs)
;;;;
;;;;   length :: [α] → Int
;;;;   length = fold (λx n → 1 + n) 0
;;;;
;;;;   reverse :: [α] → [α]
;;;;   reverse = fold (λx xs → xs ++ [x]) []
;;;;
;;;;   map :: (α → β) → ([α] → [β])
;;;;   map f = fold (λx xs → f x : xs) []
;;;;
;;;;   filter (α → Bool) → ([α] → [α])
;;;;   filter p = fold (λx xs → if p x then x : xs else xs) []
;;;;
;;;;   By contrast:
;;;;   foldl :: (β → α → β) → β → ([α] → β)
;;;;   foldl f v [] = v
;;;;   foldl f v (x : xs) = foldl f (f v x) xs
;;;;
;;;;   redefine the function foldl in terms of fold:
;;;;   foldl f v xs = fold (λx g → (λa → g (f a x))) id xs v   ?? What does this mean??
;;;;
;;;;   In contrast, it is not possible to redefine fold in terms of foldl , due to the fact that
;;;;   foldl is strict in the tail of its list argument but fold is not.
;;;;
;;;;   reverse :: [α] → [α]
;;;;   reverse = foldl (λxs x → x : xs) []
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :fp (:use :common-lisp :core :test) (:shadow :map :length :append :max :filter :conjoin :disjoin :compose :last1))

(in-package :fp)

;;;
;;;    Only handles single LIST, not multiple SEQUENCES!
;;;    
(defun map (f list)
  (reduce #'(lambda (elt rest)
              (cons (funcall f elt) rest))
          list
          :initial-value '()
          :from-end t))

(deftest test-map ()
  (check
   (equal (map #'1+ (loop for i from 0 to 10 collect i)) '(1 2 3 4 5 6 7 8 9 10 11))
   (equal (map #'cl:length (list "Is" "this" "not" "pung?")) '(2 4 3 5))))

(defun filter (f list)
  (reduce #'(lambda (elt rest)
              (if (funcall f elt)
                  (cons elt rest)
                  rest))
          list
          :initial-value '()
          :from-end t))

(deftest test-filter ()
  (check
   (equal (filter #'evenp (loop for i from 0 to 10 collect i)) '(0 2 4 6 8 10))
   (equal (filter #'oddp (loop for i from 0 to 10 collect i)) '(1 3 5 7 9))
   (equal (filter #'(lambda (elt) (< elt 6)) (loop for i from 0 to 10 collect i)) '(0 1 2 3 4 5))
   (equal (filter #'(lambda (elt) (evenp (cl:length elt))) (list "Is" "this" "not" "pung?")) '("Is" "this"))))

;;;
;;;    Initial input is the "empty" function IDENTITY. Thus, all functions can only take one argument (All Curried?)
;;;    见 improved definition in core.lisp
;;;    
(defun compose (&rest fs)
  (reduce #'(lambda (f g)
              #'(lambda (x)
                  (funcall f (funcall g x))))
          fs
          :initial-value #'identity
          :from-end t))

;(funcall (compose #'sin #'(lambda (degrees) (* degrees (/ pi 180)))) 210)

(defun max (&rest xs)
  (reduce #'(lambda (x y) (if (> y x) y x)) xs))

(deftest test-max ()
  (check
   (eql (max 1 2 3) 3)
   (eql (max 3 2 1) 3)
   (eql (max 0 -99.3 pi) pi)
   (eql (max 2d0 2) 2.0d0) ; Not guaranteed by cl:max
   (eql (max 2 2d0) 2)))

(defun length (l)
  (reduce #'(lambda (count elt)
              (declare (ignore elt))
              (1+ count))
          l
          :initial-value 0))

(deftest test-length ()
  (check
   (= (length #1='()) (cl:length #1#))
   (= (length #2='(a)) (cl:length #2#))
   (= (length #3='(a b c d e)) (cl:length #3#))))

(defun append (l1 l2)
  (reduce #'cons l1 :from-end t :initial-value l2))

(deftest test-append ()
  (check
   (equal (append #1='() #1#) (cl:append #1# #1#))
   (equal (append #1# #2='(a)) (cl:append #1# #2#))
   (equal (append #3='(a) #4='(b c)) (cl:append #3# #4#))
   (equal (append #5='(a b c) #6='(1 2 3)) (cl:append #5# #6#))
   (equal (append #5# #6#) (fold-right #'cons #6# #5#))))
   
;;;
;;;    Learn you a Haskell 77 页
;;;    
(defun last1 (l)
  (reduce #'(lambda (_ x) (declare (ignore _)) x) l))

;;;
;;;    SICP pg. 116
;;;    Very close to the conventional definition of APPEND!
;;;    (fold-right #'cons l2 l1)
;;;    
(defun fold-right (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (first sequence) (fold-right op initial (rest sequence)))) )

(deftest test-fold-right ()
  (check
   (= 15 (fold-right #'+ 0 #1='(1 2 3 4 5)))
   (= 120 (fold-right #'* 1 #1#))
   (= 7 (fold-right #'- 9 '(5 6 7 8)))
   (= 15/8 (fold-right #'/ 1 #1#))
   (= 2 (fold-right #'- 0 '(1 2 3)))
   (equal #1# (fold-right #'cons '() #1#))
   (equal '(1 (2 (3 NIL))) (fold-right #'list '() '(1 2 3)))) )

;;;
;;;    SICP pg. 121
;;;
(defun fold-left (op initial sequence)
  (labels ((iter (result seq)
             (if (null seq)
                 result
                 (iter (funcall op result (first seq)) (rest seq)))) )
    (iter initial sequence)))

(deftest test-fold-left ()
  (check
   (= 15 (fold-left #'+ 0 #1='(1 2 3 4 5)))
   (= 120 (fold-left #'* 1 #1#))
   (= -25 (fold-left #'- 5 '(6 7 8 9)))
   (= (fold-left #'/ 1 #1#) 1/120 (/ (fold-left #'* 1 #1#)))
   (= -6 (fold-left #'- 0 '(1 2 3)))
   (equal '(((((NIL . 1) . 2) . 3) . 4). 5) (fold-left #'cons '() #1#))
   (equal (reverse #1#) (fold-left #'(lambda (cdr car) (cons car cdr)) '() #1#))
   (equal #1# (fold-left #'(lambda (cdr car) (cons car cdr)) '() (reverse #1#)))
   (equal '(((NIL 1) 2) 3) (fold-left #'list '() '(1 2 3)))
   (= (fold-left #'(lambda (x y) (- y x)) 9 '(8 7 6 5)) (fold-right #'- 9 '(5 6 7 8)))) )
   
;;;
;;;    Haskell
;;;
;; (defun foldl1 (f xs)
;;   (labels ((fold (acc xs)
;;              (if (null xs)
;;                  acc
;;                  (fold (funcall f acc (first xs)) (rest xs)))) )
;;     (if (null xs)
;;         (error "Empty list")
;;         (fold (first xs) (rest xs)))) )

(defun foldl (f acc xs)
  (if (null xs)
      acc
      (foldl f (funcall f acc (first xs)) (rest xs))))

(defun foldl1 (f xs)
  (if (null xs)
      (error "Empty list")
      (foldl f (first xs) (rest xs))))

;;;
;;;    SICP definition above is cleaner for FOLDR.
;;;    This version of FOLDR1 simply avoids calling BUTLAST/LAST.
;;;
;;;    In other words, there is a beautiful symmetry/clarity with
;;;    FOLDL and FOLDR, but FOLDR1 mucks things up...
;;;    
;; (defun foldr1 (f xs)
;;   (labels ((fold (xs) ; Not tail-recursive!
;;              (if (singlep xs)
;;                  (first xs)
;;                  (funcall f (first xs) (fold (rest xs)))) ))
;;     (if (null xs)
;;         (error "Empty list")
;;         (fold xs))))

(defun foldr (f acc xs)
  (if (null xs)
      acc
      (funcall f (first xs) (foldr f acc (rest xs)))) )

(defun foldr1 (f xs)
  (if (null xs)
      (error "Empty list")
      (foldr f (last1 xs) (butlast xs)))) ; Ugh...

(deftest test-foldl1 ()
  (check
   (= 8 (foldl1 #'+ '(8)))
   (= 20 (foldl1 #'+ '(8 12)))
   (= 15 (foldl1 #'+ (cons 0 #1='(1 2 3 4 5))))
   (= 120 (foldl1 #'* #1#))
   (= -25 (foldl1 #'- '(5 6 7 8 9)))
   (= (foldl1 #'/ #1#) 1/120 (/ (foldl1 #'* #1#)))
   (= -6 (foldl1 #'- '(0 1 2 3)))
   (equal '((((1 . 2) . 3) . 4) . 5) (foldl1 #'cons #1#))
   (equal '(5 4 3 2 . 1) (foldl1 #'(lambda (cdr car) (cons car cdr)) #1#))
   (equal '(1 2 3 4 . 5) (foldl1 #'(lambda (cdr car) (cons car cdr)) (reverse #1#)))
   (equal '((1 2) 3) (foldl1 #'list '(1 2 3)))
   (= (foldl1 #'(lambda (x y) (- y x)) '(9 8 7 6 5)) (foldr1 #'- '(5 6 7 8 9)))) )

(deftest test-foldr1 ()
  (check
   (= 8 (foldr1 #'+ '(8)))
   (= 20 (foldr1 #'+ '(8 12)))
   (= 15 (foldr1 #'+ (cons 0 #1='(1 2 3 4 5))))
   (= 120 (foldr1 #'* #1#))
   (= 7 (foldr1 #'- '(5 6 7 8 9)))
   (= 15/8 (foldr1 #'/ #1#))
   (= 2 (foldr1 #'- '(1 2 3 0)))
   (= -2 (foldr1 #'- '(0 1 2 3)))
   (equal '(1 2 3 4 . 5) (foldr1 #'cons #1#))
   (equal '(1 (2 3)) (foldr1 #'list '(1 2 3)))) )

(defun map (f list)
  (fold-right #'(lambda (elt rest)
                  (cons (funcall f elt) rest))
              '()
              list))

(defun fold-right* (op initial sequence)
  (fold-left #'(lambda (x y) (funcall op y x)) initial (reverse sequence)))

;;;
;;;    250728 月
;;;    Not short-circuiting!
;;;
(defun conjoin (&rest ps)
  #'(lambda (&rest args)
      (reduce #'(lambda (b p) (and b (apply p args))) ps :initial-value t)))

(defun disjoin (&rest ps)
  #'(lambda (&rest args)
      (reduce #'(lambda (b p) (or b (apply p args))) ps :initial-value nil)))

;;;
;;;    Lisp vs. Haskell
;;;
;; Common Lisp and Haskell both provide support for the operation of folding a list with a binary operator
;; Generalize operator

;; L vs. R

;; Initial value vs. purely list elts
;; A X B                A X A

;; REDUCE does it all

;; Lisp edge case!
;; A X B -> A
;; (reduce #'(lambda (max s) (max max (length s))) '("Is" "this" "not" "pung?") :initial-value 0) => 5
;; vs. heterogeneous list (only first elt!)
;; (reduce #'(lambda (max s) (max max (length s))) '(0 "Is" "this" "not" "pung?")) => 5

;; -Traverse list backwards vs. recursive calls vs.
;; reverse foldl -> foldr

;; x₁⊕x₂⊕x₃⊕x₄⊕...⊕xₙ
;; (...((((x₁⊕x₂)⊕x₃)⊕x₄)⊕)...)⊕xₙ
;; x₁⊕(x₂⊕(x₃⊕(x₄⊕(...⊕xₙ)))...)

;; Associative operator:
;; 7+4+3+2+9 = (((7+4)+3)+2)+9 = 7+(4+(3+(2+9)))

;; 7-4-3-2-9 ? (((7-4)-3)-2)-9 ≠ 7-(4-(3-(2-9)))
