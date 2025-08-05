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
;;;;   foldl f v xs = fold (λx g → (λa → g (f a x))) id xs v
;;;;
;;;;   In contrast, it is not possible to redefine fold in terms of foldl , due to the fact that
;;;;   foldl is strict in the tail of its list argument but fold is not.
;;;;
;;;;   reverse :: [α] → [α]
;;;;   reverse = foldl (λxs x → x : xs) []
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :fp (:use :common-lisp :test) (:shadow :map :length :append :max))

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
   (= (fold-right #'+ 0 #1='(1 2 3 4 5)) 15)
   (= (fold-right #'* 1 #1#) 120)
   (= (fold-right #'- 9 '(5 6 7 8)) 7)
   (= (fold-right #'/ 1 #1#) 15/8)
   (= (fold-right #'- 0 '(1 2 3)) 2)
   (equal (fold-right #'cons '() #1#) #1#)
   (equal (fold-right #'list '() '(1 2 3)) '(1 (2 (3 NIL)))) ))

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
   (= (fold-left #'+ 0 #1='(1 2 3 4 5)) 15)
   (= (fold-left #'* 1 #1#) 120)
   (= (fold-left #'- 5 '(6 7 8 9)) -25)
   (= (fold-left #'/ 1 #1#) (/ (fold-left #'* 1 #1#)) 1/120)
   (= (fold-left #'- 0 '(1 2 3)) -6)
   (equal (fold-left #'cons '() #1#) '(((((NIL . 1) . 2) . 3) . 4). 5))
   (equal (fold-left #'(lambda (cdr car) (cons car cdr)) '() #1#) (reverse #1#))
   (equal (fold-left #'(lambda (cdr car) (cons car cdr)) '() (reverse #1#)) #1#)
   (equal (fold-left #'list '() '(1 2 3)) '(((NIL 1) 2) 3))
   (= (fold-left #'(lambda (x y) (- y x)) 9 '(8 7 6 5)) (fold-right #'- 9 '(5 6 7 8)))) )
   
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
