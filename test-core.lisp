;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               test-core.lisp
;;;;
;;;;   Started:            Thu Feb 20 01:29:30 2020
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

(in-package :core)

(use-package :test)

;;;
;;;    Missing
;;;
;dotuples

;;;
;;;    Fix the list implementation!!
;;;    
(deftest test-splice ()
  (check
   (string= "Who ordered this pung?" (splice "Why all this pung?" 0 7 "Who ordered"))
   (string= "Is this not pung?" (splice "Is this pung?" 8 0 "not "))
   (string= "Is this not pung?" (splice "Is this foo?" 8 3 "not pung"))
   (equal '(a b c d e) (splice '(a b d e) 2 0 '(c)))
   (equal '(a b c e) (splice '(a b d e) 2 1 '(c)))
   (equal '(a b c) (splice '(a b d e) 2 2 '(c)))) )

;(defun list-to-string (l)

;(defun show-symbols (package-name)

(deftest test-transfer ()
  (check
   (multiple-value-bind (elt seq) (transfer '(a b c) 0) (and (eql 'a elt) (equal '(b c) seq)))
   (multiple-value-bind (elt seq) (transfer '(a b c) 1) (and (eql 'b elt) (equal '(a c) seq)))
   (multiple-value-bind (elt seq) (transfer '(a b c) 2) (and (eql 'c elt) (equal '(a b) seq)))
   (multiple-value-bind (ch s) (transfer "pung" 0) (and (char= #\p ch) (string= "ung" s)))
   (multiple-value-bind (ch s) (transfer "pung" 1) (and (char= #\u ch) (string= "png" s)))
   (multiple-value-bind (ch s) (transfer "pung" 3) (and (char= #\g ch) (string= "pun" s)))
   (multiple-value-bind (elt v) (transfer #(:a :b :c :d) 0) (and (eql :a elt) (equalp #(:b :c :d) v)))
   (multiple-value-bind (elt v) (transfer #(:a :b :c :d) 1) (and (eql :b elt) (equalp #(:a :c :d) v)))
   (multiple-value-bind (elt v) (transfer #(:a :b :c :d) 3) (and (eql :d elt) (equalp #(:a :b :c) v))) ))

(deftest test-drop ()
  (check
   (equal '(a b c d) (drop 0 '(a b c d)))
   (equal '(b c d) (drop 1 '(a b c d)))
   (equal '(c d) (drop 2 '(a b c d)))
   (equal '() (drop 4 '(a b c d)))
   (equal '() (drop 5 '(a b c d)))
   (equals #(:a :b :c) (drop 0 #(:a :b :c)))
   (equals #(:b :c) (drop 1 #(:a :b :c)))
   (equals #(:c) (drop 2 #(:a :b :c)))
   (equals #() (drop 3 #(:a :b :c)))
   (equals #() (drop 4 #(:a :b :c)))
   (string= "this not pung?" (drop 3 "Is this not pung?"))
   (string= "pung?" (drop 12 "Is this not pung?"))
   (string= "" (drop 17 "Is this not pung?"))))

(deftest test-take ()
  (check
   (equal '() (take 0 '(a b c d)))
   (equal '(a) (take 1 '(a b c d)))
   (equal '(a b) (take 2 '(a b c d)))
   (equal '(a b c d) (take 4 '(a b c d)))
   (equal '(a b c d) (take 5 '(a b c d)))
   (equals #() (take 0 #(:a :b :c)))
   (equals #(:a) (take 1 #(:a :b :c)))
   (equals #(:a :b) (take 2 #(:a :b :c)))
   (equals #(:a :b :c) (take 3 #(:a :b :c)))
   (equals #(:a :b :c) (take 4 #(:a :b :c)))
   (string= "Is this" (take 7 "Is this not pung?"))
   (string= "" (take 0 "Is this not pung?"))))

(deftest test-take-and-drop ()
  (check
   (equal '(b c) (take 2 (drop 1 '(a b c d))))
   (equal '(b) (drop 1 (take 2 '(a b c d))))
   (equals #(:c) (take 1 (drop 2 #(:a :b :c))))
   (string= "pung" (take 4 (drop 12 "Is this not pung?")))))

(deftest test-take-drop ()
  (check
   (multiple-value-bind (before after) (take-drop 0 '(a b c d))
     (and (equal '() before)
          (equal '(a b c d) after)))
   (multiple-value-bind (before after) (take-drop 1 '(a b c d))
     (and (equal '(a) before)
          (equal '(b c d) after)))
   (multiple-value-bind (before after) (take-drop 2 '(a b c d))
     (and (equal '(a b) before)
          (equal '(c d) after)))
   (multiple-value-bind (before after) (take-drop 5 '(a b c d))
     (and (equal '(a b c d) before)
          (equal '() after)))
   (multiple-value-bind (before after) (take-drop 0 #(1 2 3))
     (and (equalp #() before)
          (equalp #(1 2 3) after)))
   (multiple-value-bind (before after) (take-drop 1 #(1 2 3))
     (and (equalp #(1) before)
          (equalp #(2 3) after)))
   (multiple-value-bind (before after) (take-drop 3 #(1 2 3))
     (and (equalp #(1 2 3) before)
          (equalp #() after)))
   (multiple-value-bind (before after) (take-drop 8 #(1 2 3))
     (and (equalp #(1 2 3) before)
          (equalp #() after)))
   (multiple-value-bind (before after) (take-drop 7 "Is this not pung?")
     (and (string= "Is this" before)
          (string= " not pung?" after)))) )

(deftest test-take-while ()
  (check
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "A")) '(a b c d))
     (and (equal '() before)
          (equal '(a b c d) after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "B")) '(a b c d))
     (and (equal '(a) before)
          (equal '(b c d) after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "D")) '(a b c d))
     (and (equal '(a b c) before)
          (equal '(d) after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "Z")) '(a b c d))
     (and (equal '(a b c d) before)
          (equal '() after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "A")) #(:a :b :c))
     (and (equalp #() before)
          (equalp #(:a :b :c) after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "B")) #(:a :b :c))
     (and (equalp #(:a) before)
          (equalp #(:b :c) after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "C")) #(:a :b :c))
     (and (equalp #(:a :b) before)
          (equalp #(:c) after)))
   (multiple-value-bind (before after) (take-while #'(lambda (sym) (string< (symbol-name sym) "Z")) #(:a :b :c))
     (and (equalp #(:a :b :c) before)
          (equalp #() after)))
   (multiple-value-bind (before after) (take-while #'(lambda (ch) (char/= ch #\n)) "Is this not pung?")
     (and (equal "Is this " before)
          (equal "not pung?" after)))
   (multiple-value-bind (before after) (take-while #'digit-char-p "Is this not pung?")
     (and (equal "" before)
          (equal "Is this not pung?" after)))
   (multiple-value-bind (before after) (let ((sum 0)) (take-while #'(lambda (elt) (incf sum elt) (< sum 20)) '(2.3 9 0.7 8.4 6 4 3)))
     (and (equal '(2.3 9 0.7) before)
          (equal '(8.4 6 4 3) after))) ))

(deftest test-take-until ()
  (check
   (equal '((1 2 3 4) (5 6 7 8 9 10))
          (multiple-value-list (take-until #'(lambda (x) (> x 4)) (loop for i from 1 to 10 collect i))))
   (equal '((1 2 3 4 5 6 7 8 9 10) ())
          (multiple-value-list (take-until #'(lambda (x) (> x 14)) (loop for i from 1 to 10 collect i))))
   (equal '(() (1 2 3 4 5 6 7 8 9 10))
          (multiple-value-list (take-until #'(lambda (x) (> x 0)) (loop for i from 1 to 10 collect i))))
   (equals '(#(1 2 3 4) #(5 6 7 8 9 10))
           (multiple-value-list (take-until #'(lambda (x) (> x 4)) (apply #'vector #[1 10]))))
   (equal '((0 2 4) (5 7 2 9))
          (multiple-value-list (take-until #'oddp '(0 2 4 5 7 2 9))))
   (equals '(#(0 2 4) #(5 7 2 9))
           (multiple-value-list (take-until #'oddp [0 2 4 5 7 2 9])))) )

(deftest test-drop-while ()
  (check
   (equal '(a b c d)
          (drop-while #'(lambda (sym) (string< (symbol-name sym) "A")) '(a b c d)))
   (equal '(b c d)
          (drop-while #'(lambda (sym) (string< (symbol-name sym) "B")) '(a b c d)))
   (equal '(d)
          (drop-while #'(lambda (sym) (string< (symbol-name sym) "D")) '(a b c d)))
   (equal '()
          (drop-while #'(lambda (sym) (string< (symbol-name sym) "Z")) '(a b c d)))
   (equalp #(:a :b :c)
           (drop-while #'(lambda (sym) (string< (symbol-name sym) "A")) #(:a :b :c)))
   (equalp #(:b :c)
           (drop-while #'(lambda (sym) (string< (symbol-name sym) "B")) #(:a :b :c)))
   (equalp #(:c)
           (drop-while #'(lambda (sym) (string< (symbol-name sym) "C")) #(:a :b :c)))
   (equalp #()
           (drop-while #'(lambda (sym) (string< (symbol-name sym) "Z")) #(:a :b :c)))
   (equal "not pung?"
          (drop-while #'(lambda (ch) (char/= ch #\n)) "Is this not pung?"))
   (equal "Is this not pung?"
          (drop-while #'digit-char-p "Is this not pung?"))
   (equal '(8.4 6 4 3)
          (let ((sum 0)) (drop-while #'(lambda (elt) (incf sum elt) (< sum 20)) '(2.3 9 0.7 8.4 6 4 3)))) ))

(deftest test-drop-until ()
  (check
   (equal '(5 6 7 8 9 10) (drop-until #'(lambda (x) (> x 4)) (loop for i from 1 to 10 collect i)))
   (equal '() (drop-until #'(lambda (x) (> x 14)) (loop for i from 1 to 10 collect i)))
   (equal '(1 2 3 4 5 6 7 8 9 10) (drop-until #'(lambda (x) (> x 0)) (loop for i from 1 to 10 collect i)))
   (equals #(5 6 7 8 9 10) (drop-until #'(lambda (x) (> x 4)) (apply #'vector #[1 10])))
   (equal '(5 7 2 9) (drop-until #'oddp '(0 2 4 5 7 2 9)))
   (equals #(5 7 2 9) (drop-until #'oddp [0 2 4 5 7 2 9]))))

(deftest test-equals ()
  (check
   (equals 2 2d0)
   (equals "pung" "pung")
   (not (equals "pung" "Pung"))
   (equals #\h #\h)
   (not (equals #\h #\H))
   (equals '(1 2 3) '(1 2 3))
   (equals #(1 2 3) #(1 2 3))
   (equals '(1 2 3) '(1d0 2d0 3d0))
   (equals #(1 2 3) #(1d0 2d0 3d0))
   (equals '("pung" "foo" "bar") '("pung" "foo" "bar"))
   (equals #("pung" "foo" "bar") #("pung" "foo" "bar"))
   (equals 'cl-user::cxr 'core::cxr)
   (equals '(:a #(1 2) (#\c ("d" "e"))) '(:a #(1 2) (#\c ("d" "e"))))
   (equals '(:a #(1 2d0) (#\c ("d" "e"))) '(:a #(1d0 2) (#\c ("d" "e")))) ))

(deftest test-eqls ()
  (check
   (eqls 2 2)
   (not (eqls 2 2d0))
   (eqls "pung" "pung")
   (not (eqls "pung" "Pung"))
   (eqls #\h #\h)
   (not (eqls #\h #\H))
   (eqls '(1 2 3) '(1 2 3))
   (eqls #(1 2 3) #(1 2 3))
   (not (eqls '(1 2 3) '(1d0 2d0 3d0)))
   (not (eqls #(1 2 3) #(1d0 2d0 3d0)))
   (eqls '("pung" "foo" "bar") '("pung" "foo" "bar"))
   (eqls #("pung" "foo" "bar") #("pung" "foo" "bar"))
   (not (eqls 'cl-user::cxr 'core::cxr))
   (eqls '(:a #(1 2) (#\c ("d" "e"))) '(:a #(1 2) (#\c ("d" "e"))))
   (not (eqls '(:a #(1 2d0) (#\c ("d" "e"))) '(:a #(1d0 2) (#\c ("d" "e")))) )))

(deftest test-prefixp ()
  (check
   (prefixp '() '())
   (prefixp '() '(g t c a t))
   (not (prefixp '(g t c a t) '()))
   (prefixp '(g t c) '(g t c a t))
   (not (prefixp '(g t c a t) '(g t c)))
   (not (prefixp '(g t c) '(t c a t)))
   (prefixp #1='(g t c a t) #1#)
   (not (prefixp '(g t c) '(a g g t c)))
   (not (prefixp '((a . 1)) '((a . 1) (b . 2) (c . 3))))
   (prefixp '((a . 1)) '((a . 1) (b . 2) (c . 3)) :test #'equal)
   (not (prefixp '("Is" "this" "not") '("Is" "this" "not" "pung?")))
   (prefixp '("Is" "this" "not") '("Is" "this" "not" "pung?") :test #'string=)
   (not (prefixp '(1 2 5/2) '(1d0 2d0 2.5d0 3d0)))
   (prefixp '(1 2 5/2) '(1d0 2d0 2.5d0 3d0) :test #'=)
   (prefixp "" "Is this not pung?")
   (prefixp "Is this not pung?" "Is this not pung?")
   (prefixp "Is" "Is this not pung?")
   (not (prefixp "IS" "is this not pung?"))
   (prefixp "IS" "is this not pung?" :test #'char-equal)
   (prefixp #*101 #*101111)
   (prefixp [3 4] (subseq [1 2 3 4 5] 2))
   (prefixp #[1 3] #[1 10])
   (prefixp #() #(:a :b :c :d :e))
   (prefixp #2=#(:a :b :c :d :e) #2#)
   (prefixp #(:a :b :c :d) #(:a :b :c :d :e))))

(deftest test-suffixp ()
  (check
   (suffixp '() '())
   (suffixp '() '(g t c a t))
   (suffixp '(c a t) '(g t c a t))
   (not (suffixp '(g t c a t) '(c a t)))
   (not (suffixp '(c a t) '(g t c a)))
   (suffixp #1='(g t c a t) #1#)
   (not (suffixp '(a g g) '(a g g t c)))
   (suffixp '() '(a b c))
   (suffixp '(c) '(a b c))
   (suffixp '(b c) '(a b c))
   (suffixp '(a b c) '(a b c))
   (suffixp "" "asdf")
   (suffixp "f" "asdf")
   (suffixp "df" "asdf")
   (suffixp "sdf" "asdf")
   (suffixp "asdf" "asdf")
   (not (suffixp '("this" "not" "pung?") '("Is" "this" "not" "pung?")))
   (suffixp '("this" "not" "pung?") '("Is" "this" "not" "pung?") :test #'string=)
   (not (suffixp '(2 3 7/2 4) '(1d0 2d0 3d0 3.5d0 4d0)))
   (suffixp '(2 3 7/2 4) '(1d0 2d0 3d0 3.5d0 4d0) :test #'=)
   (suffixp "" "Is this not pung?")
   (suffixp "Is this not pung?" "Is this not pung?")
   (suffixp "pung?" "Is this not pung?")
   (suffixp "PUNG?" "is this not pung?" :test #'char-equal)
   (suffixp "this" (subseq "Is this not pung?" 0 7))
   (suffixp #*111 #*101111)
   (suffixp [3 4] (subseq [1 2 3 4 5] 0 4))
   (suffixp #[8 10] #[1 10])
   (suffixp #() #(a b c))
   (suffixp #(c) #(a b c))
   (suffixp #(b c) #(a b c))
   (suffixp #(a b c) #(a b c))
   (suffixp #(a b c) #(a a b c))
   (suffixp #() #(:a :b :c :d :e))
   (suffixp #2=#(:a :b :c :d :e) #2#)
   (suffixp #(:b :c :d) #(:a :b :c :d))))

(deftest test-starts-with ()
  (check
   (starts-with "Is" "Is")
   (starts-with "Is this not pung?" "Is")
   (not (starts-with "Is this not pung?" "is"))
   (starts-with "Is this not pung?" "is" :test #'char-equal)
   (starts-with (subseq "Is this not pung?" 12) "pung")
   (starts-with '(a b c) '(a b))
   (starts-with '(a b) '(a b))
   (starts-with '(1 2 3 4 5) '(1 2))
   (not (starts-with '(1 2 3 4 5) '(2 3)))
   (starts-with '((a . 1) (b . 2) (c . 3)) '((a . 1)) :test #'equal)
   (starts-with (subseq [1 2 3 4 5] 2) [3 4])
   (starts-with #[1 10] #[1 3])))

(deftest test-ends-with ()
  (check   
    (ends-with "Is this not pung?" "pung?")
    (ends-with (subseq "Is this not pung?" 0 7) "this")
    (not (ends-with "Is this not pung?" "PUNG?"))
    (ends-with "Is this not pung?" "PUNG?" :test #'char-equal)
    (ends-with '(a b c) '(b c))
    (ends-with (subseq [1 2 3 4 5] 0 4) [3 4])
    (ends-with #[1 10] #[8 10])))

(deftest test-rotate-list0 ()
  (check
   (equal (rotate-list0 3 0) '(0 1 2))
   (equal (rotate-list0 3 1) '(2 0 1))
   (equal (rotate-list0 3 2) '(1 2 0))) )

(deftest test-rotate-list1 ()
  (check
   (equal (rotate-list1 3 0) '(1 2 3))
   (equal (rotate-list1 3 1) '(3 1 2))
   (equal (rotate-list1 3 2) '(2 3 1))
   (equal (rotate-list1 3 3) '(1 2 3))))

(deftest test-shift-list0 ()
  (check
   (equal (shift-list0 5 0) '(0 1 2 3 4))
   (equal (shift-list0 5 1) '(5 1 2 3 4))
   (equal (shift-list0 5 2) '(5 6 2 3 4))
   (equal (shift-list0 5 3) '(5 6 7 3 4))
   (equal (shift-list0 5 4) '(5 6 7 8 4))
   (equal (shift-list0 5 5) '(5 6 7 8 9))))

(deftest test-shift-list1 ()
  (check
   (equal (shift-list1 5 0) '(1 2 3 4 5))
   (equal (shift-list1 5 1) '(6 2 3 4 5))
   (equal (shift-list1 5 2) '(6 7 3 4 5))
   (equal (shift-list1 5 3) '(6 7 8 4 5))
   (equal (shift-list1 5 4) '(6 7 8 9 5))
   (equal (shift-list1 5 5) '(6 7 8 9 10))))





(deftest test-cycle ()
  (check
   (equal (cycle #'1+ 5 '(1 2 3 4 5 6 7)) '(2 3 4 5 6))
   (equal (cycle #'1+ 8 '(1 2 3 4 5 6 7)) '(2 3 4 5 6 7 8 2))
   (equal (cycle #'1+ 20 '(1 2 3 4 5 6 7)) '(2 3 4 5 6 7 8 2 3 4 5 6 7 8 2 3 4 5 6 7))))


(defun make-random-tree (generator)
  (if (< (random 1d0) 0.5)
      (list (funcall generator))
      (cons (make-random-tree generator)
            (make-random-tree generator))))

(defun make-random-tree (n)
  (let ((rs (make-random-state t)))
    (labels ((make ()
               (loop for i from 1 to n
                     if (< (random 1d0 rs) 0.7)
                       collect i
                     else
                       collect (make)
                  end)))
      (make))))

(defun make-big-tree (n)
  (labels ((make (m)
             (if (zerop m)
                 (list (random 1d0))
                 (loop for i from 1 to m collect (make (1- m)))) ))
    (make n)))

(defun tree-size (tree)
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (+ (tree-size (car tree))
              (tree-size (cdr tree))))))
;; (let ((i 0)) (make-random-tree #'(lambda () (prog1 i (incf i)))))
;; (let ((i 0)) (make-random-tree #'(lambda () (prog1 (code-char (+ i (char-code #\A))) (incf i)))))  
;; (defvar *tree* (let ((i 0)) (make-random-tree #'(lambda () (prog1 i (incf i))))))

(deftest test-flatten ()
  (check
   (equal (flatten '(((0) (1) (2) 3) ((4) (5) ((6) ((7) (8) (9) 10) (11) 12) 13) (14) 15))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
   (equal (flatten '((((0) (1) (2) ((3) 4) 5) (6) ((7) (((8) (9) 10) ((((11) 12) ((13) 14) (15) 16) (17) 18) 19) 20) 21) (22) 23))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))
   (equal (flatten '((0) (((1) (((((((((2) (((3) (((4) ((5) (6) 7) 8) (9) (((10) 11) 12) 13) 14) (15) (16) 17) (18) (19) 20) (((21) (22) (23) ((24) ((25) (26) 27) 28) 29) 30) 31) 32) 33) (((34) (35) 36) 37) (((38) 39) 40) 41) 42) (43) (44) 45) 46) 47) 48) 49))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49))))

(deftest test-approximately= ()
  (check
   (approximately= 0.001d0 0.0010000002d0)
   (not (approximately= 0.001d0 0.001000002d0))
   (approximately= 0.001d0 0.001000002d0 1d-4)))

(deftest test-find-some-if ()
  (check
   (equal (multiple-value-list (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) '(a "pung" 5 t))) '(5 2.236068))
   (equal (multiple-value-list (find-some-if (iffn #'numberp #'sqrt) '(a "pung" 5 t))) '(5 2.236068))
   (null (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) '(a "pung" :j t)))
   (equal (multiple-value-list (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) #('a "pung" 5 t))) '(5 2.236068))
   (null (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) #('a "pung" :j t)))
   (equal (multiple-value-list (find-some-if #'(lambda (ch) (if (member ch '(#\a #\e #\i #\o #\u)) (char-upcase ch) nil)) "Is this not pung?")) '(#\i #\I))
   (equal (multiple-value-list (find-some-if (iffn #'(lambda (ch) (member ch '(#\a #\e #\i #\o #\u))) #'char-upcase) "Is this not pung?")) '(#\i #\I))
   (equal (multiple-value-list (find-some-if #'(lambda (s) (if (= (length s) 4) (string-upcase s) nil)) #("Yoshimi" "Battles" "The" "Pink" "Robots"))) '("Pink" "PINK"))))
   

(deftest test-last1 ()
  (check
   (null (last1 '()))
   (equal 'c (last1 '(a b c)))
   (equal 'b (last1 '(a b . c)))) )

(deftest test-singlep ()
  (check
   (singlep '(a))
   (not (singlep '()))
   (not (singlep '(a b c)))) )

(deftest test-append1 ()
  (check
   (equal '(a b c) (append1 '(a b) 'c))
   (equal '(a) (append1 '() 'a))
   (equal '(a b (c d)) (append1 '(a b) '(c d)))) )

(deftest test-conc1 ()
  (check
   (equal '(a b c) (conc1 '(a b) 'c))
   (equal '(a) (conc1 '() 'a))
   (equal '(a b (c d)) (conc1 '(a b) '(c d)))
   (let ((l (list 1 2 3)))
     (eq l (conc1 l 4)))) )

(deftest test-mklist ()
  (check
   (eq #1='(a b c) (mklist #1#))
   (equal '(a) (mklist 'a))))

(deftest test-longerp ()
  (check
   (longerp '(a b c) '(d e))
   (not (longerp '(a b c) '(d e f)))
   (not (longerp '(a b c) '(d e f g)))
   (longerp (loop for i upto 100000 collect i) '(a b c))
   (not (longerp '(a b c) (loop for i upto 100000 collect i)))
   (longerp "abc" "de")
   (not (longerp "abc" "def"))
   (not (longerp "abc" "defg"))
   (longerp [1 2 3] [4 5])
   (not (longerp [1 2 3] [4 5 6]))
   (not (longerp [1 2 3] [4 5 6 7]))
   (longerp (loop for i upto 100000 collect i) "yep")
   (not (longerp "nope" (loop for i upto 100000 collect i)))
   (longerp '(a b c d) [4 5])
   (longerp ["Is" "this" "not" "pung?"] '(:nope))))

;; (flet ((f (x) (if (numberp x) (1+ x) nil))) (mapcar #'f (remove-if-not #'f '(a 1 2 b 3 c d 4)))) => (2 3 4 5)
;; (flet ((f (x) (if (numberp x) (1+ x) nil))) (map 'vector #'f (remove-if-not #'f '[a 1 2 b 3 c d 4]))) => #(2 3 4 5)
;; (flet ((f (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch)))) (map 'string #'f (remove-if-not #'f "Is this not pung?"))) => "STHISNOTPUNG"
;; (flet ((f (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch)))) (mapcar #'f (remove-if-not #'f (coerce "Is this not pung?" 'list)))) => (#\S #\T #\H #\I #\S #\N #\O #\T #\P #\U #\N #\G)

(deftest test-filter ()
  (check
   (equal '(2 3 4 5) (filter #'(lambda (x) (if (numberp x) (1+ x) nil)) '(a 1 2 b 3 c d 4)))
   (equal '(2 3 4 5) (filter (iffn #'numberp #'1+) '(a 1 2 b 3 c d 4)))
   (equalp [2 3 4 5] (filter #'(lambda (x) (if (numberp x) (1+ x) nil)) ['a 1 2 'b 3 'c 'd 4]))
   (string= "STHISNOTPUNG" (filter #'(lambda (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch))) "Is this not pung?"))
   (string= "STHISNOTPUNG" (filter (every-pred #'alpha-char-p #'lower-case-p #'char-upcase) "Is this not pung?"))
   (equal '(#\S #\T #\H #\I #\S #\N #\O #\T #\P #\U #\N #\G)
          (filter #'(lambda (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch))) (coerce "Is this not pung?" 'list)))
   (equal '(#\S #\T #\H #\I #\S #\N #\O #\T #\P #\U #\N #\G)
          (filter (every-pred #'alpha-char-p #'lower-case-p #'char-upcase) (coerce "Is this not pung?" 'list)))))

(deftest test-group ()
  (check
   (null (group (loop for i from 1 to 10 collect i) 0))
   (equal '((1 2 3) (4 5 6) (7 8 9))
          (group (loop for i from 1 to 9 collect i) 3))
   (equal '((1 2 3) (4 5 6) (7 8 9) (10))
          (group (loop for i from 1 to 10 collect i) 3))
   (equal '((1 2 3 4 5 6 7) (8 9 10))
          (group (loop for i from 1 to 10 collect i) 7))
   (equalp '(#(1 2 3) #(4 5 6) #(7 8 9))
           (group (coerce (loop for i from 1 to 9 collect i) 'vector) 3))
   (equalp '(#(1 2 3) #(4 5 6) #(7 8 9) #(10))
           (group (coerce (loop for i from 1 to 10 collect i) 'vector) 3))
   (equalp '(#(1 2 3 4 5 6 7) #(8 9 10))
           (group (coerce (loop for i from 1 to 10 collect i) 'vector) 7))
   (equal '("Is" " t" "hi" "s " "no" "t " "pu" "ng" "?")
          (group "Is this not pung?" 2))
   (equal  '("Is this not" " pung?")
           (group "Is this not pung?" 11))
   (equal '("Is this" " not pu" "ng?")
          (group "Is this not pung?" 7))
   (equal '((:a 1) (:b 2) (:c 3) (:d 4)) ; Property list -> Association list
          (group '(:a 1 :b 2 :c 3 :d 4) 2))))

(defun count-vowels (s)
  (count-if #'(lambda (ch) (member ch (coerce "aeiou" 'list))) s :key #'char-downcase))
(defun count-consonants (s)
  (count-if #'(lambda (ch) (member ch (coerce "bcdfghjklmnpqrstvwxyz" 'list))) s :key #'char-downcase))

;;;
;;;    This typically does not make sense for an equality test!
;;;    (group-until #'(lambda (l) (= (reduce #'+ l) 2)) '(1 2 1 2)) => ((1 2 1 2))
;;;    (group-until #'(lambda (l) (= (reduce #'+ l) 3)) '(1 2 1 2)) => ((1) (2) (1) (2))
;;;    (group-until #'(lambda (l) (> (reduce #'+ l) 3)) '(1 2 1 2)) => ((1 2) (1 2))
;;;    
(deftest test-group-until ()
  (check
   (equal (group-until #'(lambda (l) (> (reduce #'+ l) 20)) '(4 19 4 9 5 12 5 3 4 1 1 9 5 18)) '((4) (19) (4 9 5) (12 5 3) (4 1 1 9 5) (18)))
   (equal (group-until #'(lambda (l) (> (reduce #'+ l) 20)) '(4 19 4 9 5 12 5 3 4 1 1 9 5 18 1)) '((4) (19) (4 9 5) (12 5 3) (4 1 1 9 5) (18 1)))
   (equal (group-until #'(lambda (chs) (or (> (count-vowels chs) 2) (> (count-consonants chs) 4))) "The quick brown fox jumps over the LAZY dog.")
          '((#\T #\h #\e #\  #\q #\u) (#\i #\c #\k #\  #\b #\r #\o) (#\w #\n #\  #\f #\o #\x #\ ) (#\j #\u #\m #\p #\s #\  #\o) (#\v #\e #\r #\  #\t #\h #\e #\ ) (#\L #\A #\Z #\Y #\  #\d #\o) (#\g #\.)))
   (equal (group-until #'(lambda (coins) (> (count :head coins) 3)) '(:HEAD :HEAD :HEAD :TAIL :HEAD :TAIL :HEAD :TAIL :TAIL :HEAD :TAIL :TAIL :TAIL :HEAD :TAIL :HEAD :TAIL :HEAD :TAIL :TAIL))
          '((:HEAD :HEAD :HEAD :TAIL) (:HEAD :TAIL :HEAD :TAIL :TAIL :HEAD :TAIL :TAIL :TAIL) (:HEAD :TAIL :HEAD :TAIL :HEAD :TAIL :TAIL)))))

(deftest test-prune ()
  (check
   (equal '(:b (:c (:d) :e) :g (:h))
          (prune :a '(:b :a (:c (:a :d) :e) :g :a (:h :a))))
   (equal '(3 2.0d0 (1 (2.0d0 -4) 5.0d0) 9 2.0d0 (0 2.0d0))
          (prune 2 '(3 2.0d0 (1 (2.0d0 -4) 5.0d0) 9 2.0d0 (0 2.0d0))))
   (equal '(3 (1 (-4) 5.0d0) 9 (0))
          (prune 2 '(3 2.0d0 (1 (2.0d0 -4) 5.0d0) 9 2.0d0 (0 2.0d0)) :test #'=))))

(deftest test-prune-if ()
  (check
   (equal '(1 (3 (5)) 7 (9))
          (prune-if #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))))
   (equal '(((4 5) 6) 7 8 (9))
          (prune-if #'(lambda (elt) (< elt 4)) '(1 2 (3 (4 5) 6) 7 8 (9)))) ))

(deftest test-prune-if-not ()
  (check
   (equal '(2 ((4) 6) 8 ())
          (prune-if-not #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))))
   (equal '(1 2 (3 ()) ())
          (prune-if-not #'(lambda (elt) (< elt 4)) '(1 2 (3 (4 5) 6) 7 8 (9)))) ))

(deftest test-same-shape-tree-p ()
  (check
   (same-shape-tree-p '(((a) a (a) (a) ((a (a (a (a a) a)) a) a) a) a a)
                      '(((b) b (b) (b) ((b (b (b (b b) b)) b) b) b) b b))
   (not (same-shape-tree-p '(((a) a (a) (a) (((a (a (a a) a)) a) a) a) a a)
                           '(((b) b (b) (b) ((b (b (b (b b) b)) b) b) b) b b)))) )

(deftest test-before ()
  (check
   (equal '(A B C D) (before 'a 'b '(a b c d)))
   (not (before 'a 'b '()))
   (not (before 'a 'b '(b a c d)))
   (equal '(A C D) (before 'a 'b '(a c d)))
   (not (before 2 3 '(0 1 2.0d0 3 2)))
   (equal '(2.0d0 3 2) (before 2 3 '(0 1 2.0d0 3 2) :test #'=))
   (= 12 (before #\p #\u "Is this not pung?"))
   (not (before #\p #\u "Is this not Pung?"))
   (= 12 (before #\p #\u "Is this not Pung?" :test #'char-equal))
   (not (before #\u #\p "Is this not pung?"))
   (not (before '(a b) '(c d) (vector '(:a) "foo" '(a b) '(c d))))
   (= 2 (before '(a b) '(c d) (vector '(:a) "foo" '(a b) '(c d)) :test #'equal))
   (equal '((:A 9) (:E 5) (:B -6))
          (before :a :b '((:d 7) (:c 12) (:a 9) (:e 5) (:b -6)) :key #'first))
   (= 2 (before :a :b #((:d 7) (:c 12) (:a 9) (:e 5) (:b -6)) :key #'first))
   (equal '(12 15 18 20) (before 3 5 '(4 8 12 15 18 20) :test #'(lambda (x elt) (zerop (mod elt x))))) ; Is an element divisible by 3 before any divisible by 5?
   (not (before 5 3 '(4 8 12 15 18 20) :test #'(lambda (x elt) (zerop (mod elt x)))) )))

(deftest test-after ()
  (check
   (equal '(B C D) (after 'b 'a '(a b c d)))
   (equal '(D) (after 'd 'a '(a b c d)))
   (not (after 'a 'b '(a b c d)))
   (not (after 'a 'b '(a b c a d)))
   (not (after 'a 'a '(a b c a d)))
   (= 1 (after 'b 'a '#(a b c d)))
   (= 3 (after 'd 'a '#(a b c d)))
   (not (after 'e 'a '#(a b c d)))
   (not (after 'a 'b '#(a b c d)))
   (= 13 (after #\u #\p "Is this not pung?"))
   (= 5 (after #\i #\s "Is this not pung?"))
   (not (after #\i #\s "Is this not pung?" :test #'char-equal))
   (not (after 2 0 #(4 6 8 0 2.0 3 5 7 9)))
   (= 4 (after 2 0 #(4 6 8 0 2.0 3 5 7 9) :test #'=))
   (equal '((:B -6))
          (after :b :a '((:d 7) (:c 12) (:a 9) (:e 5) (:b -6)) :key #'first))
   (= 4 (after :b :a #((:d 7) (:c 12) (:a 9) (:e 5) (:b -6)) :key #'first))
   (equal '(15 18 20) (after 5 3 '(4 8 12 15 18 20) :test #'(lambda (x elt) (zerop (mod elt x)))) )))
        
(deftest test-duplicatep ()
  (check
   (not (duplicatep 'a '(a b c d)))
   (equal '(A E F G) (duplicatep 'a '(a b c d a e f g)))
   (not (duplicatep #\a "abcd"))
   (= 4 (duplicatep #\a "abcdaefg"))
   (not (duplicatep #\a "abcdAEFG"))
   (= 4 (duplicatep #\a "abcdAEFG" :test #'char-equal))
   (not (duplicatep 2 #(2 4 6 8)))
   (= 5 (duplicatep 2 #(2 4 6 8 0 2 3 5 7 9)))
   (not (duplicatep 2 #(2 4 6 8 0 2.0 3 5 7 9)))
   (= 5 (duplicatep 2 #(2 4 6 8 0 2.0 3 5 7 9) :test #'=))
   (not (duplicatep 'c '((a . 1) (c . 2) (d . 3) (c . 4))))
   (equal '((c . 4)) (duplicatep 'c '((a . 1) (c . 2) (d . 3) (c . 4)) :key #'first))))

(defclass dude ()
  ((name :reader name :initarg :name)
   (height :reader height :initarg :height)))

(defgeneric tallerp (dude1 dude2))
(defmethod tallerp ((d1 dude) (d2 dude))
  (> (height d1) (height d2)))

(defgeneric shorterp (dude1 dude2))
(defmethod shorterp ((d1 dude) (d2 dude))
  (< (height d1) (height d2)))

(deftest test-most ()
  (check
   (equal '(nil nil) (multiple-value-list (most #'length '())))
   (equal '(() 0) (multiple-value-list (most #'length '(()))) )
   (equal '((A B) 2) (multiple-value-list (most #'length '((a b)))) )
   (equal '((A) 1) (multiple-value-list (most #'length '((a) (b) (c) (d)))) )
   (equal '((A B C) 3) (multiple-value-list (most #'length '((a b) (a b c) (a) (e f g)))) )
   (equal '((e f g h i j) 6) (multiple-value-list (most #'length '((a b) (a b c) (a) (e f g h i j)))) )
   (equal '(nil nil) (multiple-value-list (most #'char-code "")))
   (equal '(#\a 97) (multiple-value-list (most #'char-code "a")))
   (equal '(#\a 97) (multiple-value-list (most #'char-code "aaaaaaaaaa")))
   (equal '(#\u 117) (multiple-value-list (most #'char-code "Is this not pung?")))
   (equal '("abc" 3) (multiple-value-list (most #'length #("ab" "abc" "a" "efg"))))
   (equal '("abcdefg" 7) (multiple-value-list (most #'length #("ab" "abc" "a" "abcdefg" "efg"))))
   (equal '(nil nil) (multiple-value-list (most #'abs #())))
   (equal '(8 8) (multiple-value-list (most #'abs #(8))))
   (equal '(8 8) (multiple-value-list (most #'abs #(8 -8 8 -8))))
   (equal '(-28 28) (multiple-value-list (most #'abs #(-9 8 -7 3 25 0 -28))))
   (equal '(28 28) (multiple-value-list (most #'abs #(-9 8 -7 3 25 28 0 -28))))
   (equal '(4 3) (multiple-value-list (most #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))))
   (equal '(-8 3) (multiple-value-list (most #'integer-length (reverse #(0 1 3 4 7 -1 -4 -7 -8)))) )
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (string= "Tom" (name (most #'height dudes)))) ))

(deftest test-least ()
  (check
   (equal '(nil nil) (multiple-value-list (least #'length '())))
   (equal '(() 0) (multiple-value-list (least #'length '(()))) )
   (equal '((A B) 2) (multiple-value-list (least #'length '((a b)))) )
   (equal '((A) 1) (multiple-value-list (least #'length '((a) (b) (c) (d)))) )
   (equal '((A) 1) (multiple-value-list (least #'length '((a b) (a b c) (a) (e f g)))) )
   (equal '((a) 1) (multiple-value-list (least #'length '((a b) (a b c) (a) (e f g h i j)))) )
   (equal '(nil nil) (multiple-value-list (least #'char-code "")))
   (equal '(#\a 97) (multiple-value-list (least #'char-code "a")))
   (equal '(#\a 97) (multiple-value-list (least #'char-code "aaaaaaaaaa")))
   (equal '(#\space 32) (multiple-value-list (least #'char-code "Is this not pung?")))
   (equal '("a" 1) (multiple-value-list (least #'length #("ab" "abc" "a" "efg"))))
   (equal '("a" 1) (multiple-value-list (least #'length #("ab" "abc" "a" "abcdefg" "efg"))))
   (equal '(nil nil) (multiple-value-list (least #'abs #())))
   (equal '(8 8) (multiple-value-list (least #'abs #(8))))
   (equal '(8 8) (multiple-value-list (least #'abs #(8 -8 8 -8))))
   (equal '(0 0) (multiple-value-list (least #'abs #(-9 8 -7 3 25 0 -28))))
   (equal '(-1 1) (multiple-value-list (least #'abs #(-9 8 -7 3 -1 25 28 1 -28))))
   (equal '(0 0) (multiple-value-list (least #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))))
   (equal '(-1 0) (multiple-value-list (least #'integer-length (reverse #(0 1 3 4 7 -1 -4 -7 -8)))) )
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (string= "Bob" (name (least #'height dudes)))) ))

(deftest test-most-least ()
  (check
   (equal '(nil nil nil nil) (multiple-value-list (most-least #'length '())))
   (equal '(() 0 () 0) (multiple-value-list (most-least #'length '(()))) )
   (equal '((A B) 2 (A B) 2) (multiple-value-list (most-least #'length '((a b)))) )
   (equal '((A B) 2 (A B) 2) (multiple-value-list (most-least #'length '((a b) (b c) (b a) (e f)))) )
   (equal '((A B C) 3 (A) 1) (multiple-value-list (most-least #'length '((a b) (a b c) (a) (z) (e f g)))) )
   (equal '((e f g h i j) 6 (a) 1) (multiple-value-list (most-least #'length '((a b) (a b c) (a) (e f g h i j)))) )
   (equal '(nil nil nil nil) (multiple-value-list (most-least #'char-code "")))
   (equal '(#\a 97 #\a 97) (multiple-value-list (most-least #'char-code "a")))
   (equal '(#\a 97 #\a 97) (multiple-value-list (most-least #'char-code "aaaaaaaa")))
   (equal '(#\u 117 #\SPACE 32) (multiple-value-list (most-least #'char-code "Is this not pung?")))
   (equal '("abc" 3 "a" 1) (multiple-value-list (most-least #'length #("ab" "abc" "a" "efg"))))
   (equal '("abcdefg" 7 "ab" 2) (multiple-value-list (most-least #'length #("ab" "abc" "abcdefg" "efg"))))
   (equal '(nil nil nil nil) (multiple-value-list (most-least #'abs #())))
   (equal '(8 8 8 8) (multiple-value-list (most-least #'abs #(8))))
   (equal '(8 8 8 8) (multiple-value-list (most-least #'abs #(8 -8 8 -8))))
   (equal '(-28 28 0 0) (multiple-value-list (most-least #'abs #(-9 8 -7 3 25 0 -28))))
   (equal '(28 28 0 0) (multiple-value-list (most-least #'abs #(-9 8 -7 3 25 28 0 -28))))
   (equal '(4 3 0 0) (multiple-value-list (most-least #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))))
   (equal '(-8 3 -1 0) (multiple-value-list (most-least #'integer-length (reverse #(0 1 3 4 7 -1 -4 -7 -8)))) )))

(deftest test-mostn ()
  (check
   (equal '(nil nil) (multiple-value-list (mostn #'length '())))
   (equal '(((A B) (A C) (B A) (E G)) 2) (multiple-value-list (mostn #'length '((a b) (a c) (b a) (e g)))) )
   (equal '(((A B) (A C) (E G)) 2) (multiple-value-list (mostn #'length '((a b) (a c) (a) (e g)))) )
   (equal '(((A B C) (E F G)) 3) (multiple-value-list (mostn #'length '((a b) (a b c) (a) (e f g)))) )
   (equal '(("abc" "efg") 3) (multiple-value-list (mostn #'length #("ab" "abc" "a" "efg"))))
   (equal '((#\u) 117) (multiple-value-list (mostn #'char-code "Is this not pung?")))
   (equal '((-28) 28) (multiple-value-list (mostn #'abs #(-9 8 -7 3 25 0 -28))))
   (equal '((28 -28) 28) (multiple-value-list (mostn #'abs #(-9 8 -7 3 25 28 0 -28)))) ))

(deftest test-leastn ()
  (check
   (equal '(nil nil) (multiple-value-list (leastn #'length '())))
   (equal '(((A B) (A C) (B A) (E G)) 2) (multiple-value-list (leastn #'length '((a b) (a c) (b a) (e g)))) )
   (equal '(((A)) 1) (multiple-value-list (leastn #'length '((a b) (a c) (a) (e g)))) )
   (equal '(((A) (B)) 1) (multiple-value-list (leastn #'length '((a b) (a b c) (a) (e f g) (b)))) )
   (equal '(("x" "a") 1) (multiple-value-list (leastn #'length #("ab" "x" "abc" "a" "efg"))))
   (equal '((#\space #\space #\space) 32) (multiple-value-list (leastn #'char-code "Is this not pung?")))
   (equal '((0 0 0.0) 0) (multiple-value-list (leastn #'abs #(-9 8 0 -7 3 25 0 -28 0.0)))) ))

(deftest test-most-least-n ()
  (check
   (equal '(nil nil nil nil) (multiple-value-list (most-least-n #'length '())))
   (equal '(((A B) (A C) (B A) (E G)) 2 ((A B) (A C) (B A) (E G)) 2) (multiple-value-list (most-least-n #'length '((a b) (a c) (b a) (e g)))) )
   (equal '(((A B) (A C) (E G)) 2 ((A)) 1) (multiple-value-list (most-least-n #'length '((a b) (a c) (a) (e g)))))
   (equal '(((X X X)) 3 ((A)) 1) (multiple-value-list (most-least-n #'length '((a b) (a c) (a) (e g) (x x x)))))
   (equal '((-28) 28 (0) 0) (multiple-value-list (most-least-n #'abs #(-9 8 -7 3 25 0 -28))))
   (equal '((28 -28) 28 (0) 0) (multiple-value-list (most-least-n #'abs #(-9 8 -7 3 25 28 0 -28))))
   (equal '((4 7 -7 -8) 3 (0 -1) 0) (multiple-value-list (most-least-n #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))))
   (equal '((#\u) 117 (#\Space  #\Space  #\Space ) 32) (multiple-value-list (most-least-n #'char-code "Is this not pung?")))
   (equal '(((X X X)) 3 ((A)) 1) (multiple-value-list (most-least-n #'length #((a b) (a c) (a) (e g) (x x x)))) )))

(deftest test-best ()
  (check
   (equal '() (best #'> '()))
   (equal '() (best #'> #()))
   (= 1 (best #'> '(1 1 1)))
   (= 1 (best #'< '(1 1 1)))
   (= 5 (best #'> '(1 2 3 4 5)))
   (= 1 (best #'< '(1 2 3 4 5)))
   (= 5 (best #'> (shuffle (vector 1 2 3 4 5))))
   (= 1 (best #'< (shuffle (vector 1 2 3 4 5))))
   (char= #\u (best #'char> "Is this not pung?"))
   (char= #\Space (best #'char< "Is this not pung?"))
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (string= "Tom" (name (best #'tallerp dudes))))
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (string= "Bob" (name (best #'shorterp dudes)))) ))

(deftest test-worst ()
  (check
   (equal '() (worst #'> '()))
   (equal '() (worst #'> #()))
   (= 1 (worst #'> '(1 1 1)))
   (= 1 (worst #'< '(1 1 1)))
   (= 1 (worst #'> '(1 2 3 4 5)))
   (= 5 (worst #'< '(1 2 3 4 5)))
   (= 1 (worst #'> (shuffle (vector 1 2 3 4 5))))
   (= 5 (worst #'< (shuffle (vector 1 2 3 4 5))))
   (char= #\Space (worst #'char> "Is this not pung?"))
   (char= #\u (worst #'char< "Is this not pung?"))
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (string= "Bob" (name (worst #'tallerp dudes))))
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (string= "Tom" (name (worst #'shorterp dudes)))) ))

(deftest test-best-worst ()
  (check
   (equal '(nil nil) (multiple-value-list (best-worst #'> '())))
   (equal '(nil nil) (multiple-value-list (best-worst #'> #())))
   (equal '(1 1) (multiple-value-list (best-worst #'> '(1 1 1))))
   (equal '(1 1) (multiple-value-list (best-worst #'< '(1 1 1))))
   (equal '(5 1) (multiple-value-list (best-worst #'> '(1 2 3 4 5))))
   (equal '(1 5) (multiple-value-list (best-worst #'< '(1 2 3 4 5))))
   (equal '(5 1) (multiple-value-list (best-worst #'> (shuffle (vector 1 2 3 4 5)))) )
   (equal '(1 5) (multiple-value-list (best-worst #'< (shuffle (vector 1 2 3 4 5)))) )
   (equal '(#\u #\Space) (multiple-value-list (best-worst #'char> "Is this not pung?")))
   (equal '(#\Space #\u) (multiple-value-list (best-worst #'char< "Is this not pung?")))
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (equal '("Tom" "Bob") (mapcar #'name (multiple-value-list (best-worst #'tallerp dudes)))) )
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70))))
     (equal '("Bob" "Tom") (mapcar #'name (multiple-value-list (best-worst #'shorterp dudes)))) )))

(deftest test-bestn ()
  (check
   (equal (bestn #'> '()) '())
   (equal (bestn #'> '(1 1 1)) '(1 1 1))
   (equal (bestn #'< '(1 1 1)) '(1 1 1))
   (equal (bestn #'> '(1 2 3 4 5)) '(5))
   (equal (bestn #'> '(1 2 5 3 4 5)) '(5 5))
   (equal (bestn #'> '(4 1 2 3 4 5)) '(5))
   (equal (bestn #'< '(1 2 3 4 5)) '(1))
   (equal (bestn #'> #()) '())
   (equal (bestn #'> (shuffle (vector 1 2 3 4 5 5 5))) '(5 5 5))
   (equal (bestn #'< (shuffle (vector 1 1 1 2 3 4 5))) '(1 1 1))
   (equal (bestn #'char> "Is this not png?") '(#\t #\t))
   (equal (bestn #'char-greaterp "Is this NOT png?") '(#\t #\T))
   (equal (bestn #'char< "Is this not pung?") '(#\Space #\Space #\Space))
   (let ((dudes (list (make-instance 'dude :name "Bob" :height 64)
                      (make-instance 'dude :name "Tom" :height 73)
                      (make-instance 'dude :name "Larry" :height 70)
                      (make-instance 'dude :name "Jim" :height 73))))
     (equal (mapcar #'name (bestn #'tallerp dudes)) '("Tom" "Jim")))) )


(deftest test-mapa-b ()
  (check
   (equal '(NIL T NIL T NIL) (mapa-b #'evenp 1 5))
   (equal '(T NIL T NIL T NIL) (mapa-b #'evenp 0 5))
   (equal '(1 2 0 1 2 0 1 2 0 1) (mapa-b (partial* #'mod 3) 1 10))
   (equal '(0 1 2 0 1 2 0 1 2 0) (mapa-b (partial* #'mod 3) 0 9))
   (equal (mapcar #'sqrt #[1d0 10d0]) (mapa-b #'sqrt 1d0 10d0))))

(deftest test-map0-n ()
  (check
   (equal '(T NIL T NIL T NIL) (map0-n #'evenp 5))
   (equal '(0 1 2 0 1 2 0 1 2 0) (map0-n (partial* #'mod 3) 9))))

(deftest test-map1-n ()
  (check
   (equal '(NIL T NIL T NIL) (map1-n #'evenp 5))
   (equal '(1 2 0 1 2 0 1 2 0 1) (map1-n (partial* #'mod 3) 10))))

(deftest test-range ()
  (check
   (null (range 0))
   (equal #[1] (range 1))
   (equal #[5] (range 5))
   (equal #[1 5] (range 1 5))
   (equal #[1 5] (range 1 5 1))
   (equal #[1 5 2] (range 1 5 2))
   (equal #[1 5 0.8] (range 1 5 0.8))
   (equal #[1 5 1/5] (range 1 5 1/5))
   (equal #[5 1] (range 5 1))
   (equal #[5 1] (range 5 1 1))
   (equal #[5 1 1/5] (range 5 1 1/5))
;; Can't do this with Haskell??  (take 10 (iterate (partial * 2) 1)) (1 2 4 8 16 32 64 128 256 512)
   (equal '(1 2 4 8 16 32 64 128 256 512 1024) (range 1 1024 (partial #'* 2)))
;; Newton-Raphson
   (equal #[1 20 3] (range 1 20 (partial #'+ 3)))
   (equal #[#\c] (range #\c)) ; Not what you think!!
   (equal #[#\a #\z] (range #\a #\z))
   (equal #[#\a #\z 3] (range #\a #\z 3))
   (equal #[#\z #\a] (range #\z #\a))
   (equal #[#\z #\a 2] (range #\z #\a 2))
   (handler-case (range 1 10 0)
     (error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "STEP of 0 not allowed.")))
   (handler-case (range 1 10 -0.2)
     (error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Negative STEP not allowed.")))
   (handler-case (range #\a #\m 0.1)
     (error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Fractional STEP not allowed for characters.")))) )

(deftest test-map-> ()
  (check
   ;; (mapa-b #'1+ -2 0 0.5)
   (equal (map-> #'1+ -2 #'(lambda (x) (> x 0)) #'(lambda (x) (+ x 0.5))) '(-1 -0.5 0.0 0.5 1.0))
   ;; (mapcar #'length '("Is" "this" "not" "pung?"))
   (equal (map-> #'(lambda (l) (length (first l))) '("Is" "this" "not" "pung?") #'null #'rest) '(2 4 3 5))
   (equal (map-> (compose #'length #'first) '("Is" "this" "not" "pung?") #'null #'rest) '(2 4 3 5))
   ;; (maplist #'reverse '(1 2 3 4))
   (equal (map-> #'reverse '(1 2 3 4) #'null #'rest) '((4 3 2 1) (4 3 2) (4 3) (4)))
   (equal (map-> #'(lambda (i) (list (code-char i) i)) (char-code #\p) #'(lambda (i) (> i (char-code #\z))) #'(lambda (x) (+ x 2))) '((#\p 112) (#\r 114) (#\t 116) (#\v 118) (#\x 120) (#\z 122)))
   (equal (map-> #'(lambda (l) (string-upcase (first l))) #1='("Is" "this" "not" "pung?") #'null #'cdr) (mapcar #'string-upcase #1#))
   (equal (map-> (compose #'string-upcase #'first) #2='("Is" "this" "not" "pung?") #'null #'cdr) (mapcar #'string-upcase #2#))
   (equal '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0) (map-> #'(lambda (x) (log x 2)) 1 #'(lambda (x) (> x 1024)) #'(lambda (x) (* 2 x))))
   (equal '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192) (map-> #'identity 1 (partial #'< 10000) (partial #'* 2)))) )


;; (map-> #'identity 1 (partial #'< 1000) (partial #'* 2))
;; (1 2 4 8 16 32 64 128 256 512)
;; * 
;; *

;; (let ((current 0)
;;       (next 1)
;;       (count 0))
;;   (labels ((fib (_) (declare (ignore _)) current)
;;            (succ (i) (declare (ignore i)) (psetf current next next (+ current next)) (incf count))
;;            (test (_) (declare (ignore _)) (> count 10)))
;;     (map-> #'fib 0 #'test #'succ)))





(deftest test-mapcars ()
  (check
   (equal (mapcars #'abs '(2 5 6) '(99 -23 -8)) '(2 5 6 99 23 8))
   (equal (mapcars #'sqrt (range 2 7) (range 9 4)) '(1.4142135 1.7320508 2.0 2.236068 2.4494898 2.6457512 3.0 2.828427 2.6457512 2.4494898 2.236068 2.0))))

;;;
;;;    See PAIP ch.1 (pg. 19)
;;;
(defun number-and-negation (obj)
  (if (numberp obj)
      (list obj (- obj))
      nil))

(deftest test-mappend ()
  (check
   (equal (mappend #'number-and-negation '(testing 1 2 3 test))
          (mapcan #'number-and-negation '(testing 1 2 3 test)))
   (equal (mappend #'list '(a b c d) '(1 2 3 4))
          (mapcan #'list '(a b c d) '(1 2 3 4)))
   (equal (mappend #'list '(a b c d) '(1 2 3))
          (mapcan #'list '(a b c d) '(1 2 3)))
   (equal (mappend #'list '(a b c) '(1 2 3 4))
          (mapcan #'list '(a b c) '(1 2 3 4)))) )
  
(deftest test-iterate ()
  (check
   (let ((iterator (iterate #'1+ 0)))
     (equal (loop for i from 0 below 5 collect i) (loop repeat 5 collect (funcall iterator))))
   (let ((iterator (iterate (partial #'* 2) 1)))
     (equal (loop for i from 0 below 10 collect (expt 2 i)) (loop repeat 10 collect (funcall iterator)))) ))

(deftest test-rmapcar ()
  (check
   (equal (rmapcar #'1+ '(1 2 (3 4 (5) 6) 7 (8 9))) '(2 3 (4 5 (6) 7) 8 (9 10)))
   (equal (rmapcar #'(lambda (s1 s2) (concatenate 'string s1 s2)) '("Is" ("this" ("not" ("pung?")))) '("Ça" ("plane" ("pour" ("moi" "Plastic" "Bertrand")))) ) '("IsÇa" ("thisplane" ("notpour" ("pung?moi")))) )))

(deftest test-tree-map ()
  (check
   (equal (tree-map #'length "is") 2)
   (equal (tree-map #'length '("Ça" ("plane" ("pour" ("moi" "Plastic" "Bertrand")))) ) '(2 (5 (4 (3 7 8)))) )
   (equal (tree-map #'length '("Ça" ("plane" ("pour" ("moi" . "Plastic") . "Bertrand")))) '(2 (5 (4 (3 . 7) . 8))))
   (equal (tree-map #'< 1 2) T)
   (equal (tree-map #'1+ '((1 2) (3 4))) '((2 3) (4 5)))
   (equal (tree-map #'cons '(a b c) '((d e) (f g) (h i)))
          '((A D E) (B F G) (C H I)))
   (equal (tree-map #'cons '((a) (b) (c)) '((d e) (f g) (h i)))
          '(((A . D)) ((B . F)) ((C . H))))
   (equal (tree-map #'cons '(a (b c)) '((d e) ((f g) (h i))))
          '((A D E) ((B F G) (C H I))))
   (equal (tree-map #'cons '(a b) '((d e) (f g) (h i)))
          '((A D E) (B F G)))
   (equal (tree-map #'cons '(a b c) '((d e) (f g)))
          '((A D E) (B F G)))
   (equal (tree-map #'cons '(a b c) '((1) (2 3) (4 5 6)))
          '((A 1) (B 2 3) (C 4 5 6)))
   (equal (tree-map #'cons '((a . b) (c . d) (e . f)) '((1 . 2) (3 . 4) (5 . 6))) '(((A . 1) B . 2) ((C . 3) D . 4) ((E . 5) F . 6)))
   (equal (tree-map #'list '((a . b) (c . d) (e . f)) '((1 . 2) (3 . 4) (5 . 6))) '(((A 1) B 2) ((C 3) D 4) ((E 5) F 6)))
   (equal (tree-map #'cons '(a b c d e f) '(1 2 3 4 5 6)) '((A . 1) (B . 2) (C . 3) (D . 4) (E . 5) (F . 6)))
   (equal (tree-map #'list '(a b c d e f) '(1 2 3 4 5 6)) '((A 1) (B 2) (C 3) (D 4) (E 5) (F 6)))
   (equal (tree-map #'1+ '((1 . 2) (3 . 4) (5 . 6))) '((2 . 3) (4 . 5) (6 . 7)))
   (equal (tree-map #'+ '((1 . 2) (3 . 4) (5 . 6)) '((9 . 9) (9 . 9) (9 . 9)))
          '((10 . 11) (12 . 13) (14 . 15)))
   (equal (tree-map #'+ '((1 . 2) (3 . 4) (5 . 6)) '((9 . 9) (9 . 9)))
          '((10 . 11) (12 . 13)))
   (equal (tree-map #'+ '((1 2) (3 4) (5 6)) '((9 9) (9 9) (9 9)))
          '((10 11) (12 13) (14 15)))
   (equal (tree-map #'+ '((1 . 2) (3 . 4) (5 . 6) . 7) '((9 . 9) (9 . 9) (9 . 9) . 9))
          '((10 . 11) (12 . 13) (14 . 15) . 16))
   (equal (tree-map #'+ '((1 2) (3 4) (5)) '((9 9) (9) (9 9)))
          '((10 11) (12) (14)))
   (equal (tree-map #'+ '((1 2) (3 4) (5) (6 7)) '((9 9) (9) (9 9) (9 9)) '((0.5) (0.5 0.5) (0.5 0.5) (0.5 0.5)))
          '((10.5) (12.5) (14.5) (15.5 16.5)))
   (equal (tree-map #'+ '((1 2) (3 4) (5) (6 7)) '((9 9) (9) (9 9) (9 9)) '((0.5) (0.5 0.5) (0.5 0.5) (0.5 0.5) (0.5 0.5)))
          '((10.5) (12.5) (14.5) (15.5 16.5)))
   (equal (tree-map #'1+ '(1 2 (3 4 (5) 6) 7 (8 9))) '(2 3 (4 5 (6) 7) 8 (9 10)))
   (equal (tree-map #'(lambda (s1 s2) (concatenate 'string s1 s2)) '("Is" ("this" ("not" ("pung?")))) '("Ça" ("plane" ("pour" ("moi" "Plastic" "Bertrand")))) ) '("IsÇa" ("thisplane" ("notpour" ("pung?moi")))))
   ;;
   ;;    See Slade ch. 4 exercises.
   ;;    TREE-AVERAGE/TREE-ADDITION
   ;;    
   (= (let ((count 0) (sum 0)) (tree-map #'(lambda (x) (incf count) (incf sum x)) '(1 2 (3 (4 (5) 6) (7)) 8 (9))) (/ sum count)) 5)
   (= (let ((count 0) (sum 0)) (tree-map #'(lambda (x) (incf count) (incf sum x)) '((1 . 2) (3 (4 (5 . 6))) (7) 8 . 9)) (/ sum count)) 5)
   (= (let ((count 0) (sum 0)) (tree-map #'(lambda (x) (incf count) (incf sum x)) '(((( ((1)) )))) ) (/ sum count)) 1)
   (equal (tree-map #'(lambda (x) (+ x 2)) '(5 4 3 2 1)) '(7 6 5 4 3))
   (equal (tree-map #'(lambda (x) (+ x 3)) '(1 2 (3 (4 (5) 6) (7)) 8 (9))) '(4 5 (6 (7 (8) 9) (10)) 11 (12)))
   (equal (tree-map #'(lambda (x) (+ x 5)) '(((( (1) )))) ) '(((( (6) )))) )))

;;;
;;;    见 ~/lisp/books/Tanimoto/ch02/2010/ch02.lisp
;;;    
(defun exify (obj)
  (build-tree #'(lambda (_) (declare (ignore _)) 'x) obj))

(deftest test-exify ()
  (check
   (equal '(x (x x) x x nil x) (exify '(a (b c) x y nil z)))) )

(defun make-copy (obj)
  (build-tree #'identity obj))

(deftest test-make-copy ()
  (check
   (equal #1='(((0) (1) (2) 3) ((4) (5) ((6) ((7) (8) (9) 10) (11) 12) 13) (14) 15) (make-copy #1#))
   (equal #2='((((0) (1) (2) ((3) 4) 5) (6) ((7) (((8) (9) 10) ((((11) 12) ((13) 14) (15) 16) (17) 18) 19) 20) 21) (22) 23) (make-copy #2#))
   (equal #3='((0) (((1) (((((((((2) (((3) (((4) ((5) (6) 7) 8) (9) (((10) 11) 12) 13) 14) (15) (16) 17) (18) (19) 20) (((21) (22) (23) ((24) ((25) (26) 27) 28) 29) 30) 31) 32) 33) (((34) (35) 36) 37) (((38) 39) 40) 41) 42) (43) (44) 45) 46) 47) 48) 49) (make-copy #3#))))

(defun tree-plus (obj delta)
  (build-tree #'(lambda (x) (+ x delta)) obj))

(deftest test-tree-plus ()
  (check
   (equal (tree-plus 8 9) 17)
   (equal (tree-plus '(1 (2 ((3) 4) (5 (6)))) 9) '(10 (11 ((12) 13) (14 (15)))) )))

(defun tree-upcase (obj)
  (build-tree #'string-upcase obj))

(deftest test-tree-upcase ()
  (check
   (equal (tree-upcase "pung") "PUNG")
   (equal (tree-upcase '("is" ("this") ("not" ((("pung?")))))) '("IS" ("THIS") ("NOT" ((("PUNG?"))))) )))

(deftest test-compose ()
  (check
   (equal (funcall (compose #'first #'rest #'rest #'rest #'rest) #1='(a b c d e f)) (fifth #1#))
   (equal (mapcar (compose #'1+ #'1+) #2=(loop for i from 1 to 10 collect i)) (mapcar #'(lambda (x) (+ x 2)) #2#))
   ;; IDENTITY!
   (equal #3=(loop for i from 1 to 10 collect i) (mapcar (compose #'1+ #'1-) #3#))
   (equal (mapcar (compose #'list #'(lambda (x) (* x 2))) #4=(loop for i from 1 to 5 collect i)) (mapcar #'(lambda (x) (list (* x 2))) #4#))
   ;; COUNT-IF
   (equal (funcall (compose #'length #'remove-if-not) #'evenp #5=(loop for i from 1 to 10 collect i)) (length (remove-if-not #'evenp #5#)))
   (equal (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4)) 4)
   (equal (mapcar (compose #'length #'cons) '(a b c d) '((1 2) (3) () (4 5 6))) '(3 2 1 4))
   ;; COMPLEMENT
   (equal (mapcar (compose #'not #'evenp) #6=(loop for i from 1 to 10 collect i)) (mapcar (complement #'evenp) #6#))))

(deftest test-juxtapose ()
  (check
   (equal (multiple-value-list (funcall (juxtapose #'truncate #'floor #'ceiling #'round) 23 10)) '((2 3) (2 3) (3 -7) (2 3)))
   (equal (multiple-value-list (funcall (juxtapose #'truncate #'floor #'ceiling #'round) 2.3)) '((2 0.29999995) (2 0.29999995) (3 -0.70000005) (2 0.29999995)))
   (equal (multiple-value-list (funcall (juxtapose #'string-upcase #'string-downcase #'string-capitalize) "Is this not pung?")) '(("IS THIS NOT PUNG?") ("is this not pung?") ("Is This Not Pung?")))) )

(deftest test-partial ()
  (check
   (= 6 (funcall (partial #'reduce #'+) '(1 2 3)))
   (funcall (partial #'> (funcall (partial #'reduce #'+) '(1 2 3))) 3)
   (= (1+ 8) (funcall (partial #'+ 1) 8))
   (string= "Twelve thousand three hundred forty-five" (funcall (partial #'format nil "~@(~R~)") 12345))
   (equal '(B C 2 Y Z) (funcall (compose (partial #'apply #'nconc) #'mapcar) #'rest (copy-tree '((a b c) (1 2) (x y z))))) ; MAPCAN
   (let ((xxs '((1 2 3 4 5) (5 4 5 4 3 2) (7 8 9 0 1 2 3)))) ; Learn You A Haskell 18 页
     (equal '((2 4) (4 4 2) (8 0 2)) (mapcar (partial #'remove-if-not #'evenp) xxs)))
   (every (compose (partial #'= (length "pung")) #'length) '("over" "your" "turn" "send"))))
;   (every #'(lambda (s) (= (length "pung") (length s))) '("over" "your" "turn" "send"))))

(deftest test-partial* ()
  (check
   (funcall (partial* #'typep 'atom) 'a) ; ATOM
   (not (funcall (partial* #'typep 'atom) '(1 2)))
   (= (funcall (partial* #'- 1) 8) (1- 8))
   (equal '(t nil t nil) (mapcar (compose (partial* #'< 10000) #'abs) '(23 12345 -80 -80000)))) )

(defun smallp (s)
  (< (length s) 10))

(defun odd-length-p (s)
  (oddp (length s)))

(setf (symbol-function 'append-if) (curry (pred s suffix) (if (funcall pred s) (concatenate 'string s suffix) s)))

;; (curry (x y) (* x (expt 2 y))) <-- Can't curry every function???
(deftest test-curry ()
  (check
   (let ((f (curry (x y z) (+ x (* y z)))))
     (and (functionp f)
          (let ((g (funcall f 3)))
            (and (functionp g)
                 (let ((h (funcall g 5)))
                   (and (functionp h)
                        (let ((v (funcall h 8)))
                          (and (not (functionp v))
                               (= 43 v)))) )))) )
   (equal "Hello World"
          (funcall (funcall (funcall #'append-if #'smallp) "Hello World") "!!!"))
   (equal "Hi World!!!"
          (funcall (funcall (funcall #'append-if #'smallp) "Hi World") "!!!"))
   (equal "Hello World!!!"
          (funcall (funcall (funcall #'append-if #'odd-length-p) "Hello World") "!!!"))
   (equal "Hi World"
          (funcall (funcall (funcall #'append-if #'odd-length-p) "Hi World") "!!!"))
   (equal "IS THIS NOT PUNG?"
          (funcall (funcall (curry (n) (if (evenp n) #'string-upcase #'string-downcase)) 0) "Is this not pung?"))
   (equal "is this not pung?"
          (funcall (funcall (curry (n) (if (evenp n) #'string-upcase #'string-downcase)) 9) "Is this not pung?"))))

;; CORE(72): (curry (x y z) (+ x (* y z)))
;; #<Interpreted Function (unnamed) @ #x10007825172>
;; CORE(73): (curry-apply * '(3 5 8))
;; 43
;; CORE(74): (curry-apply ** '(3 5))
;; #<Interpreted Closure (unnamed) @ #x1000782ecf2>
;; CORE(75): (curry-apply * '(8))
;; 43

;; CORE(77): (curry-call (curry (x y z) (+ x (* y z))) 3 5 8)
;; 43
;; CORE(78): (curry-call (curry (x y z) (+ x (* y z))) 3 5)
;; #<Interpreted Closure (unnamed) @ #x10007845482>
;; CORE(79): (curry-call * 8)
;; 43
;; CORE(80): (curry () (+ 2 3))
;; 5
;; CORE(81): (curry-call (curry () (+ 2 3)))
;; 5
;; CORE(82): (curry-apply (curry () (+ 2 3)) '())
;; 5


;;;
;;;    Look for examples in other tests.
;;;    
(deftest test-iffn ()
  (check
   (equal (mapcar (iffn #'oddp #'1+ #'1-) (loop for i from 1 to 6 collect i)) (mapcar #'(lambda (n) (if (oddp n) (1+ n) (1- n))) (loop for i from 1 to 6 collect i)))
   (equal (mapcar (iffn #'oddp #'1+ #'identity) (loop for i from 1 to 6 collect i)) (mapcar #'(lambda (n) (if (oddp n) (1+ n) n)) (loop for i from 1 to 6 collect i)))
   (equal (mapcar (iffn #'integerp #'oddp) '(1 2 3 c)) '(t nil t nil)) ; Defect? No way to distinguish between missing 'else' function and function that returns NIL?
   (equal (mapcar (iffn #'evenp #'1-) (loop for i from 1 to 6 collect i)) (mapcar #'(lambda (n) (if (evenp n) (1- n))) (loop for i from 1 to 6 collect i)))) )

;;;
;;;    Look for examples in other tests. Good for FILTER!!
;;;    
(deftest test-every-pred ()
  (check
   (equal (mapcar (every-pred #'integerp #'oddp) '(a "a" 2 3)) '(nil nil nil t))
   (funcall (every-pred #'integerp #'oddp #'plusp) 3)
   (not (funcall (every-pred #'integerp #'oddp #'plusp) 3.0))
   (not (funcall (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) 3))
   (funcall (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) 7)
   (every (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '(7))
   (every (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '(7 21 35))
   (every (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '())
   (some (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '(8 21 200))
   (let* ((f #'integerp) (g #'oddp) (h #'plusp) (i #'(lambda (x) (zerop (mod x 7)))) (preds (list f g h i)))
     (every (apply #'every-pred preds) '(7 21 35)))) )

(deftest test-some-pred ()   
  (check
   (equal (mapcar (some-pred #'integerp #'symbolp) '(a "a" 2 3)) '(t nil t t))
   (funcall (some-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) -3)
   (some (some-pred #'integerp #'plusp #'(lambda (x) (zerop (mod x 7)))) '(9.0 7.0 -21.0))
   (some (some-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '(9 7 -22 35))
   (every (some-pred (every-pred #'integerp #'oddp) #'plusp #'(lambda (x) (zerop (mod x 7)))) '(9 7.0 22 -42))
   (not (some (some-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '()))) )

;;;
;;;    This one is weird...
;;;    
(deftest test-partition ()
  (check
   (equal (multiple-value-list (partition '())) '(() ()))
   (equal (multiple-value-list (partition '(a))) '(() (a)))
   (equal (multiple-value-list (partition '(a b))) '((a) (b)))
   (equal (multiple-value-list (partition '(a b c))) '((b) (a c)))
   (equal (multiple-value-list (partition '(a b c d))) '((a c) (b d)))) )

(deftest test-stable-partition ()
  (check
   (equal (multiple-value-list (stable-partition '())) '(() ()))
   (equal (multiple-value-list (stable-partition '(a))) '((a) ()))
   (equal (multiple-value-list (stable-partition '(a b))) '((a) (b)))
   (equal (multiple-value-list (stable-partition '(a b c))) '((a c) (b)))
   (equal (multiple-value-list (stable-partition '(a b c d))) '((a c) (b d)))) )

(deftest test-stable-stream-partition ()
  (check
   (equal (multiple-value-list (stable-stream-partition (make-string-input-stream ""))) '(() ()))
   (equal (multiple-value-list (stable-stream-partition (make-string-input-stream "p"))) '((#\p) ()))
   (equal (multiple-value-list (stable-stream-partition (make-string-input-stream "pu"))) '((#\p) (#\u)))
   (equal (multiple-value-list (stable-stream-partition (make-string-input-stream "pun"))) '((#\p #\n) (#\u)))
   (equal (multiple-value-list (stable-stream-partition (make-string-input-stream "pung"))) '((#\p #\n) (#\u #\g)))) )

(deftest test-prefix-generator ()
  (check
   (equal (loop with l = '() with generator = (prefix-generator l) repeat (1+ (length l)) collect (funcall generator))
          '(()))
   (equal (loop with l = '(a b c d) with generator = (prefix-generator l) repeat (1+ (length l)) collect (funcall generator))
          '(() (A) (A B) (A B C) (A B C D)))) )

(deftest test-build-prefix ()
  (check
   (let ((l '(a b c d)))
     (dotimes (i (1+ (length l)) t)
       (unless (equal (build-prefix l (nthcdr i l)) (take i l))
         (return nil)))) ))

(deftest test-destructure ()
  (check
   (let ((l1 '(a b c))
         (l2 '(1 2 3 4 5 6)))
     (equal (destructure ((x y z &optional zz) l1
                          (i j k . nums) l2)
              (list (list x y z zz) (list i j k nums)))
            '((a b c nil) (1 2 3 (4 5 6)))) )))

;macroexpand-all

;;;
;;;    TREE-MAP doesn't handle NIL properly here?
;;;
;; (let ((tree '(LET ((L '(A B C D)))
;;                                   (DOTIMES (I (1+ (LENGTH L)) T)
;;                                     (UNLESS
;;                                         (EQUAL (BUILD-PREFIX L (NTHCDR I L))
;;                                                (TAKE I L))
;;                                       (RETURN NIL)))))) (tree-map #'cons (analyze-tree tree) tree))

;; ((COMMON-LISP . LET)
;;  (((CORE . L)
;;    ((COMMON-LISP . QUOTE) ((CORE . A) (CORE . B) (CORE . C) (CORE . D)))))
;;  ((COMMON-LISP . DOTIMES)
;;   ((CORE . I) ((COMMON-LISP . 1+) ((COMMON-LISP . LENGTH) (CORE . L)))
;;    (COMMON-LISP . T))
;;   ((COMMON-LISP . UNLESS)
;;    ((COMMON-LISP . EQUAL)
;;     ((CORE . BUILD-PREFIX) (CORE . L)
;;      ((COMMON-LISP . NTHCDR) (CORE . I) (CORE . L)))
;;     ((CORE . TAKE) (CORE . I) (CORE . L)))
;;    ((COMMON-LISP . RETURN) NIL)))) ; <---------------------- 


(deftest test-analyze-tree ()
  (check
   (equal (analyze-tree nil) 'COMMON-LISP)
   (equal (analyze-tree '(a b c)) '(CORE CORE CORE))
   (equal (analyze-tree '(if (and p q) r s)) '(COMMON-LISP (COMMON-LISP CORE CORE) CORE CORE))
   (equal (analyze-tree '(LET ((L '(A B C D)))
                          (DOTIMES (I (1+ (LENGTH L)) T)
                            (UNLESS
                                (EQUAL (BUILD-PREFIX L (NTHCDR I L))
                                       (TAKE I L))
                              (RETURN NIL)))))
          '(COMMON-LISP ((CORE (COMMON-LISP (CORE CORE CORE CORE))))
            (COMMON-LISP (CORE (COMMON-LISP (COMMON-LISP CORE)) COMMON-LISP)
             (COMMON-LISP
              (COMMON-LISP (CORE CORE (COMMON-LISP CORE CORE)) (CORE CORE CORE))
              (COMMON-LISP COMMON-LISP)))) )))

(deftest test-for ()
  (check
   (let ((sum 0))
     (for (i 1 19)
       (incf sum i))
     (= sum 190))
   (let ((l '()))
     (for (i 2 5)
       (push i l))
     (equal l '(5 4 3 2)))) )

(deftest test-if-let ()
  (check
   (equal (if-let (p (evenp 9))
            (cons p '(b))
            :duh)
          :duh)
   (= (if-let (ns (rest '(1 2 3)))
        (apply #'+ ns)
        0)
      5)))

(deftest test-when-let ()
  (check
   (let ((i 0))
     (when-let (p (find 7 '(2 4 6 8)))
       (incf i))
     (zerop i))
   (let ((i 0))
     (when-let (p (find 7 '(2 4 6 8 7)))
       (incf i))
     (= i 1))))

(deftest test-when-let* ()
  (check
   (= (when-let* ((x (find-if #'consp '(a (1 2) b)))
                  (y (find-if #'oddp x)))
        (+ y 10))
      11)
   (= (when-let* ((x (find-if #'consp '(a (1 2) b)))
                  (y (find-if #'evenp x)))
        (+ y 10))
      12)
   (null (when-let* ((x (find-if #'consp '(a (1 2) b)))
                     (y (find-if #'zerop x)))
           (+ y 10)))) )

(deftest test-cond-let ()
  (labels ((foo (n)
             (cond-let (((zerop n) (x :pung) (y "Too small"))
                        ((oddp n) (x :foo) (y "Too odd"))
                        (t (x :bar) (y "Ahh...nice")))
                       (list x y))))
    (check
     (equal (cond-let (((= 1 2) (x (princ 'a)) (y (princ 'b)))
                       ((= 1 1) (y (princ 'c)) (x (princ 'd)))
                       (t (x (princ 'e)) y (z (princ 'f))))
                      (list x y z))
            '(d c nil))
     (equal (cond-let (((= 1 2) (x (princ 'a)) (y (princ 'b)))
                       ((= 1 3) (y (princ 'c)) (x (princ 'd)))
                       ((= 1 1)) ; Degenerate case...Still functions
                       (t (x (princ 'e))  (z (princ 'f))))
                      (list x y z))
            '(nil nil nil))
     (equal (foo 0) '(:PUNG "Too small"))
     (equal (foo 9) '(:FOO "Too odd"))
     (equal (foo 4) '(:BAR "Ahh...nice")))) )

(deftest test-if3 ()
  (check
   (eql (if3 (< 2 3) :foo :bar :baz) :foo)
   (eql (if3 (> 2 3) :foo :bar :baz) :bar)
   (eql (if3 '? :foo :bar :baz) :baz)
   (eql (if3 '#:? :foo :bar :baz) :baz))) ; Any symbol with name "?" should be uncertain.

(deftest test-nif ()
  (check
   (equal (mapcar #'(lambda (x) (nif x 'p 'z 'n)) '(0.0 1d0 -1)) '(z p n))))

(deftest test-in ()
  (check
   (in (mod 11 4) (3 (+ 2 2) (/ 2 0)))
   (in (mod 11 7) (3 (+ 2 2) (/ 2 0)))
   (handler-case (in (mod 11 1) (3 (+ 2 2) (/ 2 0)))
     (error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       nil))
   (let ((operator '+))
     (in operator ('+ '- '* '/)))
   (not (let ((operator '%))
          (in operator ('+ '- '* '/)))) ))

(deftest test-inq ()
  (check
   (let ((operator '+))
     (inq operator (+ - * /)))
   (let ((operator '%))
     (not (inq operator (+ - * /)))) ))

(deftest test-in-if ()
  (check
   (in-if #'oddp (2 (/ 6 2) (/ 2 0)))
   (handler-case (in-if #'zerop (2 (/ 6 2) (/ 2 0)))
     (error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       nil))))

(deftest test->case ()
  (check
   (eq (>case 9
         (((+ 8 1) (* 3 3)) :yep)
         (((+ 3 4)) :nope)
         (((/ 3 0)) :whoops)
         (t :huh))
       :yep)
   (eq (>case 7
         (((+ 8 1) (* 3 3)) :yep)
         (((+ 3 4)) :nope)
         (((/ 3 0)) :whoops)
         (t :huh))
       :nope)
   (handler-case (>case 8
                   (((+ 8 1) (* 3 3)) :yep)
                   (((+ 3 4)) :nope)
                   (((/ 3 0)) :whoops)
                   (t :huh))
     (error (e)
       (declare (ignore e))
       t)
     (:no-error (obj)
       (declare (ignore obj))
       nil))))

(defun route (source destination path)
  (let ((link (gethash source path)))
    (cond ((null link) nil)
          ((eql destination link) t)
          (t (route link destination path)))) )

;;;
;;;    LPN ch. 10
;;;    
(deftest test-open-path ()
  (check
   (let ((graph (make-hash-table)))
     (open-path (source destination) '(nancy metz faulquemont stAvold freyming forbach saarbrücken dudweiler)
       (setf (gethash source graph) destination))
     (route 'nancy 'dudweiler graph))))
                                       

;DOTUPLES
;PARTITION-N
;MAPTUPLES

;; (deftest test-maptuples ()
;;   (check
;;    (equal (maptuples #'+ (x y) '(1 2 3 4)) (mapcar #'(lambda (args) (apply #'+ args)) (group '(1 2 3 4) 2)))
;;    (equal (maptuples #'+ (x y z) '(1 2 3 4 5 6)) (mapcar #'(lambda (args) (apply #'+ args)) (group '(1 2 3 4 5 6) 3)))) )

(deftest test-maptuples ()
  (check
   (equal (maptuples #'+ 2 '(1 2 3 4)) '(3 7))
   (equal (maptuples #'+ 3 '(1 2 3 4 5 6)) '(6 15))))


;; (setf (symbol-function 'my-length) (lrec #'(lambda (x f) (declare (ignore x)) (1+ (funcall f))) 0))
;; (setf (symbol-function 'every-oddp) (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t))
;; (setf (symbol-function 'my-copy-list) (lrec #'(lambda (x f) (cons x (funcall f)))))
;; (setf (symbol-function 'my-remove-duplicates) (lrec #'(lambda (x f) (adjoin x (funcall f)))))
;; (setf (symbol-function 'find-if-odd) (lrec #'(lambda (x f) (if (oddp x) x (funcall f)))))

;; (setf (symbol-function 'my-copy-tree) (ttrav #'cons))
;; (my-copy-tree '(a (b (c (d))))) => (A (B (C (D))))
;; (setf (symbol-function 'count-leaves) (ttrav #'(lambda (l r) (+ l (or r 1))) 1))
;; (count-leaves '(a (b (c (d))))) => 8
;; (count-leaves '((a b (c d)) (e) f)) => 10
;; (setf (symbol-function 'flatten) (ttrav #'nconc #'mklist))
;; (flatten '((a b (c d)) (e) f)) => (A B C D E F)

;; (setf (symbol-function 'flatten) (trec #'(lambda (o l r) (declare (ignore o)) (nconc (funcall l) (funcall r))) #'mklist))
;; (flatten '((a b (c d)) (e) f)) => (A B C D E F)
;; (setf (symbol-function 'rfind-if-oddp) (trec #'(lambda (o l r) (declare (ignore o)) (or (funcall l) (funcall r))) #'(lambda (tree) (and (oddp tree) tree))))
;; (rfind-if-oddp '(2 (4 ((6 3) 8) 7))) => 3

(deftest test-firsts-rests ()
  (check
   (equal (multiple-value-list (firsts-rests '())) '(() ()))
   (equal (multiple-value-list (firsts-rests '(())))  '(() ()))
   (equal (multiple-value-list (firsts-rests '(() (a)))) '(() ()))
   (equal (multiple-value-list (firsts-rests '((a) ()))) '(() ()))
   (equal (multiple-value-list (firsts-rests '((a b) ()))) '(() ()))
   (equal (multiple-value-list (firsts-rests '((a b c)))) '((a) ((b c))))
   (equal (multiple-value-list (firsts-rests '((a b c) (1 2 3)))) '((a 1) ((b c) (2 3))))
   (equal (multiple-value-list (firsts-rests '((a b c) (1 2 3) (:x :y :z)))) '((a 1 :x) ((b c) (2 3) (:y :z))))
   (equal (multiple-value-list (firsts-rests '((a b) (1 2 3) (:x :y :z)))) '((a 1 :x) ((b) (2 3) (:y :z))))
   (equal (multiple-value-list (firsts-rests '((a b) (c)))) '((A C) ()))
   (equal (multiple-value-list (firsts-rests '((a b) (1 2) (:x)))) '((a 1 :x) ())) ; Not (a 1 :x); ((b) (2) ())
   (equal (multiple-value-list (firsts-rests (nth-value 1 (firsts-rests '((a b c) (1 2) (:x :y :z)))) )) '((b 2 :y) ()))
   (equal (multiple-value-list (firsts-rests (nth-value 1 (firsts-rests '((a b) (1 2) (:x)))) )) '(() ()))) )

(deftest test-transition ()
  (check
   (equal (transition '(a b c)) '((NIL (A B C)) ((A) (B C)) ((A B) (C)) ((A B C) NIL)))) )

(deftest test-transition-1 ()
  (check
   (equal (transition-1 '(a b c)) '((NIL (A B C)) ((A) (B C)) ((A B) (C)))) ))

(deftest test-transition-n ()
  (check
   (equal (transition-n #1='(a b c) 0) (transition #1#))
   (equal (transition-n #2='(a b c) 1) (transition-1 #2#))))

(defclass person ()
  ((first-name :reader first-name :initarg :first)
   (last-name :reader last-name :initarg :last)))

(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t)
    (format t "~A, ~A" (last-name p) (first-name p))))

(defclass ramone (person) ())

(defmethod initialize-instance :after ((r ramone) &rest initargs)
  (declare (ignore initargs))
  (with-slots (last-name) r
    (setf last-name "Ramone")))

(deftest test-equalelts ()
  (check
   (equalelts '())
   (equalelts '(a))
   (equalelts '(a a))
   (equalelts '(a a a))
   (not (equalelts '(a b)))
   (not (equalelts '(a a b)))
   (not (equalelts '(a a a b)))
   (not (equalelts '(a b c d)))
   (not (equalelts '(a b b)))
   (not (equalelts '(a a b b)))
   (not (equalelts '(a c d b b)))
   (equalelts '((a b) (a b) (a b)))
   (equalelts '("foo" "foo" "foo" "foo"))
   (not (equalelts '("foo" "foo" "Foo" "foo")))
   (equalelts '("foo" "foo" "Foo" "foo") :test #'string-equal)
   (not (equalelts '(1.0 1 1d0)))
   (equalelts '(1.0 1 1d0) :test #'=)
   (not (equalelts '("pung" "PUNG" "pUnG" "PunG")))
   (not (equalelts '("pung" "PUNG" "pUnG" "PunG") :test #'string=))
   (equalelts '("pung" "PUNG" "pUnG" "PunG") :test #'string-equal)
   (equalelts #())
   (equalelts #(a a a))
   (equalelts "")
   (equalelts "aaaaaaaaa")
   (not (equalelts "aaaAaAAaa"))
   (equalelts "aaaAaAAaa" :test #'char-equal)
   (not (equalelts '((a . 1) (a . 2) (a . 1) (a . 4))))
   (equalelts '((a . 1) (a . 2) (a . 1) (a . 4)) :key #'first)
   (let ((andy (make-instance 'person :first "Andy" :last "Warhol"))
         (joey (make-instance 'ramone :first "Joey"))
         (dee-dee (make-instance 'ramone :first "Dee Dee"))
         (tommy (make-instance 'ramone :first "Tommy"))
         (johnny (make-instance 'ramone :first "Johnny")))
     (and (not (equalelts (list joey dee-dee tommy johnny)))
          (not (equalelts (list andy joey dee-dee tommy johnny) :key #'last-name))
          (equalelts (list joey dee-dee tommy johnny) :key #'last-name)))
   (not (equalelts '(1 4 7 10 13)))
   (equalelts '(1 4 7 10 13) :key (partial* #'mod 3))
   (equalelts #(1 4 7 10 13) :key (partial* #'mod 3))
   (equalelts "147" :key #'(lambda (ch) (mod (digit-char-p ch) 3)))) )
