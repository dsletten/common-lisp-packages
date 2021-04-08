;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               test-lang.lisp
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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :lang)

(use-package :test)

;;;
;;;    Fix the list implementation!!
;;;    
(deftest test-splice ()
  (check
   (string= (splice "Why all this pung?" 0 7 "Who ordered") "Who ordered this pung?")
   (string= (splice "Is this pung?" 8 0 "not ") "Is this not pung?")
   (string= (splice "Is this foo?" 8 3 "not pung") "Is this not pung?")
   (equal (splice '(a b d e) 2 0 '(c)) '(a b c d e))
   (equal (splice '(a b d e) 2 1 '(c)) '(a b c e))
   (equal (splice '(a b d e) 2 2 '(c)) '(a b c))))

;;;
;;;    Fix!!
;;;    
(deftest test-expand ()
  (check
   (string= (expand #1="asdf") #1#)
   (string= (expand "") "")
   (string= (expand "a-f") "abcdef")
   (string= (expand "23a-f") "23abcdef")
   (string= (expand "a-dw-z") "abcdwxyz")
   (string= (expand "A-Z0-9") "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))

(deftest test-translate ()
  (check
   (string= (translate "Is this not pung?" "nI" "Ni") "is this Not puNg?")
   (string= (translate "We oughta take it easy" "aeo" "@30") "W3 0ught@ t@k3 it 3@sy")))

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

;(defun prompt-read (prompt &rest keys &key (allow-empty t) (trim t))
;; (prompt-read "This is the real deal: " :allow-empty nil :test #'(lambda (s) (member s '("pung" "foo" "bar") :test #'string-equal)))
;; (prompt-read "Is this not pung? " :test #'(lambda (s) (every #'alpha-char-p s)) :allow-empty nil)
;; (prompt-read "Is this not pung? " :test #'(lambda (s) (every #'alpha-char-p s)) :trim nil)
;; (prompt-read "Enter a letter. " :allow-empty nil :test #'(lambda (s) (and (= (length s) 1) (alpha-char-p (char s 0)))) ) 
        
;(defun get-num (prompt &key test (precision 'double-float))

(deftest test-valid-num-p ()
  (check
   (valid-num-p 8)
   (valid-num-p 8d0)
   (valid-num-p 1/8)
   (valid-num-p 8 #'is-integer)
   (valid-num-p 8d0 #'is-integer)
   (valid-num-p (sqrt -1))
   (not (valid-num-p "8"))
   (not (valid-num-p 'eight))
   (not (valid-num-p 8d0 #'integerp))
   (not (valid-num-p 1/8 #'is-integer))
   (not (valid-num-p (sqrt -1) #'realp))))
   
(deftest test-is-integer ()
  (check
   (is-integer 2)
   (is-integer 2d0)
   (is-integer -2)
   (is-integer -2d0)
   (is-integer 4/2)
   (is-integer (sqrt 4))
   (is-integer (sqrt 4d0))
   (not (is-integer pi))
   (not (is-integer (sqrt -1)))
   (not (is-integer 1/3))))

;(defun list-to-string (l)

;(defun show-symbols (package-name)

(deftest test-transfer ()
  (check
   (multiple-value-bind (elt seq) (transfer '(a b c) 0) (and (eql elt 'a) (equal seq '(b c))))
   (multiple-value-bind (elt seq) (transfer '(a b c) 1) (and (eql elt 'b) (equal seq '(a c))))
   (multiple-value-bind (elt seq) (transfer '(a b c) 2) (and (eql elt 'c) (equal seq '(a b))))
   (multiple-value-bind (ch s) (transfer "pung" 0) (and (char= ch #\p) (string= s "ung")))
   (multiple-value-bind (ch s) (transfer "pung" 1) (and (char= ch #\u) (string= s "png")))
   (multiple-value-bind (ch s) (transfer "pung" 3) (and (char= ch #\g) (string= s "pun")))
   (multiple-value-bind (elt v) (transfer #(:a :b :c :d) 0) (and (eql elt :a) (equalp v #(:b :c :d))))
   (multiple-value-bind (elt v) (transfer #(:a :b :c :d) 1) (and (eql elt :b) (equalp v #(:a :c :d))))
   (multiple-value-bind (elt v) (transfer #(:a :b :c :d) 3) (and (eql elt :d) (equalp v #(:a :b :c)))) ))

(deftest test-drop ()
  (check
   (equal (drop 0 '(a b c d)) '(a b c d))
   (equal (drop 1 '(a b c d)) '(b c d))
   (equal (drop 2 '(a b c d)) '(c d))
   (equal (drop 4 '(a b c d)) '())
   (equal (drop 5 '(a b c d)) '())
   (equalp (drop 0 #(:a :b :c)) #(:a :b :c))
   (equalp (drop 1 #(:a :b :c)) #(:b :c))
   (equalp (drop 2 #(:a :b :c)) #(:c))
   (equalp (drop 3 #(:a :b :c)) #())
   (equalp (drop 4 #(:a :b :c)) #())
   (string= (drop 3 "Is this not pung?") "this not pung?")
   (string= (drop 12 "Is this not pung?") "pung?")
   (string= (drop 17 "Is this not pung?") "")))

(deftest test-take ()
  (check
   (equal (take 0 '(a b c d)) '())
   (equal (take 1 '(a b c d)) '(a))
   (equal (take 2 '(a b c d)) '(a b))
   (equal (take 4 '(a b c d)) '(a b c d))
   (equal (take 5 '(a b c d)) '(a b c d))
   (equalp (take 0 #(:a :b :c)) #())
   (equalp (take 1 #(:a :b :c)) #(:a))
   (equalp (take 2 #(:a :b :c)) #(:a :b))
   (equalp (take 3 #(:a :b :c)) #(:a :b :c))
   (equalp (take 4 #(:a :b :c)) #(:a :b :c))
   (string= (take 7 "Is this not pung?") "Is this")
   (string= (take 0 "Is this not pung?") "")))

(deftest test-take-and-drop ()
  (check
   (equal (take 2 (drop 1 '(a b c d))) '(b c))
   (equal (drop 1 (take 2 '(a b c d))) '(b))
   (equalp (take 1 (drop 2 #(:a :b :c))) #(:c))
   (string= (take 4 (drop 12 "Is this not pung?")) "pung")))

(deftest test-take-drop ()
  (check
   (multiple-value-bind (before after) (take-drop 0 '(a b c d)) (and (equal before '()) (equal after '(a b c d))))
   (multiple-value-bind (before after) (take-drop 1 '(a b c d)) (and (equal before '(a)) (equal after '(b c d))))
   (multiple-value-bind (before after) (take-drop 2 '(a b c d)) (and (equal before '(a b)) (equal after '(c d))))
   (multiple-value-bind (before after) (take-drop 5 '(a b c d)) (and (equal before '(a b c d)) (equal after '())))
   (multiple-value-bind (before after) (take-drop 0 #(1 2 3)) (and (equalp before #()) (equalp after #(1 2 3))))
   (multiple-value-bind (before after) (take-drop 1 #(1 2 3)) (and (equalp before #(1)) (equalp after #(2 3))))
   (multiple-value-bind (before after) (take-drop 3 #(1 2 3)) (and (equalp before #(1 2 3)) (equalp after #())))
   (multiple-value-bind (before after) (take-drop 8 #(1 2 3)) (and (equalp before #(1 2 3)) (equalp after #())))
   (multiple-value-bind (before after) (take-drop 7 "Is this not pung?") (and (string= before "Is this")) (string= after " not pung?"))))

(deftest test-prefixp ()
  (check
   (prefixp '() '(g t c a t))
   (prefixp '(g t c) '(g t c a t))
   (prefixp #1='(g t c a t) #1#)
   (not (prefixp '(g t c) '(a g g t c)))
   (prefixp "Is" "Is this not pung?")
   (prefixp "IS" "is this not pung?" :test #'char-equal)
   (prefixp #*101 #*101111)
   (prefixp #(:a :b :c :d) #(:a :b :c :d :e))))

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
   (null (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) '(a "pung" :j t)))
   (equal (multiple-value-list (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) #('a "pung" 5 t))) '(5 2.236068))
   (null (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) #('a "pung" :j t)))
   (equal (multiple-value-list (find-some-if #'(lambda (ch) (if (member ch '(#\a #\e #\i #\o #\u)) (char-upcase ch) nil)) "Is this not pung?")) '(#\i #\I))
   (equal (multiple-value-list (find-some-if #'(lambda (s) (if (= (length s) 4) (string-upcase s) nil)) #("Yoshimi" "Battles" "The" "Pink" "Robots"))) '("Pink" "PINK"))))
   

;; (flet ((f (x) (if (numberp x) (1+ x) nil))) (mapcar #'f (remove-if-not #'f '(a 1 2 b 3 c d 4)))) => (2 3 4 5)
;; (flet ((f (x) (if (numberp x) (1+ x) nil))) (map 'vector #'f (remove-if-not #'f '[a 1 2 b 3 c d 4]))) => #(2 3 4 5)
;; (flet ((f (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch)))) (map 'string #'f (remove-if-not #'f "Is this not pung?"))) => "STHISNOTPUNG"
;; (flet ((f (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch)))) (mapcar #'f (remove-if-not #'f (coerce "Is this not pung?" 'list)))) => (#\S #\T #\H #\I #\S #\N #\O #\T #\P #\U #\N #\G)

(deftest test-filter ()
  (check
   (equal (filter #'(lambda (x) (if (numberp x) (1+ x) nil)) '(a 1 2 b 3 c d 4)) '(2 3 4 5))
   (equalp (filter #'(lambda (x) (if (numberp x) (1+ x) nil)) ['a 1 2 'b 3 'c 'd 4]) [2 3 4 5])
   (string= (filter #'(lambda (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch))) "Is this not pung?") "STHISNOTPUNG")
   (equal (filter #'(lambda (ch) (and (alpha-char-p ch) (lower-case-p ch) (char-upcase ch))) (coerce "Is this not pung?" 'list)) '(#\S #\T #\H #\I #\S #\N #\O #\T #\P #\U #\N #\G))))

(deftest test-longerp ()
  (check
   (longerp '(a b c) '(d e))
   (not (longerp '(a b c) '(d e f)))
   (not (longerp '(a b c) '(d e f g)))
   (longerp "abc" "de")
   (not (longerp "abc" "def"))
   (not (longerp "abc" "defg"))
   (longerp [1 2 3] [4 5])
   (not (longerp [1 2 3] [4 5 6]))
   (not (longerp [1 2 3] [4 5 6 7]))))

(deftest test-group ()
  (check
   (equal (group (loop for i from 1 to 10 collect i) 3) '((1 2 3) (4 5 6) (7 8 9) (10)))
   (equal (group (loop for i from 1 to 10 collect i) 7) '((1 2 3 4 5 6 7) (8 9 10)))
   (equalp (group (coerce (loop for i from 1 to 10 collect i) 'vector) 3) '(#(1 2 3) #(4 5 6) #(7 8 9) #(10)))
   (equalp (group (coerce (loop for i from 1 to 10 collect i) 'vector) 7) '(#(1 2 3 4 5 6 7) #(8 9 10)))
   (equal (group "Is this not pung?" 2) '("Is" " t" "hi" "s " "no" "t " "pu" "ng" "?"))
   (equal (group "Is this not pung?" 11) '("Is this not" " pung?"))
   (equal (group "Is this not pung?" 7) '("Is this" " not pu" "ng?"))))

(defun count-vowels (s)
  (count-if #'(lambda (ch) (member ch (coerce "aeiou" 'list))) s :key #'char-downcase))
(defun count-consonants (s)
  (count-if #'(lambda (ch) (member ch (coerce "bcdfghjklmnpqrstvwxyz" 'list))) s :key #'char-downcase))

(deftest test-group-until ()
  (check
   (equal (group-until #'(lambda (l) (> (reduce #'+ l) 20)) '(4 19 4 9 5 12 5 3 4 1 1 9 5 18)) '((4) (19) (4 9 5) (12 5 3) (4 1 1 9 5) (18)))
   (equal (group-until #'(lambda (l) (> (reduce #'+ l) 20)) '(4 19 4 9 5 12 5 3 4 1 1 9 5 18 1)) '((4) (19) (4 9 5) (12 5 3) (4 1 1 9 5) (18 1)))
   (equal (group-until #'(lambda (chs) (or (> (count-vowels chs) 2) (> (count-consonants chs) 4))) "The quick brown fox jumps over the LAZY dog.")
          '((#\T #\h #\e #\  #\q #\u) (#\i #\c #\k #\  #\b #\r #\o) (#\w #\n #\  #\f #\o #\x #\ ) (#\j #\u #\m #\p #\s #\  #\o) (#\v #\e #\r #\  #\t #\h #\e #\ ) (#\L #\A #\Z #\Y #\  #\d #\o) (#\g #\.)))
   (equal (group-until #'(lambda (coins) (> (count :head coins) 3)) '(:HEAD :HEAD :HEAD :TAIL :HEAD :TAIL :HEAD :TAIL :TAIL :HEAD :TAIL :TAIL :TAIL :HEAD :TAIL :HEAD :TAIL :HEAD :TAIL :TAIL))
          '((:HEAD :HEAD :HEAD :TAIL) (:HEAD :TAIL :HEAD :TAIL :TAIL :HEAD :TAIL :TAIL :TAIL) (:HEAD :TAIL :HEAD :TAIL :HEAD :TAIL :TAIL)))))

(deftest test-prune-if ()
  (check
   (equal (prune-if #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))) '(1 (3 (5)) 7 (9)))
   (equal (prune-if #'(lambda (elt) (< elt 4)) '(1 2 (3 (4 5) 6) 7 8 (9))) '(((4 5) 6) 7 8 (9)))) )

(deftest test-prune-if-not ()
  (check
   (equal (prune-if-not #'evenp '(1 2 (3 (4 5) 6) 7 8 (9))) '(2 ((4) 6) 8 ()))
   (equal (prune-if-not #'(lambda (elt) (< elt 4)) '(1 2 (3 (4 5) 6) 7 8 (9))) '(1 2 (3 ()) ()))) )

(deftest test-same-shape-tree-p ()
  (check
   (same-shape-tree-p '(((a) a (a) (a) ((a (a (a (a a) a)) a) a) a) a a) '(((:a) :a (:a) (:a) ((:a (:a (:a (:a :a) :a)) :a) :a) :a) :a :a))
   (not (same-shape-tree-p '(((a) a (a) (a) (((a (a (a a) a)) a) a) a) a a) '(((:a) :a (:a) (:a) ((:a (:a (:a (:a :a) :a)) :a) :a) :a) :a :a)))) )

(deftest test-before ()
  (check
   (equal (before 'a 'b '(a b c d)) '(B C D))
   (not (before 'a 'b '()))
   (not (before 'a 'b '(b a c d)))
   (equal (before 'a 'b '(a c d)) '(C D))
   (= (before #\p #\u "Is this not pung?") 13)
   (not (before #\p #\u "Is this not Pung?"))
   (= (before #\p #\u "Is this not Pung?" :test #'char-equal) 13)
   (not (before #\u #\p "Is this not pung?"))
   (not (before '(a b) '(c d) (vector '(:a) "foo" '(a b) '(c d))))
   (= (before '(a b) '(c d) (vector '(:a) "foo" '(a b) '(c d)) :test #'equal) 3)))

(deftest test-after ()
  (check
   (equal (after 'b 'a '(a b c d)) '(B C D))
   (equal (after 'd 'a '(a b c d)) '(D))
   (not (after 'a 'b '(a b c d)))
   (not (after 'a 'a '(a b c a d)))
   (= (after 'b 'a '#(a b c d)) 1)
   (= (after 'd 'a '#(a b c d)) 3)
   (not (after 'e 'a '#(a b c d)))
   (not (after 'a 'b '#(a b c d)))
   (= (after #\u #\p "Is this not pung?") 13)
   (= (after #\i #\s "Is this not pung?") 5)
   (not (after #\i #\s "Is this not pung?" :test #'char-equal))
   (not (after 2 0 #(4 6 8 0 2.0 3 5 7 9)))
   (= (after 2 0 #(4 6 8 0 2.0 3 5 7 9) :test #'=) 4)))
        
(deftest test-duplicate ()
  (check
   (not (duplicate 'a '(a b c d)))
   (equal (duplicate 'a '(a b c d a e f g)) '(A E F G))
   (not (duplicate #\a "abcd"))
   (= (duplicate #\a "abcdaefg") 4)
   (not (duplicate #\a "abcdAEFG"))
   (= (duplicate #\a "abcdAEFG" :test #'char-equal) 4)
   (not (duplicate 2 #(2 4 6 8)))
   (= (duplicate 2 #(2 4 6 8 0 2 3 5 7 9)) 5)
   (not (duplicate 2 #(2 4 6 8 0 2.0 3 5 7 9)))
   (= (duplicate 2 #(2 4 6 8 0 2.0 3 5 7 9) :test #'=) 5)))

(deftest test-split-if ()
  (check
   (equal (multiple-value-list (split-if #'(lambda (x) (> x 4)) (loop for i from 1 to 10 collect i))) '((1 2 3 4) (5 6 7 8 9 10)))
   (equal (multiple-value-list (split-if #'(lambda (x) (> x 14)) (loop for i from 1 to 10 collect i))) '((1 2 3 4 5 6 7 8 9 10) ()))
   (equal (multiple-value-list (split-if #'(lambda (x) (> x 0)) (loop for i from 1 to 10 collect i))) '(() (1 2 3 4 5 6 7 8 9 10)))
   (equalp (multiple-value-list (split-if #'(lambda (x) (> x 4)) (apply #'vector #[1 10]))) '(#(1 2 3 4) #(5 6 7 8 9 10)))
   (equal (multiple-value-list (split-if #'oddp '(0 2 4 5 7 2 9))) '((0 2 4) (5 7 2 9)))
   (equalp (multiple-value-list (split-if #'oddp [0 2 4 5 7 2 9])) '(#(0 2 4) #(5 7 2 9)))))

(deftest test-most ()
  (check
   (equal (multiple-value-list (most #'length '((a b) (a b c) (a) (e f g)))) '((A B C) 3))
   (null (most #'length '()))
   (equal (multiple-value-list (most #'length '((a b)))) '((A B) 2))
   (equal (multiple-value-list (most #'length #("ab" "abc" "a" "efg"))) '("abc" 3))
   (equal (multiple-value-list (most #'char-code "Is this not pung?")) '(#\u 117))
   (equal (multiple-value-list (most #'abs #(-9 8 -7 3 25 0 -28))) '(-28 28))
   (equal (multiple-value-list (most #'abs #(-9 8 -7 3 25 28 0 -28))) '(28 28))
   (equal (multiple-value-list (most #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))) '(4 3))
   (equal (multiple-value-list (most #'integer-length (reverse #(0 1 3 4 7 -1 -4 -7 -8)))) '(-8 3))))

(deftest test-high-low ()
  (check
   (null (high-low #'length '()))
   (equal (multiple-value-list (high-low #'length '((a b) (a b c) (a) (e f g)))) '((A B C) 3 (A) 1))
   (equal (multiple-value-list (high-low #'length '((a b)))) '((A B) 2 (A B) 2))
   (equal (multiple-value-list (high-low #'length '((a b) (c d) (e f)))) '((A B) 2 (A B) 2))
   (equal (multiple-value-list (high-low #'length #("ab" "abc" "a" "efg"))) '("abc" 3 "a" 1))
   (equal (multiple-value-list (high-low #'char-code "Is this not pung?")) '(#\u 117 #\SPACE 32))
   (equal (multiple-value-list (high-low #'char-code "a")) '(#\a 97 #\a 97))
   (equal (multiple-value-list (high-low #'char-code "aaaaaaaa")) '(#\a 97 #\a 97))
   (null (high-low #'abs #()))
   (equal (multiple-value-list (high-low #'abs #(-9 8 -7 3 25 0 -28))) '(-28 28 0 0))
   (equal (multiple-value-list (high-low #'abs #(-9 8 -7 3 25 28 0 -28))) '(28 28 0 0))
   (equal (multiple-value-list (high-low #'abs #(8))) '(8 8 8 8))
   (equal (multiple-value-list (high-low #'abs #(8 -8 8 -8))) '(8 8 8 8))
   (equal (multiple-value-list (high-low #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))) '(4 3 0 0))
   (equal (multiple-value-list (high-low #'integer-length (reverse #(0 1 3 4 7 -1 -4 -7 -8)))) '(-8 3 -1 0))))

(deftest test-best ()
  (check
   (= (best #'> '(1 2 3 4 5)) 5)
   (= (best #'< '(1 2 3 4 5)) 1)
   (= (best #'> (shuffle (vector 1 2 3 4 5))) 5)
   (= (best #'< (shuffle (vector 1 2 3 4 5))) 1)
   (char= (best #'char> "Is this not pung?") #\u)
   (char= (best #'char< "Is this not pung?") #\Space)))

(deftest test-best-worst ()
  (check
   (equal (multiple-value-list (best-worst #'> '(1 2 3 4 5))) '(5 1))
   (equal (multiple-value-list (best-worst #'< '(1 2 3 4 5))) '(1 5))
   (equal (multiple-value-list (best-worst #'> (shuffle (vector 1 2 3 4 5)))) '(5 1))
   (equal (multiple-value-list (best-worst #'< (shuffle (vector 1 2 3 4 5)))) '(1 5))
   (equal (multiple-value-list (best-worst #'char< "Is this not pung?")) '(#\Space #\u))))

(deftest test-mostn ()
  (check
   (equal (multiple-value-list (mostn #'length '((a b) (a c) (a) (e  g)))) '(((A B) (A C) (E G)) 2))
   (equal (multiple-value-list (mostn #'length '((a b) (a b c) (a) (e f g)))) '(((A B C) (E F G)) 3))
   (equal (multiple-value-list (mostn #'length #("ab" "abc" "a" "efg"))) '(("abc" "efg") 3))
   (equal (multiple-value-list (mostn #'char-code "Is this not pung?")) '((#\u) 117))
   (equal (multiple-value-list (mostn #'abs #(-9 8 -7 3 25 0 -28))) '((-28) 28))
   (equal (multiple-value-list (mostn #'abs #(-9 8 -7 3 25 28 0 -28))) '((28 -28) 28))))

(deftest test-high-low-n ()
  (check
   (equal (multiple-value-list (high-low-n #'length '((a b) (a  c) (a) (e  g)))) '(((A B) (A C) (E G)) 2 ((A)) 1))
   (equal (multiple-value-list (high-low-n #'length '((a b) (a  c) (a) (e  g) (x x x)))) '(((X X X)) 3 ((A)) 1))
   (equal (multiple-value-list (high-low-n #'abs #(-9 8 -7 3 25 0 -28))) '((-28) 28 (0) 0))
   (equal (multiple-value-list (high-low-n #'abs #(-9 8 -7 3 25 28 0 -28))) '((28 -28) 28 (0) 0))
   (equal (multiple-value-list (high-low-n #'integer-length #(0 1 3 4 7 -1 -4 -7 -8))) '((4 7 -7 -8) 3 (-1) 0))
   (equal (multiple-value-list (high-low-n #'char-code "Is this not pung?")) '((#\u) 117 (#\Space  #\Space  #\Space ) 32))
   (equal (multiple-value-list (high-low-n #'length #((a b) (a  c) (a) (e  g) (x x x)))) '(((X X X)) 3 ((A)) 1))))

(deftest test-map-> ()
  (check
   (equal (map-> #'1+ -2 #'(lambda (x) (> x 0)) #'(lambda (x) (+ x 0.5))) '(-1 -0.5 0.0 0.5 1.0))
   (equal (map-> #'(lambda (i) (list (code-char i) i)) (char-code #\p) #'(lambda (i) (> i (char-code #\z))) #'(lambda (x) (+ x 2))) '((#\p 112) (#\r 114) (#\t 116) (#\v 118) (#\x 120) (#\z 122)))
   (equal (map-> #'(lambda (l) (string-upcase (first l))) #1='("Is" "this" "not" "pung?") #'null #'cdr) (mapcar #'string-upcase #1#))
   (equal (map-> #'(lambda (x) (log x 2)) 1 #'(lambda (x) (> x 1024)) #'(lambda (x) (* 2 x))) '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0))))

(deftest test-mapcars ()
  (check
   (equal (mapcars #'abs '(2 5 6) '(99 -23 -8)) '(2 5 6 99 23 8))
   (equal (mapcars #'sqrt (make-range 2 7) (make-range 9 4)) '(1.4142135 1.7320508 2.0 2.236068 2.4494898 2.6457512 3.0 2.828427 2.6457512 2.4494898 2.236068 2.0))))

(deftest test-iterate ()
  (check
   (let ((iterator (iterate #'1+ 0)))
     (equal (loop repeat 5 collect (funcall iterator)) (loop for i from 0 below 5 collect i)))) )

(deftest test-rmapcar ()
  (check
   (equal (rmapcar #'1+ '(1 2 (3 4 (5) 6) 7 (8 9))) '(2 3 (4 5 (6) 7) 8 (9 10)))
   (equal (rmapcar #'(lambda (s1 s2) (concatenate 'string s1 s2)) '("Is" ("this" ("not" ("pung?")))) '("Ça" ("plane" ("pour" ("moi" "Plastic" "Bertrand")))) ) '("IsÇa" ("thisplane" ("notpour" ("pung?moi")))) )))

;; (rmapcar #'length "is")
;; 2
;; * (rmapcar #'< 1 2)
;; T

;; * (rmapcar #'+ '(1 2) '(3 4))
;; (4 6)

;; * (rmapcar #'+ '((1 2) (3 4)))
;; ((1 2) (3 4))
;; * (rmapcar #'1+ '((1 2) (3 4)))
;; ((2 3) (4 5))

;; (rmapcar #'cons '(a b c) '((d e) (f g) (h i)))

;; ((A D E) (B F G) (C H I))

;; (rmapcar #'cons '((a) (b) (c)) '((d e) (f g) (h i)))

;; (((A . D)) ((B . F)) ((C . H)))

;; * (rmapcar #'cons '(a (b c)) '((d e) ((f g) (h i))))

;; ((A D E) ((B F G) (C H I)))
;; * (rmapcar #'cons '(a b) '((d e) (f g) (h i)))

;; ((A D E) (B F G))
;; * (rmapcar #'cons '(a b c) '((d e) (f g)))

;; ((A D E) (B F G))
;; * (rmapcar #'cons '(a b c) '((1) (2 3) (4 5 6)))

;; ((A 1) (B 2 3) (C 4 5 6))
;; * (mapcar #'cons '(a b c) '((1) (2 3) (4 5 6)))

;; ((A 1) (B 2 3) (C 4 5 6))
;; * (mapcar #'cons '(a (b c)) '((d e) ((f g) (h i))))

;; ((A D E) ((B C) (F G) (H I)))
;; * 


;; Graham does not handle arbitrary trees

;; (rmapcar #'1+ '((1 . 2) (3 . 4) (5 . 6)))

;; *** - MAPCAR: A proper list must not end with 2
;; The following restarts are available:
;; ABORT          :R1      Abort main loop
;; Break 1 LANG[135]> :a
;; LANG[136]> (tree-map-c #'1+ '((1 . 2) (3 . 4) (5 . 6)))
;; ((2 . 3) (4 . 5) (6 7))

;; (tree-map #'1+ '((1 . 2) (3 . 4) (5 . 6)))
;; ((2 . 3) (4 . 5) (6 . 7))

;; LANG[148]> (tree-map-c #'+ '((1 . 2) (3 . 4) (5 . 6)) '((9 . 9) (9 . 9) (9 . 9)))
;; ((10 . 11) (12 . 13) (14 . 15))

;; LANG[149]> (tree-map-c #'+ '((1 . 2) (3 . 4) (5 . 6)) '((9 . 9) (9 . 9)))
;; ((10 . 11) (12 . 13))

;; LANG[150]> (tree-map-c #'+ '((1 2) (3 4) (5 6)) '((9 9) (9 9) (9 9)))
;; ((10 11) (12 13) (14 15))

;; LANG[153]> (rmapcar #'+ '((1 2) (3 4) (5 6)) '((9 9) (9 9) (9 9)))
;; ((10 11) (12 13) (14 15))

;; LANG[154]> (tree-map-c #'+ '((1 . 2) (3 . 4) (5 . 6) . 7) '((9 . 9) (9 . 9) (9 . 9) . 9))
;; ((10 . 11) (12 . 13) (14 . 15) . 16)

;; LANG[155]> (tree-map-c #'+ '((1 2) (3 4) (5)) '((9 9) (9) (9 9)))
;; ((10 11) (12) (14))

;; LANG[156]> (tree-map-c #'+ '((1 2) (3 4) (5) (6 7)) '((9 9) (9) (9 9) (9 9)) '((0.5) (0.5 0.5) (0.5 0.5) (0.5 0.5)))
;; ((10.5) (12.5) (14.5) (15.5 16.5))

;; LANG[185]> (tree-map-c #'+ '((1 2) (3 4) (5) (6 7)) '((9 9) (9) (9 9) (9 9)) '((0.5) (0.5 0.5) (0.5 0.5) (0.5 0.5) (0.5 0.5)))
;; ((10.5) (12.5) (14.5) (15.5 16.5))

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
