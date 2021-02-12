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
   (starts-with "Is this not pung?" "Is")
   (starts-with "Is this not pung?" "is" :test #'char-equal)
   (starts-with (subseq "Is this not pung?" 12) "pung")
   (not (starts-with "Is this not pung?" "is"))
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




(deftest test-approximately= ()
  (check
   (approximately= 0.001d0 0.0010000002d0)
   (not (approximately= 0.001d0 0.001000002d0))
   (approximately= 0.001d0 0.001000002d0 1d-4)))

(deftest test-find-some-if ()
  (check
   (equal (multiple-value-list (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) '(a "pung" 5 t))) '(5 2.236068))
   (null (find-some-if #'(lambda (elt) (if (numberp elt) (sqrt elt) nil)) '(a "pung" :j t)))) )

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
