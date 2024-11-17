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

(deftest test-mod-rem ()
  (loop for a in '(10 -10)
        do (loop for b in '(3 -3)
                 do (check
                     (= (nth-value 1 (floor a b)) (mod a b))
                     (= (nth-value 1 (truncate a b)) (rem a b)))) ))

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
