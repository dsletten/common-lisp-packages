;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-time.lisp
;;;;
;;;;   Started:            Fri Apr 17 02:15:59 2020
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
(load "/home/slytobias/lisp/packages/time.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :time)

(use-package :test)

(deftest test-julian ()
  (check
   (= (julian 1 1 2020) 2458850)
   (= (julian 2 1 2020) 2458851)
   (= (+ 1 (julian 1 1 2020)) (julian (+ 1 1) 1 2020))
   (= (+ 10 (julian 1 1 2020)) (julian (+ 1 10) 1 2020))
   (= (julian 16 4 2020) 2458956)))

(deftest test-day-of-year ()
  (check
   (= (day-of-year 1 1 2020) 1)
   (= (day-of-year 31 12 2020) 366)
   (= (day-of-year 31 12 2019) 365)))
