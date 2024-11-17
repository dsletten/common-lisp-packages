;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               test-strings.lisp
;;;;
;;;;   Started:            Thu Nov  7 21:17:46 2024
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
(load "/home/slytobias/lisp/packages/strings.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :strings)

(use-package :test)

;;;
;;;    Fix!!
;;;    
(deftest test-expand ()
  (check
   (string= #1="asdf" (expand #1#))
   (string= "" (expand ""))
   (string= "abcdef" (expand "a-f"))
   (string= "23abcdef" (expand "23a-f"))
   (string= "abcdwxyz" (expand "a-dw-z"))
   (string= "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" (expand "A-Z0-9"))))

(deftest test-translate ()
  (check
   (string= "is this Not puNg?" (translate "Is this not pung?" "nI" "Ni"))
   (string= "W3 0ught@ t@k3 it 3@sy" (translate "We oughta take it easy" "aeo" "@30"))))

