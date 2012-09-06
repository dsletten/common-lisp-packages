;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               parser.lisp
;;;;
;;;;   Started:            Thu Dec 22 23:02:55 2005
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
;;;;   Notes: Recursive descent parser
;;;;
;;;;   What about assignment (=) ?
;;;;
;;;;

(defpackage parser (:use common-lisp tokenizer))

(in-package parser)

(let (token-list token)
  (defun infix->prefix (s)
    (tokenize s)
    (setf token-list (get-tokens))
    (setf token (pop token-list))
    (let ((result (eval-2)))
      result))

  (defun eval-2 ()
    (let ((result (eval-3)))
      (loop (unless (or (equal token '+)
			(equal token '-))
	      (return result))
	    (let ((operator token))
	      (setf token (pop token-list))
	      (let ((partial-result (eval-3)))
		(setf result (list operator result partial-result)))) )))

  (defun eval-3 ()
    (let ((result (eval-4)))
      (loop (unless (or (equal token '*)
			(equal token '/)
			(equal token '%))
	      (return result))
	    (let ((operator token))
	      (setf token (pop token-list))
	      (let ((partial-result (eval-4)))
		(setf result (list operator result partial-result)))) )))

  (defun eval-4 ()
    (let ((result (eval-5)))
      (cond ((equal token '^)
	     (let ((operator token)) ; Don't need to keep
	       (setf token (pop token-list))
	       (let ((partial-result (eval-4)))
		 (setf result (list 'expt result partial-result)))) )
	    (t result))))

  (defun eval-5 ()
    "Handle unary +/-"
    (case token
      (+ (setf token (pop token-list))
	 (eval-6))
      (- (setf token (pop token-list))
	 (list '- (eval-6)))
      (otherwise (eval-6))))

  (defun eval-6 ()
    (cond ((equal token '|(|)
	   (setf token (pop token-list))
	   (let ((result (eval-2)))
	     (setf token (pop token-list))
	     result))
	  (t (eval-atom))))
  
  ;;;
  ;;;    Atomic
  ;;;
  ;;;    Must handle number/symbol (Look up value of symbol)
  ;;;    
  (defun eval-atom ()
;    (print token)
    (let ((current-token token))
      (setf token (pop token-list))
      current-token)))
