;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               tokenizer.lisp
;;;;
;;;;   Started:            Mon Dec 26 15:36:19 2005
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

(defpackage tokenizer
  (:use common-lisp)
  (:export "TOKENIZE" "GET-TOKENS" "NEXT-TOKEN"))

(in-package tokenizer)

(defconstant no-match 'no-match)
;(defconstant no-match (list 'no-match))
(let (index length input-string state start lexeme-start
      (eoe (code-char 0)))
;      (eoe (code-char 255))
;      (eoe (list 'eoe))
;      (no-match (list 'no-match)))
  (defun tokenize (s)
    (setf index 0
          length (length s)
          input-string s
          state 0
          start 0
          lexeme-start 0))

  (defun get-tokens ()
    (loop for token = (next-token)
          until (eq token eoe)
          collect token))
  
  (defun fail ()
    (ecase start
      (0 (setf start 1))
      (1 (setf start 9))
      (9 (setf start 12))
      (12 (setf start 20))
      (20 (setf start 25))
      (25 (setf start no-match)))
    (setf index lexeme-start)
    start)

  (defun next-token ()
    (setf lexeme-start index
          start 0
          state 0)
    (let ((ch eoe))
      (loop
;       (ccase state
       (case state
         (0 (setf ch (get-next-char))
            (if (space-char-p ch)
                (incf lexeme-start)
                (setf state (fail))))
         (1 (setf ch (get-next-char))
            (if (delimiterp ch)
                (return (intern (subseq input-string lexeme-start index)))
                (setf state (fail))))
         (9 (setf ch (get-next-char))
            (if (alpha-char-p ch)
                (setf state 10)
                (setf state (fail))))
         (10 (setf ch (get-next-char))
             (if (alphanumericp ch)
                 (setf state 10)
                 (setf state 11)))
;;       (11 (unless (eq ch eoe)
;;             (retract 1))
;;           (return (subseq input-string lexeme-start index)))
         (12 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 13)
                 (setf state (fail))))
         (13 (setf ch (get-next-char))
             (cond ((digit-char-p ch) (setf state 13))
                   ((char= ch #\.) (setf state 14))
                   ((char-equal ch #\e) (setf state 16))
                   (t (setf state (fail)))) )
         (14 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 15)
                 (setf state (fail))))
         (15 (setf ch (get-next-char))
             (cond ((digit-char-p ch) (setf state 15))
                   ((char-equal ch #\e) (setf state 16))
                   (t (setf state (fail)))) )
         (16 (setf ch (get-next-char))
             (cond ((digit-char-p ch) (setf state 18))
                   ((or (char= ch #\+) (char= ch #\-)) (setf state 17))
                   (t (setf state (fail)))) )
         (17 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 18)
                 (setf state (fail))))
         (18 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 18)
                 (setf state 19)))
         (20 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 21)
                 (setf state (fail))))
         (21 (setf ch (get-next-char))
             (cond ((digit-char-p ch) (setf state 21))
                   ((char= ch #\.) (setf state 22))
                   (t (setf state (fail)))) )
         (22 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 23)
                 (setf state (fail))))
         (23 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 23)
                 (setf state 24)))
         (25 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 26)
                 (setf state (fail))))
         (26 (setf ch (get-next-char))
             (if (digit-char-p ch)
                 (setf state 26)
                 (setf state 27)))
         ((11 19 24 27) (unless (eq ch eoe)
                       (retract 1))
          (return (read-from-string (subseq input-string lexeme-start index))))
         (otherwise (return eoe))))))
;        (#.no-match eoe)))) )

  (defun get-next-char ()
    (if (< index length)
        (prog1 (char input-string index) (incf index))
        eoe))

  (defun retract (i)
    (when (<= index length)
      (decf index i))))

(defun delimiterp (ch)
  (find ch "+-*/%^=()"))

(defun space-char-p (ch)
  (find ch '(#\Space #\Page #\Newline #\Return #\Tab #\Vt)))
