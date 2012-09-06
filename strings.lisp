;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               strings.lisp
;;;;
;;;;   Started:            Sat Jun 30 20:19:23 2012
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
(load "/Users/dsletten/lisp/packages/lang.lisp")
;(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :strings
  (:use :common-lisp :lang)
;  (:use :common-lisp :lang :test)
  (:export :center :commify-list
           :irregular-plural :join
           :ljust
           :rjust
           :short-ordinal :space-char-p :split
           :squeeze :string-split :string-subst))

(in-package :strings)

;---------String Functions----------------
;;;
;;;    Split a given string into a list of strings.
;;;    The string is split on each occurrence of a given char
;;;    or on #\Space as a default.
;;;
;;;    The function recursively splits on successive occurrences of the
;;;    delimiter, building a list in reverse order.
;;;    The list is reversed before being returned to calling function.
;;;    
;; (defun split (s &optional (delimiter #\Space))
;;   "Split a given string into a list of strings. The string is split on each occurrence of a given char or on #\\Space as a default."
;;   (labels ((split-aux (s result)
;; 	     (let ((point (position delimiter s)))
;; 	       (if point
;; 		   (split-aux (subseq s (+ point 1))
;; 			      (cons (subseq s 0 point) result))
;; 		   (nreverse (cons s result)))) ))
;;     (split-aux s '())))

;;;
;;;    This version from the Cookbook (below) is better!
;;;    
(defun split (s &optional (delimiter #\Space))
  (loop for i = 0 then (1+ j)
        for j = (position delimiter s :start i)
        collect (subseq s i j)
        while j))

;; (defun string-split (s &optional (delimiter " "))
;;   "Split a given string into a list of strings. The string is split on each occurrence of a given string or on \" \" as a default."
;;   (let ((d-length (length delimiter)))
;;     (labels ((string-split-aux (s result)
;; 	       (let ((point (search delimiter s)))
;; 		 (if point
;; 		     (string-split-aux (subseq s (+ point d-length))
;; 				       (cons (subseq s 0 point) result))
;; 		     (nreverse (cons s result)))) ))
;;       (if (equal delimiter "")
;; 	  (map 'list #'string s)
;; 	  (string-split-aux s '()))) ))

(defun string-split (s &optional (delimiter " "))
  (let ((delimiter-length (length delimiter)))
    (loop for i = 0 then (+ j delimiter-length)
          for j = (search delimiter s :start2 i)
          collect (subseq s i j)
          while j)))

;;;
;;;    John Landahl
;;;    
;; (defun string-split (split-string string)
;;    "Returns a list containing items in 'string' split from
;;  occurrences of 'split-string'."
;;    (loop with l = (length split-string)
;;          for n = 0 then (+ pos l)
;;          for pos = (search split-string string :start2 n)
;;          if pos collect (subseq string n pos)
;;          else collect (subseq string n)
;;          while pos))

;;;
;;;    CL Cookbook
;;;    
;; (defun split-by-one-space (string)
 
;;  "Returns a list of substrings of string 
;;  divided by ONE space each.
;;  Note: Two consecutive spaces will be seen as if there were an empty string
;;  between them."
 
;;      (loop for i = 0 then (1+ j)
;;            as j = (position #\Space string :start i)
;;            collect (subseq string i j)
;;            while j))

;;;
;;;    Join a list of elements together as a string using a given string as
;;;    glue.
;;;
;;;    (join <strings-list> [<glue-string>])
;;;
;; (defun join (strings &optional (glue ""))
;;   "Join a list of elements together as a string using a given string as glue."
;;   (labels ((join-aux (strings result)
;; 	     (cond ((null strings) result)
;; 		   ((stringp (car strings))
;; 		    (join-aux (cdr strings)
;; 			      (concatenate 'string result glue
;; 					   (car strings))))
;; 		   ((characterp (car strings))
;; 		    (join-aux (cdr strings)
;; 			      (concatenate 'string result glue
;; 					   (string (car strings)))) )
;; 		   (t
;; 		    (join-aux (cdr strings)
;; 			      (concatenate 'string result glue
;; 					   (format nil "~S"
;; 						   (car strings)))) ))))
;;     (cond ((null strings) "")
;; 	  ((stringp (car strings))
;; 	   (join-aux (cdr strings) (car strings)))
;; 	  ((characterp (car strings))
;; 	   (join-aux (cdr strings) (string (car strings))))
;; 	  (t
;; 	   (join-aux (cdr strings) (format nil "~S" (car strings)))) )))

;;;
;;;    Adam Warner (original DELIMITER was CHAR)
;;;    
;; (defun join (list &optional (delimiter ":"))
;;   (let ((countdown (length list)))
;;     (with-output-to-string (stream)
;;       (dolist (item list)
;;         (write-string item stream)
;;         (decf countdown)
;;         (unless (zerop countdown)
;;           (write-string delimiter stream)))) ))

;;;
;;;    Matthew Danish (originally with PRINC)
;;;
;;;
;;;>>However the main reason my version was faster is that I used WRITE-STRING
;;;>>instead of PRINC. Replace Matthew's PRINCs with WRITE-STRINGs and his
;;;>>version is almost exactly the same speed with three elements (0.25s). With
;;;>> three times the number of elements it takes 0.63s.
;;; Binding *PRINT-PRETTY* to NIL has the same effect.  It also makes the
;;; FORMAT only solution run over twice as fast and cons 1/5 as much.
;; (defun join (list)
;;   (format nil "~{~A~^:~}" list))

;; (defun join (list &optional (delim ":"))
;;   (with-output-to-string (stream)
;;     (loop for cons on list do
;; 	    (write-string (car cons) stream)
;; 	      (when (cdr cons) (write-string delim stream)))))

;;;
;;;    John Thingstad (with minor modifications)
;;;    (Doesn't handle empty LIST arg)
;;;
;; (defun join (list delimiter)
;;   (reduce #'(lambda (a b) (concatenate 'string a delimiter b)) list)

;;
;;    100709
;;    
;; (defun join (strings &optional (glue ""))
;;   (with-output-to-string (s)
;;     (write-string (first strings) s)
;;     (dolist (string (rest strings))
;;       (write-string glue s)
;;       (write-string string s))))

;; (defun join (strings &optional (glue ""))
;;   (format nil (format nil "~~{~~A~~^~A~~}" glue) strings))
;;   (format nil "~{~}" (format nil "~~A~~^~A" glue) strings))
;;   (format nil "~{~}" (concatenate 'string "~A~^" glue) strings))

(defun join (strings &optional (glue ""))
  (format nil "~{~}" (format nil "~~A~~^~A" glue) strings))

;;;
;;;    See CLHS 22.3.7.2
;;;    
(defun english-list (&rest items)
  (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^, ~}~]" items))

;;;
;;;    Adapted from Perl Cookbook
;;;
;; (defun commify-list (&rest items)
;;   (let ((separator (if (some #'(lambda (s) (find #\, s)) items) #\; #\,)))
;;     (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~}~]" (format nil "~~#[~~;and ~~]~~A~~^~C " separator) items)))
(defun has-comma-p (s)
  (find #\, s))

(defun commify-list (&rest items)
  (if (some #'has-comma-p items)
      (commify-with #\; items)
      (commify-with #\, items)))

(defun commify-with (ch items)
  (apply #'format nil (make-comma-control-string ch) items))

(defun make-comma-control-string (comma-char)
  (concatenate 'string "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^" (string comma-char) "~}~]"))

;; (defun commify-list (&rest items)
;;   (if (some #'(lambda (s) (find #\, s)) items)
;;       (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^; ~}~]" items)
;;       (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^, ~}~]" items)))

;;;
;;;    Fix this...
;;;    
;;;    Replace part(s) of a string with another.
;;;
;;;    Default requires case-sensitive match.
;;;    Use :test #'char-equal for case-insensitive match.
;;;
(defun string-subst (new old string &rest keys &key global (test #'char=))
  (let ((i (search old string :test test)))
    (cond ((null i) string)
          (global (apply #'string-subst new old (splice string i (length old) new) keys))
          (t (splice string i (length old) new)))) )

;[55]> (string-subst "pung" "foo" "foo bar foo baz")
;"pung bar foo baz"
;[56]> (string-subst "pung" "foo" "foo bar foo baz" :global t)
;"pung bar pung baz"
; [13]> (string-subst "pung" "foo" "pung in the FOO bar...foo!" :test #'char-equal)
; "pung in the pung bar...foo!"
; [14]> (string-subst "pung" "foo" "pung in the FOO bar...foo!" :test #'char-equal :global t)
; "pung in the pung bar...pung!"

;;;
;;;    A roundabout way of uppercasing a string:
;;;
; [18]> (setf s "Is this not pung?")
; "Is this not pung?"
; [19]> (setf l (split s))
; ("Is" "this" "not" "pung?")
; [20]> (dolist (w l) (setf s (string-subst (string-upcase w) w s)))
;;;   Or to capitalize each word:
;[26]> (dolist (w l) (setf s (string-subst (string-capitalize w) w s)))

;;;
;;;    Provide Ruby's center(), ljust(), rjust() string methods.
;;;    
(defun center (s width)
  (format nil "~V<~;~A~;~>" width s))

(defun ljust (s width)
  (format nil "~V<~A~;~>" width s))

(defun rjust (s width)
  (format nil "~V<~A~>" width s))

;;;
;;;    What's going on here?
;;;    
(defun squeeze (s &optional ch)
  (declare (ignore ch)) ;?
  (compress-string s))

;;;
;;;    See Slade ch. 9 2011
;;;    
(defun compress-string (s)
  (if (zerop (length s))
      s
      (with-output-to-string (result)
        (do ((i 0 (1+ i))
             (j 1 (1+ j)))
            ((= j (length s)) (write-char (char s i) result))
          (unless (char= (char s i) (char s j))
            (write-char (char s i) result)))) ))
  
(defun space-char-p (ch)
  (find ch #(#\Space #\Page #\Newline #\Return #\Tab)))
;;;
;;;    Clozure doesn't like #\Vt...
;;;    
;  (find ch #(#\Space #\Page #\Newline #\Return #\Tab #\Vt)))
;  (find ch '(#\Space #\Page #\Newline #\Return #\Tab #\Vt)))

;;;
;;;    Read a number from STDIN using a given prompt and meeting the given
;;;    TEST criterion. Continue prompting until a valid number is entered.
;;;
;;;    Note: READ-LINE is used so that all input is consumed rather than
;;;          leaving anything in buffer.
;;;          
; (defun get-num (prompt &optional min max)
;   (do ((num nil))
;       ((if (numberp num)
; 	   (and (if (numberp min)
; 		    (>= num min)
; 		    t)
; 		(if (numberp max)
; 		    (<= num max)
; 		    t))
; 	   nil)
;        num)
;     (format t "~A" prompt)
;     (setf num (read-from-string (read-line) nil))) )


;;;
;;;    This is really just to illustrate the FORMAT directive.
;;;    
(defun irregular-plural (n singular plural)
  (format nil (format nil "~~:[~A~~;~A~~]" plural singular) (= n 1)))

(defun short-ordinal (n)
  (let ((ordinal (format nil "~:R" n)))
    (format nil "~D~A" n (subseq ordinal (- (length ordinal) 2)))) )

;;;
;;;    Add Ruby-style string interpolation.
;;;    (let ((pung 8)) #"Is this not ${pung}?") => "Is this not 8?"
;;;
;;;    Should use Lispier syntax? (Problems with embedded commas in textual string!)
;;;    (let ((pung 8)) #"Is this not ,pung?") => "Is this not 8?"
;;;
;;;    Fix these:
;;;    Fixed 1. #"Is this not ${(concatenate 'string \"pun\" (string \"g\"))}"
;;;             Arbitrary expression should be allowed inside ${}. Should not need to escape quotes.
;;;    Still needs fixing:    Recursion allowed? #"Is this not ${#"pu${(coerce '(#\n #\g) 'string)}"}?"
;;;    Fixed 2. #"Is this not ${(concatenate 'string \"pun\" (string #\g))}"
;;;        #\g not handled properly here: Error: No dispatch function defined for #\g. [file position = 51]
;;;        

(defun read-delimited-string (stream)
  (labels ((read-non-escaped (out)
             (let ((ch (read-char stream nil nil t)))
               (case ch
                 ((nil) (error "Non-terminated string!"))
                 (#\\ (read-escaped out))
                 (#\")
                 (#\$ (write-char ch out) (read-possible-interpolated out))
                 (otherwise (write-char ch out) (read-non-escaped out)))) )
           (read-escaped (out)
             (let ((ch (read-char stream nil nil t)))
               (case ch
                 ((nil) (error "Non-terminated string!"))
                 (otherwise (write-char ch out) (read-non-escaped out)))) )
           (read-possible-interpolated (out)
             (let ((ch (read-char stream nil nil t)))
               (case ch
                 ((nil) (error "Non-terminated string!"))
                 (#\{ (write-char ch out) (read-interpolated out))
                 (#\$ (write-char ch out) (read-possible-interpolated out))
                 (#\\ (read-escaped out))
                 (#\")
                 (otherwise (write-char ch out) (read-non-escaped out)))) )
           (read-interpolated (out)
             (let ((ch (peek-char nil stream nil nil)))
               (case ch
                 ((nil) (error "Non-terminated string!"))
                 (#\} (read-non-escaped out))
                 (otherwise (prin1 (read stream nil nil t) out) (read-interpolated out)))) ))
;                 (otherwise (prin1 (print (read stream nil nil t)) out) (read-interpolated out)))) ))
    (with-output-to-string (out)
      (read-non-escaped out))))

(set-dispatch-macro-character #\# #\"
                              #'(lambda(stream ch n)
                                  (declare (ignore ch n))
                                  (let ((s (read-delimited-string stream))
                                        (result (gensym))
                                        (substring (gensym)))
                                    `(with-output-to-string (,result)
                                       (dolist (,substring (list ,@(interpolation-list s)))
                                         (princ ,substring ,result)))) ))

;; (set-dispatch-macro-character #\# #\"
;;                               #'(lambda(stream ch n)
;;                                   (declare (ignore ch n))
;;                                   (let ((s (read-delimited-string stream)))
;;                                     (with-output-to-string (result)
;;                                       (dolist (substring (list (interpolation-list s)))
;;                                         (princ substring result)))) ))

;; (set-dispatch-macro-character #\# #\"
;;                               #'(lambda(stream ch n)
;;                                   (declare (ignore ch n))
;;                                   (let ((s (read-delimited-string stream))
;;                                         (result (gensym))
;;                                         (substring (gensym)))
;;                                     (intern (symbol-name result))
;;                                     (intern (symbol-name substring))
;;                                     (let ((result (find-symbol (symbol-name result)))
;;                                           (substring (find-symbol (symbol-name substring))))
;;                                     `(with-output-to-string (,result)
;;                                        (dolist (,substring (list ,@(interpolation-list s)))
;;                                          (princ ,substring ,result)))) )))

(defun interpolation-list (s)
  (do* ((start (search "${" s) (search "${" s :start2 (+ start 2)))
        (index 0)
        (result '()))
       ((null start) (push (subseq s index) result) (nreverse result))
;       ((null start) (push (subseq s index) result) (print (nreverse result)))
    (let ((end (search "}" s :start2 (+ start 2))))
      (push (subseq s index start) result)
      (push (read-from-string s nil nil :start (+ start 2) :end end) result)
      (setf index (1+ end)))) )

