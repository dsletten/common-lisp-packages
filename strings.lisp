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
;(load "/home/slytobias/lisp/packages/test.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/core" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/core.lisp" :verbose nil))

(defpackage :strings
  (:use :common-lisp :core)
;  (:use :common-lisp :core :test)
  (:export :center :commify :commify-list :elide :english-and-list :english-or-list :get-article
           :irregular-plural :join
           :ljust
           :rjust
           :short-ordinal :space-char-p :split
           :squeeze :string-split :string-substitute))

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
(defun english-and-list (&rest items)
  (apply #'english-list 'and items))

(defun english-or-list (&rest items)
  (apply #'english-list 'or items))

(defun english-list (conjunction &rest items)
  (ecase conjunction  ; Lame! fix this...
    (and (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^, ~}~]" items))
    (or (apply #'format nil "~#[~;~A~;~A or ~A~:;~@{~#[~;or ~]~A~^, ~}~]" items))))

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
  (concatenate 'string "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^" (string comma-char) " ~}~]"))

;; (english-list 1 2 3) vs. (commify-list "1" "2" "three" "4")!!
;;
;;
;; (defun commify-list (&rest items)
;;   (if (some #'(lambda (s) (find #\, s)) items)
;;       (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^; ~}~]" items)
;;       (apply #'format nil "~#[~;~A~;~A and ~A~:;~@{~#[~;and ~]~A~^, ~}~]" items)))

;;;    
;;;    Replace part(s) of a string with another.
;;;
;;;    Default requires case-sensitive match.
;;;    Use :test #'char-equal for case-insensitive match.
;;;
(defun string-substitute (new old string &key (count nil) (start 0) (end nil) (test #'char=))
  (let ((match (search old string :test test :start2 start :end2 end)))
    (if (or (null match)
            (and (numberp count) (zerop count)))
        string
        (with-output-to-string (result)
          (string-replace new old string match result :count count :test test :end end)))) )

;;; Not designed to be called independently of STRING-SUBSTITUTE...(e.g., Assumes (not (zerop count)) initially, no need for :START)
(defun string-replace (new old source match result-stream &key count test end)
  (labels ((next (count)
             (if (null count)
                 count
                 (1- count)))
           (completed (count)
             (if (null count)
                 nil
                 (zerop count))))
    (do* ((length (length old))
          (count count (next count))
          (index 0 (+ match length))
          (match match (search old source :start2 index :test test :end2 end)))
         ((or (completed count) (null match)) (write-string (subseq source index) result-stream))
      (write-string (subseq source index match) result-stream)
      (write-string new result-stream))))

;;
;;    3 main cases:
;;    1. OLD not found in STRING => Return STRING as is.
;;    2. OLD only found once => Replace that single occurrence.
;;    3. OLD found in multiple locations => Replace all occurrences.
;;
;;    The 2nd and 3rd case have multiple variants:
;;    2a. OLD at beginning of STRING.
;;    2b. OLD in middle of STRING.
;;    2c. OLD at end of STRING.
;;
;;    3a. One occurrence at start of STRING.
;;    3b. One occurrence at end of STRING.
;;
;;    All 3 cases are impacted by the bounding indices START/END if present.
;;    
#|
(deftest test-string-substitute ()
  (check
   (eq (string-substitute "FOO" "zzzz" #1="The pattern is not present in the target string.") #1#)
   (string= (string-substitute "FOO" "pung" "pung is at the beginning of this string.") "FOO is at the beginning of this string.")
   (string= (string-substitute "FOO" "pung" "The pattern pung is in the middle of this string.") "The pattern FOO is in the middle of this string.")
   (string= (string-substitute "FOO" "pung" "This string ends in the pattern pung") "This string ends in the pattern FOO")
   (string= (string-substitute "FOO" "pung" "pung occurs in lots of pung places in this string pung but not at the end.")
            "FOO occurs in lots of FOO places in this string FOO but not at the end.")
   (string= (string-substitute "FOO" "pung" "The pattern pung occurs in lots of pung places in this string pung. Not at the start but at the end. pung")
            "The pattern FOO occurs in lots of FOO places in this string FOO. Not at the start but at the end. FOO")
   (string= (string-substitute "FOO" "pung" "pung occurs in lots of pung places in this string pung but not at the end." :count nil :start 0 :end nil)
            "FOO occurs in lots of FOO places in this string FOO but not at the end.")
   (string= (string-substitute "FOO" "pung" "The pattern pung occurs in lots of pung places in this string pung. Not at the start but at the end. pung" :count nil :start 0 :end nil)
            "The pattern FOO occurs in lots of FOO places in this string FOO. Not at the start but at the end. FOO")
   (eq (string-substitute "FOO" "pung" #2="This string contains the pattern pung. But it is outside the bounded subsequence." :end 10) #2#)
   (eq (string-substitute "FOO" "pung" #2# :end 34) #2#)
   (not (eq (string-substitute "FOO" "pung" #2# :end 37) #2#))
   (string= (string-substitute "FOO" "pung" #2# :end 37) "This string contains the pattern FOO. But it is outside the bounded subsequence.")
   (eq (string-substitute "FOO" "pung" #3="pung starts this string, but it is outside the bounded subsequence." :start 4) #3#)
   (string= (string-substitute "FOO" "pung" "pung occurs in lots of pung places in this string pung but the first occurrence of pung is outside the bounded subsequence." :start 4)
            "pung occurs in lots of FOO places in this string FOO but the first occurrence of FOO is outside the bounded subsequence.")
   (string= (string-substitute "FOO" "pung" "pung occurs in lots of pung places in this string pung but the first occurrence of pung is outside the bounded subsequence. So is this last pung." :start 4 :end 87)
            "pung occurs in lots of FOO places in this string FOO but the first occurrence of FOO is outside the bounded subsequence. So is this last pung.")
   (string= (string-substitute "FOO" "pung" #4="pung foo bar baz pung foo pung" :count 0) #4#)
   (string= (string-substitute "FOO" "pung" #4# :count 1) "FOO foo bar baz pung foo pung")
   (string= (string-substitute "FOO" "pung" #4# :count 2) "FOO foo bar baz FOO foo pung")
   (string= (string-substitute "FOO" "pung" #4# :count 2 :start 4) "pung foo bar baz FOO foo FOO")
   (string= (string-substitute "FOO" "pung" #4# :count 3 :end (- (length #4#) 4)) "FOO foo bar baz FOO foo pung")
   (string= (string-substitute "FOO" "PUNG" #5="Is this not pung?") #5#)
   (string= (string-substitute "FOO" "PUNG" "Is this not pung?" :test #'char-equal) "Is this not FOO?")
   (string= (string-substitute "FOO" "PUNG" "Is this not Pung?" :test #'char-equal) "Is this not FOO?")
   (string= (string-substitute "FOO" "PuNg" "Is this not pung?" :test #'char-equal) "Is this not FOO?")))
|#   
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
;  (format nil "~V:@<~A~>" width s))

(defun ljust (s width)
;  (format nil "~V<~A~;~>" width s))
  (format nil "~VA" width s))

(defun rjust (s width)
;  (format nil "~V<~A~>" width s))
  (format nil "~V@A" width s))


;;!!!
;; (defun center (s width)
;;   (spacify (list "" s "") width))

;; (defun ljust (s width)
;;   (spacify (list s "") width))

;; (defun rjust (s width)
;;   (spacify (list s) width))

;; (defun spacify (args width)
;;   (apply #'format nil "~V<~A~;~^~A~;~^~A~>" width args))

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

; (contains #{#\Space #\Page #\Newline #\Return #\Tab} ch)
;; (defun space-char-p (ch)
;;   (find ch #(#\Space #\Page #\Newline #\Return #\Tab)))

(defun space-char-p (ch)
  (case ch
    ((#\Space #\Page #\Newline #\Return #\Tab) t)
    (otherwise nil)))

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

(defun get-article (phrase)
  (cond ((symbolp phrase) (get-article (symbol-name phrase)))
        ((or (special-case-p phrase)
             (and (starts-with-vowel-p phrase) (not (unicornp phrase)))) "an")
        (t "a")))

(defun unicornp (phrase)
  (some #'(lambda (oddball)
            (search oddball phrase :test #'char-equal))
        '("unicorn" "uniform" "unique" "union" "united" "unilateral")))

(defun special-case-p (phrase)
  (some #'(lambda (oddball)
            (search oddball phrase :test #'char-equal))
        '("hour" "honor" "herb" "honest" "honorable" "honorary" "hourglass" "hourly" "hors d'oeuvre")))

(defun starts-with-vowel-p (phrase)
  (find (char phrase 0) "aeiou" :test #'char-equal))

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
;;;        Works! #"Is this not ${(concatenate 'string "pun" (string #\g))}" => "Is this not pung"
;;;    Still needs fixing:    Recursion allowed? #"Is this not ${#"pu${(coerce '(#\n #\g) 'string)}"}?"
;;;    Fixed 2. #"Is this not ${(concatenate 'string \"pun\" (string #\g))}"
;;;        #\g not handled properly here: Error: No dispatch function defined for #\g. [file position = 51]
;;;        Works! #"Is this not ${(concatenate 'string "pun" (string #\g))}" => "Is this not pung"
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

(defun elide (s width &optional (ellipsis "..."))
  (if (> (length s) width)
      (concatenate 'string (subseq s 0 (- width (length ellipsis))) ellipsis)
      s))

;;;
;;;    See utils.rb for handling floats.
;;;    
(defun commify (n)
  (format t "~:D~%" n))
