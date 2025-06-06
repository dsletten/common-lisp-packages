;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test-io.lisp
;;;;
;;;;   Started:            Tue Nov  5 00:53:07 2024
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
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(in-package :io)

(use-package :test)

;;;
;;;    No test
;;;
;print-plist

;(defun prompt-read (prompt &rest keys &key (allow-empty t) (trim t))
;; (prompt-read "This is the real deal: " :allow-empty nil :test #'(lambda (s) (member s '("pung" "foo" "bar") :test #'string-equal)))
;; (prompt-read "Is this not pung? " :test #'(lambda (s) (every #'alpha-char-p s)) :allow-empty nil)
;; (prompt-read "Is this not pung? " :test #'(lambda (s) (every #'alpha-char-p s)) :trim nil)
;; (prompt-read "Enter a letter. " :allow-empty nil :test #'(lambda (s) (and (= (length s) 1) (alpha-char-p (char s 0)))) ) 
        
;(defun get-num (prompt &key test (precision 'double-float))

(deftest test-read-num ()
  (check
   (handler-case (read-num "1:2")
     (error (e)
       (format t "Error was not handled: ~A~%" e)
       nil)
     (:no-error (obj)
       (declare (ignore obj))
       (format t "Package separator should not signal error.")
       t))))
  
(deftest test-valid-num-p ()
  (check
   (valid-num-p 8)
   (valid-num-p 8d0)
   (valid-num-p 1/8)
   (valid-num-p 8 #'integralp)
   (valid-num-p 8d0 #'integralp)
   (valid-num-p (sqrt -1))
   (not (valid-num-p "8"))
   (not (valid-num-p 'eight))
   (not (valid-num-p 8d0 #'integerp))
   (not (valid-num-p 1/8 #'integralp))
   (not (valid-num-p (sqrt -1) #'realp))))

(deftest test-read-word ()
  (check
   (string= "foo" (with-input-from-string (s "foo") (read-word s)))
   (string= "" (with-input-from-string (s "   ") (read-word s)))
   (string= "" (with-input-from-string (s "") (read-word s)))) )

(deftest test-read-words ()
  (check
   (equal '("Is" "this" "not" "pung?")
          (with-input-from-string (s "Is this not pung?")
            (read-words s)))
   (equal '("This" "love" "has" "taken" "its" "toll" "on" "me." "She" "said" "goodbye" "too" "many" "times" "before.")
          (with-input-from-string (s "This love has taken its toll on me. She said goodbye too many times before.")
            (read-words s)))) )
