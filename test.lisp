;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               test.lisp
;;;;
;;;;   Started:            Fri Jan  6 19:55:43 2006
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
;;;;   Notes: Seibel's test suite.
;;;;
;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/core" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/core.lisp" :verbose nil))

(defpackage :test
  (:use :common-lisp :core)
  (:export :check :combine-results :compare-reference-function :defsuite :deftest :find-tests :*test-name*))

(in-package :test)

(defvar *test-name* '())

(defmacro deftest (name (&rest parameters) &body body)
  `(defun ,name (,@parameters)
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;;;
;;;    This doesn't scale: suite of tests -> suite of suites? # of tests reported is confusing.
;;;    
(defmacro defsuite (name (&rest parameters) &body tests)
  (let ((count (length tests)))
    `(deftest ,name (,@parameters)
       (prog1 (combine-results ,@tests)
         (format t "~D test~:P run.~%" ,count)))) )

;; (defmacro defsuite (name (&rest parameters) &body tests)
;;   (let ((count (length tests)))
;;     `(defun ,name (,@parameters)
;;        (let ((*test-name* (append *test-name* (list ',name))))
;;          (prog1 (combine-results ,@tests)
;;            (format t "~D test~:P run.~%" ,count)))) ))

;;;
;;;    Test suites in different packages.
;;;    
;;(let ((packages '(ch02 ch03 ch04))) (loop for package in packages collect `#',(find-symbol (concatenate 'string "TEST-" (symbol-name package)) package)))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms
            for sym = (gensym)
            collect `(let ((,sym ,f)) (report-result ,sym ',f) ,sym))))

(defun combine-results (&rest args)
  (every #'identity args))

;; (defmacro combine-results (&body forms)
;;   (with-gensyms (result)
;;     `(let ((,result t))
;;        ,@(loop for f in forms
;;                collect `(unless ,f (setf ,result nil)))
;;        ,result)))

;;;
;;;    Don't print anything for the name if *TEST-NAME* is NIL.
;;;    
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ...~@[ ~A:~] ~S~%" result *test-name* form))

(defun find-tests (&optional (package *package*))
  (loop for sym being each symbol of package
        when (starts-with (symbol-name sym) "TEST")
        collect sym))

;;;
;;;    Compare a function to another reference implementation. The new version
;;;    should produce the same results as the original.
;;;
;;;    Example:
;;;    (deftest test-string-lessp ()
;;;      (compare-reference-function string-lessp
;;;                                  cl:string-lessp
;;;                                  *test-data*
;;;                                  :test eql))
;;;                                  
(defmacro compare-reference-function (f reference data &key (test 'equal))
  (let* ((datum (gensym))
         (form `(,test (apply #',f ,datum) (apply #',reference ,datum))))
    `(apply #'combine-results (loop for ,datum in ,data
                                    collect (report-result ,form (sublis (list (cons ',datum `',,datum)) ',form)))) ))
;                                  collect (report-result ,form (sublis (list (cons ',datum (list 'quote ,datum))) ',form)))) ))

;; (defmacro compare-reference-function (f reference data &key (test 'equal))
;;   (let* ((datum (gensym))
;;          (form `(,test (apply #',f ,datum) (apply #',reference ,datum))))
;;     `(combine-results (loop for ,datum in ,data
;;                             collect (report-result ,form ',form)))) )

;; (defmacro compare-reference-function (f reference data &key (test 'equal))
;;   `(let ((*test-name* (append *test-name* (list ',f))))
;;      (combine-results (loop for datum in ,data
;;                             collect (report-result (,test (apply #',f datum) (apply #',reference datum)) '(,test (apply #',f datum) (apply #',reference datum)))) )))

;; (defmacro compare-reference-function (f reference data &key (test 'equal))
;;   (let ((data data))
;;     `(check ,@(loop for datum in data
;;                     collect `(,test (,f ,@datum) (,reference ,@datum)))) )

#|
(defmacro compare-reference-function (f reference data &key (test 'equal))
  `(check ,@(loop for datum in data
                  collect `(,test (,f ,@datum) (,reference ,@datum)))) )

(macroexpand-1 '(compare-reference-function string-greaterp
                              cl:string-greaterp
                              (("abc" "pdg")
                               ("abc" "apdg")
                               ("abc" "abcd")
                               ("abcd" "abc")
                               ("alpha" "beta")
                               ("beta" "alpha")
                               ("alphabet" "alphabetize")
                               ("alphabetize" "alphabet"))
                              :test eql))
(CHECK (EQL (STRING-GREATERP "abc" "pdg")
            (COMMON-LISP:STRING-GREATERP "abc" "pdg"))
       (EQL (STRING-GREATERP "abc" "apdg")
            (COMMON-LISP:STRING-GREATERP "abc" "apdg"))
       (EQL (STRING-GREATERP "abc" "abcd")
            (COMMON-LISP:STRING-GREATERP "abc" "abcd"))
       (EQL (STRING-GREATERP "abcd" "abc")
            (COMMON-LISP:STRING-GREATERP "abcd" "abc"))
       (EQL (STRING-GREATERP "alpha" "beta")
            (COMMON-LISP:STRING-GREATERP "alpha" "beta"))
       (EQL (STRING-GREATERP "beta" "alpha")
            (COMMON-LISP:STRING-GREATERP "beta" "alpha"))
       (EQL (STRING-GREATERP "alphabet" "alphabetize")
            (COMMON-LISP:STRING-GREATERP "alphabet" "alphabetize"))
       (EQL (STRING-GREATERP "alphabetize" "alphabet")
            (COMMON-LISP:STRING-GREATERP "alphabetize" "alphabet")))
|#

;;;---------------Code-testing Functions------------------------
;;;
;;;    Should return how many tests passed/failed?
;;;    
(defun test (f test-data)
  "Perform automated testing of function F using TEST-DATA. The data should consist of a list of lists of trials. Each sublist should consist of a list of args to which F is applied and an expected result. The expected result is compared to the actual result."
  (format t "~&Testing function: ~A~%" f)
  (when (documentation f 'function)
    (format t "~4T~A~%" (documentation f 'function)))
  (format t "~%")
  (dolist (trial test-data)
    (let* ((inputs (car trial))
           (expected (cadr trial))
           (result (apply f inputs)))
      (if (equal result expected)
          (format t "~&Test passed: ~{ ~S~} => ~S~%" inputs result)
          (format t "~&Test FAILED: ~{ ~S~} => ~S [Should be ~S]~%"
                  inputs result expected))))
  (format t "~%") )

