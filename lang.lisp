;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               lang.lisp
;;;;
;;;;   Started:            Fri Jan  6 19:06:25 2006
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

;;;
;;;    Need queue for TAKE-DROP
;;;    

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/collections" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/collections.lisp" :verbose nil))

(defpackage :lang
  (:shadowing-import-from :collections :intersection :set :subsetp :union)
  (:use :common-lisp :collections)
  (:export :after :append1 :approximately= :array-indices :before :best
           :class-template :compose :conc1 :copy-array :cycle
           :defchain :destructure :dohash :doset :dostring :dotuples :dovector :drop :duplicate
           :ends-with :expand :explode
           :fif :filter :filter-split :find-some-if :find-subtree :fint :firsts-rests :flatten :fun
           :get-num :group :horners
	   :is-integer
           :last1 :list-to-string :longerp
           :macroexpand-all :make-identity-matrix :make-range
           :map-> :map-array :map-array-index :map0-n :map1-n :mapa-b :mapcars :mappend :mapset
           :memoize :mklist :mkstr :most :mostn
           :ppmx :prefixp :print-plist :prompt :prompt-read :prune :prune-if-not
           :read-num :repeat :reread :rmapcar 
           :rotate0 :rotate-list0 :rotate1 :rotate-list1
           :shift0 :shift-list0 :shift1 :shift-list1
           :show-symbols :shuffle :singlep :sort-symbol-list :splice
           :split-if :starts-with :symb
           :take :take-drop :transfer
           :transition :transition-1 :transition-n :translate :traverse :tree-find-if
           :until :valid-num-p :while :with-gensyms)  
  (:shadow :while :until :prefixp :dovector :macroexpand-all))

(in-package lang)


;;;
;;;    Splice a new sequence into another.
;;;    (Works like Perl splice()/substr())
;;;    Extend to vectors too?
;;;    
;;;    This is slightly different than Perl in that the new elements are
;;;    expected to be bundled into a sequence (list or string) rather than
;;;    passed as separate args. This is done to accommodate both lists and
;;;    strings in a uniform way.
;;;
(defun splice (seq &optional (offset 0) (length (length seq)) new-seq)
  "Splice a subsequence into a given sequence at a specified location."
  (if (minusp offset)
      (if (> (abs offset)
             (length seq))
          (setf offset 0)
          (setf offset (+ (length seq) offset))))
  (if (minusp length)
      (if (< (+ (length seq) length)
             offset)
          (setf length 0)
          (setf length (- (+ (length seq) length)
                          offset))))
  (let ((seq-type (typecase seq
                    (string 'string)
                    (list 'list))))
    (concatenate seq-type
                 (subseq seq 0 offset)
                 new-seq
                 (subseq seq (min (length seq) (+ offset length)))) ))

;;;
;;;    See translate.lisp
;;;    
(defun expand (char-range)
  (let ((pos (position #\- char-range)))
    (if pos
	(cond ((zerop pos) (error "Can't start with -"))
	      ((= 1 pos)
	       (let ((start (char char-range 0))
		     (end   (char char-range 2)))
		 (if (char< start end)
		     (concatenate 'string
				  (coerce (make-range start end) 'string)
				  (expand (subseq char-range 3)))
		     (error "Inverted range"))))
	      (t (concatenate 'string (subseq char-range 0 (1- pos))
			      (expand (subseq char-range (1- pos)))) ))
	char-range)))

(defun translate (target in out)
  (coerce (sublis (mapcar #'cons
			  (coerce (expand in) 'list)
			  (coerce (expand out) 'list))
		  (coerce target 'list))
	  'string))

;;;
;;;    These 2 are actually generic sequence functions.
;;;    - Separate implementation for lists?
;;;    
;; (defun starts-with (s1 s2 &key (test #'eql))
;;   "Determines whether or not the first sequence arg contains the second sequence arg as a subsequence at its start."
;;   (let ((length1 (length s1))
;;         (length2 (length s2)))
;;     (and (>= length1 length2)
;;          (search s2 s1 :end2 length2 :test test))))

(defun starts-with (s1 s2 &key (test #'eql))
  (let ((mismatch (mismatch s1 s2 :test test)))
    (if mismatch
        (>= mismatch (length s2))
        t)))

;;;
;;;    Simply using EVERY can fail when (< (length s1) (length s2))
;;;    E.g., (every #'eql "T" "TEST") => T but "T" does not start with "TEST"
;;;    
;; (defun starts-with (s1 s2 &key (test #'eql))
;;   (let ((length1 (length s1))
;;         (length2 (length s2)))
;;     (and (>= length1 length2)
;;          (every test s1 s2))))
  
;; (deftest test-starts-with ()
;;   (check   
;;     (starts-with "Is this not pung?" "Is")
;;     (starts-with "Is" "Is")
;;     (starts-with (subseq "Is this not pung?" 12) "pung")
;;     (not (starts-with "Is this not pung?" "is"))
;;     (starts-with "Is this not pung?" "is" :test #'equalp)
;;     (starts-with '(a b c) '(a b))
;;     (starts-with '(a b) '(a b))
;;     (starts-with (subseq [1 2 3 4 5] 2) [3 4])
;;     (starts-with #[1 10] #[1 3])))

;; (defun ends-with (s1 s2 &key (test #'eql))
;;   "Determines whether or not the second sequence arg is the ending subsequence of the first arg."
;;   (let ((length1 (length s1))
;;         (length2 (length s2)))
;;     (if (< length1 length2)
;;         nil
;;         (search s2 s1 :from-end t :start2 (- (length s1) (length s2)) :test test))))

(defun ends-with (s1 s2 &key (test #'eql))
  "Determines whether or not the second sequence arg is the ending subsequence of the first arg."
  (not (mismatch s1 s2 :start1 (max (- (length s1) (length s2)) 0) :test test :from-end t)))

;; (deftest test-ends-with ()
;;   (check   
;;     (ends-with "Is this not pung?" "pung?")
;;     (ends-with (subseq "Is this not pung?" 0 7) "this")
;;     (not (ends-with "Is this not pung?" "PUNG?"))
;;     (ends-with "Is this not pung?" "PUNG?" :test #'equalp)
;;     (ends-with '(a b c) '(b c))
;;     (ends-with (subseq [1 2 3 4 5] 0 4) [3 4])
;;     (ends-with #[1 10] #[8 10])))

(defun prompt-read (prompt &rest keys &key (allow-empty t) (trim t) test)
  (labels ((validate (response)
             (cond ((and (string= response "") (not allow-empty)) (fail))
                   ((null test) response)
                   ((funcall test response) response)
                   (t (fail))))
           (fail ()
             (apply #'prompt-read prompt keys)))
    (format *query-io* prompt)
    (force-output *query-io*)
    (let ((response (read-line *query-io*)))
      (if trim
          (validate (string-trim " " response))
          (validate response)))) )

;(defun get-num (prompt &optional test)
(defun get-num (prompt &key test (precision 'double-float))
  (let ((num (read-num (prompt-read prompt) :test test :precision precision)))
    (if (null num)
        (get-num prompt :test test :precision precision)
        num)))

(defun read-num (s &key test (precision 'double-float))
  "Attempt to read a number from string S. Apply the TEST validity function if provided. Return NIL if value is not valid."
  (let* ((*read-default-float-format* precision)
         (*read-eval* nil)
         (num (read-from-string s nil)))
    (if (valid-num-p num test)
        num
        nil)))

(defun valid-num-p (obj &optional test)
  (if (numberp obj)
      (if test
          (funcall test obj)
          t)
      nil))

;; (defun is-integer (x)
;;   (zerop (nth-value 1 (truncate x))))

;; (defun is-integer (x)
;;   (multiple-value-bind (_ rem) (truncate x)
;;     (declare (ignore _))
;;     (zerop rem)))      

(defun is-integer (x)
  (and (realp x) (zerop (rem x 1))))

;;;    The number may be constrained by optional min and max
;;;    args (which are inclusive).
; (defun get-num (prompt &optional min max)
;   (format t "~A" prompt)
;   (or (valid-num (read-from-string (read-line) nil nil) min max)
;       (get-num prompt min max)) )

; ;;;
; ;;;    GET-NUM should send in 3 args, but by making MIN and MAX optional
; ;;;    this can be used by other functions too.
; ;;;    
; (defun valid-num (num &optional min max)
;   (and (numberp num)
;        (if min (>= num min) t)
;        (if max (<= num max) t)
;        num) )

;;;
;;;    Christopher Stacy
;;;    
;; (defun read-a-number (&key stream default)
;;   (let* ((line (read-line stream))
;;          (n (let* ((*read-eval* nil))
;;               (ignore-errors (read-from-string line nil)))))
;;     (if (numberp n)
;;         n
;;       default)))

;;;
;;;    Matthew Danish
;;;    
;; (defun parse-float (string &optional (default 0.0))
;;   (let* ((*read-eval* nil)
;; 	 (val (ignore-errors (read-from-string string))))
;;     (typecase val
;;       (number val)
;;       (t default))))

;;;
;;;    Create a list of successive integers from START to END.
;;;    (Like Perl's (START..END))
;;;    See below...
; (defun make-range (&optional (start 0) (end 0))
;   (if (> start end)
;       (rotatef start end))
;   (mapcar #'(lambda (x)
; 	      (setf x start)
; 	      (incf start)
; 	      x)
; 	  (make-list (1+ (- end start)))) )

;;;
;;;    Convert NIL to ()
;;;    
(defun list-to-string (l)
  (labels ((list-to-string-aux (obj stream)
             (cond ((atom obj) (format stream "~S" obj))
                   (t (destructuring-bind (head . tail) obj
                        (if (listp head)
                            (format stream "~A" (list-to-string head))
                            (list-to-string-aux head stream))
                        (unless (null tail)
                          (if (atom tail)
                              (format stream " . ")
                              (format stream " "))
                          (list-to-string-aux tail stream)))) )))
    (if (listp l)
        (with-output-to-string (out)
          (format out "(")
          (unless (null l)
            (list-to-string-aux l out))
          (format out ")"))
        l)))

;; (defun list-to-string (l)
;;   (labels ((list-to-string-aux (obj stream)
;;              (cond ((atom obj) (format stream "~S" obj))
;;                    (t (if (listp (first obj))
;;                           (format stream "~A" (list-to-string (first obj)))
;;                           (list-to-string-aux (car obj) stream))
;;                       (unless (null (cdr obj))
;;                         (format stream " ")
;;                         (list-to-string-aux (cdr obj) stream)))) ))
;;     (if (listp l)
;;         (with-output-to-string (out)
;;           (format out "(")
;;           (unless (null l)
;;             (list-to-string-aux l out))
;;           (format out ")"))
;;         l)))


;;;
;;;    Pascal Bourguignon
;;;    
(defun show-symbols (package-name)
  (do-external-symbols (s package-name)
    (when (fboundp s) 
      (format t "~40A : ~A~%" s
	      (cond
		((special-operator-p s) "special-operator")
		((macro-function s) "macro")
		(t "function"))))
    (when (boundp s)
      (format t "~40A : ~A~%" s
	      (cond
		((constantp s) "constant variable")
		(t "variable")))) ))

;;;
;;;    This is slightly different than the version below. If (> i (length l))
;;;    this returns NIL for REMOVED.
;;;
;;;    It also builds a completely new list for the REMAINDER.
;;;    
(defun transfer (seq i)
  "Remove the ith element from a sequence, returning the element and the new sequence."
  (typecase seq
    (list (loop for elt in seq
                for j from 0
                unless (= i j) collect elt into remainder
                when (= i j) collect elt into removed
                finally (return (values (first removed) remainder))))
    (string (let ((result (make-string (1- (length seq)))))
              (setf (subseq result 0 i) (subseq seq 0 i)
                    (subseq result i) (subseq seq (1+ i)))
              (values (elt seq i) result)))
    (vector (let ((result (make-array (1- (length seq)))) )
              (setf (subseq result 0 i) (subseq seq 0 i)
                    (subseq result i) (subseq seq (1+ i)))
              (values (elt seq i) result)))) )

;; (defun transfer (seq i)
;;   "Remove the ith element from a sequence, returning the element and the new sequence."
;;   (typecase seq
;;     (list (loop for elt in seq
;;                 for j from 0
;;                 unless (= i j) collect elt into remainder
;;                 when (= i j) collect elt into removed
;;                 finally (return (values (first removed) remainder))))
;;     (vector (let ((result (make-sequence (type-of seq) (1- (length seq)))))    ; <---- Doesn't work. Shorter vector is different type...
;;               (setf (subseq result 0 i) (subseq seq 0 i)
;;                     (subseq result (1+ i)) (subseq seq (1+ i)))
;;               (values (elt seq i) result)))) )

;; (defun transfer (l i)
;;   "Remove the ith element from a list, returning the element and the new list."
;;   (loop for elt in l
;;         for j from 0
;;         unless (= i j) collect elt into remainder
;;         when (= i j) collect elt into removed
;;         finally (return (values (first removed) remainder))))

;; (defun transfer (l i)
;;   "Remove the ith element from a list, returning the element and the new list."
;;   (let* (removed
;;          (remainder (loop for elt in l
;;                           for j from 0
;;                           unless (= i j) collect elt
;; 			              when (= i j) do (setf removed elt))))
;;     (values removed remainder)))

;;;
;;;    Surprisingly (?) the tail-recursive version is faster than the iterative
;;;    version. (Because of the extra AND test on every loop?)
;;;    The naive version is the slowest. It does the most CONSing. The other
;;;    two do less (the same amount, in fact).
;;;
;;;    Note: The new list returned may share structure with the original!
;;;    
;; (defun transfer (l i)
;;   "Remove the ith element from a list, returning the element and the new list."
;;   (labels ((transfer-aux (l i result)
;; 	     (cond ((null l) (error "Could not transfer element"))
;; 		   ((zerop i) (values (car l)
;; 				      (append (nreverse result) (cdr l))))
;; 		   (t (transfer-aux (cdr l)
;; 				    (1- i)
;; 				    (cons (car l) result)))) ))
;;     (transfer-aux l i '())))

; (defun transfer-a (l i)
;   (let ((elt (nth i l)))
;     (values elt (remove elt l :start i :count 1))))

; (defun transfer-b (l i)
;   "Remove the ith element from a list, returning the element and the new list."
;   (do ((result '() (cons (car list) result))
;        (list l (cdr list))
;        (index i (1- index)))
;       ((and list (zerop index)) (values (car list)
; 					(append (nreverse result) (cdr list))))
;     (when (null list)
;       (error "Could not transfer element"))))

;; (defun drop (l n)
;;   "Drop the first N elements of list L."
;;   (assert (typep n `(integer 0 ,(length l)))
;; 	  (n)
;; 	  "N must be a non-negative integer less than or equal to ~D."
;; 	  (length l))
;;   (nthcdr n l))

(defun drop (n seq)
  "Drop the first N elements of sequence SEQ."
  (typecase seq
    (list (nthcdr n seq))
    (vector (subseq seq (min n (length seq)))) ))

;; (defun take (l n)
;;   "Take the first N elements of list L."
;;    (assert (listp l) (l) "L should be a list.")
;; ;;   (assert (typep n `(integer 0 ,(length l)))
;; ;; 	  (n)
;; ;; 	  "N must be a non-negative integer less than or equal to ~D."
;; ;; 	  (length l))
;;   (loop for elt in l
;; 	for i from 1
;; 	while (<= i n) collect elt))

(defun take (n seq)
  "Take the first N elements of sequence SEQ."
  (typecase seq
    (list (loop for i from 0 below n
                    for elt in seq
                    collect elt))
    (vector (subseq seq 0 (min n (length seq)))) ))

;; (defun take (l n)
;;   "Take the first N elements of list L."
;;   (assert (listp l) (l) "L should be a list.")
;;   (assert (typep n `(integer 0 ,(length l)))
;; 	  (n)
;; 	  "N must be a non-negative integer less than or equal to ~D."
;; 	  (length l))
;;   (labels ((take-aux (l n take)
;; 	     (cond ((zerop n) (nreverse take))
;; 		   (t (take-aux (cdr l) (1- n) (cons (car l) take)))) ))
;;     (take-aux l n '())))

;; (defun take-drop (l n)
;;   (values (butlast l (- (length l) n))
;; 	  (nthcdr n l)))

;; (defun take-drop (l n)
;;   "Split a list at the Nth element. Return the sublists before and after."
;;   (list (butlast l (- (length l) n))
;; 	(nthcdr n l)))

(defun take-drop (n seq)
  "Split a list at the Nth element. Return the sublists before and after."
  (assert (typep n `(integer 0))
          (n)
          "N must be a non-negative integer.")
  (typecase seq
    (list ;(if (zerop n) <-- Unnecessary when using queue explicitly. Only needed with LOOP version?
          ;    (values '() seq)
              (do ((q (make-linked-queue))
                   (tail seq (rest tail))
                   (i 0 (1+ i)))
                  ((or (= i n) (endp tail)) (values (elements q) tail))
                (enqueue q (first tail)))); )
;;               (loop for i from 0 below n
;;                     for elt in seq
;;                     for tail on seq
;;                     collect elt into q
;;                     do (print (list i elt q tail))
;;                     finally (return (values q #- :clisp (rest tail) #+ :clisp tail))) ))
    (vector (values (take n seq) (drop n seq)))) ) ; Works for strings too

;; (defun empty-seq (in)
;;   (map (type-of in) #'identity in))

;; (defun empty-seq (in)
;;   (make-sequence (type-of in) 0))

(defun empty-seq (in)
  (typecase in
    (list (list))
    (string (make-string 0))
    (vector (vector))))

;; (defun take-drop (l n)
;;   "Split a list at the Nth element. Return the sublists before and after."
;;   (assert (typep n `(integer 0 ,(length l)))
;; 	  (n)
;; 	  "N must be a non-negative integer less than or equal to ~D."
;; 	  (length l))
;;   (do ((q (make-linked-queue))
;;        (tail l (rest tail))
;;        (i 0 (1+ i)))
;;       ((= i n) (values (elements q) tail))
;;     (enqueue q (first tail))))

;; (defun take-drop (l n)
;;   "Split a list at the Nth element. Return the sublists before and after."
;;   (assert (typep n `(integer 0 ,(length l)))
;; 	  (n)
;; 	  "N must be a non-negative integer less than or equal to ~D."
;; 	  (length l))
;;   (labels ((take-drop-aux (l n take)
;; 	     (cond ((zerop n) (values (nreverse take) l))
;; 		   (t (take-drop-aux (cdr l) (1- n) (cons (car l) take)))) ))
;;     (take-drop-aux l n '())))


(defun prefix-generator (l)
  (let ((q (make-linked-queue))
        (list l))
    #'(lambda ()
        (prog1 (copy-list (elements q))
;        (prog1 (elements q)
          (if (endp list)
              (make-empty q)
              (enqueue q (pop list)))) )))

;;;
;;;    Lists
;;;    
;; (defun prefixp (l1 l2)
;;   (cond ((null l1) t) ; endp?
;; 	((null l2) nil)
;; 	((eql (first l1) (first l2)) (prefixp (rest l1) (rest l2)))
;; 	(t nil)))

;;;
;;;    Sequences
;;;    
;; (defun prefixp (s1 s2)
;;   "Is S1 a prefix of S2"
;;   (let ((l1 (length s1)))
;;     (if (>= (length s2) l1)
;;         (search s1 s2 :end2 l1) ; (= (mismatch s1 s2) l1)
;;         nil)))

(defun prefixp (seq1 seq2 &key (test #'eql))
  (let ((match (search seq1 seq2 :test test)))
    (if (null match)
        nil
        (zerop match))))

;---------------Macros------------------------
;; (defmacro while (test &body body)
;;   `(do ()
;;        ((not ,test))
;;      ,@body) )

(defmacro while (test &body body)
  `(loop (unless ,test (return))
      ,@body))

;; (defmacro while (test &body body)
;;   `(loop while ,test
;;          do ,@body))

;; (defmacro while (test &body body)
;;   (let ((tag (gensym)))
;;     `(block nil
;;        (tagbody
;;           ,tag
;;           (unless ,test (return))
;;           ,@body
;;           (go ,tag)))) )

;; (defmacro until (test &body body)
;;   `(do ()
;;        (,test)
;;      ,@body) )

(defmacro until (test &body body)
  `(loop (when ,test (return))
      ,@body) )

;; (defmacro dovector ((var vector-exp &optional result) &body body)
;;   (let ((v (gensym))
;;         (l (gensym))
;;         (i (gensym)))
;;     `(do* ((,v ,vector-exp)
;;            (,l (length ,v))
;;            (,i 0 (1+ ,i))
;;            ,var)
;;           ((= ,i ,l) ,result)
;;        (setf ,var (aref ,v ,i))
;;        ,@body)))

(defmacro dovector ((var vector-exp &optional result) &body body)
  (let ((v (gensym))
        (l (gensym))
        (i (gensym)))
    `(do* ((,v ,vector-exp)
           (,l (length ,v))
           (,i 0 (1+ ,i)))
          ((= ,i ,l) ,result)
       (let ((,var (aref ,v ,i)))
         ,@body))))

;;;
;;;    Don't (declare (ignore ,i))!!
;;;    The macro doesn't use it directly, but DOTIMES does!
;;;    
(defmacro repeat (count &body body)
  (let ((i (gensym)))
    `(dotimes (,i ,count)
       ,@body)))

;; (defmacro repeat (count &body body)
;;   (if (null body)
;;       nil
;;       `(loop repeat ,count
;;              do ,@body)))

;; (defmacro repeat (count &body body)
;;   `(loop repeat ,count
;;          do ,@body))

;;;
;;;    Disallow crosstalk w/ LOOP keywords in BODY?
;;;    Binding of VARS should be available in RESULT...
;;;    
(defmacro dotuples (((&rest vars) l &optional result) &body body)
  (let ((list (gensym))
        (rest (gensym)))
    `(do ((,list ,l))
         ((endp ,list) ,result)
       (destructuring-bind (,@vars . ,rest) ,list
         ,@body
         (setf ,list ,rest)))) )

;; (defmacro dotuples (((&rest vars) l &optional result) &body body)
;;   (let ((res (gensym)))
;;     `(let ((,res ,result))
;;        (loop for ,vars on ,l by #'(lambda (l) (nthcdr ,(length vars) l))
;;              do ,@body)
;;        (or ,res nil))))

;; (defmacro dostring ((ch string &optional result) &body body)
;;   (let ((s (gensym))
;;         (l (gensym))
;;         (i (gensym)))
;;     `(do* ((,s ,string)
;;            (,l (length ,s))
;;            (,i 0 (1+ ,i))
;;            ,ch)
;;           ((= ,i ,l) ,result)
;;        (setf ,ch (char ,s ,i))
;;        ,@body)))

(defmacro dostring ((ch string &optional result) &body body)
  (let ((s (gensym))
        (l (gensym))
        (i (gensym)))
    `(do* ((,s ,string)
           (,l (length ,s))
           (,i 0 (1+ ,i)))
          ((= ,i ,l) ,result)
       (let ((,ch (char ,s ,i)))
         ,@body))))

;; (defmacro dostring ((ch s &optional result) &body body)
;;   `(dovector (,ch ,s ,result)
;;      ,@body))             

;; (defmacro dostring ((ch s &optional result) &body body)
;;   (let ((res (gensym)))
;;     `(let ((,res ,result))
;;        (loop for ,ch across ,s
;;              do ,@body)
;;        (or ,res nil))))

(defmacro dohash (((key val) hash &optional result) &body body)
  (let ((next (gensym))
        (more (gensym)))
    `(with-hash-table-iterator (,next ,hash)
       (loop (multiple-value-bind (,more ,key ,val) (,next)
               (unless ,more (return ,result))
               ,@body)))) )

(defmacro doset ((elt set &optional result) &body body)
  `(dolist (,elt (elements ,set) ,result)
     ,@body))

;; (defmacro doset ((elt set &optional result) &body body)
;;   (let ((next (gensym))
;;         (more (gensym))
;;         (val (gensym)))
;;     `(with-hash-table-iterator (,next ,set)
;;        (loop (multiple-value-bind (,more ,elt ,val) (,next)
;;                (declare (ignore ,val))
;;                (unless ,more (return ,result))
;;                ,@body)))) )

;;;
;;;    Should be (defun mapset (f &rest sets)?
(defun mapset (f set)
  (let ((result (make-set :test (hash-table-test (slot-value set 'elements)))) )
    (doset (elt set)
      (add-elt result (funcall f elt)))
    result))

;---------------Touretzky------------------------
(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
    (cond ((equal exp exp1)
	   (format t "~&Macro expansion:")
	   (pprint exp))
	  (t (format t "~&First step of expansion:")
	     (pprint exp1)
	     (format t "~2%Final expansion:")
	     (pprint exp)))
    (format t "~2%")
    (values)))

;---------------Graham On Lisp------------------------
(defun last1 (l)
  (first (last l)))

(defun singlep (l)
  (and (consp l) (null (rest l))))

(defun append1 (l obj)
  (append l (list obj)))

(defun conc1 (l obj)
  (nconc l (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

;;;
;;;    Graham calls this LONGER. I believe this is inappropriate since this
;;;    function does not return the 'longer' of the two sequences. Rather, it
;;;    simply tests whether SEQ1 is longer than SEQ2.
;;;    
(defun longerp (seq1 seq2)
  (labels ((compare (seq1 seq2)
             (cond ((endp seq1) nil)
                   ((endp seq2) t)
                   (t (compare (rest seq1) (rest seq2)))) ))
    (if (and (listp seq1) (listp seq2))
        (compare seq1 seq2)
        (> (length seq1) (length seq2)))) )

;; (defun longerp (l1 l2)
;;   (labels ((compare (l1 l2)
;; 	     (and (consp l1)
;; 		  (or (null l2)
;; 		      (compare (cdr l1) (cdr l2)))) ))
;;     (if (and (listp l1) (listp l2))
;; 	(compare l1 l2)
;; 	(> (length l1) (length l2)))) )

;; (deftest test-longerp ()
;;   (check
;;    (longerp '(a b c) '(d e))
;;    (not (longerp '(a b c) '(d e f)))
;;    (not (longerp '(a b c) '(d e f g)))
;;    (longerp "abc" "de")
;;    (not (longerp "abc" "def"))
;;    (not (longerp "abc" "defg"))
;;    (longerp [1 2 3] [4 5])
;;    (not (longerp [1 2 3] [4 5 6]))
;;    (not (longerp [1 2 3] [4 5 6 7]))))

;;;
;;;    This collects the _values_ of applying the function to elts, not
;;;    the elts themselves. Different from REMOVE-IF-NOT!
;;;    
(defun filter (f seq)
  (typecase seq
    (list (loop for elt in seq
                for val = (funcall f elt)
                when val collect val))
    (vector (loop for elt across seq
                  for val = (funcall f elt)
                  when val collect val into result
                  finally (return (coerce result 'vector)))) ))

;; (defun filter (fn list)
;;   (let ((acc '()))
;;     (dolist (x list)
;;       (let ((val (funcall fn x)))
;; 	(when val (push val acc))))
;;     (nreverse acc)))

;;;
;;;    Collect elts that pass test.
;;;    
;; (defun filter (fn list)
;;   (let ((acc '()))
;;     (dolist (x list)
;;       (let ((val (funcall fn x)))
;; 	(when val (push x acc))))
;;     (nreverse acc)))

;; (defun filter (f seq)
;;   (remove-if-not f seq))

;; (defun filter (f seq)
;;   (etypecase seq
;;     (list (loop for elt in seq
;;                 when (funcall f elt)
;;                 collect elt))
;;     (string (coerce (loop for elt across seq
;;                           when (funcall f elt)
;;                           collect elt)
;;                     'string))
;;     (vector (coerce (loop for elt across seq
;;                           when (funcall f elt)
;;                           collect elt)
;;                     'vector))))

;;;
;;;    See matrix::list-to-rows-fill-rows
;;;    
;; (defun group (source n)
;;   (when (zerop n) (error "Invalid length."))
;;   (labels ((group-aux (source acc)
;;              (let ((rest (nthcdr n source)))
;;                (if (consp rest)
;;                    (group-aux rest (cons (subseq source 0 n) acc))
;;                    (nreverse (cons source acc)))) ))
;;     (if source
;;         (group-aux source '())
;;         '())))

(defun group (l n)
  (loop for take-drop = (multiple-value-list (take-drop n l))
                   then (multiple-value-list (take-drop n (second take-drop)))
        until (null (first take-drop))
        collect (first take-drop)))

(defun flatten (tree)
  (labels ((flatten-aux (tree result)
             (cond ((null tree) (nreverse result))
                   ((null (car tree)) (flatten-aux (cdr tree) result))
                   ((atom (car tree)) (flatten-aux (cdr tree) (cons (car tree) result)))
                   (t (flatten-aux (list* (caar tree) (cdar tree) (cdr tree)) result)))) )
    (if (atom tree)
        tree
        (flatten-aux tree '()))) )

;; (defun flatten (obj)
;;   (labels ((flatten-aux (obj results)
;;              (cond ((null obj) results)
;;                    ((atom obj) (cons obj results))
;;                    (t (flatten-aux (car obj)
;;                                    (flatten-aux (cdr obj) results)))) ))
;;     (flatten-aux obj '()))) 

(defun prune (pred obj)
  (labels ((prune-aux (obj acc)
             (cond ((null obj) (nreverse acc))
                   ((atom (car obj)) (prune-aux (cdr obj)
                                                (if (funcall pred (car obj))
                                                    acc
                                                    (cons (car obj) acc))))
                   (t (prune-aux (cdr obj)
                                 (cons (prune-aux (car obj) '()) acc)))) ))
    (prune-aux obj '())))

(defun prune-if-not (pred obj)
  (labels ((prune-aux (obj acc)
             (cond ((null obj) (nreverse acc))
                   ((atom (car obj)) (prune-aux (cdr obj)
                                                (if (funcall pred (car obj))
                                                    (cons (car obj) acc)
                                                    acc)))
                   (t (prune-aux (cdr obj)
                                 (cons (prune-aux (car obj) '()) acc)))) ))
    (prune-aux obj '())))

;;;
;;;    Graham describes this (find2) as a combination of FIND-IF and SOME. It returns
;;;    the first element (FIND-IF) and the value of applying a function to that element (SOME)
;;;    when that value is true.
;;;    
;; (defun find-some-if (fn list)
;;   (if (endp list)
;;       nil
;;       (let ((val (funcall fn (first list))))
;;         (if val
;;             (values (first list) val)
;;             (find-some-if fn (rest list)))) ))

;; (defun find-some-if (f list)
;;   (do* ((l list (rest l))
;;         (elt (first l) (first l))
;;         (val (funcall f elt) (funcall f elt)))
;;        ((endp l) nil)
;;     (when val
;;       (return (values elt val)))) )

(defun find-some-if (f list)
  (loop for elt in list
        for val = (funcall f elt)
        when val
        return (values elt val)))

;;;
;;;    Does X occur before Y in list?
;;;    Y need not actually occur in the list! (See AFTER)
;;;    
(defun before (x y list &key (test #'eql))
  (and list
       (let ((first (car list)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) list)
	       (t (before x y (cdr list) :test test)))) ))

(defun after (x y list &key (test #'eql))
  (let ((rest (before y x list :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj list &key (test #'eql))
  (member obj (cdr (member obj list :test test)) :test test))

(defun split-if (fn list)
  (let ((acc '()))
    (do ((src list (cdr src)))
	((or (null src)
	     (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn list)
  (if (null list)
      (values nil nil)
      (let* ((wins (car list))
             (max (funcall fn wins)))
        (dolist (obj (cdr list))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
		    max score))))
        (values wins max))))

(defun best (fn list)
  (if (null list)
      nil
      (let ((wins (car list)))
        (dolist (obj (cdr list))
          (when (funcall fn obj wins)
	    (setq wins obj)))
        wins)))

(defun mostn (fn list)
  (if (null list)
      (values nil nil)
      (let ((result (list (car list)))
            (max (funcall fn (car list))))
        (dolist (obj (cdr list))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;;
;;;    What about nil as an element?
;;;    
;; (defun tree-find-if (fn obj)
;;   (cond ((null obj) nil)
;; 	((atom obj) (and (funcall fn obj) obj))
;; 	(t (or (tree-find-if fn (first obj))
;; 	       (tree-find-if fn (rest obj)))) ) )

(defun tree-find-if (fn obj &key (direction :dfs))
  (ecase direction
    (:dfs (tree-find-if-dfs fn obj))
    (:bfs (tree-find-if-bfs fn obj))))

(defun tree-find-if-dfs (fn obj)
  (cond ((null obj) nil)
	((atom obj) (and (funcall fn obj) obj))
	(t (or (tree-find-if fn (first obj))
	       (tree-find-if fn (rest obj)))) ) )

(defun tree-find-if-bfs (fn obj)
  (let ((nodes (make-linked-queue)))
    (labels ((traverse-level (l)
	       (unless (endp l)
		 (enqueue nodes (first l))
		 (traverse-level (rest l))))
	     (process ()
	       (print nodes)
	       (cond ((emptyp nodes) nil)
		     ((atom (front nodes)) (cond ((funcall fn (front nodes)) (front nodes))
						 (t (dequeue nodes)
						    (process))))
		     (t (traverse-level (dequeue nodes))
			(process)))) )
      (traverse-level obj)
      (process))))

;;;
;;;    Find subtree with OBJ as head. I.e., find the CONS with OBJ as CAR.
;;;    OBJ itself may be any value. (Specific :test may need to be applied.)
;;;
(defun find-subtree (obj tree &key (test #'eql))
  (cond ((atom tree) nil)
	((funcall test (car tree) obj) tree)
	(t (or (find-subtree obj (car tree) :test test)
	       (find-subtree obj (cdr tree) :test test))) ))

;;;
;;;    Compare TRANSITION:
;;;    
;; (dotimes (i 5) (print (multiple-value-list (take-drop i #[1 4]))))

;; (NIL (2 3 4)) 
;; ((1) (2 3 4)) 
;; ((1 2) (3 4)) 
;; ((1 2 3) (4)) 
;; ((1 2 3 4) NIL) 

;; (loop for i from 0 to 5 collect (multiple-value-list (take-drop i #[1 4])))

;; ((NIL (2 3 4)) ((1) (2 3 4)) ((1 2) (3 4)) ((1 2 3) (4)) ((1 2 3 4) NIL)
;;  ((1 2 3 4) NIL))

;;;
;;;    Split a list into successive CDRs and everything before that CDR.
;;;
;;;    This should do NREVERSE at end?
;;;    (transition '(a b c)) => (((A B C) NIL) ((A B) (C)) ((A) (B C)) (NIL (A B C)))
;;;    Above result is backwards?
;;;    
;; (defun transition (l)
;;   (do ((acc '() (cons (list head tail) acc))
;;        (head '() (append1 head (car tail)))
;;        (tail l (cdr tail)))
;;       ((null tail) (cons (list head tail) acc))))

; (defun transition-1 (l)
;   (labels ((transition-aux (l1 l2 results)
; 	     (cond ((null l2) (cons (list l1 l2) results))
; 		   (t (transition-aux (append1 l1 (car l2))
; 				     (cdr l2)
; 				     (cons (list l1 l2) results)))) ))
;     (transition-aux '() l '())))

;;;
;;;    Given a list and a tail of that list, accumulate all elements
;;;    preceding the tail.
;;;    (let ((l (list 1 2 3 4 5))) (build-prefix l (nthcdr 4 l))) => (1 2 3 4)
;;;    
(defun build-prefix (l tail)
  (cond ((eq l tail) '())
        (t (cons (first l) (build-prefix (rest l) tail)))) )

;;;
;;;    Similar to TRANSITION (below), but doesn't capture the final pair where
;;;    all elements are in the prefix and the tail is empty.
;;;
;;;    (transition '(a b c)) => ((NIL (A B C)) ((A) (B C)) ((A B) (C)) ((A B C) NIL))
;;;    (transition-1 '(a b c)) => ((NIL (A B C)) ((A) (B C)) ((A B) (C)))
(defun transition-1 (l)
  (maplist #'(lambda (tail)
               (list (build-prefix l tail) tail)) l))

;;;
;;;    This seems to be faster than above DO loop version.
;;;    
(defun transition (l)
  (transition-aux l l))

(defun transition-aux (l tail)
  (cond ((endp tail) (list (list l '())))
        (t (cons (list (build-prefix l tail) tail)
                 (transition-aux l (rest tail)))) ))

;; (defun transition (l)
;;   (loop for tail on l
;;         collect (list (build-prefix l tail) tail)
;;         when (null (cdr tail))
;;         collect (list l '())))

;;;
;;;    (transition-n l 0) == (transition l)
;;;    (transition-n l 1) == (transition-1 l)
;;;    
(defun transition-n (l n)
  (transition-n-aux l l (nthcdr n l)))

(defun transition-n-aux (l tail sentinel)
  (cond ((endp sentinel) (cons (list (build-prefix l tail) tail) '()))
        (t (cons (list (build-prefix l tail) tail)
                 (transition-n-aux l (rest tail) (rest sentinel)))) ))

;; (defun mapa-b (fn a b &optional (step 1))
;;   (do ((i a (+ i step))
;;        (result '()))
;;       ((> i b) (nreverse result))
;;     (push (funcall fn i) result)))

(defun mapa-b (f a b &optional (step 1))
  (loop for i from a to b by step
        collect (funcall f i)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun make-range (start &optional end (step 1))
  (cond ((characterp start)
         (cond ((characterp end)
                (mapcar #'code-char (make-range (char-code start)
                                                (char-code end)
                                                step)))
               ;;
               ;;    Does it make sense for 1-arg with character?
               ((null end) (mapcar #'code-char (make-range 0
                                                           (1- (char-code start))
                                                           step)))
               (t (error "Mismatched input types."))))
        ((numberp start)
         (cond ((numberp end)
                (if (> start end)
                    (loop for i from start downto end by step collect i)
                    (loop for i from start to end by step collect i)))
;;                 (when (> start end)
;;                   (rotatef start end))
;;                 (loop for i from start to end by step collect i))
               ((null end)
                (loop for i from 0 below start by step collect i))
               (t (error "Mismatched input types.")))) ))

;; (defun make-range (start end &optional (step 1))
;;   (cond ((characterp start)
;; 	 (if (characterp end)
;; 	     (mapcar #'code-char (make-range (char-code start)
;; 					     (char-code end)
;;                                           step))
;; 	     (error "Mismatched input types.")))
;; 	(t (when (> start end)
;; 	     (rotatef start end))
;; 	   (do ((i start (+ i step))
;; 		(result '()))
;; 	       ((> i end) (nreverse result))
;; 	     (push i result)))) )

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result '()))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result '()))
    (dolist (l lsts)
      (dolist (obj l)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar #'(lambda (&rest args)
			  (apply #'rmapcar fn args))
	     args)))

;(defun readlist (&rest args)
(defun read-list (&rest args)
  (values (read-from-string
           (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
	 (return)
	 (format *query-io* "~A~%" (funcall fn in)))) ))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))) )))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (last1 fns))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;;;
;;;    Slightly modified from Graham.
;;;    
(defun fif (if then &optional else)
  (if else
      #'(lambda (x)
	  (if (funcall if x)
	      (funcall then x)
	      (funcall else x)))
      #'(lambda (x)
	  (if (funcall if x)
	      (funcall then x)))) )

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x)))) ))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	#'(lambda (x)
	    (or (funcall fn x) (funcall chain x)))) ))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop , stop))
         ((> ,var ,gstop))
       ,@body)))

					 
(defun shuffle (v &optional (random-state *random-state*))
  "Randomize vector V using Fisher-Yates algorithm."
  (loop for i from (1- (length v)) downto 1
        for j = (random (1+ i) random-state)
        unless (= i j)
        do (rotatef (aref v i) (aref v j)))
  v)

;;;
;;;    (shift-list0 12 2) => (12 13 2 3 4 5 6 7 8 9 10 11)
;;;    (shift-list1 12 2) => (13 14 3 4 5 6 7 8 9 10 11 12)
;;;    (rotate-list0 12 2) => (10 11 0 1 2 3 4 5 6 7 8 9)
;;;    (rotate-list1 12 2) => (11 12 1 2 3 4 5 6 7 8 9 10)
;;;    (rotate-list0 12 -2) => (2 3 4 5 6 7 8 9 10 11 0 1)
;;;    (rotate-list1 12 -2) => (3 4 5 6 7 8 9 10 11 12 1 2)
;;;
;;;    (shift-list0 12 1) => (12 1 2 3 4 5 6 7 8 9 10 11)
;;;    (rotate-list1 12 1) => (12 1 2 3 4 5 6 7 8 9 10 11)
;;;    
(defun rotate1 (n m i)
  (1+ (mod (+ i (- n (1+ m))) n)))

(defun rotate0 (n m i)
  (mod (+ i (- n m)) n))

(defun rotate-list1 (n m)
  (let ((list (loop for i from 1 to n collect i)))
    (mapcar #'(lambda (x) (rotate1 n m x)) list)))

(defun rotate-list0 (n m)
  (let ((list (loop for i from 0 to (1- n) collect i)))
    (mapcar #'(lambda (x) (rotate0 n m x)) list)))

(defun shift1 (n m i)
  (+ (mod (+ i (- n (1+ m))) n) (1+ m)))

(defun shift0 (n m i)
  (+ (mod (+ i (- n m)) n) m))

;;;
;;;    Given the numbers 1, . . ., n, map the first m elements to n+1, . . ., n+m
;;;    I.e., 1, . . ., n -> n+1, . . ., n+m, m+1, . . ., n
;;;
;;;    Example:
;;;    Zeller's congruence expects January, February to be months 13, 14 of previous year:
;;;    1, . . ., 12 -> 13, 14, 3, . . ., 12
;;;    
(defun shift-list1 (n m)
  (let ((list (loop for i from 1 to n collect i)))
    (mapcar #'(lambda (x) (shift1 n m x)) list)))

;;;
;;;    Given the numbers 0, . . ., n-1, map the first m elements to n, . . ., n+m-1
;;;    I.e., 0, . . ., n-1 -> n, . . ., n+m-1, m, . . ., n-1
;;;
;;;    Example: 
;;;    Military clock 0, . . ., 11 -> 12, . . ., 11
;;;
;;;    This doesn't really belong here...
;;;    (format t "~D ~[~:[a.m.~;midnight~]~;~:[p.m.~;noon~]~]~%" (1+ (mod (+ hour 11) 12)) (truncate hour 12) (zerop (mod hour 12)))
;;;    
(defun shift-list0 (n m)
  (let ((list (loop for i from 0 to (1- n) collect i)))
    (mapcar #'(lambda (x) (shift0 n m x)) list)))

;;;
;;;    This screws up fractions!
;;;    
;; (defun slash-reader (stream char)
;;   (declare (ignore char))
;;   `(path . ,(loop for dir = (read-preserving-whitespace stream t nil t)
;;                   then (progn (read-char stream t nil t)
;;                               (read-preserving-whitespace stream t nil t))
;;                   collect dir
;;                   while (eql (peek-char nil stream nil nil t) #\/))))

;(set-macro-character #\/ #'slash-reader)

;; (defun cycle (f n l)
;;   (loop repeat n
;;         for elt in (make-circular-list l)
;;         collect (funcall f elt)))

;; (defun cycle (f n l)
;;   "Apply the function F to the elements of the list L repeatedly until N elements have been captured.
;;    The resulting list is returned."
;;   (let* ((vals (coerce l 'vector))
;;          (length (length vals)))
;;     (loop for i from 0 below n
;;           for elt = (aref vals (mod i length))
;;           collect (funcall f elt))))

(defun cycle (f n l)
  "Apply the function F to the elements of the list L repeatedly until N elements have been captured.
   The resulting list is returned."
  (loop repeat n
        for l1 = l then (if (endp (rest l1)) l (rest l1))
        collect (funcall f (first l1))))


;;;
;;;    This segregates _elements_ of SEQ not the _values_ of applying F.
;;;    (See FILTER above).
;;;    
(defun filter-split (f seq)
  (typecase seq
    (list (loop for elt in seq
                when (funcall f elt) collect elt into trues
                else collect elt into falses
                finally (return (list trues falses))))
    (vector (loop for elt across seq
                  when (funcall f elt) collect elt into trues
                  else collect elt into falses
                  finally (return (list (coerce trues 'vector)
                                        (coerce falses 'vector)))) )))

;; (defun filter-split (f seq)
;;   (let ((trues '())
;;         (falses '()))
;;     (map nil #'(lambda (elt) (if (funcall f elt) (push elt trues) (push elt falses))) seq)
;;     (list (nreverse trues) (nreverse falses))))

(defun firsts-rests (lol)
  "Traverse a list of lists and collect the first elements of each as well as the tails of each."
  (loop for list in lol
        until (null list)
        collect (first list) into firsts
        collect (rest list) into rests
        finally (return (values firsts rests))))

; (firsts-rests '((a b c) (1 2) (x))) => (A 1 X); ((B C) (2) NIL)
;;;
;;;    See ~/lisp/programs/mapping.lisp
;;;    
(defmacro traverse ((l (&rest lists)) process rest end)
  (let ((result (gensym)))
    `(do ((,result '() (cons ,process ,result))
          (,l ,lists ,rest))
         (,end (nreverse ,result)))) )

;;;
;;;    See notes pg. 739 regarding EVAL-WHEN
;;;    

;;;
;;;    Elts not evaluated
;;;    
;; (set-macro-character #\[ #'(lambda (stream char)
;;                              (declare (ignore char))
;;                              (apply #'vector (read-delimited-list #\] stream t))))

;;;
;;;    Vector syntax: [1 2 3]
;;;    ['a 'b 'c 'd]
;;;    [(+ 1 2) (- 9 4)]
;;;    
(set-macro-character #\[ #'(lambda (stream ch)
                             (declare (ignore ch))
                             `(vector ,@(read-delimited-list #\] stream t))))
(set-syntax-from-char #\] #\))

;;;
;;;    Hash table syntax: {"pung" 'foo "bar" 'baz}
;;;    
(set-macro-character #\{ #'(lambda (stream ch)
                             (declare (ignore ch))
                             (let ((table (gensym))
                                   (key (gensym))
                                   (value (gensym)))
                               `(let ((,table (make-hash-table :test #'equalp)))
                                  (dotuples ((,key ,value) (list ,@(read-delimited-list #\} stream t)))
                                    (setf (gethash ,key ,table) ,value))
                                  ,table))))

;;;
;;;    Capture the keys in order:
;;;    
;; (set-macro-character #\{ #'(lambda (stream ch)
;;                              (declare (ignore ch))
;;                              (let ((table (gensym))
;;                                    (keys (gensym))
;;                                    (key (gensym))
;;                                    (value (gensym)))
;;                                `(let ((,table (make-hash-table :test #'equalp))
;;                                       (,keys '()))
;;                                   (dotuples ((,key ,value) (list ,@(read-delimited-list #\} stream t)))
;;                                     (push ,key ,keys)
;;                                     (setf (gethash ,key ,table) ,value))
;;                                   (values ,table (nreverse ,keys)))) ))

(set-syntax-from-char #\} #\))

;;;
;;;    Range syntax: #[1 5] => (1 2 3 4 5)
;;;                  #[n] => (0 1 2 ... n-1)
;;;                  #[5 1] => (5 4 3 2 1)
;;;    New:
;;;    #[1 9 2] => (1 3 5 7 9)
;;;    #[#\a #\z] => (#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
;;;    #[#\a #\z 2] => (#\a #\c #\e #\g #\i #\k #\m #\o #\q #\s #\u #\w #\y)
;;;
;;;    In any case, reader returns an expression, which when evaluated yields a list:
;;;    (macroexpand-1 '#[1 5]) => '(1 2 3 4 5)
;;;    (macroexpand-1 '#[1 (1+ 4)]) => (LANG:MAKE-RANGE 1 (1+ 4))
;;;    
(set-dispatch-macro-character #\# #\[
  #'(lambda (stream ch arg)
      (declare (ignore ch arg))
      (destructuring-bind (m &optional n step) (read-delimited-list #\] stream t)
        (if step
            (if (and (numberp step)
                     (or (and (numberp m) (numberp n))
                         (and (characterp m) (characterp n))))
                `',(make-range m n step)
                `(make-range ,m ,n ,step))
            (if n
                (if (or (and (numberp m) (numberp n))
                        (and (characterp m) (characterp n)))
                    `',(make-range m n)
                    `(make-range ,m ,n))
                (if (or (numberp m) (characterp m))
                    `',(make-range m)
                    `(make-range ,m)))) )))

;; (set-dispatch-macro-character #\# #\[
;;   #'(lambda (stream ch arg)
;;       (declare (ignore ch arg))
;;       (destructuring-bind (m &optional n step) (read-delimited-list #\] stream t)
;;         (cond (step `(make-range ,m ,n ,step))
;;               (n `(make-range ,m ,n))
;;               (t `(make-range ,m)))) ))

;; (set-dispatch-macro-character #\# #\[
;;   #'(lambda (stream ch arg)
;;       (declare (ignore ch arg))
;;       (destructuring-bind (m &optional n) (read-delimited-list #\] stream t)
;;         (let ((i (gensym)))
;;           (cond ((null n) 
;;                  `(loop for ,i from 0 below ,m
;;                         collect ,i))
;;                 ((< m n)
;;                  `(loop for ,i from ,m upto ,n
;;                         collect ,i))
;;                 (t
;;                  `(loop for ,i from ,m downto ,n
;;                         collect ,i)))) )))

;;;
;;;    Set syntax: #{'a 'b 'c}
;;;    How to allow quoting entire set?
;;;    '#{1 2 3} => (MAKE-SET :TEST #'EQUALP :ELEMENTS (LIST 1 2 3))
;;;    '#.#{1 2 3} => #{1 2 3}
;;;    '#.#{a b c} => debugger invoked on a UNBOUND-VARIABLE: The variable A is unbound.
;;;    
(set-dispatch-macro-character #\# #\{
  #'(lambda (stream ch arg)
      (declare (ignore ch arg))
      `(make-set :test #'equalp :elements (list ,@(read-delimited-list #\} stream t)))) ) ; Should this be EQUALP?

(defmacro class-template (name (&rest superclasses) (&rest slots))
  `'(defclass ,name ,superclasses
     ,(mapcar #'(lambda (slot)
                  `(,slot :accessor ,slot :initarg ,(intern (symbol-name slot) :keyword)))
              slots)))

(defun macroexpand-all (expr &optional env)
  (print expr)
  (multiple-value-bind (expanded expandedp) (macroexpand-1 expr env)
    (cond (expandedp (macroexpand-all expanded env))
          ((atom expanded) expanded)
          (t (cons (first expanded)
                   (mapcar #'(lambda (expr) (macroexpand-all expr env)) (rest expanded)))) )))

;; (defun merge (l1 l2)
;;   (cond ((endp l1) l2)
;;         ((endp l2) l1)
;;         (t (destructure ((h1 . t1) l1
;;                          (h2 . t2) l2)
;;              (if (inorderp h1 h2)
;;                  (cons h1 (merge t1 l2))
;;                  (cons h2 (merge l1 t2)))) )))

(defmacro destructure ((&rest bindings) &body body)
  (cond ((null bindings) `(progn ,@body))
        (t (destructuring-bind (binding expression . rest) bindings
             (if (null rest)
                 `(destructuring-bind ,binding ,expression
                    ,@body)
                 `(destructuring-bind ,binding ,expression
                    (destructure ,rest
                                 ,@body)))) )))


;; (defun print-plist (sym)
;;   (do ((plist (symbol-plist sym) (cddr plist)))
;;       ((null plist))
;;     (format t "Property: ~S~%" (car plist))
;;     (format t "Value: ~S~%" (cadr plist))))

(defun print-plist (sym)
  (dotuples ((property value) (symbol-plist sym))
    (format t "Property: ~S~%" property)
    (format t "Value: ~S~%" value)))
            
;;;
;;;    Display the packages to which each of the symbols in a form belong.
;;;    
(defun analyze-tree (obj)
  (cond ((null obj) obj)
        ((symbolp obj) (intern (package-name (symbol-package obj))))
        ((atom obj) obj)
        (t (cons (analyze-tree (car obj))
                 (analyze-tree (cdr obj)))) ))      

(defmacro defchain (var vals)
  (let ((chain (mapcar #'list vals (append (rest vals) (list (first vals)))) ))
    `(case ,var ,@chain)))


(defun array-indices (a row-major-index)
  (assert (typep row-major-index `(integer 0 (,(array-total-size a))))
          (row-major-index)
          "The index ~A is invalid for the given array."
          row-major-index)
  (do ((dimensions (array-dimensions a) (rest dimensions))
       (index row-major-index (mod index (apply #'* (rest dimensions))))
       (indices '() (cons (floor index (apply #'* (rest dimensions))) indices)))
      ((null dimensions) (nreverse indices))))

;;;
;;;    Kent Pitman/Barry Margolin
;;;    
(defun copy-array (a)
  (adjust-array (make-array (array-dimensions a)
                            :displaced-to a
                            :element-type (array-element-type a))
                (array-dimensions a)
                :displaced-to nil))

(defun map-array (f a)
  (let ((result (copy-array a)))
    (dotimes (i (array-total-size result) result)
      (let ((elt (row-major-aref result i)))
        (setf (row-major-aref result i) (funcall f elt)))) ))

(defun map-array-index (f a)
  (let ((result (copy-array a)))
    (dotimes (i (array-total-size result) result)
      (let ((elt (row-major-aref result i))
            (indices (array-indices a i)))
        (setf (row-major-aref result i) (apply f elt indices)))) ))

(defun make-identity-matrix (n)
  (map-array-index #'(lambda (elt i j)
                       (declare (ignore elt))
                       (if (= i j)
                           1d0
                           0d0))
                   (make-array (list n n))))

(defun sort-symbol-list (symbols)
  (sort (copy-list symbols) #'string-lessp :key #'symbol-name))

;; (defun sort-symbol-list (symbols)
;;   (sort (copy-list symbols) #'(lambda (s1 s2) (string-lessp (symbol-name s1) (symbol-name s2)))) )

;;;
;;;    Seibel pg. 101
;;;
;; (defmacro with-gensyms ((&rest names) &body body)
;;   `(let ,(loop for n in names
;;                collect `(,n (gensym)))
;;      ,@body))

;;;
;;;    Seibel's downloadable code is slightly different:
;;;    
;; (defmacro with-gensyms ((&rest names) &body body)
;;   `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
;;      ,@body))

;; (defmacro with-gensyms ((&rest names) &body body)
;;   `(let ,(loop for n in names
;;                collect `(,n (make-symbol ,(symbol-name n))))
;;      ,@body))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names
               collect `(,n ',(make-symbol (symbol-name n)))) ; ??
     ,@body))

;;;
;;;    http://www.amazon.com/Data-Structures-Algorithms-Made-Easy/dp/1466304162
;;;    Problem #2: Find the nth node from the end of the list
;;;    
(defun nth-from-end (n l)
  (do ((l1 l (rest l1))
       (l2 l)
       (i 0 (1+ i)))
      ((null l1) (if (>= i n) l2 nil))
    (when (>= i n)
      (setf l2 (rest l2)))) )

(defun partition (l)
  "Partition a list into sublists of the 'odd' and 'even' elements. The corresponding elements will always be grouped together, however, which sublist is considered 'odd' and which 'even' depends on the number of elements
   in the original list."
  (labels ((partition-aux (l odds evens)
             (if (endp l)
                 (values odds evens)
                 (partition-aux (rest l) evens (cons (first l) odds)))) )
    (partition-aux l '() '())))

(defun stable-partition (l)
  "Partition a list into sublists of the 'odd' and 'even' elements. The 'odd' and 'even' sublists always reflect the elements' positions (odd or even) in the original list."
  (labels ((partition-odd (l odds evens)
             (if (endp l)
                 (values odds evens)
                 (partition-even (rest l) (cons (first l) odds) evens)))
           (partition-even (l odds evens)
             (if (endp l)
                 (values odds evens)
                 (partition-odd (rest l) odds (cons (first l) evens)))) )
    (partition-odd l '() '())))

(defun stable-stream-partition (stream)
  "Partition elements of a character stream into sublists of 'odd' and 'even' elements."
  (labels ((partition-odd (odds evens)
             (let ((ch (read-char stream nil)))
               (if (null ch)
                   (values odds evens)
                   (partition-even (cons ch odds) evens))))
           (partition-even (odds evens)
             (let ((ch (read-char stream nil)))
               (if (null ch)
                   (values odds evens)
                   (partition-odd odds (cons ch evens)))) ))
    (partition-odd '() '())))

(defun approximately= (a b &optional (epsilon 1d-6))
  (<= (abs (- a b)) (* epsilon (abs a))))

;;;
;;;    See programs/horners.lisp
;;;    
(defun horners (x coefficients)
  (reduce #'(lambda (a b) (+ (* a x) b)) coefficients :initial-value 0))
