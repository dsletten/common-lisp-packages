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
  (:export :after :append1 :approximately= :array-indices :before :best :best-worst
           :class-template :compose :conc1 :copy-array :cycle
           :defchain :destructure :dohash :doset :dostring :dotuples :dovector :drop :duplicate
           :ends-with :expand :explode
           :fif :filter :filter-split :find-some-if :find-subtree :fint :firsts-rests :flatten :fun
           :get-num :group :high-low :high-low-n :horners
	   :is-integer
           :last1 :list-to-string :longerp
           :macroexpand-all :make-identity-matrix :make-range
           :map-> :map-array :map-array-index :map0-n :map1-n :mapa-b :mapcars :mappend :mapset
           :memoize :mklist :mkstr :most :mostn
           :ppmx :prefixp :print-plist :prompt :prompt-read :prune-if :prune-if-not
           :read-num :repeat :reread :rmapcar 
           :rotate0 :rotate-list0 :rotate1 :rotate-list1
           :same-shape-tree-p
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

;;;
;;;   This version simply returns all elements if n > length.
;;;   -Trying to avoid calling LENGTH on list...
;;;   
(defun take (n seq)
  "Take the first N elements of sequence SEQ."
  (typecase seq
    (list (loop repeat n
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

;;;
;;;    Using queue since LOOP semantics are so goofy!!
;;;    See SPLIT-IF
(defun take-drop (n seq)
  "Split a sequence at the Nth element. Return the subsequences before and after."
  (assert (typep n `(integer 0))
          (n)
          "N must be a non-negative integer.")
  (typecase seq
    (list (do ((q (make-linked-queue))
               (tail seq (rest tail))
               (i 0 (1+ i)))
              ((or (= i n) (endp tail)) (values (elements q) tail))
            (enqueue q (first tail))))
    (vector (values (take n seq) (drop n seq)))) ) ; Works for strings too

;;;
;;;    Why is this so hard to get right?!?
;;;    - Problem cases: N = 0, N > length
;;;    (Inconsistency with CLISP???)
;;;    
;; (defun take-drop (n seq)
;;   "Split a sequence at the Nth element. Return the subsequences before and after."
;;   (assert (typep n `(integer 0))
;;           (n)
;;           "N must be a non-negative integer.")
;;   (typecase seq
;;     (list (if (zerop n) ;<-- Unnecessary when using queue explicitly. Only needed with LOOP version?
;;               (values '() seq)
;;               (loop for i from 0 below n
;;                     for elt in seq
;;                     for tail on seq
;;                     collect elt into q
;;                     do (print (list i elt q tail))
;;                     finally (return (values q #- :clisp (rest tail) #+ :clisp tail))) ))
;;     (vector (values (take n seq) (drop n seq)))) ) ; Works for strings too

;; (defun take-drop (n seq)
;;   (loop repeat n
;;         for elt in seq
;;         for tail on (rest seq)
;;         collect elt into take
;;         finally (return (values take tail))))

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


;; (defun empty-seq (in)
;;   (map (type-of in) #'identity in))

;; (defun empty-seq (in)
;;   (make-sequence (type-of in) 0))

(defun empty-seq (in)
  (typecase in
    (list (list))
    (string (make-string 0))
    (vector (vector))))

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

;;;
;;;    This collects the _values_ of applying the function to elts, not
;;;    the elts themselves. Different from REMOVE-IF-NOT!
;;;
;;;    More efficient version of: (mapcar #'f (remove-if-not #'f seq))
;;;    FILTER uses the result of applying F to determine which elements to keep.
;;;    
(defun filter (f seq)
  (typecase seq
    (list (loop for elt in seq
                for val = (funcall f elt)
                when val collect val))
    (vector (loop for elt across seq
                  for val = (funcall f elt)
                  when val collect val into result
                  finally (return (coerce result (if (stringp seq) 'string 'vector)))) )))

;; (defun filter (f seq)
;;   (labels ((filter-seq ()
;;              (let ((result '()))
;;                (map nil #'(lambda (elt)
;;                             (let ((val (funcall f elt)))
;;                               (when val (push val result))))
;;                     seq)
;;                (nreverse result))))
;;     (typecase seq
;;       (list (filter-seq))
;;       (string (coerce (filter-seq) 'string))
;;       (vector (coerce (filter-seq) 'vector)))) )

;; (defun filter (fn list)
;;   (let ((acc '()))
;;     (dolist (elt list (nreverse acc))
;;       (let ((val (funcall fn elt)))
;; 	(when val (push val acc)))) ))

;;;
;;;    Conventional FILTER below. Not Graham's.
;;;    Collect elts that pass test.
;;;    
;; (defun filter (fn list)
;;   (let ((acc '()))
;;     (dolist (elt list (nreverse acc))
;;       (when (funcall fn elt) (push elt acc)))) )

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

;;;
;;;    See Clojure partition-all
;;;    
(defun group (seq n)
  (flet ((emptyp (seq)
           (typecase seq
             (list (null seq))
             (vector (zerop (length seq)))) ))
    (loop for take-drop =      (multiple-value-list (take-drop n seq))
                          then (multiple-value-list (take-drop n (second take-drop)))
          until (emptyp (first take-drop))
          collect (first take-drop))))

(defun group-until (f seq)
  "Group elements of SEQ into a subsequence until F returns true for that subsequence, then start next subsequence."
  (let ((groups (reduce #'(lambda (groups elt)
                            (destructuring-bind (current . result) groups
                              (if (funcall f (cons elt current))
                                  (cons (list elt) (cons (nreverse current) result))
                                  (cons (cons elt current) result))))
                        seq
                        :initial-value (cons '() '()))) )
    (destructuring-bind (current . result) groups
      (nreverse (cons (nreverse current) result)))) )

;;;
;;;    This is not tail-recursive, so it will fail with a large enough (?!) tree.
;;;    (length (flatten (loop repeat 100000 collect t))) => 100000
;; INFO: Control stack guard page unprotected
;; Control stack guard page temporarily disabled: proceed with caution

;; debugger invoked on a SB-KERNEL::CONTROL-STACK-EXHAUSTED in thread
;; #<THREAD "main thread" RUNNING {10005204C3}>:
;;   Control stack exhausted (no more space for function call frames).
;; This is probably due to heavily nested or infinitely recursive function
;; calls, or a tail call that SBCL cannot or has not optimized away.

;;;
;;;    Third version:
;;;    (length (flatten (loop repeat 100000 collect t))) => 100000
;;;    
;;;    But it is substantially faster than the third version since that one CONSes a lot more.
;;;    (Although that one is pretty fast too...)
;;;    
;; (defun flatten (l)
;;   (labels ((flatten-aux (l result)
;;              (cond ((endp l) result)
;;                    ((listp (first l)) (flatten-aux (first l) (flatten-aux (rest l) result)))
;;                    (t (cons (first l) (flatten-aux (rest l) result)))) ))
;;     (flatten-aux l '())))

;;;
;;;    Also not tail-recursive. Almost as fast as previous one. (Same as On Lisp version.)
;;;    
;; (defun flatten (obj)
;;   (labels ((flatten-aux (obj results)
;;              (cond ((null obj) results)
;;                    ((atom obj) (cons obj results))
;;                    (t (flatten-aux (car obj)
;;                                    (flatten-aux (cdr obj) results)))) ))
;;     (flatten-aux obj '()))) 

;; (defun flatten (tree)
;;   (labels ((flatten-aux (tree result)
;;              (cond ((null tree) (nreverse result))
;;                    ((null (car tree)) (flatten-aux (cdr tree) result))
;;                    ((atom (car tree)) (flatten-aux (cdr tree) (cons (car tree) result)))
;;                    (t (flatten-aux (list* (caar tree) (cdar tree) (cdr tree)) result)))) )
;;     (if (atom tree)
;;         tree
;;         (flatten-aux tree '()))) )

(defun flatten (tree)
  (labels ((flatten-aux (tree result)
             (cond ((null tree) (nreverse result))
                   ((null (car tree)) (flatten-aux (cdr tree) result))
                   ((atom (car tree)) (flatten-aux (cdr tree) (cons (car tree) result)))
                   (t (destructuring-bind ((head . tail1) . tail2) tree
                        (flatten-aux (list* head tail1 tail2) result)))) ))
    (if (atom tree)
        tree
        (flatten-aux tree '()))) )

;; See prune.lisp
(defun prune-if (pred tree)
  "Remove all leaves of TREE for which PRED is true. Like REMOVE-IF for trees."
  (labels ((prune-aux (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((atom (car tree)) (prune-aux (cdr tree)
                                                (if (funcall pred (car tree))
                                                    acc
                                                    (cons (car tree) acc))))
                   (t (prune-aux (cdr tree)
                                 (cons (prune-aux (car tree) '()) acc)))) ))
    (prune-aux tree '())))

(defun prune-if-not (pred tree)
  "Remove all leaves of TREE for which PRED is not true. Like REMOVE-IF-NOT for trees."
  (prune-if (complement pred) tree))

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

(defun find-some-if (f seq)
  (typecase seq
    (list (loop for elt in seq
                for val = (funcall f elt)
                when val
                return (values elt val)))
    (vector (loop for elt across seq
                  for val = (funcall f elt)
                  when val
                  return (values elt val)))) )

;;;
;;;    Does X occur before Y in list?
;;;    Y need not actually occur in the list! (See AFTER)
;;;    
;; (defun before (x y list &key (test #'eql))
;;   (and list
;;        (let ((first (car list)))
;; 	 (cond ((funcall test y first) nil)
;; 	       ((funcall test x first) list)
;; 	       (t (before x y (cdr list) :test test)))) ))

;;    Do we reach X _before_ we see Y?
(defun before (x y seq &key (test #'eql))
  "Does X occur before Y in SEQ? This is true whenever X occurs without Y having yet been encountered, 
i.e., even if Y is not actually in the sequence. For a positive result, returns the rest of the list or
the index of the position immediately following X."
  (typecase seq
    (null nil)
    (list (loop for (elt . rest) on seq
                when (funcall test y elt) return nil
                when (funcall test x elt) return rest))
    (vector (loop for elt across seq
                  for i from 1
                  when (funcall test y elt) return nil
                  when (funcall test x elt) return i))))

;;    Do we reach X only _after_ we see Y?
(defun after (x y seq &key (test #'eql))
  "Does X occur after Y in SEQ? X must explicitly and exclusively be present after Y."
  (let ((rest (before y x seq :test test)))
    (if (null rest)
        nil
        (typecase seq
          (list (member x rest :test test))
          (vector (position x seq :start rest :test test)))) ))

(defun duplicate (obj seq &key (test #'eql))
  "Are there duplicate instances of OBJ in SEQ as determined by TEST? If so return the tail of the list starting with the duplicate or the index in the sequence of the duplicate."
  (typecase seq
    (list (member obj (rest (member obj seq :test test)) :test test))
    (vector (let ((initial (position obj seq :test test)))
              (if initial
                  (position obj seq :start (1+ initial) :test test)
                  nil)))) )

;;;
;;;    See TAKE-DROP
;;;    (This is sort of DROP-WHILE/TAKE-WHILE)
;;;    Or MEMBER-IF that returns both parts of sequence.
;;;    
(defun split-if (f seq)
  "Split a sequence into the initial subsequence of all elements that fail the given test and the remaining subsequence from the first element which passes the test."
  (typecase seq
    (list (do ((q (make-linked-queue))
               (tail seq (rest tail)))
              ((or (endp tail) (funcall f (first tail))) (values (elements q) tail))
            (enqueue q (first tail))))
    (vector (let ((initial (or (position-if f seq) 0)))
              (values (take initial seq) (drop initial seq)))) ))

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

;;;
;;;    This works, but it CONSes a lot!
;;;    
(defun most (f seq)
  "Locate the first element in SEQ that yields the highest value when F is applied. The secondary value is the value returned by F for that element."
  (let ((first (elt seq 0)))
    (values-list (reduce #'(lambda (winner elt)
                             (let ((score (funcall f elt)))
                               (if (> score (second winner))
                                   (list elt score)
                                   winner)))
                         seq
                         :initial-value (list first (funcall f first)))) ))

;;;
;;;    Not totally FP!!
;;;    
(defun most (f seq)
  "Locate the first element in SEQ that yields the highest value when F is applied. The secondary value is the value returned by F for that element."
  (let* ((winner (elt seq 0))
         (max (reduce #'(lambda (max elt)
                          (let ((score (funcall f elt)))
                            (cond ((> score max) (setf winner elt) score)
                                  (t max))))
                      seq
                      :initial-value (funcall f winner))))
    (values winner max)))

(defun most (f seq)
  (labels ((most-list (seq winner max)
             (if (endp seq)
                 (values winner max)
                 (let* ((elt (first seq))
                        (score (funcall f elt)))
                   (if (> score max)
                       (most-list (rest seq) elt score)
                       (most-list (rest seq) winner max)))) )
           (most-vector (i winner max)
             (if (= i (length seq))
                 (values winner max)
                 (let* ((elt (elt seq i))
                        (score (funcall f elt)))
                   (if (> score max)
                       (most-vector (1+ i) elt score)
                       (most-vector (1+ i) winner max)))) ))
    (typecase seq
      (list (if (null seq)
                nil
                (most-list (rest seq) (first seq) (funcall f (first seq)))) )
      (vector (if (zerop (length seq))
                  nil
                  (most-vector 1 (elt seq 0) (funcall f (elt seq 0)))) ))))

(defun most (f seq)
  "Locate the first element in SEQ that yields the highest value when F is applied. The secondary value is the value returned by F for that element."
  (typecase seq
    (list (if (null seq)
              nil
              (loop with winner = (first seq)
                    with max = (funcall f winner)
                    for elt in (rest seq)
                    for score = (funcall f elt)
                    when (> score max) do (setf winner elt max score)
                    finally (return (values winner max)))) )
    (vector (if (zerop (length seq))
                nil
                (loop with winner = (elt seq 0)
                      with max = (funcall f winner)
                      for i from 1 below (length seq)
                      for elt = (elt seq i)
                      for score = (funcall f elt)
                      when (> score max) do (setf winner elt max score)
                      finally (return (values winner max)))) )))

;;;
;;;    These are nice but they don't handle capturing the element itself...
;;;    
;; (defun most (f seq)
;;   (reduce #'(lambda (winner elt)
;;               (let ((score (funcall f elt)))
;;                 (if (> score winner)
;;                     score
;;                     winner)))
;;           seq
;;           :initial-value (funcall f (elt seq 0))))

;; (defun most (f seq)
;;   (typecase seq
;;     (list (loop for elt in seq maximize (funcall f elt)))
;;     (vector (loop for elt across seq maximize (funcall f elt)))) )

;;;
;;; Compare MOST
;;;
(defun high-low (f seq)
  (labels ((high-low-list (seq winner max loser min)
             (if (endp seq)
                 (values winner max loser min)
                 (let* ((elt (first seq))
                        (score (funcall f elt)))
                   (cond ((> score max) (high-low-list (rest seq) elt score loser min))
                         ((< score min) (high-low-list (rest seq) winner max elt score))
                         (t (high-low-list (rest seq) winner max loser min)))) ))
           (high-low-vector (i winner max loser min)
             (if (= i (length seq))
                 (values winner max loser min)
                 (let* ((elt (elt seq i))
                        (score (funcall f elt)))
                   (cond ((> score max) (high-low-vector (1+ i) elt score loser min))
                         ((< score min) (high-low-vector (1+ i) winner max elt score))
                         (t (high-low-vector (1+ i) winner max loser min)))) )))
    (typecase seq
      (list (if (null seq)
                nil
                (let ((starter (funcall f (first seq))))
                  (high-low-list (rest seq) (first seq) starter (first seq) starter))))
      (vector (if (zerop (length seq))
                  nil
                  (let ((starter (funcall f (elt seq 0))))
                    (high-low-vector 1 (elt seq 0) starter (elt seq 0) starter)))) )))

(defun high-low (f seq)
  "Locate the first elements in SEQ that yield the highest/lowest value when F is applied. The values returned by F for those elements are also returned."
  (typecase seq
    (list (if (null seq)
              nil
              (loop with winner = (first seq)
                    with loser = winner
                    with max = (funcall f winner)
                    with min = max
                    for elt in (rest seq)
                    for score = (funcall f elt)
                    when (> score max) do (setf winner elt max score)
                    else when (< score min) do (setf loser elt min score)
                    finally (return (values winner max loser min)))) )
    (vector (if (zerop (length seq))
                nil
                (loop with winner = (elt seq 0)
                      with loser = winner
                      with max = (funcall f winner)
                      with min = max
                      for i from 1 below (length seq)
                      for elt = (elt seq i)
                      for score = (funcall f elt)
                      when (> score max) do (setf winner elt max score)
                      else when (< score min) do (setf loser elt min score)
                      finally (return (values winner max loser min)))) )))

(defun best (fn list)
  (if (null list)
      nil
      (let ((wins (car list)))
        (dolist (obj (cdr list))
          (when (funcall fn obj wins)
	    (setq wins obj)))
        wins)))

(defun best (f seq)
  ""
  (typecase seq
    (list (if (null seq)
              nil
              (loop with winner = (first seq)
                    for elt in (rest seq)
                    when (funcall f elt winner) do (setf winner elt)
                    finally (return winner))))
    (vector (if (zerop (length seq))
                nil
                (loop with winner = (elt seq 0)
                      for i from 1 below (length seq)
                      for elt = (elt seq i)
                      when (funcall f elt winner) do (setf winner elt)
                      finally (return winner)))) ))

(defun best (f seq)
  "Return the first element as if the elements of SEQ were sorted by means of F."
  (reduce #'(lambda (winner elt)
              (if (funcall f elt winner)
                  elt
                  winner))
          seq))

;;;
;;;    CONSes!
;;;    
(defun best-worst (f seq)
  "Return the first and last elements as if the elements of SEQ were sorted by means of F."
  (let ((first (elt seq 0))) ; Assumes not empty?!
    (values-list (reduce #'(lambda (winner elt)
                             (destructuring-bind (max min) winner
                               (cond ((funcall f elt max) (list elt min))
                                     ((funcall f min elt) (list max elt))
                                     (t winner))))
                         seq
                         :initial-value (list first first)))) )

(defun best-worst (f seq)
  "Return the first and last elements as if the elements of SEQ were sorted by means of F."
  (typecase seq
    (list (if (null seq)
              nil
              (loop with winner = (first seq)
                    with loser = winner
                    for elt in (rest seq)
                    when (funcall f elt winner) do (setf winner elt)
                    else when (funcall f loser elt) do (setf loser elt)
                    finally (return (values winner loser)))) )
    (vector (if (zerop (length seq))
                nil
                (loop with winner = (elt seq 0)
                      with loser = winner
                      for i from 1 below (length seq)
                      for elt = (elt seq i)
                      when (funcall f elt winner) do (setf winner elt)
                      else when (funcall f loser elt) do (setf loser elt)
                      finally (return (values winner loser)))) )))

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

(defun mostn (f seq)
  (let* ((first (elt seq 0))
         (results (reduce #'(lambda (winner elt)
                              (let ((score (funcall f elt)))
                                (destructuring-bind (winners high-score) winner
                                  (cond ((> score high-score) (list (list elt) score))
                                        ((= score high-score) (list (cons elt winners) high-score))
                                        (t winner)))) )
                          seq
                          :initial-value (list '() (funcall f first)))) )
    (values (nreverse (first results)) (second results))))

(defun mostn (f seq)
  "Locate all elements in SEQ that yield the highest value when F is applied. The secondary value is the value returned by F for those elements."
  (typecase seq
    (list (if (null seq)
              nil
              (loop with winners = (make-linked-queue)
                    with max = (funcall f (first seq))
                    for elt in seq
                    for score = (funcall f elt)
                    if (= score max)
                      do (enqueue winners elt)
                    else if (> score max)
                      do (make-empty winners)
                         (enqueue winners elt)
                         (setf max score)
                    end
                    finally (return (values (elements winners) max)))) )
    (vector (if (zerop (length seq))
                nil
                (loop with winners = (make-linked-queue)
                      with max = (funcall f (elt seq 0))
                      for elt across seq
                      for score = (funcall f elt)
                      if (= score max)
                        do (enqueue winners elt)
                      else if (> score max)
                        do (make-empty winners)
                           (enqueue winners elt)
                           (setf max score)
                      end
                      finally (return (values (elements winners) max)))) )))

(defun high-low-n (f seq)
  "Locate all elements in SEQ that yield the highest/lowest value when F is applied. The values returned by F for those elements are also returned."
  (typecase seq
    (list (if (null seq)
              nil
              (loop with winners = (make-linked-queue)
                    with losers = (make-linked-queue)
                    with max = (funcall f (first seq))
                    with min = max
                    for elt in seq
                    for score = (funcall f elt)
                    if (= score max)
                      do (enqueue winners elt)
                    else if (> score max)
                      do (make-empty winners)
                         (enqueue winners elt)
                         (setf max score)
                    else if (= score min)
                      do (enqueue losers elt)
                    else if (< score min)
                      do (make-empty losers)
                         (enqueue losers elt)
                         (setf min score)
                    end
                    finally (return (values (elements winners) max (elements losers) min)))) )
    (vector (if (zerop (length seq))
                nil
                (loop with winners = (make-linked-queue)
                      with losers = (make-linked-queue)
                      with max = (funcall f (elt seq 0))
                      with min = max
                      for elt across seq
                      for score = (funcall f elt)
                      if (= score max)
                        do (enqueue winners elt)
                      else if (> score max)
                        do (make-empty winners)
                           (enqueue winners elt)
                           (setf max score)
                      else if (= score min)
                        do (enqueue losers elt)
                      else if (< score min)
                        do (make-empty losers)
                           (enqueue losers elt)
                           (setf min score)
                      end
                      finally (return (values (elements winners) max (elements losers) min)))) )))

;;;
;;;    PAIP pg. 76
;;;    
(defun same-shape-tree-p (a b)
  "Do trees A and B have the same structure even if values are different?"
  (tree-equal a b :test (constantly t)))

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

;; (defun mapa-b (f a b &optional (step 1))
;;   (loop for i from a to b by step
;;         collect (funcall f i)))

;; (defun mapa-b (f a b &optional (step 1))
;;   (if (plusp step)
;;       (loop for i from a to b by step collect (funcall f i))
;;       (loop for i from a downto b by (abs step) collect (funcall f i))))

(defun mapa-b (f a b &optional (step 1))
  (assert (plusp step) () "Step increment must be positive.")
  (if (<= a b)
      (loop for i from a to b by step collect (funcall f i))
      (loop for i from a downto b by step collect (funcall f i))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

;;;
;;;
;; (defun make-range (start &optional end (step 1))
;;   (cond ((characterp start)
;;          (cond ((characterp end)
;;                 (mapcar #'code-char (make-range (char-code start)
;;                                                 (char-code end)
;;                                                 step)))
;;                ;;
;;                ;;    Does it make sense for 1-arg with character?
;;                ((null end) (mapcar #'code-char (make-range 0
;;                                                            (1- (char-code start))
;;                                                            step)))
;;                (t (error "Mismatched input types."))))
;;         ((numberp start)
;;          (cond ((numberp end)
;;                 (if (> start end)
;;                     (loop for i from start downto end by step collect i)
;;                     (loop for i from start to end by step collect i)))
;;                ((null end)
;;                 (loop for i from 0 below start by step collect i))
;;                (t (error "Mismatched input types.")))) ))

(defun make-range (start &optional end (step 1))
  (etypecase start
    (character (typecase end
                 (character
                   (if (char> start end)
                       (loop for i from (char-code start) downto (char-code end) by step collect (code-char i))
                       (loop for i from (char-code start) to (char-code end) by step collect (code-char i))))
                 (null (loop for i from 0 below (char-code start) by step collect (code-char i)))
                 (otherwise (error "Mismatched input types."))))
    (number (typecase end
              (number (if (> start end)
                          (loop for i from start downto end by step collect i)
                          (loop for i from start to end by step collect i)))
              (null (loop for i from 0 below start by step collect i))
              (otherwise (error "Mismatched input types.")))) ))

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

(defun map-> (f start test step)
  "Collect repeated application of the function F to START as it is transformed by the STEP function until TEST returns true."
  (loop for obj = start then (funcall step obj)
        until (funcall test obj)
        collect (funcall f obj)))

;;;
;;;    See Clojure iterate.
;;;    start, (f start), (f (f start)), ...
;;;    
(defun iterate (f start)
  #'(lambda ()
      (prog1 start
        (setf start (funcall f start)))) )

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result '()))
    (dolist (l lsts)
      (dolist (obj l)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun mapcars (f &rest lists)
  "Map the function F over each element of each list argument provided."
  (loop with result = (make-linked-queue)
        for list in lists
        do (loop for elt in list
                 do (enqueue result (funcall f elt)))
        finally (return (elements result))))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar #'(lambda (&rest args)
			  (apply #'rmapcar fn args))
	     args)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar #'(lambda (&rest car-args)
			  (apply #'rmapcar fn car-args))
	     args)))

;;;    MAPCAR all args are same length...or terminate when shortest is consumed. Analog for trees?

;;;
;;;    Graham describes RMAPCAR as "a version of MAPCAR for trees". The name
;;;    is short for "recursive MAPCAR". What MAPCAR does on flat lists, it does on trees.
;;;
;;;    The simplest idea would be to map a function F over a single TREE. The mapping
;;;    function would traverse deeper into TREE until it reached the leaves and then
;;;    apply F to the values of the leaves. But what are "the leaves"? In this simple
;;;    case it's just a case of recursively calling the mapping function with objects
;;;    that are not themselves CONSes, i.e., atoms.
;;;
;;;    But we still have to potentially deal with dotted pairs of non-NULL atoms. There
;;;    are 2 choices here:
;;;    1. Have the mapping function deal with NULL as a special terminator and only apply
;;;       F to other non-NULL atoms.
;;;
;; (defun tree-map (f tree)
;;   (cond ((null tree) tree)
;;         ((atom tree) (funcall f tree))
;;         (t (cons (tree-map f (car tree))
;;                  (tree-map f (cdr tree)))) ))

;; (tree-map  #'1+ '(1 2 (3 4 (5) 6) 7 (8 9))) => (2 3 (4 5 (6) 7) 8 (9 10))
;; (tree-map  #'oddp '(1 2 (3 4 (5) 6) 7 (8 9))) => (T NIL (T NIL (T) NIL) T (NIL T))
;; (tree-map  #'1+ '(1 2 (3 4 (5 . 4.2) 6 . -1) 7 (8 9 . 12))) => (2 3 (4 5 (6 . 5.2) 7 . 0) 8 (9 10 . 13))
;; (tree-map #'string-upcase '("is" ("this" (("not") "pung?")))) => ("IS" ("THIS" (("NOT") "PUNG?")))
;; (tree-map #'length '("is" ("this" (("not") "pung?")))) => (2 (4 ((3) 5)))
;;;
;;;    2. A less obvious choice would be to call F for any atoms encountered. F could
;;;       decide what to do with NIL.
;; (defun tree-map (f tree)
;;   (if (atom tree)
;;       (funcall f tree)
;;       (cons (tree-map f (car tree))
;;             (tree-map f (cdr tree)))) )

;; (tree-map  #'(lambda (elt) (if (null elt) elt (1+ elt))) '(1 2 (3 4 (5) 6) 7 (8 9))) => (2 3 (4 5 (6) 7) 8 (9 10))
;; (tree-map  #'(lambda (elt) (if (null elt) elt (oddp elt))) '(1 2 (3 4 (5) 6) 7 (8 9))) => (T NIL (T NIL (T) NIL) T (NIL T))
;; (tree-map #'(lambda (elt) (if (null elt) elt (string-upcase elt))) '("is" ("this" (("not") "pung?")))) => ("IS" ("THIS" (("NOT") "PUNG?")))
;; (tree-map #'(lambda (elt) (if (null elt) elt (length elt))) '("is" ("this" (("not") "pung?")))) => (2 (4 ((3) 5)))
;;;
;;;    It seems like this 2nd approach is not as useful...
;;;    
;;;    But to truly make this function like MAPCAR, it must be able to traverse 1+ trees simultaneously.
;;;    When provided a single tree, it should behave as above. With multiple trees, the simplest case is
;;;    where both trees have identical shapes and the leaves of each provide one of N args to F.
;; (defun tree-map (f &rest trees)
;;   (multiple-value-bind (cars cdrs) (strip-trees trees)
;;     (cond ((every #'null cars) '())
;;           ((every #'atom cars) (cons (apply f cars) (apply #'tree-map f cdrs)))
;;           (t (cons (apply #'tree-map f cars)
;;                    (apply #'tree-map f cdrs)))) ))

(defun tree-map-a (f &rest trees)
  (cond ((every #'null trees) '())
        ((every #'atom trees) (apply f trees)) ; Too strict to allow different depths.
;        (t (multiple-value-bind (cars cdrs) (strip-trees trees)
        (t (multiple-value-bind (cars cdrs) (firsts-rests trees)
             (cons (apply #'tree-map-a f cars)
                   (apply #'tree-map-a f cdrs)))) ))

;;;
;;;    This version is adequate to handle multiple trees of identical structure. The mapped function F may
;;;    have to be modified depending on the number of trees...
;;;    
(defun tree-map-b (f &rest trees)
  (cond ((every #'null trees) '())
        ((some #'atom trees) (apply f trees))
;        (t (multiple-value-bind (cars cdrs) (strip-trees trees)
        (t (multiple-value-bind (cars cdrs) (firsts-rests trees)
             (cons (apply #'tree-map-b f cars)
                   (apply #'tree-map-b f cdrs)))) ))

;; (tree-map #'1+ '(1 2 (3 4 (5) 6) 7 (8 9))) => (2 3 (4 5 (6) 7) 8 (9 10))
;; (tree-map #'(lambda (s1 s2) (concatenate 'string s1 s2)) '("Is" ("this" ("not" ("pung?")))) '("Ça" ("plane" ("pour" ("moi"))))) => ("IsÇa" ("thisplane" ("notpour" ("pung?moi"))))
;; (tree-map #'(lambda (&rest strings) (apply #'concatenate 'string strings)) '("Is" ("this" ("not" ("pung?")))) '("Ça" ("plane" ("pour" ("moi")))) '("a" ("b" ("c" ("d"))))) => ("IsÇaa" ("thisplaneb" ("notpourc" ("pung?moid"))))

;;;
;;;    It is similar in behavior to Graham's RMAPCAR when the trees are compatible. This appears to mean that depths can vary, but
;;;    lengths at a given depth must be consistent. (Graham gets around this via MAPCAR, which simply stops with shortest sublist...)
;;;
;; (tree-map #'cons '(a b c) '((d e) (f g) (h i))) => ((A D E) (B F G) (C H I))
;; (tree-map #'cons '((a) (b) (c)) '((d e) (f g) (h i))) => (((A . D) NIL E) ((B . F) NIL G) ((C . H) NIL I))  ; This one is weird
;; (tree-map #'cons '(a (b c)) '((d e) ((f g) (h i)))) => ((A D E) ((B F G) (C H I)))
;; (tree-map #'cons '(a b c) '((1) (2 3) (4 5 6))) => ((A 1) (B 2 3) (C 4 5 6))

;;;
;;;    The behavior is wrong, however, when the lengths don't match up:
;;;
;; (tree-map #'(lambda (s1 s2) (concatenate 'string s1 s2)) '("Is" ("this" ("not" ("pung?")))) '("Ça" ("plane" ("pour" ("moi" "Plastic" "Bertrand")))))

;; debugger invoked on a TYPE-ERROR in thread
;; #<THREAD "main thread" RUNNING {10005204C3}>:
;;   The value
;;     "Plastic"
;;   is not of type
;;     CHARACTER
;;   when setting an element of (ARRAY CHARACTER)

;;;
;;;    Same as Graham???
;;;    
;(defun tree-map-c (f &rest trees)
(defun tree-map (f &rest trees)
  (cond ((null trees) '()) ; Pathological initial case: (tree-map-c f). Also recursive case where trees are not same size.
        ((some #'null trees) '())
        ((some #'atom trees) (apply f trees))
;        (t (multiple-value-bind (cars cdrs) (strip-trees trees)
        (t (multiple-value-bind (cars cdrs) (firsts-rests trees)
             (cons (apply #'tree-map f cars)
                   (apply #'tree-map f cdrs)))) ))


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

;;;
;;;    I wrote these without realizing I already had FIRSTS-RESTS...
;; (defun strip-trees (trees)
;;   (labels ((strip (trees cars cdrs)
;;              (if (endp trees)
;;                  (values (nreverse cars) (nreverse cdrs))
;;                  (strip (rest trees) (cons (first (first trees)) cars) (cons (rest (first trees)) cdrs)))) )
;;     (strip trees '() '())))

(defun strip-trees (trees)
  (loop for cons in trees ; Not necessarily CONS! If unequal lengths...
        collect (car cons) into cars
        collect (cdr cons) into cdrs
        finally (return (values cars cdrs))))


;; (defun firsts-rests (lol)
;;   "Traverse a list of lists and collect the first elements of each as well as the tails of each."
;;   (loop for list in lol
;;         until (null list)
;;         collect (first list) into firsts
;;         collect (rest list) into rests
;;         finally (return (values firsts rests))))

;;;
;;;    Behavior is similar to MAPCAR. Stops collecting tails as soon as any sublist is exhausted.
;;;    
(defun firsts-rests (lol)
  "Traverse LOL, a list of lists, and collect the first elements of each as well as the tails of each."
  (labels ((collect-heads-tails (lol heads tails)
             (cond ((null lol) (values (elements heads) (elements tails)))
                   (t (destructuring-bind (first . rest) lol
                        (cond ((atom first) (values '() '())) ; No CAR/CDR for this sublist. Results are meaningless. Abort.
                              (t (destructuring-bind (head . tail) first
                                   (enqueue heads head)
                                   (cond ((null tail) (collect-heads rest heads)) ; Encountering a single-elt list anywhere means all tails are discarded.
                                         (t (enqueue tails tail)
                                            (collect-heads-tails rest heads tails)))) )))) ))
           (collect-heads (lol heads)
;             (cond ((null lol) (values (elements heads) (list '())))
             (cond ((null lol) (values (elements heads) '()))
                   (t (destructuring-bind (first . rest) lol
                        (cond ((atom first) (values '() '())) ; No CAR/CDR for this sublist. Results are meaningless. Abort.
                              (t (destructuring-bind (head . tail) first
                                   (declare (ignore tail))
                                   (enqueue heads head)
                                   (collect-heads rest heads)))) )))) )
    (collect-heads-tails lol (make-linked-queue) (make-linked-queue))))

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
  (let ((chain (mapcar #'(lambda (key val) `(,key ',val)) vals (append (rest vals) (list (first vals)))) ))
    `(case ,var ,@chain)))

;; (defmacro defchain (var vals)
;;   (let ((chain (mapcar #'list vals (append (rest vals) (list (first vals)))) ))
;;     `(case ,var ,@chain)))


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
