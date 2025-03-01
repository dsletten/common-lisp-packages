;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               core.lisp
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
;;;;   Needs work: MAPTUPLES
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/collections" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/collections.lisp" :verbose nil))

(defpackage :core
  (:shadowing-import-from :collections :intersection :set :subsetp :union)
  (:use :common-lisp :collections)
  (:export :after :append1 :analyze-tree :approximately= :array-indices :as-if
           :before :best :best-index :best-worst :best-worst-n :bestn :build-prefix :build-tree
           :>case :class-template :comment :compose :conc1 :copy-array :cycle
           :defchain :destructure :dohash :doset :dostring :dotuples :dovector
           :drop :drop-until :drop-while :duplicatep
           :emptyp :ends-with :equalelts :equals :eqls :every-pred :explode
           :filter :filter-split :find-some-if :find-subtree :firsts-rests :for :flatten
           :group :group-until :horners
	   :if-let :if3 :iffn :in :in-if :inq :integralp :iterate
           :juxtapose
           :last1 :least :leastn :list-to-string :longerp
           :macroexpand-all :make-empty-seq :make-identity-matrix
           :map-> :map-array :map-array-index :map0-n :map1-n :mapa-b :mapcars :mappend :mapset
           :memoize :mklist :mkstr :most :mostn :most-least :most-least-n :nif
           :partial :partial* :partition :ppmx
           :prefix-generator :prefixp :prune :prune-if :prune-if-not
           :range :repeat :rmapcar 
           :rotate0 :rotate-list0 :rotate1 :rotate-list1
           :same-shape-tree-p
           :shift0 :shift-list0 :shift1 :shift-list1
           :show-symbols :shuffle :singlep :some-pred :sort-symbol-list :splice
;           :split-if
           :stable-partition :starts-with :stream-partition :suffixp :symb
           :take :take-drop :take-while :take-until :totally :transfer
           :transition :transition-1 :transition-n :transition-stream :traverse :tree-find-if :tree-map
           :until
           :when-let :when-let* :while :with-gensyms :worst :worstn)
  (:shadow :emptyp :next))
;  (:shadow :while :until :prefixp :dovector :macroexpand-all)) ; ????

(in-package :core)

(proclaim '(inline last1 singlep append1 conc1 mklist))

(defun integralp (x)
  (and (realp x) (zerop (rem x 1))))

(defmacro comment (&body body)
  (declare (ignore body)))

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

;;;
;;;    Should it be called WITH-GENSYMS if it uses MAKE-SYMBOL???
;;;    
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names
               collect `(,n ',(make-symbol (symbol-name n)))) ; ??
     ,@body))

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

(defgeneric drop (n seq)
  (:documentation "Drop the first N elements of sequence SEQ."))
(defmethod drop (n (seq list))
  (nthcdr n seq))
(defmethod drop (n (seq vector))
 (subseq seq (min n (length seq))))

(defgeneric take (n seq)
  (:documentation "Take the first N elements of sequence SEQ."))
(defmethod take (n (seq list))
  (loop repeat n for elt in seq collect elt))
(defmethod take (n (seq vector))
  (subseq seq 0 (min n (length seq))))

(defgeneric take-drop (n seq)
  (:documentation "Split a sequence at the Nth element. Return the subsequences before and after."))
(defmethod take-drop :around (n seq)
  (assert (typep n `(integer 0))
          (n)
          "N must be a non-negative integer.")
  (call-next-method))
(defmethod take-drop (n (seq list))
  (do ((tail seq (rest tail))
       (i 0 (1+ i)))
      ((or (= i n) (endp tail))
       (if (endp tail)
           (values seq '())
           (values (subseq seq 0 i) tail)))) )
(defmethod take-drop (n (seq vector))
  (values (take n seq) (drop n seq))) ; Works for strings too

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

;;;
;;;    Call TAKE-WHILE, get DROP-WHILE for free.
;;;    
(defgeneric take-while (pred seq)
  (:documentation "Take elements of sequence SEQ until PRED evaluates to false. Return remainder of sequence as secondary value."))
(defmethod take-while (pred (seq list))
  (loop for tail on seq
        for elt in seq
        while (funcall pred elt)
        collect elt into result
        finally (return (values result tail))))
(defmethod take-while (pred (seq vector))
  (take-drop (or (position-if-not pred seq) (length seq)) seq))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defgeneric drop-while (pred seq)
  (:documentation "Drop elements of sequence SEQ until PRED evaluates to false."))
(defmethod drop-while (pred (seq list))
  (loop for tail on seq
        for elt in seq
        while (funcall pred elt)
        finally (return tail)))
(defmethod drop-while (pred (seq vector))
  (drop (or (position-if-not pred seq) (length seq)) seq))

(defun drop-until (pred seq)
  (drop-while (complement pred) seq))

;; (defun empty-seq (in)
;;   (map (type-of in) #'identity in))

;; (defun empty-seq (in)
;;   (make-sequence (type-of in) 0))

(defun make-empty-seq (in)
  (typecase in
    (list (list))
    (string (make-string 0))
    (vector (vector))))

(defun prefix-generator (l)
  "Given a list L return a function that successively yields all prefixes of L."
  (let ((q (make-linked-queue))
        (list l))
    #'(lambda ()
        (prog1 (copy-list (elements q))
          (if (endp list)
              (make-empty q)
              (enqueue q (pop list)))) )))

;; pathname, structure, hash table, bit vector
(defgeneric equals (o1 o2)
  (:documentation "Is O1 equal to O2 in a type-specific sense?"))
(defmethod equals (o1 o2)
  (eql o1 o2))
(defmethod equals ((n1 number) (n2 number))
  (= n1 n2))
(defmethod equals ((s1 string) (s2 string))
  (string= s1 s2))
(defmethod equals ((ch1 character) (ch2 character))
  (char= ch1 ch2))
(defmethod equals ((l1 list) (l2 list))
  (cond ((null l1) (null l2))
        ((null l2) nil)
        ((equals (first l1) (first l2)) (equals (rest l1) (rest l2)))
        (t nil)))
(defmethod equals ((v1 vector) (v2 vector))
  (if (= (length v1) (length v2))
      (do ((i 0 (1+ i)))
          ((= i (length v1)) t)
        (unless (equals (aref v1 i) (aref v2 i))
          (return nil)))) )
(defmethod equals ((s1 symbol) (s2 symbol))
  (equals (symbol-name s1) (symbol-name s2)))
;; (defmethod equals ((k1 keyword) (k2 keyword))
;;   (call-next-method))

;; pathname, structure, hash table, bit vector
(defgeneric eqls (o1 o2)
  (:documentation "Is O1 EQL to O2 or are all elements EQL?"))
(defmethod eqls (o1 o2)
  (eql o1 o2))
(defmethod eqls ((s1 string) (s2 string))
  (string= s1 s2))
(defmethod eqls ((l1 list) (l2 list))
  (cond ((null l1) (null l2))
        ((null l2) nil)
        ((eqls (first l1) (first l2)) (eqls (rest l1) (rest l2)))
        (t nil)))
(defmethod eqls ((v1 vector) (v2 vector))
  (if (= (length v1) (length v2))
      (do ((i 0 (1+ i)))
          ((= i (length v1)) t)
        (unless (eqls (aref v1 i) (aref v2 i))
          (return nil)))) )

(defgeneric prefixp (s1 s2 &key test)
  (:documentation "Is sequence S1 a prefix of S2?"))
(defmethod prefixp ((v1 vector) (v2 vector) &key (test #'eql))
  (let ((length1 (length v1)))
    (if (>= (length v2) length1)
        (let ((index (mismatch v1 v2 :test test)))
          (or (null index) (= index length1)))
        nil)))
(defmethod prefixp ((l1 list) (l2 list) &key (test #'eql))
  (cond ((null l1) t)
        ((null l2) nil)
        ((funcall test (first l1) (first l2)) (prefixp (rest l1) (rest l2) :test test))
        (t nil)))
  

;;;
;;;    (prefixp (reverse l1) (reverse l2)) !!!!
;;;    

(defgeneric suffixp (s1 s2 &key test)
  (:documentation "Is sequence S1 and suffix of S2?"))
(defmethod suffixp ((v1 vector) (v2 vector) &key (test #'eql))
  (if (>= (length v2) (length v1))
      (let ((index (mismatch v1 v2 :test test :from-end t)))
        (or (null index) (zerop index)))
      nil))
;; (defmethod suffixp ((l1 list) (l2 list) &key (test #'eql))
;;   (labels ((match1 (l1 l2)
;;              (cond ((null l1) (null l2))
;;                    ((null l2) nil)
;;                    ((funcall test (first l1) (first l2))
;;                     (or (match2 (rest l1) (rest l2))
;;                         (match1 l1 (rest l2))))
;;                    (t (match1 l1 (rest l2)))) )
;;            (match2 (l1 l2)
;;              (cond ((null l1) (null l2))
;;                    ((null l2) nil)
;;                    ((funcall test (first l1) (first l2))
;;                     (match2 (rest l1) (rest l2)))
;;                    (t nil))))
;;     (or (null l1) (match1 l1 l2))))
(defmethod suffixp ((l1 list) (l2 list) &key (test #'eql))
  (cond ((null l2) (null l1))
        ((not (mismatch l1 l2 :test test)))
        (t (suffixp l1 (rest l2) :test test))))

(defun starts-with (s1 s2 &key (test #'eql))
  (prefixp s2 s1 :test test))

(defun ends-with (s1 s2 &key (test #'eql))
  (suffixp s2 s1 :test test))

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

;;;
;;;    Don't (declare (ignore ,i))!!
;;;    The macro doesn't use it directly, but DOTIMES does!
;;;    
(defmacro repeat (count &body body)
  (with-gensyms (i)
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

;; (defmacro dovector ((var vector &optional result) &body body)
;;   (let ((v (gensym))
;;         (l (gensym))
;;         (i (gensym)))
;;     `(do* ((,v ,vector)
;;            (,l (length ,v))
;;            (,i 0 (1+ ,i))
;;            ,var)
;;           ((= ,i ,l) ,result)
;;        (setf ,var (aref ,v ,i))
;;        ,@body)))

(defmacro dovector ((var vector &optional result) &body body)
  (with-gensyms (v l i)
    `(do* ((,v ,vector)
           (,l (length ,v))
           (,i 0 (1+ ,i)))
          ((= ,i ,l) ,result)
       (let ((,var (aref ,v ,i)))
         ,@body))))

;;;
;;;    Disallow crosstalk w/ LOOP keywords in BODY?
;;;    Binding of VARS should be available in RESULT... This is consistent with DOLIST but not very useful?
;;;    
;; (defmacro dotuples ((vars l &optional result) &body body)
;;   (with-gensyms (list rest)
;;     `(do ((,list ,l))
;;          ((endp ,list) ,result)
;;        (destructuring-bind (,@vars . ,rest) ,list
;;          ,@body
;;          (setf ,list ,rest)))) )

(defmacro dotuples ((vars l &optional result) &body body)
  "Iterate over list L consuming VARS variables on each iteration. The bindings of these variables are visible in BODY."
  `(loop for ,vars on ,l by #'(lambda (l) (nthcdr ,(length vars) l))
         do ,@body
         finally (return ,result)))

;;;
;;;    Kind of weird? VARS specifies variable bindings, but there is no body in which they are visible...
;;;    
;; (defmacro maptuples (f vars l)
;;   "Map the function F over the list L, consuming VARS variables on each iteration. 
;; If the length of L is not a multiple of the number of VARS, then some variables will be assigned NIL on the final iteration."
;;   `(loop for ,vars on ,l by #'(lambda (l) (nthcdr ,(length vars) l))
;;          collect (funcall ,f ,@vars)))

(defun maptuples (f n l)
  "Map function F over list L, consuming N elements on each invocation."
  (mapcar #'(lambda (args) (apply f args)) (group l n)))

;;;
;;;    Cosmetic changes to Graham's DO-TUPLES/O
;;;    
(defmacro open-path (vars source &body body)
  (if vars
      (with-gensyms (src)
        `(let ((,src ,source))
           (mapc #'(lambda ,vars ,@body)
                 ,@(map0-n #'(lambda (n)
                               `(nthcdr ,n ,src))
                           (1- (length vars)))) ))
      nil))

;;;
;;;    Derived from my DOTUPLES above
;;;    Graham's is more elegant.
;;;
;; (defmacro open-path (vars source &body body)
;;   (with-gensyms (src rest)
;;     `(do ((,src ,source (rest ,src)))
;;          (nil)
;;        (destructuring-bind (,@vars . ,rest) ,src
;;          ,@body
;;          (when (null ,rest)
;;            (return)))) ))

;; (macroexpand-1 '(do-tuples/o (x y) '(a b c d) (print (list x y))))

;; (PROG ((#:G4225 '(A B C D)))
;;   (MAPC #'(LAMBDA (X Y) (PRINT (LIST X Y)))
;;         (NTHCDR 0 #:G4225)
;;         (NTHCDR 1 #:G4225)))
;; T
;; * (macroexpand-1 '(open-path (x y) '(a b c d) (print (list x y))))

;; (LET ((#:SRC '(A B C D)))
;;   (MAPC #'(LAMBDA (X Y) (PRINT (LIST X Y))) (NTHCDR 0 #:SRC) (NTHCDR 1 #:SRC)))
;; T


;;;    CLOSED-PATH
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar #'(lambda (args)
                                    `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n #'(lambda (n)
                                         `(nth ,(1- n) 
                                               ,rest))
                                     len))))))))))
 
(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
              (map1-n #'(lambda (n) 
                          (let ((x (+ m n)))
                            (if (>= x len)
                                `(nth ,(- x len) ,src)
                                `(nth ,(1- x) ,rest))))
                      len))
          (- len 2)))


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
  (with-gensyms (s l i)
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
  (with-gensyms (next more)
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

;; (defgeneric mklist (obj))
;; (defmethod mklist (obj) (list obj))
;; (defmethod mklist ((l list)) l)

;;;
;;;    Graham calls this LONGER. I believe this is inappropriate since this
;;;    function does not return the 'longer' of the two sequences. Rather, it
;;;    simply tests whether SEQ1 is longer than SEQ2.
;;;    
;; (defun longerp (seq1 seq2)
;;   "Is SEQ1 strictly longer than SEQ2?"
;;   (labels ((compare (seq1 seq2)
;;              (cond ((endp seq1) nil)
;;                    ((endp seq2) t)
;;                    (t (compare (rest seq1) (rest seq2)))) ))
;;     (if (and (listp seq1) (listp seq2))
;;         (compare seq1 seq2)
;;         (> (length seq1) (length seq2)))) )

;; (defun longerp (seq1 seq2)
;;   "Is SEQ1 strictly longer than SEQ2?"
;;   (labels ((compare-ll (seq1 seq2)
;;              (cond ((endp seq1) nil)
;;                    ((endp seq2) t)
;;                    (t (compare-ll (rest seq1) (rest seq2)))) )
;;            (compare-ln (seq n)
;;              (cond ((endp seq) nil)
;;                    ((zerop n) t)
;;                    (t (compare-ln (rest seq) (1- n)))) )
;;            (compare-nl (n seq)
;;              (cond ((zerop n) nil)
;;                    ((endp seq) t)
;;                    (t (compare-nl (1- n) (rest seq)))) ))
;;     (cond ((and (listp seq1) (listp seq2)) (compare-ll seq1 seq2))
;;           ((listp seq1) (compare-ln seq1 (length seq2)))
;;           ((listp seq2) (compare-nl (length seq1) seq2))
;;           (t (> (length seq1) (length seq2)))) ))

(defgeneric longerp (seq1 seq2)
  (:documentation "Is SEQ1 strictly longer than SEQ2?"))
(defmethod longerp ((seq1 list) (seq2 list))
  (cond ((endp seq1) nil)
        ((endp seq2) t)
        (t (longerp (rest seq1) (rest seq2)))) )
(defmethod longerp ((seq1 list) (seq2 sequence))
  (labels ((compare (seq n)
             (cond ((endp seq) nil)
                   ((zerop n) t)
                   (t (compare (rest seq) (1- n)))) ))
    (compare seq1 (length seq2))))
(defmethod longerp ((seq1 sequence) (seq2 list))
  (labels ((compare (n seq)
             (cond ((zerop n) nil)
                   ((endp seq) t)
                   (t (compare (1- n) (rest seq)))) ))
    (compare (length seq1) seq2)))
(defmethod longerp ((seq1 sequence) (seq2 sequence))
  (> (length seq1) (length seq2)))

;;;
;;;    This collects the _values_ of applying the function to elts, not
;;;    the elts themselves. Different from REMOVE-IF-NOT!
;;;
;;;    More efficient version of: (mapcar #'f (remove-if-not #'f seq))
;;;    FILTER uses the result of applying F to determine which elements to keep.
;;;    
;; (defun filter (f seq)
;;   (typecase seq
;;     (list (loop for elt in seq
;;                 for val = (funcall f elt)
;;                 when val collect val))
;;     (vector (loop for elt across seq
;;                   for val = (funcall f elt)
;;                   when val collect val into result
;;                   finally (return (coerce result (if (stringp seq) 'string 'vector)))) )))
(defgeneric filter (f seq)
  (:documentation "Retain non-nil values obtained by applying F to elts of SEQ."))
(defmethod filter (f (seq list))
  (loop for elt in seq
        for val = (funcall f elt)
        when val collect val))
(defmethod filter (f (seq sequence))
  (loop for elt across seq
        for val = (funcall f elt)
        when val collect val))
(defmethod filter (f (seq vector))
  (coerce (call-next-method) 'vector))
(defmethod filter (f (seq string))
  (coerce (call-next-method) 'string))

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

(defun emptyp (seq)
  (typecase seq
    (list (null seq))
    (vector (zerop (length seq)))) )

;;;
;;;    See Clojure partition-all
;;;    
;; (defun group (seq n)
;;   (loop for take-drop =      (multiple-value-list (take-drop n seq))
;;                         then (multiple-value-list (take-drop n (second take-drop)))
;;         until (emptyp (first take-drop))
;;         collect (first take-drop)))

;;;
;;;    Must terminate when (EMPTYP TAKE) to handle N = 0.
;;;    
(defun group (seq n)
  (loop for (take drop) =      (multiple-value-list (take-drop n seq))
                          then (multiple-value-list (take-drop n drop))
        until (emptyp take)
        collect take))

;;;
;;;    This is even slower
;;;    
;; (defun group (seq n)
;;   (do ((result (make-linked-queue))
;;        (row (make-linked-queue))
;;        (l seq (rest l))
;;        (i 0 (1+ i)))
;;       ((emptyp l) (unless (collections:emptyp row)
;;                     (enqueue result (elements row)))
;;        (elements result))
;;     (when (= i n)
;;       (setf i 0)
;;       (enqueue result (elements row))
;;       (make-empty row))
;;     (enqueue row (first l))))

;;;
;;;    The semantics here seem wrong since the reversed list is tested but then flipped around when included in the result.
;;;    That may actually be irrelevant here since the TIPPING-POINT function normally tests the subsequence as a set, i.e.,
;;;    independent of its ordering.
;;;    
;; (defun group-until-list (tipping-point seq)
;;   "Group elements of SEQ into a subsequence until TIPPING-POINT returns true for that subsequence, then start next subsequence."
;;   (let ((groups (reduce #'(lambda (groups elt)
;;                             (destructuring-bind (current . result) groups
;;                               (if (funcall tipping-point (cons elt current))           ; Reversed list is tested??
;;                                   (cons (list elt) (cons (nreverse current) result)) ; But flipped for result???
;;                                   (cons (cons elt current) result))))
;;                         seq
;;                         :initial-value (cons '() '()))) )
;;     (destructuring-bind (current . result) groups
;;       (nreverse (cons (nreverse current) result)))) )

;;;
;;;    The above version is faster than these 2, presumably due to CLOS below.
;;;    The persistant-queue version is surprisingly competitive to the rollback version.
;;;    I expected that the ELEMENTS method for the persistent-queue would sink it.
;;;
;;;    It does appear to slow down as the length of the accumulated subsequence grows:
;;;    (defvar *l* (loop repeat 800 collect (random 20)))
;;;    (time (dotimes (i 10000) (GROUP-UNTIL #'(LAMBDA (L) (> (REDUCE #'+ L) 20)) *l*)))
;;;    (time (dotimes (i 10000) (GROUP-UNTIL-persistent #'(LAMBDA (L) (> (REDUCE #'+ L) 20)) *l*)))
;;;
;;;    vs.
;;;
;;;    (time (dotimes (i 10000) (GROUP-UNTIL #'(LAMBDA (L) (> (REDUCE #'+ L) 100)) *l*)))
;;;    (time (dotimes (i 10000) (GROUP-UNTIL-persistent #'(LAMBDA (L) (> (REDUCE #'+ L) 100)) *l*)))
;;;    
(defun group-until-persistent (tipping-point seq)
  "Group elements of SEQ into a subsequence until TIPPING-POINT returns true for that subsequence, then start next subsequence."
  (let ((groups (reduce #'(lambda (groups elt)
                            (destructuring-bind (current . result) groups
                              (if (funcall tipping-point (elements (enqueue current elt)))
                                  (cons (enqueue (make-persistent-queue) elt) (enqueue result (elements current)))
                                  (cons (enqueue current elt) result))))
                        seq
                        :initial-value (cons (make-persistent-queue) (make-persistent-queue)))) )
    (destructuring-bind (current . result) groups
      (elements (enqueue result (elements current)))) ))

;; (defun group-until (tipping-point seq)
;;   "Group elements of SEQ into a subsequence until TIPPING-POINT returns true for that subsequence, then start next subsequence."
;;   (let ((groups (reduce #'(lambda (groups elt)
;;                             (destructuring-bind (current . result) groups
;;                               (enqueue current elt)
;;                               (if (funcall tipping-point (elements current))
;;                                   (let ((new-current (make-rollback-queue)))
;;                                     (rollback current)
;;                                     (enqueue result (elements current))
;;                                     (enqueue new-current elt)
;;                                     (cons new-current result))
;;                                   (cons current result))))
;;                         seq
;;                         :initial-value (cons (make-rollback-queue) (make-linked-queue)))) )
;;     (destructuring-bind (current . result) groups
;;       (elements (enqueue result (elements current)))) ))

;;;
;;;    All of the fat trimmed off...
;;;    
(defun group-until (tipping-point seq)
  "Group elements of SEQ into a subsequence until TIPPING-POINT returns true for that subsequence, then start next subsequence."
  (let ((groups (reduce #'(lambda (groups elt)
                            (destructuring-bind (current . result) groups
                              (enqueue current elt)
                              (when (funcall tipping-point (elements current))
                                (rollback current)
                                (enqueue result (elements current))
                                (make-empty current)
                                (enqueue current elt))
                              groups))
                        seq
                        :initial-value (cons (make-rollback-queue) (make-linked-queue)))) )
    (destructuring-bind (current . result) groups
      (elements (enqueue result (elements current)))) ))

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


;; (defun flatten-q (tree)
;;   (labels ((flatten-aux (tree result)
;;              (cond ((null tree) (elements result))
;;                    ((null (car tree)) (flatten-aux (cdr tree) result))
;;                    ((atom (car tree)) (flatten-aux (cdr tree) (enqueue result (car tree))))
;;                    (t (destructuring-bind ((head . tail1) . tail2) tree
;;                         (flatten-aux (list* head tail1 tail2) result)))) ))
;;     (if (atom tree)
;;         tree
;;         (flatten-aux tree (make-linked-queue)))) )

(defun flatten-q (tree)
  (labels ((flatten-aux (tree result)
             (if (null tree)
                 (elements result)
                 (destructuring-bind (car . cdr) tree
                   (cond ((null car) (flatten-aux cdr result))
                         ((atom car) (flatten-aux cdr (enqueue result car)))
                         (t (destructuring-bind (car1 . cdr1) car
                              (flatten-aux (list* car1 cdr1 cdr) result)))) ))))
    (if (atom tree)
        tree
        (flatten-aux tree (make-linked-queue)))) )

(defun flatten-dfs-q (tree)
  (labels ((flatten-aux (tree result)
             (cond ((null tree) result)
                   ((atom tree) (enqueue result tree))
                   (t (flatten-aux (cdr tree) (flatten-aux (car tree) result)))) ))
    (if (atom tree)
        tree
        (elements (flatten-aux tree (make-linked-queue)))) ))

(defun prune (item tree &key (test #'eql) (key #'identity))
  "Remove all instances of the atom ITEM from TREE."
  (labels ((prune-aux (tree result)
	     (cond ((null tree) (nreverse result))
                   (t (destructuring-bind (car . cdr) tree
                        (cond ((and (atom car) (funcall test item (funcall key car))) (prune-aux cdr result))
                              ((atom car) (prune-aux cdr (cons car result)))
                              (t (prune-aux cdr (cons (prune-aux car '()) result)))) )))) )
    (prune-aux tree '())))

;; See prune.lisp
;; (defun prune-if (pred tree)
;;   "Remove all leaves of TREE for which PRED is true. Like REMOVE-IF for trees."
;;   (labels ((prune-aux (tree acc)
;;              (cond ((null tree) (nreverse acc))
;;                    ((atom (car tree)) (prune-aux (cdr tree)
;;                                                  (if (funcall pred (car tree))
;;                                                      acc
;;                                                      (cons (car tree) acc))))
;;                    (t (prune-aux (cdr tree)
;;                                  (cons (prune-aux (car tree) '()) acc)))) ))
;;     (prune-aux tree '())))

(defun prune-if (pred tree)
  "Remove all leaves of TREE for which PRED is true."
  (labels ((prune-aux (tree result)
	     (cond ((null tree) (nreverse result))
                   (t (destructuring-bind (car . cdr) tree
                        (cond ((and (atom car) (funcall pred car)) (prune-aux cdr result))
                              ((atom car) (prune-aux cdr (cons car result)))
                              (t (prune-aux cdr (cons (prune-aux car '()) result)))) )))) )
    (prune-aux tree '())))

;;;
;;;    Like REMOVE-IF-NOT for trees.
;;;    
(defun prune-if-not (pred tree)
  "Remove all leaves of TREE for which PRED is not true."
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
;;;    Touretzky pg. 171 has a looser version:
;;;
;; (defun beforep (x y l)
;;   (member y (member x l)))

;;;    (beforep 'a 'b '(b a b u s h k a)) => (B U S H K A)
;;;    (before 'a 'b '(b a b u s h k a)) => NIL
;;;    (beforep 'a 'u '(b a b u s h k a)) => (U S H K A)
;;;    (before 'a 'u '(b a b u s h k a)) => (A B U S H K A)
;;;    

(defgeneric before (x y seq &key test key)
  (:documentation  "Does X occur before Y in SEQ? This is true whenever X occurs without Y having yet been encountered, 
i.e., even if Y is not actually in the sequence. For a positive result, returns the tail of the list
starting with X or the index of the position of X in the sequence."))
(defmethod before (x y (seq null) &key test key)
  (declare (ignore x y seq test key))
  nil)
(defmethod before (x y (seq list) &key (test #'eql) (key #'identity))
  (if (funcall test x y)
      nil
      (loop for cons on seq
            for elt = (first cons)
            when (funcall test y (funcall key elt)) return nil
            when (funcall test x (funcall key elt)) return cons)))
(defmethod before (x y (seq vector) &key (test #'eql) (key #'identity))
  (if (funcall test x y)
      nil
      (loop for elt across seq
            for i from 0
            when (funcall test y (funcall key elt)) return nil
            when (funcall test x (funcall key elt)) return i)))

(defgeneric after (x y seq &key test key)
  (:documentation "Does X occur after Y in SEQ? X must explicitly and exclusively be present after Y."))
(defmethod after (x y (seq null) &key test key)
  (declare (ignore x y seq test key))
  nil)
(defmethod after (x y (seq list) &key (test #'eql) (key #'identity))
 (member x (before y x seq :test test :key key) :test test :key key))
(defmethod after (x y (seq vector) &key (test #'eql) (key #'identity))
  (let ((index (before y x seq :test test :key key)))
    (and index
         (position x seq :start index :test test :key key))))

(defgeneric duplicatep (obj seq &key test key)
  (:documentation "Are there duplicate instances of OBJ in SEQ as determined by TEST? If so return the tail of the list starting with the duplicate or the index in the vector of the duplicate."))
(defmethod duplicatep (obj (seq null) &key test key)
  (declare (ignore obj seq test key))
  nil)
(defmethod duplicatep (obj (seq list) &key (test #'eql) (key #'identity))
  (member obj (rest (member obj seq :test test :key key)) :test test :key key))
(defmethod duplicatep (obj (seq vector) &key (test #'eql) (key #'identity))
  (let ((initial (position obj seq :test test :key key)))
    (and initial
         (position obj seq :start (1+ initial) :test test :key key))))

;; (defun most-concept (f seq)
;;   (let* ((scores (map 'vector #'(lambda (elt) (list elt (funcall f elt))) seq))
;;          (sorted (stable-sort scores #'> :key #'second)))
;;     (values-list (aref sorted 0))))

;; (defun most (f seq)
;;   (labels ((most-list (seq winner max)
;;              (if (endp seq)
;;                  (values winner max)
;;                  (let* ((elt (first seq))
;;                         (score (funcall f elt)))
;;                    (if (> score max)
;;                        (most-list (rest seq) elt score)
;;                        (most-list (rest seq) winner max)))) )
;;            (most-vector (i winner max)
;;              (if (= i (length seq))
;;                  (values winner max)
;;                  (let* ((elt (elt seq i))
;;                         (score (funcall f elt)))
;;                    (if (> score max)
;;                        (most-vector (1+ i) elt score)
;;                        (most-vector (1+ i) winner max)))) ))
;;     (typecase seq
;;       (list (if (null seq)
;;                 nil
;;                 (most-list (rest seq) (first seq) (funcall f (first seq)))) )
;;       (vector (if (zerop (length seq))
;;                   nil
;;                   (most-vector 1 (elt seq 0) (funcall f (elt seq 0)))) ))))

(defclass generator () ())
(defclass list-generator (generator)
  ((contents :initarg :contents)))
(defclass vector-generator (generator)
  ((contents :initarg :contents)
   (index :initform 0 :initarg :index)))

(defun make-generator (seq)
  (etypecase seq
    (list (make-instance 'list-generator :contents seq))
    (vector (make-instance 'vector-generator :contents seq))))

(defgeneric exhaustedp (generator)
  (:documentation "Has every element of a generator's sequence been consumed?"))
(defmethod exhaustedp ((g list-generator))
  (with-slots (contents) g
    (null contents)))
(defmethod exhaustedp ((g vector-generator))
  (with-slots (contents index) g
    (= index (length contents))))

(defgeneric current (generator)
  (:documentation "Retrieve current element of the generator's sequence."))
(defmethod current :around ((g generator))
  (if (exhaustedp g)
      (error "The generator has been exhausted.")
      (call-next-method)))
(defmethod current ((g list-generator))
  (with-slots (contents) g
    (first contents)))
(defmethod current ((g vector-generator))
  (with-slots (contents index) g
    (elt contents index)))

(defgeneric next (generator)
  (:documentation "Return new generator advanced to next element."))
(defmethod next :around ((g generator))
  (if (exhaustedp g)
      (error "The generator has been exhausted.")
      (call-next-method)))
(defmethod next ((g list-generator))
  (with-slots (contents) g
    (make-instance 'list-generator :contents (rest contents))))
(defmethod next ((g vector-generator))
  (with-slots (contents index) g
    (make-instance 'vector-generator :contents contents :index (1+ index))))

;;;
;;;    TODO: Use persistent queues
;;;    
(defun extrema (seq &key (test #'>) (key #'identity))
  (if (emptyp seq)
      (values nil nil nil nil)
      (labels ((beats (a b)
                 (funcall test a b))
               (find-extrema (generator winners max losers min)
                 (if (exhaustedp generator)
                     (values (elements winners) max (elements losers) min)
                     (let* ((elt (current generator))
                            (score (funcall key elt)))
                       (update-winners generator winners max losers min elt score))))
               (update-winners (generator winners max losers min elt score)
                 (cond ((beats score max)
                        (make-empty winners)
                        (enqueue winners elt)
                        (update-losers generator winners score losers min elt score))
                       (t (unless (beats max score)
                            (enqueue winners elt))
                          (update-losers generator winners max losers min elt score))))
               (update-losers (generator winners max losers min elt score)
                 (cond ((beats min score)
                        (make-empty losers)
                        (enqueue losers elt)
                        (find-extrema (next generator) winners max losers score))
                       (t (unless (beats score min)
                            (enqueue losers elt))
                          (find-extrema (next generator) winners max losers min)))) )
        (let* ((generator (make-generator seq))
               (max (funcall key (current generator)))
               (min max))
          (find-extrema generator
                        (make-linked-queue)
                        max
                        (make-linked-queue)
                        min)))) )

(defun most-least-n (f seq)
  (extrema seq :key f))

(defun most (f seq)
  (multiple-value-bind (winners max) (most-least-n f seq)
    (values (first winners) max)))
(defun least (f seq)
  (multiple-value-bind (winners max losers min) (most-least-n f seq)
    (declare (ignore winners max))
    (values (first losers) min)))
(defun most-least (f seq)
  (multiple-value-bind (winners max losers min) (most-least-n f seq)
    (values (first winners) max (first losers) min)))

(defun mostn (f seq)
  (multiple-value-bind (winners max) (most-least-n f seq)
    (values winners max)))
(defun leastn (f seq)
  (multiple-value-bind (winners max losers min) (most-least-n f seq)
    (declare (ignore winners max))
    (values losers min)))

(defun best-worst-n (f seq)
  (multiple-value-bind (winners max losers min) (extrema seq :test f)
    (declare (ignore max min))
    (values winners losers)))

(defun best (f seq)
  (multiple-value-bind (winners) (best-worst-n f seq)
    (first winners)))
(defun worst (f seq)
  (multiple-value-bind (winners losers) (best-worst-n f seq)
    (declare (ignore winners))
    (first losers)))
(defun best-worst (f seq)
  (multiple-value-bind (winners losers) (best-worst-n f seq)
    (values (first winners) (first losers))))

(defun bestn (f seq)
  (multiple-value-bind (winners) (best-worst-n f seq)
    winners))
(defun worstn (f seq)
  (multiple-value-bind (winners losers) (best-worst-n f seq)
    (declare (ignore winners))
    losers))

  

;; (defun best-concept (f seq)
;;   (let ((sorted (stable-sort (copy-seq seq) f)))
;;     (elt sorted 0)))

;;;
;;;    Graham's original
;;;    
;; (defun best (fn list)
;;   (if (null list)
;;       nil
;;       (let ((wins (car list)))
;;         (dolist (obj (cdr list))
;;           (when (funcall fn obj wins)
;; 	    (setq wins obj)))
;;         wins)))

;; (defun best (f seq)
;;   ""
;;   (typecase seq
;;     (list (if (null seq)
;;               nil
;;               (loop with winner = (first seq)
;;                     for elt in (rest seq)
;;                     when (funcall f elt winner) do (setf winner elt)
;;                     finally (return winner))))
;;     (vector (if (zerop (length seq))
;;                 nil
;;                 (loop with winner = (elt seq 0)
;;                       for i from 1 below (length seq)
;;                       for elt = (elt seq i)
;;                       when (funcall f elt winner) do (setf winner elt)
;;                       finally (return winner)))) ))

(defun best* (f seq)
  "Return the first element as if the elements of SEQ were sorted by means of F."
  (labels ((a-vs-b (b a)
             (if (funcall f a b) a b)))
    (if (emptyp seq)
        nil
        (reduce #'a-vs-b seq))))

;; (defun bestn (f seq)
;;   "Return all elements with the highest score as if the elements of SEQ were sorted by means of F."
;;   (labels ((elt-beats-winner (elt winner)
;;              (funcall f elt winner))
;;            (elt-is-winner (elt winner)
;;              (not (funcall f winner elt))))
;;     (if (emptyp seq)
;;         nil
;;         (typecase seq
;;           (list (loop with winners = (make-linked-queue)
;;                       with winner = (first seq)
;;                       for elt in seq
;;                       do (cond ((elt-beats-winner elt winner)
;;                                 (make-empty winners)
;;                                 (enqueue winners elt)
;;                                 (setf winner elt))
;;                                ((elt-is-winner elt winner)
;;                                 (enqueue winners elt)))
;;                       finally (return (elements winners))))
;;           (vector (loop with winners = (make-linked-queue)
;;                         with winner = (elt seq 0)
;;                         for elt across seq
;;                         do (cond ((elt-beats-winner elt winner)
;;                                   (make-empty winners)
;;                                   (enqueue winners elt)
;;                                   (setf winner elt))
;;                                  ((elt-is-winner elt winner)
;;                                   (enqueue winners elt)))
;;                         finally (return (elements winners)))) ))))

(defun bestn* (f seq)
  "Return all elements with the highest score as if the elements of SEQ were sorted by means of F."
  (labels ((elt-beats-winner (elt winner)
             (funcall f elt winner))
           (elt-is-winner (elt winner)
             (not (funcall f winner elt))))
    (if (emptyp seq)
        nil
        (let ((winners (make-linked-queue)))
          (reduce #'(lambda (winner elt)
                      (cond ((elt-beats-winner elt winner)
                             (make-empty winners)
                             (enqueue winners elt)
                             elt)
                            ((elt-is-winner elt winner)
                             (enqueue winners elt)
                             winner)
                            (t winner)))
                  seq
                  :initial-value (elt seq 0))
          (elements winners)))) )

;;;
;;;    CONSes!
;;;    
;; (defun best-worst (f seq)
;;   "Return the first and last elements as if the elements of SEQ were sorted by means of F."
;;   (let ((first (elt seq 0))) ; Assumes not empty?!
;;     (values-list (reduce #'(lambda (winner elt)
;;                              (destructuring-bind (max min) winner
;;                                (cond ((funcall f elt max) (list elt min))
;;                                      ((funcall f min elt) (list max elt))
;;                                      (t winner))))
;;                          seq
;;                          :initial-value (list first first)))) )

;; (defun best-worst (f seq)
;;   "Return the first and last elements as if the elements of SEQ were sorted by means of F."
;;   (if (emptyp seq)
;;       nil
;;       (typecase seq
;;         (list (loop with winner = (first seq)
;;                     with loser = winner
;;                     for elt in (rest seq)
;;                     when (funcall f elt winner) do (setf winner elt)
;;                     else when (funcall f loser elt) do (setf loser elt)
;;                     finally (return (values winner loser))))
;;         (vector (loop with winner = (elt seq 0)
;;                       with loser = winner
;;                       for i from 1 below (length seq)
;;                       for elt = (elt seq i)
;;                       when (funcall f elt winner) do (setf winner elt)
;;                       else when (funcall f loser elt) do (setf loser elt)
;;                       finally (return (values winner loser)))) )))

(defun best-worst* (f seq)
  "Return the first and last elements as if the elements of SEQ were sorted by means of F."
  (if (emptyp seq)
      nil
      (typecase seq
        (list (loop with winner = (first seq)
                    with loser = winner
                    for elt in (rest seq)
                    when (funcall f elt winner) do (setf winner elt)
                    else when (funcall f loser elt) do (setf loser elt)
                    finally (return (values winner loser))))
        (vector (loop with winner = (elt seq 0)
                      with loser = winner
                      for i from 1 below (length seq)
                      for elt = (elt seq i)
                      when (funcall f elt winner) do (setf winner elt)
                      else when (funcall f loser elt) do (setf loser elt)
                      finally (return (values winner loser)))) )))


(defun best-index (f seq)
  (bestn #'(lambda (a b) (funcall f (second a) (second b)))
         (loop for elt in seq 
               for i from 0 
               collect (list i elt))))

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
	       (cond ((collections:emptyp nodes) nil)
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
;;;    Compare:
;;;    (let ((l (list 1 2 3 4 5))) (take 4 l)) => (1 2 3 4)
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
;; (defun transition (l)
;;   (transition-aux l l))

(defun transition-aux (l tail)
  (cond ((endp tail) (list (list l '())))
        (t (cons (list (build-prefix l tail) tail)
                 (transition-aux l (rest tail)))) ))

(defun transition (l)
  (do ((result (make-linked-queue))
       (head (make-linked-queue))
       (tail l (rest tail)))
      ((endp tail) (enqueue result (list (copy-list (elements head)) tail))
       (elements result))
    (enqueue result (list (copy-list (elements head)) tail))
    (enqueue head (first tail))))

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

;;;
;;;    Create a stream of the heads/tails of the given list.
;;;    (defvar *stream* (transition-stream '(a b c)))
;;;    (funcall *stream*) => (NIL (A B C))
;;;    (funcall *stream*) => ((A) (B C))
;;;    (funcall *stream*) => ((A B) (C))
;;;    (funcall *stream*) => ((A B C) NIL)
;;;    (funcall *stream*) => NIL
;;;    
(defun transition-stream (l)
  (let ((head (make-linked-queue))
        (tail l))
    #'(lambda ()
        (cond ((null head) nil)
              (t (prog1 (list (copy-list (elements head)) tail)
                   (cond ((endp tail) (setf head nil))
                         (t (enqueue head (first tail))
                            (setf tail (rest tail)))) )))) ))

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
;;;    Some may consider this behavior inconsistent.
;;;    If both START and END are provided, then the endpoints are inclusive.
;;;    If only START is provided, then it is treated as an exclusive upper bound:
;;;    (range 2 5) => (2 3 4 5)
;;;    (range 3) => (0 1 2)
;;;    
(defun range (start &optional end (step 1))
  (make-range start end step))
(defgeneric make-range (start end step)
  (:documentation "Create a list from START to END inclusive by STEP."))
(defmethod make-range ((start character) (end character) (step integer))
  (mapa-b #'code-char (char-code start) (char-code end) step))
(defmethod make-range ((start character) (end null) (step integer))
  (declare (ignore end))
  (if (char> start #\0)
      (make-range #\0 (code-char (1- (char-code start))) step)
      '()))
(defmethod make-range ((start character) end step)
  (error "Mismatched input types."))
(defmethod make-range ((start number) (end number) (step number))
  (mapa-b #'identity start end step))
(defmethod make-range ((start number) (end number) (step function))
  (if (<= start end)
      (loop with f = (iterate step start)
            for elt = (funcall f)
            until (> elt end)
            collect elt)
      (loop with f = (iterate step start)
            for elt = (funcall f)
            until (< elt end)
            collect elt)))
(defmethod make-range ((start number) (end null) (step number))
  (declare (ignore end))
  (if (plusp start)
      (make-range 0 (1- start) step)
      '()))
(defmethod make-range ((start number) end step)
  (error "Mismatched input types."))
  
;; (defun map-> (fn start test-fn succ-fn)
;;   (do ((i start (funcall succ-fn i))
;;        (result '()))
;;       ((funcall test-fn i) (nreverse result))
;;     (push (funcall fn i) result)))

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

;;;
;;;    Non-destructive alternative to MAPCAN.
;;;    
(defun mappend (f &rest lists)
  (apply #'append (apply #'mapcar f lists)))

;;;
;;;    Map over multiple lists in sequence, accumulating all results.
;;;    
(defun mapcars (fn &rest lsts)
  (let ((result '()))
    (dolist (l lsts)
      (dolist (obj l)
	(push (funcall fn obj) result)))
    (nreverse result)))

;; (defun mapcars (f &rest lists)
;;   "Map the function F over each element of each list argument provided."
;;   (loop with result = (make-linked-queue)
;;         for list in lists
;;         do (loop for elt in list
;;                  do (enqueue result (funcall f elt)))
;;         finally (return (elements result))))

;; Graham does not handle arbitrary trees
;; (rmapcar #'1+ '((1 . 2) (3 . 4) (5 . 6)))
;; (defun rmapcar (fn &rest args)
;;   (if (some #'atom args)
;;       (apply fn args)
;;       (apply #'mapcar #'(lambda (&rest args)
;; 			  (apply #'rmapcar fn args))
;; 	     args)))

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

;; (defun tree-map-a (f &rest trees)
;;   (cond ((every #'null trees) '())
;;         ((every #'atom trees) (apply f trees)) ; Too strict to allow different depths.
;; ;        (t (multiple-value-bind (cars cdrs) (strip-trees trees)
;;         (t (multiple-value-bind (cars cdrs) (firsts-rests trees)
;;              (cons (apply #'tree-map-a f cars)
;;                    (apply #'tree-map-a f cdrs)))) ))

;;;
;;;    This version is adequate to handle multiple trees of identical structure. The mapped function F may
;;;    have to be modified depending on the number of trees...
;;;    
;; (defun tree-map-b (f &rest trees)
;;   (cond ((every #'null trees) '())
;;         ((some #'atom trees) (apply f trees))
;; ;        (t (multiple-value-bind (cars cdrs) (strip-trees trees)
;;         (t (multiple-value-bind (cars cdrs) (firsts-rests trees)
;;              (cons (apply #'tree-map-b f cars)
;;                    (apply #'tree-map-b f cdrs)))) ))

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

;;;
;;;    见 ~/lisp/books/Tanimoto/ch02/2010/ch02.lisp REASSEMBLE
;;;    
(defun build-tree (f obj)
  (cond ((null obj) obj)
        ((atom obj) (funcall f obj))
        (t (cons (build-tree f (car obj))
                 (build-tree f (cdr obj))))) )

;; (defun equalelts (seq &key (test #'equal) (key #'identity))
;;   "Are all elements of SEQ equal with respect to TEST after applying KEY?"
;;   (or (emptyp seq)
;;       (multiple-value-bind (head tail) (take-drop 1 seq)
;;         (let ((exemplar (funcall key (elt head 0))))
;;           (every (compose (partial* test exemplar) key) tail)))) )

;; (defgeneric equalelts (seq &key test key)
;;   (:documentation "Are all elements of SEQ equal with respect to TEST after applying KEY?"))
;; (defmethod equalelts ((seq null) &key test key)
;;   (declare (ignore seq test key))
;;   t)
;; (defmethod equalelts ((seq list) &key (test #'equal) (key #'identity))
;;   (every #'(lambda (x y) (funcall test (funcall key x) (funcall key y))) seq (rest seq)))
;; (defmethod equalelts ((seq vector) &key (test #'equal) (key #'identity))
;;   (not (mismatch seq seq :start1 1 :end2 (1- (length seq)) :key key :test test)))

(defgeneric equalelts (seq &key test key)
  (:documentation "Are all elements of SEQ equal with respect to TEST after applying KEY?"))
(defmethod equalelts :around (seq &key test key)
  (declare (ignore test key))
  (or (emptyp seq)
      (call-next-method)))
(defmethod equalelts ((seq list) &key (test #'equal) (key #'identity))
  (loop for elt in seq
        with exemplar = (funcall key (elt seq 0))
        always (funcall test exemplar (funcall key elt))))
(defmethod equalelts ((seq vector) &key (test #'equal) (key #'identity))
  (loop for elt across seq
        with exemplar = (funcall key (elt seq 0))
        always (funcall test exemplar (funcall key elt))))

(defun totally (seq) (notany #'not seq))

(defun as-if (seq) (every #'not seq))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; (defun explode (sym)
;;   (map 'list #'(lambda (c)
;; 		 (intern (make-string 1 :initial-element c)))
;;        (symbol-name sym)))

(defun explode (sym)
  "Return a list of single-letter symbols from the characters in the symbol name of SYM."
  (map 'list (compose #'intern #'string) (symbol-name sym)))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))) )))


;;;    Multiple value compose???

;;;
;;;    This executes REDUCE every time the composed function is called.
;;;    
;; (defun compose (&rest fns)
;;   (if fns
;;       (let ((fn1 (last1 fns))
;;             (fns (butlast fns)))
;;         #'(lambda (&rest args)
;;             (reduce #'funcall fns
;;                     :from-end t
;;                     :initial-value (apply fn1 args))))
;;       #'identity))


;;;
;;;    Same here for > 2 functions...
;;;    Same logic for multiple functions.
;;;    
;; (defun compose (&rest fs)
;;   (if (null fs)
;;       #'identity
;;       (destructuring-bind (f . more) fs
;;         (if (null more)
;;             f
;;             (destructuring-bind (g . more) more
;;               (if (null more)
;;                   #'(lambda (&rest args) (funcall f (apply g args)))
;;                   (destructuring-bind (f* . fs*) (reverse fs)
;;                     #'(lambda (&rest args)
;;                         (reduce #'(lambda (x f) (funcall f x)) fs* :initial-value (apply f* args)))) )))) ))

;;;
;;;    Clojure-style
;;;    Composed function is constructed entirely prior to invocation.
;;;    
(defun compose (&rest fs)
  (if (null fs)
      #'identity
      (destructuring-bind (f . more) fs
        (if (null more)
            f
            (destructuring-bind (g . more) more
              (if (null more)
                  #'(lambda (&rest args) (funcall f (apply g args)))
                  (reduce #'compose fs)))) )))

;;;
;;;    Inspired by Clojure juxt
;;;    - Multiple values returned by each function are captured in their own list.
;;;
(defun juxtapose (&rest fs)
  #'(lambda (&rest args)
      (values-list (mapcar #'(lambda (f) (multiple-value-list (apply f args))) fs))))
;      (values-list (mapcar #'(lambda (f) (apply f args)) fs))))

;;;
;;;    Graham ACL pg. 110
;;;
;;;    "Curry"
;;;    Have to pay runtime penalty rather than using function literal (essentially inline)?
;;;
;;;    Clozure:
;;;    
;; ? (time (dotimes (i 10000) (funcall #'(lambda (x) (+ x 8)) 7)))
;; (DOTIMES (I 10000) (FUNCALL #'(LAMBDA (X) (+ X 8)) 7))
;; took  7 microseconds (0.000007 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      10 microseconds (0.000010 seconds) were spent in user mode
;;       0 microseconds (0.000000 seconds) were spent in system mode
;; NIL
;; ? (time (dotimes (i 10000) (funcall (partial #'+ 8) 7)))
;; (DOTIMES (I 10000) (FUNCALL (PARTIAL #'+ 8) 7))
;; took 6,144 microseconds (0.006144 seconds) to run.
;;      1,107 microseconds (0.001107 seconds, 18.02%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      6,161 microseconds (0.006161 seconds) were spent in user mode
;;          0 microseconds (0.000000 seconds) were spent in system mode
;;  1,280,000 bytes of memory allocated.
;; NIL
;; ? (time (dotimes (i 10000) (funcall #'(lambda (x) (+ x 8)) 7)))
;; (DOTIMES (I 10000) (FUNCALL #'(LAMBDA (X) (+ X 8)) 7))
;; took 12 microseconds (0.000012 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      16 microseconds (0.000016 seconds) were spent in user mode
;;       0 microseconds (0.000000 seconds) were spent in system mode
;; NIL
;; ? (time (dotimes (i 10000) (funcall (partial #'+ 8) 7)))
;; (DOTIMES (I 10000) (FUNCALL (PARTIAL #'+ 8) 7))
;; took 4,169 microseconds (0.004169 seconds) to run.
;; During that period, and with 16 available CPU cores,
;;      4,171 microseconds (0.004171 seconds) were spent in user mode
;;          0 microseconds (0.000000 seconds) were spent in system mode
;;  1,280,000 bytes of memory allocated.
;; NIL
;; (defun partial (f &rest args)
;;   (if (null args)
;;       f
;;       #'(lambda (&rest args2)
;;           (apply f (append args args2)))) )

;;;
;;;    Clojure style
;;;    
(defun partial (f &rest args)
  (if (null args)
      f
      (destructuring-bind (arg0 &rest args) args
        (if (null args)
            #'(lambda (&rest args2)
                (apply f arg0 args2))
            (destructuring-bind (arg1 &rest args) args
              (if (null args)
                  #'(lambda (&rest args2)
                      (apply f arg0 arg1 args2))
                  (destructuring-bind (arg2 &rest args) args
                    (if (null args)
                        #'(lambda (&rest args2)
                            (apply f arg0 arg1 arg2 args2))
                        #'(lambda (&rest args2)
                            (apply f arg0 arg2 arg2 (append args args2)))) )))) )))
;;;
;;;    "Right Curry"
;;;    
(defun partial* (f &rest args)
  (if (null args)
      f
      #'(lambda (&rest args2)
          (apply f (append args2 args)))) )

;; (defmacro defcurry (name (param &rest params) &body body)
;;   (if (null params)
;;       `(defun ,name (,param)
;;          ,@body)
;;       `(defcurry ,name (,param)
;;          ,(expand params body))))

;; (defun expand (params body)
;;   (if (endp params)
;;       body
;;       (destructuring-bind (param &rest more) params
;;       `((lambda (,param) ,(expand more body)))) ))

;; (defmacro defcurry (name (param &rest params) &body body)
;;   (if (null params)
;;       `(defun ,name ()
;;          #'(lambda (,param) ,@body))
;;       `(defcurry ,name (,param)
;;          (funcall ,(expand params body))))

;; (defun expand (params body)
;;   (if (endp params)
;;       body
;;       (destructuring-bind (param &rest more) params
;;       `((lambda (,param) ,(expand more body)))) ))

;; (defmacro defcurry ((param &rest params) &body body)
;;   (if (null params)
;;       `#'(lambda (,param) ,@body)
;;       `(partial (defcurry ,params ,body) 

;; (defmacro defcurry ((param &rest params) &body body)
;;   (if (null params)
;;       `#'(lambda (,param) ,@body)
;;       `#'(lambda (,param) (defcurry ,params ,@body))))


;;;
;;;    (curry (x y) (+ x y))) => #'(LAMBDA (X) #'(LAMBDA (Y) (PROGN (+ X Y))))
;;;
;;;    (macroexpand-1 '(curry (x y) (+ x y))) => #'(LAMBDA (X) (CURRY (Y) (+ X Y)))
;;;
;;;    (macroexpand-1 '(CURRY (Y) (+ X Y))) => #'(LAMBDA (Y) (CURRY NIL (+ X Y)))
;;;
;;;    (macroexpand-1 '(CURRY NIL (+ X Y))) => (PROGN (+ X Y))
;;;    
(defmacro curry ((&rest params) &body body)
  (if (null params)
      `(progn ,@body)
      (destructuring-bind (param &rest more) params
        `#'(lambda (,param) (curry ,more ,@body)))) )

(defmacro curry* ((&rest params) body)
  (if (null params)
      body
      (destructuring-bind (param &rest more) params
        `#'(lambda (,param) (curry* ,more ,body)))) )

(defun curry-apply (f args)
  (if (null args)
      f
      (curry-apply (funcall f (first args)) (rest args))))

(defun curry-call (f &rest args)
  (curry-apply f args))

(defmacro defcurry (f (x &rest args) body)
  `(defun ,f (,x)
     (curry ,args ,body)))

;;;
;;;    Slightly modified from Graham.
;;;    
;(defun fif (if then &optional else)
;; (defun iffn (if then &optional else)
;;   (if else
;;       #'(lambda (x)
;; 	  (if (funcall if x)
;; 	      (funcall then x)
;; 	      (funcall else x)))
;;       #'(lambda (x)
;; 	  (if (funcall if x)
;; 	      (funcall then x)
;;               nil))))

(defun iffn (if then &optional (else (constantly nil)))
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (funcall else x))))

;;; 
;;;    Multiple args to pred??
;;;    ACL pg. 110 CONJOIN/DISJOIN accept multiple args to pred, but...
;;;    Clojure's version takes potentially multiple predicates and applies them all to 0+ args.
;;;    That's easy enough to replicate with Graham's simpler version which only directly handles 1 arg:
;;;    (every (every-pred #'integerp #'oddp #'plusp #'(lambda (x) (zerop (mod x 7)))) '(7 21 35))
;;;    

;; (defun fint (fn &rest fns)
;;   (if (null fns)
;;       fn
;;       (let ((chain (apply #'fint fns)))
;; 	#'(lambda (x)
;; 	    (and (funcall fn x) (funcall chain x)))) ))

;;;    COMPOSE no required args
;;;    EVERY-PRED at least 1
;; (defun every-pred (p &rest ps)
;;   (if (null ps)
;;       p
;;       (reduce #'(lambda (result f)
;;                   #'(lambda (x)
;;                       (and (funcall result x) (funcall f x))))
;; ;                      (and (funcall f x) (funcall result x))))
;;               ps
;; ;              :from-end t
;;               :initial-value p)))

;;;
;;;    Same approach as COMPOSE...
;;;    
;; (defun every-pred (p &rest ps)
;;   (if (null ps)
;;       p
;;       (destructuring-bind (p1 . more) ps
;;         (if (null more)
;;             #'(lambda (x) (and (funcall p x) (funcall p1 x)))
;;             (reduce #'every-pred ps :initial-value p)))) )

;;;
;;;    Back to Graham's way.
;;;    
(defun every-pred (p &rest ps)
  (if (null ps)
      p
      (let ((chain (apply #'every-pred ps)))
        #'(lambda (x) (and (funcall p x) (funcall chain x)))) ))

;;;
;;;    Graham's does appear to be slightly faster:
;;;
;; * (time (dotimes (i 1000000) (filter (every-pred #'alpha-char-p #'lower-case-p #'char-upcase) "Is this not pung?")))
;; Evaluation took:
;;   0.588 seconds of real time
;;   0.588153 seconds of total run time (0.588153 user, 0.000000 system)
;;   [ Run times consist of 0.011 seconds GC time, and 0.578 seconds non-GC time. ]
;;   100.00% CPU
;;   2,117,341,076 processor cycles
;;   352,011,040 bytes consed

;;;
;;;    vs. REDUCE (The COMPOSE way)
;;;
;; * (time (dotimes (i 1000000) (filter (every-pred #'alpha-char-p #'lower-case-p #'char-upcase) "Is this not pung?")))
;; Evaluation took:
;;   0.640 seconds of real time
;;   0.640302 seconds of total run time (0.640287 user, 0.000015 system)
;;   [ Run times consist of 0.014 seconds GC time, and 0.627 seconds non-GC time. ]
;;   100.00% CPU
;;   2,305,244,700 processor cycles
;;   415,988,880 bytes consed

;; (defun fun (fn &rest fns)
;;   (if (null fns)
;;       fn
;;       (let ((chain (apply #'fun fns)))
;; 	#'(lambda (x)
;; 	    (or (funcall fn x) (funcall chain x)))) ))

;; (defun some-pred (p &rest ps)
;;   (if (null ps)
;;       p
;;       (destructuring-bind (p1 . more) ps
;;         (if (null more)
;;             #'(lambda (x) (or (funcall p x) (funcall p1 x)))
;;             (reduce #'some-pred ps :initial-value p)))) )

(defun some-pred (p &rest ps)
  (if (null ps)
      p
      (let ((chain (apply #'some-pred ps)))
        #'(lambda (x) (or (funcall p x) (funcall chain x)))) ))

;;;
;;;    Not sold on these three...Seems like forced refactoring.
;;;    Refactor simply because there is a pattern.
;;;    -Produces inherently non tail recursive functions (LREC).
;;;    -A lot of boilerplate!
;;;    -Functions such as EVERY already exist!
;;;    
;; (defun lrec (f &optional (base nil base-provided-p))
;;   (labels ((no-base (l)
;;              (unless (null l)
;;                (funcall f (first l) #'(lambda () (no-base (rest l)))) ))
;;            (value-base (l)
;;              (if (null l)
;;                  base
;;                  (funcall f (first l) #'(lambda () (value-base (rest l)))) ))
;;            (function-base (l)
;;              (if (null l)
;;                  (funcall base)
;;                  (funcall f (first l) #'(lambda () (function-base (rest l)))) )))
;;     (cond ((not base-provided-p) #'no-base)
;;           ((functionp base) #'function-base)
;;           (t #'value-base))))

;; (defun ttrav (f &optional (base #'identity))
;;   (labels ((value-base (tree)
;;              (if (atom tree)
;;                  base
;;                  (funcall f 
;;                           (value-base (car tree))
;;                           (when (cdr tree)
;;                             (value-base (cdr tree)))) ))
;;            (function-base (tree)
;;              (if (atom tree)
;;                  (funcall base tree)
;;                  (funcall f
;;                           (function-base (car tree))
;;                           (when (cdr tree)
;;                             (function-base (cdr tree)))) )))
;;     (cond ((functionp base) #'function-base)
;;           (t #'value-base))))

;; (defun trec (f &optional (base #'identity))
;;   (labels ((value-base (tree)
;;              (if (atom tree)
;;                  base
;;                  (funcall f tree
;;                           #'(lambda () (value-base (car tree)))
;;                           #'(lambda () (when (cdr tree)
;;                                          (value-base (cdr tree)))) )))
;;            (function-base (tree)
;;              (if (atom tree)
;;                  (funcall base tree)
;;                  (funcall f tree
;;                           #'(lambda () (function-base (car tree)))
;;                           #'(lambda () (when (cdr tree)
;;                                          (function-base (cdr tree)))) ))))
;;     (cond ((functionp base) #'function-base)
;;           (t #'value-base))))

;;;
;;;    Why???
;;;
;; `(loop for ,var from ,start to ,stop
;;        do ,@body)

;; (defmacro for ((var start stop) &body body)
;;   (let ((gstop (gensym)))
;;     `(do ((,var ,start (1+ ,var))
;;           (,gstop ,stop))
;;          ((> ,var ,gstop))
;;        ,@body)))

(defmacro for ((var start stop) &body body)
  (with-gensyms (gstop)
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))


;;;
;;;    Based on Clojure's `if-let`.
;;;    Binding only created in case where test succeeds.
;;;    
(defmacro if-let ((var test) then else)
  (with-gensyms (result)
    `(let ((,result ,test))
       (if ,result
           (let ((,var ,result))
             ,then)
           ,else))))

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))
					 
;;;
;;;    If any test fails, the whole body is skipped.
;;;    
(defmacro when-let* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((var test) &rest more) bindings
        `(let ((,var ,test))
           (when ,var
             (when-let* ,more ,@body)))) ))

;;;
;;;    Given a list of COND-LET clauses scavenge all variable occurrences to determine the full set of variables.
;;;    Not all variables may be named in all clauses, and there will be duplicates among the clauses.
;;;
;;;    Example:
;;;    (condlet (((= 1 2) (x (princ 'a)) (y (princ 'b))) --> X, Y
;;;              ((= 1 1) (y (princ 'c)) (x (princ 'd))) --> Y, X
;;;              (t (x (princ 'e)) y (z (princ 'f))))    --> X, Y, Z
;;;      (list x y z)))
;;;
;;;    As with LET, the binding clauses may take 3 forms:
;;;    (x <INIT-FORM>)
;;;    (y)
;;;    z
;;;      
(defun extract-vars (clauses)
  (let ((bindforms (mappend #'rest clauses)))
    (remove-duplicates (mapcar #'extract-var bindforms))))

(defun extract-var (bindform)
  (if (consp bindform)
      (first bindform) 
      bindform))

(defmacro cond-let (clauses &body body)
  (with-gensyms (bodyfn)
    (let* ((vars (extract-vars clauses))
           (var-table (mapcar #'(lambda (v) (cons v (gensym))) vars)))
      (labels ((lookup (var)
                 (cdr (assoc var var-table)))
               (build-clause (clause)
                 (destructuring-bind (test . bindforms) clause
                   `(,test (let ,(build-bindings bindforms)
                             (,bodyfn ,@(mapcar #'lookup vars)))) )) ; Set up call to body
               (build-bindings (bindforms)
                 (let* ((bindings (mapcar #'(lambda (bindform)
                                              (if (consp bindform)
                                                  (destructuring-bind (var &optional (initial nil initialp)) bindform
                                                    (if initialp
                                                        (list (lookup var) initial) ; (x 1) => (#:G123 1)
                                                        (list (lookup var)))) ; (y)   => (#:G124)
                                                  (lookup bindform))) ; z => #:G125
                                          bindforms))
                        (bound-vars (mapcar #'extract-var bindings))
                        (unbound-vars (loop for (nil . gensym) in var-table
                                            unless (member gensym bound-vars)
                                            collect gensym)))
                   (append bindings unbound-vars))))
        `(flet ((,bodyfn ,vars ; Arbitrary order (after duplicates removed) of params. Must match invocation order, i.e., in VAR-TABLE alist.
                  ,@body))
           (cond ,@(mapcar #'build-clause clauses)))) )))

;; (defmacro condlet (clauses &body body)
;;   (let ((bodfn (gensym))
;;         (vars (mapcar #'(lambda (v) (cons v (gensym)))
;;                       (remove-duplicates
;;                         (mapcar #'car 
;;                                 (mappend #'cdr clauses))))))
;;     `(labels ((,bodfn ,(mapcar #'car vars)
;;                  ,@body))
;;        (cond ,@(mapcar #'(lambda (cl)
;;                            (condlet-clause vars cl bodfn))
;;                        clauses)))))

;; (defun condlet-clause (vars cl bodfn)
;;   `(,(car cl) (let ,(mapcar #'cdr vars)
;;                 (let ,(condlet-binds vars cl)
;;                   (,bodfn ,@(mapcar #'cdr vars))))))

;; (defun condlet-binds (vars cl)
;;   (mapcar #'(lambda (bindform)
;;               (if (consp bindform)
;;                   (cons (cdr (assoc (car bindform) vars))
;;                         (cdr bindform))))
;;           (cdr cl)))

(defun transform-clause (clause predicate value)
  (ecase (length clause)
    (1 `(t ,@clause))
    (2 `((funcall ,predicate ,(first clause) ,value) ,(second clause)))
    (3 (destructuring-bind (target delimiter fn) clause
         (assert (eq delimiter :>>) () "Malformed delimiter: ~S" delimiter)
         `((funcall ,predicate ,target ,value) (funcall ,fn (funcall ,predicate ,target ,value)))) )))

(defmacro condp (pred val &rest clauses)
  (let ((predicate (make-symbol "PREDICATE"))
        (value (make-symbol "VALUE")))
    `(let ((,predicate ,pred)
           (,value ,val))
       (assert (functionp ,predicate) () "Predicate must be a function: ~A" ,predicate)
       (cond ,@(loop for clause in clauses
                     collect (transform-clause clause predicate value)))) ))

;; (defmacro if3 (test true false uncertain)
;;   `(case ,test
;;      ((nil) ,false)
;;      (? ,uncertain) ; Only matches core::?  !!!
;;      (t ,true)))

;; (defmacro if3 (test true false uncertain)
;;   "Three-valued logic: true, false, uncertain. Uncertainty is expressed as any symbol whose name is \"?\""
;;   (with-gensyms (result)
;;     `(let ((,result ,test))
;;        (typecase ,result
;;          (symbol (cond ((null ,result),false)
;;                        ((string= "?" ,result) ,uncertain)
;;                        (t ,true)))
;;          (otherwise ,true)))) )

(defmacro if3 (test true false uncertain)
  "Three-valued logic: true, false, uncertain. Uncertainty is expressed as any symbol whose name is \"?\""
  (with-gensyms (result)
    `(let ((,result ,test))
       (cond ((null ,result) ,false)
             ((and (symbolp ,result) (string= "?" ,result)) ,uncertain)
             (t ,true)))) )

;; (defmacro nif (expr pos zero neg)
;;   `(case (truncate (signum ,expr))
;;      (1 ,pos)
;;      (0 ,zero)
;;      (-1 ,neg)))

;;;
;;;    No good. Only covers rationals, double-precision floats...
;;;    
;; (defmacro nif (expr pos zero neg)
;;   `(case (signum ,expr)
;;      ((1 1d0) ,pos)
;;      ((0 0d0) ,zero)
;;      ((-1 -1d0) ,neg)))

(defmacro nif (expr pos zero neg)
  "Conditional evaluation based on sign of EXPR."
  (with-gensyms (v)
    `(let ((,v ,expr))
       (cond ((plusp ,v) ,pos)
             ((zerop ,v) ,zero)
             (t ,neg)))) )

(defmacro in (expr choices)
  (with-gensyms (target)
    `(let ((,target ,expr))
       (or ,@(mapcar #'(lambda (choice)
                         `(eql ,target ,choice))
                     choices)))) )

;;;
;;;    Suppress evaluation
;;;    
(defmacro inq (expr choices)
  `(in ,expr ,(mapcar #'(lambda (choice)
                          `',choice)
                      choices)))

(defmacro in-if (f choices)
  (with-gensyms (test)
    `(let ((,test ,f))
       (or ,@(mapcar #'(lambda (choice)
                         `(funcall ,test ,choice))
                     choices)))) )

;;;
;;;    Variant of CASE. Key forms are evaluated.
;;;    
(defmacro >case (expr &rest clauses)
  (with-gensyms (v)
    `(let ((,v ,expr))
       (cond ,@(mapcar #'(lambda (clause) (>casex v clause)) clauses)))) )

(defun >casex (valsym clause)
  (destructuring-bind (key . forms) clause
    (cond ((consp key) `((in ,valsym ,key) ,@forms))
          ((inq key (t otherwise)) `(t ,@forms))
          (t (error "Bad >case clause")))) )

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

;; (shift0 3 1 (mod 1 3))
;; ;;;
;; ;;;    Convert 1-based index to column number:
;; ;;;    1 -> 1
;; ;;;    2 -> 2
;; ;;;    3 -> 3
;; ;;;    4 -> 1
;; ;;;    ...
;; (defun bin-column (bin)
;;   (assert (< 0 bin (1- roulette-bins)))
;;   (+ (mod (+ bin 2) 3) 1))

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
  (with-gensyms (result)
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
;(set-macro-character #\[ #'(lambda (stream ch)
;                             `(vector ,@(funcall (get-dispatch-macro-character #\# #\() stream ch nil))))
(set-syntax-from-char #\] #\))

;;;
;;;    Hash table syntax: {"pung" 'foo "bar" 'baz}
;;;    
(set-macro-character #\{ #'(lambda (stream ch)
                             (declare (ignore ch))
                             (with-gensyms (table key value)
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
;;;    (macroexpand-1 '#[1 (1+ 4)]) => (CORE:MAKE-RANGE 1 (1+ 4))
;;;    
(set-dispatch-macro-character #\# #\[
  #'(lambda (stream ch arg)
      (declare (ignore ch arg))
      (destructuring-bind (m &optional n step) (read-delimited-list #\] stream t)
        (if step
            (if (and (numberp step)
                     (or (and (numberp m) (numberp n))
                         (and (characterp m) (characterp n))))
                `',(range m n step)
                `(range ,m ,n ,step))
            (if n
                (if (or (and (numberp m) (numberp n))
                        (and (characterp m) (characterp n)))
                    `',(range m n)
                    `(range ,m ,n))
                (if (or (numberp m) (characterp m))
                    `',(range m)
                    `(range ,m)))) )))

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
;  (print expr)
  (multiple-value-bind (expanded expandedp) (macroexpand-1 expr env)
    (cond (expandedp (macroexpand-all expanded env))
          ((atom expanded) expanded)
          (t (cons (first expanded)
                   (mapcar #'(lambda (expr) (macroexpand-all expr env)) (rest expanded)))) )))

(defmacro destructure ((&rest bindings) &body body)
  (cond ((null bindings) `(progn ,@body))
        (t (destructuring-bind (binding expression . rest) bindings
             (if (null rest)
                 `(destructuring-bind ,binding ,expression
                    ,@body)
                 `(destructuring-bind ,binding ,expression
                    (destructure ,rest
                                 ,@body)))) )))

;;;
;;;    Display the packages to which each of the symbols in a form belong.
;;;    
(defun analyze-tree (obj)
  (cond ((symbolp obj) (intern (package-name (symbol-package obj))))
        ((atom obj) obj)
        ((null (cdr obj)) (cons (analyze-tree (car obj)) (cdr obj)))
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

;; (defun make-identity-matrix (n)
;;   (map-array-index #'(lambda (elt i j)
;;                        (declare (ignore elt))
;;                        (if (= i j)
;;                            1d0
;;                            0d0))
;;                    (make-array (list n n))))

(defun make-identity-matrix (n &optional (zero 0))
  (let ((a (make-array (list n n) :initial-element 0))
        (one (case zero (0f0 1f0) (0d0 1d0) (otherwise 1))))
    (dotimes (i n a)
      (setf (aref a i i) one))))

(defun sort-symbol-list (symbols)
  (sort (copy-list symbols) #'string-lessp :key #'symbol-name))

;; (defun sort-symbol-list (symbols)
;;   (sort (copy-list symbols) #'(lambda (s1 s2) (string-lessp (symbol-name s1) (symbol-name s2)))) )

;;;
;;;    http://www.amazon.com/Data-Structures-Algorithms-Made-Easy/dp/1466304162
;;;    Problem #2: Find the nth node from the end of the list
;;;    (This is actually the related solution to Problem #5: pg. 58)
;;;    
(defun nth-from-end (n l)
  (do ((l1 l (rest l1))
       (l2 l)
       (i 0 (1+ i)))
      ((null l1) (if (>= i n) l2 nil))
    (when (>= i n)
      (setf l2 (rest l2)))) )

;;;
;;;    Nonsense?
;;;    
(defun partition (l)
  "Partition a list into sublists of the 'odd' and 'even' elements. The corresponding elements will always be grouped together, however, which sublist is considered 'odd' and which 'even' depends on the number of elements in the original list."
  (labels ((partition-aux (l odds evens)
             (if (endp l)
                 (values (elements odds) (elements evens))
                 (partition-aux (rest l) evens (enqueue odds (first l))))) )
    (partition-aux l (make-linked-queue) (make-linked-queue))))

(defun stable-partition (l)
  "Partition a list into sublists of the 'odd' and 'even' elements. The 'odd' and 'even' sublists always reflect the elements' positions (odd or even) in the original list."
  (labels ((partition-odd (l odds evens)
             (if (endp l)
                 (values (elements odds) (elements evens))
                 (partition-even (rest l) (enqueue odds (first l)) evens)))
           (partition-even (l odds evens)
             (if (endp l)
                 (values (elements odds) (elements evens))
                 (partition-odd (rest l) odds (enqueue evens (first l))))) )
    (partition-odd l (make-linked-queue) (make-linked-queue))))

(defun stable-stream-partition (stream)
  "Partition elements of a character stream into sublists of 'odd' and 'even' elements."
  (labels ((partition-odd (odds evens)
             (let ((ch (read-char stream nil)))
               (if (null ch)
                   (values (elements odds) (elements evens))
                   (partition-even (enqueue odds ch) evens))))
           (partition-even (odds evens)
             (let ((ch (read-char stream nil)))
               (if (null ch)
                   (values (elements odds) (elements evens))
                   (partition-odd odds (enqueue evens ch)))) ))
    (partition-odd (make-linked-queue) (make-linked-queue))))

(defun partition-n (n l)
  "Partition the elements of list L into N lists of approximately equal length."
  (let ((result (loop repeat n
                      collect (make-linked-queue))))
    (labels ((partition (l)
               (if (endp l)
                   (mapcar #'elements result)
                   (partition-elt l result)))
             (partition-elt (l queues)
               (if (endp queues)
                   (partition l)
                   (progn (enqueue (first queues) (first l))
                          (partition-elt (rest l) (rest queues)))) ))
      (partition l))))

(defun approximately= (a b &optional (epsilon 1d-6))
  (<= (abs (- a b)) (* epsilon (abs a))))

;;;
;;;    See programs/horners.lisp
;;;    
(defun horners (x coefficients)
  (reduce #'(lambda (a b) (+ (* a x) b)) coefficients :initial-value 0))
