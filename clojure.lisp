;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               clojure.lisp
;;;;
;;;;   Started:            Thu Oct  7 21:12:54 2021
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

(defpackage :clojure
  (:use :common-lisp)
  (:shadow :set :union :intersection :subsetp))

(in-package :clojure)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names
               collect `(,n ',(make-symbol (symbol-name n)))) ; ??
     ,@body))

(defmacro dotuples ((vars l &optional result) &body body)
  "Iterate over list L consuming VARS variables on each iteration. The bindings of these variables are visible in BODY."
  `(loop for ,vars on ,l by #'(lambda (l) (nthcdr ,(length vars) l))
         do ,@body
         finally (return ,result)))

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
                             (with-gensyms (table key value)
                               `(let ((,table (make-hash-table :test #'equalp)))
                                  (dotuples ((,key ,value) (list ,@(read-delimited-list #\} stream t)))
                                    (setf (gethash ,key ,table) ,value))
                                  ,table))))

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

(defclass collection () ())

(defgeneric make-empty (collection)
  (:documentation "Remove all elements from given collection."))

(defgeneric emptyp (collection)
  (:documentation "Test whether or not a collection has any elements."))

(defgeneric size (collection)
  (:documentation "Determine the number of elements in a collection."))

(defgeneric contains (collection obj &key test)
  (:documentation "Does the collection contain the given object?"))

(defgeneric elements (collection)
  (:documentation "Return the elements of the collection as a list."))

(defgeneric copy (collection)
  (:documentation "Return a copy of the given collection."))

(defclass set (collection)
  ((elements :initarg :elements)))

(defun make-set (&key (test #'eql) elements)
  (let ((set (make-instance 'set :elements (make-hash-table :test test))))
    (if elements
        (add-all set elements)
        (values set nil))))

(defmethod print-object ((s set) stream)
  (format stream "#{~{~S~^ ~}}" (elements s)))

;; (defmethod print-object ((s set) stream)
;;   (print-unreadable-object (s stream :type t)
;;     (format stream "~S" (elements s))))

(defmethod contains ((s set) elt &key test)
  (declare (ignore test))
  (gethash elt (slot-value s 'elements)))

(defmethod make-empty ((s set))
  (clrhash (slot-value s 'elements)))

(defmethod size ((s set))
  (hash-table-count (slot-value s 'elements)))

(defmethod emptyp ((s set))
  (zerop (size s)))

(defmethod elements ((s set))
  (hash-keys (slot-value s 'elements)))

(defun hash-keys (h)
  (loop for k being each hash-key in h collect k))

;;;
;;;    Returns two values:
;;;    - Primary value: the new set
;;;    - Secondary value: boolean indicating whether any duplicates were added
;;;    
(defgeneric add-all (set elts))
(defmethod add-all ((s set) elts)
  (let ((duplicatep nil))
    (with-slots (elements) s
      (dolist (elt elts)
        (when (contains s elt)
          (setf duplicatep t))
        (setf (gethash elt elements) t)))
    (values s duplicatep)))

(defgeneric add-elt (set elt))
(defmethod add-elt ((s set) elt)
  (let ((duplicatep (contains s elt)))
    (setf (gethash elt (slot-value s 'elements)) t)
    (values s duplicatep)))

(defgeneric remove-elt (set elt))
(defmethod remove-elt ((s set) elt)
  (values s (remhash elt (slot-value s 'elements))))

(defgeneric union (s1 s2))
(defmethod union ((s1 set) (s2 set))
  (let ((result (make-set :elements (elements s1)))) ; TEST?
    (add-all result (elements s2))
    result))

(defgeneric intersection (s1 s2))
(defmethod intersection ((s1 set) (s2 set))
  (let ((result (make-set))) ; TEST?
    (dolist (elt (elements s1))
      (when (contains s2 elt)
        (add-elt result elt)))
    result))

;;;
;;;    The two sets may have different equality tests!
;;;    
(defgeneric subsetp (s1 s2))
(defmethod subsetp ((s1 set) (s2 set))
  (every #'(lambda (elt) (contains s1 elt)) (elements s2)))

(defgeneric difference (s1 s2))
(defmethod difference ((s1 set) (s2 set))
  (let ((result (make-set :elements (elements s1))))
    (dolist (elt (elements s2))
      (remove-elt result elt))
    result))

(defgeneric set-equal-p (s1 s2))
(defmethod set-equal-p ((s1 set) (s2 set))
  (and (subsetp s1 s2) (subsetp s2 s1)))

(defgeneric symmetric-difference (s1 s2))
(defmethod symmetric-difference ((s1 set) (s2 set))
  (union (difference s1 s2) (difference s2 s1)))

;; (cartesian-product #{1 2} #{'a 'b}) => #<SET ((1 A) (1 B) (2 A) (2 B))>
;; (cartesian-product #{'a 'b} #{1 2}) => #<SET ((A 1) (A 2) (B 1) (B 2))>
;; (defgeneric cartesian-product (s1 s2))
;; (defmethod cartesian-product ((s1 set) (s2 set))
;;   (do ((result (make-set :test (hash-table-test (slot-value s1 'elements))))
;;        (iter1 (iterators:make-set-iterator s1))) ; Fix this dependency!!
;;       ((null (iterators:has-next-p iter1)) result)
;;     (do ((elt1 (iterators:next iter1))
;;          (iter2 (iterators:make-set-iterator s2)))
;;         ((null (iterators:has-next-p iter2)))
;;       (add-elt result (list elt1 (iterators:next iter2)))) ))

