;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               collections.lisp
;;;;
;;;;   Started:            Sat Jan  7 21:43:55 2006
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
;;;;   Notes: Rescue CARTESIAN-PRODUCT from PAIP ch. 2!
;;;;
;;;;

(defpackage collections
  (:use :common-lisp)
  (:export :add-all :add-elt
           :collectionp :contains :copy
           :dequeue :difference
           :elements :emptyp :enqueue
           :front
           :hash-keys :hash-keys-values :hash-values
           :intersection
           :linked-queue
           :make-circular-list :make-empty :make-linked-queue :make-ring-buffer :make-set :make-vector-queue
           :next
           :queue :queuep
           :remove-elt
           :set :set-equal-p :setp :size :subsetp :symmetric-difference
           :union
           :vector-queue)
  (:shadow :intersection :set :subsetp :union))

(in-package :collections)

(defclass collection () ())

(defun collectionp (obj)
  (typep obj '(or collection hash-table sequence)))

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

(defmethod make-empty ((h hash-table))
  (clrhash h))

(defmethod emptyp ((h hash-table))
  (zerop (size h)))

(defmethod size ((h hash-table))
  (hash-table-count h))

(defmethod contains ((h hash-table) obj &key test)
  (declare (ignore test))
  (not (null (gethash obj h))))

(defmethod elements ((h hash-table))
  (mapcar #'list (hash-keys h) (hash-values h)))

;(defmethod copy ((h hash-table)))

(defmethod make-empty ((v vector))
 (error "Not implemented."))

(defmethod emptyp ((v vector))
  (zerop (size v)))

(defmethod size ((v vector))
  (length v))

(defmethod contains ((v vector) obj &key (test #'eql))
  (find obj v :test test))

(defmethod elements ((v vector))
  (coerce v 'list))

;(defmethod copy ((v vector)))

(defclass queue (collection) ())

(defun queuep (obj)
  (typep obj 'queue))

(defmethod contains ((q queue) obj &key (test #'eql))
  (find obj (elements q) :test test))

(defgeneric enqueue (queue obj)
  (:documentation "Add a new element to the end of a queue."))

(defgeneric dequeue (queue)
  (:documentation "Remove and return the first element of a queue."))

(defgeneric front (queue)
  (:documentation "Return the first element in a queue without removing it."))

;;;
;;;    Linked list queue.
;;;    
(defclass linked-queue (queue)
  ((front :initform nil)
   (rear :initform nil)
   (size :reader size :initform 0)))

(defun make-linked-queue ()
  (make-instance 'linked-queue))

(defmethod print-object ((q linked-queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~A" (elements q))))

(defmethod emptyp ((q linked-queue))
  (null (slot-value q 'front)))

(defmethod enqueue ((q linked-queue) item)
  (with-slots (front rear size) q
     (let ((new-last (list item)))
       (if (emptyp q)
           (setf front new-last)
           (setf (cdr rear) new-last))
       (setf rear new-last)
       (incf size)))
  q)

;; (defmethod enqueue ((q linked-queue) item)
;;   (let ((new-last (list item)))
;;     (if (emptyp q)
;;         (setf (linked-queue-front q) new-last)
;;         (setf (cdr (linked-queue-rear q)) new-last))
;;     (setf (linked-queue-rear q) new-last))
;;   (incf (linked-queue-size q))
;;   q)

(defmethod dequeue ((q linked-queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (with-slots (front rear size) q
        (decf size)
        (if (eq front rear)
            (prog1 (front q)
              (make-empty q))
            (pop front)))) )

;; (defmethod dequeue ((q linked-queue))
;;   (if (emptyp q)
;;       (error "Queue is empty.")
;;       (prog1 (pop (linked-queue-front q))
;;         (decf (linked-queue-size q))
;;         (when (emptyp q)
;;           (setf (linked-queue-rear q) nil)))) )

(defmethod front ((q linked-queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (first (slot-value q 'front))))

(defmethod elements ((q linked-queue))
  (slot-value q 'front))

;; (defmethod size ((q linked-queue))
;;   (linked-queue-size q))

(defmethod make-empty ((q linked-queue))
  (with-slots (front rear size) q
    (setf front nil
          rear nil
          size 0)))

(defmethod copy ((q linked-queue))
  (let ((new-queue (make-linked-queue)))
    (with-slots ((old-front front) (old-rear rear) (old-size size)) q
      (with-slots ((new-front front) (new-rear rear) (new-size size)) new-queue
        (setf new-front old-front
              new-rear old-rear
              new-size old-size)))
    new-queue))

;;;
;;;    Vector-based implementation
;;;    
(defvar *default-capacity* 20)
;;;
;;;    Fix FRONT/REAR INITFORMs
;;;    
(defclass vector-queue (queue)
  ((front :initarg :front)
   (rear :initarg :rear)
   (size :reader size :initform 0) ; Just compute (- front rear)...
   (elements :initarg :elements)))

(defun make-vector-queue (&optional (capacity *default-capacity*))
  (make-instance 'vector-queue
                 :front (1- capacity)
                 :rear (1- capacity)
                 :elements (make-sequence 'simple-vector capacity)))

(defmethod print-object ((q vector-queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~A" (elements q))))

(defmethod emptyp ((q vector-queue))
  (= (slot-value q 'front) (slot-value q 'rear)))

(defmethod enqueue ((q vector-queue) item)
  (with-slots (rear size elements) q
    (setf (svref elements rear) item)
    (incf size)
    (if (zerop rear)
        (shift-queue q)
        (decf rear)))
  item)

;; (defmethod enqueue ((q vector-queue) item)
;;   (setf (svref (vector-queue-elements q) (vector-queue-rear q)) item)
;;   (incf (vector-queue-size q))
;;   (when (minusp (decf (vector-queue-rear q)))
;;     (shift-queue q)))

(defmethod dequeue ((q vector-queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (with-slots (front size elements) q
        (prog1 (svref elements front)
          (decf size)
          (decf front)))) )

;; (defmethod dequeue ((q vector-queue))
;;   (if (emptyp q)
;;       (error "Queue is empty.")
;;       (prog1 (svref (vector-queue-elements q) (vector-queue-front q))
;;         (decf (vector-queue-size q))
;;         (decf (vector-queue-front q)))) )

(defmethod front ((q vector-queue))
  (if (emptyp q)
      (error "Queue is empty.")
      (svref (slot-value q 'elements) (slot-value q 'front))))

(defmethod elements ((q vector-queue))
  (with-slots (front rear elements) q
    (loop for i from front above rear
          collect (svref elements i))))
;;   (nreverse (coerce (subseq (slot-value q 'elements) (slot-value q 'rear)) 'list)))
;;   (do ((i (1+ (vector-queue-rear q)) (1+ i))
;;        (result '() (cons (svref (vector-queue-elements q) i) result)))
;;       ((> i (vector-queue-front q)) result)))

;; (defmethod elements ((q vector-queue))
;;   (do ((i (1+ (vector-queue-rear q)) (1+ i))
;;        (result '() (cons (svref (vector-queue-elements q) i) result)))
;;       ((> i (vector-queue-front q)) result)))

;; (defmethod elements ((q vector-queue))
;;   (do ((i (1+ (vector-queue-rear q)) (1+ i))
;;        (result '()))
;;       ((> i (vector-queue-front q)) result)
;;     (push (svref (vector-queue-elements q) i) result)))

(defmethod size ((q vector-queue))
  (slot-value q 'size))

(defmethod make-empty ((q vector-queue))
  (with-slots (front rear size elements) q
    (let ((length (length elements)))
      (setf front (1- length)
            rear (1- length)
            size 0))))

;;;
;;;    Shrink and Grow
;;;    
(defgeneric shift-queue (queue))
(defmethod shift-queue ((q vector-queue))
  (with-slots (front rear size elements) q
    (let ((new-elements (if (> front (/ (length elements) 2))
                            (make-sequence 'simple-vector (* 2 (length elements)))
                            elements)))
      (setf (subseq new-elements (- (length new-elements) size))
            (subseq elements 0 (1+ front)))
      (setf elements new-elements
            front (1- (length new-elements))
            rear (- front size)))) )

;; (defun shift-queue (q)
;;   (let* ((elements (vector-queue-elements q))
;;          (new elements))
;;     (when (> (vector-queue-front q) (/ (vector-queue-size q) 2))
;;       (setq new (make-sequence 'simple-vector (* 2 (vector-queue-size q))))
;;       (setf (vector-queue-elements q) new
;;             (vector-queue-size q) (* 2 (vector-queue-size q))))
;;     (setf (vector-queue-rear q)
;;           (- (vector-queue-size q) 2 (vector-queue-front q)))
;;     (replace new elements :start1 (1+ (vector-queue-rear q)))
;;     (setf (vector-queue-front q) (1- (vector-queue-size q)))) )

(defun make-circular-list (l)
  (let ((result (copy-list l)))
    (setf (cdr (last result)) result)
    result))

;; (defun make-circular-list (l)
;;   (let ((result (copy-list l)))
;;     (rplacd (last result) result)
;;     result))

(defclass circular-queue (queue)
  ((index :accessor index :initform 0)
   (buffer :reader buffer :initarg :contents)
   (size :accessor size)))

(defun make-circular-queue (contents)
  (let ((buffer (make-instance 'ring-buffer :contents (make-array (length contents) :initial-contents contents))))
    (setf (size buffer) (length contents))
    buffer))

;; (defgeneric fullp (buffer)
;;   (:documentation "Test whether or not a buffer has room for any more elements."))

;; (defmethod fullp ((b ring-buffer))
;;   (/= (size b) (length (buffer b))))

(defgeneric next (buffer)
  (:documentation "Return the next element in the buffer."))

(defmethod next ((b circular-queue))
  (let* ((value (aref (buffer b) (index b))))
    (incf (index b))
    (when (= (index b) (size b))
      (setf (index b) 0))
    value))

(defclass ring-buffer ()
  ((index :accessor index :initform 0)
   (buffer :reader buffer :initarg :contents)
   (size :accessor size)))

(defun make-ring-buffer (contents)
  (let ((buffer (make-instance 'ring-buffer :contents (make-array (length contents) :initial-contents contents))))
    (setf (size buffer) (length contents))
    buffer))

;; (defgeneric fullp (buffer)
;;   (:documentation "Test whether or not a buffer has room for any more elements."))

;; (defmethod fullp ((b ring-buffer))
;;   (/= (size b) (length (buffer b))))

;; (defgeneric next (buffer)
;;   (:documentation "Return the next element in the buffer."))

(defmethod next ((b ring-buffer))
  (let* ((value (aref (buffer b) (index b))))
    (incf (index b))
    (when (= (index b) (size b))
      (setf (index b) 0))
    value))

;;;
;;;    Set data type.
;;;    
(defclass set (collection)
  ((elements :initarg :elements)))

(defun make-set (&key (test #'eql) elements)
  (let ((set (make-instance 'set :elements (make-hash-table :test test))))
    (if elements
        (add-all set elements)
        (values set nil))))

;;
;;    Should be type predicate?!
;;    
(defun setp (l &key (test #'eql))
  (not (nth-value 1 (make-set :elements l :test test))))

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

;---------------Hashtable------------------------
;Should be SETHASH? (Doesn't create hashtable)

;;;
;;;    Not quite right... (Doesn't destructure)
;;;    
;; (defmacro sethash (hash &rest entries)
;;   `(loop for (key val) on ',entries by #'cddr
;;     do (setf (gethash key ,hash) val)))
;;     
;;     (defvar *h1* (sethash (make-hash-table :test #'equal) ("Is" 'a) ("this" -9) ("not" 3.4) ("pung?" '(1 2 3))))
;;  or
;;     (sethash *h1* ("Is" 'a) ("this" -9) ("not" 3.4) ("pung?" '(1 2 3)))
;;     
(defmacro sethash (hash &rest entries)
  (let ((h (gensym)))
    `(let ((,h ,hash))
      (dolist (entry ',entries ,h)
        (setf (gethash (first entry) ,h) (second entry)))) ))

(defun hash-keys (h)
  (loop for k being each hash-key in h collect k))

(defun hash-values (h)
  (loop for v being each hash-value in h collect v))

;; (defun vals (h)
;;   (loop for k being the hash-keys in h using (hash-value v)
;; 	collect v))

(defun hash-keys-values (h)
  (loop for k being each hash-key in h using (hash-value v)
        collect k into keys
        collect v into values
        finally (return (values keys values))))
	  
;; (defun keys-vals (h)
;;   (let ((keys (make-linked-queue))
;;         (vals (make-linked-queue)))
;;     (loop for k being each hash-key in h using (hash-value v)
;;           do (enqueue keys k) (enqueue vals v))
;;     (values (elements keys) (elements vals))))

;;;
;;;    See LANG:DOHASH
;;;    (Also ITERATORS:HASH-TABLE-ITERATOR)
;;;    
;; (defmacro hash-loop (((key val) hash) &body body)
;;   (let ((next (gensym))
;;         (more (gensym)))
;;     `(with-hash-table-iterator (,next ,hash)
;;        (loop (multiple-value-bind (,more ,key ,val) (,next)
;;              (unless ,more (return))
;;              ,@body)))) )
