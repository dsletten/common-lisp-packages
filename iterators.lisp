;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               iterators.lisp
;;;;
;;;;   Started:            Sat Jun 30 20:19:29 2012
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
;;;;   Notes: Create WITH- macros: WITH-LIST-ITERATOR, WITH-SET-ITERATOR, WITH-STRING-ITERATOR, WITH-VECTOR-ITERATOR
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :iterators
  (:shadowing-import-from :collections :intersection :set :set-difference :subsetp :union)
  (:use :common-lisp :collections :test)
  (:export :has-next-p
           :make-hash-table-iterator :make-list-iterator :make-set-iterator :make-string-iterator :make-vector-iterator
           :next)
  (:shadow :next)) ; Is this a good idea???

(in-package :iterators)

(defclass iterator ()
  (index
   (contents :initarg :contents)))

(defclass list-iterator (iterator) ())

(defgeneric has-next-p (iterator))
(defgeneric next (iterator))

(defun make-list-iterator (list)
  (assert (listp list))
  (make-instance 'list-iterator :contents list))

(defmethod initialize-instance :after ((i list-iterator) &key)
  (with-slots (index contents) i
    (setf index contents)))

(defmethod has-next-p ((i list-iterator))
  (not (endp (slot-value i 'index))))

(defmethod next :around ((i iterator))
  (if (has-next-p i)
      (call-next-method)
      nil))

(defmethod next ((i list-iterator))
  (with-slots (index) i
    (first index)))

(defmethod next :after ((i list-iterator))
  (with-slots (index) i
    (setf index (rest index))))

(defclass vector-iterator (iterator) ())

(defun make-vector-iterator (vector)
  (assert (vectorp vector))
  (make-instance 'vector-iterator :contents vector))

(defmethod initialize-instance :after ((i vector-iterator) &key)
  (with-slots (index) i
    (setf index 0)))

(defmethod has-next-p ((i vector-iterator))
  (with-slots (index contents) i
    (< index (length contents))))

(defmethod next ((i vector-iterator))
  (with-slots (index contents) i
    (aref contents index)))

(defmethod next :after ((i vector-iterator))
  (with-slots (index contents) i
    (incf index)))

(defclass string-iterator (vector-iterator) ())

(defun make-string-iterator (string)
  (assert (stringp string))
  (make-instance 'string-iterator :contents string))

(defmethod next ((i string-iterator))
  (with-slots (index contents) i
    (char contents index)))

;;;
;;;    WITH-HASH-TABLE-ITERATOR...
;;;    
(defclass hash-table-iterator (list-iterator)
  ((hash-table :initarg :hash-table)))

(defun make-hash-table-iterator (h)
  (assert (hash-table-p h))
  (make-instance 'hash-table-iterator :contents (hash-keys h) :hash-table h))

(defmethod next ((i hash-table-iterator))
  (with-slots (index hash-table) i
    (list (first index) (gethash (first index) hash-table))))

(defclass set-iterator (list-iterator) ())

(defun make-set-iterator (s)
  (assert (typep s 'set))
  (make-instance 'set-iterator :contents (elements s)))

;; (defvar *i* (make-hash-table-iterator {:a 1 :b 2 :c 3}))
;; (loop (unless (has-next-p *i*) (return)) (print (next *i*)))

;; (:C 3) 
;; (:B 2) 
;; (:A 1) 

;; (setf *i* (make-list-iterator '(a b c d e)))
;; (loop (unless (has-next-p *i*) (return)) (print (next *i*)))

;; A 
;; B 
;; C 
;; D 
;; E 

;; (setf *i* (make-vector-iterator [1 2 3 4 5 6]))
;; (loop (unless (has-next-p *i*) (return)) (print (next *i*)))

;; 1 
;; 2 
;; 3 
;; 4 
;; 5 
;; 6 

;; (setf *i* (make-string-iterator "Is this not pung?"))
;; (loop (unless (has-next-p *i*) (return)) (print (next *i*)))

;; #\I 
;; #\s 
;; #\Space 
;; #\t 
;; #\h 
;; #\i 
;; #\s 
;; #\Space 
;; #\n 
;; #\o 
;; #\t 
;; #\Space 
;; #\p 
;; #\u 
;; #\n 
;; #\g 
;; #\? 

;; (defvar *h* {:a 1 :b 2 :c 3})
;; (setf *i* (make-hash-table-iterator *h*))
;; (loop (unless (has-next-p *i*) (return)) (destructuring-bind (key val) (next *i*) (incf (gethash key *h*))))
;; (setf *i* (make-hash-table-iterator *h*))

;; (loop (unless (has-next-p *i*) (return)) (print (next *i*)))

;; (:C 4) 
;; (:B 3) 
;; (:A 2) 

;; (setf *i* (make-set-iterator #{1 2 :k 3 :f "Bob" 4 5}))
;; (loop (unless (has-next-p *i*) (return)) (print (next *i*)))

;; 5 
;; 4 
;; "Bob" 
;; :F 
;; 3 
;; :K 
;; 2 
;; 1 

