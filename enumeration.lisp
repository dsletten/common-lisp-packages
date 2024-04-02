;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               enumeration.lisp
;;;;
;;;;   Started:            Thu Sep 12 15:11:20 2019
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
;;;;     http://defenum.sourceforge.net/introduction.html
;;;;     https://rosettacode.org/wiki/Enumerations#Clojure
;;;;     http://quickdocs.org/cl-enumeration/api
;;;;     https://common-lisp.net/project/cl-enumeration/
;;;;
;;;;
;(load "/Users/dsletten/lisp/packages/test.lisp")

;(defpackage :enumeration (:use :common-lisp :test) (:export :defenum))
(defpackage :enumeration (:use :common-lisp) (:export :defenum))

(in-package :enumeration)

(defclass enumeration ()
  ()
  (:documentation "A fixed set of pre-defined values."))

(defmethod make-instance :around ((class (eql (find-class 'enumeration))) &rest initargs)
  (find-token initargs)
  (apply #'call-next-method class initargs))

(defgeneric find-token (class args)
  (:documentation "Control access to instance creation."))
                                  
;(defmacro defenum (name slots print-method &rest instances)
(defmacro defenum (name (&rest instances))
  (let* ((init-instances (gensym))
         (values-instance (gensym))
         (slots (mapcar #'(lambda (symbol) (intern (symbol-name symbol))) '(name description)))
         (instances-slot (intern "INSTANCES"))
         (print-method (intern "DESCRIPTION"))
         (values-class (read-from-string (format nil "~S-values" name)))
         (find-method (intern (format nil "FIND-~S" name)))
         (token (gensym))
         (symbol-macros (mapcar #'(lambda (initargs)
                                   (let ((instance-name (first initargs)))
;                                   (let ((instance-name (getf initargs :name)))
                                     `(define-symbol-macro ,instance-name (,find-method ',instance-name))))
                               instances)))
    `(progn
       (defclass ,name (enumeration)
         ,(mapcar #'(lambda (slot-name)
                      `(,slot-name :initarg ,(intern (symbol-name slot-name) 'keyword) :reader ,slot-name))
                  slots))
       (defclass ,values-class ()
         ((,instances-slot :initarg :instances :reader ,instances-slot :type (list ,name))))
       (defgeneric ,find-method (,(first slots)))
       (defmethod print-object ((,name ,name) stream)
         (write-string (,print-method ,name) stream))
       (let ((token ',token))
         (defmethod find-token ((class (eql (find-class ',name))) args)
           (if (eq token (getf :token args))
               t
               (error "Can't create any more ~A!" ,name)))
;       (let* ((,init-instances (mapcar #'(lambda (initargs) (apply #'make-instance ',name initargs)) ',instances))
         (let* ((,init-instances (mapcar #'(lambda (initargs) (make-instance ',name :name (first initargs) :description (second initargs) :token token :allow-other-keys t)) ',instances))
                (,values-instance (make-instance ',values-class :instances ,init-instances)))
           (defmethod ,find-method (,(first slots))
             (find-if #'(lambda (,name) (eql (,(first slots) ,name) ,(first slots))) (,instances-slot ,values-instance)))))
       ,@symbol-macros)))


;;;
;;;    Correct expansion but problems with symbols interned in enumeration package...
;;;    Uh...hard-wired WOOD!!
;;;    
;; (defmacro defenum (name (&rest instances))
;;   (let ((init-instances (gensym))
;;         (values-instance (gensym))
;;         (slots '(name descripton))
;;         (print-method 'description)
;;         (values-class (read-from-string (format nil "~S-values" name)))
;;         (find-method (read-from-string (format nil "find-~S" name)))
;;         (symbol-macros (mapcar #'(lambda (initargs)
;;                                    (let ((instance-name (getf initargs :name)))
;;                                      `(define-symbol-macro ,instance-name (find-wood ',instance-name))))
;;                                instances)))
;;     `(progn
;;        (defclass ,name ()
;;          ,(mapcar #'(lambda (slot-name)
;;                       `(,slot-name :initarg ,(intern (symbol-name slot-name) 'keyword) :reader ,slot-name))
;;                   slots))
;;        (defclass ,values-class ()
;;          ((instances :initarg :instances :reader instances :type (list ,name))))
;;        (defgeneric ,find-method (name))
;;        (defmethod print-object ((,name ,name) stream)
;;          (write-string (,print-method wood) stream))
;;        (let* ((,init-instances (mapcar #'(lambda (initargs) (apply #'make-instance ',name initargs)) ',@instances))
;;               (,values-instance (make-instance ',values-class :instances ,init-instances)))
;;          (defmethod ,find-method (name)
;;            (find-if #'(lambda (,name) (eql (name ,name) name)) (instances ,values-instance))))
;;        ,@symbol-macros)))



;;        ,@(mapcar #'(lambda (initargs)
;;                      (let ((instance-name (getf initargs :name)))
;;                        `(define-symbol-macro ,instance-name (find-wood ',instance-name))))
;;                  instances))))

;;        ,@(mapcar #'(lambda (initargs)
;;                      (let ((instance-name (getf initargs ':name)))
;;                        `(define-symbol-macro ,instande-name (find-wood ',instance-name))))
;;                  ',@instances))))



;; (define-symbol-macro alder (find-wood 'alder))
;; (define-symbol-macro indian-rosewood (find-wood 'indian-rosewood))
;; (define-symbol-macro brazilian-rosewood (find-wood 'brazilian-rosewood))
;; (define-symbol-macro mahogany (find-wood 'mahogany))
;; (define-symbol-macro sitka (find-wood 'sitka))
;;                                   '((:name maple :description "Maple")
;;                                     (:name alder :description "Alder")
;;                                     (:name indian-rosewood :description "Indian Rosewood")
;;                                     (:name brazilian-rosewood :description "Brazilian Rosewood")
;;                                     (:name mahogany :description "Mahogany")
;;                                     (:name sitka :description "Sitka Spruce"))))
