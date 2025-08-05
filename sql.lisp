;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               sql.lisp
;;;;
;;;;   Started:            Mon Apr 14 19:32:24 2025
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ :sbcl (load "/home/slytobias/lisp/packages/strings" :verbose nil)
  #- :sbcl (load "/home/slytobias/lisp/packages/strings.lisp" :verbose nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :qmynd))

(defpackage :sql
  (:use :common-lisp :strings)
  (:export :insert :select :delete :update :where
           :connect :create-table
           :execute-query
           :default :auto-increment :pk
           :format-field :format-fields
           :not-null
           :null
           :like :is :between :in :<> :!=)
  (:shadow :delete :null))

(in-package :sql)

(defconstant null 'null)

(defgeneric format-field (field)
  (:documentation "Format Lisp value as string for SQL value."))
(defmethod format-field ((value string)) (format nil "'~A'" (string-substitute "\\'" "'" value)))
;(defmethod format-field ((value symbol)) (format-field (symbol-name value)))
(defmethod format-field ((value symbol)) value)
(defmethod format-field ((value double-float))
  (let ((*read-default-float-format* 'double-float))
    (write-to-string value)))
(defmethod format-field ((value float))
  (format-field (coerce value 'double-float)))
(defmethod format-field ((value number)) (write-to-string value))
(defmethod format-field ((value (eql t))) "true")
(defmethod format-field ((value (eql nil))) "false")
(defmethod format-field ((value (eql null))) "null")

(defun format-fields (row)
  (join (mapcar #'format-field row) ", "))

(defun not-null (s)
  (join (list s "not null") " "))

(defun default (field default)
  (join (list field "default" (write-to-string default)) " "))

(defun auto-increment (field)
  (join (list field "auto_increment") " "))

(defun pk (field)
  (format nil "primary key (~A)" field))

(defun connect (&key host username password database)
  (qmynd:mysql-connect :host host :username username :password password :database database))

(defun execute-query (conn query)
  (qmynd:mysql-query conn query))

(defun create-table (conn table body)
  (execute-query conn (format nil "create table ~A (~A)" table body)))

(defun insert (conn table fields &optional values)
  (cond ((cl:null values) (insert conn table '() fields)) ; Really no FIELDS
        ((cl:null fields) (qmynd:mysql-query conn (format nil "insert into ~A values (~A)" table (format-fields values))))
        (t (qmynd:mysql-query conn (format nil "insert into ~A (~A) values (~A)" table (join fields ", ") (format-fields values)))) ))

(defun insert (conn table group1 &optional group2)
  (if (cl:null group2)
      (qmynd:mysql-query conn (format nil "insert into ~A values (~A)" table (format-fields group1)))
      (qmynd:mysql-query conn (format nil "insert into ~A (~A) values (~A)" table (join group1 ", ") (format-fields group2)))) )

(defun select (conn table fields &optional selector-fn)
  (if (functionp selector-fn)
      (values (qmynd:mysql-query conn (format nil "select ~A from ~A where ~A" (join fields ", ") table (funcall selector-fn))))
      (values (qmynd:mysql-query conn (format nil "select ~A from ~A" (join fields ", ") table)))) )

(defun delete (conn table &optional selector-fn)
  (if (functionp selector-fn)
      (values (qmynd:mysql-query conn (format nil "delete from ~A where ~A" table (funcall selector-fn))))
      (values (qmynd:mysql-query conn (format nil "delete from ~A" table)))) ) ; Warn?!?

(defun update (conn table fields &optional selector-fn)
  (labels ((process (fields)
             (join (mapcar #'(lambda (entry)
                               (destructuring-bind (field value) entry
                                 (if (listp value)
                                     (format nil "~A = ~A" field (join (mapcar #'format-field value) " "))
                                     (format nil "~A = ~A" field (format-field value)))) )
                           fields)
                   ", ")))
    (if (functionp selector-fn)
        (values (qmynd:mysql-query conn (format nil "update ~A set ~A where ~A" table (process fields) (funcall selector-fn))))
        (values (qmynd:mysql-query conn (format nil "update ~A set ~A" table (process fields)))) ))) ; Warn?!?

;; (defmacro where (clause)
;;   (labels ((parse-clause (clause)
;;              (case (first clause)
;;                (and (format nil "(~A)" (join (mapcar #'parse-clause (rest clause)) " and ")))
;;                (or (format nil "(~A)" (join (mapcar #'parse-clause (rest clause)) " or ")))
;;                (not (format nil "not ~A" (apply #'parse-clause (rest clause))))
;;                (otherwise (destructuring-bind (column operator &rest more) clause
;;                             (case operator
;;                               ((= <> != < <= > >= like) (format nil "~A ~A ~A" column operator (apply #'format-field more)))
;;                               (is (apply #'format nil "~A ~A ~A" column operator more))
;;                               (between (apply #'format nil "~A ~A ~A and ~A" column operator (mapcar #'format-field more)))
;;                               (in (format nil "~A ~A (~A)" column operator (apply #'format-fields more)))) )))) )
;;     `#'(lambda () ,(parse-clause clause))))

;; select email from my_contacts where right(email, 4) = '.net';  <-- Can't use comma!!
;;   Comma not inside a backquote.
;; (funcall (where (right(email 4) = ".net"))) => NIL
;; (funcall (where ("right(email 4)" = ".net"))) => "right(email 4) = '.net'"
(defmacro where (clause)
  (labels ((parse-clause (clause)
             (case (first clause)
               (and `(format nil "(~A)" (join (list ,@(mapcar #'parse-clause (rest clause))) " and ")))
               (or `(format nil "(~A)" (join (list ,@(mapcar #'parse-clause (rest clause))) " or ")))
               (not `(format nil "not ~A" ,(apply #'parse-clause (rest clause))))
               (otherwise (destructuring-bind (column operator &rest more) clause
                            (case operator
;                              ((= <> != < <= > >= like) `((lambda (more) (format nil "~A ~A ~A" ,column ',operator more)) (format-field ,@more)))
;                              ((= <> != < <= > >= like) `(format nil "~A ~A ~A" ,column ',operator (format-field ,@more)))
                              ((= <> != < <= > >= like) `(format nil ,(format nil "~A ~A ~~A" column operator) (format-field ,@more)))
                              (not (assert (member (first more) '(like in))) (parse-clause (list operator (list* column more))))
                              (is `(format nil ,(format nil "~A ~A ~~A" column operator) (format-field ,@more)))
                              (between `(apply ,#'format nil ,(format nil "~A ~A ~~A and ~~A" column operator) (mapcar ,#'format-field (list ,@more))))
;                              (in `(format nil ,(format nil "~A ~A (~~A)" column operator) (format-fields (list ,@more)))) )))) ))
                              (in `(format nil ,(format nil "~A ~A (~~A)" column operator) (format-fields (list ,@(first more))) )))) ))))
    `#'(lambda () ,(parse-clause clause))))

;; (where (foo = bar))
;; #'(lambda () (format nil "foo = ~A" (format-field bar)))

;; (let ((bar "pung")) (funcall (where (foo = bar))))
;; "FOO = 'pung'"
;; (funcall (where (foo = "pung")))
;; "FOO = 'pung'"

;; (where (foo like bar))
;; #'(lambda () (format nil "foo like ~A" (format-field bar)))

;; (let ((bar "pung")) (funcall (where (foo like bar))))
;; "FOO LIKE 'pung'"
;; (let ((bar "%pung%")) (funcall (where (foo like bar))))
;; "FOO LIKE '%pung%'"

;; (funcall (where (drink_name not like "%b%")))
;; "not DRINK_NAME LIKE '%b%'"
;; (funcall (where (not (drink_name like "%b%"))))
;; "not DRINK_NAME LIKE '%b%'"

;; Illegal?
;; (funcall (where (drink_name not = "foo")))
;; "not DRINK_NAME = 'foo'"

;; (where (main in ("soda" "iced tea")))
;; (where (main in "soda" "iced tea"))
;; "MAIN IN ('soda', 'iced tea')"
;; (let ((s "iced tea")) (funcall (where (main in "soda" s))))
;; "MAIN IN ('soda', 'iced tea')"
