;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               logic.lisp
;;;;
;;;;   Started:            Sat Nov 27 01:39:46 2010
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

(defpackage logic
  (:use common-lisp)
  (:export :=> :<=> :dual :print-truth-table :truth-table :xor))

(in-package logic)

;;;
;;;    Haskell Craft section 3.1
;;;
;; (defun xor (p q)
;;   (ccase p
;;     ((t) (not q))
;;     ((nil) q)))

;; (defun xor (p q)
;;   (and (or p q)
;;        (not (and p q))))
;; (defmacro xor (p q)
;;   (let ((p1 (gensym))
;;         (q1 (gensym)))
;;     `(let ((,p1 ,p)
;;            (,q1 ,q))
;;        (and (or ,p1 ,q1)
;;             (not (and ,p1 ,q1)))) ))

(defmacro xor (&rest args)
  (case (length args)
    (0 t) ;?
    (1 nil)
    (2 (let ((p1 (gensym))
             (q1 (gensym)))
         `(let ((,p1 ,(first args))
                (,q1 ,(second args)))
            (and (or ,p1 ,q1)
                 (not (and ,p1 ,q1)))) ))
    (otherwise `(xor ,(first args) (xor ,@(rest args)))) ))

;;;
;;;    Implication is asymmetric. It returns either T or the value of Q, which
;;;    may be a generalized boolean value.
;;;    
;; (defmacro => (p q)
;;   `(or (not ,p) ,q))

;; (defmacro => (p q)
;;   `(if ,p ,q t))

;;;
;;;    Left-associative
;;;    
(defmacro -> (&rest args)
  (destructuring-bind (p q . rest) args
      (if (null rest)
          `(=> ,p ,q)
          (let ((ps (butlast args))
                (q (last args)))
            `(or (not (-> ,@ps)) ,@q)))) )

;;;
;;;    Right-associative
;;;    
(defmacro => (&rest args)
  (destructuring-bind (p q . rest) args
    (if (null rest)
        `(or (not ,p) ,q)
        (let ((ps (butlast args))
              (q (last args)))
          `(=> (and ,@ps) ,@q)))) )

(defmacro => (&rest args)
  (case (length args)
    ((0 1) (error "?!"))
    (2 `(or (not ,(first args)) ,(second args)))
    (otherwise `(=> ,(first args) (=> ,@(rest args)))) ))

;; (defmacro <=> (p q)
;;   (let ((p1 (gensym))
;;         (q1 (gensym)))
;;     `(let ((,p1 ,p)
;;            (,q1 ,q))
;;        (and (=> ,p1 ,q1)
;;             (=> ,q1 ,p1)))) )

(defmacro <=> (&rest args)
  (case (length args)
    (0 nil) ;?
    (1 t)
    (2 (let ((p1 (gensym))
             (q1 (gensym)))
         `(let ((,p1 ,(first args))
                (,q1 ,(second args)))
            (or (and ,p1 ,q1)
                (not (or ,q1 ,p1)))) ))
    (otherwise `(<=> ,(first args) (<=> ,@(rest args)))) ))

;; (defmacro <=> (&rest args)
;;   (case (length args)
;;     (0 nil) ;?
;;     (1 t)
;;     (2 (let ((p1 (gensym))
;;              (q1 (gensym)))
;;          `(let ((,p1 ,(first args))
;;                 (,q1 ,(second args)))
;;             (and (=> ,p1 ,q1)
;;                  (=> ,q1 ,p1)))) )
;;     (otherwise `(<=> ,(first args) (<=> ,(second args) ,@(cddr args)))) ))

;;;
;;;    Does this make sense? No. It's wrong!
;;;    
;; (defmacro xor (&rest args)
;;   `(and (or ,@args) (not (and ,@args))))

(defun truth-permutations (n)
  (if (= n 1)
      (list (list t) (list nil))
      (let ((result (truth-permutations (1- n))))
        (append (mapcar #'(lambda (l) (cons t l)) result)
                (mapcar #'(lambda (l) (cons nil l)) result)))) )

(defmacro truth-table ((&rest vars) &body body)
  (let ((row (gensym)))
    `(mapcar #'(lambda (,row) (destructuring-bind ,vars ,row ,@body)) ',(truth-permutations (length vars)))) )

;; (defmacro truth-table ((&rest vars) &body body)
;;   `(list ,@(mapcar #'(lambda (vals) `(let ,(mapcar #'list vars vals) ,@body)) (truth-permutations (length vars)))))

;(mapcar #'(lambda (vals) `(let ,(mapcar #'list '(p q r) vals))) (truth-permutations 3))

(defun print-table (labels inputs outputs)
  (format t "~{~5A ~} ~%" labels)
  (loop for input in inputs
        for output in outputs
        do (format t "~{~5A ~} ~A~%" input output)))

(defmacro print-truth-table ((&rest vars) &body body)
  (let ((row (gensym))
        (inputs (truth-permutations (length vars)))
        (outputs (gensym)))
    `(let ((,outputs (mapcar #'(lambda (,row) (destructuring-bind ,vars ,row ,@body)) ',inputs)))
       (print-table '(,@vars ,@body) ',inputs ,outputs))))

;;;
;;;    Non short-circuiting &/|
;;;
(defun & (&rest args)
  (every #'identity args))

;;;
;;;    D'oh! What a name!
;;;    
(defun \| (&rest args)
  (some #'identity args))

;;;
;;;    Do we really want a macro here?
;;;
;;;    Instead:
;;;    (dual '(and p (and (not q) (not r))))
;;;    (dual (dual '(and p (and (not q) (not r)))))
;;;    
(defmacro dual (body)
  (rebuild-dual body))

(defun rebuild-dual (expression)
  (cond ((atom expression) (replace-dual-atom expression))
        ((endp (rest expression)) (cons (rebuild-dual (first expression)) '()))
        (t (cons (rebuild-dual (first expression)) (rebuild-dual (rest expression)))) ))

(defun replace-dual-atom (obj)
  (case obj
    (and 'or)
    (or 'and)
    ((t) nil)
    ((nil) t)
    ((xor => <=>) (error "The dual is only defined in terms of AND, OR, and NOT."))
    (otherwise obj)))
