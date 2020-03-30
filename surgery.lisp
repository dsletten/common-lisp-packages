;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               surgery.lisp
;;;;
;;;;   Started:            Tue Feb 11 01:02:11 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;   Common Lisp provides the notion of generalized variables which allows us to treat a "place", such as part
;;;;   of a larger structure, as a variable itself. Given the following list (defvar *l1* (list 1 2 3)) we can, for
;;;;   example, treat its CADR as though it were a distinct variable: (setf (cadr *l1*) 2.0)
;;;;   We can modify the structure of the list this way as well: (push 4/3 (cdr *l1*)). However, since Lisp lists are
;;;;   only singly-linked, if we have an independent reference to the list CONS chain, a change to the sublist may
;;;;   not have an effect on the parent list: (defvar *l2* (cdr *l1*))
;;;;   (push 1.1 *l2*)
;;;;   *l2* => (1.1 4/3 2.0 3)
;;;;   *l1* => (1 4/3 2.0 3)
;;;;   In order to make the expected change here we have to perform surgery on the actual CONS cell to which *l2* refers, as
;;;;   that object is part of the chain that composes *l1*.
;;;;   To insert a new object at the current location (effectively pushing all of the other elements down by one), we have to splice
;;;;   in a new CONS cell
;;;;   (setf (cdr *l2*) (cons (car *l2*) (cdr *l2*))
;;;;         (car *l2*) 1.1))
;;;;     -Make a copy of current CONS and set that as the new CDR. Then replace CAR with new OBJ.
;;;;     
;;;;     Here SETF does not modify *L1* directly--it has no way of knowing what *L1* points to.
;;;;   It is a simpler task to simply wire in a new element _after_ the current one:
;;;;     The operation above manipulates both the CAR and the CDR of the current CONS. A variation (NSPLICE-AFTER) is simpler.
;;;;     It merely modifies the CDR of the current CONS:
;;;;     (setf (cdr l) (cons obj (cdr l)))
;;;;
;;;;    The opposite task of snipping a CONS cell out of the chain is mostly straightforward: simply remove the designated element (no notion of before or after anything...).
;;;;    The only subtlety concerns whether or not the element to be removed is the last CONS.
;;;;    If it is not the final CONS, simply copy the 2nd CONS and route around it: (The previous CDR becomes garbage)
;;;;     (setf (first l) (second l)
;;;;           (rest l) (cddr l)))
;;;;           
;;;;     If we are removing the final CONS, however, just set the CDR of the preceding CONS to NIL: (But we have to detect this before reaching the end of the list if we don't already have pointer.)
;;;;     (setf (rest l) nil)
;;;;
;;;; (defun nsnip (l i)
;;;;   (cond ((null l) (error "Bad index to snip."))
;;;;         ((zerop i) (setf (first l) (second l) ; Copy CAR/CDR of 2nd CONS to this CONS. Thus 1st CONS is copy of 2nd. 2nd CONS becomes redundant (1st CONS circumvents it, points to 3rd CONS.)
;;;;                          (rest l) (cddr l)))
;;;;         ((and (= i 1) (null (cddr l))) (setf (rest l) nil))
;;;;         (t (nsnip (rest l) (1- i)))) )
;;;;
;;;;
;;;;
;;;;
;;;;       The most fundamental idea of this code is how to modify a CONS in place simply given a
;;;;     reference to the CONS itself. In this scenario it is not possible to reassign the
;;;;     reference to another referent. Instead, the referent must be modified to reflect the change.
;;;;
;;;;     a -> b
;;;;     We know b, but we don't know what a is. In order to have an effect on a, we must modify b rather
;;;;     than assigning a new object c:
;;;;     a -> b'  not  a -> c
;;;;
;;;;     Consider the following relationship:
;;;;     (defvar *l1* (list 1 2 3))
;;;;     (defvar *l2* (cdr *l1*))
;;;;
;;;;     The folowing PUSH has no impact on the "parent" list *L1*:
;;;;     (push 9 *l2*)
;;;;     *l2* => (9 2 3)
;;;;     *l1* => (1 2 3)
;;;;     It requires "surgery" to take the tail (2 3) and replace the CAR with 9 while placing the
;;;;     existing CAR, 2, in a new CONS as the new CDR of the head.
;;;;
;;;;     *l1*  *l2*               *l1*  *l2*
;;;;       (1 . (2 . (3 . nil))) => (1 . (9 . (2 . (3 . nil)))
;;;;
;;;;     In other words, (setf (cdr *l2*) (cons (car *l2*) (cdr *l2*))      ; NSPLICE-BEFORE
;;;;                           (car *l2*) 9))
;;;;     -Make a copy of current CONS and set that as the new CDR. Then replace CAR with new OBJ.
;;;;     
;;;;     Here SETF does not modify *L1* directly--it has no way of knowing what *L1* points to.
;;;;
;;;;     The operation above manipulates both the CAR and the CDR of the current CONS. A variation (NSPLICE-AFTER) is simpler.
;;;;     It merely modifies the CDR of the current CONS:
;;;;     (setf (cdr l) (cons obj (cdr l)))
;;;;
;;;;     The inverse operation of removing an element is less complex: simply remove the designated element (no notion of before or after anything...). The only subtlety
;;;;     concerns whether or not the element to be removed is the last CONS. If it is not the final CONS, simply copy the 2nd CONS and route around it: (The previous CDR becomes garbage)
;;;;     (setf (first l) (second l)
;;;;           (rest l) (cddr l)))
;;;;           
;;;;     If we are removing the final CONS, however, just set the CDR of the preceding CONS to NIL: (We have to detect this before reaching the end of the list.)
;;;;     (setf (rest l) nil)
;;;;     
;;;;     Of course, this discussion only applies to the "non-consing" (minimally consing) functions below (names starting with "n").
;;;;     The other functions are non-destructive and rebuild a new structure from the input.
;;;;
;;;;     There are two ways of locating the CONS to be modified:
;;;;     1. As an indexed element of a sequence.
;;;;     2. As an arbitrary node in a (binary) tree where the CAR is the target object.
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
;;;;   Notes: These are mostly tree operations?? Some of this functionality exists for lists as sequence functions...
;;;;       See recipes 2-6 and 10-8 in Recipes.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :surgery 
  (:use :common-lisp :test)
  (:export :ntree-splice-before :ntree-splice-after :ntree-snip :tree-find))

(in-package :surgery)

;;;
;;;    Destructively splice OBJ as the Ith element of L.
;;;    In other words, the existing Ith and further elements are pushed down by one element.
;;;    
(defun nsplice-before (l i obj)
  (cond ((null l) (error "Bad index to splice."))
        ((zerop i) (setf (cdr l) (cons (car l) (cdr l)) 
                         (car l) obj))
        (t (nsplice-before (cdr l) (1- i) obj))))

(deftest test-nsplice-before ()
  (check
   (let ((l (list 10 20 30 40))) (nsplice-before l 2 25) (equal l '(10 20 25 30 40)))
   (let ((l (list 10 20 30 40))) (nsplice-before l 0 3) (equal l '(3 10 20 30 40)))
   (let ((l (list 80))) (nsplice-before l 0 70) (equal l '(70 80)))) )

;;;
;;;    Build a copy of list L with OBJ as the Ith element. Existing elements moved down.
;;;    
(defun splice-before (l i obj)
  (cond ((null l) (error "Bad index to splice."))
        ((zerop i) (cons obj l))
        (t (cons (first l) (splice-before (rest l) (1- i) obj)))) )

(deftest test-splice-before ()
  (check
   (equal (splice-before '(10 20 30 40) 2 25) '(10 20 25 30 40))
   (equal (splice-before '(10 20 30 40) 0 3) '(3 10 20 30 40))
   (equal (splice-before '(80) 0 70) '(70 80))))
   ;; (equal (splice-before '(80) 1 81) '(80)) ; Error
   ;; (equal (splice-before '(80) 5 81) '(80)))) ; Error

;;;
;;;    Destructively splice OBJ after Ith element of L.
;;;    
(defun nsplice-after (l i obj)
  (cond ((null l) (error "Bad index to splice."))
        ((zerop i) (setf (cdr l) (cons obj (cdr l))))
        (t (nsplice-after (cdr l) (1- i) obj))))

(deftest test-nsplice-after ()
  (check
   (let ((l (list 10 20 30 40))) (nsplice-after l 2 35) (equal l '(10 20 30 35 40)))
   (let ((l (list 10 20 30 40))) (nsplice-after l 0 13) (equal l '(10 13 20 30 40)))
   (let ((l (list 80))) (nsplice-after l 0 81) (equal l '(80 81)))) )
;   (let ((l (list 80))) (nsplice-after l 5 81) (equal l '(80 81)))) ) ; Error ...

;;;
;;;    Same results, but different procedures:
;;;
;;;    (let ((l (list 1 3))) (nsplice-after l 0 2) l) ; Only modifies CDR of first CONS.
;;;    (let ((l (list 1 3))) (nsplice-before l 1 2) l) ; Modifies both CAR/CDR of second CONS.

(defun splice-after (l i obj)
  (cond ((null l) (error "Bad index to splice."))
        ((zerop i) (cons (first l) (cons obj (rest l))))
        (t (cons (first l) (splice-after (rest l) (1- i) obj)))) )

(deftest test-splice-after ()
  (check
   (equal (splice-after '(10 20 30 40) 2 35) '(10 20 30 35 40))
   (equal (splice-after '(10 20 30 40) 0 13) '(10 13 20 30 40))
   (equal (splice-after '(80) 0 81) '(80 81))))
   ;; (equal (splice-after '(80) 1 81) '(80)) ; Error
   ;; (equal (splice-after '(80) 5 81) '(80)))) ; Error

;;;
;;;    Consolidate!
;;;    Place OBJ as Ith element of L. Effectively this places OBJ before the existing Ith element
;;;    except when I is the length of L. OBJ is placed before the final "empty" element in this case.
;;;    
(defun splice (l i obj)
  (cond ((zerop i) (cons obj l))
        ((null l) (error "Bad index to splice."))
        (t (cons (first l) (splice (rest l) (1- i) obj)))) )

(deftest test-splice ()
  (check
   (equal (splice '(10 20 30 40) 0 3) '(3 10 20 30 40))
   (equal (splice '(10 20 30 40) 3 35) '(10 20 30 35 40))
   (equal (splice '(10 20 30 40) 4 50) '(10 20 30 40 50))
   (equal (splice '(80) 0 70) '(70 80))
   (equal (splice '(80) 1 81) '(80 81)) 
   (equal (splice '() 0 0) '(0))))

;;;
;;;    Should return orginal list L as result even if L is modified in place.
;;;    - No real precedent with sequence functions. None "splice" to add to possibly empty list.
;;;    
;; (defun nsplice (l i obj)
;;   (labels ((nsplice-aux (l i)
;;           (cond ((zerop i) (setf (rest l) (cons (first l) (rest l)) 
;;                                  (first l) obj))
;;                 ((null l) (error "Bad index to splice."))
;;                 ((and (= i 1) (null (rest l)))
;;                  (setf (rest l) (list obj)))
;;                 (t (nsplice-aux (rest l) (1- i)))) ))
;;     (cond ((null l) (if (zerop i) ; Oddball case. No CONS at all to modify. Can only build new one.
;;                      (cons obj l)
;;                      (error "Bad index to splice.")))
;;        (t (nsplice-aux l i)
;;           l))))

;;;
;;;    Three cases to consider:
;;;    1. Initial list L0 is empty. (No CONS to operate on! So this is the one case that cannot modify "parent" list sharing structure before L0.)
;;;       If I is zero return (LIST OBJ). (This is the eventual state of case 3., but we have to act before we reach here.)
;;;    2. I becomes zero before end of list: (< i (length l0)). Splice OBJ in front of current first elt (NSPLICE-BEFORE).
;;;    3. I is index after final elt, i.e., (= i (length l0)). Final CONS is available once I is one.
;;;       Splice OBJ in new CONS as new tail of L0 (NSPLICE-AFTER).
;;;
;;;    Note how NSPLICE-AUX changes strategy after the first call--F simply becomes NSPLICE-BEFORE for recursive calls.
;;;
(defun nsplice (l0 i obj)
  (labels ((nsplice-before (l)
             (setf (rest l) (cons (first l) (rest l)) 
                   (first l) obj)
             l0)
           (nsplice-after (l)
             (setf (rest l) (list obj))
;            (setf (rest l) (cons obj (rest l)))
             l0)
           (nsplice-aux (l i f)
             (cond ((zerop i) (funcall f l))
                   ((null l) (error "Bad index to splice."))
                   ((and (= i 1) (null (rest l))) (nsplice-after l))
                   (t (nsplice-aux (rest l) (1- i) #'nsplice-before)))) )
    (nsplice-aux l0 i #'(lambda (l) (if (null l) (list obj) (nsplice-before l)))) ))

(deftest test-nsplice ()
  (check
   (let ((l (copy-list '(a b c)))) (equal (nsplice l 0 'x) '(x a b c)))
   (let ((l (copy-list '(a b c)))) (equal (nsplice l 1 'x) '(a x b c)))
   (let ((l (copy-list '(a b c)))) (equal (nsplice l 2 'x) '(a b x c)))
   (let ((l (copy-list '(a b c)))) (equal (nsplice l 3 'x) '(a b c x)))
   (let ((l '())) (equal (nsplice l 0 'x) '(x)))
   (let ((l (copy-list '(a b c)))) (equal (nsplice (cdr l) 2 'x) '(B C X)))
   (let ((l (copy-list '(a b c)))) (equal (nsplice (cddr l) 1 'x) '(C X)))
   (let ((l (copy-list '(a b c)))) (equal (nsplice (cdddr l) 0 'x) '(X)))) ) ; This does not modify L!!

;;;
;;;    No BEFORE/AFTER versions. Simply remove the element at index I.
;;;    
(defun snip (l i)
  (cond ((null l) (error "Bad index to snip."))
        ((zerop i) (rest l)) ; Why are these cases reversed from SPLICE???? You can splice into an empty list--can't snip from one!
        (t (cons (first l) (snip (rest l) (1- i)))) ))

(deftest test-snip ()
  (check
   (equal (snip '(a b c d e) 0) '(b c d e))
   (equal (snip '(a b c d e) 1) '(a c d e))
   (equal (snip '(a b c d e) 2) '(a b d e))
   (equal (snip '(a b c d e) 4) '(a b c d))))

;;;
;;;    Special case when elt to be removed is final elt. Must set CDR of previous CONS to NIL.
;;;    
(defun nsnip (l i)
  (cond ((null l) (error "Bad index to snip."))
        ((and (= i 1) (null (cddr l))) (setf (rest l) nil))
        ((zerop i) (setf (first l) (second l) ; Copy CAR/CDR of 2nd CONS to this CONS. Thus 1st CONS is copy of 2nd. 2nd CONS becomes redundant (1st CONS circumvents it, points to 3rd CONS.)
                         (rest l) (cddr l)))
        (t (nsnip (rest l) (1- i)))) )

(deftest test-nsnip ()
  (check
   (let ((l (copy-list '(a b c d e)))) (nsnip l 0) (equal l '(b c d e)))
   (let ((l (copy-list '(a b c d e)))) (nsnip l 1) (equal l '(a c d e)))
   (let ((l (copy-list '(a b c d e)))) (nsnip l 2) (equal l '(a b d e)))
   (let ((l (copy-list '(a b c d e)))) (nsnip l 3) (equal l '(a b c e)))
   (let ((l (copy-list '(a b c d e)))) (nsnip l 4) (equal l '(a b c d)))) )

;;;; 
;;;; Trees
;;;; Can't consolidate before/after functions?? TARGET specifies location to splice--no way to specifiy target at end of (sub)list??? NIL is same everywhere...
;;;; 

;;;
;;;    Helper function. Find subtree with OBJ as head. I.e., find the CONS with OBJ as CAR.
;;;    OBJ itself may be any value. (Specific :test may need to be applied.)
;;;
;;;    See Touretzky ex. 8.64 and On Lisp pg. 73 (My implementation in lang.lisp: TREE-FIND-IF)
;;;    
(defun tree-find (obj tree &key (test #'eql))
  (cond ((atom tree) nil)
        ((funcall test (car tree) obj) tree)
        (t (or (tree-find obj (car tree) :test test)
               (tree-find obj (cdr tree) :test test))) ))

(deftest test-tree-find ()
  (check
   (equal (tree-find 'a '(a b c)) '(a b c))
   (equal (tree-find 'b '(a b c)) '(b c))
   (equal (tree-find 'c '(a b c)) '(c))
   (equal (tree-find 'a '((b (a c)) (d e))) '(a c))
   (equal (tree-find '(a c) '((b (a c)) (d e)) :test #'equal) '((a c)))
   (equal (tree-find '+ '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))) '(+ N (FACT (- N 1)))) ))

;;;
;;;    Non-destructive. We have to rebuild tree and locate TARGET along the way.
;;;    TARGET must be CONS.
;;;    
(defun tree-splice-before (tree target obj)
  (cond ;((null tree) '()) ; Superfluous?
        ((atom tree) tree)
        ((eq tree target) (cons obj tree))
        (t (cons (tree-splice-before (car tree) target obj)
                 (tree-splice-before (cdr tree) target obj)))) )

(deftest test-tree-splice-before ()
  (check
   (let* ((tree '(a b d e)) ; Simple non-nested list
          (target (tree-find 'd tree)))
     (equal (tree-splice-before tree target 'c) '(a b c d e)))
   (let* ((tree '((a (c d)) e (f ((g)))))
          (target (tree-find 'c tree)))
     (equal (tree-splice-before tree target 'b) '((a (b c d)) e (f ((g)))) ))
   (let* ((tree '((a (c d)) e (f ((g)))))
          (target 'foo))
     (equal (tree-splice-before tree target 'b) tree)) ; COPY-TREE (TARGET not present.)
   (let* ((tree '((a (c d)) e (f ((g)))))
          (target (tree-find 'foo tree)))
     (equal (tree-splice-before tree target 'b) tree)) ; COPY-TREE (TARGET not present.)
   (let* ((tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))) ; Slade's workspace editor example (Ex. 15.7.4)
          (target (tree-find '+ tree)))
     (equal (tree-splice-before tree target '*) '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (* + N (FACT (- N 1)))) )))) ))

; ntree-splice-before? tree-nsplice-before?
;;;
;;;    Destructively splice OBJ into tree at the head of the subtree TARGET.
;;;    Same surgery as NSPLICE-BEFORE. But here we already know directly which CONS to modify.
;;;    
;(defun ntree-splice-before (tree target obj) ; ?!?!?
(defun ntree-splice-before (target obj)
  (when (consp target)
    (setf (cdr target) (cons (car target) (cdr target))
	  (car target) obj)))

;; (deftest test-ntree-splice-before ()
;;   (check
;;    (let ((tree (list 1 2 3 4)))
;;      (ntree-splice-before tree (tree-find 1 tree) 0)
;;      (equal tree '(0 1 2 3 4)))
;;    (let ((tree (list 1 2 3 4)))
;;      (ntree-splice-before tree (tree-find 3 tree) 2.5)
;;      (equal tree '(1 2 2.5 3 4)))
;;    (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
;;        (target (tree-find 'g tree)))
;;      (ntree-splice-before tree target 'k)
;;      (equal tree '(A (B C ((D) E) ((F (K G)))) )))
;;    (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
;;        (target (tree-find '(d) tree :test #'equal)))
;;      (ntree-splice-before tree target '((x) y))
;;      (equal tree '(A (B C (((X) Y) (D) E) ((F (G)))) )))
;;    (let* ((tree (copy-tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))))
;;           (target (tree-find '+ tree)))
;;      (ntree-splice-before tree target '*)
;;      (equal tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (* + N (FACT (- N 1)))) )))) ))

(deftest test-ntree-splice-before ()
  (check
   (let ((tree (list 1 2 3 4)))
     (ntree-splice-before (tree-find 1 tree) 0)
     (equal tree '(0 1 2 3 4)))
   (let ((tree (list 1 2 3 4)))
     (ntree-splice-before (tree-find 3 tree) 2.5)
     (equal tree '(1 2 2.5 3 4)))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target (tree-find 'g tree)))
     (ntree-splice-before target 'k)
     (equal tree '(A (B C ((D) E) ((F (K G)))) )))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target 'foo))
     (ntree-splice-before target 'k) ; No effect, TARGET not present.
     (equal tree tree))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target (tree-find 'foo tree)))
     (ntree-splice-before target 'k) ; No effect, TARGET not present.
     (equal tree tree))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target (tree-find '(d) tree :test #'equal)))
     (ntree-splice-before target '((x) y))
     (equal tree '(A (B C (((X) Y) (D) E) ((F (G)))) )))
   (let* ((tree (copy-tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))))
          (target (tree-find '+ tree)))
     (ntree-splice-before target '*)
     (equal tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (* + N (FACT (- N 1)))) )))) ))

;;;
;;;    Non-destructive. We have to rebuild tree and locate TARGET along the way.
;;;    TARGET must be CONS.
;;;    
(defun tree-splice-after (tree target obj)
  (cond ;((null tree) '()) ; Superfluous?
        ((atom tree) tree)
        ((eq tree target) (cons (car tree) (cons obj (cdr tree))))
        (t (cons (tree-splice-after (car tree) target obj)
                 (tree-splice-after (cdr tree) target obj)))) )

(deftest test-tree-splice-after ()
  (check
   (let* ((tree '(a b d e)) ; Simple non-nested list
          (target (tree-find 'b tree)))
     (equal (tree-splice-after tree target 'c) '(a b c d e)))
   (let* ((tree '(a b c d))
          (target (tree-find 'd tree)))
     (equal (tree-splice-after tree target 'e) '(a b c d e)))
   (let* ((tree '((a (b c)) e (f ((g)))))
          (target (tree-find 'c tree)))
     (equal (tree-splice-after tree target 'd) '((a (b c d)) e (f ((g)))) ))
   (let* ((tree '((a (b d)) e (f ((g)))))
          (target (tree-find 'b tree)))
     (equal (tree-splice-after tree target 'c) '((a (b c d)) e (f ((g)))) ))
   (let* ((tree '((a (c d)) e (f ((g))))) ; COPY-TREE
          (target 'foo))
     (equal (tree-splice-after tree target 'b) tree))
   (let* ((tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))) ; Slade's workspace editor example (Ex. 15.7.4)
          (target (tree-find '+ tree)))
     (equal (tree-splice-after tree target '*) '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ * N (FACT (- N 1)))) )))) ))

;;;
;;;    Destructively splice OBJ into tree as the CDR of the subtree TARGET.
;;;    Same surgery as NSPLICE-AFTER. But here we already know directly which CONS to modify.
;;;    
;(defun ntree-splice-after (tree target obj) ; ?!?!?
(defun ntree-splice-after (target obj)
  (when (consp target)
    (setf (cdr target) (cons obj (cdr target)))) )

(deftest test-ntree-splice-after ()
  (check
   (let ((tree (list 1 2 3 4)))
     (ntree-splice-after (tree-find 1 tree) 1.5)
     (equal tree '(1 1.5 2 3 4)))
   (let ((tree (list 1 2 3 4)))
     (ntree-splice-after (tree-find 4 tree) 4.5)
     (equal tree '(1 2 3 4 4.5)))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target (tree-find 'g tree)))
     (ntree-splice-after target 'h)
     (equal tree '(A (B C ((D) E) ((F (G H)))) )))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target 'foo))
     (ntree-splice-after target 'k) ; No effect, TARGET not present.
     (equal tree tree))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target (tree-find 'foo tree)))
     (ntree-splice-after target 'k) ; No effect, TARGET not present.
     (equal tree tree))
   (let* ((tree (copy-tree '(a (b c ((d) e) ((f (g)))) )))
          (target (tree-find '(d) tree :test #'equal)))
     (ntree-splice-after target '((x) y))
     (equal tree '(A (B C ((D) ((X) Y) E) ((F (G)))) )))
   (let* ((tree (copy-tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))))
          (target (tree-find '+ tree)))
     (ntree-splice-after target '*)
     (equal tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ * N (FACT (- N 1)))) )))) ))

(defun tree-snip (tree target)
  (cond ((atom tree) tree)
        ((eq tree target) (cdr tree))
        (t (cons (tree-snip (car tree) target)
                 (tree-snip (cdr tree) target)))) )

(deftest test-tree-snip ()
  (check
   (let* ((tree '(a b d e)) ; Simple non-nested list
          (target (tree-find 'b tree)))
     (equal (tree-snip tree target) '(a d e)))
   (let* ((tree '(a b c d))
          (target (tree-find 'd tree)))
     (equal (tree-snip tree target) '(a b c)))
   (let* ((tree '((a (b c)) e (f ((g)))))
          (target (tree-find 'c tree)))
     (equal (tree-snip tree target) '((a (b)) e (f ((g)))) ))
   (let* ((tree '((a (b d)) e (f ((g)))))
          (target (tree-find 'b tree)))
     (equal (tree-snip tree target) '((a (d)) e (f ((g)))) ))
   (let* ((tree '((a (c d)) e (f ((g))))) ; COPY-TREE
          (target 'foo))
     (equal (tree-snip tree target) tree))
   (let* ((tree '((a (c d)) e (f ((g))))) ; COPY-TREE
          (target (tree-find 'foo tree)))
     (equal (tree-snip tree target) tree))
   (let* ((tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) ))) ; Slade's workspace editor example (Ex. 15.7.4)
          (target (tree-find '+ tree)))
     (equal (tree-snip tree target) '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (N (FACT (- N 1)))) )))) ))

;;;
;;;    Only works when TARGET is not final CONS. Must replace itself with NIL...
;;;    
(defun ntree-snip (target)
  (when (consp target)
    (setf (first target) (second target)
	  (rest target) (cddr target))))

(deftest test-ntree-snip ()
  (check
   (let* ((tree (copy-tree '(a b d e))) ; Simple non-nested list
          (target (tree-find 'b tree)))
     (ntree-snip target) 
     (equal tree '(a d e)))
   ;; (let* ((tree (copy-tree '(a b c d)))
   ;;        (target (tree-find 'd tree)))
   ;;   (ntree-snip target) 
   ;;   (equal tree '(a b c)))
   ;; (let* ((tree (copy-tree '((a (b c)) e (f ((g)))) ))
   ;;        (target (tree-find 'c tree)))
   ;;   (ntree-snip target) 
   ;;   (equal tree '((a (b)) e (f ((g)))) ))
   (let* ((tree (copy-tree '((a (b d)) e (f ((g)))) ))
          (target (tree-find 'b tree)))
     (ntree-snip target) 
     (equal tree '((a (d)) e (f ((g)))) ))
   (let* ((tree (copy-tree '((a (c d)) e (f ((g)))) )) ; COPY-TREE
          (target 'foo))
     (ntree-snip target) 
     (equal tree tree))
   (let* ((tree (copy-tree '((a (c d)) e (f ((g)))) )) ; COPY-TREE
          (target (tree-find 'foo tree)))
     (ntree-snip target) 
     (equal tree tree))
   (let* ((tree (copy-tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) )))) ; Slade's workspace editor example (Ex. 15.7.4)
          (target (tree-find '+ tree)))
     (ntree-snip target) 
     (equal tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (N (FACT (- N 1)))) )))) 
   (let* ((tree (copy-tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (+ N (FACT (- N 1)))) )))) ; Slade's workspace editor example (Ex. 15.7.4)
          (target (tree-find '+ tree)))
     (ntree-snip target) 
     (ntree-splice-before target '*)
     (equal tree '(DEFUN FACT (N) (COND ((ZEROP N) 0) (T (* N (FACT (- N 1)))) )))) ))

  
;;;
;;;    Remove
;(defun snip-at (tree target)


;; (defun replace-at ...)


;;;
;;;    Broken if L2 is single-element list!!
;;;    
;; (defun nsplice-all (l i l2)
;;   ;; (cond ((zerop i) (let ((head (car l))
;;   ;;                          (tail (cdr l))
;;   (cond ((zerop i) (let      ((new-cons (cons (car l) (cdr l))))
;;                   (setf (car l) (car l2)
;;                         (cdr l) (cdr l2)
;;                         (cdr (last l2)) new-cons)))
;; ;                 (dolist (elt l2)
;;      (t (nsplice-all (cdr l) (1- i) l2))))
                       
(defun nsplice-all (l i l2)
  (cond ((zerop i) (do ((tail (cons (car l) (cdr l)))
                        (list l2 (cdr list))
                        (current l (cdr current)))
                       ((endp list))
                     (setf (car current) (first list)
                           (cdr current) (if (null (cdr list))
                                             tail
                                             (cons nil nil)))) )
        (t (nsplice-all (cdr l) (1- i) l2))))
                       



;;; TREE-SPLICE
  ;; (defun insert-subexpression (expression)
  ;;   (labels ((rebuild (expression context sub)
  ;;           (cond ((atom expression) expression)
  ;;                 ((eq expression context) (cons sub context))
  ;;                 (t (cons (rebuild (car expression) context sub)
  ;;                          (rebuild (cdr expression) context sub)))) ))
  ;;     (with-slots (definition context) expression
  ;;    (let* ((sub (prompt-read "Expression: "))
  ;;           (new-expression (rebuild definition context sub)))
  ;;      (setf definition new-expression)
  ;;      (push sub context)))) )

;;; TREE-SNIP
  ;; (defun delete-car (expression)
  ;;   (labels ((rebuild (expression context)
  ;;           (cond ((atom expression) expression)
  ;;                 ((eq expression context) (cdr context))
  ;;                 (t (cons (rebuild (car expression) context)
  ;;                          (rebuild (cdr expression) context)))) ))
  ;;     (with-slots (definition context) expression
  ;;    (let ((new-expression (rebuild definition context)))
  ;;      (setf definition new-expression
  ;;            context (cdr context)))) )) ; CONSP?


                  
