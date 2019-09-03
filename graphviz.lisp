;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               graphviz.lisp
;;;;
;;;;   Started:            Tue Oct  9 22:57:43 2012
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
;;;;   Notes: Derived from Conrad Barski's _Land of Lisp_ ch. 7.
;;;;
;;;;   The NODES value used below is a list of lists of the form:
;;;;   ((node-1 label-1) (node-2 label-2) ...)
;;;;   For example:
;;;;   ((LIVING-ROOM "You are in the living room of a wizard's house. There is a wizard snoring loudly on the couch.")
;;;;    (GARDEN "You are in a beautiful garden. There is a well in front of you.")
;;;;    (ATTIC "You are in the attic. There is a giant welding torch in the corner."))
;;;;
;;;;   Actually, nodes aren't quite so simple. In the Wizard game, the node name becomes part of the label, so we get:
;;;;     digraph{
;;;;     LIVING_ROOM[label="(LIVING-ROOM \"You are in th..."];
;;;;     GARDEN[label="(GARDEN \"You are in a beaut..."];
;;;;     ATTIC[label="(ATTIC \"You are in the atti..."];
;;;;     LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;;;;     LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;;;;     GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;;;;     ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
;;;;     }
;;;;     
;;;;   Likewise with the test nodes/edges below we get:
;;;;     digraph{
;;;;     A[label="(GRAPHVIZ::A \"A\")"];
;;;;     B[label="(GRAPHVIZ::B \"B\")"];
;;;;     C[label="(GRAPHVIZ::C \"C\")"];
;;;;     D[label="(GRAPHVIZ::D \"D\")"];
;;;;     A->B[label="1"];
;;;;     A->C[label="2"];
;;;;     C->D[label="3"];
;;;;     C->B[label="4"];
;;;;     D->C[label="5"];
;;;;     }
;;;;
;;;;   But what I want is:
;;;;     digraph{
;;;;     A[label="(GRAPHVIZ::A \"A\")"];
;;;;     B[label="(GRAPHVIZ::B \"B\")"];
;;;;     C[label="(GRAPHVIZ::C \"C\")"];
;;;;     D[label="(GRAPHVIZ::D \"D\")"];
;;;;     A->B[label="1"];
;;;;     A->C[label="2"];
;;;;     C->D[label="3"];
;;;;     C->B[label="4"];
;;;;     D->C[label="5"];
;;;;     }
;;;;
;;;;   It comes out reasonably if you're in the package:
;;;;     digraph{
;;;;     A[label="(A)"];
;;;;     B[label="(B)"];
;;;;     C[label="(C)"];
;;;;     D[label="(D)"];
;;;;     A->B[label="1"];
;;;;     A->C[label="2"];
;;;;     C->D[label="3"];
;;;;     C->B[label="4"];
;;;;     D->C[label="5"];
;;;;     }
;;;; 
;;;;   EDGES is a list of lists of the form:
;;;;   ((node-1 (node-i . edge-label-1i) (node-j . edge-label-1j) ...) ...)
;;;;   For example:
;;;;   ((LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;;;;    (GARDEN (LIVING-ROOM EAST DOOR))
;;;;    (ATTIC (LIVING-ROOM DOWNSTAIRS LADDER)))
;;;;
;(load "/Users/dsletten/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/strings.lisp")
(load "/Users/dsletten/lisp/packages/shell.lisp")

(defpackage :graphviz
  (:use :common-lisp :shell :strings)
  (:export :dot-name :dot-label :dot->png :edges->dot :graph->dot :graph->png :nodes->dot :uedges->dot :ugraph->dot :ugraph->png))

(in-package :graphviz)

(defparameter *max-label-length* 30)

;;;
;;;    Node names can only contain alphanumeric chars and _.
;;;    Possible name clash as pung- and pung* both map to pung_.
;;;    
(defun dot-name (expr)
  (cond ((symbolp expr) (dot-name (symbol-name expr))) ; Get rid of package...
        ((stringp expr) (substitute-if-not #\_ #'alphanumericp (princ-to-string expr))) ; Don't include quote delimiters
        (t (substitute-if-not #\_ #'alphanumericp (prin1-to-string expr)))) )

(defun dot-label (expr)
  (if (null expr)
      ""
      (elide (write-to-string expr :pretty nil) *max-label-length*)))

(defun nodes->dot (nodes)
  (dolist (node nodes)
    (format t "~A[label=~S];~%" (dot-name (first node)) (dot-label node))))
;    (format t "~A[label=\"~S\"];~%" (dot-name (first node)) (dot-label node))))

(defun edges->dot (edges)
  (dolist (node edges)
    (dolist (edge (rest node))
      (format t "~A->~A[label=\"~A\"];~%" (dot-name (first node)) (dot-name (first edge)) (dot-label (rest edge)))) ))

(defun graph->dot (nodes edges)
  (format t "digraph{~%")
  (nodes->dot nodes)
  (edges->dot edges)
  (format t "}~%"))

(defun dot->png (file thunk)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (funcall thunk))
  (run-shell-command "dot -Tpng -O ~A" file))

(defun graph->png (file nodes edges)
  (dot->png file #'(lambda () (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (loop for node on edges
        do (loop for edge in (rest (first node))
                 unless (assoc (first edge) (rest node))
                 do (format t "~A--~A[label=\"~A\"];~%" (dot-name (first (first node))) (dot-name (first edge)) (dot-label (rest edge)))) ))

(defun ugraph->dot (nodes edges)
  (format t "graph{~%")
  (nodes->dot nodes)
  (uedges->dot edges)
  (format t "}~%"))

(defun ugraph->png (file nodes edges)
  (dot->png file #'(lambda () (ugraph->dot nodes edges))))

;; (defun uedges->dot (edges)
;;   (mapl #'(lambda (lst)
;;             (mapc #'(lambda (edge)
;;                       (unless (assoc (car edge) (cdr lst))
;;                         (fresh-line)
;;                         (princ (dot-name (caar lst)))
;;                         (princ "--")
;;                         (princ (dot-name (car edge)))
;;                         (princ "[label=\"")
;;                         (princ (dot-label (cdr edge)))
;;                         (princ "\"];")))
;;                   (cdar lst)))
;;         edges))

(defvar *test-nodes* '((a) (b) (c) (d)))
;; (defvar *test-nodes* '((a "A") (b "B") (c "C") (d "D")))
(defvar *test-edges* '((a (b . 1) (c . 2))
                       (c (d . 3) (b . 4))
                       (d (c . 5))))
;; (defvar *test-edges* '((a (b 1) (c 2))
;;                        (c (d 3) (b 4))
;;                        (d (c 5))))

(defvar *test-undirected-edges* '((a (b . 1) (c . 2))
                                  (c (d . 3) (b . 4))
                                  (d (c . 5))))

;;;
;;;    Head first SQL pg. 429
;;;    
(defvar *clown-nodes* '((mister-sniffles) (clarabelle) (snuggles) (babe) (bonzo) (pickles) (scooter) (zippo) (mr-hobo) (elsie)))
(defvar *clown-undirected-edges* '((mister-sniffles (clarabelle . 1) (snuggles . 2))
                                   (clarabelle (babe . 3) (bonzo . 4) (pickles . 5))
                                   (snuggles (scooter . 6) (zippo . 7) (mr-hobo . 8) (elsie . 9))))


