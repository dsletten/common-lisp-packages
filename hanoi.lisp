;#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               hanoi.lisp
;;;;
;;;;   Started:            Sat Aug 26 18:30:14 2023
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
;;;;   ~/lisp/books/Winston/3e/ch05.lisp
;;;;
;;;;   ~/io/io/samples/misc/Hanoi.io
;;;;   ~/logo/mswlogo/EXAMPLES/MISC/HANOI.LGO
;;;;   ~/dylan/opendylan-2024.1/sources/examples/console/towers-of-hanoi/hanoi.dylan
;;;;   ~/java/books/JustJava/CDROM/goodies/jikes/jikesos/jt/derek/src/Hanoi.java
;;;;   ~/lisp/books/Art/abc/ex/hanoi/hanoi.cmd
;;;;   ~/lisp/books/Tatar/ch05/tower-of-hanoi.lisp
;;;;   ~/lisp/xlisp/LSP/HANOI.LSP
;;;;   ~/lisp/pclisp/HANOI.L  (Same as above)
;;;;   ~/serval/recovery/WebServer/javascript/Objects/MyNa/wroxcod6/ch4code/hanoi1.js
;;;;   ~/lisp/ACL2/acl2/books/misc/hanoi.lisp
;;;;   ~/lisp/lelisp/llib/hanoi.ll
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :hanoi (:use :common-lisp :core :test))

(in-package :hanoi)

(defvar *max-discs* 20)
(defvar *tower-width* (+ (1+ (* 2 *max-discs*)) 6))
(defvar *platform-width* (* 3 *tower-width*))
(defvar *tower-height* (+ *max-discs* 3))
(defvar *platform-height* (+ *tower-height* 3))

(defclass tower ()
  ((capacity :reader capacity :initarg :capacity)))

(defun base-widths (tower)
  (let ((upper (1+ (* 2 (capacity tower)))) )
    (list upper (+ upper 4))))

(defun tower-height (tower)
  (+ (capacity tower) 3))

(defclass platform ()
  ((left :reader left :initarg :left :initform '())
   (center :reader center :initarg :center :initform '())
   (right :reader right :initarg :right :initform '())
   (tower :reader tower :initarg :tower)))

(defun make-platform (&optional (n 10))
  (assert (<= n *max-discs*) () "Maximum of ~D discs allowed." *max-discs*)
  (make-instance 'platform
                 :tower (make-instance 'tower :capacity n)
                 :left (loop for i from 1 upto n collect i)))

(defmethod print-object ((p platform) stream)
  (with-slots (tower) p
    (destructuring-bind (upper lower) (base-widths tower)
      (let* ((height (+ (tower-height tower) 3))
             (width (+ (* 3 lower) 4))
             (frame (apply #'vector (loop repeat height
                                          collect (make-string width :initial-element #\ )))) )
        (labels ((set-char (x y ch)
                   (setf (char (aref frame y) x) ch))
                 (set-string (x y s)
                   (setf (subseq (aref frame y) x) s))
                 (draw-disc (n rod y)
                   (loop for i from (- rod n) to (+ rod n)
                         do (set-char i y #\*))
                   (set-string (- rod n) y (format nil "~D" n)))
                 (rod (n)
                   (floor (- (* n width 1/3) (/ lower 2))))
                 (draw-rod (n)
                   (let ((rod (rod n)))
                     (loop for j from 0 below (- (tower-height tower) 2)
                           do (set-char rod j #\∥))
                     (loop for i from (- rod (capacity tower))
                           repeat (1+ (* 2 (capacity tower)))
                           do (set-char i (- (tower-height tower) 2) #\|))
                     (loop for i from (- rod (capacity tower) 2)
                           repeat (+ (* 2 (capacity tower)) 5)
                           do (set-char i (1- (tower-height tower)) #\|)))) )
          (loop for i below width
                do (set-char i (- height 3) #\/)
                   (set-char i (- height 1) #\/))
          (loop for i below 2
                do (set-char i (- height 2) #\/))
          (loop for i from (- width 2) below width
                do (set-char i (- height 2) #\/))
          (loop for i from 1 to 3 do (draw-rod i))
          (loop for f in (list #'left #'center #'right)
                for i from 1
                do (loop for elt in (funcall f p)
                         for j from (- (tower-height tower) (length (funcall f p)) 2) below (- (tower-height tower) 2)
                         do (draw-disc elt (rod i) j)))
          (loop for row across frame
                do (write-line row stream)))) )))

(defun hanoi-assertion (source destination)
  (assert (and (not (null source))
               (or (null destination)
                   (< (first source) (first destination))))
          ()
          "You have violated the temple!"))

(defgeneric move (platform source destination)
  (:documentation "Move a disc from SOURCE to DESTINATION."))
(defmethod move ((p platform) source destination)
  (prog1 p
    (print p)
    (with-slots (left center right) p
      (case source
        (:left (case destination
                 (:center (hanoi-assertion left center)
                          (setf center (cons (first left) center)
                                left (rest left)))
                 (:right (hanoi-assertion left right)
                         (setf right (cons (first left) right)
                               left (rest left)))) )
        (:center (case destination
                   (:left (hanoi-assertion center left)
                          (setf left (cons (first center) left)
                                center (rest center)))
                   (:right (hanoi-assertion center right)
                           (setf right (cons (first center) right)
                                 center (rest center)))) )
        (:right (case destination
                  (:left (hanoi-assertion right left)
                         (setf left (cons (first right) left)
                               right (rest right)))
                  (:center (hanoi-assertion right center)
                           (setf center (cons (first right) center)
                                 right (rest right)))) )))) )

(defun hanoi (platform)
  (labels ((make-moves (n src dest temp)
             (cond ((= n 1) (move platform src dest))
                   (t (make-moves (1- n) src temp dest)
                      (move platform src dest)
                      (make-moves (1- n) temp dest src)))) )
    (make-moves (length (left platform)) :left :right :center)))
