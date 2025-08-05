;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               hanoi.lisp
;;;;
;;;;   Started:            Sun Jun  1 19:29:44 2025
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
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/books/Concise/concise.lisp")

(defpackage :hanoi
  (:use :common-lisp :core)
  (:export :peg :temple :a :b :c :history
           :add :remove :examine :depletedp
           :add-disks :make-temple :can-transfer :transfer
           :add-history :print-history)
  (:shadow :remove :transfer))

(in-package :hanoi)

(declaim (optimize safety))

(defclass peg ()
  ((disks :initform (make-instance 'containers:persistent-linked-stack))))

(defmethod print-object ((p peg) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (disks) p
      (format stream "~A" (or (containers:elements disks) "()")))) )

(defgeneric add (peg disk)
  (:documentation "Add a disk to a peg yielding a new peg."))
(defgeneric remove (peg)
  (:documentation "Remove top disk from a peg yielding a smaller pile."))
(defgeneric examine (peg)
  (:documentation "Look at top disk on a peg without removing it."))
(defgeneric depletedp (peg)
  (:documentation "Is the peg empty?"))

(flet ((initialize-peg (disks)
         (let ((new-peg (make-instance 'peg)))
           (with-slots ((new-disks disks)) new-peg
             (setf new-disks disks))
           new-peg)))
  (defmethod add ((p peg) disk)
    (with-slots (disks) p
      (initialize-peg (containers:push disks disk))))
  (defmethod remove ((p peg))
    (with-slots (disks) p
      (initialize-peg (containers:pop disks)))) )

(defmethod examine ((p peg))
  (with-slots (disks) p
    (containers:peek disks)))

(defmethod depletedp ((p peg))
  (with-slots (disks) p
    (containers:emptyp disks)))

(defclass temple ()
  ((a :reader a :initarg :a :initform (make-instance 'peg) :type peg)
   (b :reader b :initarg :b :initform (make-instance 'peg) :type peg)
   (c :reader c :initarg :c :initform (make-instance 'peg) :type peg)
   (history :reader history :initarg :history :initform (make-instance 'containers:persistent-linked-queue))))

(defun add-disks (peg disks)
  (loop for disk in (reverse disks)
        for peg* = (add peg disk) then (add peg* disk)
        finally (return peg*)))

(defun make-temple (&key (a (make-instance 'peg))
                         (b (make-instance 'peg))
                         (c (make-instance 'peg))
                         (history (make-instance 'containers:persistent-linked-queue)))
  (make-instance 'temple :a a :b b :c c :history history))

(defun can-transfer (src dest)
  (and (not (depletedp src))
       (or (depletedp dest)
           (< (examine src) (examine dest)))) )

(defun transfer (src dest)
  (values (remove src) (add dest (examine src))))


(defun add-history (temple)
  (containers:enqueue (history temple) (list (a temple) (b temple) (c temple))))

(defun print-history (temple)
  (with-slots (history) temple
    (loop for step from 1
          for element in (containers:elements history)
          do (format t "~3,' D: ~A~%" step element))))
