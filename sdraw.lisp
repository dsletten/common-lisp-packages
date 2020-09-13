;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               sdraw.lisp
;;;;
;;;;   Started:            Thu Jul 30 04:11:20 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       This is my rewrite of David Touretzky's SDRAW package from the book
;;;;       "Common Lisp:  A Gentle Introduction to Symbolic Computation" by David S. Touretzky.
;;;;       The Benjamin/Cummings Publishing Co., 1990.
;;;;
;;;;       This version is a hybrid of his 1989/1990 versions (the latter handles circular structures).
;;;;       It also incorporates CLOS to define the abstract concept of a renderer to draw the object.
;;;;       The generic version uses a TTY renderer which simply writes text to a terminal.
;;;;       Various parameters of the renderer are configurable, e.g., DISPLAY-WIDTH.
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
;;;;   Notes: (use-package :sdraw)
;;;;
;;;;
;;;
;;;   Explore circular structure...
;;;   (let ((x (list 1))) (sdraw (cons x x)))
;;;   vs.
;;;   (sdraw (cons #1=(list 1) #1#))
;;;   
;;;   (let ((x (list (gensym))) (y (list 'a (gensym)))) (sdraw (list* y (cons x x) y)))


(defpackage :sdraw 
  (:use :common-lisp)
  (:export :sdraw :sdraw-loop :scrawl :*sdraw-leading-arrow* :*sdraw-print-circle*))

(in-package :sdraw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; The parameters below are in units of characters (horizontal) 
;;; and lines (vertical). They apply to all versions of SDRAW, 
;;; but their values may change if cons cells are being drawn as 
;;; bit maps rather than as character sequences.

(defparameter *sdraw-display-width* 79) 

(defparameter *etc-string* "etc.") 
(defparameter *etc-spacing* 4) 

(defparameter *inter-atom-h-spacing* 3) 
(defparameter *cons-atom-h-arrow-length* 9) 
(defparameter *inter-cons-v-arrow-length* 3) 
(defparameter *cons-v-arrow-offset-threshold* 2) 
(defparameter *cons-v-arrow-offset-value* 1)

(defparameter *sdraw-num-lines* 25 "The maximum depth allocated for the display of an object. Essentially three times the number of nested levels allowed.")

(defparameter *leading-arrow-length* 4)
(defvar *sdraw-leading-arrow* nil)
(defvar *sdraw-print-circle*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW and subordinate definitions.

(defclass renderer ()
  ((display-width :reader display-width :initform *sdraw-display-width* :initarg :display-width)
   (horizontal-atom-cutoff :reader horizontal-atom-cutoff :initarg :horizontal-atom-cutoff)
   (horizontal-cons-cutoff :reader horizontal-cons-cutoff :initarg :horizontal-cons-cutoff)
   (num-lines :reader num-lines :initform *sdraw-num-lines* :initarg :num-lines)
   (vertical-cutoff :reader vertical-cutoff)
   (line-endings :reader line-endings)
   (cycles :reader cycles :initform (make-hash-table :test #'eq :size 20))
   (cycle-label :accessor cycle-label :initform 0)))

(defmethod initialize-instance :after ((r renderer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (num-lines line-endings vertical-cutoff) r
    (setf line-endings (make-array num-lines :initial-element most-negative-fixnum)
          vertical-cutoff (- num-lines 3))))

(defgeneric draw-structure (renderer directions))

(defun sdraw (obj &key
                    (display-width *sdraw-display-width*)
                    (horizontal-atom-cutoff display-width)
                    (horizontal-cons-cutoff (- display-width 14))
                    (num-lines *sdraw-num-lines*))
  (let ((circ-detected nil)
        (sdraw-circular-switch (if (boundp '*sdraw-print-circle*)
                                   *sdraw-print-circle*
                                   *print-circle*))
        (start-col (if *sdraw-leading-arrow* *leading-arrow-length* 0))
        (renderer (make-instance 'tty 
                                 :display-width display-width
                                 :horizontal-atom-cutoff horizontal-atom-cutoff
                                 :horizontal-cons-cutoff horizontal-cons-cutoff
                                 :num-lines num-lines)))
    (declare (special circ-detected sdraw-circular-switch))
    (struct-record-position renderer 0 (- start-col *inter-atom-h-spacing*))
    (let ((first-layout (struct1 renderer obj 0 start-col 0 nil))
          (second-layout (when circ-detected
                           (setf (cycle-label renderer) 0)
                           (fill (line-endings renderer) most-negative-fixnum)
                           (struct-record-position renderer 0 (- start-col *inter-atom-h-spacing*))
                           (struct1 renderer obj 0 start-col 0 t))))
      (draw-structure renderer (or second-layout first-layout))
      (values))))

(defun never-seen-p (cycles obj)
  (null (gethash obj cycles)))

(defun seen-twice-p (cycles obj)
  (numberp (gethash obj cycles)))

(defun needs-label-p (cycles obj)
  (zerop (gethash obj cycles)))

(defun struct1 (renderer obj row root-col adj second-pass-p)
  (cond ((>= row (vertical-cutoff renderer)) (struct-process-etc renderer row root-col adj))
        ((not second-pass-p) 
         (enter-in-hash-table (cycles renderer) obj)
         (struct-first-pass renderer obj row root-col adj))
        (t (struct-second-pass renderer obj row root-col adj))))

;; (defun struct1 (renderer obj row root-col cache) 
;;   (cond ((atom obj)
;;          (struct-process-atom renderer (format nil "~S" obj) row root-col)) 
;;         ((member obj cache :test #'eq)
;;          (struct-process-circ renderer row root-col))
;;         ((>= row (vertical-cutoff renderer))
;;          (struct-process-etc renderer row root-col))
;;         (t (struct-process-cons renderer obj row root-col (cons obj cache)))) )

(defun enter-in-hash-table (cycles obj)
  (declare (special sdraw-circular-switch circ-detected))
  (unless (or (not sdraw-circular-switch)
              (numberp obj)
              (and (symbolp obj) (not (null (symbol-package obj)))) )
    (cond ((never-seen-p cycles obj) (setf (gethash obj cycles) t))
          (t (setf (gethash obj cycles) 0
                   circ-detected t)))) )

(defun struct-first-pass (renderer obj row root-col adj)
  (if (seen-twice-p (cycles renderer) obj)
      (struct-process-circ-reference renderer obj row root-col adj)
      (if (atom obj)
	  (struct-unlabeled-atom renderer (format nil "~S" obj) row root-col adj)
	  (struct-unlabeled-cons renderer obj row root-col adj nil))))

(defun struct-second-pass (renderer obj row root-col adj)
  (cond ((not (seen-twice-p (cycles renderer) obj))
	 (if (atom obj)
	     (struct-unlabeled-atom renderer (format nil "~S" obj) row root-col adj)
	     (struct-unlabeled-cons renderer obj row root-col adj t)))
	((needs-label-p (cycles renderer) obj)
	 (if (atom obj)
	     (struct-label-atom renderer obj row root-col adj)
	     (struct-label-cons renderer obj row root-col adj)))
	(t (struct-process-circ-reference renderer obj row root-col adj))))

;;; Handle the simplest case:  an atom or cons with no #n= label.
(defun struct-unlabeled-atom (renderer atom-string row root-col adj) 
  (let* ((start-col (struct-find-start renderer row root-col adj))
         (end-col (+ start-col adj (length atom-string)))) 
    (cond ((< end-col (horizontal-atom-cutoff renderer))
           (struct-record-position renderer row end-col)
           `(atom ,row ,(+ start-col adj) ,atom-string))
          (t (struct-process-etc renderer row root-col adj)))) )

(defun struct-unlabeled-cons (renderer obj row root-col adj second-pass-p)
  (let* ((cons-start (struct-find-start renderer row root-col adj))
         (car-structure (struct1 renderer
                                 (car obj)
                                 (+ row *inter-cons-v-arrow-length*)
                                 cons-start adj second-pass-p))
         (start-col (third car-structure)))
    (if (>= start-col (horizontal-cons-cutoff renderer))
        (struct-process-etc renderer row root-col adj)
        (progn (struct-record-position renderer
                                       row
                                       (- (+ start-col *cons-atom-h-arrow-length*)
                                          adj *inter-atom-h-spacing*))
        `(cons ,row ,start-col ,car-structure
               ,(struct1 renderer (cdr obj) row
                         (+ start-col *cons-atom-h-arrow-length*) 
                         0 second-pass-p)))) ))

(defun struct-process-etc (renderer row root-col adj)
  "Row is too long. Ellide ending."
  (let ((start-col (struct-find-start renderer row root-col adj)))
    (struct-record-position renderer
                            row
                            (+ start-col adj (length *etc-string*) *etc-spacing*))
    `(msg ,row ,(+ start-col adj) ,*etc-string*)))

;;; Handle objects that need to be labeled with #n=.
;;; Called only on the second pass.
(defun struct-label-atom (renderer obj row root-col adj)
  (assign-label renderer obj)
  (let* ((circ-string (format nil "#~S=" (gethash obj (cycles renderer))))
	 (newadj (struct-find-adj renderer row root-col adj (length circ-string)))
	 (atom-string (format nil "~S" obj))
	 (start-col (struct-find-start renderer row root-col adj))
	 (end-col (+ start-col newadj (length atom-string))))
    (cond ((< end-col (horizontal-atom-cutoff renderer))
	   (struct-record-position renderer row end-col)
           `(atom ,row ,(+ start-col newadj) ,atom-string ,circ-string))
	  (t (struct-process-etc renderer row root-col adj)))) )

(defun struct-label-cons (renderer obj row root-col adj)
  (assign-label renderer obj)
  (let* ((msg (format nil "#~S=" (cycle-label renderer)))
	 (newadj (struct-find-adj renderer row root-col adj (length msg)))
	 (cons-start (struct-find-start renderer row root-col adj))
	 (car-structure
	  (struct1 renderer
                   (car obj)
		   (+ row *inter-cons-v-arrow-length*)
		   cons-start newadj t))
	 (start-col (third car-structure)))
    (if (>= start-col (horizontal-cons-cutoff renderer))
	(struct-process-etc renderer row root-col adj)
	(progn
	  (struct-record-position renderer
                                  row
                                  (- (+ start-col *cons-atom-h-arrow-length*)
				     adj *inter-atom-h-spacing*))
          `(cons ,row ,start-col ,car-structure
                 ,(struct1 renderer (cdr obj) row
                           (+ start-col *cons-atom-h-arrow-length*) 
                           0 t)
                 ,msg)))) )

(defun assign-label (renderer obj)
  (setf (gethash obj (cycles renderer)) (incf (cycle-label renderer))))

;;; Handle circular references by displaying them as #n#.
;;; When called on the first pass, this function always uses a label of 0.
;;; It will get the label right on the second pass.
(defun struct-process-circ-reference (renderer obj row root-col adj)
  "Circular structure detected."
  (let ((start-col (struct-find-start renderer row root-col adj))
        (msg (format nil "#~S#" (gethash obj (cycles renderer)))) )
    (struct-record-position renderer
                            row
                            (+ start-col adj (length msg)))
    `(msg ,row ,(+ start-col adj) ,msg)))

;; (defun struct-process-circ (renderer row root-col)
;;   "Circular structure detected."
;;   (let ((start-col (struct-find-start renderer row root-col)))
;;     (struct-record-position renderer
;;                             row
;;                             (+ start-col (length *circ-string*) *circ-spacing*))
;;     `(msg ,row ,start-col ,*circ-string*)))
 
(defun struct-find-start (renderer row root-col adj)
  (with-slots (line-endings) renderer
    (max root-col 
         (- (+ *inter-atom-h-spacing* (aref line-endings row)) adj))))

(defun struct-find-adj (renderer row col adj size)
  (with-slots (line-endings) renderer
    (let* ((line-end (max 0 (+ *inter-atom-h-spacing*
			       (aref line-endings row))))
	   (newadj (- line-end (- col (max size adj)))))
      (max adj (min (max newadj 0) size)))) )

(defun struct-record-position (renderer row end-col) 
  (with-slots (line-endings) renderer
    (setf (aref line-endings row) end-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SDRAW-LOOP and subordinate definitions.

(defparameter *sdraw-loop-prompt-string* "S> ")

(defun sdraw-loop (&key (display-width *sdraw-display-width*))
  "Read-eval-print loop using sdraw to display results."
  (declare (special display-width))
  (format t "~&Type any Lisp expression, or (ABORT) to exit.~%~%") 
  (sdl1))

(defun sdl1 ()
  (loop
     (format t "~&~A" *sdraw-loop-prompt-string*)
     (force-output)
     (let ((form (read)))
       (shiftf +++ ++ + - form)
       (let ((result (multiple-value-list
                      (handler-case (eval form)
                        (error (condx) condx)))))
         (typecase (first result)
           (error (display-sdl-error result))
           (t (shiftf /// // / result)
              (shiftf *** ** * (first result))
              (display-sdl-result *)))))))

(defun display-sdl-result (result) 
  (declare (special display-width))
  (let* ((*print-circle* (if (boundp '*sdraw-print-circle*)
			     *sdraw-print-circle*
		             *print-circle*))
         (*print-length* nil)
         (*print-level* nil)
         (*print-pretty* nil)
         (full-text (format nil "Result: ~S" result))
         (text (if (> (length full-text) display-width)
                   (ellide full-text display-width)
                   full-text)))
    (sdraw result :display-width display-width)
    (when (consp result)
      (format t "~%~A~%" text))
    (terpri)))

(defun ellide (s width)
  (let ((ellipsis "...)"))
    (concatenate 'string (subseq s 0 (- width (length ellipsis))) ellipsis)))

(defun display-sdl-error (error) 
  (format t "~A~%~%" error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;
;;; SCRAWL and subordinate definitions.

(defparameter *scrawl-prompt-string* "SCRAWL> ") 

(defun scrawl (obj &key (display-width *sdraw-display-width*))
  "Read-eval-print loop to travel through list"
  (declare (special display-width))
  (format t "~&Crawl through list: 'H' for help, 'Q' to quit.~%~%") 
  (let ((scrawl-object obj)
        (scrawl-current-obj obj)
        (extracting-sequence nil))
    (declare (special scrawl-object scrawl-current-obj extracting-sequence))
    (scrawl-start-cmd)
;    (sdraw obj :display-width display-width)
    (scrawl1)))

(defun scrawl1 ()
  (loop
     (format t "~&~A" *scrawl-prompt-string*) 
     (force-output)
     (let ((command (read-uppercase-char)))
       (case command
         (#\A (scrawl-car-cmd))
         (#\D (scrawl-cdr-cmd))
         (#\B (scrawl-back-up-cmd)) 
         (#\S (scrawl-start-cmd))
         (#\H (display-scrawl-help)) 
         (#\Q (return))
         (t (display-scrawl-error))))))

(defun scrawl-car-cmd ()
  (declare (special scrawl-current-obj extracting-sequence))
  (cond ((consp scrawl-current-obj)
         (push 'car extracting-sequence)
         (setf scrawl-current-obj (car scrawl-current-obj))) 
        (t (format t
                   "~&Can't take CAR or CDR of an atom. Use B to back up.~%")))
  (display-scrawl-result))
 
(defun scrawl-cdr-cmd ()
  (declare (special scrawl-current-obj extracting-sequence))
  (cond ((consp scrawl-current-obj)
         (push 'cdr extracting-sequence)
         (pop scrawl-current-obj))
;         (setf *scrawl-current-obj* (cdr *scrawl-current-obj*)))  ;; Symmetry above???
        (t (format t
                   "~&Can't take CAR or CDR of an atom. Use B to back up.~%"))) 
  (display-scrawl-result))

(defun scrawl-back-up-cmd () 
  (declare (special scrawl-object scrawl-current-obj extracting-sequence))
  (cond ((null extracting-sequence) (format t "~&Already at beginning of object."))
        (t (pop extracting-sequence) 
           (setf scrawl-current-obj (extract-obj extracting-sequence scrawl-object))))
  (display-scrawl-result))

(defun scrawl-start-cmd ()
  (declare (special scrawl-object scrawl-current-obj extracting-sequence))
  (setf scrawl-current-obj scrawl-object
        extracting-sequence nil)
  (display-scrawl-result))

;;;
;;;    SEQ is the EXTRACTING-SEQUENCE, a list of CAR/CDR symbols indicating how the top-level object
;;;    has been traversed to this point.
;;;    
(defun extract-obj (seq obj) 
  (reduce #'funcall seq :initial-value obj :from-end t))

(defun get-car/cdr-string ()
  (declare (special scrawl-object extracting-sequence))
  (if (null extracting-sequence)
      (format nil "'~S" scrawl-object)
      (format nil "(c~Ar '~S)"
              (map 'string #'(lambda (x)
                               (ecase x
                                 (car #\a)
                                 (cdr #\d))) 
                    extracting-sequence)
              scrawl-object)))

(defun display-scrawl-result ()
  (declare (special display-width scrawl-current-obj))
  (let* ((*print-pretty* #+cmu t #-cmu nil) ; ???
         (*print-length* nil)
         (*print-level* nil)
         (*print-circle* t)
         (extract-string (get-car/cdr-string))
         (text (if (> (length extract-string) display-width) 
                   (ellide extract-string display-width)
                   extract-string)))
    (sdraw scrawl-current-obj :display-width display-width)
    (format t "~&~%~A~%~%" text)))

(defun display-scrawl-help ()
  (format t "~&Legal commands:  A)car   D)cdr  B)back up~%") 
  (format t "~&                 S)start Q)quit H)help~%"))

(defun display-scrawl-error ()
  (format t "~&Illegal command.~%") 
  (display-scrawl-help))

(defun read-uppercase-char () 
  (let ((response (read-line)))
    (and (plusp (length response))
         (char-upcase (char response 0)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following definitions are specific to the tty implementation.

(defparameter *cons-string* "[*|*]") 
(defparameter *cons-cell-flatsize* 5) 
(defparameter *cons-h-arrowshaft-char* #\-) 
(defparameter *cons-h-arrowhead-char* #\>) 
(defparameter *cons-v-line* "|") 
(defparameter *cons-v-arrowhead* "v")

(defclass tty (renderer)
  ((textline-array :reader textline-array)
   (textline-lengths :reader textline-lengths)))

(defmethod initialize-instance :after ((tty tty) &rest initargs)
  (declare (ignore initargs))
  (with-slots (display-width num-lines textline-array textline-lengths) tty
    (setf textline-array (make-array num-lines)
          textline-lengths (make-array num-lines :initial-element 0))
    (dotimes (i (length textline-array))
      (setf (aref textline-array i) 
            (make-string display-width)))) )

(defmethod draw-structure ((tty tty) directions) 
  (when *sdraw-leading-arrow* (draw-leading-arrow tty))
  (follow-directions tty directions) 
  (dump-display tty))

(defun draw-leading-arrow (tty)
  (loop for i below (1- *leading-arrow-length*)
        do (char-blt tty 0 i (string *cons-h-arrowshaft-char*))
        finally (char-blt tty 0 i (string *cons-h-arrowhead-char*))))

;; (defun draw-leading-arrow (tty)
;;   (do ((i 0 (1+ i)))
;;       ((>= (1+ i) *leading-arrow-length*)
;;        (char-blt tty 0 i (string *cons-h-arrowhead-char*)))
;;     (char-blt tty 0 i (string *cons-h-arrowshaft-char*))))

(defun follow-directions (tty dirs &optional is-car) 
  (ecase (car dirs)
    (cons (draw-cons tty dirs))
    ((atom msg) (draw-msg tty dirs is-car))))

(defun draw-cons (tty directions)
  (destructuring-bind (type row col car-component cdr-component &optional string) directions
    (declare (ignore type))
    (with-slots (textline-array textline-lengths) tty
      (let ((line (aref textline-array row))
            (h-arrow-start (+ col *cons-cell-flatsize*))
            (h-arrowhead-col (1- (third cdr-component)))
            (cdr-string-p (if (eq 'cons (first cdr-component))
                              (sixth cdr-component)
                              (fifth cdr-component))))
        (when cdr-string-p
          (decf h-arrowhead-col (length cdr-string-p)))
        (char-blt tty
                  row
                  (- col (length string))
                  (if string (concatenate 'string string *cons-string*) *cons-string*))
        (do ((i h-arrow-start (1+ i)))
            ((>= i h-arrowhead-col))
          (setf (aref line i) *cons-h-arrowshaft-char*))
        (setf (aref line h-arrowhead-col) *cons-h-arrowhead-char*)
        (setf (aref textline-lengths row) (1+ h-arrowhead-col)) 
        (char-blt tty (+ row 1) (+ col 1) *cons-v-line*)
        (char-blt tty (+ row 2) (+ col 1) *cons-v-arrowhead*)
        (follow-directions tty car-component t)
        (follow-directions tty cdr-component)))) )

(defun draw-msg (tty directions is-car)
  (destructuring-bind (type row col string &optional circ-string) directions
    (declare (ignore type))
    (when circ-string
      (setf string (concatenate 'string circ-string string)))
    (char-blt tty 
              row
              (+ (- col (length circ-string))
                 (if (and is-car (<= (length string) *cons-v-arrow-offset-threshold*)) 
                     *cons-v-arrow-offset-value*
                     0))
              string)))

(defun char-blt (tty row start-col string)
  "Clear out the existing text in ROW up to position START-COL."
  (with-slots (textline-array textline-lengths) tty
    (let ((spos (aref textline-lengths row))
          (line (aref textline-array row))) 
      (fill line #\Space :start spos :end start-col)
      (replace line string :start1 start-col)
      (setf (aref textline-lengths row)
            (+ start-col (length string)))) ))

(defun dump-display (tty)
  "Actually print the structure to the screen."
  (with-slots (textline-array textline-lengths) tty
    (terpri)
    (loop for i from 0 below (length textline-array)
          for len = (aref textline-lengths i)
          for line = (aref textline-array i)
          while (plusp len)
          do (format t "~&~A" (subseq line 0 len)))
    (terpri)))
