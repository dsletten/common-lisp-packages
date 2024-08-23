;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               csv.lisp
;;;;
;;;;   Started:            Sat Jun 26 10:23:16 2021
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
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/packages/collections.lisp" :verbose nil)

(defpackage :csv
  (:shadowing-import-from :collections :intersection :set :subsetp :union)
  (:use :common-lisp :core :collections :test))

(in-package :csv)

(defclass csv-parser ()
  ((separator :initform #\,)
   (quote-char :reader quote-char :initform #\" :initarg :quote-char)
   (ignore-whitespace-p :initform t)
   (input :initarg :input)))

(defun make-csv-parser (input &optional (quote-char #\"))
 (make-instance 'csv-parser :input input :quote-char quote-char))

(defun parse (parser)
  (with-slots (input) parser
    (let ((stream (make-string-input-stream input)))
      (loop while (listen stream)
            collect (parse-line parser (read-line stream)))) ))

(defun space-char-p (ch)
  (member ch '(#\space #\tab)))

;;;
;;; a,b,c
;;; a,b,
;;; 
(defun parse-line (parser line)
  (with-slots (separator quote-char ignore-whitespace-p) parser
    (let ((stream (make-string-input-stream line)))
      (labels ((parse-field (fields)
                 (let ((field (make-string-output-stream)))
                   (cond ((listen stream) (parse-char field fields))
                         (t (enqueue fields (get-output-stream-string field)) (elements fields)))) )
               (parse-char (field fields)
                 (let ((ch (read-char stream nil nil)))
                   (cond ((null ch) (enqueue fields (get-output-stream-string field)) (elements fields))
                         ((eql ch quote-char) (parse-quoted field fields))
                         ((eql ch separator) (enqueue fields (get-output-stream-string field)) (parse-field fields))
                         ((and (space-char-p ch) ignore-whitespace-p) (parse-char field fields))
                         (t (write-char ch field)
                            (parse-char field fields)))) )
               (parse-quoted (field fields)
                 (let ((ch (read-char stream nil nil)))
                   (if (eql ch quote-char)
                       (parse-char field fields)
                       (case ch
                         ((nil) (error "Missing closing quote char"))
                         (#\\ (parse-escaped field fields))
                         (otherwise (write-char ch field)
                                    (parse-quoted field fields)))) ))
               (parse-escaped (field fields)
                 (let ((ch (read-char stream nil nil)))
                   (case ch
                     ((nil) (error "Missing closing quote char"))
                     (otherwise (write-char ch field)
                                (parse-quoted field fields)))) ))
        (parse-field (make-linked-queue)))) ))


(defun dos->unix (s)
  (with-output-to-string (us)
    (loop for ch across s
          if (char= ch #\return)
            do (write-char #\newline us)
          else
            do (write-char ch us))))

