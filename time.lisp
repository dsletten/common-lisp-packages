;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               time.lisp
;;;;
;;;;   Started:            Sat Jul 31 01:40:36 2010
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

(defpackage :time
  (:use :common-lisp)
  (:export :compare
           :date :day :day-names :day-of-week :day-of-year
           :get-second :get-minute :get-hour :get-day-of-month
           :get-month :get-month-name :get-short-month-name
           :get-year :get-day-of-week :get-day-of-week-name
           :get-short-day-of-week-name :get-daylight-savings-p :get-time-zone
           :leap-year-p :legal-date :localtime :localtime-list
           :month :month-length :month-names
           :precedesp
           :short-day-names :short-month-names
           :year :yyyy-mm-dd))

(in-package :time)

;;;
;;;    Day names are arranged to be consistent with decoded universal times. In particular, day of week is 0-6 with 0 Monday and 6 Sunday.
;;;    However, month names correspond with index from 0-11 although decoded universal time is value from 1-12. This value is offset in
;;;    the functions below.
;;;    
(defconstant day-names (vector "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defconstant month-names (vector "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))
(defconstant short-month-names (map 'vector #'(lambda (month) (subseq month 0 3)) month-names))
(defconstant short-day-names (map 'vector #'(lambda (day) (subseq day 0 3)) day-names))

;;;
;;;    Military time -> 12-hour clock
;;; (format t "~D ~[~:[a.m.~;midnight~]~;~:[p.m.~;noon~]~]~%" (+ (mod (+ hour 11) 12) 1) (truncate hour 12) (zerop (mod hour 12)))
;;; 

;;;
;;;    localtime
;;;
;;;    Return Perl-like date/time string (a la scalar(localtime))
;;;          DOW MON DD HH:MM:SS YYYY
;;;    E.g., Fri Aug  3 16:47:27 2001
;;;
(defun localtime (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day-of-week daylight-savings-p time-zone)
      (decode-universal-time time)
    (declare (ignore time-zone daylight-savings-p))
    (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~4D"
            (get-short-day-of-week-name day-of-week)
            (get-short-month-name month)
            date hour minute second year)))

(defun localtime-list (&optional (time (get-universal-time)))
  (multiple-value-list (decode-universal-time time)) )

(defun yyyy-mm-dd (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day-of-week
                        daylight-savings-p time-zone)
      (decode-universal-time time)
    (declare (ignore second minute hour day-of-week daylight-savings-p time-zone))
    (format nil "~4D-~2,'0D-~2,'0D" year month date)) )

(defun get-second (&optional (time (get-universal-time)))
  (nth-value 0 (decode-universal-time time)))
             
(defun get-minute (&optional (time (get-universal-time)))
  (nth-value 1 (decode-universal-time time)))
             
(defun get-hour (&optional (time (get-universal-time)))
  (nth-value 2 (decode-universal-time time)))
             
(defun get-day-of-month (&optional (time (get-universal-time)))
  (nth-value 3 (decode-universal-time time)))
             
(defun get-month (&optional (time (get-universal-time)))
  (nth-value 4 (decode-universal-time time)))

(defun get-month-name (&optional (month (get-month)))
  (aref month-names (1- month)))

(defun get-short-month-name (&optional (month (get-month)))
  (aref short-month-names (1- month)))

(defun get-year (&optional (time (get-universal-time)))
  (nth-value 5 (decode-universal-time time)))
             
(defun get-day-of-week (&optional (time (get-universal-time)))
  (nth-value 6 (decode-universal-time time)))

(defun get-day-of-week-name (&optional (day-of-week (get-day-of-week)))
  (aref day-names day-of-week))

(defun get-short-day-of-week-name (&optional (day-of-week (get-day-of-week)))
  (aref short-day-names day-of-week))

(defun get-daylight-savings-p (&optional (time (get-universal-time)))
  (nth-value 7 (decode-universal-time time)))
             
(defun get-time-zone (&optional (time (get-universal-time)))
  (nth-value 8 (decode-universal-time time)))

(defun month-length (&optional (month (get-month)) (year (get-year)))
  (ccase month
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    ((1 3 5 7 8 10 12) 31)))

(defgeneric leap-year-p (year))

;(defun leap-year-p (year)
(defmethod leap-year-p ((year number))
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

;; (deftype month () '(integer 1 12)) ; Should be 0-11? See above.
;; (deftype day (m y) `(integer 1 ,(month-length m y)))
;; (deftype year () '(integer 1900 *)) ; ????

;;;
;;;    Do we actually need separate classes for YEAR/MONTH/DAY? Or is a single
;;;    aggregate DATE class with slots for those values adequate?
;;;    
(defclass date ()
  ((year :reader year :initarg :year :type (integer 0 *) :initform (get-year))
   (month :reader month :initarg :month :type (integer 1 12) :initform (get-month))
   (day :reader day :initarg :day :type (integer 1 31) :initform (get-day-of-month))))

(defmethod initialize-instance :after ((d date) &rest init-args)
  (declare (ignore init-args))
  (with-slots (year month day) d
    (check-type year (integer 0 *)) ; Allow pre-Gregorian?
    (check-type month (integer 1 12))
    (assert (typep day `(integer 1 ,(month-length month year))) (day) "Day should be between 1 and ~D." (month-length month year))))

(defmethod print-object ((d date) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "~A ~A ~D, ~D" (get-day-of-week-name (day-of-week d)) (get-month-name (month d)) (day d) (year d))))

(defgeneric legal-date (date))
(defmethod legal-date ((d date))
  (format nil "~A. ~D, ~D" (get-short-month-name (month d)) (day d) (year d)))

(defgeneric compare (obj1 obj2)
  (:documentation "Which of two dates comes first?"))
(defmethod compare ((d1 date) (d2 date))
  (cond ((= (year d1) (year d2))
         (cond ((= (month d1) (month d2))
                (if (< (day d2) (day d1))
                    d2
                    d1))
               ((< (month d1) (month d2)) d1)
               (t d2)))
        ((< (year d1) (year d2)) d1)
        (t d2)))

(defgeneric precedesp (date1 date2)
  (:documentation "Does date1 precede date2?"))
(defmethod precedesp ((d1 date) (d2 date))
  (if (eq (compare d1 d2) d1)
      (not (eq (compare d2 d1) d2))
      nil))

(defgeneric day-of-week (date))

;;;
;;;    Month value adjusted to accommodate ZELLER:
;;;    January -> 11, February -> 12, March -> 1, ...
;;;    
(defmethod day-of-week ((d date))
  (zeller (day d)
          (+ (mod (+ (month d) 9) 12) 1)
          (truncate (year d) 100)
          (rem (year d) 100)
          (if (leap-year-p (year d)) 1 0)))

;;;
;;;    Final value adjusted to accommodate day of week range for universal time:
;;;    Zeller -> 0 Sunday - 6 Saturday
;;;    Universal time -> 0 Monday - 6 Sunday
;;;    
(defun zeller (n m c y l)
  (mod (+ (- (+ n
                (cl:floor (1- (* 13 m)) 5)
                y
                (cl:floor y 4)
                (cl:floor c 4))
             (* 2 c)
             (* (1+ l) (cl:floor m 11)))
          6)
       7))

;;;
;;;    Convert date in Gregorian calendar to Julian day number.
;;;    Collected Algorithms from CACM Vol. I (Algorithm 199)
;;;    
(defun julian (day month year)
  (flet ((julian-aux (m y)
           (multiple-value-bind (c ya) (floor y 100)
             (+ (floor (* c 146097) 4)
                (floor (* ya 1461) 4)
                (floor (+ (* m 153) 2) 5)
                day
                1721119))))
    (if (> month 2)
        (julian-aux (- month 3) year)
        (julian-aux (+ month 9) (1- year)))) )

(defun day-of-year (day month year)
  (1+ (- (julian day month year)
         (julian 1 1 year))))

;; (defun day-of-week (day month year)
;;   (mod (1+ (julian day month year)) 7))

;;;    https://en.wikipedia.org/wiki/Julian_day
(defun julian2 (day month year)
  (let ((month- (truncate (- month 14) 12))) ; TRUNCATE not FLOOR!!!!! (- MONTH 14) is negative!!!
    (+ (floor (* 1461 (+ year 4800 month-)) 4)
       (floor (* 367 (- month 2 (* 12 month-))) 12)
       (- (floor (* 3 (floor (+ year 4900 month-) 100)) 4))
       day
       -32075)))


;; (defclass month ()
;;   ((value :reader value :initarg :value)
;;    (name :reader name :initarg :name)
;;    (short-name :reader short-name :initarg :short-name)))

;; (defclass day ()
;;   ((value :reader value :initarg :value :type (integer 1 31))))

;; (defclass day-of-week ()
;;   ((value :reader value :initarg :value)
;;    (name :reader name :initarg :name)
;;    (short-name :reader short-name :initarg :short-name)))

;; (defclass year ()
;;   ((value :reader value :initarg :value :type (integer 0 *))))

;; (defmethod leap-year-p ((y year))
;;   (leap-year-p (value y)))

;; (defconstant months (loop for i from 1 to 12
;;                           for name across month-names
;;                           collect (make-instance 'month :value i :name name :short-name (subseq name 0 3)) into months-list
;;                           finally (return (coerce months-list 'vector))))

;; (defconstant days-of-week (loop for i from 0 to 6
;;                                 for name across day-names
;;                                 collect (make-instance 'day-of-week :value i :name name :short-name (subseq name 0 3)) into days-list
;;                                 finally (return (coerce days-list 'vector))))

;; (loop for i from 0 below 12 for name across month-names do (format t "(defconstant ~S (aref months ~D))~%" (read-from-string name) i))

;; (defconstant JANUARY (aref months 0))
;; (defconstant FEBRUARY (aref months 1))
;; (defconstant MARCH (aref months 2))
;; (defconstant APRIL (aref months 3))
;; (defconstant MAY (aref months 4))
;; (defconstant JUNE (aref months 5))
;; (defconstant JULY (aref months 6))
;; (defconstant AUGUST (aref months 7))
;; (defconstant SEPTEMBER (aref months 8))
;; (defconstant OCTOBER (aref months 9))
;; (defconstant NOVEMBER (aref months 10))
;; (defconstant DECEMBER (aref months 11))

;; ;; (loop for i from 0 to 6 for name across day-names do (format t "(defconstant ~S (aref days-of-week ~D))~%" (read-from-string name) i))

;; (defconstant MONDAY (aref days-of-week 0))
;; (defconstant TUESDAY (aref days-of-week 1))
;; (defconstant WEDNESDAY (aref days-of-week 2))
;; (defconstant THURSDAY (aref days-of-week 3))
;; (defconstant FRIDAY (aref days-of-week 4))
;; (defconstant SATURDAY (aref days-of-week 5))
;; (defconstant SUNDAY (aref days-of-week 6))
