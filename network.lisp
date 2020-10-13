;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               network.lisp
;;;;
;;;;   Started:            Thu Oct  8 15:06:28 2020
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
;;;;       sb-bsd sockets.lisp only defines SOCKET-RECEIVE (Generic function in protocol.lisp):
;;;;          (defmethod socket-receive ((socket socket) buffer length
;;;;
;;;;       Although there are 3 related functions: recv(), recvfrom(), and recvmsg(). The manpage says that: read() is approx. recv() w/o the int flags arg.
;;;;       Furthermore:
;;;;
;;;;           recv(sockfd, buf, len, flags);
;;;;
;;;;       is equivalent to
;;;;
;;;;           recvfrom(sockfd, buf, len, flags, NULL, NULL);
;;;;
;;;;    There is a recv in the Win32 code...
;;;;    And constants.lisp mentions recvmsg, but it doesn't appear to be used anywhere...
;;;;    (constants.lisp used by def-to-lisp.lisp in sb-grovel???)
;;;;
;;;;
;;;;    Request class (verb + headers + body)
;;;;    Response class (status + headers + body)
;;;;    Fix timeout???
;;;;    Handle chunked response body pg. 177

(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :network (:use :common-lisp :test))

(in-package :network)

;(defconstant line-terminator (format nil "~C~C" #\Return #\Newline))
;(defconstant user-agent "SBCL")

(defclass http-request-message ()
  ((request-line :reader request-line :initarg :request-line)
   (headers :reader headers :initarg :headers :initform '())
   (body :reader body :initarg :body :initform "")))

(defun add-header (request header)
  (with-slots (headers) request
    (push header headers)))

(defmethod print-object ((hrm http-request-message) stream)
  (format stream "~A" (request-line hrm)) ; ??
  (dolist (header (headers hrm))
    (format stream "~A" header))
  (format stream "~A" line-terminator)
  (unless (string= (body hrm) "")
    (format stream "~A~A" (body hrm) line-terminator)))

(defclass http-request-line ()
  ((request-method :reader request-method :initarg :request-method)
   (path :reader path :initarg :path)
   (version :reader version :initarg :version)))

(defmethod print-object ((hrl http-request-line) stream)
  (format stream "~A ~A HTTP/~A~A" (request-method hrl) (path hrl) (version hrl) line-terminator))

(defclass http-header ()
  ((name :reader name :initarg :name)
   (value :reader value :initarg :value)))

(defmethod print-object ((header http-header) stream)
  (format stream "~A: ~A~A" (name header) (value header) line-terminator))

(defclass http-response-message ()
  ((status-line)
   (headers)
   (body)))

(defclass http-status-line ()
  ((version :reader version :initarg :version)
   (status-code :reader code :initarg :code)
   (status-message :reader message :initarg :message)))

(defmethod print-object ((status http-status-line) stream)
  (format stream "~A ~A ~A~A" (version status) (code status) (message status) line-terminator))

;; (defun make-request-header (host path)
;;   (format nil "~
;; GET ~A HTTP/1.1
;; Host: ~A
;; User-Agent: sbcl
;; Accept: */*
;; " path host))
;; ;Accept: */*~C~C~C~C" path host #\Return #\Newline #\Return #\Newline))

;; (defun make-request-header (host path)
;;   (format nil "GET ~A HTTP/1.1~C~CHost: ~A~C~CUser-Agent: sbcl~C~CAccept: */*~C~C~C~C" path #\Return #\Newline host #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline))

;; (defun make-request-header (host path &key (user-agent "sbcl") (accept "*/*"))
;;   (let ((headers (list (format nil "GET ~A HTTP/1.1" path)
;;                        (format nil "Host: ~A" host)
;;                        (format nil "User-Agent: ~A" user-agent)
;;                        (format nil "Accept: ~A" accept))))
;;     (with-output-to-string (result)
;;       (dolist (header headers)
;;         (write-string header result)
;;         (write-string line-terminator result))
;;       (write-string line-terminator result))))

(defun read-response (socket)
  (with-output-to-string (response)
    (do* ((size 4096)
          (buffer (make-array size :element-type 'character))
          (read-count 1 (1+ read-count)))
;          (total-bytes 0))
         (nil)
      (multiple-value-bind (buffer bytes-read address port) (sb-bsd-sockets:socket-receive socket buffer size) ; :dontwait t)
        (declare (ignore address port)) ;   "Returns address and port of SOCKADDR as multiple values"
;(print b)
 (print (list read-count bytes-read))
;; (unless (minusp bytes-read)
;; (incf total-bytes bytes-read))
        (write-sequence buffer response :end bytes-read)
                                        ;(print bytes-read)
        ;; (dotimes (i bytes-read)
        ;;   (write-char (char b i) response))
;        (when (= bytes-read 0)
        (when (< bytes-read 1)
;(print total-bytes)
          (return)))) ))

(defun read-response (socket)
  (with-output-to-string (response)
    (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t)))
      (do ((ch (read-char stream nil nil) (read-char stream nil nil)))
          ((null ch) response)
        (write-char ch)
        (write-char ch response)))) )

(defun build-response-message (response)
  (let ((status-line (subseq response 0 (search line-terminator response))))
    (print status-line)
;    (print (length response))
    response))

(defun http-get (host path &optional (port 80))
  (let ((address (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))
        (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (request-message (make-instance 'http-request-message :request-line (make-instance 'http-request-line :request-method "GET" :path path :version "1.1"))))
    (add-header request-message (make-instance 'http-header :name "Accept" :value "*/*"))
    (add-header request-message (make-instance 'http-header :name "User-Agent" :value user-agent))
    (add-header request-message (make-instance 'http-header :name "Host" :value (if (= port 80) host (format nil "~A:~A" host port))))
;        (request-header (make-request-header host path)))
    (sb-bsd-sockets:socket-connect socket address port)
    (let ((request (format nil "~A" request-message)))
      (sb-bsd-sockets:socket-send socket request (length request)))
;    (sb-bsd-sockets:socket-send socket request-header (length request-header))
    (prog1 (build-response-message (read-response socket))
      (sb-bsd-sockets:socket-close socket))))


;;;
;;;    From SOCKET-RECEIVE
;;;    
;;                (sb-bsd-sockets::socket-error-case ("recvfrom"
;;                                    (sockint::recvfrom fd copy-buffer length
;;                                                       flags sockaddr (sb-alien:addr sa-len))
;;                                    len)
;;                    (progn
;;                      (loop for i from 0 below (min len length)
;;                         do (setf (elt buffer i)
;;                                  (cond
;;                                    ((or (eql element-type 'character) (eql element-type 'base-char))
;;                                     (code-char (sb-alien:deref (sb-alien:deref copy-buffer) i)))
;;                                    (t (sb-alien:deref (sb-alien:deref copy-buffer) i)))))
;;                      (apply #'values buffer len (multiple-value-list
;;                                                  (bits-of-sockaddr socket sockaddr))))
;;                    (:interrupted nil))

;;;
;;;    Expanded
;;;    
;; (LET ((LEN
;;        (SB-BSD-SOCKETS-INTERNAL::RECVFROM FD COPY-BUFFER LENGTH FLAGS SOCKADDR
;;                                           (ADDR SA-LEN))))
;;   (COND
;;    ((AND (= LEN -1)
;;          (SB-BSD-SOCKETS::INTERRUPTED-P (SB-BSD-SOCKETS::SOCKET-ERRNO)))
;;     NIL)
;;    ((= LEN -1) (SB-BSD-SOCKETS:SOCKET-ERROR "recvfrom"))
;;    (T
;;     (PROGN
;;      (LOOP FOR I FROM 0 BELOW (MIN LEN LENGTH)
;;            DO (SETF (ELT BUFFER I)
;;                       (COND
;;                        ((OR (EQL ELEMENT-TYPE 'CHARACTER)
;;                             (EQL ELEMENT-TYPE 'BASE-CHAR))
;;                         (CODE-CHAR (DEREF (DEREF COPY-BUFFER) I)))
;;                        (T (DEREF (DEREF COPY-BUFFER) I)))))
;;      (APPLY #'VALUES BUFFER LEN
;;             (MULTIPLE-VALUE-LIST (BITS-OF-SOCKADDR SOCKET SOCKADDR)))))))

