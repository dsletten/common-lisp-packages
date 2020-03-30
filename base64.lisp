;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               base64.lisp
;;;;
;;;;   Started:            Sat Dec 21 16:23:38 2019
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
;;;;   Notes: https://en.wikipedia.org/wiki/Base64
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :base64 (:use :common-lisp :test))

(in-package :base64)

(defun write-range (start &optional end)
  (if (null end)
      (write-char start)
      (loop for i from (char-code start) upto (char-code end)
            do (write-char (code-char i)))) )

(defun setup (table)
  (loop for ch across *encode-table*
        for i from 0
        do (setf (gethash ch table) i)))

(unless (boundp '*decode-table*)
  (defvar *encode-table* (with-output-to-string (s)
                           (let ((*standard-output* s))
                             (write-range #\A #\Z)
                             (write-range #\a #\z)
                             (write-range #\0 #\9)
                             (write-range #\+)
                             (write-range #\/))))
  (defvar *decode-table* (make-hash-table :test #'eql))
  (setup *decode-table*))

(defun lookup (ch)
  (gethash ch *decode-table*))

(defun paddingp (ch)
  (or (null ch) (char= ch #\=)))

(defun decode-group (stream ch0 ch1 &optional ch2 ch3)
  (let ((b0 (lookup ch0))
        (b1 (lookup ch1)))
    (declare ((byte 8 0) b0 b1))
    (write-char (code-char (logior (ash b0 2) (ash b1 -4))) stream)
    (unless (paddingp ch2)
      (let ((b2 (lookup ch2)))
        (declare ((byte 8 0) b2))
        (write-char (code-char (logior (ash (logand b1 #xF) 4) (ash b2 -2))) stream)
        (unless (paddingp ch3)
          (let ((b3 (lookup ch3)))
            (declare ((byte 8 0) b3))
            (write-char (code-char (logior (ash (logand b2 #x3) 6) b3)) stream)))) )))

(defun decode (base64-string)
  (let ((length (length base64-string)))
    (if (= (mod length 4) 1)
        (error "Encoded string is wrong length.")
        (with-output-to-string (result)
          (do ((i 0 (+ i 4)))
              ((>= i length))
            (apply #'decode-group result (loop for j from i below (+ i 4) collect (if (< j length)
                                                                                      (char base64-string j)
                                                                                      nil)))) ))))
;            (apply #'decode-group result (coerce (subseq base64-string i (+ i 4)) 'list))))
;        (error "String is wrong length. Padded incorrectly."))))

(deftest test-decode ()
  (check
   (string= (decode "YW55IGNhcm5hbCBwbGVhc3Vy") "any carnal pleasur")
   (string= (decode "YW55IGNhcm5hbCBwbGVhc3V=") "any carnal pleasu")
   (string= (decode "YW55IGNhcm5hbCBwbGVhc3==") "any carnal pleas")
   (string= (decode "YW55IGNhcm5hbCBwbGVhc3V") "any carnal pleasu")
   (string= (decode "YW55IGNhcm5hbCBwbGVhc3") "any carnal pleas")))


#|
Man is distinguished, not only by his reason, but by this singular passion from other animals, 
which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable 
generation of knowledge, exceeds the short vehemence of any carnal pleasure.

TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz
IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg
dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu
dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo
ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=

TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS

|#
