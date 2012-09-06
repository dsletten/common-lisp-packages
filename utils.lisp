;#!/usr/local/bin/clisp

;;;;
;;;;   NAME:               utils.lisp
;;;;
;;;;   STARTED:            010713
;;;;   MODIFICATIONS:
;;;;
;;;;   PURPOSE:
;;;;
;;;;
;;;;
;;;;   CALLING SEQUENCE:
;;;;
;;;;
;;;;   INPUTS:
;;;;
;;;;   OUTPUTS:
;;;;
;;;;   EXAMPLE:
;;;;
;;;;   NOTES: Obsolete! Functionality has been divided into separate
;;;;   packages and files. See primarily lang.lisp, io.lisp, shell.lisp.
;;;;
;;;;

(defpackage utils
  (:use common-lisp)
  (:export "ADD-COLUMN" "AFTER" "APPEND1" "BEFORE" "BEST" "BREAK-LOOP"
	   "CENTER"
	   "COMPOSE" "CONC1" "DROP" "DUPLICATE" "DEFAULT-DIRECTORY"
	   "ENDS-WITH" "EXPAND"
	   "EXPLODE" "FIF" "FILTER" "FIND2" "FINT" "FLATTEN" "FUN"
	   "GET-ARGV" "GET-NUM" "GETENV" "GROUP" "JOIN" "LAST1"
	   "LIST-TO-STRING" "LIST-TO-STRING-AUX"
	   "LJUST" "LOCALTIME" "LOCALTIME-LIST"
	   "LONGERP" "MAKE-RANGE" "MAP->" "MAP0-N" "MAP1-N" "MAPA-B" "MAPCARS"
	   "MAPPEND" "MEMOIZE" "MKLIST" "MKSTR" "MOST" "MOSTN" "PARTITION"
	   "PRINT-PLIST" "PROMPT" "PRUNE" "PRUNE-IF-NOT" "READ-FILE"
	   "READLIST" "REREAD" "RJUST"
	   "RMAPCAR" "SEARCH-FILE" "SEARCH-LINES-OF-FILE"
	   "SET-FILE-PERMISSIONS" "SET-FILE-PERMISSIONS" "SHOW-SYMBOLS"
	   "SINGLE" "SPLICE" "SPLIT" "SPLIT-IF" "STARTS-WITH" "STRING-SPLIT"
	   "STRING-SUBST" "SYMB" "TAKE" "TAKE-DROP" "TEST" "TRANSFER"
	   "TRANSLATE" "TREE-FIND-IF" "VALID-NUM" "WRITE-FILE" "YYYY-MM-DD"))
(in-package utils)

(defun get-argv (n)
  #+ :sbcl (if (>= n (length sb-ext:*posix-argv*))
               nil
               (nth (+ n 1) sb-ext:*posix-argv*))
  #+ :clisp (nth n *args*))

;;;
;;;    SBCL also POSIX-ENVIRON
;;;    CLISP has SETENV
;;;
;; (defun getenv (key)
;;   #+ :sbcl (posix-getenv key)
;;   #+ :clisp (ext:getenv key))
;;;
;;;    From cookbook
;;;    
(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or #+Allegro (sys:getenv name)
      #+CLISP (ext:getenv name)
      #+ECL (si:getenv name)
      #+SBCL (sb-unix::posix-getenv name)
      #+LISPWORKS (lispworks:environment-variable name)
      default))

;;;
;;;    Steve Gonedes
;;;
(defun default-directory ()
  #+cmu (default-directory)
;  #+:cmu (ext:default-directory)
  #+clisp (ext:default-directory)
  #+excl (excl:current-directory)
  #+(or gcl sbcl) (truename "."))

;; #+clisp (defsetf default-directory lisp:cd)
;; #+excl (defsetf default-directory excl:chdir)
;; #+gcl (defsetf default-directory si:chdir)
;; #+lww (defsetf default-directory change-directory)

;;MCL uses the following:
;;For default-directory: (ccl:mac-default-directory)
;;To set default directory: (ccl:set-mac-default-directory pathname)

;;CMUCL (EXTENSIONS:DEFAULT-DIRECTORY), LispWorks (HCL:CHANGE-DIRECTORY
;;   and HCL:GET-WORKING-DIRECTORY), and AllegroCL (EXCL:CURRENT-DIRECTORY
;;   and EXCL:CHDIR)

;; (defun explode (object)
;;      (loop for char across (prin1-to-string object)
;;            collect (intern (string char))))

;;    (defun implode (list)
;;      (read-from-string (coerce (mapcar #'character list) 'string)))

;; An alternate definition of EXPLODE which uses MAP instead of LOOP is:

;;    (defun explode (object)
;;      (map 'list #'(lambda (char) 
;;                     (intern (string char)))
;;           (prin1-to-string object)))

;---------String Functions----------------
;;;
;;;    Split a given string into a list of strings.
;;;    The string is split on each occurrence of a given char
;;;    or on #\Space as a default.
;;;
;;;    The function recursively splits on successive occurrences of the
;;;    delimiter, building a list in reverse order.
;;;    The list is reversed before being returned to calling function.
;;;    
(defun split (s &optional (delimiter #\Space))
  "Split a given string into a list of strings. The string is split on each occurrence of a given char or on #\\Space as a default."
  (labels ((split-aux (s result)
	     (let ((point (position delimiter s)))
	       (if point
		   (split-aux (subseq s (+ point 1))
			      (cons (subseq s 0 point) result))
		   (nreverse (cons s result)))) ))
    (split-aux s '())))

(defun string-split (s &optional (delimiter " "))
  "Split a given string into a list of strings. The string is split on each occurrence of a given string or on \" \" as a default."
  (let ((d-length (length delimiter)))
    (labels ((string-split-aux (s result)
	       (let ((point (search delimiter s)))
		 (if point
		     (string-split-aux (subseq s (+ point d-length))
				       (cons (subseq s 0 point) result))
		     (nreverse (cons s result)))) ))
      (if (equal delimiter "")
	  (map 'list #'string s)
	  (string-split-aux s '()))) ))

;;;
;;;    Join a list of elements together as a string using a given string as
;;;    glue.
;;;
;;;    (join <strings-list> [<glue-string>])
;;;
(defun join (strings &optional (glue ""))
  "Join a list of elements together as a string using a given string as glue."
  (labels ((join-aux (strings result)
	     (cond ((null strings) result)
		   ((stringp (car strings))
		    (join-aux (cdr strings)
			      (concatenate 'string result glue
					   (car strings))))
		   ((characterp (car strings))
		    (join-aux (cdr strings)
			      (concatenate 'string result glue
					   (string (car strings)))) )
		   (t
		    (join-aux (cdr strings)
			      (concatenate 'string result glue
					   (format nil "~S"
						   (car strings)))) ))))
    (cond ((null strings) "")
	  ((stringp (car strings))
	   (join-aux (cdr strings) (car strings)))
	  ((characterp (car strings))
	   (join-aux (cdr strings) (string (car strings))))
	  (t
	   (join-aux (cdr strings) (format nil "~S" (car strings)))) )))

;; (defun join (strings &optional (glue ""))
;;   (format nil (format nil "~~{~~A~~^~A~~}" glue) strings))

;;;
;;;    Adam Warner (original DELIMITER was CHAR)
;;;    
;; (defun join (list &optional (delimiter ":"))
;;   (let ((countdown (length list)))
;;     (with-output-to-string (stream)
;;       (dolist (item list)
;;         (write-string item stream)
;;         (decf countdown)
;;         (unless (zerop countdown)
;;           (write-string delimiter stream)))) ))

;;;
;;;    Matthew Danish (originally with PRINC)
;;;
;;;
;;;>>However the main reason my version was faster is that I used WRITE-STRING
;;;>>instead of PRINC. Replace Matthew's PRINCs with WRITE-STRINGs and his
;;;>>version is almost exactly the same speed with three elements (0.25s). With
;;;>> three times the number of elements it takes 0.63s.
;;; Binding *PRINT-PRETTY* to NIL has the same effect.  It also makes the
;;; FORMAT only solution run over twice as fast and cons 1/5 as much.
;; (defun join (list)
;;   (format nil "~{~A~^:~}" list))

;; (defun join (list &optional (delim ":"))
;;   (with-output-to-string (stream)
;;     (loop for cons on list do
;; 	    (write-string (car cons) stream)
;; 	      (when (cdr cons) (write-string delim stream)))))

;;;
;;;    John Thingstad (with minor modifications)
;;;    (Doesn't handle empty LIST arg)
;;;
;; (defun join (list delimiter)
;;   (reduce #'(lambda (a b) (concatenate 'string a delimiter b)) list)




;;;
;;;    Splice a new sequence into another.
;;;    (Works like Perl splice()/substr())
;;;    Extend to vectors too?
;;;    
;;;    This is slightly different than Perl in that the new elements are
;;;    expected to be bundled into a sequence (list or string) rather than
;;;    passed as separate args. This is done to accommodate both lists and
;;;    strings in a uniform way.
;;;
(defun splice (seq &optional (offset 0) (length (length seq)) new-seq)
  "Splice a subsequence into a given sequence at a specified location."
  
  (if (minusp offset)
      (if (> (abs offset)
	     (length seq))
	  (setf offset 0)
	  (setf offset (+ (length seq) offset))))
  (if (minusp length)
      (if (< (+ (length seq) length)
	     offset)
	  (setf length 0)
	  (setf length (- (+ (length seq) length)
			  offset))))
  (let ((seq-type (typecase seq
		    (string 'string)
		    (list 'list))))
    (concatenate seq-type
		 (subseq seq 0 offset)
		 new-seq
		 (subseq seq (min (length seq)
				  (+ offset length)))) ) )

;;;
;;;    Replace part(s) of a string with another.
;;;
;;;    Default requires case-sensitive match.
;;;    Use :test #'char-equal for case-insensitive match.
;;;
(defun string-subst (new old string &rest keys &key global (test #'char=))
  (let ((i (search old string :test test)))
    (cond ((null i) string)
	  (global (apply #'string-subst new old (splice string
							i (length old) new)
				keys))
	  (t (splice string i (length old) new)))) )

;[55]> (string-subst "pung" "foo" "foo bar foo baz")
;"pung bar foo baz"
;[56]> (string-subst "pung" "foo" "foo bar foo baz" :global t)
;"pung bar pung baz"
; [13]> (string-subst "pung" "foo" "pung in the FOO bar...foo!" :test #'char-equal)
; "pung in the pung bar...foo!"
; [14]> (string-subst "pung" "foo" "pung in the FOO bar...foo!" :test #'char-equal :global t)
; "pung in the pung bar...pung!"

;;;
;;;    A roundabout way of uppercasing a string:
;;;
; [18]> (setf s "Is this not pung?")
; "Is this not pung?"
; [19]> (setf l (split s))
; ("Is" "this" "not" "pung?")
; [20]> (dolist (w l) (setf s (string-subst (string-upcase w) w s)))
;;;   Or to capitalize each word:
;[26]> (dolist (w l) (setf s (string-subst (string-capitalize w) w s)))

;;;
;;;    See translate.lisp
;;;    
(defun expand (char-range)
  (let ((pos (position #\- char-range)))
    (if pos
	(cond ((zerop pos) (error "Can't start with -"))
	      ((= 1 pos)
	       (let ((start (char char-range 0))
		     (end   (char char-range 2)))
		 (if (char< start end)
		     (concatenate 'string
				  (coerce (make-range start end) 'string)
				  (expand (subseq char-range 3)))
		     (error "Inverted range"))))
	      (t (concatenate 'string (subseq char-range 0 (1- pos))
			      (expand (subseq char-range (1- pos)))) ))
	char-range)))

(defun translate (target in out)
  (coerce (sublis (mapcar #'cons
			  (coerce (expand in) 'list)
			  (coerce (expand out) 'list))
		  (coerce target 'list))
	  'string))

;;;
;;;    These 2 are actually generic sequence functions.
;;;    
(defun starts-with (s1 s2 &key (test #'eql))
  "Determines whether or not the first string arg contains the second string arg at its start."
  (search s2 s1 :end2 (length s2) :test test))

(defun ends-with (s1 s2 &key (test #'eql))
  "Determines whether or not the first string arg ends with the second string arg"
  (search s2 s1 :from-end t :start2 (- (length s1) (length s2)) :test test))

;;;
;;;    Provide Ruby's center(), ljust(), rjust() string methods.
;;;    
(defun center (s width)
  (format nil "~V:@<~A~>" width s))

(defun ljust (s width)
  (format nil "~V@<~A~>" width s))

(defun rjust (s width)
  (format nil "~V<~A~>" width s))

;; (defun center (s width)
;;   (format nil "~V<~;~A~;~>" width s))

;; (defun ljust (s width)
;;   (format nil "~V<~A~;~>" width s))

;;;
;;;    See JOIN above.
;;;
;; (defun strcat (&rest strings)
;;   (apply #'concatenate 'string strings))

(defun strcat (&rest strings)
  (let ((result (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop for s in strings do
	  (loop for ch across s do (vector-push-extend ch result))) result))

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

;----------I/O Functions-----------------
;;;
;;;    Read each line of a file into a list of strings.
;;;    Returns nil if file does not exist.
;;;    
(defun read-file (file-name)
  (with-open-file (in-stream file-name :if-does-not-exist nil)
    (if in-stream
        (loop for line = (read-line in-stream nil nil)
              while line
              collect line)
        (format *error-output* "Error: File does not exist!~%"))))

;; (defun read-file (file-name)
;;   (with-open-file (in-stream file-name :if-does-not-exist nil)
;;     (cond ((null in-stream)
;; 	   (format *error-output* "Error: File does not exist!~%"))
;; 	  (t (let ((eof (list 'eof)))
;; 	       (loop for line = (read-line in-stream nil eof)
;; 		     until (eq line eof) collect line)))) ))

; (defun read-file (file-name)
;   (with-open-file (in-stream file-name :if-does-not-exist nil)
;     (cond ((null in-stream)
; 	   (format *error-output* "Error: File does not exist!~%")
; 	   (return-from read-file nil)))
;     (do ((results '())
; 	 (line (read-line in-stream nil nil)
; 	       (read-line in-stream nil nil)))
; 	((null line) (reverse results))
;       (setf results (cons line results)))) )

;; (defun read-file (file-name)
;;   (with-open-file (in-stream file-name :if-does-not-exist nil)
;;     (unless in-stream
;;       (format *error-output* "Error: File does not exist!~%")
;;       (return-from read-file nil))
;;     (let ((eof (list 'eof)))
;;       (do ((line (read-line in-stream nil eof)
;; 		 (read-line in-stream nil eof))
;; 	   (results '() (cons line results)))
;; 	  ((eq line eof) (nreverse results)))) ))

;;;
;;;    This loses the last line. (It quits too soon.)
;;;    
;	((not (listen in-stream)) (reverse results)))) )

;;;
;;;    Write a list of strings to a file.
;;;    Prompt user if file already exists.
;;;    Returns nil if file exists and user chooses not to overwrite.
;;;    
(defun write-file (file-name contents &optional overwrite)
  (with-open-file (out-stream file-name
			      :direction :output
			      :if-exists (if overwrite :supersede nil))
;			      :if-exists (if overwrite :overwrite nil))
    (if (null out-stream)
	;; Assume file exists?              ;;;Fix this!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	(cond ((y-or-n-p (format nil "The file ~A already exists. Overwrite?"
				 file-name))
	       (write-file file-name contents t))
	      (t (format *error-output* "Error: File already exists!~%")
		 (return-from write-file nil))))
    (dolist (line contents)
      (format out-stream "~A~%" line))) )

(defun add-column (file-name)
  "Add a column to a table in an HTML file."
  (with-open-file (s file-name)
    (let ((eof (list 'eof)))
      (do ((line0 (read-line s nil eof) (read-line s nil eof))
	   (line1 "" line0))
	  ((eq line0 eof))
	(when (search "</tr>" line0)
	  (write-line line1))
	(write-line line0)))) )
  
;;;
;;;    The following are by Kent Pitman:
;;;    http://www.nhplace.com/kent/PS/Hindsight.html
;;;    
(defun search-lines-of-file (string file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream)
	  for i from 0
	  when (search string line)
	  do (format t "~&Line ~D: ~A" i line)
	  finally (format t "~&Done.~%"))))

(defun search-file (string file)
  (handler-case (search-lines-of-file string file)
    (file-error (condition)
      (format t "~&File problem: ~A~%" condition))))


;;;
;;;    Read a number from STDIN using a given prompt and meeting the given
;;;    TEST criterion. Continue prompting until a valid number is entered.
;;;
;;;    Note: READ-LINE is used so that all input is consumed rather than
;;;          leaving anything in buffer.
;;;          
; (defun get-num (prompt &optional min max)
;   (do ((num nil))
;       ((if (numberp num)
; 	   (and (if (numberp min)
; 		    (>= num min)
; 		    t)
; 		(if (numberp max)
; 		    (<= num max)
; 		    t))
; 	   nil)
;        num)
;     (format t "~A" prompt)
;     (setf num (read-from-string (read-line) nil))) )

(defun get-num (prompt &optional test)
  (format t "~A" prompt)
  (force-output)
  (let ((num (read-from-string (read-line) nil nil)))
    (if (valid-num num test)
	num
	(get-num prompt test))))

(defun valid-num (num test)
  (if (numberp num)
      (if test
	  (funcall test num)
	  t)
      nil))

(defun parse-float (s &key (start 0) end junk-allowed)
  (let ((input (string-trim " " s))
	(eof (cons nil nil)))
    (multiple-value-bind (val i) (read-from-string input nil eof :start start :end end)
      (cond ((eq i eof) (error 'parse-error))
	    ((numberp val) (if junk-allowed
			       val
			       (cond (end (if (= i end)
					      val
					      (error 'parse-error)))
				     ((= i (length input)) val)
				     (t (error 'parse-error)))) )
	    (t (error 'parse-error)))) ))
	  

;;;    The number may be constrained by optional min and max
;;;    args (which are inclusive).
; (defun get-num (prompt &optional min max)
;   (format t "~A" prompt)
;   (or (valid-num (read-from-string (read-line) nil nil) min max)
;       (get-num prompt min max)) )

; ;;;
; ;;;    GET-NUM should send in 3 args, but by making MIN and MAX optional
; ;;;    this can be used by other functions too.
; ;;;    
; (defun valid-num (num &optional min max)
;   (and (numberp num)
;        (if min (>= num min) t)
;        (if max (<= num max) t)
;        num) )

;;;
;;;    Christopher Stacy
;;;    
;; (defun read-a-number (&key stream default)
;;   (let* ((line (read-line stream))
;;          (n (let* ((*read-eval* nil))
;;               (ignore-errors (read-from-string line nil)))))
;;     (if (numberp n)
;;         n
;;       default)))

;;;
;;;    Matthew Danish
;;;    
;; (defun parse-float (string &optional (default 0.0))
;;   (let* ((*read-eval* nil)
;; 	 (val (ignore-errors (read-from-string string))))
;;     (typecase val
;;       (number val)
;;       (t default))))

;-------------Time/Date Functions------------------------

;;;
;;;    localtime
;;;
;;;    Return Perl-like date/time string (a la scalar(localtime))
;;;          DOW MON DD HH:MM:SS YYYY
;;;    E.g., Fri Aug  3 16:47:27 2001
;;;
(defun localtime (&optional (time (get-universal-time)))
  (let ((month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	(days-of-week '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
    (multiple-value-bind (second minute hour date month year day-of-week
			  daylight-savings-p time-zone)
      (decode-universal-time time)
      (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~4D"
	      (nth day-of-week days-of-week)
	      (nth (1- month) month-names)
	      date hour minute second year))) )

(defun localtime-list (&optional (time (get-universal-time)))
  (multiple-value-list (decode-universal-time time)) )

(defun yyyy-mm-dd (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day-of-week
			       daylight-savings-p time-zone)
      (decode-universal-time time)
    (format nil "~4D-~2,'0D-~2,'0D" year month date)) )
;---------------Code-testing Functions------------------------
;;;
;;;    Should return how many tests passed/failed?
;;;    
(defun test (f test-data)
  "Perform automated testing of function F using TEST-DATA. The data should consist of a list of lists of trials. Each sublist should consist of a list of args to which F is applied and an expected result. The expected result is compared to the actual result."
  (format t "~&Testing function: ~A~%" f)
  (if (documentation f 'function)
      (format t "~4T~A~%" (documentation f 'function)))
  (format t "~%")
  (dolist (trial test-data)
    (let* ((inputs (car trial))
	   (expected (cadr trial))
	   (result (apply f inputs)))
      (if (equal result expected)
	  (format t "~&Test passed: ~{ ~S~} => ~S~%" inputs result)
	  (format t "~&Test FAILED: ~{ ~S~} => ~S [Should be ~S]~%"
		  inputs result expected))))
  (format t "~%") )

;---------------Miscellaneous Functions------------------------
;;;
;;;    Set permissions on named file
;;;
;;;    CLISP:
;;;    (ext:execute <program> &rest <args>)
;;;    (ext:shell &optional <command-string>)
;;;    (ext:run-shell-command command &key :input :output :if-output-exists
;;;      :wait)
;;;    (ext:run-program program &key :arguments :input :output
;;;      :if-output-exists :wait)
;;;
;; (defun set-file-permissions (file permissions)
;;   (let ((chmod "/bin/chmod"))
;;     #+:sbcl (sb-ext:run-program chmod
;; 				(list (format nil "~D" permissions)
;; 				      file))
;; ;;     #+(or :sbcl :cmu) (#+:sbcl sb-ext:run-program run-program chmod
;; ;; 			  (list (format nil "~D" permissions)
;; ;; 				file))
;; ;    #+:allegro (run-shell-command ...?
;; ;    #+:lispworks (system:open-pipe fullstring :direction :io)
;;     #+:clisp (ext:run-shell-command
;; 	      (format nil "chmod ~D ~A" permissions file)) ))

(defun set-file-permissions (file permissions)
  #+ :sbcl (sb-ext:run-program "/bin/chmod"
			(list (format nil "~D" permissions)
			      file))

  #+ :clisp (run-shell-command (format nil "chmod ~D ~A" permissions file)) )

;;;
;;;    From LTK
;;;    
;; (defun do-execute (program args &optional (wt nil))
;;   "execute program with args a list containing the arguments passed to the program
;;    if wt is non-nil, the function will wait for the execution of the program to return.
;;    returns a two way stream connected to stdin/stdout of the program"
  
;;   (let ((fullstring program))
;;     (dolist (a args)
;;       (setf fullstring (concatenate 'string fullstring " " a)))
;;     #+:cmu (let ((proc (run-program program args :input :stream :output :stream :wait wt)))
;;              (unless proc
;;                (error "Cannot create process."))
;;              (make-two-way-stream
;;               (ext:process-output proc)
;;               (ext:process-input proc))
;;              )
;;     #+:clisp (let ((proc (ext:run-program program :arguments args :input :stream :output :stream :wait t)))
;;              (unless proc
;;                (error "Cannot create process."))
;; 	     proc
;;              )
;;     #+:sbcl (let ((proc (sb-ext:run-program program args :input :stream :output :stream :wait wt)))
;;              (unless proc
;;                (error "Cannot create process."))
;; 	     (make-two-way-stream 
;; 	      (process-output proc)              
;; 	      (process-input proc))	     
;;              )
;;     #+:lispworks (system:open-pipe fullstring :direction :io)
;;     #+:allegro (let ((proc (excl:run-shell-command
;;                          (apply #'vector program program args)
;;                          :input :stream :output :stream :wait wt)))
;; 		(unless proc
;; 		  (error "Cannot create process."))   
;; 		proc
;; 		)
;;     #+:ecl(ext:run-program program args :input :stream :output :stream
;; :error :output)
;;      #+:openmcl (let ((proc (ccl:run-program program args :input
;; :stream :output :stream :wait wt)))
;; 		  (unless proc
;; 		    (error "Cannot create process."))
;; 		  (make-two-way-stream
;; 		   (ccl:external-process-output-stream proc)
;; 		   (ccl:external-process-input-stream proc)))
;;     ))

;;;
;;;    Create a list of successive integers from START to END.
;;;    (Like Perl's (START..END))
;;;    See below...
; (defun make-range (&optional (start 0) (end 0))
;   (if (> start end)
;       (rotatef start end))
;   (mapcar #'(lambda (x)
; 	      (setf x start)
; 	      (incf start)
; 	      x)
; 	  (make-list (1+ (- end start)))) )

;;;
;;;    Like warn() in Perl
;;;    
; (defun alert (&rest args)
;   (apply #'format *error-output* args)
;   (finish-output *error-output*) )
;;; Lisp already has a WARN function!!!!

(defun print-plist (sym)
  (do ((plist (symbol-plist sym) (cddr plist)))
      ((null plist))
    (format t "Property: ~S~%" (car plist))
    (format t "Value: ~S~%" (cadr plist))))

;;;
;;;    Convert NIL to ()
;;;    
(defun list-to-string (l)
  (with-output-to-string (s)
    (format s "(")
    (when l
      (list-to-string-aux l s))
    (format s ")")))

(defun list-to-string-aux (obj s)
  (cond ((atom obj) (format s "~S" obj))
	(t (if (listp (car obj))
	       (format s "~A" (list-to-string (car obj)))
	       (list-to-string-aux (car obj) s))
	   (unless (null (cdr obj))
	     (format s " ")
	     (list-to-string-aux (cdr obj) s)))))

;;;
;;;    Pascal Bourguignon
;;;    
(defun show-symbols (package-name)
  (do-external-symbols (s package-name)
    (when (fboundp s) 
      (format t "~40A : ~A~%" s
	      (cond
		((special-operator-p s) "special-operator")
		((macro-function s) "macro")
		(t "function"))))
    (when (boundp s)
      (format t "~40A : ~A~%" s
	      (cond
		((constantp s) "constant variable")
		(t "variable")))) ))

;;;
;;;    Surprisingly (?) the tail-recursive version is faster than the iterative
;;;    version. (Because of the extra AND test on every loop?)
;;;    The naive version is the slowest. It does the most CONSing. The other
;;;    two do less (the same amount, in fact).
;;;
;;;    Note: The new list returned may share structure with the original!
;;;    
(defun transfer (l i)
  "Remove the ith element from a list, returning the element and the new list."
  (labels ((transfer-aux (l i result)
             (cond ((endp l) (error "Could not transfer element"))
                   ((zerop i) (values (car l)
                                      (nconc (nreverse result) (cdr l))))
;                                     (append (nreverse result) (cdr l))))
                   (t (transfer-aux (cdr l)
                                    (1- i)
                                    (cons (car l) result)))) ))
    (transfer-aux l i '())))

;;;
;;;    The following differs slightly from TRANSFER:
;;;
;; * (transfer '() 0)

;; debugger invoked on a SIMPLE-ERROR: Could not transfer element

;; Type HELP for debugger help, or (SB-EXT:QUIT) to exit from SBCL.

;; restarts (invokable by number or by possibly-abbreviated name):
;;   0: [ABORT] Exit debugger, returning to top level.

;; (TRANSFER NIL 0)
;; 0] :a

;; * (transfer-1 '() 0)

;; NIL
;; NIL
;; * (transfer '(()) 0)

;; NIL
;; NIL
;; * (transfer-1 '(()) 0)

;; NIL
;; NIL
(defun transfer-1 (l n)
  (let* ((result (list '()))
	 (tail result))
    (do ((list (cdr l) (cdr list))
	 (elt (car l) (car list))
	 (i 0 (1+ i)))
	((= i n) (setf (cdr tail) list) (values elt (cdr result)))
      (if (endp list)
	  (error "Could not transfer element")
	  (setf tail (setf (cdr tail) (list elt)))) )))
	
(defun transfer-2 (l n)
  (let* ((accum (list '()))
	 (tail accum))
    (loop for elt in l
	  for cons on (rest l)
	  for i from 0 below n
	  do (setf tail (setf (cdr tail) (list elt)))
	  finally (cond ((< i (1- n)) (error "Could not transfer element"))
			(t (setf (cdr tail) cons)
			   (return (values elt (cdr accum)))) ))))

;;;
;;;    Slightly different behavior:
;;;    (transfer-3 (make-numlist 20) 19) =>
;;;    20
;;;    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
;;;
;;;    (transfer-3 (make-numlist 20) 20) =>
;;;    20
;;;    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
(defun transfer-3 (l n)
  (loop for elt in l
	for cons on (rest l)
	for i from 0 below n
	collect elt into result
	finally (return (values elt (nconc result cons)))) )

;;;
;;;    Christian Haselbach
;;;    
;; (defun but-nth (l n)
;;   "Returns a list with the n-th element removed. Destroyes the input list."
;;   (cond
;;     ((>= n (length l)) (values nil l))
;;     ((= 0 n) (values (car l) (cdr l)))
;;     (t (let* ((lp (nthcdr (1- n) l))
;; 	      (x (cadr lp)))
;;      (setf (cdr lp) (cddr lp))
;;      (values x l)))))

	
;; (defun but-nth-1 (n l)
;;   "Returns a list with the n-th element removed."
;;   (cond
;;     ((>= n (length l)) (values nil l))
;;     ((= 0 n) (values (car l) (cdr l)))
;;     (t (let* ((l (copy-seq l))
;; 	      (lp (nthcdr (1- n) l))
;; 	      (x (cadr lp)))
;;      (setf (cdr lp) (cddr lp))
;;      (values x l)))))

; (defun transfer-a (l i)
;   (let ((elt (nth i l)))
;     (values elt (remove elt l :start i :count 1))))

; (defun transfer-b (l i)
;   "Remove the ith element from a list, returning the element and the new list."
;   (do ((result '() (cons (car list) result))
;        (list l (cdr list))
;        (index i (1- index)))
;       ((and list (zerop index)) (values (car list)
; 					(append (nreverse result) (cdr list))))
;     (when (null list)
;       (error "Could not transfer element"))))

(defun drop (l n)
  "Drop the first N elements of list L."
  (assert (typep n `(integer 0 ,(length l)))
	  (n)
	  "N must be a non-negative integer less than or equal to ~D."
	  (length l))
  (nthcdr n l))

(defun take (l n)
  "Take the first N elements of list L."
  (assert (listp l) (l) "L should be a list.")
  (assert (typep n `(integer 0 ,(length l)))
	  (n)
	  "N must be a non-negative integer less than or equal to ~D."
	  (length l))
  (labels ((take-aux (l n take)
	     (cond ((zerop n) (nreverse take))
		   (t (take-aux (cdr l) (1- n) (cons (car l) take)))) ))
    (take-aux l n '())))

;; (defun take-drop (l n)
;;   (values (butlast l (- (length l) n))
;; 	  (nthcdr n l)))

;; (defun take-drop (l n)
;;   "Split a list at the Nth element. Return the sublists before and after."
;;   (list (butlast l (- (length l) n))
;; 	(nthcdr n l)))

(defun take-drop (l n)
  "Split a list at the Nth element. Return the sublists before and after."
  (assert (typep n `(integer 0 ,(length l)))
	  (n)
	  "N must be a non-negative integer less than or equal to ~D."
	  (length l))
  (labels ((take-drop-aux (l n take)
	     (cond ((zerop n) (values (nreverse take) l))
		   (t (take-drop-aux (cdr l) (1- n) (cons (car l) take)))) ))
    (take-drop-aux l n '())))

;---------------Macros------------------------
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body) )

(defmacro while (test &body body)
  `(loop (unless ,test (return))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body) )

(defmacro dovec ((var vector-exp &optional result) &body body)
  (let ((v (gensym))
	(l (gensym))
	(i (gensym)))
    `(do* ((,v ,vector-exp)
	   (,l (length ,v))
	   (,i 0 (1+ ,i))
	   (,var nil))
          ((= ,i ,l) ,result)
       (setf ,var (aref ,v ,i))
       ,@body)))

;; (defmacro read-from-file ((file-name var) &body body)
;;   (let ((stream (gensym))
;; 	(eof (gensym)))
;;     `(with-open-file (,stream ,file-name :if-does-not-exist nil)
;;       (cond ((null ,stream) (warn "File does not exist!~%"))
;; 	    (t (do* ((,eof (list 'eof))
;; 		     (,var (read-line ,stream nil ,eof)
;; 			   (read-line ,stream nil ,eof)))
;; 		   ((eq ,var ,eof))
;; 		 ,@body)))) ))


;---------------Touretzky------------------------
(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
    (cond ((equal exp exp1)
	   (format t "~&Macro expansion:")
	   (pprint exp))
	  (t (format t "~&First step of expansion:")
	     (pprint exp1)
	     (format t "~2%Final expansion:")
	     (pprint exp)))
    (format t "~2%")
    (values)))

;---------------Graham On Lisp------------------------
(defun last1 (l)
  (car (last l)))

(defun single (l)
  (and (consp l) (null (cdr l))))

(defun append1 (l obj)
  (append l (list obj)))

(defun conc1 (l obj)
  (nconc l (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

;;;
;;;    Graham calls this LONGER. I believe this is inappropriate since this
;;;    function does not return the 'longer' of the two sequences. Rather, it
;;;    simply tests whether L1 is longer than L2.
;;;    
(defun longerp (l1 l2)
  (labels ((compare (l1 l2)
	     (and (consp l1)
		  (or (null l2)
		      (compare (cdr l1) (cdr l2)))) ))
    (if (and (listp l1) (listp l2))
	(compare l1 l2)
	(> (length l1) (length l2)))) )

(defun filter (fn list)
  (let ((acc '()))
    (dolist (x list)
      (let ((val (funcall fn x)))
	(when val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (when (zerop n) (error "Invalid length."))
  (labels ((group-aux (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (group-aux rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc)))) ))
    (if source
	(group-aux source '())
	'())))

(defun flatten (obj)
  (labels ((flatten-aux (obj results)
             (cond ((null obj) results)
                   ((atom obj) (cons obj results))
                   (t (flatten-aux (car obj)
				   (flatten-aux (cdr obj) results)))) ))
    (flatten-aux obj '()))) 

(defun prune (pred obj)
  (labels ((prune-aux (obj acc)
	     (cond ((null obj) (nreverse acc))
		   ((atom (car obj)) (prune-aux (cdr obj)
						(if (funcall pred (car obj))
						    acc
						    (cons (car obj) acc))))
		   (t (prune-aux (cdr obj)
				 (cons (prune-aux (car obj) '()) acc)))) ))
    (prune-aux obj '())))

(defun prune-if-not (pred obj)
  (labels ((prune-aux (obj acc)
	     (cond ((null obj) (nreverse acc))
		   ((atom (car obj)) (prune-aux (cdr obj)
						(if (funcall pred (car obj))
						    (cons (car obj) acc)
						    acc)))
		   (t (prune-aux (cdr obj)
				 (cons (prune-aux (car obj) '()) acc)))) ))
    (prune-aux obj '())))

(defun find2 (fn list)
  (if (null list)
      nil
      (let ((val (funcall fn (car list))))
	(if val
	    (values (car list) val)
	    (find2 fn (cdr list)))) ))

;;;
;;;    Does X occur before Y in list?
;;;    Y need not actually occur in the list! (See AFTER)
;;;    
(defun before (x y list &key (test #'eql))
  (and list
       (let ((first (car list)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) list)
	       (t (before x y (cdr list) :test test)))) ))

(defun after (x y list &key (test #'eql))
  (let ((rest (before y x list :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj list &key (test #'eql))
  (member obj (cdr (member obj list :test test)) :test test))

(defun split-if (fn list)
  (let ((acc '()))
    (do ((src list (cdr src)))
	((or (null src)
	     (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn list)
  (if (null list)
      (values nil nil)
      (let* ((wins (car list))
             (max (funcall fn wins)))
        (dolist (obj (cdr list))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
		    max score))))
        (values wins max))))

(defun best (fn list)
  (if (null list)
      nil
      (let ((wins (car list)))
        (dolist (obj (cdr list))
          (when (funcall fn obj wins)
	    (setq wins obj)))
        wins)))

(defun mostn (fn list)
  (if (null list)
      (values nil nil)
      (let ((result (list (car list)))
            (max (funcall fn (car list))))
        (dolist (obj (cdr list))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

;;;
;;;    What about nil as an element?
;;;    
(defun tree-find-if (fn obj)
  (cond ((null obj) nil)
	((atom obj) (and (funcall fn obj) obj))
	(t (or (tree-find-if fn (first obj))
	       (tree-find-if fn (rest obj)))) ) )

;;;
;;;    Split a list into successive CDRs and everything before that CDR.
;;;    
(defun partition (l)
  (do ((acc '() (cons (list head tail) acc))
       (head '() (append1 head (car tail)))
       (tail l (cdr tail)))
      ((null tail) (cons (list head tail) acc))))

; (defun partition-1 (l)
;   (labels ((partition-aux (l1 l2 results)
; 	     (cond ((null l2) (cons (list l1 l2) results))
; 		   (t (partition-aux (append1 l1 (car l2))
; 				     (cdr l2)
; 				     (cons (list l1 l2) results)))) ))
;     (partition-aux '() l '())))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result '()))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun make-range (start end &optional (step 1))
  (cond ((characterp start)
	 (if (characterp end)
	     (mapcar #'code-char (make-range (char-code start)
					     (char-code end)
					     step))
	     (error "Mismatched input types.")))
	(t (when (> start end)
	     (rotatef start end))
	   (loop for i from start to end by step collect i))))

;; (defun make-range (start end &optional (step 1))
;;   (cond ((characterp start)
;; 	 (if (characterp end)
;; 	     (mapcar #'code-char (make-range (char-code start)
;; 					     (char-code end)
;;                                           step))
;; 	     (error "Mismatched input types.")))
;; 	(t (when (> start end)
;; 	     (rotatef start end))
;; 	   (do ((i start (+ i step))
;; 		(result '()))
;; 	       ((> i end) (nreverse result))
;; 	     (push i result)))) )

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result '()))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result '()))
    (dolist (l lsts)
      (dolist (obj l)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar #'(lambda (&rest args)
			  (apply #'rmapcar fn args))
	     args)))

(defun readlist (&rest args)
  (values (read-from-string (concatenate 'string
					 "("
					 (apply #'read-line args)
					 ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
	 (return)
	 (format *query-io* "~A~%" (funcall fn in)))) ))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))) )))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (last1 fns))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))

;;;
;;;    Slightly modified from Graham.
;;;    
(defun fif (if then &optional else)
  (if else
      #'(lambda (x)
	  (if (funcall if x)
	      (funcall then x)
	      (funcall else x)))
      #'(lambda (x)
	  (if (funcall if x)
	      (funcall then x)))) )

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x)))) ))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	#'(lambda (x)
	    (or (funcall fn x) (funcall chain x)))) ))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop , stop))
         ((> ,var ,gstop))
       ,@body)))

					 