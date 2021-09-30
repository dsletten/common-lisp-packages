;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               io.lisp
;;;;
;;;;   Started:            Fri Jan  6 19:12:16 2006
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

(defpackage :io
  (:use :common-lisp)
  (:export :add-column
           :atime
;	   "BREAK-LOOP"
           :copy-file
           :ctime
           :file-each-line
;	   "GET-NUM"
           :mtime
           :number-file
;	   "PROMPT"
           :read-file
           :read-file-as-string
;	   "READLIST"
           :search-file :search-lines-of-file
;	   "VALID-NUM"
           :write-file))

(in-package :io)

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require 'sb-posix))

;----------I/O Functions-----------------
;;;
;;;    Read each line of a file into a list of strings.
;;;    Returns nil if file does not exist.
;;;    
;; (defun read-file (file-name)
;;   (with-open-file (in-stream file-name :if-does-not-exist nil)
;;     (if in-stream
;;         (loop for line = (read-line in-stream nil nil)
;;               while line
;;               collect line)
;;         (format *error-output* "Error: File does not exist!~%"))))

(defun read-file (file-name)
  (with-open-file (in-stream file-name :if-does-not-exist nil)
    (if (null in-stream)
        (format *error-output* "Error: File does not exist!~%")
        (loop for line = (read-line in-stream nil nil)
              until (null line)
              collect line))))

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

(defun read-file-as-string (file-name)
  (with-open-file (in-stream file-name :if-does-not-exist nil)
    (if in-stream
        (with-output-to-string (result)
          (loop for line = (read-line in-stream nil nil)
                while line
                do (write-line line result)))
        (format *error-output* "Error: File does not exist!~%"))))

;;;
;;;    Edi Weitz
;;;
(defun read-file-as-string (file-name)
  (with-open-file (in-stream file-name :if-does-not-exist nil)
    (if in-stream
        (let* ((result (make-string (file-length in-stream)))
               (pos (read-sequence result in-stream)))
          (subseq result 0 pos))
        (format *error-output* "Error: File does not exist!~%"))))

;;;
;;;    Wade Humeniuk
;;;
;; (defun read-file-as-string (file-name)
;;   (with-open-file (in-stream file-name)
;;     (if in-stream
;;         (let ((result (make-array (file-length in-stream)
;;                                   :element-type 'character
;;                                   :adjustable t
;;                                   :fill-pointer t)))
;;           (setf (fill-pointer result) (read-sequence result in-stream))
;;           result)
;;         (format *error-output* "Error: File does not exist!~%"))))

(defun file-each-line (file-name f)
  (dolist (line (read-file file-name))
    (funcall f line)))

(defun number-file (file-name)
  (let ((i 1))
    (file-each-line file-name #'(lambda (line) (format t "~4,'0D: ~A~%" i line) (incf i)))) )

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
;;;    Lauri Siponen, Helsinki University of Technology
;;;    (From CMU repository)
;;;    (Modified slightly)
;;;    
(defun copy-file (source target &optional overwrite)
  (with-open-file (in source
                   :direction :input
                   :if-does-not-exist nil)
    (with-open-file (out target 
                     :direction :output
                     :if-exists (if overwrite :supersede nil))
      (cond ((null in) (format *error-output* "Error: Source file does not exist!~%"))
            ((null out) (format *error-output* "Error: Target already exists!~%"))
            (t (loop with buffer = (make-string (* 64 512))
                     for n = (read-sequence buffer in)
	                 until (= n 0)
	                 do (write-sequence buffer out :end n)))) )))

; CMUCL (unix:unix-stat file)...
(defun atime (file)
  #+sbcl (sb-posix:stat-atime (sb-posix:stat file))
  #+clisp (posix:file-stat-atime (posix:file-stat file))
  #-(or sbcl clisp)
  (error "ATIME not implemented"))

(defun ctime (file)
  #+sbcl (sb-posix:stat-ctime (sb-posix:stat file))
  #+clisp (posix:file-stat-ctime (posix:file-stat file))
  #-(or sbcl clisp)
  (error "CTIME not implemented"))

(defun mtime (file)
  #+sbcl (sb-posix:stat-mtime (sb-posix:stat file))
  #+clisp (file-stat-mtime (file-stat file))
  #-(or sbcl clisp)
  (error "MTIME not implemented"))

#|
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
|#

#|
SB-POSIX:CREAD 
SB-POSIX:VQUIT 
SB-POSIX:EPERM 
SB-POSIX:ELIBSCN 
SB-POSIX:TCOON 
SB-POSIX:ENODEV 
SB-POSIX:O-RDWR 
SB-POSIX:TIMEVAL-USEC 
SB-POSIX:O-DIRECTORY 
SB-POSIX:OPENLOG 
SB-POSIX:R-OK 
SB-POSIX:TCSAFLUSH 
SB-POSIX:TCSADRAIN 
SB-POSIX:LOG-AUTHPRIV 
SB-POSIX:ENOTDIR 
SB-POSIX:OFILL 
SB-POSIX:ONLRET 
SB-POSIX:VSTOP 
SB-POSIX:ETOOMANYREFS 
SB-POSIX:CR1 
SB-POSIX:PUTENV 
SB-POSIX:LOG-DAEMON 
SB-POSIX:B57600 
SB-POSIX:UMASK 
SB-POSIX:MS-INVALIDATE 
SB-POSIX:WIFSTOPPED 
SB-POSIX:ENFILE 
SB-POSIX:B600 
SB-POSIX:CREAT 
SB-POSIX:S-IFWHT 
SB-POSIX:S-ISBLK 
SB-POSIX:EMEDIUMTYPE 
SB-POSIX:LOG-LOCAL6 
SB-POSIX:SYSCALL-ERRNO 
SB-POSIX:CS5 
SB-POSIX:O-CREAT 
SB-POSIX:WTERMSIG 
SB-POSIX:EBUSY 
SB-POSIX:EBFONT 
SB-POSIX:LOG-LOCAL7 
SB-POSIX:EFBIG 
SB-POSIX:ICANON 
SB-POSIX:CS8 
SB-POSIX:S-IXOTH 
SB-POSIX:B134 
SB-POSIX:CFSETISPEED 
SB-POSIX:W-OK 
SB-POSIX:ERANGE 
SB-POSIX:EBADR 
SB-POSIX:SYSCALL-ERROR 
SB-POSIX:ECHONL 
SB-POSIX:SIGXCPU 
SB-POSIX:IOCTL 
SB-POSIX:IGNCR 
SB-POSIX:LOG-LOCAL3 
SB-POSIX:ECHOE 
SB-POSIX:F-OK 
SB-POSIX:LOG-ALERT 
SB-POSIX:ENETDOWN 
SB-POSIX:ENETUNREACH 
SB-POSIX:OPEN 
SB-POSIX:SIGTSTP 
SB-POSIX:VERASE 
SB-POSIX:LSEEK 
SB-POSIX:PASSWD-SHELL 
SB-POSIX:ECOMM 
SB-POSIX:S-IWOTH 
SB-POSIX:UTIME 
SB-POSIX:GETPAGESIZE 
SB-POSIX:S-ISREG 
SB-POSIX:EL2HLT 
SB-POSIX:ECHRNG 
SB-POSIX:MMAP 
SB-POSIX:FSYNC 
SB-POSIX:PROT-WRITE 
SB-POSIX:EBADF 
SB-POSIX:B200 
SB-POSIX:ENOTEMPTY 
SB-POSIX:SETEUID 
SB-POSIX:EIO 
SB-POSIX:SIGTRAP 
SB-POSIX:SIGBUS 
SB-POSIX:F-TLOCK 
SB-POSIX:RENAME 
SB-POSIX:S-IFCHR 
SB-POSIX:SETPGID 
SB-POSIX:ISTRIP 
SB-POSIX:EADDRINUSE 
SB-POSIX:FF0 
SB-POSIX:LOG-NOTICE 
SB-POSIX:SIGPWR 
SB-POSIX:LOG-USER 
SB-POSIX:SIGIO 
SB-POSIX:S-ISDIR 
SB-POSIX:IXOFF 
SB-POSIX:S-IROTH 
SB-POSIX:SETUID 
SB-POSIX:WEXITSTATUS 
SB-POSIX:EPFNOSUPPORT 
SB-POSIX:OPOST 
SB-POSIX:O-RSYNC 
SB-POSIX:ELIBEXEC 
SB-POSIX:ENOTNAM 
SB-POSIX:DUP2 
SB-POSIX:ICRNL 
SB-POSIX:S-IRUSR 
SB-POSIX:TAB1 
SB-POSIX:EPROTOTYPE 
SB-POSIX:TERMIOS-LFLAG 
SB-POSIX:B75 
SB-POSIX:SIGPROF 
SB-POSIX:SEEK-SET 
SB-POSIX:S-IWUSR 
SB-POSIX:GROUP 
SB-POSIX:EUNATCH 
SB-POSIX:NOFLSH 
SB-POSIX:WAIT 
SB-POSIX:F-GETOWN 
SB-POSIX:FLOCK-PID 
SB-POSIX:ENOCSI 
SB-POSIX:VEOL 
SB-POSIX:ENOSR 
SB-POSIX:CSIZE 
SB-POSIX:PASSWD-NAME 
SB-POSIX:SIGCHLD 
SB-POSIX:FTRUNCATE 
SB-POSIX:O-ASYNC 
SB-POSIX:FF1 
SB-POSIX:PASSWD-PASSWD 
SB-POSIX:ESOCKTNOSUPPORT 
SB-POSIX:LOG-LOCAL5 
SB-POSIX:LOCKF 
SB-POSIX:B0 
SB-POSIX:SETRESUID 
SB-POSIX:SIGALRM 
SB-POSIX:B300 
SB-POSIX:MCL-CURRENT 
SB-POSIX:TIMEVAL-SEC 
SB-POSIX:PIPE 
SB-POSIX:NL0 
SB-POSIX:TERMIOS-OFLAG 
SB-POSIX:ENOSPC 
SB-POSIX:CHOWN 
SB-POSIX:B230400 
SB-POSIX:O-LARGEFILE 
SB-POSIX:ETIME 
SB-POSIX:READLINK 
SB-POSIX:WIFEXITED 
SB-POSIX:EUCLEAN 
SB-POSIX:UNLINK 
SB-POSIX:S-IFSOCK 
SB-POSIX:BS0 
SB-POSIX:PASSWD-GID 
SB-POSIX:MCL-FUTURE 
SB-POSIX:LOG-FTP 
SB-POSIX:EL3HLT 
SB-POSIX:STAT-INO 
SB-POSIX:LOG-UUCP 
SB-POSIX:F-SETFD 
SB-POSIX:CLOSEDIR 
SB-POSIX:LSTAT 
SB-POSIX:EREMOTE 
SB-POSIX:S-IFMT 
SB-POSIX:PASSWD 
SB-POSIX:GETGRNAM 
SB-POSIX:O-DSYNC 
SB-POSIX:S-ISUID 
SB-POSIX:GETUID 
SB-POSIX:CLOSELOG 
SB-POSIX:DIRENT-NAME 
SB-POSIX:ETXTBSY 
SB-POSIX:EFAULT 
SB-POSIX:ENOBUFS 
SB-POSIX:STAT-CTIME 
SB-POSIX:EMSGSIZE 
SB-POSIX:LOG-MAIL 
SB-POSIX:EINVAL 
SB-POSIX:O-APPEND 
SB-POSIX:PROT-NONE 
SB-POSIX:S-ISVTX 
SB-POSIX:O-NONBLOCK 
SB-POSIX:ECHILD 
SB-POSIX:TERMIOS-CFLAG 
SB-POSIX:FCNTL 
SB-POSIX:SIGCONT 
SB-POSIX:FORK 
SB-POSIX:GETEUID 
SB-POSIX:IXON 
SB-POSIX:STAT-NLINK 
SB-POSIX:CHMOD 
SB-POSIX:EUSERS 
SB-POSIX:EIDRM 
SB-POSIX:NL1 
SB-POSIX:EILSEQ 
SB-POSIX:F-DUPFD 
SB-POSIX:SIGQUIT 
SB-POSIX:MKSTEMP 
SB-POSIX:SIGTTIN 
SB-POSIX:TCIOFLUSH 
SB-POSIX:ELIBBAD 
SB-POSIX:EXFULL 
SB-POSIX:EDOTDOT 
SB-POSIX:TERMIOS-IFLAG 
SB-POSIX:O-NOCTTY 
SB-POSIX:SYMLINK 
SB-POSIX:TCSETATTR 
SB-POSIX:MAP-FIXED 
SB-POSIX:TERMIOS 
SB-POSIX:ENOMSG 
SB-POSIX:MKDTEMP 
SB-POSIX:ELIBACC 
SB-POSIX:TAB2 
SB-POSIX:ELOOP 
SB-POSIX:LOG-CONS 
SB-POSIX:UTIMES 
SB-POSIX:EL3RST 
SB-POSIX:SETPGRP 
SB-POSIX:EREMCHG 
SB-POSIX:EINPROGRESS 
SB-POSIX:S-IFBLK 
SB-POSIX:MS-ASYNC 
SB-POSIX:ELNRNG 
SB-POSIX:DUP 
SB-POSIX:GETENV 
SB-POSIX:ECONNREFUSED 
SB-POSIX:LOG-LOCAL1 
SB-POSIX:MLOCKALL 
SB-POSIX:SIGINT 
SB-POSIX:FLOCK-START 
SB-POSIX:B115200 
SB-POSIX:VSUSP 
SB-POSIX:O-NOFOLLOW 
SB-POSIX:CR0 
SB-POSIX:EDEADLK 
SB-POSIX:LOG-LOCAL0 
SB-POSIX:GETRESGID 
SB-POSIX:LINK 
SB-POSIX:LOG-CRON 
SB-POSIX:CFSETOSPEED 
SB-POSIX:EAGAIN 
SB-POSIX:SIGURG 
SB-POSIX:GROUP-NAME 
SB-POSIX:EALREADY 
SB-POSIX:PASSWD-GECOS 
SB-POSIX:VKILL 
SB-POSIX:S-IRGRP 
SB-POSIX:EHOSTDOWN 
SB-POSIX:X-OK 
SB-POSIX:RMDIR 
SB-POSIX:ENOPROTOOPT 
SB-POSIX:F-TEST 
SB-POSIX:ESHUTDOWN 
SB-POSIX:ENOPKG 
SB-POSIX:BSDLY 
SB-POSIX:SETGID 
SB-POSIX:O-SYNC 
SB-POSIX:S-ISGID 
SB-POSIX:OCRNL 
SB-POSIX:S-ISSOCK 
SB-POSIX:EDEADLOCK 
SB-POSIX:LOG-NEWS 
SB-POSIX:PAUSE 
SB-POSIX:ENXIO 
SB-POSIX:SPEED-T 
SB-POSIX:SIGUSR2 
SB-POSIX:STAT 
SB-POSIX:S-IFDIR 
SB-POSIX:LOG-LOCAL4 
SB-POSIX:VT0 
SB-POSIX:TABDLY 
SB-POSIX:EINTR 
SB-POSIX:FLOCK-LEN 
SB-POSIX:SIGSYS 
SB-POSIX:F-WRLCK 
SB-POSIX:TCOOFF 
SB-POSIX:LOG-LOCAL2 
SB-POSIX:ESTRPIPE 
SB-POSIX:SETFSGID 
SB-POSIX:ENOTCONN 
SB-POSIX:CHDIR 
SB-POSIX:S-IXGRP 
SB-POSIX:TERMIOS-CC 
SB-POSIX:ESPIPE 
SB-POSIX:EPIPE 
SB-POSIX:SETEGID 
SB-POSIX:TCFLAG-T 
SB-POSIX:AF-INET 
SB-POSIX:SETREUID 
SB-POSIX:SIGRTMIN 
SB-POSIX:SIGRTMAX 
SB-POSIX:B4800 
SB-POSIX:PASSWD-UID 
SB-POSIX:GETPID 
SB-POSIX:TAB0 
SB-POSIX:MKFIFO 
SB-POSIX:EISDIR 
SB-POSIX:BRKINT 
SB-POSIX:EBADMSG 
SB-POSIX:EMFILE 
SB-POSIX:EPROTO 
SB-POSIX:PROT-READ 
SB-POSIX:F-GETLK 
SB-POSIX:O-DIRECT 
SB-POSIX:ECONNRESET 
SB-POSIX:WAITPID 
SB-POSIX:GROUP-PASSWD 
SB-POSIX:PASSWD-DIR 
SB-POSIX:WIFSIGNALED 
SB-POSIX:FDATASYNC 
SB-POSIX:PARMRK 
SB-POSIX:B1800 
SB-POSIX:TCIOFF 
SB-POSIX:STAT-ATIME 
SB-POSIX:EDOM 
SB-POSIX:LOG-NOWAIT 
SB-POSIX:SIGKILL 
SB-POSIX:TCIFLUSH 
SB-POSIX:EEXIST 
SB-POSIX:MS-SYNC 
SB-POSIX:CFGETOSPEED 
SB-POSIX:ENAMETOOLONG 
SB-POSIX:TCION 
SB-POSIX:SETSID 
SB-POSIX:O-EXCL 
SB-POSIX:MKTEMP 
SB-POSIX:S-IREAD 
SB-POSIX:TRUNCATE 
SB-POSIX:STAT-MODE 
SB-POSIX:KILL 
SB-POSIX:ELIBMAX 
SB-POSIX:MKDIR 
SB-POSIX:EACCES 
SB-POSIX:LCHOWN 
SB-POSIX:SIGWINCH 
SB-POSIX:S-IFLNK 
SB-POSIX:STAT-DEV 
SB-POSIX:CLOCAL 
SB-POSIX:ENOSYS 
SB-POSIX:S-IEXEC 
SB-POSIX:S-IFIFO 
SB-POSIX:TCSANOW 
SB-POSIX:NLDLY 
SB-POSIX:SIGSEGV 
SB-POSIX:B1200 
SB-POSIX:ENOTTY 
SB-POSIX:EADV 
SB-POSIX:S-IXUSR 
SB-POSIX:WSTOPSIG 
SB-POSIX:F-GETFD 
SB-POSIX:ENOEXEC 
SB-POSIX:B19200 
SB-POSIX:ENAVAIL 
SB-POSIX:B50 
SB-POSIX:EAFNOSUPPORT 
SB-POSIX:S-ISCHR 
SB-POSIX:EROFS 
SB-POSIX:SIGABRT 
SB-POSIX:GETGRGID 
SB-POSIX:LOG-DEBUG 
SB-POSIX:EHOSTUNREACH 
SB-POSIX:LOG-KERN 
SB-POSIX:TOSTOP 
SB-POSIX:ENOANO 
SB-POSIX:SETREGID 
SB-POSIX:EDESTADDRREQ 
SB-POSIX:GETPGID 
SB-POSIX:ENOENT 
SB-POSIX:IGNBRK 
SB-POSIX:SIGUSR1 
SB-POSIX:EBADE 
SB-POSIX:ENOSTR 
SB-POSIX:SYSLOG 
SB-POSIX:ENOMEM 
SB-POSIX:TCGETATTR 
SB-POSIX:SEEK-CUR 
SB-POSIX:CLOSE 
SB-POSIX:EMLINK 
SB-POSIX:GETPGRP 
SB-POSIX:GETGID 
SB-POSIX:LOG-LPR 
SB-POSIX:SIGXFSZ 
SB-POSIX:LOG-PID 
SB-POSIX:GETPWUID 
SB-POSIX:EXDEV 
SB-POSIX:ACCESS 
SB-POSIX:FCHMOD 
SB-POSIX:TIMEVAL 
SB-POSIX:SYNC 
SB-POSIX:S-ISFIFO 
SB-POSIX:ECHOK 
SB-POSIX:ESTALE 
SB-POSIX:EISCONN 
SB-POSIX:MAP-PRIVATE 
SB-POSIX:SIGILL 
SB-POSIX:SETUP-MACH-EXCEPTIONS 
SB-POSIX:SIGTTOU 
SB-POSIX:B2400 
SB-POSIX:ERESTART 
SB-POSIX:VTIME 
SB-POSIX:TAB3 
SB-POSIX:ENOTUNIQ 
SB-POSIX:CS7 
SB-POSIX:ENOMEDIUM 
SB-POSIX:B110 
SB-POSIX:MUNLOCKALL 
SB-POSIX:O-NDELAY 
SB-POSIX:STAT-UID 
SB-POSIX:SEEK-END 
SB-POSIX:ECONNABORTED 
SB-POSIX:F-GETFL 
SB-POSIX:E2BIG 
SB-POSIX:VT1 
SB-POSIX:BS1 
SB-POSIX:CFGETISPEED 
SB-POSIX:ENONET 
SB-POSIX:ENOLCK 
SB-POSIX:EDQUOT 
SB-POSIX:ETIMEDOUT 
SB-POSIX:CR3 
SB-POSIX:F-SETLK 
SB-POSIX:F-SETFL 
SB-POSIX:EPROTONOSUPPORT 
SB-POSIX:FFDLY 
SB-POSIX:SIGSTOP 
SB-POSIX:MAP-SHARED 
SB-POSIX:EOVERFLOW 
SB-POSIX:IGNPAR 
SB-POSIX:GETEGID 
SB-POSIX:F-LOCK 
SB-POSIX:NCCS 
SB-POSIX:ISIG 
SB-POSIX:EMULTIHOP 
SB-POSIX:SIGEMT 
SB-POSIX:SIGFPE 
SB-POSIX:EOPNOTSUPP 
SB-POSIX:F-SETOWN 
SB-POSIX:EWOULDBLOCK 
SB-POSIX:IEXTEN 
SB-POSIX:LOG-SYSLOG 
SB-POSIX:S-IWRITE 
SB-POSIX:MUNMAP 
SB-POSIX:GETRESUID 
SB-POSIX:ENOTBLK 
SB-POSIX:HUPCL 
SB-POSIX:SETRESGID 
SB-POSIX:SIGPIPE 
SB-POSIX:O-TRUNC 
SB-POSIX:EL2NSYNC 
SB-POSIX:STAT-GID 
SB-POSIX:S-IWGRP 
SB-POSIX:F-SETLKW 
SB-POSIX:ENODATA 
SB-POSIX:INPCK 
SB-POSIX:SIGTERM 
SB-POSIX:MSYNC 
SB-POSIX:F-RDLCK 
SB-POSIX:TIME 
SB-POSIX:FLOCK-TYPE 
SB-POSIX:GROUP-GID 
SB-POSIX:ESRCH 
SB-POSIX:ENOTSOCK 
SB-POSIX:B150 
SB-POSIX:PARENB 
SB-POSIX:ENETRESET 
SB-POSIX:PARODD 
SB-POSIX:KILLPG 
SB-POSIX:CR2 
SB-POSIX:ALARM 
SB-POSIX:TCOFLUSH 
SB-POSIX:CHROOT 
SB-POSIX:LOG-ODELAY 
SB-POSIX:FLOCK-WHENCE 
SB-POSIX:SIGVTALRM 
SB-POSIX:LOG-NDELAY 
SB-POSIX:EBADFD 
SB-POSIX:CRDLY 
SB-POSIX:VMIN 
SB-POSIX:GETCWD 
SB-POSIX:GETPPID 
SB-POSIX:OPENDIR 
SB-POSIX:FSTAT 
SB-POSIX:ENOLINK 
SB-POSIX:POSIX-FORK 
SB-POSIX:STAT-MTIME 
SB-POSIX:STAT-SIZE 
SB-POSIX:VTDLY 
SB-POSIX:S-ISLNK 
SB-POSIX:LOG-ERR 
SB-POSIX:S-IFREG 
SB-POSIX:LOG-WARNING 
SB-POSIX:LOG-EMERG 
SB-POSIX:INLCR 
SB-POSIX:SIGHUP 
SB-POSIX:EBADSLT 
SB-POSIX:SETFSUID 
SB-POSIX:VSTART 
SB-POSIX:GETSID 
SB-POSIX:FLOCK 
SB-POSIX:B9600 
SB-POSIX:FCHDIR 
SB-POSIX:GETPWNAM 
SB-POSIX:EREMOTEIO 
SB-POSIX:EBADRQC 
SB-POSIX:LOG-INFO 
SB-POSIX:F-ULOCK 
SB-POSIX:CS6 
SB-POSIX:EADDRNOTAVAIL 
SB-POSIX:PROT-EXEC 
SB-POSIX:O-RDONLY 
SB-POSIX:VEOF 
SB-POSIX:ECHO 
SB-POSIX:B38400 
SB-POSIX:LOG-PERROR 
SB-POSIX:LOG-CRIT 
SB-POSIX:F-UNLCK 
SB-POSIX:CSTOPB 
SB-POSIX:FCHOWN 
SB-POSIX:O-WRONLY 
SB-POSIX:VINTR 
SB-POSIX:ESRMNT 
SB-POSIX:READDIR 
|#
