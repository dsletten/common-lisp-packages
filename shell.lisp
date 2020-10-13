;;;;
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               shell.lisp
;;;;
;;;;   Started:            Fri Jan  6 19:12:48 2006
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
;;;;   Clozure -> get-argv?
;;;;   SBCL -> setenv?
;;;;
;;;;

(defpackage :shell
  (:use :common-lisp)
  (:export :default-directory :get-args :get-argv :getenv :run-shell-command :set-file-permissions :setenv))

(in-package :shell)

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require 'sb-posix))

;;;
;;;    SBCL command line args different depending on use of --script flag? 110612
;;;    See lshell.
;;;    
(defun get-argv (n)
  #+ :sbcl (if (>= n (length sb-ext:*posix-argv*))
               nil
               (nth (+ n 1) sb-ext:*posix-argv*))
  ;;
  ;;    These must come after -- on command line.
  ;;    See os-interface.htm
  ;;    
  #+ :allegro (if (>= (1+ n) (sys:command-line-argument-count))
                  nil
                  (sys:command-line-argument (1+ n)))
  #+ :clisp (nth n *args*))

(defun get-args ()
  #+ :sbcl (rest sb-ext:*posix-argv*)
  ;;
  ;;    These must come after -- on command line.
  ;;    See os-interface.htm
  ;;    
  #+ :allegro (rest (sys:command-line-arguments))
  #+ :clisp (copy-list *args*))

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
;; (defun getenv (name &optional default)
;;   #+CMU
;;   (let ((x (assoc name ext:*environment-list*
;;                   :test #'string=)))
;;     (if x (cdr x) default))
;;   #-CMU
;;   (or #+Allegro (sys:getenv name)
;;       #+CLISP (ext:getenv name)
;;       #+ECL (si:getenv name)
;;       #+SBCL (sb-unix::posix-getenv name)
;;       #+LISPWORKS (lispworks:environment-variable name)
;;       default))

(defun getenv (name)
  #+cmu
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #+sbcl (sb-posix:getenv name)
  #+clozure (ccl:getenv name)
  #+allegro (sys:getenv name)
  #+clisp (ext:getenv name)
  #+ecl (si:getenv name)
  #+lispworks (lispworks:environment-variable name)
  #-(or cmu sbcl clozure allegro clisp ecl lispworks)
  (error "GETENV not implemented"))

(defun setenv (name value)
  #+clozure (ccl:setenv name value)
  #+sbcl (sb-posix:putenv (concatenate 'string name "=" value)) ; See ~/lisp/sbcl/sbcl-1.0.43/contrib/sb-posix/interface.lisp SETENV missing?!
  #-(or sbcl clozure)
  (error "SETENV not implemented"))

;;;
;;;    Steve Gonedes
;;;
(defun default-directory ()
  #+cmu (default-directory)
                                        ;  #+:cmu (ext:default-directory)
  #+clozure (ccl::current-directory-name)
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
;;   #+sbcl (sb-ext:run-program "/bin/chmod"
;;                                (list (format nil "~O" permissions) file))
  #+sbcl(sb-posix:chmod file permissions)
  #+allegro (excl.osi:chmod file permissions)
  #+clisp (run-shell-command (format nil "chmod ~O ~A" permissions file))
  #+openmcl (ccl:run-program "/bin/chmod" (list (format nil "~O" permissions) file)))

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
;;;    From asdf.lisp
;;;    
;; (defun run-shell-command (control-string &rest args)
;;   "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
;; synchronously execute the result using a Bourne-compatible shell, with
;; output to *verbose-out*.  Returns the shell's exit code."
;;   (let ((command (apply #'format nil control-string args)))
;;     (format *verbose-out* "; $ ~A~%" command)
;;     #+sbcl (sb-impl::process-exit-code (sb-ext:run-program "/bin/sh"
;;                                                            (list  "-c" command)
;;                                                            :input nil
;;                                                            :output *verbose-out*))
;;     #+(or cmu scl) (ext:process-exit-code (ext:run-program "/bin/sh"
;;                                                            (list  "-c" command)
;;                                                            :input nil
;;                                                            :output *verbose-out*))

;;     #+allegro (excl:run-shell-command command :input nil :output *verbose-out*)
    
;;     #+lispworks (system:call-system-showing-output command
;;                                                    :shell-type "/bin/sh"
;;                                                    :output-stream *verbose-out*)
;;     ;XXX not exactly *verbose-out*, I know
;;     #+clisp (ext:run-shell-command  command :output :terminal :wait t)

;;     #+openmcl (nth-value 1
;;                          (ccl:external-process-status
;;                           (ccl:run-program "/bin/sh" (list "-c" command)
;;                                            :input nil
;;                                            :output *verbose-out*
;;                                            :wait t)))
;;     ;; courtesy of Juan Jose Garcia Ripoll
;;     #+ecl (si:system command)
;;     #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
;;     (error "RUN-SHELL-PROGRAM not implemented for this Lisp")))

(defun run-shell-command (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *verbose-out*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    #+sbcl (sb-impl::process-exit-code (sb-ext:run-program "/bin/sh"
                                                           (list  "-c" command)
                                                           :input t
                                                           :output t))
    #+(or cmu scl) (ext:process-exit-code (ext:run-program "/bin/sh"
                                                           (list  "-c" command)
                                                           :input nil
                                                           :output *verbose-out*))

    #+allegro (excl:run-shell-command command :input nil :output *verbose-out*)
    
    #+lispworks (system:call-system-showing-output command
                                                   :shell-type "/bin/sh"
                                                   :output-stream *verbose-out*)
    ;XXX not exactly *verbose-out*, I know
    #+clisp (ext:run-shell-command  command :output :terminal :wait t)

    #+openmcl (nth-value 1
                         (ccl:external-process-status
                          (ccl:run-program "/bin/sh" (list "-c" command)
                                           :input t
                                           :output t
                                           :wait t)))
    ;; courtesy of Juan Jose Garcia Ripoll
    #+ecl (si:system command)
    #-(or openmcl clisp lispworks allegro scl cmu sbcl ecl)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")))
