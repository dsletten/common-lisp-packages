;#!/usr/local/bin/clisp

;;
;   NAME:               cgi.lisp
;
;   STARTED:            010722
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;
(load "utils.lisp")

;;;;
;;;;    HTML tag functions
;;;;    
(defconstant background-color "#CCDDFF")
(defconstant text-color "#000000")
;;;
;;;    Main paired-tag function. First element of content list may be optional
;;;    list of attributes.
;;;    
(defun make-tag (tag content)
  (let ((attributes-list (if (listp (car content))
			     (pop content)
			     nil)))
    (format nil "<~A~A>~A</~A>" tag
	  (make-attributes attributes-list)
	  (join content)
	  tag)) )

;;;
;;;    XHTML-compliant for standalone tags.
;;;    
(defun make-solo-tag (tag content)
  (let ((attributes-list (if (listp (car content))
			     (pop content)
			     nil)))
    (format nil "<~A~A />" tag
	    (make-attributes attributes-list))) )

;;;
;;;    Create string of attributes/values for an HTML tag. The attributes, if
;;;    any, are expected to come in a list of keywords and values. A keyword
;;;    is expected to be paired with a value string or number following it. If a
;;;    keyword is followed by another keyword (or by nothing), the first is
;;;    assumed to be one of the few standalone attributes such as CHECKED,
;;;    SELECTED, or NOSHADE. In such a case, the value of the attribute is
;;;    simply the attribute itself to comply with the XHTML standard.
;;;
;;;    Any empty string following a keyword causes that keyword to be removed
;;;    from the final attributes string.
;;;
;;;    Example: '(:border 1 :nowrap :width "80%")
;;;          -> "border=\"1\" nowrap=\"nowrap\" width=\"80%\""
;;;          
(defun make-attributes (attributes-list &optional (attributes-string ""))
  (cond ((null attributes-list) attributes-string)
	((not (keywordp (car attributes-list))) 'missing-keyword)
	((or (keywordp (cadr attributes-list)) ;No value to go with keyword.
	     (null (cdr attributes-list)))
	 (make-attributes (cdr attributes-list)
			  (concatenate 'string
				       attributes-string
				       (format nil
					       " ~(~A~)=\"~:*~(~A~)\""
					       (car attributes-list)))) )
	(t
	 (make-attributes (cddr attributes-list)
			  (concatenate 'string
				       attributes-string
				       (if (equal (cadr attributes-list) "")
					   ""
					   (format nil
						   " ~(~A~)=\"~A\""
						   (car attributes-list)
						   (cadr attributes-list)))) ))) )


; (defun h1 (&optional (text ""))
;   (format nil "<h1>~A</h1>" text) )
(defun html (&rest content)
  (make-tag "html" content) )

(defun head (&rest content)
  (make-tag "head" content) )

(defun title (&rest content)
  (make-tag "title" content) )

(defun meta (&rest content)
  (make-solo-tag "meta" content) )

;;;
;;;    Get comments correct here.
;;;    Only add the comment strings if there is actual content for the tag.
;;;    If the only arg is a list of attributes, don't add comments.
;;;    
(defun style (&rest content)
  (apply #'style-script "style" content))

(defun script (&rest content)
  (apply #'style-script "script" content))

(let ((open-comment (format nil "<!--~%"))
      (close-comment "//-->"))
  (defun style-script (tag-name &rest content)
    (cond ((listp (car content))
	   (if (cdr content)
	       (let ((attributes (pop content)))
		 (push open-comment content)
		 (push attributes content))))
	  (content (push open-comment content)))
  (make-tag tag-name (if (cdr content)
			 (append content
				 (list close-comment))
			 content))))

(defun body (&rest content)
  (cond ((listp (car content))
	 (unless (member :bgcolor (car content))
	   (push background-color (car content))
	   (push :bgcolor (car content)))
	 (unless (member :text (car content))
	   (push text-color (car content))
	   (push :text (car content)))
	 (make-tag "body" content))
	(t (make-tag "body" (cons (list :bgcolor background-color
					:text text-color)
				  content)))) )

(defun div (&rest content)
  (make-tag "div" content) )

(defun span (&rest content)
  (make-tag "span" content) )

;;;
;;;    Table tags
;;;    
; (defun table (&rest args)
; ; 	      &key (border "" border-flag) (width "" width-flag)
; ; 	           (height "" height-flag) (cellpadding "" pad-flag)
; ; 	           (cellspacing "" space-flag) (align "" align-flag)
; ; 	           (bgcolor "" bgcolor-flag) &allow-other-keys)
;   (let ((border (or (second (member :border args)) ""))
; 	(width (or (second (member :width args)) ""))
; 	(height (or (second (member :height args)) ""))
; 	(cellpadding (or (second (member :cellpadding args)) ""))
; 	(cellspacing (or (second (member :cellspacing args)) ""))
; 	(align (or (second (member :align args)) ""))
; 	(bgcolor (or (second (member :bgcolor args)) "")))
		    

; ; 	(border (if border-flag
; ; 		    (format nil " border=\"~A\"" border)
; ; 		    border))
; ; 	(width (if width-flag
; ; 		   (format nil " width=\"~A\"" width)
; ; 		   width))
; ; 	(height (if height-flag
; ; 		    (format nil " height=\"~A\"" height)
; ; 		    height))
; ; 	(cellpadding (if pad-flag
; ; 			 (format nil " cellpadding=\"~A\"" cellpadding)
; ; 			 cellpadding))
; ; 	(cellspacing (if space-flag
; ; 			 (format nil " cellspacing=\"~A\"" cellspacing)
; ; 			 cellspacing))
; ; 	(align (if align-flag
; ; 		   (format nil " align=\"~A\"" align)
; ; 		   align))
; ; 	(bgcolor (if bgcolor-flag
; ; 		     (format nil " bgcolor=\"~A\"" bgcolor)
; ; 		     bgcolor)))
;     (format nil "<table~A~A~A~A~A~A~A>~A</table>" border width height
; 	    cellpadding cellspacing align bgcolor args))
;   (format t "~A~%" args))
; (defun table (&rest table-content)
;   (format nil "<table~A>~A</table>"
; 	  (join (make-attributes (remove-if-not #'listp table-content)))
; 	  (join (remove-if #'listp table-content))) )

; (defun tr (&rest row-content)
;   (format nil "<tr~A>~A</tr>" 
; 	  (join (make-attributes (remove-if-not #'listp row-content)))
; 	  (join (remove-if #'listp row-content))) )

; (defun td (&rest cell-content)
;   (format nil "<td~A>~A</td>" 
; 	  (join (make-attributes (remove-if-not #'listp cell-content)))
; 	  (join (remove-if #'listp cell-content))) )

; (defun strong (&optional (text ""))
;   (format nil "<strong>~A</strong>" text) )
(defun table (&rest content)
  (make-tag "table" content) )

(defun tr (&rest content)
  (make-tag "tr" content) )

(defun th (&rest content)
  (make-tag "th" content) )

(defun td (&rest content)
  (make-tag "td" content) )

(defun h1 (&rest content)
  (make-tag "h1" content) )

(defun h2 (&rest content)
  (make-tag "h2" content) )

(defun ul (&rest content)
  (make-tag "ul" content) )

(defun li (&rest content)
  (make-tag "li" content) )

(defun strong (&rest content)
  (make-tag "strong" content) )

(defun em (&rest content)
  (make-tag "em" content) )

(defun u (&rest content)
  (make-tag "u" content))

(defun sup (&rest content)
  (make-tag "sup" content) )

(defun sub (&rest content)
  (make-tag "sub" content) )

(defun p (&rest content)
  (make-tag "p" content) )

(defun a (&rest content)
  (make-tag "a" content) )

(defun font (&rest content)
  (make-tag "font" content) )


(defun img (&rest content)
  (make-solo-tag "img" content) )

(defun hr (&rest content)
  (make-solo-tag "hr" content) )

(defun br (&rest content)
  (make-solo-tag "br" content) )

;;;
;;;    !?????????????????
;;;    
(defun -- (&rest content)
  (make-solo-tag "!--" content) )

;;;
;;;    Form tags
;;;
(defun form (&rest content)
  (make-tag "form" content) )

(defun input (&rest content)
  (make-solo-tag "input" content) )

; (defun start-form (&key (method "" method-flag) (action "" action-flag)
; 		        (name "" name-flag))
;   (let ((action (if action-flag
; 		    (format nil " action=\"~A\"" action)
; 		    action))
; 	(name (if name-flag
; 		  (format nil " name=\"~A\"" name)
; 		  name))
; 	(method (if method-flag
; 		    (format nil " method=\"~A\"" method)
; 		    method)))
;     (format nil "<form~A~A~A>" name method action)) )

; (defun end-form ()
;   (format nil "</form>") )

; (defun textfield (&key (name "" name-flag))
;   (let ((name (if name-flag
; 		  (format nil " name=\"~A\"" name)
; 		  name)))
;     (format nil "<input type=\"text\"~A>" name)) )
  
; (defun submit (&key (name "" name-flag) (value "" value-flag))
;   (let ((name (if name-flag
; 		  (format nil " name=\"~A\"" name)
; 		  name))
; 	(value (if value-flag
; 		   (format nil " value=\"~A\"" value)
; 		   value)))
;     (format nil "<input type=\"submit\"~A~A>" name value)) )
; (defun textfield (&rest content)
;   (make-solo-tag "input" (cons '(type "text") content)) )

; (defun submit (&rest content)
;   (make-solo-tag "input" (cons '(type "submit") content)) )
  



;;;
;;;    Get form data from either POST or GET request.
;;;    Returns an association list, where the car of each sublist is
;;;    the name of some form element and the cdr contains the value(s)
;;;    for the element. (Checkboxes may have multiple values associated
;;;    with a single element.) All names/values are strings.
;;;
;;;    If no form data have been submitted, then FORM-DATA-RAW will either
;;;    be an empty string or NIL.
;;;    
; (defun get-form-data ()
;   (let ((form-data-raw (cond ((equal (system::getenv "REQUEST_METHOD") "GET")
; 			      (system::getenv "QUERY_STRING"))
; 			     ((equal (system::getenv "REQUEST_METHOD") "POST")
; 			      (read-post-data)))) )
;     (cond ((or (null form-data-raw)
; 	       (string= form-data-raw ""))
; 	   nil)
; 	  (t (let ((form-fields (split form-data-raw #\&)))
; 	       (let ((form-values (mapcar #'(lambda (s)
; 					      (split s #\=))
; 					  form-fields)))
; 		 (consolidate (uri-unencode form-values)))) ))) )

;;;
;;;    These probably only work in CLISP.
;;;    
(defun get-request-method ()
  (getenv "REQUEST_METHOD"))

(defun get-query-string ()
  (getenv "QUERY_STRING"))

(defun get-form-data ()
  (let* ((request-method (get-request-method))
;  (let* ((request-method (find-request-method))
	 (form-data-raw (cond ((equal request-method "GET")
			       (get-query-string))
			      ((equal request-method "POST")
			       (read-post-data))
			      (t nil)))
	 (form-data (make-hash-table :test #'equal)))
    (cond ((or (null form-data-raw)
	       (string= form-data-raw ""))
	   nil)
	  (t (let ((key-value-a-list
		    (mapcar #'(lambda (s)
				(split s #\=))
			    (split form-data-raw #\&))))
	       (dolist (pair key-value-a-list form-data)
		 (let ((key (uri-unencode (car pair)))
		       (value (uri-unencode (cadr pair))))
		   (multiple-value-bind (form-value flag)
		       (gethash key form-data)
		     (if flag
			 (if (listp form-value)
			     (push value (gethash key form-data))
			     (setf (gethash key form-data)
				 (list value form-value)))
			 (setf (gethash key form-data) value)))) )))) ))

(defun get-content-length ()
  (let ((content-length (or (getenv "CONTENT_LENGTH") "0")))
    (parse-integer content-length)))

;;;
;;;    This is pretty retarded...
;;;    Isn't there some way to just read N bytes from a stream?!
;;;    
; (defun read-post-data (&optional (s nil) (n (read-from-string (system::getenv "CONTENT_LENGTH"))))
;   (cond ((zerop n) (coerce (reverse s) 'string))
; 	(t (read-post-data (cons (read-char) s) (- n 1)))) )

(defun read-post-data ()
  (let* ((data (make-string (get-content-length)))
	 (content-read (read-sequence data *standard-input*)))
    (if (= content-read (get-content-length))
	data
	nil)))

(let ((safe '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))))
  (defun encode-string (s)
    (cond ((string= s "") "")
	  (t (let ((ch (character (subseq s 0 1))))
	       (cond ((or (char-not-greaterp #\A ch #\Z)
			  (char<= #\0 ch #\9)
			  (member ch safe))
		      (concatenate 'string (list ch) (encode-string (subseq s 1))))
		     (t (concatenate 'string (format nil "%~2X" (char-code ch))
				     (encode-string (subseq s 1)))) )))) ))
#|


    $toencode=~s/([^a-zA-Z0-9_.-])/uc sprintf("%%%02x",ord($1))/eg;

Netscape 4.7/6 encode all of $mark chars but '-', '_', '.', and '*'
$mark       = q(-_.!~*'());                                    #'; emacs
$unreserved = "A-Za-z0-9\Q$mark\E";
$uric       = quotemeta($reserved) . $unreserved . "%";
|#

;;;
;;;    Unencode an a-list of pairs of form elements and their associated
;;;    values.
;;;    
(defun uri-unencode (a-list)
  (labels ((unencode-aux (l)
	     (list (unencode-string (car l))
		   (unencode-string (cadr l))))
	   (unencode-string (s)
	     (unhexify (substitute #\Space #\+ s))))
    (let ((result ()))
      (dolist (pair a-list (nreverse result))
	(push (unencode-aux pair) result)))) )

;;;
;;;    Convert each sequence of '%XX' in a string into its corresponding
;;;    character. The 'XX' is assumed to represent a hexadecimal integer.
;;;    If it isn't actually a hex number, the sequence is converted to a
;;;    space.
;;;    
(defun unhexify (s)
  (let ((pos (position #\% s)))
    (cond ((null pos) s)
	  (t (concatenate 'string
			  (subseq s 0 pos)
			  (string (code-char
				   (or (parse-integer s
						      :radix 16
						      :start (+ pos 1)
						      :end (+ pos 3)
						      :junk-allowed t)
				       (char-code #\Space))))
			  (unhexify (subseq s (+ pos 3)))) ))) )

(defun uri-unencode (s)
  (let ((pos (position #\% s)))
    (cond ((null pos) s)
	  (t (concatenate 'string
			  (subseq s 0 pos)
			  (string (code-char
				   (or (parse-integer s
						      :radix 16
						      :start (+ pos 1)
						      :end (+ pos 3)
						      :junk-allowed t)
				       (char-code #\Space))))
			  (uri-unencode (subseq s (+ pos 3)))) ))) )

;;;
;;;    Consolidate an association list (of strings) so that all values
;;;    associated with a given key are placed in the same sublist.
;;;
;;;    E.g., ((a 1) (b 2) (a 3) (c 4) (b 5) (a 6)) -> ((a 1 3 6) (b 2 5) (c 4))
;;;    
(defun consolidate (a-list)
  (cond ((null a-list) nil)
	((member (caar a-list) (cdr a-list) :key #'car :test #'equal)
	 (cons (remove nil
		       (append (car a-list)
			       (mapcar #'(lambda (l) ;(remove nil) is safe since we
					;assume elements are strings?
					   ;; If a sublist contains the
					   ;; current key as its key return
					   ;; the second element. Otherwise
					   ;; return nil.
					   (if (equal (caar a-list) (car l))
					       (cadr l)
					       nil))
				       (cdr a-list))))
	       (consolidate (remove (caar a-list) (cdr a-list) :key #'car :test #'equal))))
	(t (cons (car a-list) (consolidate (cdr a-list)))) ) )


;;;
;;;    Get the value of a given parameter or return names of all parameters.
;;;    Assumes FORM-DATA is an a-list where keys/vals are strings. Thus, if
;;;    no val is found for a given key return empty string rather than empty
;;;    list: "" vs. nil.
;;;
(defun param (form-data &optional field-name)
  (cond (field-name (or (second (assoc field-name form-data :test #'equal))
			""))
	(t (mapcar #'first form-data))) )

(defun display-form-data (form-data-a-list)
  (table '(border 0) '(cellspacing 0)
	 (tr
	  (th '(colspan 3) '(bgcolor "#EEEEEE") "Submitted Form Data"))
	 (tr
	  (td '(align "center") '(bgcolor "red") "Form Field")
	  (td '(bgcolor "red") "&nbsp;")
	  (td '(align "center") '(bgcolor "yellow") "Value"))
	 (form-data-rows form-data-a-list)))

(defun form-data-rows (form-data-a-list)
  (join (mapcar #'(lambda (x)
		    (tr
		     (td '(align "right") '(bgcolor "red")
			 (font '(color "blue") (car x)))
		     (td '(bgcolor "red") "=")
		     (td '(align "left") '(bgcolor "yellow")
			 (strong "[ " (join (cdr x) ", ") " ]")) ))
		form-data-a-list)) )

(defun output-html (html-tags)
  (dolist (tag html-tags)
    (format t "~A~%" (eval tag))) )


;;;
;;;    Generic HTML header.
;;;
(defun start-html (&optional (mime-type "text/html"))
  (format nil "Content-type: ~A~%~%" mime-type))

(defun header (&key (title "" title-flag) (script "" script-flag)
	            (onload "" onload-flag) (bgcolor "#FFFFFF" bgcolor-flag)
	            (text "#000000" text-flag))
  (let ((title (if title-flag
		   (concatenate 'string
				": "
				title)
		   title))
	(script (if script-flag
		    (format nil "
    <script language=\"JavaScript\">
        <!--
        ~A
        //-->
    </script>" script)
		    script))
	(onload (if onload-flag
		    (concatenate 'string
				 " onload=\""
				 onload
				 "\"")
		    onload)))
    (format nil "
<html>

<head>
    <title>Dave's CGI Factory~A</title>
    ~A
</head>

<body bgcolor=\"~A\" text=\"~A\"~A>
     <div align=\"center\">" title script bgcolor text onload)) )

;;;
;;;    Generic HTML footer.
;;;    
(defun end-html ()
  (format nil "
    </div>
</body>

</html>~%") )
