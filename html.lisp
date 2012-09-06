;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               html.lisp
;;;;
;;;;   Started:            Wed Oct 19 10:39:05 2005
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
(load "utils.lisp")

(defpackage html (:use common-lisp))

(in-package html)


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
  (let ((attributes-list (if (listp (first content))
			     (pop content)
			     nil)))
    (format nil "<~A~A>~A</~A>" tag
	  (make-attributes attributes-list)
	  (utils:join content)
	  tag)) )

;;;
;;;    XHTML-compliant for standalone tags.
;;;    
(defun make-solo-tag (tag content)
  (let ((attributes-list (if (listp (first content))
			     (pop content)
			     nil)))
    (format nil "<~A~A />" tag
	    (make-attributes attributes-list))) )

;;;
;;;    Create string of attributes/values for an HTML tag. The attributes, if
;;;    any, are expected to come in a list of keywords and values. A keyword
;;;    is expected to be paired with a value string or number following it. If
;;;    a keyword is followed by another keyword (or by nothing), the first is
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
	((not (keywordp (first attributes-list))) 'missing-keyword)
	((or (keywordp (second attributes-list)) ;No value to go with keyword.
	     (null (rest attributes-list)))
	 (make-attributes (rest attributes-list)
			  (concatenate 'string
				       attributes-string
				       (format nil
					       " ~(~A~)=\"~:*~(~A~)\""
					       (first attributes-list)))) )
	(t
	 (make-attributes (cddr attributes-list)
			  (concatenate 'string
				       attributes-string
				       (if (equal (second attributes-list) "")
					   ""
					   (format nil
						   " ~(~A~)=\"~A\""
						   (first attributes-list)
						   (second attributes-list)))) ))) )


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
    (cond ((listp (first content))
	   (if (rest content)
	       (let ((attributes (pop content)))
		 (push open-comment content)
		 (push attributes content))))
	  (content (push open-comment content)))
  (make-tag tag-name (if (rest content)
			 (append content
				 (list close-comment))
			 content))))

(defun body (&rest content)
  (cond ((listp (first content))
	 (unless (member :bgcolor (first content))
	   (push background-color (first content))
	   (push :bgcolor (first content)))
	 (unless (member :text (first content))
	   (push text-color (first content))
	   (push :text (first content)))
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
  
