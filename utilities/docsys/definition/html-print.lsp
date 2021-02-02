
#|
 $Id: html-print.lsp,v 1.1 1998/07/30 23:08:23 ael Exp ael $
 $Log: html-print.lsp,v $
 Revision 1.1  1998/07/30 23:08:23  ael
 Initial revision


X-POP3-Rcpt: cca@bow.intnet.bj
Return-Path: <ralmond@ets.org>
Date: Mon, 27 Jul 1998 16:46:02 -0400
From: Russell Almond <ralmond@ets.org>
Subject: Progress on Definitions pacakge
To: tdye@lava.net
Cc: cca@bow.intnet.bj
Reply-to: ralmond@ets.org

Okay, I have pass 1 at the defintions stuff now in place.  I'll send
it to you uuencoded gziped tar file in the next message.  If you want
to check out what it does, try the following script.

(load "xlos/defclass.lsp")   ;; Fake CLOS for defsytem stuff
(load "xlos/defgeneric.lsp")
(load "definition/system.lsp") ;; The main load file
(load "definition/script.lsp") ;; A sample script which runs the
			       ;; system  on definitions.

Tom, what you need to do is look at this and see how it matches up
with the high level stuff you have done.

Andy, you volunteered to look at the issue of writing out HTML.
Probably what you need to do is look at the tex-print code and see how
you can make a version for html-print.  It should be pretty
straightforward (except for all the stupid "<" and ">" quoting).  I
don't know whether we want to make tex or html target a switch in the
print functions or a separate series of functions, I'll leave that up
to you.

	--Russell

- ael - Wed Aug 12 18:56:50 1998

	I decided to make a separate series of functions. They follow. The file
html-script creates an html version of the user-manual and the reference-manual.
In the directory containing the html-script.lsp file, type

(load "../xlos/defclass.lsp") ;; Fake CLOS for defsytem stuff
(load "../xlos/defgeneric.lsp")
(load "system.lsp") ;; The main load file
(load "html-print.lsp") ;; this overwrites certain definitions so that html is
			;; produced, rather than tex.
(load "html-script.lsp") ;; this tests the result
|#
;;; ael html modifications to RGA modifications to JAM's definition objects
(in-package :Definitions)

;;;-------------------------------------------------------

(defun fix-html-string (s0)
  "Massage a string so that html special characters will come out
as something reasonable."
  (declare (type String s0)
	   (:returns (type String)))
  (with-output-to-string
   (out)
   (let ((chars         '(#\> #\< #\&))
	 (replacements  '("&gt;" "&lt;" "&amp;")))
    (dotimes (i (length s0))
      (let ((c (char s0 i)))
	(case
	 c
	 ((#\> #\< #\&)
	  ;; replace by their string replacements:
	  (format out (elt replacements (search (list c) chars)))
	  )
	 (otherwise
	  (write-char c out))))))))

;;;=======================================================

(defgeneric print-html-definition-headline (def stream)
  (:documentation   "Print-Html a Headline for the definition.")
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  )

(defmethod print-html-definition-headline ((def Definition) 
				      &key
				      (stream *standard-output*))

  "Print-Html a Headline for the definition."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((name-string (fix-html-string (definition-name->string
					    def)))
	    (tag-string (definition-name->tag def)))
	
	
    ;; name box
	(format stream "~%<p><h2><a name=\"~A\">~A </a>(<i>~A</i>)</h2>~%"
		tag-string
		name-string 
		(definition-class-nice-name def))
	def)))

;;;=======================================================
(defgeneric print-html-definition-usage (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print-Html a description of how to ``call'' the definition.")
  )

(defmethod print-html-definition-usage ((def Definition) 
				   &key
				   (stream *standard-output*))

  "Print a description of how to ``call'' the definition."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (multiple-value-bind (usage-string usage-indent)
	  (definition-usage def)
	(format stream "<p><b>Usage: </b><p><center>~A</center><p>~%"
	    usage-string))
    def))

;;;=======================================================

(defgeneric print-html-definition-arg-types (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print-Html a list of args and their expected types.")
  )


(defmethod print-html-definition-arg-types ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of args and their expected types."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (with-readable-printing
      (let ((arg-types (definition-arg-types def)))
	(when arg-types
	  (format stream "<p><b>Arguments:</b><p>~%<ul>~%")
	  (dolist (arg-type arg-types)
	    (format stream
		    "<li><i>~A</i> -- ~:(~S~)</li>~%" 
		    (first arg-type) (second arg-type)))
	  (format stream "</ul>~%"))))
  def)

;;;=======================================================

(defgeneric print-html-definition-slot-types (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
	(:documentation 
	"Print a list of visible slot and their expected types and 
	documentation.")
  )

(defmethod print-html-definition-slot-types ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of visible slots and their expected types and documentation."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  def)

(defmethod print-html-definition-slot-types ((def Class-Definition)
					     &key (stream *standard-output*))
  (with-readable-printing
   (let ((slot-specs (definition-slots def)))
     (when
      slot-specs
      (format stream "<p><b>Slots:</b><p>~%<ul>~%")
      (dolist
       (slot-spec slot-specs)
       (let ((name (car slot-spec))
	     (initargs (get-slot-initarg-list slot-spec))
	     (slot-type (getf (cdr slot-spec) :type T))
	     (slot-doc (getf (cdr slot-spec) :documentation)))
	 (when
	  initargs
	  (format
	   stream
	   "<li><i>~A</i> --
            <Code>~:(~S~)</Code> --
            ~:(~S~)<br>"
	   name initargs slot-type)
	  (if slot-doc
	      (format stream "~A</li>~%"
		      slot-doc)
	    (format stream "</li>~%")))))
      (format stream "</ul>~%"))))
  def)



(defmethod print-html-definition-slot-types ((def Prototype-Definition)
					     &key (stream *standard-output*))
  (let ((definee (definition-definee def)))
    (with-readable-printing
     (let ((slots (send definee :own-slots)))
       (when
	slots
	(format stream "<p><b>Slots:</b><p>~%<ul>~%")
	(dolist
	 (slot slots)
	 (let ((slot-type (send definee :slot-type slot))
	       (slot-doc (send definee :documentation slot)))
	   (format
	    stream
	    "<li> ~(~S~) -- ~:(~S~)<br>~%"
	    slot slot-type)
	   (if slot-doc
	       (format stream "~A</li>~%"
		       slot-doc)
	     (format stream "</li>~%"))))
	(format stream "</ul>~%")))))
  def)





;;;=======================================================

(defun html-format-return-spec (return-spec stream)
  (cond ((atom return-spec)
	 (format stream "<i>~A</i>~%" return-spec))
	((eq (first return-spec) 'type)
	 (ecase (length return-spec)
	   (2 (format stream "Object of Type: ~:(~A~)~%"
		      (second return-spec)))
	   (3 (format stream "<i>~a</i> -- ~:(~S~)~%"
		      (third return-spec)
		      (second return-spec)))))
	(t (error "Invalid :returns spec"))))

(defgeneric print-html-definition-returns (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print-Html a list of returned values and/or their
      (let ((children (types.")
  )

(defmethod print-html-definition-returns ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print-Html a list of returned values and/or their types."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((returns (definition-returns def)))
	(when returns
	  (format stream "<p><b>Returns:</b> ~%")
	  (cond ((and (listp (second returns))
		      (eq (first (second returns)) 'values))
		 (format stream "<ul>")
		 (dolist (return-spec (rest (second returns)))
		   (format stream "<li>")
		   (html-format-return-spec return-spec stream)
		   (format stream "</li>")
		   )
		 (format stream "</ul>")
		 )
		(t ;; else a single return value
		 (html-format-return-spec (second returns) stream)))
	  (format stream "<p>~%"))))
  def)


;;;=======================================================


(defgeneric print-html-definition-parents (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print a list of parent definitions.")
  )


(defmethod print-html-definition-parents ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print-Html a list of parent definitions."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((parents (mapcar #'princ-with-nicknames (definition-parents def))))

	(when parents
	  (format stream "~%<b>Parents:</b> ~A~%"
		  (fix-html-string
		   (format nil "~:(~A~)~{, ~:(~A~)~}"
			   ;; Add commas between
			   (car parents) (cdr parents)))))))
  def)


;;;=======================================================


(defgeneric print-html-definition-children (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print-Html a list of parent definitions.")
  )


(defmethod print-html-definition-children ((def Definition) 
				       &key
				       (stream *standard-output*))

  "Print a list of parent definitions."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((children (mapcar #'princ-with-nicknames (definition-children def))))
	
	(when children
	  (format stream "<p><b>Children:</b> ~A<p>~%"
		  (fix-html-string
		   (format nil "~:(~A~)~{, ~:(~A~)~}"
			   ;; Add commas between
			   (car children) (cdr children)))))))
  def)

;;;=======================================================

(defgeneric print-html-definition-documentation (def &key stream)
 (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
 (:documentation "Print-Html the documentation string for the definition.")
 )


(defmethod print-html-definition-documentation ((def Definition) 
					   &key
					   (stream *standard-output*))

  "Print-Html the documentation string for the definition."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((documentation (fix-html-string (definition-documentation def))))
	(when (and documentation (not (zerop (length documentation))))
	      (format stream "<p><b>Documentation:</b><p>~%")
	      (format stream "~A" documentation)
	      (format stream "<p>~%"))))
  def)

;;;=======================================================

(defgeneric print-html-definition-source-path (def &key stream)
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (:documentation "Print the pathname from which this definition was
      (let ((children (read.")
  )


(defmethod print-html-definition-source-path ((def Definition) 
					 &key
					 (stream *standard-output*))

  "Print-Html the pathname from which this definition was read."

  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((path (definition-path def)))
	(when path
	  (format stream "~%<p><i>Source file: </i>~%~A<p>~%"
		  (fix-html-string (format nil "~a" path))))))
  def)


;;;=======================================================

(defgeneric print-html-definition-methods (def &key stream all)
  (:documentation "Print-Htmls out a list of methods associated with this definition.")
  )

(defmethod print-html-definition-methods ((def Definition) &key stream all)
  "Prints out a list of methods associated with this definition."

  (declare (type Definition def)
	   (type (or T Null) all)
	   (type Stream stream)
	   (:returns def))

  (with-readable-printing
      (let ((methods (mapcar #'princ-with-nicknames (definition-methods def :all all))))
	
	(when methods
	  (format stream "<p><b>Methods:</b> ~A<p>~%"
		  (fix-html-string
		   (format nil "~:(~A~)~{, ~:(~A~)~}"
			   (car methods) (cdr methods)))))))
  def)


(defgeneric print-html-definition-signals (def &key stream)
  (:documentation "Print-Htmls out a list of announcements associated with this definition.")
  )
(defmethod print-html-definition-signals ((def Definition) &key stream)
  "Print-Htmls out a list of announcements associated with this definition."
  def)

;;;----------------------------------------------------------------------
(defgeneric print-html-definition (def &key stream)
  (declare (type Definition def)
	   (type Stream stream))
  (:documentation
   "Print-Html a definition object in html."))
(defmethod print-html-definition ((def Definition) 
			     &key
			     (stream *standard-output*))
  "Print-Htmls out the reference manual entry for a definition object in html."
  (declare (type Definition def)
	   (type Stream stream)
	   (:returns def))
  (with-readable-printing
      (let (
	    ;;(name-string (fix-html-string (definition-name->string def)))
	    )
	(format stream "<hr>~%")
	(print-html-definition-headline def :stream stream)
	(print-html-definition-documentation def :stream stream)
	(print-html-definition-usage def :stream stream)
	(print-html-definition-arg-types def :stream stream)
	(print-html-definition-slot-types def :stream stream)
	(print-html-definition-returns def :stream stream)
	(print-html-definition-parents def :stream stream)
	(print-html-definition-children def :stream stream)
	(print-html-definition-methods def :stream stream)
	(print-html-definition-signals def :stream stream)
	(print-html-definition-source-path def :stream stream)
	def)))
;;;=======================================================
(defun print-html-definitions (defs path 
				&key (title "Xlispstat Documentation file"))
  "Print-Html a html representation of the definition objects in <defs>
on the file corresponding to <path>. 
  Title is used as the title of the page."
  (declare (type List defs)
	   (type String title)
	   (type (or String Pathname) path)
	   (:returns defs))
  (with-open-file (s path
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :supersede)
    (format s "<html>
<head>
<title> ~A </title>
</head>
<body>
<center><h1> ~A </h1></center>
" title title)
    (dolist (def defs) (print-html-definition def :stream s))
    (format s "
</body>
</html>
")
    )
  (values defs))



