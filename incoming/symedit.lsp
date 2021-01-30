
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; file symedit.lsp, frederic udina, 11 dec 98
;;;; was edit-globals, dec 5 1996
;;;;
;;;; calling (edit-symbols) a dialog window will appear 
;;;; making it easy to see a list of available symbols,
;;;; its values and allowing also to edit them.
;;;;
;;;; added edit-symbols, may 1998 
;;;; public release dec 98
;;;; added look-also-doc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(format t "current package: ~a" *package*)

(unless (find-package "EDIT-SYMS")
	(make-package "EDIT-SYMS" :use '(xlisp)))
(use-package "EDIT-SYMS")

(in-package "EDIT-SYMS")

(export '(edit-symbols edit-object))

;;;some xls implementations have this limit
(defvar *maxnum-edit-symbols* 1500 "max number of symbols to put in
the item-list")

(defvar *max-lines-info* 
  #+UNIX 60
  #-UNIX 40 "the max number of lines to display in info dlg")

(defvar *version* "v.22/12/98")

;;;write-to-string could be used instead!!
(defun break-string-lines (num str-text &key max-lines)
 "args: num str. Changes spaces to newlines in a copy of str to fit the text in
num width. Fails if a single word is larger than num"
(let ((str (copy-seq str-text))
      (last-break 0)
      (last-space 0)
      (pos 0)
      (num-lines 1))
  (loop
   (when (and max-lines (> num-lines max-lines))
     (return (concatenate 'string (subseq str 0 pos)  (format nil "@@/@@"))))
   (when (>= pos (length str))
     (return str))
   (if (char= (select str pos) #\Space)
       (setf last-space pos))
   (when  (char= (Select str pos) #\newline)
     (setf last-break pos)
     (when max-lines (incf num-lines)))
   (when (>= (- pos last-break) num)
	 (setf (select str last-space) #\newline)
	 (when max-lines (incf num-lines))
	 (setf last-break last-space))
   (incf pos))))
	   

(defun edit-symbols ()
"Open a dialog window to list and inspect the currently declared symbols"
(let* (current currsymbol
       doc 
       symbol-list 
       tmp tmptext
       (pack-list (list-all-packages))
       (choice-list '("global (*) vars" "Objects" "All variables" "Functions" "All symbols"))
       (radio-symbol-choice
	(send choice-item-proto :new choice-list :value 0))
       (radio-packages
	(send choice-item-proto :new 
	      (cons "All packages" 
		    (mapcar #'package-name (list-all-packages)))
	      :value (1+ (position "USER" 
				   (mapcar #'package-name 
					   pack-list)
				   :test #'string-equal))))
       (itlist (send list-item-proto :new 
		     ;;(mapcar #'symbol-name 
		     ;;(delete-if-not #'boundp symbol-list))
		     (make-array *maxnum-edit-symbols* :initial-element "")
		     :columns 1))
       (last-list-length 0);;*maxnum-edit-symbols*
       (btn1 (send button-item-proto :new "Show"))
       (btn2 (send button-item-proto :new "Info"))
       (btn3 (send button-item-proto :new "View/Edit"))
       (helpbtn (send button-item-proto :new "Help"))
       (apropbtn (send button-item-proto :new "Apropos"))
       (apropstring "")
       (taprop (send text-item-proto :new "           "))
       (tfake (send text-item-proto :new "           "))
       (tnam  (send text-item-proto :new "                               "))
       (tval  (send text-item-proto :new "                               "))
       (tfval (send text-item-proto :new "                               "))
       (tpack (send text-item-proto :new "                                    "))
       (numsym (send text-item-proto :new "# of symbols: 1500  "))
       (only-in-toggle (send toggle-item-proto :new "Only created in pkg" :value nil))
       (also-in-doc (send toggle-item-proto :new "Look also in doc" :value nil))
       (dlg (send dialog-proto :new 
		  (list 
		   (list
		    (list (send text-item-proto :new "What to see")
			  (list radio-symbol-choice)
			  (send text-item-proto :new "What package")
			  (list radio-packages)
			  (list only-in-toggle))
		    (list (list itlist (list
					(list btn2)
					(list btn3)
					(list helpbtn)
					(list tfake)
					(list apropbtn)	
					(list taprop)
					(list also-in-doc)))
			  (list numsym)
			  (list (send text-item-proto :new "  Symbol:")tnam)
			  (list (send text-item-proto :new "   Value:") tval)
			  (list (send text-item-proto :new "Function:") tfval)
			  (list tpack))
		    ))
		  :title (format nil "View and edit symbols (~a)" *version*)
		  :modal nil))
       (get-documentation;;get the apropriate doc for symbol
	;;first looking according to buttons, then the one available
	#'(lambda (sym)
	    (setq tmptext
		  (documentation sym
				 (if (= 3 (send radio-symbol-choice :value))
				     'function 'variable)))
	    (unless tmptext
	      (setq tmptext
		    (documentation sym 'variable)))
	    (unless tmptext
	      (setq tmptext
		    (documentation sym 'function)))
	    tmptext))
       (get-aprop-string
	#'(lambda ()
	    (setq tmp (get-string-dialog "Enter a string to be matched:"
					 :initial apropstring))
	    (when tmp
	      (setq apropstring (string-upcase tmp))
	      (send taprop :text apropstring)
	      (funcall update-symbol-list))))
       (symbol-match-apropos 
	#'(lambda (sym)
	    "return t if symbol contains the string aprop in the name"
	    (let* ((la (length apropstring)) 
		   (i 0)
		   (symn (symbol-name sym))
		   (ls (length symn))
		   (j (+ i la)))
	      (loop
	       (when (> j ls)
		 (return nil))
	       (when (string= apropstring symn :start2 i :end2 j)
		 (return t))
	       (incf i) (incf j)))))
       (symbol-or-doc-match-apropos 
	#'(lambda (sym)
	    (if (funcall symbol-match-apropos sym) t
	      (let* ((la (length apropstring)) 
		     (i 0)
		     (symn (string-upcase (funcall get-documentation sym)))
		     (ls (length symn))
		     (j (+ i la)))
		(loop
		 (when (> j ls)
		   (return nil))
		 (when (string= apropstring symn :start2 i :end2 j)
		   (return t))
		 (incf i) (incf j))))))
       (eval-to-objectp
	#'(lambda (sym)
	    (objectp (eval sym))))
       (is-in-pckg
	#'(lambda (sym)
	    (eq (symbol-package sym)
		(select pack-list (1- (send radio-packages :value))))))
       small-dlg
       (update-symbol-list		; looking at radio buttons
	#'(lambda ()
	    "this is the main function. Looking at the state
               of controls, it builds the list and calls install-symbols-in-list"
;;;	      (setq small-dlg
;;;		    (send dialog-proto :new (list (send text-item-proto :new "Wait...")) 
;;;			  :modeless t :go-away nil :location (send dlg :location)
;;;			  :show t))
	    (setf symbol-list nil)
	    (cond ((= 0 (send radio-packages :value));;all-packs
		   (do-all-symbols (sym)
				   (setf symbol-list (cons sym symbol-list))))
		  (t (do-symbols (sym (select pack-list (1- (send radio-packages :value))))
				 (setf symbol-list (cons sym symbol-list)))))
	    (case (send radio-symbol-choice :value)
		  (4 nil)		;all symbols
		  (3			;functions
		   (setf symbol-list (delete-if-not #'fboundp symbol-list)))
		  (2			;all vars
		   (setf symbol-list (delete-if-not #'boundp symbol-list)))
		  (1			;objects
		   (setf symbol-list (delete-if-not #'boundp symbol-list))
		   (setf symbol-list (delete-if-not eval-to-objectp symbol-list)))
		  (0			; global vars
		   (setf symbol-list (delete-if-not #'boundp symbol-list))
		   (setf symbol-list (delete-if-not #'(lambda (x)
							(position #\*
								  (symbol-name x) 
								  :test #'char=))
						    symbol-list))))
	    (when (and (send only-in-toggle :value)
		       (< 0 (send radio-packages :value)))
	      (setf symbol-list (delete-if-not is-in-pckg symbol-list)))
	    (unless (string= apropstring "")
	      (if (send also-in-doc :value)
		  (setq symbol-list (delete-if-not symbol-or-doc-match-apropos symbol-list))
		(setq symbol-list (delete-if-not symbol-match-apropos symbol-list))))
	    ;;(format t "~a ~a:~a~%" (send radio-symbol-choice :value)
	    ;;      (send radio-packages :value)
	    ;;    (size symbol-list))
	    (if (= 0 (length symbol-list))
		(setf symbol-list (make-array 1 :initial-contents '(nothing)))
	      (setf symbol-list (make-array (length symbol-list)
					    :initial-contents symbol-list)))
	    (setf symbol-list (sort symbol-list #'string<))
	    (funcall install-symbols-in-list)
	    (setq last-list-length (length symbol-list))
	    (send numsym :text (format nil "# of symbols: ~a"
				       (length symbol-list)))
	    (when (objectp small-dlg)
	      (send small-dlg :dispose))
	    )
	)
       (install-symbols-in-list
	#'(lambda ()
	    (when (> (length symbol-list) *maxnum-edit-symbols*)
	      (progn 
		(message-dialog 
		 (format nil "Number of symbols ~a exceeds ~a, list will be choped"
			 (length symbol-list) *maxnum-edit-symbols*))
		(setf symbol-list (select symbol-list (iseq *maxnum-edit-symbols*)))))
	    (dotimes (i (min *maxnum-edit-symbols*
			     (max last-list-length
				  (length symbol-list))))
		     (if (< i (length symbol-list))
			 (send itlist :set-text i (symbol-name (aref symbol-list i)))
		       (send itlist :set-text i "")))
	    (when (> (send itlist :selection) (length symbol-list)
		     (send itlist :selection 0)))
	    (if (send itlist :selection);;doesn't work?
		(send itlist :do-action))))
       )
  ;;body begin here
  (send only-in-toggle :slot-value 'action update-symbol-list)
  (send also-in-doc :slot-value 'action
	#'(lambda ()
	    (unless (string= apropstring "")
	      (funcall update-symbol-list))))
  (defmeth itlist :selection (&optional (index nil iset))
    (if iset
	(call-next-method index)
      (if (call-next-method)
	  (max 0 (min (1- (length symbol-list)) (call-next-method))))))
  (send itlist :slot-value 'action;; for the list
	#'(lambda (&optional (double nil))
	    (setq current (send itlist :selection))
	    (setq currsymbol (select symbol-list current))
	    (setq tmptext "")
;;	    (if (and (boundp currsymbol)
;;		     (> (flatsize (eval (select symbol-list current))) 20000))
;;		(setq tmptext "--string too large--")
;;	      (setq tmptext
;;		    (format nil "~s"
;;			    (if (boundp (select symbol-list current))
;;				(format nil "Value: ~s"
;;					(eval (select symbol-list current)))
;;			      (if (fboundp (select symbol-list current))
;;				  (symbol-function (select symbol-list current))
;;				"<not-bound-fbound>")))))
	    (when (boundp currsymbol)
	      (if (> (flatsize (eval currsymbol)) 20000)
		 (setq tmptext "--string too large--")
		(setq tmptext 
		      (format nil "~s" (eval currsymbol)))))
	    (when (> (length tmptext) 50)
	      (setq tmptext (subseq tmptext 0 50)))
	    (send tval :text tmptext)
	    (if (fboundp currsymbol)
		(setq tmptext (format nil "~s" (symbol-function currsymbol)))
	      (setq tmptext ""))
	    (send tfval :text tmptext)
	    (send tnam :text (symbol-name currsymbol))
	    (send tpack :text (format nil "~s" 
				      (symbol-package (select symbol-list current))))))
  (send itlist :selection 0)
  (send radio-packages :action update-symbol-list)
  (send radio-symbol-choice :action update-symbol-list)
  (send apropbtn :action get-aprop-string)
  (send btn2 :action;;get-info
	#'(lambda ()
	    (setq current (send itlist :selection))
	    ;;(format t "~a ~a ~%"  current (select  *varlist* current))
	    (setq doc (funcall get-documentation (select symbol-list current)))
	    (setq tmptext (format nil "~a"
				  (if (boundp (select symbol-list current))
				      (eval (select symbol-list current))
				    (if (fboundp (select symbol-list current))
					(symbol-function (select symbol-list current))
				      "<not-bound>"))))
	    (when (> (length tmptext) 1500)
	      (setq tmptext (subseq tmptext 0 1500)))
	    (message-dialog (break-string-lines
			     80 
			     (format nil "~a: ~s~%~a"
				     (symbol-name (select symbol-list current))
				     tmptext
				     (if doc doc "No help available"))
			     :max-lines *max-lines-info*))))
  (send btn3 :action;;edit values
	#'(lambda ()
	    (setq current (send itlist :selection))
	    (setq currsymbol (select symbol-list current))
	    (cond ((boundp currsymbol)
		   (if (objectp (eval currsymbol)) ;objects
		       (edit-object currsymbol)
		     (progn
		       (if (> (flatsize (eval currsymbol)) 5000)
			   (setq tmptext (make-string 201 :initial-element #\x))
			 (setq tmptext (format nil "~s"
					       (eval currsymbol))))
		       (if (> (length tmptext) 200)
			   (message-dialog "Value too long to edit")
			 (when (setq newval 
				     (get-string-dialog 
				      "Edit a new value (at your own risk!)"
				      :initial tmptext))
			   (set currsymbol 
				(car (eval-string newval)))
			   (send itlist :do-action))))))
		  ((and (fboundp currsymbol)
			(function-lambda-expression (symbol-function currsymbol)))
		   (when (ok-or-cancel-dialog
			  "Edit not allowed. ~%Want to pprint it to the listener?")
		     (pprint (function-lambda-expression (symbol-function currsymbol)))
		     (terpri))
		   )
		  (t (message-dialog (format nil "nothing to view/edit for ~s" 
					     currsymbol))))))
  (send helpbtn :action ;;; aixo ha de ser un help
	#'help-pages)
  (funcall update-symbol-list)
  ;;(def edit-symbols-dialog dlg)
  dlg
  ))

;;;this is a piece of code to edit a lambda expression, discarded because of interface limitations
;;		(if (and (fboundp (select symbol-list current))
;;			 (function-lambda-expression 
;;			  (symbol-function (select symbol-list current)))
;;			 (setf tmptext (write-to-string
;;					(append (list 'defun (select symbol-list current))
;;						(cdr (function-lambda-expression 
;;						      (symbol-function 
;;						       (select symbol-list
;;							       current)))))
;;					:escape t :length 70))
;;			 (< (length tmptext) 500))
;;		    (when (setq newval 
;;				(get-string-dialog 
;;				 "You can short the defun (at your own risk!)"
;;				 :initial (break-string-lines
;;					   70 tmptext)))
;;			  (eval-string newval)
;;			  (send itlist :action)))



(defun help-pages (&key (strings *help-strings*)
				   (title (format nil "Help for Edit-symbols (~a)" *version*)))
  (let* ((current-page 0)
	 (next (send button-item-proto :new "Next >"))
	 (prev (send button-item-proto :new "< Prev"))
	 (text (send text-item-proto :new (select strings 0)))
	 (page (send text-item-proto :new (format nil "Page 1 of ~a" (length strings))))
	 dlg
	 (update-dialog
	  #'(lambda ()
	      (send text :text (select strings current-page))
	      (send page :text (format nil "Page ~a of ~a" 
				       (1+ current-page) (length strings))))))
    (setq dlg (send dialog-proto :new (list text (list prev next page))
		    :title title
		    :modeless t))
    (send next :slot-value 'action
	  #'(lambda ()
	      (setf current-page (mod (1+ current-page) (length strings)))
	      (funcall update-dialog)))
    (send prev :slot-value 'action
	  #'(lambda ()
	      (setf current-page (mod (1- current-page) (length
					       strings)))
	      (funcall update-dialog)))
))


(defun edit-object (symbol)
"Arg SYMBOL, a symbol bound to an object or the object itself
 Open a dialog window to list and inspect it"
(let* (current currsymbol currval
       doc 
       symbol-list
       (object (cond ((objectp symbol) symbol)
		     ((objectp (eval symbol))  (eval symbol))
		     (t (error "symbol must be an object or bound to object ~a" symbol))))
       (choice-list '("Slots" "Methods" "Proto" "Parents" "Prec. list"))
       (radio-symbol-choice
	(send choice-item-proto :new choice-list :value 0))
       (only-owned (send toggle-item-proto :new "Only owned" :value nil))
       (itlist (send list-item-proto :new 
		     (make-array *maxnum-edit-symbols* :initial-element "")
		     :columns 1))
       (last-list-length 0)
       (helpbtn (send button-item-proto :new "Help"))
       (infobtn (send button-item-proto :new "Doc"))
       (editbtn (send button-item-proto :new "View/edit"))
       (apropbtn (send button-item-proto :new "Apropos"))
       (apropstring "")
       (taprop (send text-item-proto :new "           "))
       (tname (send text-item-proto :new 
		    "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"))
       (tval (send text-item-proto :new  "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"))
       (tvalf (send text-item-proto :new "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"))
       (tfake (send text-item-proto :new "          "))
       (numsym (send text-item-proto :new "# of symbols: 1500  "))
       (also-in-doc (send toggle-item-proto :new "Look also in doc" :value nil))
       tmp tmptext
       (dlg (send dialog-proto :new 
		  (list 
		   (list
		    (list (send text-item-proto :new "What to list")
			  (list radio-symbol-choice)
			  (list only-owned)
			  helpbtn)
		    (list (list itlist (list
					(list infobtn)
					(list editbtn)
					;(list tfake)
					;(list apropbtn)	
					;(list taprop)
					;(list also-in-doc)
					))
			  (list numsym)
			  ;;;(list ti)
			    ;;;(list tdoc)
			  ;;;(list tpack)
			  ))
		   (list (list tname
			       tval ));;tvalf
		   )
		  :title (format nil "View and edit object ~a (~a)" 
				 (if (symbolp symbol) symbol "")
				 *version*)
		  :modal nil))
       ;;some symbols to be funcall'ed
       (get-value
	#'(lambda ()
	    (unless (eq '--nothing-- (select symbol-list (send itlist :selection)))
	      (case (send radio-symbol-choice :value)
		    (0 ;slots
		     (send object :slot-value (select symbol-list (send itlist :selection)))
		     )
		    (1 ;methods
		     (send object :get-method 
			   (find-symbol (symbol-name 
					 (select symbol-list (send itlist :selection)))
					(find-package 'keyword)))
		     )))))
       (get-doc
	#'(lambda ()
	    (setq current (send itlist :selection))
	    (setq currsymbol (select symbol-list current))
	    (case (send radio-symbol-choice :value)
		  (0 ;slots: can it have doc?
		   (send object :documentation currsymbol)
		   )
		  (1 ;methods
		   (send object :documentation
			 (find-symbol (symbol-name currsymbol)
				      (find-package 'keyword)))))))
       (get-name 
	#'(lambda (obj);get some string to represent it in the window's list
	    (if (objectp obj)
		(format nil "~a" obj)
	      (symbol-name obj))))
       (format< ; for sorting anything
	#'(lambda(x y)
	    (string< (format nil "~a" x)
			 (format nil "~a" y))))
       (get-prec-list
	#'(lambda ()
	    (send object :precedence-list)))
       (get-slots 
	#'(lambda ()
	    (if (send only-owned :value)
		(send object :own-slots)
	      (let ((precs (send object :precedence-list))
		    tmplist)
		(setq tmplist (mapcar #'(lambda (o) (send o :own-slots)) precs))
		(setq tmplist (reduce #'union tmplist))))))
       (get-methods
	#'(lambda ()
	    (if (send only-owned :value)
		(send object :own-methods)
	      (let ((precs (send object :precedence-list))
		    tmplist)
		(setq tmplist (mapcar #'(lambda (o) (send o :own-methods)) precs))
		(setq tmplist (reduce #'union tmplist))))))
       (update-symbol-list ; looking at radio buttons
	#'(lambda ()
	    "this is the main function. Looking at the state
               of controls, it builds the list and calls install-symbols-in-list"
	    (setf symbol-list nil)
	    (case (send radio-symbol-choice :value)
		  (4			;prec list
		   (setf symbol-list (funcall get-prec-list)))
		  (3			;parents
		   (setf symbol-list (send object :parents)))
		  (2		        ;proto
		   (setf symbol-list (list (send object :slot-value 'proto-name))))
		  (1			;methods
		   (setf symbol-list (funcall get-methods)))
		  (0			; slots
		   (setf symbol-list (funcall get-slots))))
	    (unless (string= apropstring "")
	      (if (send also-in-doc :value)
		  (setq symbol-list nil);;(delete-if-not symbol-or-doc-match-apropos symbol-list)
		(setq symbol-list nil)));; (delete-if-not symbol-match-apropos symbol-list)
	    ;;(format t "~a ~a:~a~%" (send radio-symbol-choice :value)
	    ;;      (send radio-packages :value)
	    ;;    (size symbol-list))
	    (if (= 0 (length symbol-list))
		(setf symbol-list (make-array 1 :initial-contents '(--nothing--)))
	      (setf symbol-list (make-array (length symbol-list)
					    :initial-contents symbol-list)))
	    (when (member (send radio-symbol-choice :value) '(0 1))
	      (setf symbol-list (sort symbol-list format<)))
	    (funcall install-symbols-in-list)
	    (setq last-list-length (length symbol-list))
	    (send numsym :text (format nil "# of symbols: ~a"
				       (length symbol-list)))
	    ;;(user::def symlist symbol-list)
	    )
	)
       (install-symbols-in-list
	#'(lambda ()
	    (when (> (length symbol-list) *maxnum-edit-symbols*)
	      (progn 
		(message-dialog 
		 (format nil "Number of symbols ~a exceeds ~a, list will be choped"
			 (length symbol-list) *maxnum-edit-symbols*))
		(setf symbol-list (select symbol-list (iseq *maxnum-edit-symbols*)))))
	    (dotimes (i (min *maxnum-edit-symbols*
			     (max last-list-length
				  (length symbol-list))))
		     (if (< i (length symbol-list))
			 (send itlist :set-text i 
			       (funcall get-name (aref symbol-list i)))
		       (send itlist :set-text i "")))
	    (when (> (send itlist :selection) (length symbol-list)
		     (send itlist :selection 0)))
	    (if (send itlist :selection);;doesn't work?
		(send itlist :do-action))))
       )
  ;;body begins here
  (send only-owned :slot-value 'action 
	#'(lambda ()
	    (if (member (send radio-symbol-choice :value) '(0 1))
		(funcall update-symbol-list)
	      (sysbeep))))
  (send itlist :slot-value 'action;; for the list
	#'(lambda (&optional (double nil))
	    (setq current (send itlist :selection))
	    (send tname :text (funcall get-name (select symbol-list current)))
	    (setq currval (funcall get-value))
	    (cond ((member (send radio-symbol-choice :value) '(2 3 4))
		   (setq tmptext ""))
		  ((< 5000 (flatsize currval))
		   (setq tmptext "--value too long to display--"))
		  ((< 1000 (length (setq tmptext (format nil "~s" currval))))
		   (setq tmptext (subseq tmptext 0 100)))
		  (t
		   (setq tmptext (format nil "~a" (funcall get-value)))))
	    (send tval :text tmptext)
	    (send tvalf :text "")))
  (defmeth itlist :selection (&optional (index nil iset))
    (if iset
	(call-next-method index)
      (if (call-next-method)
	  (max 0 (min (1- (length symbol-list)) (call-next-method))))))
  (send itlist :selection 0)
  (send radio-symbol-choice :action update-symbol-list)
  (send editbtn :action;;edit values
	#'(lambda ()
	    (setq current (send itlist :selection))
	    (setq currsymbol (select symbol-list current))
	    (cond ((or (objectp currsymbol)
		       (and (symbolp currsymbol)
			    (boundp currsymbol)
			    (objectp (eval currsymbol))))
		   (edit-object currsymbol))
		  ((and (= 0 (send radio-symbol-choice :value))
			(objectp (funcall get-value)))
		   (edit-object (funcall get-value)))
		  ((= 0 (send radio-symbol-choice :value));slots
		   (setq currval (funcall get-value))
		   (if (> (flatsize currval) 5000)
		       (setq tmptext (make-string 201 :initial-element #\x))
		     (setq tmptext (format nil "~s" currval)))
		   (if (> (length tmptext) 200)
		       (message-dialog "Value too long to edit")
		     (when (setq newval 
				 (get-string-dialog 
				  "Edit a new value (at your own risk!)"
				  :initial tmptext))
		       (send object :slot-value currsymbol  
			     (car (eval-string newval)))
		       (send itlist :do-action))))
		  ((= 1 (send radio-symbol-choice :value));methods
		   (setq currval (funcall get-value))
		   (if (function-lambda-expression currval)
		       (when (ok-or-cancel-dialog
			      "Edit not allowed. ~%Want to pprint it to the listener?")
			 (pprint (function-lambda-expression currval))
			 (terpri))
		     (message-dialog (format nil "nothing to view/edit for ~s" 
					     currsymbol)))))))
  (Send helpbtn :action
	#'(lambda ()
	    (help-pages :strings *help-pages-edit-object*
			:title (format nil "Help for Edit-objects (~a)" *version*))))
  (send infobtn :action;;info-doc
	#'(lambda ()
	    (setq tmptext (funcall get-doc))
	    (when (< 300 (length tmptext))
	      (setq tmptext (subseq tmptext 0 300)))
	    (message-dialog (format nil "~a" tmptext))))
  (funcall update-symbol-list)
  (send itlist :selection 0)

  dlg
  ;;(user::def objdlg dlg)
  ))


(defvar *help-strings* "" "")

(setq *help-strings* 
#("Help for edit-symbols in symedit.lsp file.

Click to select options and the list will be updated.
Click on a symbol and, below the list, value and originating package of the symbol
   will be shown.
Info shows the doc (as help does) for the symbol.
Edit can do it in some cases.
Apropos is to restrict the symbols listed.
   (look also in doc is veeery time consuming)



Choose 'next' for more details.


.
"

"'What to list' options:

Global (*) vars: those that have * in their name.
   Usually created by defvar, so they are special.

Objects: those symbols that evaluate to an object.

All vars: all symbols that are bound to something.

Functions: all symbols that are fbound to something.

All symbols: all.
"

" 'What package' options:

All existing packages when the dialog is created are listed.
The symbols accessible from the package will appear in the list.
Below the list, the total number of listed symbols is shown.

XLISP contains most of xlisp and xlispstat symbols.
KEYBOARD contains the symbols used as keys for functions and methods.
USER contain some xlispstat created symbols and user created ones.
XLSCMP contain symbols for the compiler.
EDIT-SYMS contain symbols for edit-symbols and related functions.
"

" 'Only created in pckg' option:

When this option is checked, only the symbols created in the currently
   selected package are shown.
If the list shows '--NOTHING--' it means that no symbol is listed... ;-).
Each symbol has a package cell that stores the package
   where the symbol was created.
If this symbol is exported and other packages 'use' the original
   package, the symbol will be accessible from those packages, so it
   will be listed when those packages are selected for listing.
Take a look at Steele's bible if you want to know more than myself...
"

" The symbols list:

List-item objects are not the best XLISPSTAT feature, as you probably know.
They're not sizeable, number of items can't be changed dynamically and has 
a low limit, item change must de done one by one, ... My solution has been
to use a fixed number of items (1500) and to fill it with empty strings.
You'll see sometimes an empty list, look at the top items!
If you click once in the list, the keyboard is activated: arrows, pg-up,
   pg-down, initials, will work (Windoze, not Unix).
When a symbol is selected, its value, function value and original package
   are shown below. The value is what is bound to the symbol. The function
   value is what is fbound to the symbol, usually made by a defun call.

"

" Buttons:

Info: tries to show info on the symbol. Usually the same text as produced 
      by a help call. The value is also shown, if it's too large, it's trimmed.
View/Edit: Allows to change the symbol's value. Don't use it unless you 
      really know what you are doing. This is not always true for me.
      Canceling is always safe. For objects, open a new window to view/edit it.
      For functions, when posible, shows the definition.
Help: You already know what's used for.
Apropos: the typed string is used to restrict the list to those symbols
      that contain the string. The string is shown below the button.
      You need to introduce the empty string to cancel.
Look also in doc: the apropos string will be searched for also in the doc string
      for the symbol. Don't use it in a slow machine.... needs optimization.
"

" Disclaimer:
                 Pots prendre mal si jugues amb aquest diýleg...

This is catalan for: If you play with this tool, you can get some
   damage. It can be used safely to get info about symbols you know
   about.

This version looks quite stable and safe, but... if you use it for fun,
save your work first.

Written by Frederic udina <http://libiya.upf.es/>
Any comment welcome: udina@upf.es

"))

(defvar *help-pages-edit-object* "" "Vector of pages for help")

(setq *help-pages-edit-object*
#("Help for edit-object in symedit.lsp file.

Click to select options and the central list will be updated.

Click on a symbol and, below the list, value and originating package of the symbol
   will be shown.

Doc shows the available documentation (as help does) for a method.

View/Edit can do it in some cases.


Choose 'next' for more details.
.
"

"What to list:

Slots: the slots for the object are listed. If 'only owned' is checked,
       only the slot owned by the object, not by their precedents, are listed.
       When Slots is selected, the name and value for the slot are displayed 
       at the bottom.
Methods: Similar as previous. At the bottom, the function bound is displayed.
Proto: The name of the prototype that created the object is displayed.
Parents: All the parents for the object are listed (frequently only one).
Precedence list: All the objects in the precedence list are listed.

See the help pages for edit-symbols for peculiarities of the list item.

.
"
"Help for edit-object buttons

Doc shows the available documentation for the selected method. Shows nil
    for slots or other.

View/Edit allows edition of slot values (do it carefully, damage can be done
    if you don't know exactly what you're doing). Canceling is always safe.
    For methods that have lambda-expressions, it can be printed to the listener.
    For objects, the button open a new window for that object.


Written by Frederic udina <http://libiya.upf.es/>
Any comment welcome: udina@upf.es

"
))



(defun eval-string (aString)
  "Returns a list of the expressions obtained 
on lisp evaluation of the characters in aString"  
  (let ((st (make-string-input-stream aString))   
        (result nil)   
        (expr nil))  
    (loop (setq expr (read st nil 'eof))  
          (when (eql expr 'eof)    
                (return (reverse result)))  
          (setf result (cons expr result)))))

(provide "edit-syms")
;;;;;;;;;;;;;;end of file symedit.lsp
