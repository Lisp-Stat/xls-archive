;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; file edit-globals.lsp, frederic udina, dec 5 1996
;;;;
;;;; calling (edit-globals) a dialog window will appear 
;;;; making it easy to see the list of global variables
;;;; its values and allowing also to edit them.
;;;;
;;;; history: fu 96/12/5 wrote the first quick version 
;;;;                     waiting for someone to take it on
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defvar *varlist* '(*APPLYHOOK* *ASK-ON-REDEFINE* *BAKTRACE-PRINT-ARGUMENTS* *BREAKENABLE* *COMMAND-LINE* *COMPILE-PRINT* *COMPILE-VERBOSE* *DEFAULT-PATH* *ERROR-OUTPUT* *EVALHOOK* *FEATURES* *FLOAT-FORMAT* *GC-FLAG* *KEEP-DOCUMENTATION-STRINGS* *MODULES*
 *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE* *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL* *PRINT-PRETTY* *PRINT-RADIX* *PRINT-READABLY* *RANDOM-STATE* *READ-CIRCLE-DATA* *READ-SUPPRESS* *STANDARD-INPUT* *STANDARD-OUTPUT*
 *STARTUP-FUNCTIONS* *TERMINAL-IO* *TRACE-OUTPUT* *TRACELIMIT* *TRACELIST* *TRACENABLE* *VARIABLES*))

(defun edit-globals ()
  (let* (current
	 (itlist (send list-item-proto :new (mapcar #'symbol-name *varlist*) :columns 1))
	 (btn1 (send button-item-proto :new "Show"))
	 (btn2 (send button-item-proto :new "Info"))
	 (btn3 (send button-item-proto :new "Edit"))
	 (ti (send text-item-proto :new "                               "))
	 (dlg (send dialog-proto :new 
		    (list
		     (list itlist)
		     (list ti)
		     (list btn2 btn3))
		    :modal nil)))
    (send itlist :slot-value 'action
	  #'(lambda (&optional (double nil))
	      (send ti :text (format nil "Value: ~a" 
				     (eval (select *varlist* 
						   (send itlist :selection)))))))
    (send btn1 :action 
	  #'(lambda ()
	      (setq current (send itlist :selection))
	      ;;(format t "~a ~a ~%"  current (select  *varlist* current))
	      (message-dialog (format nil "~a is ~a"
				      (symbol-name (select *varlist* current))
				      (eval (select *varlist* current))))))
    (send btn2 :action 
	  #'(lambda ()
	      (setq current (send itlist :selection))
	      ;;(format t "~a ~a ~%"  current (select  *varlist* current))
	      (message-dialog (format nil "~a: ~a~%~a"
				     (symbol-name (select *varlist* current))
				     (eval (select *varlist* current))
				     (documentation (select *varlist* current) 
						    'variable)))))
    (send btn3 :action 
	  #'(lambda ()
	      (setq current (send itlist :selection))
	      (when (setq newval 
			  (get-string-dialog 
			   "Edit a new nalue (at your own risk!)"
			   :initial (format nil "~s"
					    (eval (select *varlist* current)))))
		    (set (select *varlist* current) 
			  (car (eval-string newval))))))
    (def edit-globals-dialog dlg)
))


(defun read-string (&optional (stream *standard-input*)
			      (error-on-eof t)
			      (end-of-read-char #\Newline))
  "Args optional (stream *standard-input*) (error-on-eof t) 
(end-of-read-char #\Newline) 
Reads  all chars from stream until end-of-read-char is found, 
and returns them in a string. Max 25600 chars! 
If error-on-eof is nil and eof is reached, 
reading is also completed and the string is returned."  
(let* ((max-len 25600)
       (theString (make-array max-len))
       (len 0)        
       (aChar nil))    
  (loop (setq aChar (read-char stream))    
        (when (and (eql aChar nil) error-on-eof)
              (print "end of file  on read-string")      
              (return nil)) 
        (when (or (eql end-of-read-char aChar)   
                  (eql aChar nil))   
              (return (coerce (select theString (iseq len)) 'string)))  
        (setf (elt theString len) aChar)  
        (setq len (+ len 1)) 
        (when (>= len max-len)      
              (print "maximum length string reached")     
              (return nil))))) 



(defun eval-string (aString)
  "Returns a list of the expressions obtained 
on evaluation of the characters in aString"  
  (let ((st (make-string-input-stream aString))   
        (result nil)   
        (expr nil))  
    (loop (setq expr (read st nil 'eof))  
          (when (eql expr 'eof)    
                (return (reverse result)))  
          (setf result (cons expr result)))))



