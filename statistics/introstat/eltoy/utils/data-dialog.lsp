
;;; Copyright 1992 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines a "modal" variation on the get-value-dialog

(require :el-superset (strcat ElToY-directory "utils" *d-sep*  "superset.lsp"))
(require :el-checks (strcat ElToY-directory "utils" *d-sep*  "checks.lsp"))


(defproto evaluate-or-reset-dialog-proto 
  '(evaluate-button reset-button) () dialog-proto
  "Sort of like the ok-or-cancel-dialog, but modalless")

(defmeth evaluate-or-reset-dialog-proto :isnew
         (items &rest args &key recipient message
		(evaluate-default t) 
		(evaluate-action
		 #'(lambda () (send recipient message t)))
		 (reset-action 
		  #'(lambda () (send recipient message nil))))
 (let ((items (if (consp items) items (list items)))
        (evaluate-button (send button-item-proto :new "Evaluate" 
                         :action evaluate-action))
        (reset-button (send button-item-proto :new "Reset"
                             :action reset-action)))
    (setf items (mapcar #'(lambda (x) 
                                  (if (stringp x) 
                                      (send text-item-proto :new x)
                                      x))
                        items))
    (setf (slot-value 'evaluate-button) evaluate-button)
    (setf (slot-value 'reset-button) reset-button)
    (apply #'call-next-method
           (append items (list (list evaluate-button reset-button)))
           args)
    (send self :default-button
	  (if evaluate-default evaluate-button reset-button))))

(send evaluate-or-reset-dialog-proto :documentation :new
"Args: items &rest args &key recipient message
         (evaluate-default t) evaluate-action reset-action

Note:  text strings in items (which should be a list) are turned into
text items. 

Default is to (send recipient message (or T nil)) whenever one of the
buttons is pressed (T for evaluate, nil for reset).
")

;
;;; ---- Edit String Dialog -----



(defproto edit-string-dialog-proto '(default-fun edit-item) ()
  evaluate-or-reset-dialog-proto)

(defmeth edit-string-dialog-proto :isnew
  (prompt &rest args &key owner message (text-length 20)
	  (evaluate-action nil)
	  (reset-action #'(lambda ()
			    (send self :reset))))
  (setf (slot-value 'default-fun) #'(lambda () (send owner message)))
  (let* ((prompt-item (send text-item-proto :new prompt))
	 (initial (send owner message))
         (edit-item (send edit-text-item-proto :new 
                          (format nil "~a" initial)
			  :text-length text-length)))
    (setf (slot-value 'edit-item) edit-item)
    (unless evaluate-action
	    (setf evaluate-action
		  #'(lambda ()
		      (send owner message (send edit-item :text)))))
    (apply #'call-next-method 
           (list prompt-item edit-item)
           :evaluate-action evaluate-action
	   :reset-action reset-action
           args)))


(send edit-string-dialog-proto :documentation :new
"Args: prompt &rest args &key owner message (text-length 20)
         evaluate-action reset-action

Assumes that message Gets/Sets a string value.  Default string is
taken to be (send owner message).

Default action for evaluate is to (send owner message string) 
Default action for reset is to set text from (send owner message)
")


(defmeth edit-string-dialog-proto :reset ()
  (send (slot-value 'edit-item) :text
	(format nil "~a" (funcall (slot-value 'default-fun)))))

(send edit-string-dialog-proto :documentation :reset
      "Resets string value.")

;
;;; ---- Edit Value Dialog -----



(defproto edit-value-dialog-proto '(edit-item default-fun) ()
  evaluate-or-reset-dialog-proto)

(defmeth edit-value-dialog-proto :isnew
	  (prompt &rest args &key owner message (text-length 45)
		  (evaluate-action nil)
		  (reset-action #'(lambda ()
				    (send self :reset))))
  (setf (slot-value 'default-fun) #'(lambda () (send owner message)))
  (let* ((prompt-item (send text-item-proto :new prompt))
	 (initial (send owner message))
	 (edit-item (send edit-text-item-proto :new 
                          (format nil "`~S" initial)
			  :text-length text-length)))
    (setf (slot-value 'edit-item) edit-item)
    (unless evaluate-action
	    (setf evaluate-action
		  #'(lambda ()
		      (send owner message
			    (eval (read (make-string-input-stream
					 (send edit-item :text))))))))
    (apply #'call-next-method 
           (list prompt-item edit-item)
           :evaluate-action evaluate-action
	   :reset-action reset-action
           args)))


(send edit-value-dialog-proto :documentation :new
"Args: prompt &rest args &key owner message (text-length 45)
         evaluate-action reset-action

Input string is evaluated before returning.

Assumes that message Gets/Sets a value value.  Default value is
taken to be (send owner message).

Default action for evaluate is to (send owner message value) 
Default action for reset is to set text from (send owner message)
")


(defmeth edit-value-dialog-proto :reset ()
  (send (slot-value 'edit-item) :text
	(format nil "`~S" (funcall (slot-value 'default-fun)))))

(send edit-value-dialog-proto :documentation :reset
      "Resets value.")



(new-provide :data-dialogs)

