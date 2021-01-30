;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the parameter display objects for parameter manipulation


(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :slider (strcat ElToY-directory "utils" *d-sep*  "slider.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-nui-parameters (strcat ElToY-directory "params" *d-sep*  "nui-params.lsp"))
(require :el-data-mixin (strcat ElToY-directory "params" *d-sep*  "data-mixin.lsp"))

;;; data-disp-proto --- general purpose display object for displaying
;;; parameters 

(defproto data-disp-proto 
  '(data				;data displayed
    prompt-text-items			;data names displays
    data-text-items			;data value display/set items
    evaluate-button-items		;evaluate expression buttons
    reset-button-items			;reset expression buttons
    data-parent-signal			;signal to be used with parent
    ) '() (list ElToY-dialog-proto subtool-mixin ElToY-object)
"Group of data dialogs"
)


;;; :print and describe
(defmeth data-disp-proto :print (&optional (stream t))
   (format stream "#<data-disp: ~S ~A>"
	   (send self :title)
	   (if (slot-value 'data) 
	       (send (slot-value 'data) :print nil)
	     nil)))

;; :describe
(defmeth data-disp-proto :describe (&optional (stream t) (verbose nil))
   (format stream "~S: Data Display for: ~A~%"
	   (send self :title)
	   (send (slot-value 'data) :print))
   (format stream "Variables: ~S~%" (send (slot-value 'data) :names))
   )


; :isnew method
(defmeth data-disp-proto :isnew
	 (other-items &rest args
	  &key data (supertool nil)
	       (title nil) (data-parent-signal :data)
	       (data-parent-signal-fun #'data-disp-parent-signal)
	  &allow-other-keys)

   ; set up title/supertool
   (setq title (if title title
		   (format nil "Data: ~S" (send data :names))))
   (setf (slot-value 'supertool) supertool)
   
   ; set parameters in place
   (setf (slot-value 'data) data)
   (send self :data-parent-signal data-parent-signal data-parent-signal-fun)

   ; now start spawning manipulation tools
   (setf (slot-value 'prompt-text-items)
	 (mapcar #'(lambda (name)
		     (send text-item-proto :new
			   (format nil "~A: (Expression to evaluate)"
				   name)
			   :text-length 50))
		 (send data :names)))
   (setf (slot-value 'data-text-items)
	 (mapcar #'(lambda (value name)
		     (send edit-text-item-proto :new (format nil "'~S" value)
			   :text-length 50
			   :action 
			   #'(lambda (val)
			       (send self :value-e name val))))
		 (send data :data)
		 (send data :names)))
   (setf (slot-value 'evaluate-button-items)
	 (mapcar #'(lambda (name)
		     (send button-item-proto :new
			   (format nil "Evaluate ~A" name)
			   :action 
			   #'(lambda ()
			       (send self :evaluate-button name))))
		 (send data :names)))
   (setf (slot-value 'reset-button-items)
	 (mapcar #'(lambda (name)
		     (send button-item-proto :new
			   (format nil "Reset ~A" name)
			   :action 
			   #'(lambda ()
			       (send self :reset-button name))))
		 (send data :names)))
   
      
   ; now figure create the master dialog
   (apply #'call-next-method
	 (append other-items
		 (apply #'nconc
			(mapcar #'(lambda (prompt data evaluate reset)
				    (list prompt data
					  (list evaluate reset)))
				(slot-value 'prompt-text-items)
				(slot-value 'data-text-items)
				(slot-value 'evaluate-button-items)
				(slot-value 'reset-button-items))))
	 :title title
	 args)
   
   ;return self
   self
   )


(send data-disp-proto :documentation :isnew
      "other-items
Keywords:
  :supertool (parent tool object)
  :data (displayed data object)
  :title 
  :data-parent-signal (signal to send to parent when changing displayed
         object)
  :data-parent-signal-fun (function which handles local update request).
")

;; destruct -- destroys sub-objects then object itself
(defmeth data-disp-proto :destruct ()
   (setf (slot-value 'data) nil)
   (setf (slot-value 'prompt-text-items) nil)
   (setf (slot-value 'data-text-items) nil)
   (setf (slot-value 'evaluate-button-items) nil)
   (setf (slot-value 'reset-button-items) nil)
   (call-next-method))
   


;;; :update --- the general purpose update handler
;;; can use default value of this function.




;;; this is the dispatch function which handles the parent-signal
;;; dispatch.  
(defun data-disp-parent-signal (self &optional (signal :values)
				     &rest args)
  (let ((trace (find (send self :data-parent-signal) (trace))))
    (when trace (format t "Object ~S" self))
    (when (and (listp signal) (every #'numberp (element-seq signal)))
	  (push signal args) (setq signal :data))
    (if (eq signal :local)		;local set of some nature
	(let* ((signal (pop args))
	       (who (position signal (send (slot-value 'data) :names))))
	  (when trace
		(format t "local-signal: signal:~S  args:~S who: ~S~%"
			signal args who))
	  (if who				;single value change
	      (case (car args)
		    ((:value :values :data)
		     (apply #'send self :data-values1 who :local (cdr args)))
		    ((:name :names)
		     (apply #'send  self :data-names1
			    who :local (cdr args)))
		    (t (apply #'send self :data-values1 who :local args)))
	    (if (and (listp (car args)) (every #'numberp (element-seq (car args))))
		(apply #'send self :data-values :local args)
	      (apply #'send self signal :local args))))
    
    ;non-local set, send to parents
      (let ((set (if (member signal (send (slot-value 'data) :names))
		     (or (> (list-length args) 1)
			 (every #'numberp (element-seq (car args))))
		   args)))
	(when trace
	      (format t "data-parent-signal: signal:~S  set:~S args:~S ~%"
		      signal set args ))
	(if set
	    (apply #'send  self :supertool
		   (send self :data-parent-signal) signal args)
	  (apply #'send  (slot-value 'data) signal args))))))



;;; set/select methods:

;; :data-parent-signal
(defmeth data-disp-proto :data-parent-signal (&optional (signal nil set)
							(signal-fun
							 #'data-disp-parent-signal
							 psf-given))
   (when set
	 (setf (slot-value 'data-parent-signal) signal)
					; take care of local parameter updating
	 (when (and (slot-value 'data-parent-signal)
		    (send self :has-method (slot-value 'data-parent-signal)))
	       (unless psf-given
		       (setq signal-fun (send self :get-method
					      (slot-value 'data-parent-signal))))
	       (send self :remove-method (slot-value 'data-parent-signal)))
	 (send self :add-method signal signal-fun))
   (slot-value 'data-parent-signal))

(send data-disp-proto :documentation :data-parent-signal
      ":data-parent-signal &optional signal signal-fun

Returns/sets data-parent-signal.  

If setting, the new <signal> is made a method executing the
<signal-fun>.
")


;;; :action methods

;; :value-e changes named value, value sent as expression to eval
(defmeth data-disp-proto :value-t (name value)
  (send self :supertool (send self :data-parent-signal) name
	(round-if (eval (read-string value))
		  (send (slot-value 'data) name :integer?))))

(send data-disp-proto :documentation :value-e
      ":value-e name value

Changes value sent as string.
")


(defmeth data-disp-proto :evaluate-button (name)
  (let* ((who (position name (send (slot-value 'data) :names)))
	 (int? (send (slot-value 'data) name :integer?))
	 (new-val (round-if (eval (read-string
				   (send (nth who (slot-value 'data-text-items))
					 :text)))
			    int?)))
    (if (equalp (send (slot-value 'data) name) new-val)
	(send self :data-values1 who :local new-val)	;update display
      (send self :supertool (send self :data-parent-signal)
	    name :data new-val))
    ))

(send data-disp-proto :documentation :evaulate-button
      "Causes changed data values to be read.
")

(defmeth data-disp-proto :reset-button (name)
  (let* ((who (position name (send (slot-value 'data) :names)))
	 (old-val (format nil "`~S" (send (slot-value 'data) name))))
    (send (nth who (slot-value 'data-text-items)) :text old-val)))


(send data-disp-proto :documentation :reset-button
      "Causes changed data values text fields to be reset to current
value. 
")



;;; parameter setting functions

(defmeth data-disp-proto :data-values (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((new-vals (send (slot-value 'data) :data)))
	; use set values which should have been properly inherited
	(mapcar #'(lambda (value text-item)
		    (send text-item :text (format nil "`~S" value)))
		new-vals (slot-value 'data-text-items)
		(slot-value 'slider-items)))
    (apply #'send self :supertool (send self :data-parent-signal)
	          :data args)))

(send data-disp-proto :documentation :data-values
      "[:local] Updates display, new data.
Otherwise follows update logic.
")


(defmeth data-disp-proto :data-values1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((value (send (slot-value 'data) :data1 who)))
	(send (nth who (slot-value 'data-text-items))
	      :text (format nil "`~S" value)))
    (apply #'send self :supertool (send self :data-parent-signal)
	   (nth who (send (slot-value 'data) :names))
	   :data args)))


(send data-disp-proto :documentation :data-values1
      "[:local] Updates display, new data values.
Otherwise follows update logic.
")

(defmeth data-disp-proto :data-names (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((new-vals (send (slot-value 'data) :names)))
	; use set values which should have been properly inherited
	(mapcar #'(lambda (value text-item)
		    (send text-item :text (format nil "~g" value))
		new-vals (slot-value 'name-text-items))
		))
    (apply #'send self :supertool (send self :data-parent-signal)
	   :names args)))

(send data-disp-proto :documentation :names
      "[:local] Updates display, new parameter names.
Otherwise follows update logic.
")

(defmeth data-disp-proto :names1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((new-val (send (slot-value 'data) :names1 who)))
	; use set values which should have been properly inherited
	(send (nth who (slot-value 'name-text-items))
	      :text (format nil "~g" new-val)))
    (apply #'send self :supertool (send self :data-parent-signal)
	   :names args)))

(send data-disp-proto :documentation :names1
      "[:local] Updates display, new parameter names.
Otherwise follows update logic.
")



;;;------------------------------------------------------------------------
;;; data-nui-para-disp-proto
;;;------------------------------------------------------------------------

;;; Dialog for both data and nuisance parameters

(defproto data-nui-para-disp-proto
  '() '() (list para-disp-proto data-disp-proto))

;;; should get just about everything but initial title from multiple
;;; inheritance. 

(defmeth data-nui-para-disp-proto :isnew
  (other-items &rest args &key (title nil) data parameters
	       &allow-other-keys)
  
  (apply #'call-next-method other-items
	 :title (if title title
		  (format nil "Data: ~A   Parameters: ~A"
			  (send data :names)
			  (send parameters :names)))
	 args))


;;; :print/:describe


(defmeth data-nui-para-disp-proto :print (&optional (stream t))
   (format stream "#<data-para-disp: ~S (~A | ~A) >"
	   (send self :title)
	   (if (slot-value 'data) 
	       (send (slot-value 'data) :print nil)
	     nil)
	   (if (slot-value 'paras) 
	       (send (slot-value 'paras) :print nil)
	     nil)))

;; :describe
(defmeth data-nui-para-disp-proto :describe (&optional (stream t) (verbose nil))
   (format stream "~S: Data Display for: ~A~%"
	   (send self :title)
	   (send (slot-value 'data) :print))
   (format stream "Variables: ~S~%" (send (slot-value 'data) :names))
   (format stream "~S: Parameter Display for: ~A~%"
	   (send self :title)
	   (send (slot-value 'paras) :print))
   (format stream "Variables: ~S~%" (send (slot-value 'paras) :names))
   )


(new-provide :el-data-disp)
