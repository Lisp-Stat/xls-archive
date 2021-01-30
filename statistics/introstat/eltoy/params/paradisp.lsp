;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the parameter display objects for parameter manipulation


(require :el-utils (strcat ElToY-directory "utils" *d-sep*  "utils.lsp"))
(require :slider (strcat ElToY-directory "utils" *d-sep*  "slider.lsp"))
(require :el-update (strcat ElToY-directory "utils" *d-sep*  "update.lsp"))
(require :el-parameters (strcat ElToY-directory "params" *d-sep*  "parameters.lsp"))


;;; para-disp-proto --- general purpose display object for displaying
;;; parameters 

(defproto para-disp-proto 
  '(paras				;parameters displayed
    name-text-items			;parameter names displays
    value-text-items			;value display/set items
    update-button-items			;update value buttons
    lb-text-items			;slider-limits display (lower bound)
    ub-text-items			;slider-limits display (upper bound)
    slider-items			;parameter value sliders
    parent-signal			;signal to be used with parent
    ) '() (list ElToY-dialog-proto subtool-mixin ElToY-object)
"Group of sliders displaying setting"
)


;;; :print and describe
(defmeth para-disp-proto :print (&optional (stream t))
   (format stream "#<para-disp: ~S ~A>"
	   (send self :title)
	   (if (slot-value 'paras) 
	       (send (slot-value 'paras) :print nil)
	     nil)))

;; :describe
(defmeth para-disp-proto :describe (&optional (stream t) (verbose nil))
   (format stream "~S: Parameter Display for: ~A~%"
	   (send self :title)
	   (send (slot-value 'paras) :print))
   (format stream "Variables: ~S~%" (send (slot-value 'paras) :names))
   )


; :isnew method
(defmeth para-disp-proto :isnew
	 (other-items &rest args
	  &key parameters
	       (title nil) (parent-signal :parameters)
	       (parent-signal-fun #'para-disp-parent-signal)
	       (supertool nil)
	  &allow-other-keys)

   ; set up title/supertool
   (setq title (if title title
		   (format nil "~S" (send parameters :names))))
   (setf (slot-value 'supertool) supertool)
   
   ; set parameters in place
   (setf (slot-value 'paras) parameters)
   (send self :parent-signal parent-signal parent-signal-fun)

   ; now start spawning manipulation tools
   (setf (slot-value 'name-text-items)
	 (mapcar #'(lambda (name)
		     (send text-item-proto :new (symbol-name name)))
		 (send parameters :names)))
   (setf (slot-value 'value-text-items)
	 (mapcar #'(lambda (value name)
		     (send edit-text-item-proto :new (format nil "~10g" value)
			   :text-length 15
			   :action 
			   #'(lambda (val)
			       (send self :value-t name val))))
		 (send parameters :values)
		 (send parameters :names)))
   (setf (slot-value 'update-button-items)
	 (mapcar #'(lambda (name)
		     (send button-item-proto :new
			   (format nil "Update ~A" name)
			   :action 
			   #'(lambda ()
			       (send self :update-button name))))
		 (send parameters :names)))
   (setf (slot-value 'lb-text-items)
	 (mapcar #'(lambda (range name)
		     (send edit-text-item-proto :new
			   (format nil "~g" (car range))
			   :text-length 5
			   :action 
			   #'(lambda (val)
			       (send self :lb name val))))
		 (send parameters :range)
		 (send parameters :names)))
   (setf (slot-value 'ub-text-items)
	 (mapcar #'(lambda (range name)
		     (send edit-text-item-proto :new
			   (format nil "~g" (cadr range))
			   :text-length 5
			   :action 
			   #'(lambda (val)
			       (send self :ub name val))))
		 (send parameters :range)
		 (send parameters :names)))
   (setf (slot-value 'slider-items)
	 (mapcar #'(lambda (value range name gran int?)
		     (send range-slider-proto :new
			   :range range
			   :value value
			   :granularity gran
			   :integer? int?
			   :action 
			   #'(lambda (val)
			       (send self :value-n name val))))
		 (send parameters :values)
		 (send parameters :range)
		 (send parameters :names)
		 (send parameters :granularity)
		 (send parameters :integer?)))
   
      
   ; now figure create the master dialog
   (apply #'call-next-method
	  (append other-items
		  (apply #'nconc
			 (mapcar #'(lambda (name value update lb slider ub)
				     (list (list name value update)
					   (list lb (send slider :scroll-item)
						 ub)))
				 (slot-value 'name-text-items)
				 (slot-value 'value-text-items)
				 (slot-value 'update-button-items)
				 (slot-value 'lb-text-items)
				 (slot-value 'slider-items)
				 (slot-value 'ub-text-items))))
	  :title title args)
   
   ;kludge to make sliders start in right position
   (mapc #'(lambda (slider value)
	     (send slider :value value))
	 (slot-value 'slider-items)
	 (send (slot-value 'paras) :values))
   ;return self
   self
   )


(send para-disp-proto :documentation :isnew
      "other-items (list of more dialog items)
Keywords:
  :supertool (parent tool object)
  :parameters (displayed parameter object)
  :title 
  :parent-signal (signal to send to parent when changing displayed
         object)
  :parent-signal-fun (function which handles local update request).
")

;; destruct -- destroys sub-objects then object itself
(defmeth para-disp-proto :destruct ()
   (setf (slot-value 'paras) nil)
   (setf (slot-value 'name-text-items) nil)
   (setf (slot-value 'value-text-items) nil)
   (setf (slot-value 'lb-text-items) nil)
   (setf (slot-value 'ub-text-items) nil)
   (mapc #'(lambda (slide)
		(send slide :dispose))
	(slot-value 'slider-items))
   (setf (slot-value 'slider-items) nil)
   (call-next-method))


;;; :update --- the general purpose update handler
;;; can use default value of this function.




;;; this is the dispatch function which handles the parent-signal
;;; dispatch.  
(defun para-disp-parent-signal (self &optional (signal :values)
				     &rest args)
  (let ((trace (find (send self :parent-signal) (trace))))
    (when trace (format t "Object ~S" self))
    (when (and (listp signal) (every #'numberp signal))
	  (push signal args) (setq signal :values))
    (if (eq signal :local)		;local set of some nature
	(let* ((signal (pop args))
	       (who (position signal (send (slot-value 'paras) :names))))
	  (when trace
		(format t "local-signal: signal:~S  args:~S who: ~S~%"
			signal args who))
	  (if who				;single value change
	      (case (car args)
		    ((:value :values)
		     (apply #'send self :values1 who :local (cdr args)))
		    (:range (apply #'send  self :range1
				   who :local (cdr args)))
		    ((:limit :limits)
		     (apply #'send  self :limits1 who :local (cdr args)))
		    (:granularity
		     (apply #'send  self :granularity1
			    who :local (cdr args)))
		    ((:name :names)
		     (apply #'send  self :names1
			    who :local (cdr args)))
		    (:integer?
		     (apply #'send self :integer?1 who :local args))
		    (t (apply #'send self :values1 who :local args)))
	    (if (and (listp signal) (every #'numberp signal))
		(apply #'send self :values :local args)
	      (apply #'send self signal :local args))))
    
    ;non-local set, send to parents
      (let ((set (if (member signal (send (slot-value 'paras) :names))
		     (or (> (list-length args) 1) (numberp (car args)))
		   args)))
	(when trace
	      (format t "parent-signal: signal:~S  set:~S args:~S ~%"
		      signal set args ))
	(if set
	    (apply #'send  self :supertool
		   (send self :parent-signal) signal args)
	  (apply #'send  (slot-value 'paras) signal args))))))



;;; set/select methods:

;; :parent-signal
(defmeth para-disp-proto :parent-signal (&optional (signal nil set)
						   (signal-fun
						    #'para-disp-parent-signal
						    psf-given))
   (when set
	 (when (and (slot-value 'parent-signal)
		    (send self :has-method (slot-value 'parent-signal)))
	       (unless psf-given
		       (setq signal-fun (send self :get-method
					      (slot-value 'parent-signal))))
	       (send self :remove-method (slot-value 'parent-signal)))
	 (setf (slot-value 'parent-signal) signal)
					; take care of local parameter updating
	 (send self :add-method signal signal-fun))
   (slot-value 'parent-signal))
  
(send para-disp-proto :documentation :parent-signal
      ":parent-signal &optional signal signal-fun

Returns/sets parent-signal.  

If setting, the new <signal> is made a method executing the
<signal-fun>.
")


;;; :action methods

;; :value-n changes named value
;; need to trap for the fact that setting the slider value with the
;; :value message cause the :action to happen.  Damn it Luke, why
;; don't you document these things instead of letting us find out the
;; hard way!!!
;; Should be fixed by use of range-slider item.
(defmeth para-disp-proto :value-n (name value)
  (send self :supertool (send self :parent-signal) name
	(round-if value (send (slot-value 'paras) name :integer?))))
	
(send para-disp-proto :documentation :value-n
      ":value-n name value

Changes named value.
")

;; :value-t changes named value, value sent as text
(defmeth para-disp-proto :value-t (name value)
  (send self :supertool (send self :parent-signal) name
	(round-if (read-string value)
		  (send (slot-value 'paras) name :integer?))))
  
(send para-disp-proto :documentation :value-t
      ":value-t name value

Changes value sent as string.
")

;; :lb changes lower bound of range
(defmeth para-disp-proto :lb (name value)
  (let ((oldrange (send (slot-value 'paras) name :range)))
      (send self :supertool (send self :parent-signal) name
	    :range (cons (round-if (read-string value)
				   (send (slot-value 'paras)
					 name :integer?))
			 (cdr oldrange)))))


(send para-disp-proto :documentation :lb
      ":lb name value

Changes lower bound of range sent as string.
")

	
;; :ub changes lower bound of range
(defmeth para-disp-proto :ub (name value)
  (let ((oldrange (send (slot-value 'paras) name :range)))
      (send self :supertool (send self :parent-signal) name :range
	    (list (car oldrange)
		  (round-if (read-string values)
			    (send (slot-value 'paras) name :integer?))))))

(send para-disp-proto :documentation :ub
      ":ub name value

Changes upper bound of range sent as string.
")


(defmeth para-disp-proto :update-button (name)
  (let* ((who (position name (send (slot-value 'paras) :names)))
	 (int? (send (slot-value 'paras) name :integer?))
	 (new-val (round-if (read-string
			     (send (nth who (slot-value 'value-text-items))
				   :text))
			     int?))
	 (oldrange (send (slot-value 'paras) name :range))
	 (newrange (list
		    (round-if
		     (read-string
		      (send (nth who (slot-value 'lb-text-items))
			    :text))
		     int?)
		    (round-if
		     (read-string
		      (send (nth who (slot-value 'ub-text-items))
			    :text))
		     int?))))
    (if (eql (send (slot-value 'paras) name) new-val)
	(send self :values1 who :local new-val)	;update display
      (send self :supertool (send self :parent-signal)
	    name :value new-val))
    (if (equalp oldrange newrange)
	(send self :range1 who :local newrange)	;update display
      (progn 
	(send self :supertool (send self :parent-signal)
	      name :range newrange)))))

(send para-disp-proto :documentation :update-button
      "Causes changed text display values to be read.
")



;;; parameter setting functions

(defmeth para-disp-proto :values (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((new-vals (send (slot-value 'paras) :values)))
	; use set values which should have been properly inherited
	(mapcar #'(lambda (value text-item slider-item)
		    (send text-item :text (format nil "~g" value))
		    (send slider-item :value value))
		new-vals (slot-value 'value-text-items)
		(slot-value 'slider-items)))
    (apply #'send self :supertool (send self :parent-signal)
	          :values args)))

(send para-disp-proto :documentation :values
      "[:local] Updates display, new values.
Otherwise follows update logic.
")


(defmeth para-disp-proto :values1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((value (send (slot-value 'paras) :values1 who)))
	(send (nth who (slot-value 'value-text-items))
	      :text (format nil "~g" value))
	(send (nth who (slot-value 'slider-items))
	      :value value))
    (apply #'send self :supertool (send self :parent-signal)
	   (nth who (send (slot-value 'paras) :names))
	   :value args)))


(send para-disp-proto :documentation :values1
      "[:local] Updates display, new values.
Otherwise follows update logic.
")


(defmeth para-disp-proto :range (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (prog1
	  (mapcar #'(lambda (range lb-item ub-item slider-item)
		      (send lb-text-item :text
			    (format nil "~g" (car range)))
		      (send ub-text-item :text
			    (format nil "~g" (cdr range)))
		      (send slider-item :range range))
		  (send (slot-value 'paras) :range)
		  (slot-value 'lb-text-items)
		  (slot-value 'ub-text-items)
		  (slot-value 'slider-items))
	(send self :values :local))
    (apply #'send self :supertool (send self :parent-signal)
	          :range args)))

(send para-disp-proto :documentation :range
      "[:local] Updates display, new range.
Otherwise follows update logic.
")


(defmeth para-disp-proto :range1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((range (send (slot-value 'paras) :range1 who)))
	(send (nth who (slot-value 'lb-text-items))
	      :text (format nil "~g" (car range)))
	(send (nth who (slot-value 'slider-items))
	      :range range)
	(send (nth who (slot-value 'ub-text-items))
	      :text (format nil "~g" (cadr range)))
	(send self :values1 who :local))
    (apply #'send self :supertool (send self :parent-signal)
	          (nth who (send (slot-value 'paras) :names))
		  :range args)))

(send para-disp-proto :documentation :range1
      "[:local] Updates display, new range.
Otherwise follows update logic.
")


(defmeth para-disp-proto :limits (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (send self :update :range (send (slot-value 'para) :range))
    (apply #'send self :supertool (send self :parent-signal)
	          :limits args)))

(send para-disp-proto :documentation :limits
      "[:local] Updates display, new limits (affects range).
Otherwise follows update logic.
")


(defmeth para-disp-proto :limits1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (send self :update :range1 who
	     (send (slot-value 'para) :range1 who))
    (apply #'send self :supertool (send self :parent-signal)
	          (nth who (send (slot-value 'paras) :names))
		  :range args)))


(send para-disp-proto :documentation :limits1
      "[:local] Updates display, new limits1 (affects range).
Otherwise follows update logic.
")


(defmeth para-disp-proto :granularity (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (mapcar #'(lambda (granularity slider-item)
		  (send slider-item :granularity granularity))
	      (cadr args) 
	      (slot-value 'slider-items))
    (apply #'send self :supertool (send self :parent-signal)
	          :granularity args)))

(send para-disp-proto :documentation :granularity
      "[:local] Updates display, new granularity.
Not Yet Implimented.
")


(defmeth para-disp-proto :granularity1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (send (nth who (slot-value 'slider-items))
	    :granularity (cadr args))
    (apply #'send self :supertool (send self :parent-signal)
	          (nth who (send (slot-value 'paras) :names))
		  :granularity args)))

(send para-disp-proto :documentation :granularity1
      "[:local] Updates display, new granularity.
Not Yet Implimented.
")

(defmeth para-disp-proto :integer? (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (mapcar #'(lambda (who int? val range slider-item)
		  (send slider-item :integer? int?)
		  (unless (eql val (send slider-item :value))
			  (send self :value1 who :local val))
		  (unless (eql range (send slider-item :range))
			  (send self :range1 who :local range)))
	      (iseq 0 (1- (length (send (slot-value 'paras) :names))))
	      (send (slot-value 'paras) :integer?)
	      (send (slot-value 'paras) :values)
	      (send (slot-value 'paras) :range)
	      (slot-value 'slider-items))
    (apply #'send self :supertool (send self :parent-signal)
	   :integer? args)))

(send para-disp-proto :documentation :integer?
      "[:local] Updates display, new integer? flag
Otherwise follows update logic.
")


(defmeth para-disp-proto :integer?1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((int? (send (slot-value 'paras) :integer?1 who))
	    (slider-item (nth who (slot-value 'slider-items)))
	    (val (nth who (send (slot-value 'paras)) :values))
	    (range (nth who (send (slot-value 'paras)) :range)) )
		  (send slider-item :integer? int?)
		  (unless (eql val (send slider-item :value))
			  (send self :value1 who :local val))
		  (unless (eql range (send slider-item :range))
			  (send self :range1 who :local range)))
    (apply #'send self :supertool (send self :parent-signal)
	   (nth who (send (slot-value 'paras) :names))
	   :integer? args)))

(send para-disp-proto :documentation :integer?1
      "[:local] Updates display, new integer? flag.
Otherwise follows update logic.
")


(defmeth para-disp-proto :names (&rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((new-vals (send (slot-value 'paras) :names)))
	; use set values which should have been properly inherited
	(mapcar #'(lambda (value text-item)
		    (send text-item :text (format nil "~g" value))
		new-vals (slot-value 'name-text-items))
		))
    (apply #'send self :supertool (send self :parent-signal)
	   :names args)))

(send para-disp-proto :documentation :names
      "[:local] Updates display, new parameter names.
Otherwise follows update logic.
")

(defmeth para-disp-proto :names1 (who &rest args)
  (if (and (consp args) (eql (car args) :local))
      (let ((new-val (send (slot-value 'paras) :names1 who)))
	; use set values which should have been properly inherited
	(send (nth who (slot-value 'name-text-items))
	      :text (format nil "~g" new-val)))
    (apply #'send self :supertool (send self :parent-signal)
	   :names args)))

(send para-disp-proto :documentation :names1
      "[:local] Updates display, new parameter names.
Otherwise follows update logic.
")


(new-provide :el-paradisp)
