;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the global prototype for an elicitation tool

(require :el-distdisp (strcat ElToY-directory "fam" *d-sep*  "distdisp.lsp"))
(require :el-data-link (strcat ElToY-directory "conj" *d-sep*  "data-link.lsp"))
(require :el-hyperparameters (strcat ElToY-directory "params" *d-sep*  "hyperparams.lsp"))

;;; el-tool --- primary elicitation tool
(defproto el-tool-proto 
  '(prior-tool				;prior display tool
    posterior-tool			;list of posterior tools
    control-dialog
    ) '() (list conjugate-family-mixin hyperparameter-mixin titled-object)
"The el-tool is the most generic elicitation tool available.  Some
non-obvious messages it accepts.
	:return -- kills child tools and returns parameter values
        :spawn-data-link -- spawns a new fictional data and posterior tool
"
)



;;;------- printing methods: :describe :print ---------------

; :print method
(defmeth el-tool-proto :print (&optional (stream t))
   (format stream "#<El-Tool: ~S (~S) ~S>"
	   (send self :title)
	   (if (slot-value 'conjugate-family)
	       (send (send self :conjugate-family) :name))
	   (if (slot-value 'hyperparameters)
	       (send (slot-value 'hyperparameters) :values))))

; :describe method
(defmeth el-tool-proto :describe (&optional (stream t))
   (format stream "An elicitation tool for the ~S--~S family~%"
	   (send (send self :prior-family) :name)
	   (send (send self :likelihood-family) :name))
   (format stream "Current parameter estimates are:")
   (send self :hyperparameter :describe stream nil)
   (format stream "Supports ~d data-posterior units~%~%"
	   (list-length (slot-value 'posterior-tool))))

;
;;;

   
; :isnew method
(defmeth el-tool-proto :isnew
	 (&rest args 
	  &key (conjugate-family undefined-conj-family)
	       (hyperparameter-values nil) (hyperparameter-names nil)
	       (hyperparameter-range nil) (hyperparameter-limits nil)
	       (hyperparameter-granularity nil)
	       (hyperparameter-integer? nil)
	       (hyperparameter-constraint-fun nil)
	       (parameter-names nil) (nui-parameter-names nil)
	       (data-names nil)
	       (title nil) (catch-return nil)
	       &allow-other-keys)

   ; zero out other tools
   (setf (slot-value 'prior-tool) nil)
   (setf (slot-value 'posterior-tool) '())


   ;set parameter names, defaults come from prior and likelihood
   ;families and are already set
   (if parameter-names
       (send conjugate-family :parameter-names parameter-names))
   (if nui-parameter-names
       (send conjugate-family :nui-parameter-names nui-parameter-names))
   (if data-names
       (send conjugate-family :data-names data-names))

   ;; recrusive call to pick up other sets
   (apply #'call-next-method
					;hyperparameter initialization
	:hyperparameter-names
	(if hyperparameter-names hyperparameter-names
	  (send conjugate-family :hyperparameter-names))
	:hyperparameter-values
	(if hyperparameter-values hyperparameter-values
	  (send conjugate-family :default-hyperparameters))
	:hyperparameter-range
	(if hyperparameter-range hyperparameter-range
	  (send conjugate-family :hyperparameter-range-default))
	:hyperparameter-limits
	(if hyperparameter-limits hyperparameter-limits
	  (send conjugate-family :hyperparameter-limits))
	:hyperparameter-granularity
	(if hyperparameter-granularity hyperparameter-granularity
	  (send conjugate-family :hyperparameter-granularity))
	:hyperparameter-integer?
	(if hyperparameter-integer? hyperparameter-integer?
	  (send conjugate-family :hyperparameter-integer?))
	:hyperparameter-constraint-fun
	(if hyperparameter-constraint-fun hyperparameter-constraint-fun
	  (send conjugate-family :hyperparameter-constraint-fun))
	
					;title
	:title
	(if title title
	  (format nil "~A Elicitation Tool" (send conjugate-family :name)))
	
					;anything else
	args)


   
   ;constants initialized now spawn tools

   (send self :spawn-control-dialog '() :catch-return catch-return)
   (send self :spawn-prior)
   (send self :spawn-data-link) 

   ;; return self
   self
   )

(send el-tool-proto :documentation :isnew
"(&rest args 
  &key :conjugate-family conjugate-family-instance
       :hyperparameter-names ---sets hyperpameter-names
       :hyperparameter-[values, range, limits, granularity, integer?,
		        constraint-fun] sets as appropriate
       :parameter-names :data-names sets appropriate names
       :nui-parameter-names nusance parameter names
       :title object title
       :catch-return if `t :modal-dialog-return executes a throw
  &allow-other-keys  passed on down
)
")


;; :destruct -- destroys sub-objects then object itself
(defmeth el-tool-proto :destruct ()
   (send (slot-value 'prior-tool) :destruct)
   (setf (slot-value 'prior-tool) nil)
   (mapcar #'(lambda (sub-tool) (send sub-tool :destruct))
	   (slot-value 'posterior-tool))
   (setf (slot-value 'posterior-tool) nil)
   (send (slot-value 'control-dialog) :destruct)
   (setf (slot-value 'control-dialog) nil)
   (call-next-method))

     
;; :return -- destroys the tool and returns a value
(defmeth el-tool-proto :return ()
   (let* ((values (send self :hyperparameters :values))
	  (dialog-string (format nil "Accept hyperparameters: ~S (~S)"
				 values (send self :hyperparameter-names)))
	  (okp (ok-or-cancel-dialog dialog-string t)))
     (unless okp (return nil))
     (send self :destruct)
     values))


	 


;;; :update messages

;; :update strategy works as follows  el-tool updates itself and then
;; sends an update message to each of its children.  

(defmeth el-tool-proto :update (signal &rest args)
  (apply #'call-next-method signal args)
  (let ((trace (find :update (trace))))
  (mapcar #'(lambda (sub-tool)
	      (unless (null sub-tool)
		      (if trace
			  (format t ":update (el-tool-proto) send to subtool ~S~%"
				  sub-tool))
		      (apply #'send  sub-tool :update signal args)))
	  (list* (slot-value 'control-dialog)
		 (slot-value 'prior-tool)
		 (slot-value 'posterior-tool)))))



;
;;; graphical subobjects: :spawn-prior :span-data-link :spawn-control-dialog

(defmeth el-tool-proto :spawn-control-dialog (other-items &rest args
							  &allow-other-keys)
  (let ((spawn-data-link-button
	 (send button-item-proto :new "Spawn New Data Link"
	       :action #'(lambda () (send self :spawn-data-link))))
	(return-button
	 (send modal-button-proto :new "Return Values and Exit"
	       :action #'(lambda () (send self :return)))))
    (setf (slot-value 'control-dialog)
	  (apply #'send el-tool-dialog-proto :new
		 (list* spawn-data-link-button return-button other-items)
		 :title (send self :title)
		 args))))



(defmeth el-tool-proto :spawn-prior ()
  (setf (slot-value 'prior-tool)
	(send pri-post-disp-proto :new
	      :supertool self
	      :family (send self :prior-family)
	      :parameters (send self :hyperparameter-object)
	      :title (format nil "Prior: ~A"
			     (send self :title))
	      :parent-signal :hyperparameters
	      :conjugate-family (send self :conjugate-family))
	))

(defmeth el-tool-proto :spawn-data-link ()
  (push (send data-link-proto :new
	      :supertool self
	      :prior-parameters (send self :hyperparameter-object)
	      :title (format nil "Data-Link: ~A"
			     (send self :title))
	      :conjugate-family (send self :conjugate-family))
	(slot-value 'posterior-tool)))



;
;;;------------------------------------------------------------------------
;;; el-tool-dialog-proto
;;;------------------------------------------------------------------------

(defproto el-tool-dialog-proto `(catch-return) () (list ElToY-dialog-proto))

(defmeth el-tool-dialog-proto :isnew (items &rest args &key (catch-return nil)
					    &allow-other-keys)
  (setf (slot-value 'catch-return) catch-return)
  (apply #'call-next-method items args))

(defmeth el-tool-dialog-proto :modal-dialog-return (values)
  (if (slot-value 'catch-return)
      (throw 'ElToY-return values)
    (format t "ElToY-values: ~S~%" values)))



(new-provide :el-tool)




