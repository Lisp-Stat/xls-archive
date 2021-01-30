;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy and use this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


(require :el-conj-mixin (strcat ElToY-directory "conj" *d-sep*  "conj-mixin"))
(require :el-ppdistdisp (strcat ElToY-directory "conj" *d-sep*  "ppdistdisp"))
(require :el-data-disp (strcat ElToY-directory "params" *d-sep*  "data-disp"))

;;; The data-link joins the el-tool to a posterior distribution while
;;; connecting it to data

(defproto data-link-proto
  '(					;parent el-tool
    prior-parameters			;prior distribution parameters
    posterior-parameters			;the parameters a posteriori
    data-display			;display of the data
    posterior-display			;display of the posterior
					;distribution
    link-dialog				;dialog explaining link
    ) '() (list el-data-mixin nui-parameter-mixin
		inh-conjugate-family-mixin subtool-mixin titled-object)
"This object links an el-tool to a posterior distribution"
)      


;;; :describe & :print

; :print method
(defmeth data-link-proto :print (&optional (stream t))
   (format stream "#<Data-Link: ~A (~S--~S) ~S>"
	   (send self :title)
	   (if (slot-value 'conjugate-family)
	       (send (send self :prior-family) :name)
	     nil)
	   (if (slot-value 'conjugate-family)
	       (send (send self :likelihood-family) :name)
	     nil)
	   	   (if (slot-value 'data)
	       (send self :data :names)
	     nil)))

; :describe method
(defmeth data-link-proto :describe (&optional (stream t) &rest args)
   (format stream "~A: A data-link for the ~S--~S family~%"
	   (send self :title)
	   (send (send self :prior-family) :name)
	   (send (send self :likelihood-family) :name))
   (format stream "Current data are:")
   (send self :data :describe stream nil))



;;; :isnew

(defmeth data-link-proto :isnew
	 (&rest args 
	  &key (prior-parameters nil)
	       conjugate-family
	       (title (gensym 'data-link))
	       nui-parameter-names nui-parameter-values
	       nui-parameter-range nui-parameter-limits
	       nui-parameter-granularity nui-parameter-integer?
	       nui-parameter-constraint-fun 
	       data-names data-values data-limits data-range
	       data-integer? data-constraint-fun 
	       &allow-other-keys)

  
  ; capture inherited info
  (apply #'call-next-method :title title

					;nui-parameter initialization
	:nui-parameter-names
	(if nui-parameter-names nui-parameter-names
	  (send conjugate-family :nui-parameter-names))
	:nui-parameter-values
	(if nui-parameter-values nui-parameter-values
	  (send conjugate-family :nui-default-parameters))
	:nui-parameter-range
	(if nui-parameter-range nui-parameter-range
	  (send conjugate-family :nui-parameter-range-default))
	:nui-parameter-limits
	(if nui-parameter-limits nui-parameter-limits
	  (send conjugate-family :nui-parameter-limits))
	:nui-parameter-granularity
	(if nui-parameter-granularity nui-parameter-granularity
	  (send conjugate-family :nui-parameter-granularity))
	:nui-parameter-integer?
	(if nui-parameter-integer? nui-parameter-integer?
	  (send conjugate-family :nui-parameter-integer?))
	:nui-parameter-constraint-fun
	(if nui-parameter-constraint-fun nui-parameter-constraint-fun
	  (send conjugate-family :nui-parameter-constraint-fun))


					;data initial values
	:data-names
	(if data-names data-names
	  (send conjugate-family :data-names))
	:data-values
	(if data-values data-values
	  (send conjugate-family :default-data))
	:data-range
	(if data-range data-range
	  (send conjugate-family :data-range-default))
	:data-limits
	(if data-limits data-limits
	  (send conjugate-family :data-limits))
	:data-integer?
	(if data-integer? data-integer?
	  (send conjugate-family :data-integer?))
	:data-constraint-fun
	(if data-constraint-fun data-constraint-fun
	  (send conjugate-family :data-constraint-fun))
	args)

  (setf (slot-value 'prior-parameters) prior-parameters)

  ;set posterior hyperparameters
  (setf (slot-value 'posterior-parameters)
	(send para-proto :new
	      :names
	      (send self :prior-parameters :names)
	      :values
	      (send self :forward-link)
	      :limits
	      (send self :prior-parameters :limits)
	      :range
	      (send self :prior-parameters :range)
	      :integer?
	      (send self :prior-parameters :integer?)
	      :constraint-fun
	      (send self :prior-parameters :constraint-fun)
	      :granularity
	      (send self :prior-parameters :granularity)))


  ; spawn displays
  (setf (slot-value 'data-display)
	(send data-nui-para-disp-proto :new '()
	      :supertool self
	      :data (send self :data-object)
	      :parameters (send self :nui-parameter-object)
	      :parent-signal :nui-parameters))
	      
  (setf (slot-value 'posterior-display)
	(send pri-post-disp-proto :new
	      :supertool self
	      :family (send self :prior-family)
	      :parameters (send self :posterior-parameter-object)
	      :title (format nil "Posterior: ~A"
			     (send self :supertool :title))
	      :parent-signal :posterior-parameters
	      :conjugate-family (send self :conjugate-family)))

  (setf (slot-value 'link-dialog)
	(send link-dialog-proto :new '() :title title))

  ; link first values
  (send self :update :posterior-parameters (send self :forward-link))

  ; return self
  self
)

(send data-link-proto :documentation :isnew
      (strcat 
  "Data-link-proto :isnew 
               :prior-parameters --- prior parameter object
"
  (send el-data-mixin :documentation :isnew)
  (send nui-parameter-mixin :documentation :isnew)
  (send subtool-mixin :documentation :isnew)
  (send titled-object :documentation :isnew)
  ))



;; destruct -- destroys sub-objects then object itself
(defmeth data-link-proto :destruct ()
   (send (slot-value 'data-display) :destruct)
   (setf (slot-value 'data-display) nil)
   (send (slot-value 'posterior-display) :destruct)
   (setf (slot-value 'posterior-display) nil)
   (send (slot-value 'posterior-parameters) :destruct)
   (setf (slot-value 'posterior-parameters) nil)
   (setf (slot-value 'prior-parameters) nil)
   (send (slot-value 'link-dialog) :destruct)
   (setf (slot-value 'posterior-parameters) nil)
   (call-next-method))



;;; :update

(defmeth data-link-proto :update (signal &rest args)
  (apply #'call-next-method signal args)
  (when (slot-value 'link-dialog)
	(if (find :update (trace))
	    (format t ":update (data-link) send to link-dialog~%"))
	(send (slot-value 'link-dialog) :update signal))
  (when (slot-value 'data-display)
	(if (find :update (trace))
	    (format t ":update (data-link) send to data-display~%"))
	(apply #'send (slot-value 'data-display) :update signal args))
  (if (or (eql signal :prior-parameters) (eql signal :hyperparameters)
	  (eql signal :data) (eql signal :nui-parameters))
      (send self :update :posterior-parameters
	    (send self :forward-link)))
  (when (slot-value 'posterior-parameters)
	(if (find :update (trace))
	    (format t ":update (data-link) send to posterior-parameters~%"))
	(apply #'send (slot-value 'posterior-parameters) :update signal args))
  (when (slot-value 'posterior-display)
	(if (find :update (trace))
	    (format t ":update (data-link) send to posterior-display~%"))
	(apply #'send (slot-value 'posterior-display) :update signal args))
  )


(send data-link-proto :documentation :update
      (strcat 
" data-link, on :update, converts :prior-parameter signals to
:posterior-parameter signals.

"
      (send nui-parameter-mixin :documentation :update)))

  

;;; :parameters

(defmeth data-link-proto :hyperparameter-names
  	 (&optional (hyperparameter-names nil set))
   (when set (send self :prior-parameters :names hyperparameter-names))
   (when set (send self :posterior-parameters :names hyperparameter-names))
   (send (slot-value 'prior-parameters) :names))


;; hyperparameter methods: :hyperparameters (:value :rescale :limits
;; :describe :names :range :granularity)
;; signal could be :local, in which case the rest of the message is
;; interpreted as a set to the hyperparameters, one of the valid
;; signals for the hyperparameter or omitted in which case it defaults
;; to values



;; furthermore signal could come to either prior or posterior
;; hyperparameters.  If it comes to prior, change must be made
;; globally, if it comes to posterior, change must be made to prior
;; which must be made globally.


;; prior--parameter methods --- non-local changes are sent to the
;; parent as an update message.  local changes are propagated by
;; sending an to the derived posterior-parameters via a forward-link

(defmeth data-link-proto :prior-parameters (&optional signal &rest args)
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set of some nature
      (progn
	(when (find :prior-parameters (trace))
	      (format t ":local---do nothing~%"))
;	(apply #'send  self :update :posterior-parameters
;	       :values (send self :forward-link))
	)
    (let ((set (if (member signal (send self :hyperparameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 args)))
      (if set
	  (progn
	    (when (find :prior-parameters (trace))
		  (format t "set---send to el-tool as :hyperparameters ~S ~S~%"
			  signal args))
	    (apply #'send  self :supertool
		   :hyperparameters signal args))
	(progn
	  (when (find :prior-parameters (trace))
		  (format t "information request---pass toprior-parameters ~S ~S~%"
			  signal args))
	  (if signal 
	      (apply #'send  (slot-value 'prior-parameters) signal args)
	    (send  (slot-value 'prior-parameters) :values)))))))

(send data-link-proto :documentation :prior-parameters
      "(&optional signal &rest args)
Sends signal with args to prior-parameters.  Follows update mechanism
which should update posterior parameters via forward link.
")



;; posterior--parameter methods --- non-local changes are sent to the
;; prior-parameters via a reverse-link.  local changes are passed to
;; the  posterior-parameters.

(defmeth data-link-proto :posterior-parameters
  	(&optional signal &rest args )
  (when (and (listp signal) (every #'numberp signal))
	(push signal args) (setq signal :values))
  (if (eq signal :local)		;local set, probably from
					;:prior-parameters
      (let ((signal (car args))
	    (rargs (cdr args)))
	(when (and (listp signal) (every #'numberp signal))
	      (push signal rargs) (setq signal :values))
	(when (find :posterior-parameters (trace))
	      (format t ":local---send to posterior-parameters ~S ~S~%"
		      signal rargs))
	(apply #'send  (slot-value 'posterior-parameters) signal rargs))
    (let ((set (if (member signal (send self :hyperparameter-names))
		   (or (> (list-length args) 1) (numberp (car args)))
		 args)))
      (if set
	  ;; change something and see what happens
	  (let* ((return-values
		  (apply #'send self :update :posterior-parameters
			 signal args)) 
		 )
	    (when (find :posterior-parameters (trace))
		  (format t "send to :hyperparameters via reverse link~%"))
	    (send self :supertool :hyperparameters :values
		  (send self :reverse-link))
	    return-values)
	(progn
	  (when (find :posterior-parameters (trace))
		(format t "inforamtion request: send to params ~S ~S~%"
			signal args))
	  (if signal
	      (apply #'send  (slot-value 'posterior-parameters) signal args)
	    (send (slot-value 'posterior-parameters) :values)))))))

(send data-link-proto :documentation :prior-parameters
      "(&optional signal &rest args)
Sends signal with args to posterior-parameters.  When setting, if
values change, send :hyperparameters method to parent getting values
through reverse-link.
")


;;; ---------  Parameter Object Methods ---------------------------

;;; :prior-parameters
(defmeth data-link-proto :prior-parameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'prior-parameters) (cadr args))
	(apply #'send self :supertool :prior-parameter-object args))
    (slot-value 'prior-parameters)))


(send data-link-proto :documentation :prior-parameter-object
      "Returns/updates (sets in parent) actual prior-parameter object")




;;; :posterior-parameters

(defmeth data-link-proto :posterior-parameter-object (&rest args)
  (if args
      (if (eql :local (car args))
	  (setf (slot-value 'posterior-parameters) (cadr args))
	(apply #'send self :update :posterior-parameter-object args))
    (slot-value 'posterior-parameters)))

(send data-link-proto :documentation :posterior-parameter-object
      "Returns/updates (sets with :local) actual parameter object")






;;; Link-functions

;;; :forward-link --- returns posterior parameters given current value
;;; of prior-parameters, data and nui-sance parameters

(defmeth data-link-proto :forward-link (&rest args)
  (send (slot-value 'conjugate-family) :forward-link
	:parameters (send self :prior-parameters)
	:nui-parameters (send self :nui-parameters)
	:data (send self :data)))


;;; :reverse-link --- returns prior parameters given current values of
;;; other info

(defmeth data-link-proto :reverse-link (&rest args)
  (send (slot-value 'conjugate-family) :reverse-link
	:parameters (send self :posterior-parameters)
	:nui-parameters (send self :nui-parameters)
	:data (send self :data)))



;;;------------------------------------------------------------------------
;;; link-dialog-proto
;;;------------------------------------------------------------------------

;;; This dialog is meant to keep a running track of what is going on.
;;; It would also serve as a useful place to hang buttons.

(defproto link-dialog-proto
  '(item-list				;list of text items
    last-signal				;flag for catching echo calls
    )
  () (list ElToY-dialog-proto)
  "Display/Control Box for data-link"
  )
	

;;; ----------------  Birth and Death  -----------------------

;;; :isnew

(defmeth link-dialog-proto :isnew (other-items &rest args
					       &key
					       (text-length 65)
					       (num-items 5)
					       &allow-other-keys)
  (let ((text-items nil))
    (setf (slot-value 'item-list)
	  (dotimes (i num-items text-items)
	     (push (send text-item-proto :new  ""
			 :text-length text-length)
		   text-items)))
    (send (first text-items) :text
	  "Data \\_/-\\_/ Link")
    (send (fourth text-items) :text
	  "Changes in prior are linked to posterior")
    (send (fifth text-items) :text
	  "Changes in data and nuisance parameters are linked to posterior")
    (apply #'call-next-method (nconc text-items other-items) args)))
		      

(send link-dialog-proto :documentation :isnew
      "args: other-items &rest args
             &key (text-length num-items title) &allow-other-keys

Args are passed to dialog-proto.
")


;;; :destruct
(defmeth link-dialog-proto :destruct ()
  (setf (slot-value 'item-list) nil)
  (call-next-method))



;;; -------- :update and :parameter methods  ----------------
  
(defmeth link-dialog-proto :update (signal &rest args)
  (send self :monitor-update signal args)
  (if (send self :has-method signal)
	(send self signal args)
    )
  )



(send link-dialog-proto :documentation :update
      "Does not follow normal update logic.
Instead it traps signals :prior-parameters :posterior-parameters 
:nui-parameters  and :data.
")

(defmeth link-dialog-proto :monitor-update (signal &rest args)
  )

(send link-dialog-proto :documentation :monitor-update
      "Does nothing, but tracing should reveal pattern of messages
through data link.
")

(defmeth link-dialog-proto :prior-parameters (&rest args)
  (case (slot-value 'last-signal)
	(:backward-link nil)
	((:hyperparameters :posterior-parameters nil)
	 (send (second (slot-value 'item-list))
	       :text "Prior Parameters have changed")
	 (send (third (slot-value 'item-list))
	       :text "Updating Posterior"))
	(t (warn "link-dialog::Weird signal sequence ~S -- ~S~%"
		  (slot-value 'last-signal) :prior-parameters)))
  (setf (slot-value 'last-signal) :prior-parameters))

(defmeth link-dialog-proto :hyperparameters (&rest args)
  (case (slot-value 'last-signal)
	(:backward-link nil)
	((:hyperparameters :posterior-parameters nil)
	 (send (second (slot-value 'item-list))
	       :text "Prior Parameters have changed")
	 (send (third (slot-value 'item-list))
	       :text "Updating Posterior"))
	(t (warn "link-dialog::Weird signal sequence ~S -- ~S~%"
		  (slot-value 'last-signal) :prior-parameters)))
  (setf (slot-value 'last-signal) :hyperparameters))





(defmeth link-dialog-proto :posterior-parameters (&rest args)
  (case (slot-value 'last-signal)
	((:backward-link :data :nui-parameters :hyperparameters
	  :prior-parameters nil)
	 (setf (slot-value 'last-signal) :posterior-parameters))
	((:posterior-parameters)
					;two :post-parameter signals
					;in a row, must be a reverse
					;link.
	  (send (second (slot-value 'item-list))
		:text "Posterior parameters changed:")
	  (send (third (slot-value 'item-list))
		:text "Changing prior parameters.")
	  (setf (slot-value 'last-signal) :backward-link))
	(t (warn "link-dialog::Weird signal sequence ~S -- ~S~%"
		  (slot-value 'last-signal) :posterior-parameters)
	   (setf (slot-value 'last-signal) :posterior-parameters))))

		      
(defmeth link-dialog-proto :data (&rest args)
  (case (slot-value 'last-signal)
	((:hyperparameters :posterior-parameters nil)
	 (send (second (slot-value 'item-list))
	       :text "Data have changed")
	 (send (third (slot-value 'item-list))
	       :text "Updating Posterior"))
	(t (warn "link-dialog::Weird signal sequence ~S -- ~S~%"
		  (slot-value 'last-signal) :data)))
  (setf (slot-value 'last-signal) :prior-parameters))

(defmeth link-dialog-proto :nui-parameters (&rest args)
  (case (slot-value 'last-signal)
	((:hyperparameters :posterior-parameters nil)
	 (send (second (slot-value 'item-list))
	       :text "Nuisance Parameters have changed")
	 (send (third (slot-value 'item-list))
	       :text "Updating Posterior"))
	(t (warn "link-dialog::Weird signal sequence ~S -- ~S~%"
		  (slot-value 'last-signal) :nui-parameters)))
  (setf (slot-value 'last-signal) :prior-parameters))

	 




(new-provide :el-data-link)

