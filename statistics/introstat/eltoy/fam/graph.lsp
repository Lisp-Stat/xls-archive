;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines a graph display
;;; tool. 

(require :el-paramixin (strcat ElToY-directory "params" *d-sep*  "paramixin.lsp"))
(require :el-family-mixin (strcat ElToY-directory "fam" *d-sep*  "family-mixin.lsp"))

;;; pdf-graph-proto --- graphical display of a continuous pdf
(defproto pdf-graph-proto 
  '(percents				;plotted percents
    quantiles				; quantiles
    density				; density values
    pdf-graph
    ) '() (list  inh-family-mixin inh-parameter-mixin
		 subtool-mixin titled-object)
"Displays a continuous distribution.
"
)

;; generic methods: :describe :print :isnew
(defmeth pdf-graph-proto :print (&optional (stream t))
  (format stream "#<pdf-graph: ~S (~S)>"
	  (send self :title)
	  (if (send self :family)
	      (send self :family :name))))

;; :describe method
(defmeth pdf-graph-proto :describe (&optional (stream t))
   (format stream "A distribution display for the ~S family~%"
	   (send (slot-value 'family) :name))
   (format stream "Current parameter estimates are:")
   (send self :parameters :describe stream nil)
   (format stream "Dispalying percents: ~S~%"
	   (slot-value 'percents))
   (call-next-method stream))


;;; :isnew --- winds up calling the next methods, eventually we should
;;; get the to scatterplot :is-new method.
;;; pdf-graph-proto --- graphical display of a continuous pdf
(defmeth pdf-graph-proto :isnew
         (&rest args
	  &key
	       (percents (rseq .01 .99 101))	;plotted percents
	       (title nil)
	  &allow-other-keys)
	  
   
  (apply #'call-next-method :title (if title title
				     (format nil "~S Distrubtion"
					     (send family :name)))
	 args)
  ; first the easy sets
  (setf (slot-value 'percents) percents)

  ; set quantiles and density
  (send self :recalculate)
  
  (setf (slot-value 'pdf-graph)
	(plot-lines (send self :quantiles) (send self :density)
		    :title (send self :title)
		    :variable-labels
		    (list (format nil "~S" (send self :rv-names))
			  "Probability Density")))

; return ourself
  self)

;; destruct -- destroys sub-objects then object itself
(defmeth pdf-graph-proto :destruct ()
  (setf (slot-value 'percents) nil)
  (setf (slot-value 'quantiles) nil)
  (setf (slot-value 'density) nil)
  (send (slot-value 'pdf-graph) :close)
  (setf (slot-value 'pdf-graph) nil)
  (call-next-method))

  

;;; :values --- meant to trap parameter value changes
(defmeth pdf-graph-proto :values (&optional set
				    &rest args)
   (if set
       (if (eql set :local)
	   (progn
	       (send self :recalculate)
	       (send self :redisplay))
	 (apply #'send self :supertool :parameters :values args))
     (send self :parameters :values)))
(defmeth pdf-graph-proto :values1 (who &optional set
				    &rest args)
   (if set
       (if (eql set :local)
	   (progn
	       (send self :recalculate)
	       (send self :redisplay))
	 (apply #'send self :supertool :parameters :values1 who set args))
     (send self :parameters :values1 who)))


;;; :rv-names

(defmeth pdf-graph-proto :rv-names (&optional set
				    &rest args)
   (if (and set (eql set :local))
       (send (slot-value 'pdf-graph) :variable-label 1
	                             (car args)))
   (apply #'call-next-method (if set (cons (set args))
			       nil)))


;;; :percents  -- simple set/selcect method

(defmeth pdf-graph-proto :percents (&optional (percents nil set))
   (when set
	 (setf (slot-value 'percents) percents)
	 (send self :update :values))
   (slot-value 'percents))
(send pdf-graph-proto :documentation :percents
      "Returns/sets the percentage points at which the distribution is
displayed.
")


;;; :quantiles :density --- quantiles and density are mainly select
;;; methods.  ANY set will force a call to recalculate and redisplay
;;; (through a call to values).

(defmeth pdf-graph-proto :quantiles (&optional (set nil))
   (when set (setf (slot-value 'quantiles))
	 (send self :calculate-density)
	 (send self :redisplay))
   (slot-value 'quantiles))
(send pdf-graph-proto :documentation :quantiles
      "Returns/sets the quantiles at which the distribution is
displayed.
")

(defmeth pdf-graph-proto :density (&optional (set nil))
   (when set (send self :recalculate)
	 (send self :redisplay))
   (slot-value 'density))
(send pdf-graph-proto :documentation :density
      "Returns the denesity values for displayed quantiles.
With arg forces recalculation first.
")


;;; :recalculate --- recalculates displayed points
(defmeth pdf-graph-proto :recalculate (&rest args)
  (let ((parameters (send self :parameters)))
    (setf (slot-value 'quantiles)
	  (send self :family :quantiles
		(slot-value 'percents) :parameters parameters))
    (setf (slot-value 'density)
	  (send self :family :density
		(slot-value 'quantiles) :parameters parameters))))
(send pdf-graph-proto :documentation :recalculate
      "Calculates new quantiles and density based on current
parameters values.
")

;;; :calculate-density --- recalculates density of displayed points
(defmeth pdf-graph-proto :calculate-density (&rest args)
  (let ((parameters (send self :parameters)))
    (setf (slot-value 'density)
	  (send self :family :density
		(send self :quantiles) :parameters parameters))))
(send pdf-graph-proto :documentation :calculate-density
      "Calculates new density for each quantile. 
")

    
;;; :redisplay --- displays the the current quantiles vs density
(defmeth pdf-graph-proto :redisplay (&rest args)
  (send (slot-value 'pdf-graph) :clear)
  (send (slot-value 'pdf-graph) :add-lines
	(send self :quantiles) (send self :density)))
(send pdf-graph-proto :documentation :redisplay
      "Updates display to reflect current values of quantiles and density.
")



;;; :update --- this mostly the bottom of a long signal passing chain.
;;; It only updates locally.  The most important signals are
;;; :parameters and :parameters :values.  These both generate a local
;;; :values message which updates the displayed values.
(defmeth pdf-graph-proto :update (&optional signal
				  &rest args)
  (if (eql signal :redisplay)
      (progn (send (slot-value 'pdf-graph) :update)
	     (send self :redisplay))
    (apply #'call-next-method signal args)))





  
;
;;;======================================================================
;;; Discrete Density Display
;;;======================================================================

;;; pmf-graph-proto --- graphical display of a discrete pmf
(defproto pmf-graph-proto 
  '(display-min-mass			;plotted percents
    displayed-atoms			; quantiles
    mass				; density values
    pmf-graph
    ) '() (list  inh-family-mixin inh-parameter-mixin titled-object)
"Displays a discrete distribution.
"
)

;; generic methods: :describe :print :isnew
(defmeth pmf-graph-proto :print (&optional (stream t))
  (format stream "#<pmf-graph: ~S (~S)>"
	  (send self :title)
	  (if (send self :family)
	      (send self :family :name))))

;; :describe method
(defmeth pmf-graph-proto :describe (&optional (stream t))
   (format stream "A discrete distribution display for the ~S family~%"
	   (send (slot-value 'family) :name))
   (format stream "Current parameter estimates are:")
   (send self :parameter :describe stream nil)
   (format stream "Dispalying atoms: ~S~%"
	   (slot-value 'atoms))
   (call-next-method stream))


;;; :isnew --- winds up calling the next methods, eventually we should
;;; get the to scatterplot :is-new method.
;;; pmf-graph-proto --- graphical display of a discrete pmf
(defmeth pmf-graph-proto :isnew
         (&rest args
	  &key
	       (family Undefined-Family)
	       (display-min-mass .001)	;plotted percents
	       (title nil)
	  &allow-other-keys)
	  
   
  (apply #'call-next-method :title (if title title
				     (format nil "~S Distrubtion"
					     (send family :name)))
	 args)
  ; first the easy sets
  (setf (slot-value 'display-min-mass) display-min-mass)

  ; set quantiles and density
  (send self :recalculate)
  
  (setf (slot-value 'pmf-graph)
	(plot-lines (send self :displayed-atoms) (send self :mass)
		    :title (send self :title)
		    :variable-labels
		    (list (format nil "~S" (send self :rv-names))
			  "Probability Mass")))
  (send self :redisplay)

; return ourself
  self)

;; destruct -- destroys sub-objects then object itself
(defmeth pmf-graph-proto :destruct ()
  (setf (slot-value 'display-min-mass) nil)
  (setf (slot-value 'displayed-atoms) nil)
  (setf (slot-value 'mass) nil)
  (send (slot-value 'pmf-graph) :close)
  (setf (slot-value 'pmf-graph) nil)
  (call-next-method))

  

;;; :values --- meant to trap parameter value changes
(defmeth pmf-graph-proto :values (&optional set
				    &rest args)
   (if set
       (if (eql set :local)
	   (progn
	       (send self :recalculate)
	       (send self :redisplay))
	 (apply #'send self :supertool :parameters :values args))
     (send self :parameters :values)))
(defmeth pmf-graph-proto :values1 (who &optional set
				       &rest args)
   (if set
       (if (eql set :local)
	   (progn
	       (send self :recalculate)
	       (send self :redisplay))
	 (apply #'send self :supertool :parameters :values1 who set args))
     (send self :parameters :values1 who)))


;;; :rv-names

(defmeth pmf-graph-proto :rv-names (&optional set
				    &rest args)
   (if (and set (eql set :local))
       (send (slot-value 'pmf-graph) :variable-label 1
	                             (car args)))
   (apply #'call-next-method (if set (cons (set args))
			       nil)))


;;; :display-min-mass  

(defmeth pmf-graph-proto :display-min-mass (&optional (percents nil set))
   (when set
	 (setf (slot-value 'display-min-mass) percents)
	 (send self :recalculate)
	 (send self :redisplay))
   (slot-value 'display-min-mass))
(send pmf-graph-proto :documentation :display-min-mass
      "Returns/sets the minimum mass used when selecting atoms for
display.
On set, :recalculate and :redisplay.
")


(defmeth pmf-graph-proto :displayed-atoms (&optional (set nil))
   (when set (setf (slot-value 'displayed-atoms) set)
	 (send self :calculate-mass)
	 (send self :redisplay))
   (slot-value 'displayed-atoms))
(send pmf-graph-proto :documentation :displayed-atoms
      "Returns/sets the list of displayed atoms.
On set, :calculate-mass and :redisplay.
")


(defmeth pmf-graph-proto :mass (&optional (set nil))
   (when set (send self :recalculate)
	 (send self :redisplay))
   (slot-value 'mass))
(send pmf-graph-proto :documentation :mass
      "Returns the denesity values for displayed quantiles.
With arg forces recalculation first.
")


;;; :recalculate --- recalculates displayed points
(defmeth pmf-graph-proto :recalculate (&rest args)
  (let* ((parameters (send self :parameters))
	 (display-min-mass (send self :display-min-mass))
	 (atoms (send self :family :atoms
		      :parameters parameters
		      :min-mass display-min-mass))
	 (mass (send self :family :mass atoms
		     :parameters parameters)))
    (setf (slot-value 'displayed-atoms)
	  (select atoms (which (< display-min-mass mass))))
    (setf (slot-value 'mass)
	  (select mass (which (< display-min-mass mass))))
    ))
(send pmf-graph-proto :documentation :recalculate
      "Calculates new atoms and mass based on current version of
parameters. 
")


;;; :calculate-mass --- recalculates mass of displayed points
(defmeth pmf-graph-proto :calculate-mass (&rest args)
  (let ((parameters (send self :parameters)))
    (setf (slot-value 'mass)
	  (send self :family :mass
		(send self :displayed-atoms)
		:parameters parameters))))
(send pmf-graph-proto :documentation :calculate-mass
      "Calculates new mass for each displayed atom. 
")

    
;;; :redisplay --- displays the the current quantiles vs density
(defmeth pmf-graph-proto :redisplay (&rest args)
  (send (slot-value 'pmf-graph) :clear)
  (send (slot-value 'pmf-graph) :add-d-lines
	(list (send self :displayed-atoms) (send self :mass))))


;;; :update --- this mostly the bottom of a long signal passing chain.
;;; It only updates locally.  The most important signals are
;;; :parameters and :parameters :values.  These both generate a local
;;; :values message which updates the displayed values.
(defmeth pmf-graph-proto :update (&optional signal
				  &rest args)
  (if (eql signal :redisplay)
      (progn (send (slot-value 'pmf-graph) :update)
	     (send self :redisplay))
    (apply #'call-next-method signal args)))


;
;;; The following addition to the family prototype chooses the type of
;;; display graph for that family

(defmeth discrete-family-proto :dist-graph-proto ()
  pmf-graph-proto)

(defmeth continuous-family-proto :dist-graph-proto ()
  pdf-graph-proto)



;
;;; The following patch to the scatterplot prototype should cause it
;;; to draw vertical lines in "d" mode.
  

(defmeth scatterplot-proto :add-d-lines (data)
  (let* ((X-back (cdr (reverse data)))
	 (Y (car (last data)))
	 (nvar (length Y))
	 (Y0 (repeat 0 nvar))
	 (data0 (reverse (cons Y0 X-back)))
	 (i1 (iseq nvar))		;linestart indexes
	 (i2 (+ nvar i1)))
    (send self :add-lines
	  (mapcar #'append data0 data) :draw nil)
    (send self :linestart-next i1 i2)
    (send self :linestart-next i2 nil)
    (send self :redraw)))
(send scatterplot-proto :documentation :add-d-lines
      "Adds lines from origin to each data point, 
where last variable in list is set to zero for the 'base' of the
line.   Similar is spirit to S type='d'.
")

    
	 

(new-provide :el-graph)
