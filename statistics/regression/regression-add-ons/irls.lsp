;;;;
;;;;	Nicola Chapman STA 450S
;;;;
;;;;

(provide "IRLS")
(require "statistics")
(require "graphics")
(require "help")

;;;;
;;;;	IRLS Regression Model Prototype
;;;;

(defproto IRLS-proto
	'(x y intercept weights wf-name predictor-names response-name 
	  tol count-limit se-estimates coef-estimates boot-iterations)
	()
	*object*
	"IRLS Regression Model")

(defun IRLS-model (x y &key (intercept t)
                              (print t)
                              (weights (repeaT 1 (length y)))
                              (wf-namE 'huber)
                              (tol .0001)
			      (count-limit 50)
			      (boot-iterations 400)
			      predictor-names
                              response-name)
"Args: (x y &key (intercept t) (print t) (weights 1,1,...1) (wf-name 'huber)
                 (tol 0.0001) (count-limit 20) predictor-names response-name)
X	    -list of independent variables or X matrix
Y	    -dependent variable
intercept   -T to include (default), nil for no intercept
print	    -to display summary info, nil not to
weights	    -inital weights
wf-name     -name of weight function to be used (CAUCHY, HUBER or BIWEIGHT)
tol	    -tolerance to be used in determining when to terminate iteration
count-limit -similar to tol, gives max. # of iterations that will be done
PREDICTOR-NAMES
RESPONSE-NAME
Returns an IRLS regression model. To examine the model further, assign the
result to a variable and send it messages."
	(let ((m (send IRLS-proto :new))
	      (x (cond
		   ((matrixp x) x)
		   ((vectorp x) (list x))
	 	   ((and (consp x) (numberp (car x))) (list x))
                   (t x))))
           (send m :x (if (matrixp x) x (apply #'bind-columns x)))
	   (send m :y y)
	   (send m :intercept intercept)
           (send m :weights weights)
	   (send m :wf-namE  wf-namE)
	   (send m :tol tol)
           (send m :count-limit count-limiT)
	   (send m :predictor-names predictor-names)
           (send m :response-name response-name)
	   (send m :boot-iterations boot-iterations)
	   (if print (send m :display))
	   m))

(defmeth IRLS-proto :isnew () (send self :needs-computing t))

(defmeth IRLS-proto :save ()
"Message args: ()
Returns an expression that will reconstruct the model."
   '(IRLS-model ',(send self :x)
                  ',(send self :y)
                  :intercept ',(send self :intercept)
                  :wf-name ',(send self :wf-name)
                  :tol ',(send self :tol)
		  :count-limit ',(send self :count-limit)
		  :predictor-names ',(send self :predictor-names)
		  :response-name ',(send self :response-name)))


;;;
;;;	Computing and Display Methods
;;;

(defmeth IRLS-proto :compute ()
"Message args: ()
Recomputes the estimates. For internal use by other messages."
	(let ((x (send self :x))
              (y (send self :y))
              (weights (send self :weights))
              (tol (send self :tol))
              (count-limit (send self :count-limit))
              (wf (make-wf (send self :wf-name)))
              (intercept (send self :intercept)))
	(send self :coef-estimates
	    (IRLS-regression x y :intercept intercept
	                         :weights weights
 				 :tol tol
				 :count-limit count-limit
				 :wf wf))
	(when (ok-or-cancel-dialog (format nil
			 "Calculate estimates of standard error?"))
		(setf current (send self :boot-iterations))
		(setf change-to 
		    (first (get-value-dialog "Number of bootstrap iterates?"
				         :initial current)))
		(send self :boot-iterations change-to)
                (send self :se-estimates
	        	(send self :calculate-se-estimates change-to))) ))
	
(defun IRLS-regression (x y &key intercept 
 	                         weights
				 tol
			         count-limit 
				 wf )
"Args (x y intercept weights tol count-limit wf	)
Used internally to compute the coefficient estimates, called iteratively to
get a bootstrap estimate of standard error. The user should be using the
function IRLS-model to create a model, not this message,which only returns
coefficient estimates."
          (labels ((as-list (x) (coerce (compound-data-seq x) 'list))
                   (rel-err (x y)
                        (mean (/ (abs (- x y)) (+ 1 (abs x)))))
		   (reg-coefs (weights)
		         (let* ((m (make-sweep-matrix x y weights))
                                (p (array-dimension x 1)))
                            (as-list
                                (select (first (sweep-operator m
                                               (iseq (if intercept 1 0) p)))
					 (+ 1 p)
                                         (iseq (if intercept 0 1) p)))))
                   (fit-vals (beta)
                           (if intercept (+ (first beta)
				 (matmult x (rest beta)))
                              (matmult x beta)))
		   (improve-guess (beta)
			   (let* ((resids (- y (fit-vals beta)))
                                   (scale (/ (median (abs resids)) .6745))
                                   (wts (funcall wf (/ resids scale))))
                               (reg-coefs wts)))
                   (good-enough-p (last beta count)
                            (or (> count count-limit)
                                (and last (< (rel-err beta last) tol)))))
           (do ((last nil beta)
                (count 0 (+ count 1))
                (beta (reg-coefs weights) (improve-guess beta)))
               ((good-enough-p last beta count) beta))))
 

(defmeth IRLS-proto :needs-computing (&optional set)
	(if set (setf (slot-value 'coef-estimates) nil))
	(null (slot-value 'coef-estimateS) ))

(defmeth IRLS-proto :display ()
"Message args: ()
Prints the IRLS regression summary. Lists coefficient estimates and standard
errors (if calculated) and shows model parameters."
	(let ((coefs (coerce (send self :coef-estimates) 'list))
	      (se-s (send self :se-estimates))
              (x (send self :x))
	      (p-names (send self :predictor-names)))
           (format t "~% IRLS Coefficent Estimates (standard errors): ~2%")
	   (when (send self :intercept)
		(format t "Constant             ~10g       ~A~%"
		     (car coefs) (list (car se-s)) )
	   	(setf coefs (cdr coefs))
		(setf se-s (cdr se-s)))
	   (dotimes (i (array-dimension x 1))
	       (format t "~22a ~10g     ~A~%"
		   (select p-names i) (car coefs) (list (car se-s)))
	   	(setf coefs (cdr coefs))
		(setF se-s (cdr se-s)))
	   (format t "~%Weight Function:       ~a~%" (send self :wf-name))
	   (format t "Tolerance:             ~G~%" (send self :tol))
	   (format t "Iteration Limit:       ~G~%" (send self :count-limit)) 
	   (format t "Standard errors (if requested) were based on ~G bootstrap iterates. ~2%"
                          (send self :boot-iterationS)) ))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth IRLS-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument 
NEW-X sets the x matrix to new-x and recomputes the estimates."
	(when (and new-x (matrixp new-x))
              (setf (slot-value 'x) new-x)
              (send self :needs-computing t))
        (slot-value 'x))

(defmeth IRLS-proto :y (&optional new-y)
"Message args: (&optional new-y)
With no argument returns the y sequence as supplied to m. With an argument
NEW-Y sets the y sequence to NEW-Y and recomputes the estimates."
	(when (and new-y (sequencep new-y))
              (setf (slot-value 'y) new-y)
              (send self :needs-computing  t))
     	(slot-value 'y))

(defmeth IRLS-proto :intercept (&optional (val nil set))
"Message args: (&optional new-intercept)
With no argument returns t if the model includes an intercept term, nil if 
not. With an argument NEW-INTERCEPT the model is changed to include or exclude
an intercept, according to the value of NEW-INTERCEPT."
	(when set
	     (setf (slot-value 'intercept) val)
 	     (send self :needs-computing t))
        (slot-value 'intercept))

(defmeth IRLS-proto :weights (&optional (new-w nil set))
"Message args: (&optional new-w)
With no argument returns the original weight sequence as supplied to m;
NEW-W sets the original weight sequence to NEW-W and recomputes..."
	(when set
	     (setf (slot-value 'weights) new-w)
             (send self :needs-computing t))
	(slot-value 'weights))

(defmeth IRLS-proto :predictor-names (&optional (names nil set))
"Message args: (&optional names)
With no argument returns the predictor names. NAMES sets the names."
 	(if set (setf (slot-value 'predictor-names) (mapcar #'string names)))
        (leT ((p (array-dimension (send self :x) 1))
              (p-names (slot-value 'predictor-names)))
	   (if (not (and p-names (= (length p-names) p)))
	       (setf (slot-value 'predictor-names)
                      (mapcar #'(lambda (a) (format nil "Variable ~a" a))
                                  (iseq 0 (- p 1))))))
    	(slot-value 'predictor-names))

(defmeth IRLS-proto :response-name (&optional (name "Y" set))
"Message args: (&optional name)
With no argument, returns the response name. NAME sets the name."
	(if set (setf (slot-value 'response-name) (if name (string name) "Y")))
	(slot-value 'response-name))

(defmeth IRLS-proto :tol (&optional new-tol)
"Message args: (&optional new-tol)
With no argument, returns the value of the tolerance parameter. Otherwise, 
resets tolerance parameter to NEW-TOL and recomputes estimates."
	(when (and new-tol (floatp new-tol))
	              (setf (slot-value 'tol) new-tol)
                      (send self :needs-computing t))
        (slot-value 'tol))

(defmeth IRLS-proto :count-limit (&optional new-limit)
"Message args: (&optional new-limit)
With no argument, returns the value of the iteration limit parameter.
Otherwise, resets the iteration limit parameter to NEW-LIMIT and recomputes
estimates."
	(when (and new-limit (integerp new-limit))
              (setf (slot-value 'count-limit) new-limit)
	      (send self :needs-computing t))
	(slot-value 'count-limit))

(defmeth IRLS-proto :coef-estimates (&optional new-ests)
"Message args: (&optional new-ests)
With no argument, returns the values of the coefficent estimates for
the last model fitted. When NEW-ESTS is supplied, the stored value
of the estimates is changed to NEW-ESTS. THIS OPTIONAL ARGUMENT IS
INTENDED FOR INTERNAL USE ONLY!!!"
	(when new-ests
		(setf (slot-value 'coef-estimates) new-ests))
	(if (send self :needs-computing) (send self :compute))  
        (slot-value 'coef-estimates))

(defmeth IRLS-proto :se-estimates (&optional new-ests)
"Message args: (&optional new-ests)
With no argument, returns the last calculated bootstrap estimation
of the coefficient estimates, as long as the errors were requested
at that time. If they were not, it returns nil. Again, THE OPTIONAL
ARGUMENT IS INTENDED FOR INTERNAL USE ONLY!!"
	(when new-ests
		(setf (slot-value 'se-estimates) new-ests))
	(slot-value 'se-estimates))

(defmeth IRLS-proto :wf-namE (&optional new-wf-name)
"Message args: (&optional new-wf-name)
With no argument, returns the name of the weight function presently
being used in the re-weighting scheme. When NEW-WF-NAME is supplied, 
that weight function is substituted, and the coefficients are re-
calculated. (NEW-WF-NAME must be HUBER, CAUCHY or BIWEIGHT)"
	(when new-wf-name
	      (setf (slot-value 'wf-name) new-wf-name)
	      (send self :needs-computing t))
	(slot-value 'wf-name))

(defmeth IRLS-proto :boot-iterations (&optional new-it)
"Message args: (&optional new-it)
When no argument, returns the default number of bootstrap iterations 
to be used in estimation of coefficient standard error. When NEW-IT 
is supplied, it becomes the new default value. "
	(when (and new-it (integerp new-it))
	      (setf (slot-value 'boot-iterations) new-it))
	(slot-value 'boot-iterations))

(defmeth IRLS-proto :calculate-se-estimates (&optional (B 40))
"Message args (&optional B)
Called internally to calculate the bootstrap standard error estimates. If 
the user wishes to call this method, he/she should provide a value for B,
which is the number of bootstrap iterations on which the standard error 
estimates will be based."
	(let* ((x (send self :x))
	       (y (send self :y))
	       (n (length y))
	       (intercept (send self :intercept))
	       (weights (send self :weights))
	       (wf-name (send self :wf-name))
	       (tol (send self :tol))
	       (count-limit (send self :count-limit))
	       (data (row-list (bind-columns x y)))
	       (p (if intercept (+ (array-dimension x 1) 1) (array-dimension x 1)))
	       (storage (make-array (list B p)))
	       (rows (iseq 0 (- n 1)))
	       (xcols (iseq 0 (- (array-dimension x 1) 1)))
 	       (pars (iseq 0 (- p 1))))

	(dotimes (count B (mapcar #'standard-deviation (column-list storage)))
		  (setf boot-sample (apply #'bind-rows (sample data n t)))
		  (setf new-x
			(select boot-sample rows xcols))
		  (setf new-y
			(select boot-sample rows (array-dimension x 1)))
		  (setf boot-beta
			(IRLS-regression new-x new-y :intercept intercept
                                                :weights weights
						:tol tol
						:count-limit count-limit
						:wf (make-wf wf-name)))
		(setf (select storage count pars) 
                           (make-array (list 1 P) :initial-contents (list  boot-beta))))))

;;;
;;; 	Other Methods
;;;	None of these methods can change any slot values...
;;;	       

(defmeth IRLS-proto :fit-values ()
"Message args : ()
Returns the fitted values for the model."
	(let ((x (send self :x))
               (beta (send self :coef-estimates))
               (intercept (send self :intercept)))
	  (if intercept 
		(+ (first beta) (matmult x (rest beta))) 
		(matmult x beta))))

(defmeth IRLS-proto :raw-residuals ()
"Message args: ()
Returns the raw-residuals for the model."
	(- (send self :y) (send self :fit-values)))

(defmeth IRLS-proto :final-weights ()
"Message args: ()
Returns the final weights used in the fit. (ie the model is the usual 
LS model using the output weights)."
	(let* ((resids (send self :raw-residuals))
	        (wf (make-wf (send self :wf-name)))
	        (scale (/ (median (abs resids)) .6745)))
	  (funcall wf (/ resids scale))))

(defmeth IRLS-proto :sigma-hat ()
"Message args: ()
Returns a robust estimate of sigma. See documentation for origin.."
	(let* ((y (send self :y))
               (yhat (send self :fit-values))
               (mad (median (abs (- y yhat)))) )
          (/ mad .6745 ) )) 

(defmeth IRLS-proto :residuals ()
"Message args: ()
Returns the raw residuals times the square roots of the final weights."
	(let ((raw-residuals (send self :raw-residuals))
              (sigma (send self :sigma-hat)))
          (/ raw-residuals sigma )))

(defmeth IRLS-proto :plot-residuals (&optional x-values)
"Message args:(&optional x-values)
Returns a plot of the residuals vs X-VALUES or the fitted values (default)."
	(plot-points
	     (if x-values x-values (send self :fit-values))
	     (send self :residuals)
	     :title "IRLS Residual Plot"))

(defmeth IRLS-proto :normal-plot ()
        (let* ((res (send self :residuals))
               (ranks (rank res))
               (n (length res))
               (nq (normal-quant (/ (+ ranks 1) (+ n 1)))))
            (plot-points nq res
                         :title "IRLS Normal Plot")))

(defun make-wf (name &optional
                   (k (case name
			     (biweight 4.685)
			     (cauchy 2.385)
			     (huber 1.345))))
     #'(lambda (r)
          (let ((u (abs (/ r k))))
		(case name
		    (biweight (^ (- 1 (^ (pmin u 1) 2)) 2))
		    (cauchy (/ 1 (+ 1 (^ u 2))))
		    (huber (/ 1 (pmax u 1)))))))

