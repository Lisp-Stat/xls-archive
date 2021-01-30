;;;;
;;;;	Nicola Chapman STA 450S 
;;;;

(provide "LMS")
(require "statistics")
(require "graphics")
(require "help")

;;;;
;;;; 	LMS Regression Model Prototype
;;;;

(defproto LMS-proto '(x y intercept predictor-names response-name
		     par-iterations coef-estimates)
		     () 
		     *object*
  		     "LMS Regression Model")

(defun LMS-model (x y &key (intercept t)
			   (print t)
			   (par-iterations 500)
			   predictor-names
			   response-name)
"Args: (x y &key (intercept t) (print t) (par-iterations 500) predictor-names
		response-name) 
X	-list of independent variables, or X matrix
Y	-dependent variable
intercept 	-t to include, nil for no intercept
print		-t to display summary info, nil not to
par-iterations	-number of beta estimates to compute, to chose from
PREDICTOR-NAMES
RESPONSE-NAME
Returns an LMS regression model. To examine the model futher , assign the result
to a variable and send it messages."
	(let ((m (send LMS-proto :new ))
              (x (cond
		    ((matrixp x) x)
                    ((vectorp x) (list x))
                    ((and (consp x) (numberp (car x))) (list x))
		    ( t x))))
	   (send m :x (if (matrixp x) x (apply #'bind-columns x)))
	   (send m :y y)
	   (send m :intercept intercept)
	   (send m :par-iterations par-iterations)
	   (send m :predictor-names predictor-names)
	   (send m :response-name response-name)
 	   (if print (send m :display)) m ))

(defmeth LMS-proto :isnew () (send self :needs-computing t))

;;;
;;;	Computing and Display Methods
;;;

(defmeth LMS-proto :compute ()
"Message args: ()
Recomputes the estimates. For internal use by other messages."
	(let ((x (send self :x))
	      (y (send self :y))
	      (intercept (send self :intercept))
	      (par-its (send self :par-iterations)))
          (send self :coef-estimates
	     (LMS-regression x y :intercept intercept :b par-its)) ))

(defun LMS-regression  (x y &key intercept B)
"Args: (x y &key intercept B)
Used internally to compute the coefficent estimates. The user should not be 
accessing this function: use LMS-model to create an LMS regression model."
        (let* ((p (if intercept (+ (array-dimension x 1) 1)
                                (array-dimension x 1))) 
               (n (length y))
               (Arows (iseq 0 (- p 1)))
               (Acols (iseq 0 (- p 1)))
               (all-rows (iseq 0 (- n 1)))
               (data (row-list (if intercept
				(bind-columns (repeat 1 n) x y)
				(bind-columns x y)))))
        (dotimes (count1 B good-theta)
                (setf new-sample (apply #'bind-rows (sample data  p)))
                (setf A (select new-sample Arows acols))
                (setf b (select new-sample Arows p))
		(when (not (= (determinant A) 0))
                	(setf this-theta (solve A b)))
		(setf this-theta (coerce (coerce this-theta 'vector) 'list))
                (setf resids (- y (matmult
                            (select (apply #'bind-rows data) all-rows acols)
                                   this-theta)))
                (setf medhat (median (^ resids 2))) 
                (when (or (= count1 0) (< medhat good-med))
                        (setf good-med medhat)
                        (setf good-theta this-theta))  ) ) )


(defmeth LMS-proto :needs-computing (&optional set)
	(if set (setf (slot-value 'coef-estimates) nil))
	(null (slot-value 'coef-estimates)))


(defmeth LMS-proto :display ()
"Message args: ()
Prints the LMS regression summary. Lists coefficient estimates and model par."
        (let ((coefs (coerce (send self :coef-estimates) 'list))
 	      (x (send self :x))
	      (R (send self :Rsquare))
              (sigma (send self :sigma-hat*))
	      (its (send self :par-iterations))
	      (p-names (send self :predictor-names)))
           (format t "~% LMS Coefficent Estimates: ~2%")
           (when (send self :intercept)
                (format t "Constant             ~10g~%" (car coefs))
                (setf coefs (cdr coefs)))
           (dotimes (i (array-dimension x 1))
               (format t "~22a ~10g~%"
                   (select p-names i) (car coefs) )
                (setf coefs (cdr coefs)))
	    (format t "R-squared:           ~8g~%" R)
	    (format t "Scale estimate :     ~8g~%" sigma)
	    (format t "Coefficient estimates were based on ~d samples.~%" its)
))

;;;
;;;	Slot Accessors and Mutators 
;;;

(defmeth LMS-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument
NEW-X sets the x matrix to new-x and recomputes the estimates."
        (when (and new-x (matrixp new-x))
              (setf (slot-value 'x) new-x)
              (send self :needs-computing t))
        (slot-value 'x))

(defmeth LMS-proto :y (&optional new-y)
"Message args: (&optional new-y)
With no argument returns the y sequence as supplied to m. With an argument
NEW-Y sets the y sequence to NEW-Y and recomputes the estimates."
        (when (and new-y (sequencep new-y))
              (setf (slot-value 'y) new-y)
              (send self :needs-computing  t))
        (slot-value 'y))

(defmeth LMS-proto :intercept (&optional (val nil set))
"Message args: (&optional new-intercept)
With no argument returns t if the model includes an intercept term, nil if
not. With an argument NEW-INTERCEPT the model is changed to include or exclude
an intercept, according to the value of NEW-INTERCEPT."
        (when set
             (setf (slot-value 'intercept) val)
             (send self :needs-computing t))
        (slot-value 'intercept))

(defmeth LMS-proto :predictor-names (&optional (names nil set))
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

(defmeth LMS-proto :response-name (&optional (name "Y" set))
"Message args: (&optional name)
With no argument, returns the response name. NAME sets the name."
        (if set (setf (slot-value 'response-name) (if name (string name) "Y")))
        (slot-value 'response-name))

(defmeth LMS-proto :par-iterations (&optional new-it)
"Message args: (&optional new-it)
With no argument, returns the number of iterations used in coefficient 
estimation. See external documentation. When NEW-IT is supplied, it is
stored and the coefficient estimates are recalculated."
	(when (and new-it (integerp new-it))
	      (setf (slot-value 'par-iterations) new-it)
	      (send self :needs-computing t))
	(slot-value 'par-iterations))


(defmeth LMS-proto :coef-estimates (&optional new-estS)
"Message args: (&optional new-ests)
With no argument, returns the values of the coefficent estimates for
the last model fitted. When NEW-ESTS is supplied, the stored value
of the estimates is changed to NEW-ESTS. THIS OPTIONAL ARGUMENT IS
INTENDED FOR INTERNAL USE ONLY!!!"
	(when new-ests
		(setf (slot-value 'coef-estimates) new-ests))
	(if (send self :needs-computing) (send self :compute))
	(slot-value 'coef-estimates))


;;;
;;;     Other Methods
;;;     None of these methods can change any slot values...
;;;

(defmeth LMS-proto :fit-values ()
"Message args : ()
Returns the fitted values for the model."
        (let ((x (send self :x))
              (beta (send self :coef-estimates) )
               (intercept (send self :intercept)))
          (if intercept
                (+ (first beta) (matmult x (rest beta)))
                (matmult x beta))))

(defmeth LMS-proto :num-coefs()
"Message args: ()
Returns the number of coefficients estimtated..."
	(let ((beta (send self :coef-estimates)))
          (length beta )))

(defmeth LMS-proto :raw-residuals ()
"Message args: ()
Returns the raw-residuals for the model."
        (- (send self :y) (send self :fit-values)))

(defmeth LMS-proto :scale-est ()
"Message args: ()
Returns a preliminary scale estimate to be used in calcualtion of 
suggested weights. See write up for details on its origin>"
	(let* ((r (send self :raw-residuals))
	       (n (length r))
	       (p (send self :num-coefs)))
         (* 1.4826 (+ 1 (/ 5 (- n p))) (sqrt (median (^ r 2)))) ))

(defmeth LMS-proto :residuals ()
"Message args: ()
Returns r/scale-estimate."
	(let ((r (send self :raw-residuals))
              (s (send self :scale-est)))
          (/ r s) ))

(defmeth LMS-proto :suggested-weights ()
"Message-args: ()
Returns suggsted weights for a future LS model, based on outlier removal."
	(let ((stand-res (send self :residuals)))
	  (mapcar #'(lambda (x) (cond
				   ((<= (abs x) 2) 1)
                                   ((and (> (abs x) 2) (< (abs x) 3)) 0.5)
				   ((>= (abs x) 3) 0))) stand-res) ))

(defmeth LMS-proto :sigma-hat* ()
"Message args: ()
Returns the final scale estimate for the LMS regression."
	(let ((w (send self :suggested-weights))
              (r (send self :raw-residuals))
              (p (length (send self :coef-estimates))) )
       (sqrt (/ (sum (* w (^ r 2)))
                (- (sum w) p))) ))

(defmeth LMS-proto :Rsquare ()
"Message args: ()
A measure analagous to R^2 in an LS fit. See external documentation."
	(let* ((r (send self :raw-residuals))
               (med (median (abs r)))
               (y (send self :y))
               (mad (median (abs (- y (median y)))))
	       (intercept (send self :intercept)) )
       (if intercept
	    (- 1 (^ (/ med mad) 2))
            (- 1 (^ (/ med (median (abs y))) 2)) ) ))

(defmeth LMS-proto :normal-plot ()
        (let* ((res (send self :residuals))
               (ranks (rank res))
               (n (length res))
	       (nq (normal-quant (/ (+ ranks 1) (+ n 1)))))
            (plot-points nq res
			 :title "LMS Normal Plot")))

(defmeth LMS-proto :plot-residuals (&optional x-values)
"Message args:(&optional x-values)
Returns a plot of the residuals vs X-VALUES or the fitted values (default)."
        (plot-points
             (if x-values x-values (send self :fit-values))
             (send self :residuals)
             :title "LMS Residual Plot"))


