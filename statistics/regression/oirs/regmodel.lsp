(provide "regmodel")

(require "regression")
(require "auxil")

;;;;
;;;;
;;;; Modified Regression Model Prototype
;;;; includes transform list and model-building methods
;;;; written by Julian Faraway, Department of Statistics,
;;;; University of Michigan.

(defproto reg-model-proto
          '(tran-list vb-select history)
          '()
          regression-model-proto
	  "Modified linear regression model")

(defun reg-model (x y &key 
		    (intercept T) 
		    (print T) 
		    weights
		    (included (repeat t (length y)))
		    predictor-names
		    response-name
		    case-labels
		    tran-list
		    vb-select
		    (history T))
  "Args: (x y &key (intercept T) (print T) weights included 
  predictor-names response-name case-labels tran-list vb-select)
X to INCLUDED same as REGRESSION-MODEL
TRAN-LIST  - List of transforms applied to original data
VB-SELECT  - T-F list, T variables may not be selected or transformed
HISTORY - list of regression actions applied to the model
First element indicates if list should be maintained
Returns a regression model object."
  (let ((x (cond 
	    ((matrixp x) x)
	    ((vectorp x) (list x))
	    ((and (consp x) (numberp (car x))) (list x))
	    (t x)))
        (m (send reg-model-proto :new)))
    (send m :x (if (matrixp x) x (apply #'bind-columns x)))
    (send m :y y)
    (send m :intercept intercept)
    (send m :weights weights)
    (send m :included included)
    (send m :predictor-names predictor-names)
    (send m :response-name response-name)
    (send m :case-labels case-labels)
    (send m :tran-list tran-list)
    (send m :vb-select vb-select)
    (send m :history history)
    (if print (send m :display))
    m))

(defmeth reg-model-proto :save ()
"Message args: ()
Returns an expression that will reconstruct the regression model."
  `(reg-model ',(send self :x)
              ',(send self :y)
	      :intercept ',(send self :intercept)
	      :weights ',(send self :weights)
	      :included ',(send self :included)
	      :predictor-names ',(send self :predictor-names)
	      :response-name ',(send self :response-name)
	      :case-labels ',(send self :case-labels)
	      :tran-list ',(send self :tran-list)
	      :vb-select ',(send self :vb-select)
	      :history ',(send self :history)))

(defmeth reg-model-proto :tran-list (&optional (flist nil set))
  "Message args: (&optional flist)
With no argument returns the transform list. FLIST sets the flist."
  (if set (setf (slot-value 'tran-list) 
		(if flist flist (repeat  
				 '((identity)) 
				 (1+ (array-dimension (send self :x) 1))))))
  (slot-value 'tran-list))

(defmeth reg-model-proto :vb-select (&optional (flist nil set))
  "Message args: (&optional flist)
With no argument returns the selectable variables list i.e. 
(nil t t) indicates that the first predictor may not be eliminated.
FLIST sets the flist."
  (if set (setf (slot-value 'vb-select) 
		(if flist flist (repeat t (array-dimension (send self :x) 1)))))
  (slot-value 'vb-select))

(defmeth reg-model-proto :history (&optional (flist nil set))
  "Message args: (&optional flist)
With no argument returns the history list. FLIST is consed to the history.
First element indicates if list should be maintained, subsequent elements
are list pairs - first element being the action, the second the result"
  (if set (push flist (slot-value 'history)))
  (reverse (slot-value 'history)))

(defmeth reg-model-proto :display ()
"Message args: ()
Prints the least squares regression summary. Variables not used in the fit
are marked as aliased."
  (let ((coefs (coerce (send self :coef-estimates) 'list))
        (se-s (send self :coef-standard-errors))
        (x (send self :x))
        (w (send self :weights))
        (tl (send self :tran-list))
        (r-name (send self :response-name))
        (p-names (vnames (send self :tran-list) (send self :predictor-names))))
    (if (equal (car tl) '(IDENTITY))
	(format t "The response is ~A~%" r-name)
      (format t "The response is ~A ~A~%" (string-downcase (format nil "~A" (car tl))) r-name))
    (if w
	(if (> (min w) 0)
	    (format t "There are no excluded points~2%")
	  (format t "Points no. ~A were excluded~2%" (+ 1 (which (= 0 w)))))
      (format t "~%There are no excluded points~2%"))
    (when (send self :intercept)
          (format t "Constant               ~10g   ~A~%"
                  (car coefs) (list (car se-s)))
          (setf coefs (cdr coefs))
          (setf se-s (cdr se-s)))
    (dotimes (i (array-dimension x 1)) 
             (cond 
               ((member i (send self :basis))
                (format t "~22a ~10g   ~A~%"
                        (select p-names i) (car coefs) (list (car se-s)))
                (setf coefs (cdr coefs) se-s (cdr se-s)))
               (t (format t "~22a    aliased~%" (select p-names i)))))
    (format t "~%")
    (format t "R Squared:             ~10g~%" (send self :r-squared))
    (format t "Sigma hat:             ~10g~%" (send self :sigma-hat))
    (format t "Number of cases:       ~10d~%" (send self :num-cases))
    (if (/= (send self :num-cases) (send self :num-included))
        (format t "Number of cases used:  ~10d~%" (send self :num-included)))
    (format t "Degrees of freedom:    ~10d~%" (send self :df))
    (format t "~%")))

(defmeth reg-model-proto :predictor-names (&optional (names nil set))
"Message args: (&optional (names nil set))
With no argument returns the predictor names. NAMES sets the names."
  (if set (setf (slot-value 'predictor-names) (mapcar #'string names)))
  (let ((p (array-dimension (send self :x) 1))
        (p-names (slot-value 'predictor-names)))
    (if (not p-names)
        (setf (slot-value 'predictor-names)
              (mapcar #'(lambda (a) (format nil "Variable ~a" a)) 
                      (iseq 1 p)))))
  (slot-value 'predictor-names))
;; Outlier test

(defmeth reg-model-proto :outlier-test 
  (&key (p-value 0.05))
  "Message args: (&key (p-value 0.05))
Tests for outliers in a regression model by computing
the externally studentized residuals and using a Bonferroni
correction to determine significance (see Weisberg p116)
Significant points are excluded"
  (if (> (abs (- (send self :r-squared) 1)) 1.0e-5) 
      (let* ((n (send self :num-included))  
	     (df (send self :df))
	     (ow (send self :weights))
	     (j (send self :externally-studentized-residuals))
	     (crit (t-quant (- 1 (/ p-value (* 2 n))) df))
	     (nw (if-else (> (abs j) crit) (repeat 0 n) (repeat 1 n)))
	     (change (and (> (sum nw) (- n df)) (= (min nw) 0))))
	(if (car (last (send self :history)))
	    (let ((s (if change
			 (format nil "Points ~a were found to be outliers"
				 (+ 1 (which (= 0 nw))))
		       (format nil "No change"))))
	      (send self :history (list "Outlier test" s))))
	(if change
	      (send self :weights (if ow (* nw ow) nw)))
	change)))

;; Need to change original method to deal with zero residuals
(defmeth reg-model-proto :externally-studentized-residuals ()
"Message args:  ()
Computes the externally studentized residuals."
  (let* ((res (send self :studentized-residuals))
         (df (send self :df))
	 (resd (- df (^ res 2)))
	 (ir (which (= resd 0))))
    (setf (select resd ir) (repeat 1 (length ir)))
    (if-else (send self :included)
             (* res (sqrt (/ (- df 1) resd)))
             res)))


;; Influential points test

(defmeth reg-model-proto :test-influence ()
"Message args: ()
Tests if Cook's distances exceed 1, points that do are
excluded"
  (if (> (abs (- (send self :r-squared) 1)) 1.0e-10) 
  (let* ((n (send self :num-cases))
	 (ow (send self :weights))
	 (nw (if-else (> (send self :cooks-distances) 1) 
		      (repeat 0 n) (repeat 1 n)))
	 (change (and (> (sum nw) (- n (send self :df))) (= (min nw) 0))))
    (if (car (last (send self :history)))
	(let ((s (if change
		     (format nil "Points ~a were found to be influential"
			     (+ 1 (which (= 0 nw))))
		   (format nil "No change"))))
	  (send self :history (list "Influence test" s))))
    (if change
	(send self :weights (if ow (* nw ow) nw)))
    change)))

(defmeth reg-model-proto :hetero-test
(&key (p-value 0.05))
"Message args: (&key (p-value 0.05))
Heteroscedascity test (see p135 Weisberg) Tests if the squared
residuals are quadratic in the response and if so reweights 
appropriately."
  (if (> (abs (- (send self :r-squared) 1)) 1.0e-10)
    (let* ((e (send self :residuals)) 
	   (n (send self :num-included))
	   (iweights (send self :weights))
	   (weights (if iweights 
			(let ((i (which (/= 0 iweights))))
			  (setf (select iweights i) (repeat 1 (length i)))
			  iweights)
		      (repeat 1 n)))
	   (u (/ (* e e n) (sum (* e e))))
	   (y (send self :y))
	   (y2 (* y y))
	   (s (regression-model (list y y2) u :print nil :weights weights))
	   (mu (mean u))
	   (tss (sum (** (- u mu) 2)))
	   (score (/ (- tss (send s :sum-of-squares)) 2))
	   (change (< (- 1 (chisq-cdf score 2)) p-value)))
      (if (car (last (send self :history)))
	  (let ((s (if change (format nil "Heteroscedascity found")
		     (format nil "No change"))))
	    (send self :history (list "Heteroscedascity test" s))))
      (if change
	  (progn;; Iterate the estimation once more time
	    (send self :weights (/ weights (abs (send s :fit-values))))
	    (setf s (regression-model (let ((yi (send self :y))) 
					(list yi (^ yi 2)))
				      (^ (send self :residuals) 2)
				      :weights (^ (send self :weights) 2)
				      :print nil))
	    (send self :weights (/ weights (abs (send s :fit-values)))))
	(send self :weights (if-else (= weights 0) (repeat 0 n) (repeat 1 n))))
      change)))

;; Box-Cox test to transform the response (Weisberg p148)

(defmeth reg-model-proto :box-cox-test
  (&key (box-cox-indices '(3.0 2.0 1.5 1.0 0.5 0.0 -0.5 -1.0 -1.5 -2.0 -3.0)))
  "Message args: (&key 
(box-cox-indices '(3.0 2.0 1.5 1.0 0.5 0.0 -0.5 -1.0 -1.5 -2.0 -3.0)))
Calculate the best index for the tranformation of the response
using the Box Cox test and if indicated makes that transformation"
(let ((s nil))
  (if (and (not (any (send self :y) #'(lambda (x) (<= x 0))))
           (equal (car (send self :tran-list)) '(IDENTITY)))
      (progn
	(let* ((bcl (bc (send self :x)
                        (send self :y)
                        (send self :weights)
                        box-cox-indices))
               (maxl (max bcl))
               (maxi (select box-cox-indices (car (which (= maxl bcl)))))
	       (bi (if (or (= maxi (max box-cox-indices)) 
			   (= maxi (min box-cox-indices))) 1 maxi))
	       (change (/= bi 1)))
	  (if change
	      (progn
		(setf s (format nil "Response transformed with index ~a" bi))
		(let* ((fun (last (assoc bi *fun-list* :test '=)))
		       (tlist (send self :tran-list))
		       (yt (car tlist)))
		  (if fun
		      (if (equal yt '(IDENTITY))
			  (setf (select tlist 0) fun)
			(setf (select tlist 0) (append fun (list yt))))
		    (error "Undefined index ~A in reset-y~%" bi))
		  (send self :tran-list tlist)
		  (send self :y (funcall (car fun) (send self :y)))))))))
  (if (car (last (send self :history)))
      (send self :history (list "Box-Cox test" (if s s "No change"))))
  (if s t s)))


(defmeth reg-model-proto :log-transform 
(&key (max-ratio 100))
"Message args: (&key (max-ratio 100))
Checks whether a log-transform of variables is appropriate by
determining if the the ration of the maximum to the minimum 
exceeds max-ratio. If so a log transformation is made"
  (let ((change nil)
	(x (column-list (send self :x)))
	(tlist (send self :tran-list))
	(y (send self :y))
	(s ""))

    (dotimes (i (- (send self :num-coefs) 1))
	     (let ((ithx (nth i x)))
	       (if (log-transform ithx max-ratio)
		   (progn
		     (setf change t)
		     (setf (select x i) (log ithx))
		     (let ((pn (send self :predictor-names)))
		       (if pn
			   (setf s (format nil "~a ~A was log-transformed " 
					   s (nth i pn)))
			 (setf s  (format nil "~a Predictor ~A was log-transformed " 
					  s i))))
		     (setf (select tlist (+ i 1)) '(log))))))
    (if change
	(send self :x (apply 'bind-columns x)))
    (if (log-transform y max-ratio) 
	(progn
	  (setf change t)
	  (setf s (format nil "~a Response was log-transformed" s))
	  (setf (select tlist 0) '(log))
	  (send self :y (log y))))
    (if (car (last (send self :history)))
      (send self :history (list "Skewness test" s)))
    (if change
	(send self :tran-list tlist))
    change))

;; Variable selection by backward elimination

(defmeth reg-model-proto :bw-elim
  (&key (p-value 0.05))
  (do* ((np (array-dimension (send self :x) 1) (array-dimension (send self :x) 1))
	(bas (send self :basis) (send self :basis))
	(done (cond ((< (length bas) np)
		     (saturated-elimination bas np (send self :tran-list)))
		    ((or (> (send self :r-squared) 0.99999) 
			(complexp (car (send self :coef-standard-errors)))) nil)
		    ((send self :coef-standard-errors)
		     (backward-elimination 
		      (abs (cdr (/ (send self :coef-estimates)
				   (send self :coef-standard-errors))))
		      (send self :tran-list) (send self :df) p-value (send self :vb-select)))
		    (t nil))
	      (cond ((< (length bas) np)
		     (saturated-elimination bas np (send self :tran-list)))
		    ((or (> (send self :r-squared) 0.99999) 
			(complexp (car (send self :coef-standard-errors)))) nil)
		    ((send self :coef-standard-errors)
		     (backward-elimination 
		      (abs (cdr (/ (send self :coef-estimates)
				   (send self :coef-standard-errors))))
		      (send self :tran-list) (send self :df) p-value (send self :vb-select)))
		    (t nil)))
	(s (if (and (car (last (send self :history))) done)
	       (name-elim (send self :predictor-names) 
			  (send self :tran-list) done))
	   (if (car (last (send self :history)))
	       (if done
		   (format nil "~a ~a" s (name-elim (send self :predictor-names) 
				  (send self :tran-list) done)) s)))
	(change (not (null done)))
	(bogus (if (and (car (last (send self :history))) (not done))
		   (send self :history (list "Backward Elimination" 
				       (if s s "No change"))))
	       (if (and (car (last (send self :history))) (not done))
		   (send self :history (list "Backward Elimination" 
				       (if s s "No change"))))))
       ((null done) change)
       (if done 
	   (progn
	     (setf (select (send self :tran-list) (1+ (nth 1 done)))
		   (let* ((tl (nth (1+ (nth 1 done)) (send self :tran-list))))
		     (if (= (length tl) 1)
			 '(null)
		       (rmel (nth 2 done) tl))))
	     (send self :x (delete-column (send self :x) (car done)))))))

(defmeth reg-model-proto :tran-predictors  (&key (diag-print t)  (p-value 0.05))
  (let* ((nopr (1- (length (send self :tran-list))))
	 (change nil)
	 (ctr 0)
	 (s "")
	 (np (array-dimension (send self :x) 1)))
    (if (> np (length (send self :basis))) nil
      (dotimes (i nopr change)
	       (let* ((elm (nth (1+ i) (send self :tran-list)))
		      (add-ctr (if (equal elm '(NULL)) 0
				 (length elm))))
		 (if (and (equal elm '(IDENTITY))
			  (nth i (send self :vb-select)))
		     (let ((truncl (send self :test-predictor ctr)))
		       (if (/= truncl 1)
			   (let* ((x (send self :x))
				  (varx (nth ctr (column-list x)))
				  (tvarx (if (= truncl 0) (log varx)
					   (^ varx truncl))))
			     (setf change t)
			     (setf add-ctr (1+ add-ctr))
			     (if (car (last (send self :history)))
				 (let ((pn (send self :predictor-names)))
				   (if pn
				       (setf s (format nil
						       "~a ~a transformed with index ~a" 
					       s (nth i pn) truncl))
				     (setf s (format nil
						     "~a Predictor ~a transformed with index ~a" 
					     s i truncl)))))
			     (send self :x (insert-column x tvarx (1+ ctr)))
			     (setf (select (send self :tran-list) (1+ i))
				   (let ((fun (last (assoc truncl *fun-list* :test '=))))
				     (append elm fun)))))))
		 (if (and (= (1+ i) nopr) (car (last (send self :history))))
		     (send self :history (list "Transform predictors" 
					       (if (equal s '"") "No change" s))))
		 (setf ctr (+ ctr add-ctr)))))))


(defmeth reg-model-proto :adjusted-rsquared ()
"Message Args: 
Returns the adjusted r-squared of a regression model"
    (let ((rsq (send self :r-squared))
          (n (send self :num-included))
          (p (send self :num-coefs)))
      (- 1 (* (/ (- n 1) (- n p)) (- 1 rsq)))))

;; Computes prediction and s.e.(key)at point x
;; in the original scale of y - note the calculation of s.e.
(defmeth reg-model-proto :prediction (x &key (compute-se nil))
"Args: (x &key (compute-se nil))
Returns predicted value (with optional se) at x
for reg-model x in the original scale of response"
  (let* ((lx (if (listp (car x)) x (list x)))
	 (ests (send self :coef-estimates))
	 (ax (tranx lx (send self :tran-list)))
	 (ba (cons 0 (1+ (send self :basis))))
	 (pp (mapcar (lambda (a) (sum (* (select a ba) ests))) ax)))
    (if compute-se
	(let* ((sigh (send self :sigma-hat))
	       (xtxinv  (send self :xtxinv))
	       (ytf (caar (send self :tran-list)))
	       (se (mapcar (lambda (nax) (* sigh
				(sqrt (sum (* (%* nax xtxinv) nax))))) ax)))
	(list (inverse-apply ytf pp)
	      (* (- (inverse-apply ytf (+ pp (* 0.05 se)))
		    (inverse-apply ytf (- pp (* 0.05 se)))) 10)))
      (inverse-apply (caar (send self :tran-list)) pp))))


(defmeth reg-model-proto :decon-ests (mp)
"Args: (mp)
Returns interpretable estimate list from reg-model
at interpretation point(s) mp"
  (decon-interpret (cdr (send self :coef-estimates)) 
		   (send self :tran-list)
		   mp
		   (mapcar (caar (send self :tran-list)) (send self :prediction mp))
		   (send self :vb-select)))

(defmeth reg-model-proto :decon-stderrs (mp)
"Args: (mp)
Returns interpretable se of estimate list from reg-model
at interpretation point(s) mp"
  (let* ((e (cdr (send self :coef-estimates)))
	 (s (cdr (send self :coef-standard-errors)))
	 (eu (+ e (* 0.05 s)))
	 (ed (- e (* 0.05 s)))
	 (p (send self :prediction mp))
	 (tl (send self :tran-list))
	 (vb (send self :vb-select))
	 (ieu (decon-interpret eu tl mp p vb))
	 (ied (decon-interpret ed tl mp p vb)))
    (* 10 (abs (- ieu ied)))))

;; Method to evaluate bootstrapped quantities
(defmeth reg-model-proto :bootstrap (method nb &key (conditional t))
"Message Args: (method nb &key (conditional t)) Applies
METHOD to NB bootstrapped regression models (resampling
rows unless conditional is true then resampling residuals)"
  (let ((z nil))
    (if conditional
	(let ((rt (regression-model (send self :x) (send self :y)
				    :print nil))
	      (n (send self :num-cases))
	      (res (send self :residuals))
	      (fv (send self :fit-values)))
	  (dotimes (i nb (transp z))
		   (send rt :y (+ fv (resample-y res (resample-indices n))))
		   (push (send rt method) z)))
      (let ((x (column-list (send self :x)))
	    (y (send self :y)))
	(dotimes (i nb (transp z))
		 (let ((a (smoo-bisamp x y)))
		   (push (send (regression-model (car a) (cadr a) :print nil)
			       method) z)))))))

(defmeth reg-model-proto :test-predictor (ctr &key (p-value 0.05))
"Message Args: (ctr &key (p-value 0.05))
Test for transformation of predictor CTR using method 
described in Weisberg p.153"
  (let* ((x (send self :x))
	 (varx (nth ctr (column-list x)))
	 (index (if (> (min varx) 0) 
		    (let* ((tvarx (* varx (log varx)))
			   (tmpr (regression-model 
				  (bind-columns x tvarx)
				  (send self :y)
				  :weights (send self :weights)
				  :print nil)) 
			   (beta (nth (1+ ctr) 
				      (send tmpr :coef-estimates)))
			   (eta (car (last (send tmpr :coef-estimates))))
			   (etas (car (last (send tmpr :coef-standard-errors))))
			   (critical-value (abs 
					    (t-quant p-value 
						     (+ (send tmpr :df) 1)))))
		      (if (> (abs (/ eta etas)) critical-value)
			  (+ (/ eta beta) 1) 1))
		  1))
	 (l	(/ (round (* 2 index)) 2) 1))
    (cond ((> l 2) 2)
	  ((< l -2) -2)
	  (t l))))

(defmeth reg-model-proto :restore-points (&key (p-value 0.05))
  "Message args: (&key (p-value 0.05))
Tests for outliers in a regression model by computing
the externally studentized residuals and using a Bonferroni
correction to determine significance (see Weisberg p116)
Unsignificant points are reincluded if necessary"
  (if (> (abs (- (send self :r-squared) 1)) 1.0e-5) 
      (let* ((n (send self :num-included))  
	     (df (send self :df))
	     (w (send self :weights))
	     (ow (if w w (repeat 1 n)))
	     (j (send self :alt-ext-studentized-residuals))
	     (not-outlier (< (abs j) (t-quant (- 1 (/ p-value (* 2 n))) df)))
	     (not-inc (= ow 0))
	     (reinc (mapcar #'(lambda (x y) (and x y)) not-outlier not-inc))
	     (aw (mean (select ow (which (> ow 0)))))
	     (nw (if-else reinc (repeat aw n) ow))
	     (change (> (length (which reinc)) 0)))
	(if (car (last (send self :history)))
	    (let ((s (if change
			 (format nil "Points ~a were reincluded"
				 (+ 1 (which reinc)))
		       (format nil "No change"))))
	      (send self :history (list "Point restoration" s))))
	(if change
	      (send self :weights nw))
	change)))

(defmeth reg-model-proto :alt-ext-studentized-residuals ()
"Message args: () An alternative way of computing the externally
studentized residuals - this one computes the residuals of zero
weight points - used by :restore-points"
  (let ((r (send self :externally-studentized-residuals))
	(w (send self :weights)))
    (if (and w (= (min w) 0))
	(let* ((i (which (= w 0)))
	       (rr (/ (select (send self :raw-residuals) i)
		      (send self :sigma-hat))))
	  (setf (select r i) rr)))
    r))
