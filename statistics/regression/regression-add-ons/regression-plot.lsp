;;;;
;;;;	Nicola Chapman STA 450S
;;;;

;;;
;;;	Function and associated extra methods for producing a plot
;;;	system for a regression object of any type.
;;;


(defun regression-plot (model &key (title "Regression Plot"))
"Message args: (model (&key title))
Returns a plot object. It is a scatterplot, spin-plot, or scatterplot matrix,
depending on the number of independent variables. Scatterplots and spin plots
have the line or plane fitted added to the plot. All types have menu items
which produce residual plots and normal plots for the model. For simple
regression, there are menu items which highlight points which may be outliers."
	(let* ((x (send model :x))
               (xplot (coerce (column-list x) 'list))
	      (y (send model :y))
	      (x-labels (send model :predictor-names))
              (y-label (send model :response-name))
	      (intercept (send model :intercept))
	      (coefs (if intercept (send model :coef-estimates)
			(append (list 0) (send model :coef-estimates))))
	      (dash-item (send dash-item-proto :new))
	      (dash2-item (send dash-item-proto :new))
	      (dash3-item (send dash-item-proto :new))
	      (residual-item (send menu-item-proto :new "Residual Plot"
			:action #'(lambda ()
			   (send model :plot-residuals))))
	      (normal-item (send menu-item-proto :new "Normal Plot"
			:action #'(lambda ()
			   (send model :normal-plot))))
	      (outlier-item (send menu-item-proto :new "Robust Outliers"
                        :action #'(lambda ()
                           (send m :highlight-outliers model))))
	      (leverage-item (send menu-item-proto :new "Leverages"
			:action #'(lambda ()
			   (send m :highlight-leverages model))))
	      (ext-resid-item (send menu-item-proto :new "Ext.-stud-residuals"
			:action #'(lambda ()
			   (send m :highlight-residuals model))))
	      (dfits-item (send menu-item-proto :new "DFITs"
		   	:action #'(lambda ()
			   (send m :highlight-dfits model))))
	      (refit-item (send menu-item-proto :new "Refit to visible points"
			:action #'(lambda ()
			   (send m :set-regression-line))))
              (m (case (array-dimension x 1)
	           (1 (plot-points (list (first xplot) y)
				   :variable-labels
				      (append x-labels (list y-label))))
		   (2 (spin-plot (list (first xplot) (second xplot) y)
				   :variable-labels
				      (append x-labels (list y-label))))
		   (t (scatterplot-matrix (append xplot (list y))
				   :variable-labels
				      (append x-labels (list y-label))) ) )) )
       (send m :title title)
       (send (send m :menu) :append-items dash-item residual-item normal-item
						dash3-item)	
       (case (array-dimension x 1)
          (1 (send m :abline (first coefs) (second coefs))
             (when (or (kind-of-p model LMS-proto) (kind-of-p model IRLS-proto))
	         (send (send m :menu) :append-items outlier-item dash2-item
                                                             refit-item))
	     (when (kind-of-p model regression-model-proto)
		 (send (send m :menu) :append-items leverage-item 
		ext-resid-item dfits-iteM dash2-item refit-item)))
	  (2 (send m :abcplane (first coefs) (second coefs) (third coefs))))
	m))


(defmeth scatterplot-proto :highlight-outliers (model)
"Message args:(model)
Called by the menu item ROBUST OUTLIERS in the regression plot of a simple 
LMS/IRLS regression model. Highlights the points that are recommended for  
removal."
	(let* ((weights (if (kind-of-p model LMS-proto)
			       (send model :suggested-weights)
			       (send model :final-weights)))
               (n (length weights))
	       (include (mapcar #'(lambda (x) (= x 1)) weights))
               (outliers (map 'list #'(lambda (x y) (if x x y))
                                include (iseq 0 (- n 1)))))
        (setf outliers (delete-if-not #'(lambda (x) (integerp x)) outliers))
	(send self :show-all-points)
        (if (null outliers) (message-dialog "No outliers")
	(send self :point-state outliers 'selecteD)) ))

(defmeth  scatterplot-proto :highlight-leverages (model)
"Message args: (model)
Called by the menu item LEVERAGES in the regression plot of a simple LS
regression model. Highlights the points that have leverage > 2p/n."
        (let* (
               (h (send model :leverages))
               (p (send model :num-coefs))
               (n (length h))
               (bound (/ (* 2 p) n))
               (too-big (map-elements #'(lambda (x y) (> x y))  h bound))
               (outliers (map 'list #'(lambda (x y) (if x y x))
                                                too-big (iseq 0 (- n 1)))) )
           (setf outliers (remove-if-not  #'(lambda (x) (integerp x))
                                 outliers))
           (send self :show-all-points)
	   (if (null outliers) (message-dialog "No points with large leverage")
             (send self :point-state outliers 'selecteD)) ))
 

(defmeth scatterplot-proto :highlight-residuals (model)
"Message args: (model)
Called by the menu item EXT-STUD-RESIDUALS in the regression plot of a 
simple LS regression-model. Highlights the points that have externally
studentized-residuals  whose absolute value is > t (0.975)."
        (let* (
               (e* (send model :externally-studentized-residuals))
               (n (length e*))
               (df (send  model :df))
               (t (t-cdf 0.975 df))
               (too-big (map-elements #'(lambda (x y) (> (abs x) y)) e* t))
               (outliers (map 'list #'(lambda (x y) (if x y x))
                                          too-big (iseq 0 (- n 1)))) )
        (setf outliers (remove-if-not #'(lambda (x) (integerp x))
                                          outliers))
        (send self :show-all-points)
        (if (null outliers) (message-dialog "No points with large e*")
        (send self :point-state outliers 'selecteD)) ))

(defmeth scatterplot-proto :highlight-dfits (model)
"Message args: (model)
Called by the menu item DFITS in the regression plot of a simple LS
regression-model. Highlights the points that have a dfits value > 2(p/df)^0.5"
	(let* ((dfits (send model :dfits))
               (n (length dfits))
               (p (send model :num-coefs))
	       (df (send model :df))
	       (bound (* 2 (^ (/ p df) 0.5)))
               (too-big (map-elements #'(lambda (x y) (> (abs x) y)) dfits bound))
               (outliers (map 'list #'(lambda (x y) (if x y x))
                                                too-big (iseq 0 (- n 1)))) )
           (setf outliers (remove-if-not  #'(lambda (x) (integerp x))
                                 outliers))
           (send self :show-all-points)
           (if (null outliers) (message-dialog "No points with large DFITS ")             (send self :point-state outliers 'selecteD)) ))


(defmeth scatterplot-proto :set-regression-line ()
"Message args: ()
Refits and plots a regression line to only the points showing in the 
graph window."
	(let ((coefs (send self :calculate-coefficients)))
		(send self :clear-lines :draw nil)
		(send self :abline (first coefs) (second coefs)) ))

(defmeth scatterplot-proto :calculate-coefficients ()
"Message args: ()
Recalculates the coefficients for the above message."
	(let* ((i (Send self :points-showing))
               (x (send self :point-coordinate 0 i))
	       (y (send self :point-coordinate 1 i))
	       (m (regression-model x y :print nil)))
	 (send m :coef-estimates) ))


(deF x (+ (* 3 (uniform-rand 30)) 1))
(deF e (* 0.02 (normal-rand 30)))
(deF y (+ x 2 e))

(deF x2 (+ 7 (* 0.05 (normal-rand 20))))
(deF y2 (+ 2 (* 0.05 (normal-rand 20))))

(deF xgen (concatenatE 'list x x2))
(deF ygen (concatenatE 'list y y2))


(def year (first (list (iseq 50 73)))) 
(def numcalls (list 0.44 0.47 0.47 0.59 0.66 0.73 0.81 0.88 1.06 1.20 1.35
                    1.49 1.61 2.12 11.90 12.40 14.20 15.90 18.20 21.20 4.30
                    2.40 2.70 2.90))

