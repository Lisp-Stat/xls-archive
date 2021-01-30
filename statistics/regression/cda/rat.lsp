;;;; Regression Analysis Tool - Does parallel bootstrap or jacknife
;;;; analysis on regression data.
;;;; Written by Julian Faraway, Department of Statistics,
;;;; University of Michigan.
(provide "rat")

(require "auxil")
(require "regmodel")

;; List of possible data-analytic functions
(defvar *reg-path* '(:log-transform
		   :outlier-test
		   :test-influence
		   :restore-points
		   :hetero-test
		   :box-cox-test
		   :tran-predictors
		   :bw-elim))

(defvar *reg-path-names* '("Test variables for a log transform"
			   "Test for outliers"
			   "Test for influential points"
			   "Restore points"
			   "Test for heteroscedascity"
			   "Box-Cox test"
			   "Transform predictors"
			   "Backward elimination"))

;; Functions to setup the the bootstrap and jacknife regression models
(defun init-paya (x y no-boots &key jack predictor-names response-name vb-select)
  (cons (reg-model x y :print nil 
		   :predictor-names predictor-names
		   :response-name response-name
		   :vb-select vb-select)
	(if jack (setup-payaj x y no-boots :vb-select vb-select)
	  (setup-paya x y no-boots :vb-select vb-select))))


(defun setup-paya (x y no-boots &key vb-select)
  (let ((z nil))
    (dotimes (i no-boots z)
	     (let ((rs (smoo-bisamp x y)))
	       (push (reg-model (car rs) (cadr rs) 
				:print nil :vb-select vb-select) z)))))

(defun setup-payaj (x y b &key vb-select)
  (let* ((z nil) 
	 (n (length y))
	 (r (/ (+ n (length x) -2) 2))
	 (ir (iseq 0 (- r 1))))
    (if (= b 0)
	(dotimes (i n z)
		 (push (reg-model 
			(mapcar (lambda (z) (rmel i z)) x)
			(rmel i y)
			:print nil
			:vb-select vb-select) z))
      (dotimes (i b z)
	       (let ((k (order (uniform-rand n))))
		 (push (reg-model 
			(mapcar (lambda (x) (select x (select k ir))) x)
			(select y (select k ir))
			:print nil
			:vb-select vb-select) z))))))


;; Compute predictions at p-point for list of regression models r
(defun par-predict (r p-point)
  (let ((z nil))
    (dolist (i r (transp (reverse z)))
	    (push (send i :prediction p-point) z))))

;; Compute interpreted estimates at int-point np for list of regression models r
(defun par-estimates (r mp summary-function)
  (let ((z nil))
    (dolist (i r (transp (reverse z)))
	    (push (mapcar summary-function 
			  (send i :decon-ests mp)) z))))
;;
;; The main function
(defun rat (x y &key predictor-names response-name)
  "Args: (x y &key predictor-names response-name)
Predictors - x
Response - y
Predictor names - predictor-names 
Response name - response-name
Returns list of regression models"
  (let* ((jack (= (choose-item-dialog "Title" '("Bootstrap" "Jacknife")) 1))
	 (no-vars (length x))
	 (no-boots (no-boots-dialog jack))
	 (est-int (estimates-interest-dialog predictor-names no-vars))
	 (pred-int (predictions-interest-dialog))
	 (vb-select (vb-selector est-int no-vars))
	 (r (init-paya x y no-boots :jack jack
		       :predictor-names predictor-names
		       :response-name response-name
		       :vb-select vb-select))
	 (nr (car r))
	 (nrl (cdr r))
	 (rat-info (init-rat-info nr nrl est-int pred-int))
	 (mp (interpret-point-dialog x est-int))
	 (summary-function (lambda (x) (median x))))
    (if jack
	(format t "Jacknife method~%")
      (format t "Bootstrap method~%"))
    (format t "No. of resamples=~3d~%" no-boots)
    (do ((action -1 (choose-item-dialog "Regression action" (cons "finish" *reg-path-names*))))
	((or (null action) (= action 0)) r)
	(if (> action 0)
	    (dolist (i r)
		    (send i (nth (1- action) *reg-path*))))
	(extract-estimates rat-info nr nrl est-int pred-int mp summary-function)
	(break))))

;; Some  functions for nonparametric density estimation
;; Density estimate at point z for data x and bw h
(defun kde (z x h)
  (/ (mean (mapcar 'normal-dens (/ (- x z) h))) h))
;; Density estimate at points z for data x and bw h
(defun kdest (z x h)
  (mapcar (lambda (y) (kde y x h)) z))
(defun change-plot (grid x p h)	;; function to change the plot
  (send p :clear :draw nil)
  (send p :add-lines grid (kdest grid x h)))

(defun kernel-de (x &key (mesh 50) initial-bw (bw-range 3) (bw-num  21) 
		    (title "Kernel density estimate") location size)
  "Args: (x &key mesh initial-bw bw-range bw-num title location size)
Data - x
No. of points at which density is calculated - mesh
Initial choice of bandwidth - initial-bw
Range of bandwidths (initial-bw/bw-range , (initial-bw*bw-range) - bw-range
Number of bandwidths - bw-num 
Title for plot - title
Location of plot - location
Size of plot - size
Returns density estimate plot"
  (let* ((ir (iqr x))
	 (sd (standard-deviation x))
	 (a (min sd (/ ir 1.34)))	;Use Silverman p48 for default bw.
	 (h (if initial-bw initial-bw (* 0.9 a (^ (length x) -0.2))))
	 (triml (trim x :fl 5))
	 (nx (car triml))
	 (remx (nth 1 triml))
	 (grid (rseq (- (min nx) (* 2 bw-range h)) (+ (max nx) (* 2 bw-range h)) mesh))
	 (p (plot-lines grid (kdest grid nx h) :title title :location location :size size))
	 (bw (send menu-item-proto :new "Bandwidth"
		   :action #'(lambda () 
			       (sequence-slider-dialog
				(exp (rseq (log (/ h bw-range)) (log (* h bw-range)) bw-num))
				:action #'(lambda (y) (change-plot grid nx p y)) )))))
    (if remx
	(message-dialog (format nil "Outliers ~a excluded from display" remx)))
    (send (send p :menu) :append-items bw)
    p))

(defun renew-plot (hl np npl)
  (let (titp loc siz)
    (dotimes (i (length hl))
	     (setf titp (send (nth i hl) :title))
	     (setf loc (send (nth i hl) :location))
	     (setf siz (send (nth i hl) :size))
	     (send (nth i hl) :title (format nil "Old ~a" titp))
	     (setf (nth i hl) (kernel-de (nth i npl) :title titp :location loc :size siz))
	     (send (nth i hl) :add-points (list (list (nth i np)))))))

(defun advise-user ()
  (format t " nr is current regression-model, ~% ~
               nrl is the current list of resampled models, ~%"))


(defun no-boots-dialog (jack)
  (if jack 
      (car (let ((s (format nil "Enter number of Jacknife samples~%~
                                         -Enter 0 for leave-out-one method")))
	     (get-value-dialog s :initial 0)))
    (car (get-value-dialog "Enter number of Bootstrap samples" :initial 50))))

(defun estimates-interest-dialog (pn l)
  (car (choose-subset-dialog "Parameters of interest" 
			     (if pn
				 pn
			       (mapcar (lambda (x) (format nil "~A" x)) 
				       (iseq 1 l))))))

(defun predictions-interest-dialog ()
  (do ((plist nil))
      ((not (ok-or-cancel-dialog "Enter a prediction point")) plist)
      (let ((reply (car (get-value-dialog "Enter p-point"))))
	(if reply
	    (setf plist (cons reply plist))))))

(defun interpret-point-dialog (x est-int)
  (let ((imp (if est-int (choose-item-dialog 
			  "Parameter Interpretation point?"
			  '("median" "mean" "Average over predictors" "other")))))
    (case imp
	  (0 (mapcar 'median x))
	  (1 (mapcar 'mean x))
	  (2 (transp x))
	  (3 (car (get-value-dialog "Enter point")))
	  (t (mapcar 'median x)))))



(defun list-of-nils (n)
  (let ((z '(nil)))
    (dotimes (i (- n 1) z)
	     (setf z (cons nil z)))))

(defun extract-estimates (rat-info nr nrl est-int pred-int mp summary-function)
  (if est-int
      (progn
	(send rat-info :aest (mapcar summary-function (send nr :decon-ests mp)))
	(send rat-info :best (par-estimates nrl mp summary-function))))
  (if pred-int
      (progn
	(send rat-info :apred (send nr :prediction pred-int))
	(send rat-info :bpred (par-predict nrl pred-int)))))


(defun vb-selector (i n)
  (let ((tmp (repeat t n)))
    (if i
	(progn
	  (setf (select tmp i) 
		(repeat '(nil) (length i)))
	  tmp) tmp)))

;; Define an object that maintains the information about the analysis
;; in progress and it's history

(defproto rat-info-proto '(aest best apred bpred action actual-model 
				resampled-models est-int pred-int))

(defmeth rat-info-proto :aest (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the actual parameter effect estimates for
the history of the analysis. DATA adds new estimates to the list"
  (if set (push data (slot-value 'aest)))
  (slot-value 'aest))

(defmeth rat-info-proto :best (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the resampled parameter effect estimates for
the history of the analysis. DATA adds new estimates to the list"
  (if set (push data (slot-value 'best)))
  (slot-value 'best))

(defmeth rat-info-proto :apred (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the actual predictions for
the history of the analysis. DATA adds new estimates to the list"
  (if set (push data (slot-value 'apred)))
  (slot-value 'apred))

(defmeth rat-info-proto :bpred (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the resampled predictions for
the history of the analysis. DATA adds new estimates to the list"
  (if set (push data (slot-value 'bpred)))
  (slot-value 'bpred))

(defmeth rat-info-proto :est-int (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the paramaters of interest.
DATA sets the list"
  (if set (setf (slot-value 'est-int) data))
  (slot-value 'est-int))

(defmeth rat-info-proto :pred-int (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the predictions of interest.
DATA sets the list"
  (if set (setf (slot-value 'pred-int) data))
  (slot-value 'pred-int))

(defmeth rat-info-proto :action (&optional (data nil set))
"Message Args: (&optional (data nil set))
With no argument returns list of the actions applied to the model.
DATA adds new actions to the list"
  (if set (push data (slot-value 'action)))
  (slot-value 'action))

(defmeth rat-info-proto :actual-model (&optional (model nil set))
"Message Args: (&optional (data nil set))
With no argument returns the current actual model.
MODEL sets the slot."
  (if set (setf (slot-value 'actual-model) model))
  (slot-value 'actual-model))

(defmeth rat-info-proto :resampled-models (&optional (models nil set))
"Message Args: (&optional (data nil set))
With no argument returns the current resampled models.
MODELS sets the slot."
  (if set (setf (slot-value 'resampled-models) models))
  (slot-value 'resampled-models))

(defmeth rat-info-proto :start-menu ()
"Message Args: ()
Initializes a menu to control RAT"
  (let* ((mm (send menu-proto :new "RAT"))
	 (display (send menu-item-proto :new "Display model"
			:action #'(lambda () 
				    (send (send self :actual-model) :display))))
	 (history (send menu-item-proto :new "History"
			:action #'(lambda () (send self :history))))
	 (summary (send menu-item-proto :new "Summary"
			:action #'(lambda () (send self :step-summary))))
	 (density-Eplots (send menu-item-proto :new "Effect density"
			       :action #'(lambda () (send self :density-Eplots))))
	 (density-Pplots (send menu-item-proto :new "Prediction density"
			       :action #'(lambda () (send self :density-Pplots))))
	 (effect-history (send menu-item-proto :new "Effect history"
				 :action #'(lambda () (send self :effect-history))))
	 (prediction-history (send menu-item-proto :new "Prediction history"
				 :action #'(lambda () (send self :prediction-history))))
	 (interpret (send menu-item-proto :new "Effect slice"
			  :action #'(lambda () (send self :interpret-slice))))
	 (pint (send menu-item-proto :new "Prediction slice"
			  :action #'(lambda () (send self :predictive-slice))))
	 (continue (send menu-item-proto :new "Continue"
			 :action #'(lambda () (continue))))
	 (dispose (send menu-item-proto :new "Remove Menu" :action
			#'(lambda () (send mm :dispose)))))

    (send mm :append-items display history summary density-Eplots density-Pplots 
	  effect-history prediction-history
	  interpret pint continue dispose)
    (if (null (send self :est-int))
	(progn
	  (send density-Eplots :enabled nil)
	  (send interpret :enabled nil)
	  (send effect-history :enabled nil)))
    (if (null (send self :pred-int))
	(progn
	  (send density-Pplots :enabled nil)
	  (send pint :enabled nil)
	  (send prediction-history :enabled nil)))
    (send mm :install)))
	 
(defun init-rat-info (actual-model resampled-models est-int pred-int)
"Args: (actual-model resampled-models est-int pred-int)
Start up a new RAT information object"
  (let ((ri (send rat-info-proto :new)))
    (send ri :actual-model actual-model)
    (send ri :resampled-models resampled-models)
    (send ri :est-int est-int)
    (send ri :pred-int pred-int)
    (send ri :start-menu)
    ri))

(defmeth rat-info-proto :density-Eplots ()
"Message args: ()
Constructs a kernel density estimate of the estimated parameter
effects"
  (let* ((pn (send (send self :actual-model) :predictor-names))
	 (ei (send self :est-int))
	 (j (if (> (length ei) 1)
		(choose-item-dialog "Which predictor" (select pn ei)) (car ei)))
	 (npl (car (send self :best)))
	 (s (format nil "Parameter: ~a" (nth (nth j ei) pn))))
    (send  (kernel-de (nth j npl) :title s) 
	   :add-points (list (list (nth j (car (send self :aest))))))))

(defmeth rat-info-proto :density-Pplots ()
"Message args: ()
Constructs a kernel density estimate of the predictions"
  (let* ((nprl (car (send self :bpred)))
	(pri (send self :pred-int))
	(j (if (> (length pri) 1)
	       (choose-item-dialog "Which predictor" (select pn pri)) 0))
	(s (format nil "Prediction at ~a" (nth j (send self :pred-int)))))
    (send (kernel-de (nth j nprl) :title s) 
	  :add-points (list (list (nth j (car (send self :apred))))))))

(defmeth rat-info-proto :history ()
"Message args: ()
Displays the history of the analysis"
  (let ((h (cdr (send (send self :actual-model) :history))))
    (format t "~%")
    (dolist (i h)
	    (format t "~15a ~a~%" (car i) (cadr i)))
    (format t "~%")))

(defmeth rat-info-proto :step-summary ()
"Message args: ()
Gives numerical summary of state of the analysis"
  (let* ((nr (send self :actual-model))
	 (np (car (send self :aest)))
	 (npl (car (send self :best)))
	 (npr (car (send self :apred)))
	 (nprl (car (send self :bpred)))
	 (loo (= (send nr :num-cases) 
		 (1+ (send (car (send self :resampled-models)) :num-cases)))))
    (format t "~%Adjusted R-squared=~A~%" (send nr :adjusted-rsquared))
    (if np
	(progn
	  (format t "Summary for interpreted parameter estimates:~%")
	  (format t "Original data estimates: ~a~%" np)
	  (if loo
	      (progn
		(format t "Estimate of the bias: ~a~%" 
			(map 'list #'jack-bias npl np))
		(format t "Estimate of the se: ~a~%" 
			(mapcar #'jack-se npl)))
	    (progn
	      (format t "Estimated rmse: ~a~%" 
		      (map 'list #'rmse npl np))
	      (format t "IQ ranges: ~a~%" (mapcar #'iqr npl))))))
    (if npr
	(progn
	  (format t "Summary for predictions:~%")
	  (format t "Original data estimates: ~a~%" npr)
	  (if loo
	      (progn
		(format t "Estimate of the bias: ~a~%" 
			(map 'list #'jack-bias nprl npr))
		(format t "Estimate of the se: ~a~%" 
			(mapcar #'jack-se nprl)))
	    (progn
	      (format t "Estimated rmse: ~a~%" 
		      (map 'list #'rmse nprl npr))
	      (format t "IQ ranges: ~a~%" (mapcar #'iqr nprl))))))
    (advise-user)
    (format t "~%")))
      
(defun spin-slice (x y z &rest args)
  "Args: Display density slice spin plot"
  (let ((plot (apply #'send spin-proto :new 3 :show nil args)))
    (apply #'send plot :add-surface x y z :draw nil args)
    (send plot :adjust-to-data :draw nil)
    (send plot :new-menu)
    (send plot :showing-axes nil)
    (send plot :rotate-2 0 1 (/ pi 3) :draw nil)
    (send plot :rotate-2 1 2 (- (/ pi 3)) :draw nil)
    (send plot :show-window)
    plot))

(defmeth rat-info-proto :density-history (a d s)
"Message args: (a d s)
Display density history spin plot
A is the list of actual values
D is the list of resampled values
S is the title for the plot"
  (let* ((maxd (max (mapcar #'max d)))
	 (mind (min (mapcar #'min d)))
	 (e (* 0.1 (- maxd mind)))
	 (x (rseq (- mind e) (+ maxd e) 21))
	 (z (apply #'bind-columns (mapcar #'(lambda (arg) 
					      (cadr (kernel-dens arg :xvals x))) d)))
	 (y (iseq 1 (length d)))
	 (p (spin-slice x y z :title s)))
    (send p :add-lines (list a y (repeat 0 (length d))))))

(defmeth rat-info-proto :interpret-slice ()
"Message Args: ()
Display a density slice plot for the interpreted parameter
effect"
  (let* ((pn (send (send self :actual-model) :predictor-names))
	 (ei (send self :est-int))
	 (j (if (> (length ei) 1)
		(choose-item-dialog "Which predictor" (select pn ei)) 0))
	 (minx (car (get-value-dialog "From what point?")))
	 (maxx (car (get-value-dialog "To what point?")))
	 (nos (car (get-value-dialog "Number of Slices" :initial 10)))
	 (ips (transp (mapcar #'rseq minx maxx (repeat nos (length minx)))))
	 (a (nth j (send (send self :actual-model) :decon-ests ips)))
	 (b (transp (mapcar #'(lambda (x) (nth j x))
			    (mapcar #'(lambda (x) (send x :decon-ests ips)) 
				    (send self :resampled-models)))))
	 (s "Effect slice")
	 (l (mapcar #'(lambda (x) (format nil "~s" x)) ips)))
    (case (choose-item-dialog "Which method?"
			      '("Boxplot" "Density slice" "both"))
	  (0 (send self :boxplot a b s l))
	  (1 (send self :density-history a b s))
	  (2 (send self :boxplot a b s l)
	     (send self :density-history a b s)))))

(defmeth rat-info-proto :predictive-slice ()
"Message Args: ()
Display a density slice plot for some predictions"
  (let* ((minx (car (get-value-dialog "From what point?")))
	 (maxx (car (get-value-dialog "To what point?")))
	 (nos (car (get-value-dialog "Number of Slices" :initial 10)))
	 (ips (transp (mapcar #'rseq minx maxx (repeat nos (length minx)))))
	 (a (send (send self :actual-model) :prediction ips))
	 (b (transp (mapcar #'(lambda (x) (send x :prediction ips))
			    (send self :resampled-models))))
	 (s "Prediction slice")
	 (l (mapcar #'(lambda (x) (format nil "~s" x)) ips)))
    (case (choose-item-dialog "Which method?"
			      '("Boxplot" "Density slice" "both"))
	  (0 (send self :boxplot a b s l))
	  (1 (send self :density-history a b s))
	  (2 (send self :boxplot a b s l)
	     (send self :density-history a b s)))))

(defmeth rat-info-proto :effect-history ()
  (let* ((pn (send (send self :actual-model) :predictor-names))
	 (ei (send self :est-int))
	 (j (if (> (length ei) 1)
		(choose-item-dialog "Which predictor" (select pn ei)) 0))
	 (b (reverse (mapcar #'(lambda (x) (nth j x)) (send self :best))))
	 (a (reverse (mapcar #'(lambda (x) (nth j x)) (send self :aest))))
	 (s (format nil "E.P.E. ~a history" (nth (nth j ei) pn)))
	 (l (cons "Initial"
	  (mapcar #'car (cdr (send (send self :actual-model) :history))))))
    (case (choose-item-dialog "Which method?"
			      '("Boxplot" "Density slice" "both"))
	  (0 (send self :boxplot a b s l))
	  (1 (send self :density-history a b s))
	  (2 (send self :boxplot a b s l)
	     (send self :density-history a b s)))))

(defmeth rat-info-proto :prediction-history ()
  (let* ((pri (send self :pred-int))
	 (j (if (> (length pri) 1)
		(choose-item-dialog "Which predictor" (select pn pri)) 0))
	 (s (format nil "Prediction at ~a Boxplot history" 
		    (nth j (send self :pred-int))))
	 (b (reverse (mapcar #'(lambda (x) (nth j x)) (send self :bpred))))
	 (a (reverse (mapcar #'(lambda (x) (nth j x)) (send self :apred))))
	 (l (cons "Initial"
	  (mapcar #'car (cdr (send (send self :actual-model) :history))))))
    (case (choose-item-dialog "Which method?"
			      '("Boxplot" "Density slice" "both"))
	  (0 (send self :boxplot a b s l))
	  (1 (send self :density-history a b s))
	  (2 (send self :boxplot a b s l)
	     (send self :density-history a b s)))))

(defmeth rat-info-proto :boxplot (a b s &optional l)
  (let ((p (boxplot b :title s)))
    (send p :add-points (iseq 1 (length a)) a)
    (if l
	(let* ((w (send p :canvas-width))
	       (h (send p :canvas-height))
	       (n (length l))
	       (d (round (/ w (* 2 (+ n 2)))))
	       (labs (send menu-item-proto :new "Labels"
			   :action #'(lambda () 
	  (dotimes (i n)
		   (send p :draw-string (nth i l)
			 (+ (* 3 d) (* i 2 d)) (- h 5)))))))
	  (send (send p :menu) :append-items labs)))))

