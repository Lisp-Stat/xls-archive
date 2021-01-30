;;;; Code for running simulation studies of the jacknife, bootstrap
;;;; and sample-splitting approaches to allow for the effect of
;;;; data analytic actions.
;;;; Written by Julian Faraway, Department of Statistics,
;;;; University of Michigan.
(provide "simul")

(require "auxil")
(require "regmodel")

;; Regression analysis path
(defvar *reg-path* '(:log-transform
		     :outlier-test
		     :test-influence
		     :hetero-test
		     :box-cox-test
		     :tran-predictors
		     :bw-elim
		     :restore-points))
;; Calling sequences for models described in paper
;; Model Vanilla
;;(simul 50 400 100 100 5 :p-point (repeat 0.2 5))
;; Model Outlier
;;(simul 50 400 100 100 5 :p-point (repeat 0.2 5)
;;       :contam-prob 3 :contam-amt 3)
;; Model Nonlinear
;;(simul 50 400 100 100 5 :p-point (repeat 0.2 5)
;;       :x-generators (repeat (lambda (x) (/ (uniform-rand x) 5)) 5)
;;       :y-transform (lambda (x) (exp (/ x 5)))
;;       :y-transform-d (lambda (x) (/ (exp (/ x 5)) 5))
;;       :par-int-point (repeat 0.1 5))
;; Model Hetero
;;(simul 50 400 100 100 5 :p-point (repeat 0.2 5)
;;       :x-generators (repeat (lambda (x) (/ (uniform-rand x) 5)) 5)
;;       :par-int-point (repeat 0.1 5)
;;       :hetero 1)
;; Model Collinear
;; Look at the generate-data function at the end of this file and
;; alter according to instructions given.
;; Model Saturation
;;(simul 25 400 100 100 15 :p-point (repeat 0.2 15))
;;
;; If estimates for each replication are desired set
;; *rep-print* to true. See function one-replicate for details
(defvar *rep-print* t)

       

;; Main program
(defun simul (sample-size no-reps no-boots no-jacks no-preds &key
			(true-coefs (cons 0 (repeat 1 no-preds)))
			(x-generators (repeat #'normal-rand no-preds))
			(y-transform #'identity)
			(y-transform-d (lambda (x) 1))
			(x-transforms (repeat #'identity no-preds))
			(x-transforms-d (repeat (lambda (x) 1) no-preds))
			(p-point (repeat 1 no-preds))
			(par-int-point (repeat 0 no-preds))
			(hetero nil)
			(sim-error 1.0)
			(contam-prob 1)
			(contam-amt 1)
			(smf #'median)
			(vb-select (cons nil (repeat t (1- no-preds)))))
  "Args: (sample-size no-reps no-boots no-jacks &key
			(true-coefs (cons 0 (repeat 1 no-preds)))
			(x-generators (repeat #'normal-rand no-preds))
			(y-transform #'identity)
			(y-transform-d (lambda (x) 1))
			(x-transforms (repeat #'identity no-preds))
			(x-transforms-d (repeat (lambda (x) 1) no-preds))
			(p-point (repeat 1 no-preds))
			(par-int-point (repeat 0 no-preds))
			(hetero nil)
			(sim-error 1.0)
			(contam-prob 1)
			(contam-amt 1)
			(smf #'median)
			(vb-select (cons nil (repeat t (1- no-preds)))))
Sample size - sample-size
No. of replications - no-reps
No. of Bootstrap resamples - no-boots
No. of Jacknife resamples - no-jacks (use 0 for delete-one method)
True values of regression coefficients - true coefs
Functions to generate predictors - x-generators
Transform on the response - y-transform
Derivative of the above - y-transform-d
Transforms on the predictors - x-transforms
Derivatives of the above - x-transforms-d
Point at which prediction should be made - p-point 
Point at which parameter should be interpreted - par-int-point
Multiplier on the response for heteroscedastic error - hetero 
Error SD - sim-error 
Probability of contamination is 1 in contam-prob
Amount of contamination - contam-amt 
Summary function if multiple interpretation points - smf
Variables that should not be eliminated (true) - vb-select"

  (let* ((ltc (1+ (length (which (mapcar #'null vb-select)))))
         (no-dims (+ ltc 2))
         (del-one (= no-jacks 0))
         (true-pred (generate-data sample-size no-preds :true-coefs true-coefs
			      :x-generators x-generators
			      :y-transform y-transform 
			      :x-transforms x-transforms
			      :p-point p-point))
	 (true-int-pars (generate-data sample-size no-preds :interpret t :p-point par-int-point
				       :true-coefs true-coefs
				       :x-transforms x-transforms
				       :x-transforms-d x-transforms-d
				       :y-transform y-transform 
				       :y-transform-d y-transform-d))
         (true-values (repeat '(nil) no-dims))
         (boot-means  (repeat '(nil) no-dims))
         (boot-sds  (repeat '(nil) no-dims))
         (boot-rmses  (repeat '(nil) no-dims))
         (boot-iqrs  (repeat '(nil) no-dims))
         (boot-medians (repeat '(nil) no-dims))
         (split-values  (repeat '(nil) no-dims))
         (jack-biases  (repeat '(nil) no-dims))
         (jack-ses  (repeat '(nil) no-dims))
         (jack-medians  (repeat '(nil) no-dims))
         (jack-means  (repeat '(nil) no-dims))
         (jack-iqrs  (repeat '(nil) no-dims))
         (jack-rmses  (repeat '(nil) no-dims))
         (true-std-errs  (repeat '(nil) no-dims))
         (split-std-errs (repeat '(nil) no-dims)))
    (dotimes (i no-reps)
             (let* ((res (one-replicate sample-size no-boots no-jacks smf
					no-preds true-coefs x-generators y-transform 
					x-transforms p-point hetero sim-error 
					contam-prob contam-amt vb-select))
                    (results (nth 0 res))
                    (std-errs (nth 1 res))
                    (boot-results (nth 2 res))
                    (jack-results (nth 3 res))
                    (split-results (nth 4 res))
                    (std-errs-s (nth 5 res)))
               (dotimes (j no-dims)
                        (push (nth j results) 
                              (nth j true-values))
                        (push (nth j std-errs) 
                              (nth j true-std-errs))
                        (push (rmse (nth j boot-results) (nth j results))  
                              (nth j boot-rmses))
                        (push (mean (nth j boot-results)) 
                              (nth j boot-means))
                        (push (iqr  (nth j boot-results)) 
                              (nth j boot-iqrs))
                        (push (standard-deviation (nth j boot-results))
                              (nth j boot-sds))
                        (push (median (nth j boot-results)) 
                              (nth j boot-medians))
                        (push (nth j split-results) 
                              (nth j split-values))
                        (push (nth j std-errs-s) 
                              (nth j split-std-errs))
                        (if del-one
                            (progn
			      (push (jack-bias (nth j jack-results) 
					       (nth j results))
				    (nth j jack-biases))
			      (push (jack-se (nth j jack-results)) 
				    (nth j jack-ses)))
			  (progn
			    (push (- (nth j results) (mean (nth j jack-results)))
				     (nth j jack-biases))
			    (push (wjack-se 
				   (nth j jack-results) 
				   (nth no-dims jack-results)) 
				  (nth j jack-ses))))
                        (push (mean (nth j jack-results)) 
                              (nth j jack-means))
                        (push (iqr  (nth j jack-results)) 
                              (nth j jack-iqrs))
                        (push (median  (nth j jack-results)) 
                              (nth j jack-medians))
                        (push (rmse (nth j jack-results) (nth j results))
                              (nth j jack-rmses))
			)))
		  
    (print-simul-params sample-size no-reps no-boots no-jacks sim-error
			contam-amt contam-prob x-generators
			x-transforms y-transform hetero vb-select)
    (dotimes (i ltc)
             (let* ((indpred (= i (- ltc 1)))
                    (iv (which (mapcar #'null vb-select)))
                    (true-p (if indpred true-pred
			      (nth (1+ (nth i iv)) true-int-pars))))
               (if indpred
                   (format t "~%       Actual Predicted value: ~A~%" true-pred)
		 (progn
		   (format t "~%Variable ~A:  " i)
		   (format t "True values: ~a~%" true-p)))
               (print-true-results (nth i true-values) true-p)
               (print-boot-results (nth i true-values)
                                   (nth i boot-means)
                                   (nth i boot-sds)
                                   (nth i boot-rmses)
                                   (nth i boot-medians)
                                   (nth i boot-iqrs))
               (print-jack-results (nth i jack-means)
                                   (nth i jack-biases)
                                   (nth i jack-ses)
                                   (nth i jack-rmses)
                                   (nth i jack-medians)
                                   (nth i jack-iqrs))
               (print-split-results (nth i split-values) true-p)
               (print-sd-results (nth i split-std-errs) "Naive split: ")
               (print-sd-results (nth i true-std-errs) "Naive actual:")))
    (format t "~%R-squared~%")
    (print-supp-summary (nth ltc true-values)
                        (nth ltc boot-means)
                        (nth ltc jack-means)
                        (nth ltc split-values))
    (format t "~%Number of Coefs~%")
    (print-supp-summary (nth (1+ ltc) true-values)
                        (nth (1+ ltc) boot-means)
                        (nth (1+ ltc) jack-means)
                        (nth (1+ ltc) split-values))))

;; Leave out one jacknife data analysis on x y
(defun jack-all (x y mp smf vb-select pred-point)                        
  (let* ((no-dims (+ (length x) 4))
         (tal (repeat '(nil) no-dims)))
    (dotimes (i (length y) tal)                 
             (let* ((final-model 
                     (smini 
                      (mapcar (lambda (z) (rmel i z)) x) (rmel i y) vb-select))
                    (results (extract-results 
                              final-model mp pred-point smf)))
               (dotimes (j no-dims)
                        (push (nth j results) (nth j tal)))))))

;; Leave out many jacknife data analysis on x y
(defun ran-jack (x y b mp smf vb-select pred-point &optional rss )     
  "Args: x - predictor y - response 
r - jacknife sample size b -repetitions"                
  (let* ((no-dims (+ (length (which (mapcar #'null vb-select))) 4))
         (n (length y))
         (p (length x))
         (ss (if rss rss
                 (round (/ (+ n p -2) 2))))
         (ir (iseq 0 (- ss 1)))
         (tal (repeat '(nil) no-dims)))
    (dotimes (i b tal)
             (let* ((k (order (uniform-rand n)))
                    (final-model (smini
                           (mapcar (lambda (x) (select x (select k ir))) x)
                           (select y (select k ir)) vb-select))
             (results (extract-results final-model mp pred-point smf t)))
        (dotimes (j no-dims)
                 (push (nth j results) (nth j tal)))))))

;; Function to to generate and do the data analysis on
;; (x,y) with no-boots resamples.
             
(defun boot-all (x y no-boots mp smf vb-select pred-point)                                 
  (let* ((no-dims (+ (length (which (mapcar #'null vb-select))) 3))
         (tal (repeat '(nil) no-dims)))
    (dotimes (i no-boots tal)                 
             (let* ((rs (smoo-bisamp x y))                      
                    (final-model (smini (car rs) (cadr rs) vb-select))
             (results (extract-results final-model mp pred-point smf)))
        (dotimes (j no-dims)
                 (push (nth j results) (nth j tal)))))))

(defun smini (x y vb-select &optional (rpath *reg-path*))
"Args: (x y) 
Returns reg-model resulting from data analysis of predictors x
and response y following path in rpath"
  (let ((r (reg-model x y :vb-select vb-select :history nil :print nil)))
    (dolist (i rpath r)
	    (send r i))))

(defun extract-results (model int-pt p-point smf &optional (jack nil))
  (append (mapcar smf (send model :decon-ests int-pt))
          (send model :prediction p-point)
	  (if jack
	      (list (send model :r-squared)
		    (send model :num-coefs)
		    (/ 1 (determinant (send model :xtxinv))))
	    (list (send model :r-squared)
		  (send model :num-coefs)))))

(defun split-analysis (x y vb-select)
  (let* ((n (length y))
         (cut (- (round (/ n 2)) 1))
	 (regp (if (any y #'(lambda (x) (<= x 0))) 
		   (notran *reg-path*) *reg-path*))
         (split-model-eda (smini
                           (mapcar #'(lambda (z) 
                                       (rmel (iseq 0 cut) z)) x)
                           (rmel (iseq 0 cut) y) vb-select regp))
         (selfun (lambda (z) (rmel (iseq (+ cut 1) (- n 1)) z))))
    (reg-model 
     (construct-transformed-design-matrix
      (mapcar selfun x)
      (send split-model-eda :tran-list))
     (funcall (caar (send split-model-eda :tran-list))
              (funcall selfun y))
     :tran-list (send split-model-eda :tran-list)
     :vb-select vb-select
     :print nil)))

(defun extract-standard-errors (model int-pt p-point smf)
  (append
   (mapcar smf (send model :decon-stderrs int-pt))
   (nth 1 (send model :prediction p-point :compute-se t))))

(defun print-simul-params (sample-size no-reps no-boots no-jacks sim-error
				       contam-amt contam-prob x-generators
				       x-transforms y-transform hetero vb-select)
  (format t "Sample Size: ~A~%" sample-size)
  (format t "No. of replications: ~A~%" no-reps)
  (format t "No. of Bootstraps: ~A~%" no-boots)
  (format t "No. of Jacknives: ~A~%" no-jacks)
  (format t "Amount of error: ~A~%" sim-error)
  (format t "Amount of contamination: ~A~%" contam-amt)
  (format t "Probability of contamination: 1 in ~A~%" contam-prob)
  (format t "X-generating functions: ~A~%" x-generators)
  (format t "Transformations applied to predictors: ~A~%" x-transforms)
  (format t "Transformations applied to response: ~A~%" y-transform)
  (if hetero
      (format t "Heteroscedascity multiplier: ~a~%" hetero))
  (format t "Undroppable variables list: ~A~%" vb-select)
  (format t "Regression path: ~A~%" *reg-path*)
  (if (= no-jacks 0)
      (format t "Delete-one jacknife~%")
      (format t "Delete ~d jacknife~%" (- sample-size 
         (round (/ (+ sample-size (length vb-select) -1) 2))))))

(defun one-replicate (sample-size no-boots no-jacks smf
				  no-preds true-coefs x-generators y-transform 
				  x-transforms pred-point hetero sim-error 
				  contam-prob contam-amt vb-select)
  (let* ((data (generate-data sample-size no-preds :true-coefs true-coefs
			      :x-generators x-generators
			      :y-transform y-transform 
			      :x-transforms x-transforms
			      :hetero hetero
			      :sim-error sim-error 
			      :contam-prob  contam-prob 
			      :contam-amt contam-amt))
         (y (car data))
         (x (cadr data))
         (mp (mapcar 'median x))
         (final-model (smini x y vb-select))
         (std-errs (extract-standard-errors
                    final-model mp pred-point smf))
         (results (extract-results final-model mp pred-point smf))
         (boot-results (boot-all x y no-boots mp smf vb-select pred-point))
	 (jack-results (if (= no-jacks 0)
                           (jack-all x y mp smf vb-select pred-point)
			 (ran-jack x y no-jacks mp smf vb-select pred-point)))
	 (dosplit (> (/ (length y) 2) (length x)))
         (split-model-final (if dosplit (split-analysis x y vb-select)))
         (std-errs-s (if dosplit (extract-standard-errors
			split-model-final mp pred-point smf)
		       (repeat 0 10)))
         (split-results (if dosplit (extract-results 
			   split-model-final mp pred-point smf)
			  (repeat 0 10))))
;; Printout results
    (if *rep-print*
	(progn
    (format t " ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f~%"
	    (car results)
	    (mean (car boot-results))
	    (mean (car jack-results))
	    (car split-results)
	    (car std-errs)
	    (rmse (car boot-results) (car results))
	    (rmse (car jack-results) (car results))
	    (car std-errs-s))
    (format t " ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f ~7,3f~%"
	    (cadr results)
	    (mean (cadr boot-results))
	    (mean (cadr jack-results))
	    (cadr split-results)
	    (cadr std-errs)
	    (rmse (cadr boot-results) (cadr results))
	    (rmse (cadr jack-results) (cadr results))
	    (cadr std-errs-s))))

    (list results std-errs boot-results jack-results
          split-results std-errs-s)))

(defun print-true-results (x true-p)
  (let ((mean-x (mean x)))
    (format t "            Mean            Bias    S.D           RMSE.            Median          IQR~%")
    (format t "Actual:  ~7,3f (~5,3f) ~7,3f ~7,3f        ~7,3f        ~7,3f        ~7,3f~%"
            mean-x (simse x) (- mean-x true-p) 
            (standard-deviation x)
            (rmse x true-p)
            (median x)
            (iqr x))))

(defun print-boot-results (tm bms bsds brmses bmeds biqrs)
  (let ((mx (mean bms)))
    (format t "Boot:    ~7,3f (~5,3f) ~7,3f ~7,3f(~5,3f) ~7,3f(~5,3f) ~7,3f(~5,3f) ~7,3f(~5,3f)~%"
            mx (simse bms) (- (mean tm) mx)
            (mean bsds) (simse bsds)
            (mean brmses) (simse brmses)
            (mean bmeds) (simse bmeds)
            (mean biqrs) (simse biqrs))))

(defun print-jack-results (jms jbs jsds jrmses jmeds jiqrs)
  (format t "Jack:    ~7,3f (~5,3f) ~7,3f ~7,3f(~5,3f) ~7,3f(~5,3f) ~7,3f(~5,3f) ~7,3f(~5,3f)~%"
          (mean jms) (simse jms) (mean jbs)
          (mean jsds) (simse jsds)
          (mean jrmses) (simse jrmses)
          (mean jmeds) (simse jmeds)
          (mean jiqrs) (simse jiqrs)))

(defun print-split-results (sms true-p)
  (let ((mx (mean sms)))
    (format t "Split:   ~7,3f (~5,3f) ~7,3f ~7,3f        ~7,3f        ~7,3f        ~7,3f~%"
            mx (simse sms) (- mx true-p)
            (standard-deviation sms)
            (rmse sms true-p)
            (median sms)
            (iqr sms))))

(defun print-sd-results (x s)
	       (format t "~a                 ~7,3f  ~7,3f(~5,3f)~%"
		       s 0 (mean x) (simse x)))

(defun print-supp-summary (a b j s)
  (pss-aux "Actual mean:" (mean a) (simse a))
  (pss-aux "Boot   mean:" (mean b) (simse b))
  (pss-aux "Jack   mean:" (mean j) (simse j))
  (pss-aux "Split  mean:" (mean s) (simse s)))

(defun pss-aux (s x y)
		 (format t "~a ~7,3f (~7,3f)~%" s x y))

;; Function to generate the data
(defun generate-data (n p &key
			(true-coefs (cons 0 (repeat 1 p)))
			(x-generators (repeat #'normal-rand p))
			(y-transform #'identity)
			(y-transform-d (lambda (x) 1))
			(x-transforms (repeat #'identity p))
			(x-transforms-d (repeat (lambda (x) 1) p))
			(p-point nil)
			(hetero nil)
			(interpret nil)
			(sim-error 1.0)
			(contam-prob 1)
			(contam-amt 1))
  "Args: (n p &key	(true-coefs (cons 0 (repeat 1 p)))
			(x-generators (repeat #'normal-rand p))
			(y-transform #'identity)
			(x-transforms (repeat #'identity p))
			(p-point nil)
                        (hetero nil)
			(sim-error 1.0)
			(contam-prob 1)
			(contam-amt 1))"
  (cond (interpret 
	 (let ((uyh (funcall y-transform-d (+ (car true-coefs) 
			(sum (* (cdr true-coefs) 
				(mapcar #'funcall x-transforms p-point)))))))
		(* uyh (cdr true-coefs) (mapcar #'funcall x-transforms-d p-point))))
	(p-point
	 (funcall y-transform (+ (car true-coefs) 
				 (sum (* (cdr true-coefs) 
					 (mapcar #'funcall x-transforms p-point))))))
	(t 
	 (let* ((x (mapcar #'funcall x-generators (repeat n p)))
;;       Collinearity: comment out line above, comment in 3 lines below.
;;	 (let* ((x1 (normal-rand n))
;;		(x (cons x1 (mapcar #'(lambda (z) 
;;		    (+ (* 0.1 (normal-rand z)) (- x1))) (repeat n (1- p)))))
		(sfx (+ (repeat (car true-coefs) n) 
			(mapcar #'sum (transp (map 'list #'* (cdr true-coefs) 
						   (mapcar #'funcall x-transforms  x))))))
		(e (* sim-error (if (= (random contam-prob) 0) 1 contam-amt) 
		      (normal-rand n))))
	   (if hetero
	       (list (funcall y-transform (+ (* e hetero sfx) sfx)) x)
	     (list (funcall y-transform (+ e sfx)) x))))))
	 
(defun notran (regp)
  (let ((z nil))
    (dolist (i regp (reverse z))
	    (if (not (or (equal i :LOG-TRANSFORM)
			 (equal i :BOX-COX-TEST)))
		(push i z)))))
