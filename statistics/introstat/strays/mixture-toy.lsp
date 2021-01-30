;;; file mixture-toy.lsp
;;;
;;; Creating a toy for playing with normal mixtures
;;; it is based on a prototype for probability distributions
;;; to be finished
;;;
;;; Frederic Udina, June 1995.
;;; udina@upf.es  or http://libiya.upf.es/


;;; you would like to tune up this variables
;;; this is text length for the sliders value display
#+macintosh (defvar *text-length* 5)
#-macintosh (defvar *text-length* 10)
;;; this is the sample point for graphing the density function
#+macintosh (defvar *num-x-values* 100)
#-macintosh (defvar *num-x-values* 300)


(defproto mixture-toy-proto 
  '(window distrobj x-values 
	   display) () *object*)

(defmeth mixture-toy-proto :isnew ()
  (slot-value 'display 'density)
  (slot-value 'window
	      (send graph-proto :new 2 :title "Mixture Distribution" :show nil))
  (send (slot-value 'window) :scaled-range 0 -4 4)
  (send (slot-value 'window) :scaled-range 1 0 0.7)
  (send (slot-value 'window) :show-window)
  (send (slot-value 'window) :x-axis t t 5)
  (send (slot-value 'window) :y-axis t t 4)
  (slot-value 'x-values (rseq -4 4 *num-x-values*))
  (let* ((me self)
	 (menu (send (slot-value 'window) :menu))
	 (it (send menu-item-proto :new "Switch function"
		   :action #'(lambda () (send me :switch-mode)))))
    (send menu :append-items
	  (send dash-item-proto :new) it))
  (send self :create-dialog))

(defmeth mixture-toy-proto :redraw ()
  (send (slot-value 'window) :clear-lines :draw nil)
  (case (slot-value 'display)
	('density
	 (send (slot-value 'window) :add-lines
	       (list (slot-value 'x-values)
		     (mapcar (send (slot-value 'distrobj)
				   :slot-value 'density-function)
			     (slot-value 'x-values))) :draw nil))
	('distribution
	 (send (slot-value 'window) :add-lines
	       (list (slot-value 'x-values)
		     (mapcar (send (slot-value 'distrobj)
				   :slot-value 'distribution-function)
			     (slot-value 'x-values))) :draw nil)))
  (send (slot-value 'window) :redraw))
   
(defmeth mixture-toy-proto :install-distribution (distrib)
  (slot-value 'distrobj distrib)
  (send self :redraw))

(defmeth mixture-toy-proto :switch-mode (&optional mode)
  (unless mode
	  (setf mode (case (slot-value 'display)
			   ('density 'distribution)
			   ('distribution 'density))))
  (slot-value 'display mode)
  (send self :redraw)
  (send (slot-value 'window) :adjust-to-data))






;;;thanks to naras for his slider builder tool!
(defmeth mixture-toy-proto :create-dialog ()
  "gives a dialog for adjusting a normal mixture"
  (let* ((ms '(0 1))
	 (params (append '(1) ms '(0) ms '(0) ms)) 
	 (xlo -3)
	 (xhi 3)
	 (slo  0.2)
	 (shi 2.5)
	 (user t)
	 sli-list)
    (labels
     ((iszero (x) (< (abs x) 1.d-3))
      (install-distr ()
		     (send self :install-distribution
			   (make-normal-mixture
			    (remove-if 
			     #'(lambda (trio)
				 (iszero (first trio)))
			     (split-list params 3))))
		     (send self :redraw))
      (update-it (n x)
		 (when user
		       (setf user nil)
		       ;; (format t "~a--" params)
		       (if (member n '(0 3 6))
			   (repart n x)
			 (setf (select params n) x)
			 )
		       ;; (format t ">>~a" params)
		       (install-distr)
		       (setf user t)))
      (repart (n x);proportions must add to one
	      (let* ((n1 (mod (+ n 3) 9))
		     (n2 (mod (+ n 6) 9))
		     (ix (- x (nth n params)))
		     rt inc)
		(unless (iszero ix)
			(setf (nth n params) x)
			(cond ((and (iszero (nth n1 params))
				    (iszero (nth n2 params)))
			       (setf ix (/ ix 2))
			       (setf (nth n2 params) (- (nth n2 params) ix))
			       (setf (nth n1 params) (- (nth n1 params) ix)))
			      ((iszero (nth n1 params))
			       (setf (nth n2 params) (- (nth n2 params) ix)))
			      ((iszero (nth n2 params))
			       (setf (nth n1 params) (- (nth n1 params) ix)))
			      (t (setf rt (/ (nth n1 params) (nth n2 params)))
				 (setf inc (/ ix (1+ rt)))
				 (setf (nth n2 params)
				       (- (nth n2 params) inc))
				 (setf (nth n1 params)
				       (- (nth n1 params) (* inc rt)))))
			(mapcar #'(lambda (nn so)
				    (send so :value (nth nn params)))
				'(0 3 6) sli-list)))));end labels
     (let* ((ttt (send text-item-proto :new
		       "Adjust Proportions, means and stdevs"))
	    (to0 (send text-item-proto :new
		       "Prop. 1"))
	    (vo0 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (so0 (send interval-scroll-item-proto :new
		       '(0 1)
		       :text-item vo0
		       :action
		       #'(lambda(x) (update-it 0 x))))
	    (tx3 (send text-item-proto :new
		       "Prop. 2"))
	    (vx3 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (sx3 (send interval-scroll-item-proto :new
		       '(0 1)
		       :text-item vx3
		       :action
		       #'(lambda(x) (update-it 3 x))))
	    (tx6 (send text-item-proto :new
		       "Prop. 3"))
	    (vx6 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (sx6 (send interval-scroll-item-proto :new
		       '(0 1)
		       :text-item vx6
		       :action
		       #'(lambda(x) (update-it 6 x))))
	    (tx1 (send text-item-proto :new
		       "Mean 1"))
	    (vx1 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (sx1 (send interval-scroll-item-proto :new
		       (list XLO XHI)
		       :text-item vx1
		       :action
		       #'(lambda(x) (update-it 1 x))))
	    (to4 (send text-item-proto :new
		       "Mean 2"))
	    (vo4 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (so4 (send interval-scroll-item-proto :new
		       (list XLO XHI)
		       :text-item vo4
		       :action
		       #'(lambda(x) (update-it 4 x))))
	    (tx7 (send text-item-proto :new
		       "Mean 3"))
	    (vx7 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (sx7 (send interval-scroll-item-proto :new
		       (list XLO XHI)
		       :text-item vx7
		       :action
		       #'(lambda(x) (update-it 7 x))))
	    (tx2 (send text-item-proto :new
		       "StDev 1"))
	    (vx2 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (sx2 (send interval-scroll-item-proto :new
		       (list SLO SHI)
		       :text-item vx2
		       :action
		       #'(lambda(x) (update-it 2 x))))
	    (tx5 (send text-item-proto :new
		       "StDev 2"))
	    (vx5 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (sx5 (send interval-scroll-item-proto :new
		       (list SLO SHI)
		       :text-item vx5
		       :action
		       #'(lambda(x) (update-it 5 x))))
	    (to8 (send text-item-proto :new
		       "StDev 3"))
	    (vo8 (send text-item-proto :new
		       "" :text-length *text-length*))
	    (so8 (send interval-scroll-item-proto :new
		       (list SLO SHI)
		       :text-item vo8
		       :action
		       #'(lambda(x) (update-it 8 x))))
	    (dlg (send dialog-proto :new
		       (list
			ttt
			(list (list (list to0 vo0) so0)
			      (list (list tx3 vx3) sx3)
			      (list (list tx6 vx6) sx6))
			(list (list (list tx1 vx1) sx1)
			      (list (list to4 vo4) so4)
			      (list (list tx7 vx7) sx7))
			(list (list (list tx2 vx2) sx2)
			      (list (list tx5 vx5) sx5)
			      (list (list to8 vo8) so8)))
		       :title "Make a normal mixture"
		       :go-away nil
		       :location (list (first (send (slot-value 'window)
						    :location))
				       (+ (second (send (slot-value 'window)
							:location))
					  (second (send (slot-value 'window)
							:size)))))))
       (setf sli-list (list so0 sx1 sx2 sx3 so4 sx5 sx6 sx7 so8))
       (setf user nil)
       (mapcar #'(lambda (n so)
		   (send so :value (nth n params)))
	       (iseq 9) sli-list)
       (setq user t)
       (setq sli-list (list so0 sx3 sx6))
       (install-distr)
       (send (slot-value 'window) :add-subordinate dlg)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; this is the current version of file distrobj.lsp
;;; needed by mixture.lsp
;;; but useful by itself. F. Udina, June 1995
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;distrobj.lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f. udina, jun 93

#|
 a distrobj is an object representing a probability distribution
 it contains some slots and methods:

* numeric slots and posible values
   - mean, stdev can be numbers or nil
     when they are nil, a default method will attempt to estimate them
     by integration or so.
   - support, a list defining the density function approx support or nil
     if nil, density is > 0 for all real numbers
     if it is a one number list '(x), this defines the interval (x infinity)
     if it is a two numbers list, they give the ends of the support interval
* other slots:
   - density-function, 
     distribution-function, both have closure values, no both can be nil.
   - random-generator has closure value too.
   - inverse-cdf can have closure value or nil.
   - hazard-function is a closure (x) defined for some distribution
     used in survival analysis.
* methods: some methods check for existence of a corresponding slot, if it is 
  found, it is simply funcalled, if it is not, a default procedure
  is called instead based on other already existing slots.

method :cdf (x) gives the cumulative probability, P(X<x).
method :density (x) gives the probability density for x. It simply funcalls
   the density-function slot if it exists, if not, it is estimated from
   the cdf function.
method :quantile (p) gives the quantile for probability p.
   If slot :inverse-cdf is not nil, it is funcalled, if it is nil,
   an iterative newton procedure is invoked, by means of density-function
   and cdf.
method :generate-random (&optional (num 1)) will return a list of NUM random
   numbers distributed as self.

method :plot (&key (num-points 121) will create a graph window for plotting 
   the density function.

There will too be some creation functions like:

(defun make-normal-distribution (&optional (mean 0) (stdev 1) &rest args)
(defun make-normal-mixture (params &rest args)
   where params is a list of the form ((w1 m1 sd1) (w2 m2 sd2) ...)
   of the weights, means and standard deviations involved.

(defun make-marron-wand (num)

ara cal repassar la resta de l'arxiu
|#

(provide "distrobj")

(defun ld () (load "distrobj")) ;; useful while developping


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; definition and construction of distr-proto, the prototype
;;; object for a distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto distr-proto '(title
			parameters
                        mean stdev
                        support
                        density-function distribution-function
			hazard-function
                        random-generator
                        density-derivative quantile-function
                        )
  ()
  *object*)

(defmeth distr-proto :isnew (&key title
                                  mean stdev
                                  support
                                  density-function distribution-function
				  hazard-function
                                  random-generator
                                  density-derivative quantile-function )
"initialization method of a distr-proto. Arguments are all keys:
 title
 mean stdev
 support
 density-function distribution-function
 random-generator
 density-derivative quantile-function "
  (call-next-method)
  (setf (slot-value 'title) title)
  (setf (slot-value 'mean) mean)
  (setf (slot-value 'stdev) stdev)
  (setf (slot-value 'support) support)
  (setf (slot-value 'density-function) density-function)
  (setf (slot-value 'distribution-function) distribution-function)
  (setf (slot-value 'hazard-function) hazard-function)
  (setf (slot-value 'density-derivative) density-derivative)
  (setf (slot-value 'quantile-function) quantile-function)
  (setf (slot-value 'random-generator) random-generator)
)

(defmeth distr-proto :density (x)
"returns the value of the density function on x
 as instaled in 'density slot"
 (if (slot-value 'density-function)
  (funcall (slot-value 'density-function) x)
  (error (strcat "density-function not given for distr: "
                        (slot-value 'title)) nil)))

(defmeth distr-proto :generate-random (&optional (num 1))
"Returns a list of NUM (default 1) random numbers from the distribution."
  (funcall (slot-value 'random-generator) num))

(defmeth distr-proto :density-derivative (x)
"gives the density function derivative at x or an estimate of it"
)

(defmeth distr-proto :cdf (x)
"gives the value of the accumulated probabiliy P(X<x)"
)

(defmeth distr-proto :inverse-density-function (&optional fun)
"sets or retrieves the density function inverse of the distribution"
)


(defmeth distr-proto :plot (&key (num-points 121) (xrange nil) )
"builds a graph object with the plot of the density function of self.
 Args, key (num-points 21)"
  (prog* ((ends (if xrange xrange (slot-value 'support)))
          (plot nil))
         (unless ends
                 (setq ends (+ (slot-value 'mean) 
                               (* '(-5 5) (slot-value 'stdev)))))
         (setq plot
               (apply #'plot-function (slot-value 'density-function)
                      (append ends (list :num-points num-points))))
         (when (slot-value 'title)
               (send plot :title (slot-value 'title)))
         plot))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to make usual distributions
;;; e.g. those already implemented in xlispstat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-normal-distribution (&optional (mean 0) (stdev 1) &rest args)
  (let ((distr (apply #'send distr-proto :new args)))
    (send distr :slot-value 'title (format nil "N[~a,~a]" mean stdev))
    (send distr :slot-value 'parameters (list mean stdev))
    (send distr :slot-value 'mean mean)
    (send distr :slot-value 'stdev stdev)
    (send distr :slot-value 'support (list (- mean (* 3 stdev))
                                           (+ mean (* 3 stdev))))
    (if (and (= mean 0) (= stdev 1))
        (progn
         (send distr :slot-value 'density-function #'normal-dens)
         (send distr :slot-value 'random-generator #'normal-rand)
         (send distr :slot-value 'distribution-function #'normal-cdf))
        (progn
         (send distr :slot-value 'random-generator 
               #'(lambda (x) 
                   (+ mean (* (normal-rand x) stdev))))
         (send distr :slot-value 'density-function 
               #'(lambda (x) 
                   (/ (normal-dens (/ (- x mean) stdev))
                      stdev)))
         (send distr :slot-value 'distribution-function 
               #'(lambda (x) 
                   (normal-cdf (/ (- x mean) stdev))))))
    distr))


(defun make-exponential-distribution (&optional (lambda 1) &rest args)
  (let ((distr (apply #'send distr-proto :new args)))
    (send distr :slot-value 'title (format nil "Exponential[~a]" lambda))
    (send distr :slot-value 'parameters (list lambda))
    (send distr :slot-value 'mean (/ lambda))
    (send distr :slot-value 'stdev (/ (* lambda lambda)))
    (send distr :slot-value 'support '(0 5))
    (send distr :slot-value 'random-generator 
	  #'(lambda (n) 
	      (/ (- (log (uniform-rand n))) lambda)))
    (send distr :slot-value 'density-function 
	  #'(lambda (x)
	      (* lambda (exp (* x (- lambda))))))
    (send distr :slot-value 'distribution-function 
	  #'(lambda (x) 
	      (- 1 (exp (* x (- lambda))))))
    (send distr :slot-value 'hazard-function 
	  #'(lambda (x)
	      lambda))
distr))


(defun make-weibull-distribution (&optional (lambda 1) (kappa 1) &rest args)
  (let ((distr (apply #'send distr-proto :new args)))
    (send distr :slot-value 'title (format nil
					   "Weibull[~a, ~a]" lambda kappa))
    (send distr :slot-value 'parameters (list lambda kappa))
    (send distr :slot-value 'mean nil)
    (send distr :slot-value 'stdev nil)
    (send distr :slot-value 'support '(0 5))
    (send distr :slot-value 'random-generator 
	  #'(lambda (n) 
	      (expt (/ (- (log (uniform-rand n))) lambda) (/ kappa))))
    (send distr :slot-value 'density-function 
	  #'(lambda (x)
	      (* lambda kappa
		 (expt (* lambda x) (1- kappa))
		 (exp (- (expt (* x lambda) kappa))))))
    (send distr :slot-value 'distribution-function 
	  nil)
    (send distr :slot-value 'hazard-function 
	  #'(lambda (x)
	      (* lambda kappa
		 (expt (* lambda kappa) (1- kappa)))))
distr))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to build normal mixtures
;;; e.g. those used in Marron-Wand (1992) Annals of Stat.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-normal-mixture (params &rest args)
"Args: params &rest args
 params must have the form ((w1 m1 ss1) (w2 m2 ss2) ...),
 where w are the weights, m are the means and ss are the stdevs.
 Args are to be passed to the distr-proto :new method."
(let* ((distr (send distr-proto :new))
       (distr-list (mapcar 
		    #'(lambda (trio)
			(apply #'make-normal-distribution
			       (second trio) (third  trio) args))
		    params))
       (denslist (mapcar
		  #'(lambda (obj) (send obj :slot-value 'density-function))
		  distr-list))
       (cdflist (mapcar
		  #'(lambda (obj) (send obj :slot-value 'distribution-function))
		  distr-list))
  (weights (first (transpose params))))
  (send distr :slot-value 'density-function
	#'(lambda (x)
	    (apply #'+ (* weights
			  (mapcar #'(lambda (fun)
				      (funcall fun x))
				  denslist)))))
  (send distr :slot-value 'distribution-function
	#'(lambda (x)
	    (apply #'+ (* weights
			  (mapcar #'(lambda (fun)
				      (funcall fun x))
				  cdflist)))))
  (send distr :slot-value 'random-generator
	#'(lambda (num)
	    (apply #'append
		   (mapcar 
		    #'(lambda (trio)
			(+ (second trio)
			   (* (let ((nn (normal-rand 
					 (round (* num (first trio))))))
				(unless nn
					(error
					 "Don't use a sample size so small"
					 nil))
				nn)
			      (third trio))))
		    params))))
  (let* ((supports (transpose 
		    (mapcar #'(lambda (dd) (send dd :slot-value 'support))
			    distr-list)))
	 (left (min (first supports)))
	 (right (max (second supports))))
    (send distr :slot-value 'support (list left right)))
  distr))


(defun make-marron-wand (num)
  (let ((distr (make-normal-mixture 
                (nth num marron-wand-params)))) ;;; see later for these params
    (send distr :slot-value 'mean 0)
    (send distr :slot-value 'title (format nil "Marron-Wand #~a" num)) 
    distr))

(defvar marron-wand-names
  '(nil
    "Gaussian Density"
    "Skewed Unimodel Density"
    "Strongly Skewed Density"
    "Kurtotic Unimodal Density"
    "Outlier Density"
    "Bimodal Density"
    "Separated Bimodal Density"
    "Asymetric Bimodal Density"
    "Trimodal Density"
    "Claw Density"
    "Double Claw Density"
    "Asymetric Claw Density"
    "Asymetric Db. Claw Density"
    "Smooth Comb Density"
    "Discrete Comb Density"))

(setq marron-wand-params
      (list nil ;for number 0
            '((1 0 1))
            (list '(0.2 0 1) (list 0.2 0.5 (/ 2 3) )
                  (list (/ 3 5) (/ 13 12) (/ 5 9)))
 ; strongly skewed #3
            (mapcar #'(lambda (i) 
                        (list (/ 1 8) 
                              (* 3 (1- (expt (/ 2 3) i)))
                              (expt (/ 2 3) i)))
                    (iseq 8))
; kurtotic unimodal #4
            (list (list (/ 2 3) 0 1)
                  (list (/ 1 3) 0 (/ 1 10)))
; outlier #5
            (list (list (/ 1 10) 0 1)
                  (list (/ 9 10) 0 (/ 1 10)))
; bimodal #6
            (list (list (/ 1 2) -1 (/ 2 3))
                  (list (/ 1 2) 1 (/ 2 3)))
; separated bimodal #7
            (list (list (/ 1 2) -1.5 (/ 1 2))
                  (list (/ 1 2) 1.5 (/ 1 2)))
; skewed bimodal #8
            (list (list (/ 3 4) 0 1) 
                  (list (/ 1 4) 1.5 (/ 1 3)))
; trimodal #9
            (list (list (/ 9 20) (/ -6 5) (/ 3 5)) 
                  (list (/ 9 20) (/ 6 5) (/ 3 5))  
                  (list (/ 1 10) 0 (/ 1 4)))
; claw #10
            (cons (list 0.5 0 1)
                  (mapcar #'(lambda (i) (list 0.1 (- (/ i 2) 1) 0.1))
                          (iseq 0 4)))
; double claw #11
            (append (list (list (/ 49 100) -1 (/ 2 3)))
                    (list (list (/ 49 100) 1 (/ 2 3)))
                    (mapcar #'(lambda (i) (list (/ 1 350)
                                                (/ (- i 3) 2)
                                                0.01))
                            (iseq 0 6)))
; #12
	    (append '((0.5 0 1))
		    (mapcar #'(lambda (i) (list (/ (expt 2 (- 1 i)) 31)
						(+ i 0.5)
						(/ (expt 0.5 i) 10)))
			    (iseq -2 2)))
; #13
	    (append (mapcar #'(lambda (i) (list 0.46
						(- (* 2 i) 1)
						(/ 2 3)))
			    (iseq 0 1))
		    (mapcar #'(lambda (i) (list (/ 1 300)
						(/ i -2)
						0.01))
			    (iseq 1 3))
		    (mapcar #'(lambda (i) (list (/ 7 300)
						(/ i 2)
						0.07))
			    (iseq 1 3)))

; smooth comb #14
	    (mapcar #'(lambda (i) (list (/ (expt 2 (- 5 i)) 63)
					(/ (- 65 (* 96 (expt 0.5 i))) 21)
					(/ 0.507936507936507937
					   (expt 2 i))))
		    (iseq 0 5))
; #15
	    (append (mapcar #'(lambda (i) (list (/ 2 7)
						(/ (- (* 12 i) 15) 7)
						(/ 2 7)))
			    (iseq 0 2))
		    (mapcar #'(lambda (i) (list (/ 1 21)
						(/ (* 2 i) 7)
						(/ 1 21)))
			    (iseq 8 10)))
  ))


(defun get-normal-mixture-dialog ()
"creates a normal mixture distribution object asking for the parameters
 via a dialog "
  (let* ((str (format nil 
		      "Enter three numbers for each normal component,~%            proportion mean stdev ~%with only spaces in between,~%one or more components needed~%(you can miss the last proportion)"))
	 (reply (get-string-dialog str :initial "0.5 0 1 0.5 0 2")))
    (when reply
	  (let* ((numlist (eval  (read (make-string-input-stream 
					(strcat "(list " reply ")")) nil)))
		 (lg (length numlist))
		 (arenums t
			  ;;(apply #'and (mapcar #'numberp numlist))
			  )
		 triplets sumprops)
	    (if (not arenums)
		(progn
		  (message-dialog 
		   "You must provide only numbers with spaces in between")
		  nil)
	      (if (= 1 (mod lg 3))
		  (progn
		    (message-dialog 
		     "You must provide 3n or 3n-1 numbers (n>0)")
		    nil)
		(progn
		  (when (= 2 (mod lg 3))
			(setf numlist (append (butlast (butlast numlist))
					      (list 0)
					      (select numlist
						      (list (- lg 2) 
							    (1- lg))))))
		  (setf lg (length numlist))
		  (setf triplets (split-list numlist 3))
		  (setf sumprops (sum (butlast (first (transpose triplets)))))
		  (unless sumprops (setf sumprops 0))
		  (if (or (> sumprops 1)
			  (< sumprops 0))
		      (progn (message-dialog "Proportions not OK")
			     nil)
		    (progn
		      (setf sumprops (- 1 sumprops))
		      (setf (select numlist (- lg 3)) sumprops)
		      (setf triplets (split-list numlist 3))
		      (make-normal-mixture triplets)
		      )))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;              loading             affairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar  *mtp*)
(setq  *mtp* (send mixture-toy-proto :new))

(format t "~%~a~%~a~%~a~%~a~%"
";;;;;; The mixture-toy object has been stored"
";;;;;; in *mtp* special var. Look in the menu "
";;;;;; for an item to choose between density and"
";;;;;; distribution functions."
";;;;;; Please compile this file to fully enjoying it!")
