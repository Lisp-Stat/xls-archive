;;;  Module to demonstrate a randomized response survey.
;;;  Prepared by Jon E. Anderson, UMM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; New boxplot definitions  from F. Udina
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-boxplot-path (ylo yhi fn &optional (vertical nil))
"given YLO and YHI as bounds, and FN the five numbers, returns a list of
 points to be joined by the boxplot path.
 Fourth optional argument vertical defaults to nil."
(let* ((yme (/ (+ yhi ylo) 2))
       (yoct (/ (- yhi ylo) 8)))
  (labels ((pair (x y) (if vertical (list x y)
			 (list y x))))
	  (transpose (list (pair (nth 0 fn) (- yme yoct))
			   (pair (nth 0 fn) (+ yme yoct))
			   (pair (nth 0 fn) yme)
			   (pair (nth 1 fn) yme)
			   (pair (nth 1 fn) yhi)
			   (pair (nth 2 fn) yhi)
			   (pair (nth 2 fn) ylo)
			   (pair (nth 2 fn) yhi)
			   (pair (nth 3 fn) yhi)
			   (pair (nth 3 fn) yme)
			   (pair (nth 4 fn) yme)
			   (pair (nth 4 fn) (- yme yoct))
			   (pair (nth 4 fn) (+ yme yoct))
			   (pair (nth 4 fn) yme)
			   (pair (nth 3 fn) yme)
			   (pair (nth 3 fn) ylo)
			   (pair (nth 1 fn) ylo)
			   (pair (nth 1 fn) yme)
			   (pair (nth 1 fn) yme)
			   (pair (nth 1 fn) yme))))))


  


(defun boxplot (data &key (title "Boxplot") (variable-labels nil) 
(vertical nil))
"builds a graph object with boxplot diagrams for the given DATA: a sequence,
 a list of sequences or a matrix of columns.
 Key arguments: title, variable-labels, vertical (default nil)."
(let* ((series (cond ((matrixp data) (column-list data))
		     ((numberp (first data)) (list data))
		     (t data)))
       (ns (length series))
       lines plot)
  (dotimes (i ns)
	   (setq lines (cons (make-boxplot-path (+ i 0.6) (+ i 1.4)
				    (fivnum (nth i series))
				    vertical)
			     lines)))
  (setq plot (apply #'plot-lines (first  lines)))
  (dotimes (i (1- ns))
	   (apply #'send plot :add-lines (nth (1+ i) lines)))
  (send plot :adjust-to-data)
  (send plot :add-slot 'data series)
  (defmeth plot :new-data (dt)
    (setf (slot-value 'data) dt)
    (let* ((series (cond ((matrixp dt) (column-list dt))
			 ((numberp (first dt)) (list dt))
			 (t dt)))
	   (ns (length series))
	   lines plot)
      (dotimes (i ns)
	       (setq lines (cons (make-boxplot-path (+ i 0.6) (+ i 1.4)
						    (fivnum (nth i series))
						    vertical)
				 lines)))
      (send self :clear-lines :draw nil)
      (dotimes (i ns)
	   (apply #'send self :add-lines (nth i lines)))
      (send self :adjust-to-data)))
  plot))
  
		    
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  end of new boxplot definitions    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This begins the main part of the randomized response
;;; demonstration module. 
;;;
;;; Initial sample sizes are n1=50, and n2=100
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; theta is prob of the sensitive question being yes
(def theta .1)

(def thetah .1)


;;;  Beginning definitions for sample sizes
;;;  We allow two sample sizes here to more directly observe how
;;;  sample size affects the precision of the estimates.
;;;
(def n2 50)
(def n1 100)
(def en1 100)
(def en2 50)

;;;Beginning values for theta hats
;;;
;;; We simulate 100 experiments here.
;;;
 (def x (binomial-rand 100 n1 (+ .25 (* .5 theta))))
 (def thhat (* 2 (- (/ x n1) .25)))
 (def y (binomial-rand 100 n2 (+ .25 (* .5 theta))))
 (def thhaty (* 2 (- (/ y n2) .25)))

;;; Boxplot of initial theta hats
(def box (boxplot (list thhat thhaty)))

;;;
;;; The following two functions allow us to change sample sizes
;;; in the groups.
;;;;;

(defun change-n1 (n1)
 (def en1 n1)
 (def x (binomial-rand 100 n1 (+ .25 (* .5 thetah))))
 (def thhat (* 2 (- (/ x n1) .25)))
 (send box :new-data (list thhat thhaty))
)


(defun change-n2 (n2)
 (def en2 n2)
 (def y (binomial-rand 100 n2 (+ .25 (* .5 thetah))))
 (def thhaty (* 2 (- (/ y n2) .25)))
 (send box :new-data (list thhat thhaty))
)

;;;
;;; The following commands set up sliders for various sample sizes
;;; for each of the two sample sizes.
;;;;;;
(sequence-slider-dialog (list 25 50 75 100 150 200 300) 
         :action #'change-n1 :title "n1")


(sequence-slider-dialog (list 25 50 75 100 150 200 300) 
         :action #'change-n2 :title "n2")


;;;
;;; This  function changes the true yes response probability
;;; theta for given sample sizes.
;;;

 (defun change-thet (theta)
 (def thetah theta)
 (def x (binomial-rand 100 en1 (+ .25 (* .5 theta))))
 (def thhat (* 2 (- (/ x en1) .25)))
 (def y (binomial-rand 100 en2 (+ .25 (* .5 theta))))
 (def thhaty (* 2 (- (/ y en2) .25)))
 (send box :new-data (list thhat thhaty))
)


;;;
;;; This sets up the slider definition for changing theta.
;;;

(sequence-slider-dialog (list .1 .2 .3 .4 .5 .6 .7 .8 .9) :action #'change-thet :title "theta")


;;;;;;;;;;;;
;;; end of file.
;;;;;;;;;;;;
