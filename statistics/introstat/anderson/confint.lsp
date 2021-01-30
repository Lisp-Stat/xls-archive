;;;  Module that illustrates confidence intervals.
;;;  Prepared by Jon E. Anderson, UMM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; New boxplot definitions that allow updating from a slider
;
;  From F. Udina
;;;;;;;;;;;;;;;;;


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
;;;  Here is the start of the simulation module that is a rough approximation
;;;  of the demonstration of how a confidence interval is formed in
;;;  the Activity-Based Statistics Workbook.
;;; 
;;;  The module starts by plotting boxplots of 100 binomial random variables
;;;  with sample size n=20 for probabilities .1 to .9.
;;;
;;;  A slider is then provided to change the sample size n.  Each time the
;;;  slider is activated the boxplot display changes showing how much these
;;;  boxplots change during random sampling.
;;;
;;;  The concept of a confidence interval is demonstrated by choosing sample
;;;  result on the vertical axis, and then placing a horizontal line across
;;;  the boxplots.  The probabilities of the boxplots that are crossed
;;;  represent the confidence interval.
;;;  



(def n 20)
(def a (binomial-rand 100 n .1))
(def b (binomial-rand 100 n .2))
(def c (binomial-rand 100 n .3))
(def d (binomial-rand 100 n .4))
(def e (binomial-rand 100 n .5))
(def f (binomial-rand 100 n .6))
(def g (binomial-rand 100 n .7))
(def h (binomial-rand 100 n .8))
(def i (binomial-rand 100 n .9))

(def box (boxplot (list a b c d e f g h i)))

(defun change-n (n)
   (def a (binomial-rand 100 n .1))
   (def b (binomial-rand 100 n .2))
(def c (binomial-rand 100 n .3))
(def d (binomial-rand 100 n .4))
(def e (binomial-rand 100 n .5))
(def f (binomial-rand 100 n .6))
(def g (binomial-rand 100 n .7))
(def h (binomial-rand 100 n .8))
(def i (binomial-rand 100 n .9))
(send box :new-data (list a b c d e f g h i)))


(sequence-slider-dialog (list 20 30 40 50 60 70 80 90 100) 
              :action #'change-n :title "sample size n")


;;;;;;;;;;;;;;;;;;;;
;;; end of file
;;;;;;;;;;;;;;;;;;;;;;
