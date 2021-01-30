;;; Module demonstrates a capture/recapture experiment.
;;; Prepared by Jon E. Anderson, UMM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; The following boxplot definitions are needed for this module.
;;; Provided by F. Udina
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;; end of  boxplot definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; This module attempts to simulate capture-recapture  experiments 
;;; as described in the Activity-Based Statistics workbook.
;;;
;;; We use the workbook notation for R, n, M, N
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  We initially use m=25, and n=20. The total population size is 
;;;  N=400.  
;;;
;;;  Sliders are available to observe the effects of m and n on the
;;;  estimates of the population size N.  
;;;;;;;;;;;;;;;
(def m 25)

(def em 25)

(def en 20)

(def pop (repeat (list 0 1) (list (- 400 m) m))) 

(def test (list  150 200 300 400 500 600 650))

(def box (boxplot test))


(defun change-n (n)
(def en n)
(def nhat (list ))
(dotimes (i 100)
 (def indx (sample (iseq 0 399) n))
 (def samp (select pop indx))
 (def r (sum samp))
 (cond ( (> r 0)
 (def est  (/ (* em n) r))  
 (def nhat (append nhat (list est)))  )
   (t  )  )
   )
(send box  :new-data nhat)    )


(defun change-m (m) 
(def em m)
(def pop (repeat (list 0 1) (list (- 400 m) m)))
(funcall #'change-n en)
       ) 


(sequence-slider-dialog  (list 20 40 60 80 100 120 140 160
 180 200) :action #'change-n :title "Sample Size")


(sequence-slider-dialog (list 25 50 75 100 125 150 200) :action 
            #'change-m :title "Number Tagged")

