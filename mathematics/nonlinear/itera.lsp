;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; computing and showing iterations of a single 
;;; or a couple of functions.
;;; udina, july 7 1995


;;; for testing and demo purposes

(defun r1 (x) (- 10 (* x .5)))
(defun r2 (x) (+ -1 (* x .4)))

(defun test () (itera (list #'r1 #'r2) 0.2 :x-range '(0 3) :y-range '(7 10)))

(defun test2 ()
  (itera (list #'sqrt #'/) 2 :x-range '(0 2.5) :y-range '(0.01 1.5) :max-iterates 15))

(defun test3 ()
  (itera #'sqrt 0.5 :x-range '(0 2.5) :y-range '(-0.5 1.5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun itera (funs from &key x-range y-range (plot t) (print t)
		   (max-iterates 15) (tol 1e-2))
"Args: (funs from &key (plot t) (print t) (max-iterates 15) x-range y-range)
 Given a function or a couple of functions FUNS and a starting number FROM
 iterates the functions in turn until convergence or :MAX-ITERATES is reached
 If :PLOT is T a graph showing the iteration path is given. In the graph, 
 the inverse of the second function is shown along with the first one.
 The second function is optional, it defaults to identity"
(if (not (consp funs))
    (itera (list funs #'identity) from :plot plot
	   :print print :max-iterates max-iterates :tol tol
	   :x-range x-range :y-range y-range)
  (labels
   ((distance (p1 p2)
	      (sqrt (sum (* (- p1 p2) (- p1 p2))))))
   (let* ((ofuns funs)
	  (points (list (list from 0) (list 1e99 1e99)))
	  (now t); flag the current axis
	  (nextarg from)
	  (pl (when plot (send graph-proto :new 2 :show nil)))
	  xs ys xrg yrg )
     (loop
      (when (or (>= (length points) max-iterates)
		(<= (distance (first points) (second points)) tol))
	    (return))
      (setq prevarg nextarg)
      (setq nextarg (funcall (first funs) prevarg))
      (setf points
	    (cons (if now (list prevarg nextarg)
		    (list nextarg prevarg))
	      points))
      (when print
	    (print (first points))
	    )
      (setf funs (append (cdr funs) (list (car funs))))
      (setq now (not now)))
     (setf points (cdr (reverse points)))
     (when plot
	   (setf xs (first (transpose points)))
	   (setf ys (second (transpose points)))
	   (if x-range
	       (setf xrg x-range)
	     (setf xrg (get-nice-range  (min xs) (max xs) 5)))
	   (if y-range
	       (setf yrg y-range)
	     (setf yrg (get-nice-range  (min ys) (max ys) 5)))
	   (send pl :add-lines (list xs ys))
	   (send pl :scaled-range 0 (first xrg) (second xrg))
	   (send pl :scaled-range 1 (first yrg) (second yrg))
	   (send pl :x-axis t t 5)
	   (send pl :y-axis t t 4)
	   (send pl :add-lines
		 (list (rseq (first xrg) (second xrg) 200)
		       (mapcar (first ofuns)
			       (rseq (first xrg) (second xrg) 200))))
	   (send pl :add-lines
		 (list (mapcar (second ofuns)
			       (rseq (first yrg) (second yrg) 200))
		       (rseq (first yrg) (second yrg) 200)))
	   (send pl :scaled-range 0 (first xrg) (second xrg))
	   (send pl :scaled-range 1 (first yrg) (second yrg))
	   (send pl :x-axis t t 5)
	   (send pl :y-axis t t 4)
	   (print xrg)
	   (send pl :show-window) )
     (if plot pl
       points)))))



