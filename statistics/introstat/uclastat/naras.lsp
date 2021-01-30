
; Create a modified Boxplot method, ala Tierney.

(defmeth scatterplot-proto :add-modified-boxplot (y &key (x 1.0)
					      (width 1.0) (draw t))
  (unless (= 2 (send self :num-variables))
	  (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
         (half-foot (* 0.1 width))
         (fiv (fivnum y))
	 (bot (select fiv 0))
	 (top (select fiv 4))
         (q1 (select fiv 1))
         (med (select fiv 2))
         (q3 (select fiv 3))
	 (iqr (- q3 q1))
         (low (- q1 (* 1.5 iqr)))
         (high (+ q3 (* 1.5 iqr)))
	 (outliers nil))
    (dolist (val y)
	    (if (or (> val high) (<  val low))
		(setf outliers (append outliers (list val)))))
    (when outliers
	  (send self :add-points (list x x) (list bot top))
	  (send self :add-points (repeat x (length outliers)) outliers)
	  (send self :point-symbol (iseq (+ 2 (length outliers)))
		'diamond))
    (send self :plotline (- x half-foot) low  (+ x half-foot) low  nil)
    (send self :plotline (- x half-foot) high (+ x half-foot) high nil)
    (send self :plotline x low x q1   nil)
    (send self :plotline x q3  x high nil)
    (send self :plotline (- x half-box) q1  (+ x half-box) q1  nil)
    (send self :plotline (- x half-box) med (+ x half-box) med nil)
    (send self :plotline (- x half-box) q3  (+ x half-box) q3  nil)
    (send self :plotline (- x half-box) q1  (- x half-box) q3  nil)
    (send self :plotline (+ x half-box) q1  (+ x half-box) q3  nil)))

(defun modified-boxplot (data &key (title "Modified Box Plot"))
"Args: (data &key (title \"Modified Box Plot\"))
DATA is a sequence, a list of sequences or a matrix. Makes a
boxplot of the sequence or a parallel box plot of the sequences
in the list or the columns of the matrix."
  (let ((p (send scatterplot-proto :new 2 :title title :show nil)))
    (setq data
          (cond ((matrixp data) (column-list data))
                ((or (not (listp data)) (numberp (car data))) (list data))
                (t data)))
        (let ((range (get-nice-range (min data) (max data) 4)))
          (send p :range 1 (nth 0 range) (nth 1 range))
          (send p :y-axis t nil (nth 2 range)))
    (send p :range 0 0 (1+ (length data)))

    (dotimes (i (length data))
	     (send p :add-modified-boxplot (nth i data) :x (1+ i)))
    (send p :show-window)
    p))

(defmeth data-variable-proto :plot (&optional (type 0) time-order)
"Method args: (& optional (TYPE 0))
Makes a histogram of the data if TYPE is 0, which is the
default. If TYPE is 1, we make a boxplot, if TYPE=2 a
kernel density plot, if TYPE=3 then a time-series plot, in which
case time-order can be specified by a list of integers."
(cond
((= type 0) (histogram (send self :data) :title (send self :title)))
((= type 1) (boxplot (send self :data) :title (send self :title)))
((= type 2) (plot-lines
	     (kernel-dens (send self :data) :title (send self :title))))
((= type 3) (let* ((y (send self :data))
		   (n (length y))
		   (x (if time-order
			  (coerce time-order 'list)
			(iseq 1 n))))
	      (if (= (length x) n)
		  (let ((z (plot-points x y)))
		    (send z :title (send self :title))
		    (send z :add-lines (list x y))
		    z)
		(progn
		  (error "Time sequence length not equal to data length")
		  (top-level)))))
((= type 4) (modified-boxplot (send self :data) :title (send self :title)))
))

(defmeth data-variable-proto :histogram ()
  (send self :plot 0))

(defmeth data-variable-proto :boxplot ()
  (send self :plot 1))

(defmeth data-variable-proto :modified-boxplot ()
  (send self :plot 4))

(defmeth data-variable-proto :time-series-plot (&optional time-order)
  (if time-order
      (send self :plot 3 time-order)
    (send self :plot 3)))

;; Example
(def example-1.1 (send data-variable-proto :new
		       '(28 22 36 26 28 28
			 26 24 32 30 27 24
			 33 21 36 32 31 25
			 24 25 28 36 27 32
			 34 30 25 26 26 25
			 -44 23 21 30 33 29
			 27 29 28 22 26 27
			 16 31 29 36 32 28
			 40 19 37 23 32 29
			 -2 24 25 27 24 16
			 29 20 28 27 39 23)
		       :title "Newcomb's coded measurements of Speed of Light"))




