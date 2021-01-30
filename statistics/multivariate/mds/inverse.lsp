(defun dist (x)
"Args: configuration
Computes the distance matrix between
the rows of CONFIGURATION."
(let* (
     (c (matmult x (transpose x)))
     (h (diagonal c))
     )
(sqrt (- (outer-product h h #'+) (* 2 c)))
))

(defun bmat (w del d)
"Args: weights dissimilarities distances
Computes the Guttman transform of the matrices
of WEIGHTS, DISSIMILARITIES, and DISTANCES."
(let* (
     (ones (make-array (array-dimensions d) :initial-element 1))
     (nuls (make-array (array-dimensions d) :initial-element 0))
     (indi (if-else (= d 0) ones nuls))
     (offd (* (- 1 indi) w del (/ (+ d indi))))
      )
(- (diagonal (row-sums offd)) offd)
))

(defun vmat (w)
"Args: weights
Transforms WEIGHTS to V matrix"
(- (diagonal (row-sums w)) w))

(defun vinv (v)
"Args: v-matrix
Computes the generalized inverse of V-MATRIX,
assuming non-separability."
(let (
     (a (/ (elt (array-dimensions v) 0)))
     )
(- (inverse (+ v a)) a)
))

(defun one-smacof (w vv del x)
"Args: weights dissimilarities configuration
Carries out a single smacof iteration."
(let* (
     (d (dist x))
     (s (/ (sum (* w (^ (- del d) 2))) 2))
     )
(format t "STRESS: ~15,10f~%" s)
(matmult vv (matmult (bmat w del d) x))
))

(defun smacof (w del &key initial (eps .000001) (plot t))
"ARGS: weights dissimilarities &key initial epsilon
The driver for the smacof program. It takes WEIGHTS
and DISSIMILARTIES. Default is a random initial
configuration, but we also could give INITIAL as a
keyword argument. EPSILON is the stopping criterion."
(let* (
     (del (/ del (sqrt (/ (sum (* w del del)) 2))))
     (n (elt (array-dimensions w) 0))
     (vv (vinv (vmat w)))
     (pplt (if plot (plot-points nil nil)))
     (x (if initial initial (matrix (list n 2) (normal-rand (+ n n)))))
     (y x)
     (e 0)
     )
(loop
(setf y (one-smacof w vv del x))
(setf e (sum (^ (- x y) 2)))
(cond ((< e eps) 
       (format t "~%") (print-matrix y)
       (if plot (send pplt :close))
       (plot-points (column-list y))
       (return "done"))
  (t (if plot (move-plot x y pplt)) (setf x y))))
))

(defun move-plot (x y pplt)
"Args: first last plot
Clears the PLOT, then plots the rows of the matrices
FIRST and LAST, then connect the points from first
with the corresponding points from LAST."
  (send pplt :clear)
  (send pplt :add-points (column-list x) :draw nil)
  (send pplt :add-points (column-list y) :draw nil)
  (send pplt :showing-labels)
  (send pplt :adjust-to-data)
  (send pplt :add-segments (coerce (transpose x) 'list)
        (coerce (transpose y) 'list)))

;;;; general purpose utilities

(defun row-sums (x)
"Args: matrix
Computes the row sums of MATRIX. Returns
a vector."
(coerce (mapcar 'sum (row-list x)) 'vector))

(defun col-sums (x)
"Args: matrix
Computes the column sums of MATRIX. Returns
a vector."
(coerce (mapcar 'sum (column-list x)) 'vector))

(defmeth scatterplot-proto :add-segments (x1 x2)
"Args: first second
FIRST and SECOND are lists consiting of p lists
of n coordinates. All lines from the n points
in FIRST to the correspongin n points in SECOND
are drawn. Taken from Tierney, page 353."
  (let* ((n (length (first x1)))
         (i1 (iseq n))
         (i2 (+ n i1))
         (starts (mapcar #'append x1 x2)))
(send self :add-lines starts :draw nil)
(send self :linestart-next i1 i2)
(send self :linestart-next i2 nil)
(send self :redraw)
))

(defun weighted-null-basis (w x)
"Args: weights matrix
Computes an orthonormal basis for the
null space of MATRIX in the metric WEIGHTS."
(let (
     (n (elt (array-dimensions x) 0))
     (q (elt (qr-decomp x) 0))
     )
(qr-decomp (- (identity-matrix n) (matmult q (transpose q))))
))

(defun weighted-mean-deviation (w x)
"Args: weights matrix
Transforms the columns of MATRIX to deviations from
the weighted mean defined by WEIGHTS."
(let (
     (m (second (array-dimensions x)))
     (sms (column-sums (matmult w x)))
     (nrm (sum w))
     )
(- x (outer-product (/ sms nrm) (repeat 0 m) #'+))
))

(defun weighted-orthogonal (w x)
"Args: weights matrix
Rotates MATRIX such that its columns are orthogonal
in the metric defined by WEIGHTS."
(let* (
      (c (matmult (transpose x) (matmult w x)))
      (n (array-dimensions c))
      (z (transpose (make-array n :initial-contents (eigenvectors c))))
      )
(matmult x z)
))


