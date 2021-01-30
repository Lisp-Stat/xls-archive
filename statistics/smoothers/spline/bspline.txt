;;; the driver and the starting point
(defun bspline (x k p)
"Args: points knots order
This function makes a b-spline basis at POINTS, for
interior KNOTS, of order P (degree P-1). The knots must
satisfy x_min < k_1 < ... < k_m < x_max. In fact, k_j=x_min
for all j<=0, and k_j=x_max for all j>=m+1."
(let* (
       (ma (max x))
       (mb (min x))
       (n (length x))
       (kk (concatenate 'vector (list mb) k (list (+ ma .000001))))
       (ki (< (array-to-nested-list (outer-product x kk #'-)) 0))
       g
       )
  (dotimes (i p g)
         (cond ((= i 0)
                (setf g (make-dummy
                         (mapcar #'first-true ki))))
               (t (setf g (update g x k (1+ i))))))
  ))

(defun make-dummy (x)
"Args: list
Makes a b-spline basis of order 1 (degree 0), i.e. an
indicator matrix, out of a LIST of integers."
(let* (
      (m (max x))
      (y (1+ (iseq m)))
      (n (length x))
      (a (make-array (list n m) :initial-element 1))
      (b (make-array (list n m) :initial-element 0))
      )
(if-else (outer-product x y #'equal) a b)
))
;;; the basic recurrence relation

(defun update (g x k p)
"Args: basis points knots order
Takes a B-spline BASIS of order p-1 defined at
POINTS for given KNOTS, and upgrades it to order p."
(let* (
      (n (length x))
      (m (length k))
      (ma (max x))
      (mb (min x))
      (au (repeat 0 n))
      (gg (column-list g))
      (kk (concatenate 'list (list mb) k (list ma)))
      (h (make-array (list n (+ p m)) :initial-element 0))
      )
(dotimes (j (+ p m))
(let* (
     (la (elt-alt kk (1+ (- j p))))
     (lb (elt-alt kk j))
     (lc (elt-alt kk (1+ j)))
     (ld (elt-alt kk (+ 2 (- j p))))
     (ca (if (= 0 j) au (/ (- x la) (- lb la))))
     (cb (if (= (1- (+ p m)) j) au (/ (- lc x) (- lc ld))))
     (ga (if (= 0 j) au (elt gg (1- j))))
     (gb (if (= (1- (+ p m)) j) au (elt gg j)))
     (aa (+ (* ca ga) (* cb gb)))
     ) 
(setf (select h (iseq n) (list j)) 
      (make-array (list n 1) :displaced-to (coerce aa 'vector)))
))
h))
;;;; some simple utilities

(defun first-true (x)
"Args: sequence
Indicates the index of the first true in a SEQUENCE of
true-false elements."
 (position t x))

(defun elt-alt (x i)
"Args: sequence index
Modification of elt function. If INDEX is larger than length(SEQUENCE),
the function returns the last element of SEQUENCE, 
if INDEX is negative it returns the first element."
(let (
     (n (length x))
     )
(cond 
  ((< i 0) (elt x 0))
  ((>= i n) (elt x (1- n)))
  (t (elt x i)))))
  


