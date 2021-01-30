;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regularized bivariate splines with smoothing and tension according to
;; Mitasova and Mitas (Mathematical Geology, 25, 1993, 641-669)
;; are computed. This is the interpolation method s.surf.tps in
;; GRASS 4.1. Programmed in Xlisp-Stat by Jan de Leeuw, UCLA Statistics,
;; September 2, 1994.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant euler .577215664901532860606512)

(defun thin-plate-spline (x y z &key (tension .05) (smoothing 0))
"Args: (x y z &key (tension .05) (smoothing 0))
Thin-plate spline with smoothing and tension. Matrix X has
sampled points in  two dimensions, vector Z has corresponding
function values. Matrix Y has points where we want to 
interpolate. TENSION and SMOOTHING are parameters regulating
the smooth."
(let* (
      (nn (length z))
      (rn (repeat 1 nn))
      (ez (transpose (make-array (list 2 nn) :initial-contents 
                                 (list rn (coerce z 'list)))))
      (tt (/ (^ tension 2) 4))
      (dd (set-diagonal (* tt (dist-matrix x x)) 1))
      (ee (set-diagonal (+ euler (log dd) (expo-integral dd)) (- smoothing)))
      (aa (column-list (solve ee ez)))
      (ai (/ (sum (second aa)) (sum (first aa))))
      (bb (- (* ai (first aa)) (second aa)))
      (de (* tt (dist-matrix x y)))
      (fe (+ euler (log de) (expo-integral de))) 
      )
(- ai (matmult bb fe))
))

(defun cross-thin-plate (x z &key (tension .05) (smoothing 0))
"Args: (x z &key (tension .05) (smoothing 0))
Cross-validatory evaluation of thin-plate smoothing spline.
Matrix X contains sampled points, vector Z has function
values. Each of the function values is predicted using
the delete-one method, and the prediction-error sum of
squares is returned."
(let* (
       (nn (length z))
       (in (iseq nn))
       (ss 0)
       )
(dotimes (i nn ss)
         (let* (
                (im (remove i in))
                (zz (thin-plate-spline (select x im (iseq 2)) 
                   (select x (list i) (iseq 2))
                   (select z im) :tension tension :smoothing smoothing))
                )
(setf ss (+ ss (sum (^ (- zz (select z i)) 2))))
))))

(defun func-thin-plate (x z &key (tension .05) (smoothing 0) (eps .1) 
                          (nu 6) (nv 6))
"Args: (x z &key (tension .05) (smoothing 0) (eps .1) (nu 6) (nv 6))
Draws interpolating function using thin-plate spline. Actually
NU * NV interpolating points are formed, and the usual Xlisp-Stat
interpolating spline is drawn through these points."
(let* (
       (ax (column-list x))
       (u+ (+ (max (first ax)) eps))
       (u- (- (min (first ax)) eps))
       (v+ (+ (max (second ax)) eps))
       (v- (- (min (second ax)) eps))
       (ru (rseq u- u+ nu))
       (rv (rseq v- v+ nv))
       (hh (make-pairs ru rv))
       (sp (spin-plot (list (first ax) (second ax) z)))
       (vv (thin-plate-spline x hh z 
                              :tension tension :smoothing smoothing))
       )
(send sp :add-surface ru rv 
      (make-array (list nu nv) :displaced-to vv))
))
    
(defun dist-matrix (x y)
"Args (x y)
Computes matrix of squared Euclidean distances between
rows of the matrices X and Y."
(let (
     (c (matmult x (transpose y)))
     (d (row-sums (^ x 2)))
     (e (row-sums (^ y 2)))
     )
(- (outer-product d e #'+) (* 2 c))
))
  
(defun expo-integral (x)
"Args: (x)
Exponential integral for argument X."
(if (< x 1) (expo-integral-1 x) 
  (expo-integral-2 x)))

(defun expo-integral-1 (x)
"Args: (x)
Exponential integral for 0 < X < 1. Uses polynomial
approximation."
(let (
     (a0 -.57721566)
     (a1  .99999193)
     (a2 -.24991055)
     (a3  .05519968)
     (a4 -.00976004)
     (a5  .00107857)
     )
(- (+ a0 
      (* x (+ a1 
              (* x (+ a2 
                      (* x (+ a3 
                              (* x (+ a4 
                                      (* x a5)))))))))) (log x))
))

(defun expo-integral-2 (x)
"Args: (x)
Exponential integral for X >= 1. Uses rational
approximation."
(let (
      (a1 8.5733278401)
      (a2 18.0590169730)
      (a3 8.6347608925)
      (a4 .2677737343)
      (b1 9.5733223454)
      (b2 25.6329561486)
      (b3 21.0996530827)
      (b4 3.9584969228)
      )
(/ (/ (+ a4 (* x (+ a3 (* x (+ a2 (* x a1))))))
   (+ b4 (* x (+ b3 (* x (+ b2 (* x b1))))))) (* x (exp x)))
))

(defun row-sums (x)
"Args: (x)
Returns row sums of matrix X."
(mapcar #'sum (row-list x)))

(defun set-diagonal (x a)
"Args: (x a)
Sets all diagonal elements of the square matrix X
equal to a."
(let (
     (n (first (array-dimensions x)))
     )
(+ (- x (diagonal (diagonal x))) (diagonal (repeat a n)))
))

(defun make-pairs (x y)
"Args: (x y)
Suppose X and Y are sequences of length nx and ny.
Makes an nx*ny by 2 matrix containing all ordered
pairs."
(let* (
       (nx (length x))
       (ny (length y))
       (nn (* nx ny))
       (ry (repeat y nx))
       (rx (combine (mapcar #'(lambda (z) (repeat z ny)) x)))
       )
(transpose (make-array (list 2 nn) :initial-contents (list rx ry)))
))
           

