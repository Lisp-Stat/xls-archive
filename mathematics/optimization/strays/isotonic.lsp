;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In this file we collect various monotone/isotone regression
;; routines. Some of them do the same thing, but in different ways,
;; and I included them all because they illustrate various
;; computing and programming techniques.
;;
;; I pulled all this together in August 1995, modifying some previous
;; attempts.
;;
;; Version 1.0 -- 08/06/95 --
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the pool-adjacent-violators algorithm for weighted least
;; squares monotone regression, implemented in a single defun. It
;; is a fairly straightforward implementation, although it is
;; recursive. It only does simple orders.
;;
;; I wrote this years ago, but improved it a little bit.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monotone-regression 
  (values &optional (weights (repeat 1 (length values)))
          (blocks (repeat 1 (length values))))
"ARGS: (values &optional weights blocks)
This takes one, two, or three lists of equal length. It then
computes the monotone regression of the first sequence, using
weights in the second (optional) sequence. The third (optional) 
sequence is a work array indicating size of the blocks in the 
monotone regression. In the first call this is usually just a 
vector of ones, but it changes in the recursive calls to the 
function."
  (let* ((n (length values))
         result
         (last-right (if (= n 1) nil 
                         (position t (< (difference values) 0)))))
    (if (null last-right) 
        (dotimes (i n result)
                 (setf result 
                       (append result (repeat (elt values i) 
                                              (elt blocks i)))))
        (let* ((first-wrong (1+ last-right))
               (head (iseq last-right))
               (tail (if (= n (1+ first-wrong)) nil
                         (iseq (1+ first-wrong) (1- n))))
               (u (elt values last-right))
               (v (elt values first-wrong))
               (a (elt weights last-right))
               (b (elt weights first-wrong))
               (p (elt blocks last-right))
               (q (elt blocks first-wrong))
               (r (+ a b))
               (h (+ p q))
               (s (/ (+ (* a u) (* b v)) r)))
          (setf values (append (select values head) 
                               (list s) 
                               (select values tail)))
          (setf weights (append (select weights head) 
                                (list r) 
                                (select weights tail)))
          (setf blocks (append (select blocks head) 
                               (list h) 
                               (select blocks tail)))
          (monotone-regression values weights blocks)
          )
        )
    )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is another pool-adjacent-violators algorithm, but written
;; in a much more functional style, and set up for considerable
;; greater generality. It allows for bounds, and for an arbitrary
;; function of the form $h(x) = \sum_{i=1}^n h_i(x)$ with each of
;; the $h_i(x)$ strictly unimodal. It still covers only the case
;; of a simple order.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pool-adjacent-violators (fit-in-bounds &optional bounds blocks)
  (let* ((v (funcall fit-in-bounds blocks))
         (last-right (if (single v) nil 
                       (position t (< (difference v) 0))))) 
   (if (null last-right) (blow-me-up v blocks)
       (progn
        (setf blocks (change-blocks last-right blocks)
              bounds (change-bounds last-right bounds))
        (pool-adjacent-violators fit-in-bounds bounds blocks)))
   )
  )

(defun blow-me-up (v blocks)
  (let (result
        (n (length v)))
    (dotimes (i n result) 
             (setf result 
                   (append result (repeat (elt v i) 
                                          (length (elt blocks i)))))
             )
    )
  )

(defun change-blocks (last-right blocks)
  (let ((result 
         (select blocks (which (/= last-right 
                                   (iseq (length blocks)))))))
    (setf (elt result last-right)
          (append (elt blocks last-right)
                  (elt blocks (1+ last-right))))
    result
    )
  )

(defun change-bounds (last-right bounds)
  (let ((result 
         (select bounds (which (/= last-right 
                                   (iseq (length bounds)))))))
    (setf (elt result last-right)
          (list (max (first (elt bounds last-right))
                     (first (elt bounds (1+ last-right))))
                (min (second (elt bounds last-right))
                     (second (elt bounds (1+ last-right))))))
    result
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This are examples of how the previous routines are used.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun least-squares-isotonic-regression 
  (values &optional 
          (weights (repeat 1 (length values))) 
          (bounds (repeat 
                   (list 
                    (list negative-infinity positive-infinity))
                   (length values)))) 
  (pool-adjacent-violators #'(lambda (blocks)
                               (weighted-average weights values blocks))
                           bounds
                           (mapcar #'list (iseq (length values))))
  )
		
(defun weighted-average (weights values blocks)
  (mapcar #'(lambda (x) (/ (sum (select (* weights values) x)) 
                           (sum (select weights x))))
          blocks)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This gets away from the case of simple order -- it can handle
;; arbitrary partial orders, and arbitrary strictly unimodal
;; functions which need not be seperable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun van-eeden (restrictions partition)
  (if (null restrictions) 
      (setf solution (minimum-in-bounds partition))
      (if (test-restriction (first restrictions) 
                            (setf restrictions (rest restrictions)
                                  solution (van-eeden restrictions partition)))
          (setf partition (redefine-partition partition (first restrictions))
                restrictions (rest restrictions)
                solution (van-eeden restrictions partition))
          )
      )
  )
              
(defun minimum-in-bounds (partition)
  (pmax (pmin y (mapcar #'second bounds))
        (mapcar #'first bounds))
  )
  
(defun test-restriction (active solution)
  (let ((a (elt solution (first active)))
        (b (elt solution (second active))))
    (<= a b)
    )
  )


(defun redefine-partition (partition active)
(format t "scream~%")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dykstra (JASA, 78, 1983, 837-842) discussed this algorithm to
;; project on the intersection of r closed convex sets. Further
;; details and references are in Mathar, Cyclic Projections in
;; Data Analysis, Operations Research Proceedings 1988, Spinger,
;; 1989.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dykstra-projection (x0 projections
                              &optional (weights (repeat 1 (length x0)))
                              (eps 1e-6))
  (let* ((r (length projections))
         (p (length x0))
         (m (repeat (list (repeat 0 p)) r))
         (x (repeat (list x0) (1+ r))))
    (loop
     (setf (elt x 0) (elt x r))
     (dotimes (i r)
              (let ((k (1+ i))
                    (f (elt projections i)))
                (setf 
                 (elt x k) (funcall f (- (elt x i) (elt m i)) weights)                   
                 (elt m i) (- (elt x k) (- (elt x i) (elt m i))))
                )
              )
     (if (> eps (max (abs (- (elt x 0) (elt x r)))))
         (return (elt x r)))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The Dykstra algorithm can be applied to least-squares 
;; monotone regression. This allows for weights and arbitrary partial
;; orders -- coded as pairs (i,j) where we require $x_i <= x_j$.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monotone-dykstra-regression 
  (x &optional 
     (weights (repeat 1 (length x))) 
     (pairs (simple-order (length x)))
     (eps 1e-6))
  (let ((f (mapcar #'make-from-pair pairs)))
    (dykstra-projection x f weights eps)
    )
  )

(defun make-from-pair (pair)
  (lambda (x w)
    (let* ((i (first pair))
           (j (second pair))
           (xi (elt x i))
           (xj (elt x j))
           (wi (elt w i))
           (wj (elt w j)))
      (if (>= xi xj)
          (let ((m (/ (+ (* wi xi) (* wj xj)) 
                      (+ wi wj))))
            (setf (elt x i) m (elt x j) m))
          ) 
      ) 
    x
    )
  )

(defun simple-order (n)
  (mapcar #'(lambda (i) (list i (1+ i))) (iseq (1- n)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The Dykstra algorithm can be applied to minimizing the quadratic
;; $(x-y)'W(x-y)$ over $Ax<=b$, by using each of the constraints in
;; turn. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun general-dykstra-regression 
  (y a b &optional (w (identity-matrix (length y))) (eps 1e-6))
  (let ((f (mapcar #'single-constraint a b)))
    (dykstra-projection x f w eps)
    )
  )

(defun single-constraint (a b)
  (lambda (y w)
    (let ((d (- b (sum (* a y)))))
      (if (> d 0) y
          (let* ((c (solve w a))
                 (l (/ d (sum (* a c)))))
            (+ y (* l c))
            )
          )
      )
    )
  )
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The general Dykstra algorithm above can be applied to fitting a 
;; convex or convex isotone function, allowing for weights. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convex-dykstra-regression 
  (x y &optional 
     (w (identity-matrix (length y)))
     (monotone nil)
     (eps 1e-6))
  (let* ((a (convexity-constraints x monotone))
         (b (repeat 0 (length a))))
  (general-dykstra-regression y a b w eps)
    )
  )

(defun convexity-constraints (x monotone)
  (let ((a (mapcar #'(lambda (k)
                       (single-convexity-constraint x k))
                   (rest (butlast (iseq (length x)))))))
    (if monotone 
        (append a (list (monotonicity-constraint (length x)))) a)
    )
  )
 
(defun monotonicity-constraint (n)
  (let ((a (repeat 0 n)))
    (setf (elt a 0) 1 (elt a 1) -1)
    a
    )
  )
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dykstra also applied basically the same idea to I-projections
;; (i.e. projections of vectors of proportions in the likelihood
;; metric). See Annals of Probability, 13, 1985, 975-984. This
;; is implemented in algorithm AS 228 (Applied Statistics, 36, 1987,
;; 377-383).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dykstra-i-projection (r i-projections &key (eps 1e-6))
  (let* ((k (length i-projections))
         (l (length r))
         (w (repeat (list (repeat 1 l)) k))
         (p (repeat (list (funcall (first i-projections) r)) k)))
    (loop
     (dotimes (i k)
              (let ((j (1+ i)))
                (if (< j k)
                    (setf (elt p j)
                          (funcall (elt i-projections j) 
                                   (/ (elt p i) (elt w j))))
                    (setf (first p)
                          (funcall (first i-projections) 
                                   (/ (the-last p) (first w)))))
                (if (< j k)
                    (setf (elt w j) 
                          (* (elt w j) (/ (elt p j) (elt p i))))
                    (setf (first w)
                          (* (first w) (/ (first p) (the-last p)))))
                )
                    
              )
     (if (> eps (max (abs (- (first p) (the-last p)))))
         (return (the-last p)))
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun the-last (x)
  (first (last x))
  )

(defun single (lst)
  (and (consp lst) (not (cdr lst))))
