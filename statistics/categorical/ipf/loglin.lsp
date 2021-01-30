(require "array")

(defun rc-model (x)
"Args: table
Fits Goodman's RC model to the array X. This means fitting a no-second-order
interaction model, with a log-bilinear structure for the first-order (pairwise) interactions."
)

(defun quasi-symmetry (x &key (precision 1e-6))
"Args: table
Fits Caussinus' quasisymmetry model to the array X. This means fitting
a no-second-order interaction model, in which all first-order (pairwise)
interactions are symmetric."
(let* (
      (r (row-sum x))
      (p (repeat 1 (length r)))
      (c (column-sum x))
      (q (repeat 1 (length c)))
      (s (symmetric-part x))
      (n (sum x))
      (f (/ (outer-product r c) n))
      (l (sum (* x (log x))))
      (d0 (* 2 (- l (sum (* x (log f))))))
      (d1 0)
      (i 0)
      )
(format t "Iteration ~d Deviation ~,6f~%" i d0)
(print-matrix (loop
 (setf i (1+ i))
 (setf f (* f (outer-product (/ r (row-sum f)) q)))
 (setf f (* f (outer-product p (/ c (column-sum f)))))
 (setf f (* f (/ s (symmetric-part f))))
 (setf f (* f (/ n (sum f))))
 (setf d1 (* 2 (- l (sum (* x (log f))))))
 (format t "Iteration ~d Deviation ~,6f~%" i d1)
 (if (< (abs (- d0 d1)) precision) (return f) 
       (setf d0 d1))
))
))

(defun no-high-order-interaction (x k &optional (precision 1e-6))
"Args: table
Fits the log-linear model to the array X, with all interactions 
of more than k variables equal to zero. For k=1 this is the
independence model, for k=2 it is the no-second-order interaction
model, and so on."
(let* (
       (n (array-rank x))
       (m (all-tuples n k))
       )
(log-linear x m precision)
))

(defun log-linear (x model &optional (precision 1e-6))
"Args: x mod
Fits the hierarchical linear model MODEL on the array X, 
defined as a list of lists of indices selected 
from (iseq (array-rank x))."
  (let* (
        (dx (array-dimensions x))
        (rx (array-rank x))
        (rm (length model))
        (xsum (repeat nil rm))
        (sx (sum x))
        (zz (make-array dx :initial-element (/ sx)))
        (lr (sum (* x (log x))))
        (d0 (* 2 (- lr (sum (* x (log zz))))))
        (d1 0)
        (it 0)
        )
(dotimes (i rm)
(setf (elt xsum i) (apl-reduction x (elt model i))))
(format t "Iteration ~d Deviation ~,6f~%" it d0)
(loop
 (setf it (1+ it))
 (dotimes (i rm) 
          (let* (
                 (zsum (apl-reduction zz (elt model i)))
                 (xzrt (/ (elt xsum i) zsum))       
                 )
            (setf zz (* zz (array-blow-up dx (elt model i) xzrt)))
            (setf d1 (* 2 (- lr (sum (* x (log zz))))))
)) 
(format t "Iteration ~d Deviation ~,6f~%" it d1)
 (if (< (abs (- d0 d1)) precision) (return zz) 
       (setf d0 d1))
)))
 
(defun row-sum (x)
(mapcar #'sum (row-list x)))

(defun column-sum (x)
(mapcar #'sum (column-list x)))

(defun symmetric-part (x)
(+ x (transpose x)))

(defun all-tuples (n k)
"Args: (n k)
Selects all k-tuples from (iseq n)"
(let (
     (x (mapcar #'list (iseq n)))
     )
(loop (if (= k (length (first x))) (return x)
       (setf x (select-one-more x n))))
))

(defun select-one-more (x n)
(let (
     (m (length x))
     (nn (iseq n))
     y
     )
(dotimes (i m y)
         (let* (
                (xi (elt x i))
                (xm (max xi))
                (xw (which (> nn xm)))
                )
           (dolist (j xw)
                   (setf y (append y (list (append xi (list j))))))
           ))
))