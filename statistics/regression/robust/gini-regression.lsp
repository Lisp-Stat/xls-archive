;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;
;;; This routine minimizes the Gini Mean Difference of the residuals (ie the
;;; sum of the absolute values of the differences of the residuals).
;;; This is equivalent to the rank-based regression methods of
;;; McKean and Hettmansperger. For a nice treatment, see Olkin and
;;; Yitzhaki, International Statistical Review, 1992, 60, 185-196.
;;; 
;;; The algorithms is IRLS, with a full matrix of weights (which can,
;;; of course, be quite costly).
;;;
;;; Version 1.0 ** February 24 1995 ** Jan de Leeuw
;;; Version 2.0 ** March 25 1995
;;;                Rewritten using weighted-regression-model-proto
;;;                and using a perturbation parameter
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "weighted-regression")

(defun gini-regression (x y &key (eps 1e-6) (perturbation 1e-6)
	(printls nil) (verbose nil) (display t))
(let* ((m (weighted-regression-model x y :intercept nil :print printls)) 
       (b (send m :coef-estimates))
       (n (identity-matrix (length y))))
(loop
   (let* ((r (send m :raw-residuals))
          (h (sqrt (+ (^ (outer-product r r #'-) 2)
                      (^ perturbation 2))))
          (s (sum h))
          (u (- (/ (+ n h)) n))
          (v (- (diagonal (mapcar #'sum (row-list u))) u)))
     (send m :weights v)
     (let ((c (send m :coef-estimates)))
       (if verbose (format t "~20,10f~%" s))
       (if (< (max (abs (- c b))) eps)
           (progn (if display (send m :display))
                  (return m))
         (setf b c)))))
  ))

