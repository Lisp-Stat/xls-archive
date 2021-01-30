;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is a much improved L1 (least absolute value) regression
;; function. It now takes the lack of differentiability into
;; account, which may cause convergence to the wrong solution.
;; There is a perturbation parameter. If this parameter is large,
;; we basically compute the LS estimate, if it is close to zero
;; we compute the L1 estimate. The trick is to compute the solution
;; for various values of the parameter. This can be automated
;; quite easily, of course, but that seems somewhat superfluous.
;;
;; For small perturbations the algorithm takes a lot of iterations,
;; but because of the excellent numerical properties of the
;; regression-model proto, it is still very fast.
;;
;; This version returns a regression-model-proto, from which
;; you can read the results. Observe that the residual sum of
;; squares is equal to the sum of the absolute deviations (raw
;; residuals) because of the weights.
;;
;; We may fit in other methods, along the lines of Holland and
;; Welsch, Comm Stat, A6, 1977, 813-827.
;;
;; Version 1.0 ** March 23 1995 ** Jan de Leeuw
;; Version 1.1 ** March 24 1995
;;                Added display parameter.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lav-regression
  (x y &key (eps 1e-6) (perturbation 1e-6) (display t)
     (intercept t) (printls nil) (verbose nil))
(let* ((m (regression-model x y :intercept intercept
                          :print printls))
       (b (send m :coef-estimates)))
  (loop
   (let* ((r (send m :raw-residuals))
          (h (sqrt (+ (^ r 2) 
		      (^ perturbation 2))))
          (s (sum h)))
     (send m :weights (/ h))
     (let ((c (send m :coef-estimates)))
       (if verbose (format t "~20,10f~%" s))
       (if (< (max (abs (- c b))) eps)
           (progn (if display (send m :display))
                  (return m))
         (setf b c)))))
))
       

