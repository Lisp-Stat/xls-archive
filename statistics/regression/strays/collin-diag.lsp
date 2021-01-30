;; This file contains an add-on to regression-model-proto, namely a method for
;; collinearity diagnostic:
;; If reg is a regression-model-object, then
;; (send reg :collin-diag) computes condition indices and variance
;; decompostion proportions (Belsley et al.), tolerances and VIF's.
;; Called as (send reg :collin-diag :vif nil) no tolerances and VIF's are
;; printed.
;; The methode uses three functions, 'Pi-matrix', 'tolerance' and 'VIF':
;;
;; (Pi-matrix X) 
;;     first examines if first column of X is a column of ones,
;;     if not it includes the column of ones. Data in (augmented) X are 
;;     _not_ centered, but scaled to unit column-length as recommended 
;;     by Belsley et al. Then condition indices and variance decompostion 
;;     proportions. are computed and printed.
;;     If X was originally centered, its columns are orthogonal to the column
;;     of ones and adding this column doesn't change the diagnostic for the
;;     remaining variables.
;;
;; (tolerance X) and (VIF X)
;;     First examine if X contains a column of ones as first column, if so
;;     deletes it and comuptes the diagnostics. 
;;
;;
;; Author:         Bernhard Walter 
;; First version:  17.3.1995
;; modified:       
;;
;; for any ideas, bugs, etc, please contact
;;
;; walter@pollux.edv.agrar.tu-muenchen.de
;;

(provide 'collin-diag)

(defmeth regression-model-proto :collin-diag(&key (vif t))
"Arguments: (&key (vif t))
Compute the collinearity diagonstics (Condition Indices and Variance 
Proportions). When 'vif' is T, tolerance and VIF's are printed, too."
  (let* ( (x      (send self :x-matrix))
          (incl   (send self :included))
	  (x-used (apply #'bind-rows (select (row-list x) (which incl)))))
    (Pi-matrix x-used)
    (when vif (vif x-used))))

(defun Pi-matrix(Xin)
"Arguments: (X)
Compute the collinearity diagonstics of a matrix X
(Condition Indices and Variance Proportions)."
  (if (matrixp Xin)
     (let* 
        ( (X      (if (= 0 (standard-deviation 
			    (first (column-list Xin)))) ; with intercept
		      Xin
		      (apply #'bind-columns 
			     (repeat 1 (first (array-dimensions Xin)))
			     (column-list Xin)))) ; add intercept

	  (Xu     (scale-to-unit-columns X)) ; scale to column-length 1
          (svd    (sv-decomp Xu))            ; SVD of the equilibrated matrix
          (mu     (second svd))              ; The singular values
          (eta    (/ (max mu) mu))           ; The contion indices eta
          (mu2    (coerce (^ mu 2) 'list))   ; The 'eigenvalues'
          (V2     (^ (third svd) 2))         ; Matrix with elements (v_ij)^2
          (Phi_kj (apply #'bind-columns 
		         (mapcar #'(lambda(vi mi) (/ vi mi)) 
				 (column-list V2) mu2)))
          (Phi_k  (mapcar #'sum (row-list Phi_kj)))
          (Pi-Mat (column-list 
		     (apply #'bind-rows 
			    (mapcar #'(lambda(xi mi) (/ xi mi)) 
				 (row-list Phi_kj) Phi_k))))
	  (p     (length eta))
          (i1   0)
        )

        (format t "~%")
        (format t " Cond- |  Variance-Component-Proportions of ~%")
        (format t " Index |   ")
        (dotimes (i p) (format t "  b~2s    " i))
        (format t "~%")        
        (format t "-------+")
        (dotimes (i p) (format t "---------"))
        (format t "~%")
        (dolist (l1 (coerce eta 'list))
		(format t "~6d |" (round l1))
		(dolist (l2 (coerce (select Pi-mat i1) 'list))
			(format t "~8,3f " l2) )
		(setf i1 (1+ i1))
		(format t "~%"))           
        (format t "~%")
	)
    (format t "matrix wanted ! ~%")))

(defun tolerance (Xin)
"Arguments: (X)
Compute the Tolerance values of matrix X as a collinearity diagnostic."
  (if (matrixp Xin)
      (let* ( (X  (if (= 0 (standard-deviation (first (column-list Xin))))
		      (apply #'bind-columns (rest (column-list Xin)))
		    Xin))
	      (Xc (scale-to-unit-columns (mean-center X))))
	(/ (diagonal (inverse (matmult (transpose Xc) Xc)))))
    (format t "matrix wanted ! ~%")))

(defun VIF (Xin)
"Arguments: (X)
Compute the Tolerance values and Variance Inflation Factors (VIF's) of 
matrix X as a collinearity diagnostic and print results."
  (if (matrixp Xin)
      (let* ( (int (= 0 (standard-deviation (first (column-list Xin))))) 
	      (X   (if int                              ; if intercept...
		       (apply #'bind-columns 
			      (rest (column-list Xin))) ; ... remove it
		       Xin))
	      (tol (tolerance X)))
	(format t "Variable  |  Tolerance       VIF~%")
	(format t "-------------------------------------~%")
	(when int
	      (format t "Intercept |   .         ~12,7f~%" 0.0))
	(dotimes (i (length tol))
		 (format t "  X~1d      |  ~9,7f  ~12,7f~%" 
			 (1+ i) (elt tol i) (/ (elt tol i))))
	(format t "~%"))
    (format t "matrix wanted ! ~%")))



(defun scale-to-unit-columns(X)
  (apply #'bind-columns
	 (mapcar #'(lambda(xi)
		      (/ xi (sqrt (sum (^ xi 2)))))
		 (column-list X))))

(defun mean-center(X)
  (apply #'bind-columns
	 (mapcar #'(lambda(xi) (- xi (mean xi)))
		     (column-list X))))




