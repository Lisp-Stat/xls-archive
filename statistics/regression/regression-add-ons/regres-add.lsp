;;;; Course project:  STA450/2102S, University of Toronto, Spring 1993
;;;; Instructor: N. Reid
;;;; Student: N. Chapman
;;;; Project Title:  Some additions to Tierney's regression-model-proto


;;;
;;; Methods for testing the general linear hypothesis 
;;;	H: C*beta-gamma=0 vs A: C*beta-gamma/=0 and 
;;;	printing out a table of the usual test requests.
;;;

(defmeth regression-model-proto :gen-lin-hyp (C gamma)
"Message args: (C gamma)
Returns the value of the F statistic and the associated p-value for the
test H: CB-gamma=0 vs. A: CB-gamma/=0 . "
        (let ((xtxinv (send self :xtxinv))
              (b (send self :coef-estimates))
              (s2 (^ (send self :sigma-hat) 2))
              (p (send self :num-coefs ))
              (m (array-dimension C 0))
              (df (send self :df)) )
        (when (and (= (array-dimension C 1) p)
                        (= (length gamma) (array-dimension C 0)))
          (setf Cbg (- (matmult C b) gamma))
          (setf CxtxC (matmult C xtxinv (transpose c)))
          (setf F (if (> m 1) (/ (matmult Cbg (inverse CxtxC) Cbg)
                                                           (* m s2))
                              (/ (* Cbg (/ 1 CxtxC) Cbg) s2)))
	  (list F (- 1 (f-cdf F m df))) )))

(defmeth regression-model-proto :standard-tests ()
"Message args: ()
Returns test results (F or t statistics and p-values) for some frequently
requested tests; H: beta(j)=0 vs A: beta(j)/=0   and  H: betas(all except
intercept)=0 vs A: not all 0 ."
        (let ((p (send self :num-coefs)))
        (format t "~% STANDARD TESTS: ~2%")
        (when (send self :intercept)
           (setf C (bind-columns (make-array (- p 1) :initial-element 0)
                                  (identity-matrix (- p 1))))
           (setf test (send self :gen-lin-hyp C
                                (make-array (- p 1) :initial-element  0)))
           (format t "Hypothesis:                F-value          t-value
   p-value~%")
           (format t "All B_j (j/=0) =0        ~6g         ~6G         ~6g~2%"
                          (first test) (sqrt (first test)) (second test)))
        (setf C (identity-matrix p))
        (dotimes (i  p)
                (setf testC (select C i (iseq 0 (- p 1))))
                (setf gamma (list 0))
                (setf test (send self :gen-lin-hyp testC gamma))
                (format t "B_~a = 0                  ~6g         ~6g        ~6g~%"
                       i (first test) (sqrt (first test)) (second test))) ))

;;;
;;;	Produces a normal plot of the residuals for the model
;;;

(defmeth regression-model-proto :normal-plot ()
        (let* ((res (send self :residuals))
               (ranks (rank res))
               (n (length res))
               (nq (normal-quant (/ (+ ranks 1) (+ n 1)))))
            (plot-points nq res
                         :title "LS Normal Plot")))

;;;
;;;	Some additional outlier diagnostics; DFBETAs and DFITS. Formulae
;;;	used are the standardized versions from Srivastava and Sen.
;;;	Also, a method for printing out a table of the externally 
;;;	studentized residuals, leverages, DFBETAs and DFITs for each obs.
;;;

(defmeth regression-model-proto :dfbetas ()
"Message args: ()
Returns an n*p matrix of the DFBETAS(i,j) values."
        (let* ((x (send self :x-matrix))
               (xtxinv (send self :xtxinv))
               (A (matmult xtxinv (transpose x)))
               (h (send self :leverages))
               (e* (send self :externally-studentized-residuals))
               (n (length e*))
               (p (send self :num-coefs))
               (A (transpose A)))
         (dotimes (i n A)
           (setf const (* (elt e* i) (^ (- 1 (elt h i)) 0.5)))
           (dotimes (j p)
             (setf const2 (^ (select xtxinv j j) -0.5))
             (setf (select A i j) (* const const2 (select A i j) ) )))  ))

(defmeth regression-model-proto :dfits()
"Message args: ()
Returns a list (length n) of the DFITS values."
        (let ((h (send self :leverages))
              (e* (send self :externally-studentized-residuals)))
           (* (^ (/ h (- 1 h)) 0.5) e*) ))

(defmeth regression-model-proto :outlier-diagnostics ()
"Message args: ()
Returns a table of externally studentized residuals, leverages DFbetas
and dfits."
        (let* ((e* (send self :externally-studentized-residuals))
               (h (send self :leverages))
               (dfb (send self :dfbetas))
               (dfits (send self :dfits))
               (cases (send self :case-labels))
               (n (send self :num-cases))
               (p (send self :num-coefs))
               (titles (send self :predictor-names)))

        (format t "~% OUTLIER DIAGNOSTICS ~2%")
        (format t "                                    DFBETAs~%")
        (format t "Obs.      e*        h    ")
        (when (send self :intercept)
              (format t "       Const.   "))
        (dotimes (i (- p 1))
                (format t "~12a" (elt titles i)))
        (format t "    DFITs")
        (dotimes (i n)
                (format t "~%~3g" (select cases i))
                (format t "~12,4g" (elt e* i))
                (format t "~12,4g" (elt h i))
            (dotimes (j p)
                (format t "~12,4g" (select dfb i j)))
                (format t "~12,4g" (elt dfits i)) )))




