From gliu@math.ucla.edu Sat Jan  9 14:07:31 1993
Received: from julia.math.ucla.edu by laplace.sscnet.ucla.edu (4.1/SMI-4.1)
	id AA09079; Sat, 9 Jan 93 14:07:30 PST
Received: from sonia.math.ucla.edu by julia.math.ucla.edu via SMTP 
	(Sendmail 5.61/1.07) id AA07683; Sat, 9 Jan 93 14:07:52 -0800
Return-Path: <gliu@math.ucla.edu>
Received: from oak.math.ucla.edu by sonia.math.ucla.edu via SMTP 
	(Sendmail 5.61/1.07) id AA20507; Sat, 9 Jan 93 14:07:52 -0800
Received: by oak.math.ucla.edu 
	(Sendmail 5.61/1.07) id AA23539; Sat, 9 Jan 93 14:07:48 -0800
Date: Sat, 9 Jan 93 14:07:48 -0800
From: Guanghan Liu <gliu@math.ucla.edu>
Message-Id: <9301092207.AA23539@oak.math.ucla.edu>
To: deleeuw@math.ucla.edu
Subject: GLMERC.lsp
Status: R


This is the glmerc.lsp.

--------CUT HERE-------------
(defun glmerc (Y X U q link invlink d-link &key (print t) (restrict nil)
	(tol 0.0001) (count-limit 200) (output t))

"ARGS: (Y X U q link invlink d-link) &key (print t) (restrict nil)
	(tol 0.0001) (count-limit 200) (output t)

A function to estimate coefficients and dispersions in a generalized linear 
model with random effects by De Leeuw's method. 
The following variables are requiired:
Y: list of observations, X: fixed design matrix,  U: random design matrix,
qv: a vector of sizes of random effects,  the link, invlink, d-link are link,
inverse link and differential link functions, respectively.
Options: restrict = nil uses FIML method,
print =t or nil to turn on or off print output in each iteration,
the tol specifies tolerance of convergence, the count-limit specifies
iteration limit.  You may turn off the output when you do simulations."

;; Check inputs.

   (unless (matrixp x)
	   (format t "X MUST BE IN MATRIX FORMAT, PLEASE RE-DEFINE IT! ~%")
	   (break) )
   (unless (matrixp u)
	   (format t "U MUST BE IN MATRIX FORMAT, PLEASE RE-DEFINE IT! ~%")
	   (break) )

   (setf Y (coerce Y 'vector)) 		;; vector y
   (setf n (length Y))			;; size of y
   (setf p (array-dimension X 1))	;; size of beta
   (setf XU (bind-columns X U)) 	;; form matrix (X U)
   (setf z (funcall link y)) 		;; calculate z = g(y)
   (setf wvec (/ (^ (funcall d-link y) 2))) ;; weight vector (g'(y))^(-2)
   (setf XW (transpose X)) 
   (dotimes (i n)
     (setf (select XW (iseq p) i) (* (select XW (iseq p) i) (select wvec i))))
   (setf UW (transpose U))
   (dotimes (i n)
     (setf (select UW (iseq q) i) (* (select UW (iseq q) i) (select wvec i))))
   (setf XWX (matmult XW X))                       
   (setf UWX (matmult UW X))
   (setf UWU (matmult UW U))
   (setf XWz (matmult XW z))
   (setf UWz (matmult UW z)) 
   (when restrict 
         (setf invXX (inverse (matmult (transpose X) X)) )
         (setf Pk (- (identity-matrix n) (matmult X invXX (transpose X))) )
         (setf UPU (matmult (transpose U) Pk U))  )
   
;; Define functions.
     (labels( 
	     ;; Estimate b for given variances of beta and b.
	     (est-b (sigx D)
		     (let*( (XpWX (/ XWX sigx))
			    (UpWX (/ UWX sigx)) )  
		       (setf invD (inverse D)) 
		       (setf UpWU (/ UWU sigx))
		       (setf CC (bind-rows (bind-columns XpWX (transpose UpWX))
					   (bind-columns UpWX (+ UpWU invD))))
		       (setf Cz (combine (/ XWz sigx) (/ UWz sigx)))
		       (solve CC Cz)
		       ))

	      ;; Estimate covariance matrix of random effects. 
	      (update-D (b sigx invD)
			   (setf b2 (select b (iseq p (+ p q (- 1)))))
			   (if restrict
                            (setf TM (inverse (+ invD (/ UPU sigx))))
                            (setf TM (inverse (+ invD (/ UWU sigx)))) )
                           (setf mb2 (matrix (list q 1) b2))
		        (+ TM (matmult mb2 (transpose mb2)))
		      )

	      ;; Estimate variance of fixed effect.
	      (update-sigx (b TM)
			   (let*( (resid (- z (matmult XU b)))
				  (ss (sum (* resid wvec resid))) )
			   (cond ((not restrict)
				  (setf tr (sum (diagonal (matmult UWU TM)))) 
			          (setf sigx (/ (+ ss tr) n)) )
				 (restrict
				  (setf tr (sum (diagonal (matmult UPU TM))))
			          (setf sigx (/ (+ ss tr) (- n p)))) )
			    sigx ))
              ;; Define Log Likelihood function.
              (update-Dev (sigx D b)
                     (let*( (UpWU (/ UWU sigx))
			    (w (/ wvec sigx))
			    (invD (inverse D))
                            (term1 (* n (log sigx)))
                            (term2 (log (determinant D)))
                            (term3 (log (determinant (+ invD (/ UWU sigx)))))
                            (resid (- z (matmult XU b)))
                            (term4 (sum (* resid w resid))) 
                            (b2 (select b (iseq p (+ p q (- 1)))))
                            (term5 (matmult b2 invD b2)) )
                        (+ term1 term2 term3 term4 (sum term5))
                      ))
	      ;; Define convergence criterion.
	      (good-enough-p (last new count)
		     (setf rel-err (max (/ (abs (- new last)) 
				(+ tol (abs new)) )))
		     (if (> count count-limit)
			 (if output
	 (format t "~%++++++ Iteration exceeds limit ~3d ++++++~%" count-limit))
			 (if print
         (format t "Iteration= ~4d  Deviance=~g Relative error= ~g~%" 
                count new rel-err)
			))
		     (or (> count count-limit) (and last (< rel-err tol)))
	      	     )
 	      ;; CHECK Deviance.
              (check-dev  (count sigx D b)
                          (setf ck-dev (update-Dev sigx D b))
                          (format t "Dev=~,4f b=~,4f sx=~,4f su=~,4f~%"
                                  ck-dev b sigx D)
                    )
      )

;;; Start iteration loop.
     (do*( (last 1 Dev)
	   (count 0 (+ count 1))
	   (b (repeat 0 (+ p q)) (est-b sigx D))
	   ;;(ck-dev (^ 10 200) (check-dev count sigx sigu b))
	   (D (identity-matrix q) (update-D b sigx invD))
	   ;;(ck-dev (^ 10 200) (check-dev count sigx sigu b))
	   (sigx 1 (update-sigx b TM)) 
	   ;;(ck-dev (^ 10 200) (check-dev count sigx sigu b))
           (Dev (^ 10 200) (update-Dev sigx D b)) )
	 ( (good-enough-p last Dev count)

;;; Print results.
           (def convergence (<= count count-limit))
           (def iteration (if convergence count (- count 1)))
	   (def sigu2 D)
	   (def sigx2 sigx)
	   (def b1 (select b (iseq p)))
	   (def b2 (select b (iseq p (+ p q (- 1)))))
        (when output
	   (format t"******************************************************~%") 
	   (format t"--------Restrict = ~4a, Tolerance= ~,6f, Iteration-limt=~5d" restrict tol count-limit)
	   (format t"--------~%") 
           (format t"--------Convergence=~3a, Iteration=~5d" convergence
        iteration)
           (format t"--------~%")
	   (format t "Coefficients of fixed effect:~% ~g;~%~%" b1)
	   (format t "Coefficients of random effect:~% ~g;~%" b2)
	   (format t "~%Estimate of Sigma-sq for fixed effect: ~g~%" sigx2)
	   (format t "~%Estimate of Sigma-sq for random effects:~%")
           (print-matrix sigu2)
	   (format t "*****************************************************") 
	  ) ))
))

