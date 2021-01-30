From gliu@math.ucla.edu Sat Jan  9 14:06:07 1993
Received: from julia.math.ucla.edu by laplace.sscnet.ucla.edu (4.1/SMI-4.1)
	id AA09059; Sat, 9 Jan 93 14:06:06 PST
Received: from sonia.math.ucla.edu by julia.math.ucla.edu via SMTP 
	(Sendmail 5.61/1.07) id AA07674; Sat, 9 Jan 93 14:06:28 -0800
Return-Path: <gliu@math.ucla.edu>
Received: from oak.math.ucla.edu by sonia.math.ucla.edu via SMTP 
	(Sendmail 5.61/1.07) id AA20481; Sat, 9 Jan 93 14:06:28 -0800
Received: by oak.math.ucla.edu 
	(Sendmail 5.61/1.07) id AA23476; Sat, 9 Jan 93 14:06:25 -0800
Date: Sat, 9 Jan 93 14:06:25 -0800
From: Guanghan Liu <gliu@math.ucla.edu>
Message-Id: <9301092206.AA23476@oak.math.ucla.edu>
To: deleeuw@math.ucla.edu
Subject: GLMER.lsp
Status: R


This is glmer.lsp. 

------------CUT HERE--------------------
(defun glmer (Y X U qv link invlink d-link &key (print t) (restrict nil) 
	(tol 0.0001) (count-limit 200) (output t))

"ARGS: (Y X U qv link invlink d-link &key (print t) (restrict nil) 
	(tol 0.0001) (count-limit 200) (output t))

A function to estimate coefficients and dispersions in a generalized linear 
model with random effects. The following variables are requiired:
Y: list of observations, X: fixed design matrix,  U: random design matrix, 
qv: a vector of sizes of random effects,  the link, invlink, d-link are link, 
inverse link and differential link functions, respectively. 
Options: restrict = t or nil to turn on or off restricted ML method, 
the tol specifies tolerance of convergence, the count-limit specifies 
iteration limit.  You may also turn off the output when you do simulations."

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
   (setf c (length qv))			;; number of random effects
   (setf cq (combine 0 (cumsum qv)))	
   (setf q  (sum qv))			;; total of sizes of random beta's
   (setf XU (bind-columns X U)) 	;; form matrix (X U)
   (setf z (funcall link y)) 		;; calculate z = g(y)
   (setf wvec (/ (^ (funcall d-link y) 2))) ;; (g'(y))^(-2)
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
	     ;; Estimate b for given sigmas, see Schall p722.
	     (est-b (sigx sigu)
		     (setf d (do( (i 1 (+ 1 i))
				  (d (repeat (nth 0 sigu) (nth 0 qv))
			    (combine d (repeat (nth i sigu) (nth i qv)))) )
				((> i (- c 1)) d) ))
		     (let*( (invd (diagonal (/ d)))
			    (XpWX (/ XWX sigx))
			    (UpWX (/ UWX sigx))  
			    (UpWU (/ UWU sigx)) )
		       (setf CC (bind-rows (bind-columns XpWX (transpose UpWX))
					   (bind-columns UpWX (+ UpWU invD))))
		       (setf cz (combine (/ XWz sigx) (/ UWz sigx)))
		       (solve CC cz)
		       ))

	      ;; Calculate vector v, see Schall p722
	      (est-v (CC sigu)
		     (if restrict
			 (setf TT (diagonal (select (inverse CC) 
					       (iseq p (+ p q (- 1)))
					       (iseq p (+ p q (- 1))))))
		         (setf TT (diagonal (inverse (select CC 
					       (iseq p (+ p q (- 1)))
                                               (iseq p (+ p q (- 1))))))))
                         (setf v (repeat 1 c))
			 (dotimes (i c v)
				  (setf vi (sum (select TT (iseq (nth i cq)
						 (nth (+ 1 i) (- cq 1))))))
				  (setf (select v i) vi))
			 (/ v sigu) )

	      ;; Estimate variances of random effects, see Schall p722. 
	      (update-sigu (b v)
			   (setf b2 (select b (iseq p (+ p q (- 1)))))
			   (setf sigu (repeat 1 c))
			   (dotimes (i c sigu)
				    (setf bi (select b2 
			       (iseq (nth i cq) (nth (+ 1 i) (- cq 1)))))
				    (setf (select sigu i)
					  (/ (inner-product bi bi)
					     (- (nth i qv) (nth i v)))))
			   sigu )

	      ;; Estimate variance of fixed effect, see Schall p722.
	      (update-sigx (b v)
			   (let*( (resid (- z (matmult XU b)))
				  (ss (sum (* resid wvec resid))) ) 
			     (if restrict
				 (/ ss (- (+ n (sum v)) p q))
			       (/ ss (- (+ n (sum v)) q)))
			     ))

	      ;; Define Deviance function.
	      (update-Dev (sigx sigu b)
                     (setf w0 (/ wvec sigx))
                     (setf d (do( (i 1 (+ 1 i))
                                  (d (repeat (nth 0 sigu) (nth 0 qv))
                            (combine d (repeat (nth i sigu) (nth i qv)))) )
                                ((> i (- c 1)) d) ))
                     (setf invD (diagonal (/ d)))
                     (if restrict
			    (setf TM0 (+ invD (/ UPU sigx)))
                            (setf TM0 (+ invD (/ UWU sigx))) )
                     (let*( (term1 (* n (log sigx)))
			    (term2 (sum (* qv (log sigu))))
		            (term3 (log (determinant TM0)))
			    (resid (- z (matmult XU b)))
			    (term4 (sum (* resid w0 resid))) )
			(setf b2 (select b (iseq p (+ p q (- 1)))))
			(setf term5 (repeat 1 c))
		  	(dotimes (i c term5)
				(setf bi (select b2
                               (iseq (nth i cq) (nth (+ 1 i) (- cq 1)))))
			       (setf (select term5 i)
				(/ (inner-product bi bi) (nth i sigu))))
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
              (check-dev  (count sigx sigu b)
                          (setf ck-dev (update-Dev sigx sigu b))
                          (format t "Dev=~,4f b=~,4f sx=~,4f su=~,4f~%" 
                                  ck-dev b sigx sigu)
                    )
      )
       

;;; Start iteration loop.
     (do*( (last 1 Dev)
	   (count 0 (+ count 1))
	   (b (repeat 0 (+ p q)) (est-b sigx sigu))
	   (v (repeat 0 c) (est-v CC sigu))
           ;;(ck-dev (^ 10 200) (check-dev count sigx sigu b))
	   (sigu (repeat 1 c) (update-sigu b v))
           ;;(ck-dev (^ 10 200) (check-dev count sigx sigu b))
	   (sigx 1 (update-sigx b v))
           ;;(ck-dev (^ 10 200) (check-dev count sigx sigu b)) 
	   (Dev (^ 10 200) (update-Dev sigx sigu b)) )
         ( (good-enough-p last Dev count)

;;; Print results.
	   (def convergence (<= count count-limit))
	   (def iteration (if convergence count (- count 1)))
	   (def sigu2 sigu)
	   (def sigx2 sigx)
	   (def b1 (select b (iseq p)))
	   (def b2 (select b (iseq p (+ p q (- 1)))))
        (when output
	   (format t"******************************************************~%") 
	   (format t"--------Restrict = ~3a, Tolerance= ~,6f, Iteration-limt=~5d" restrict tol count-limit)
	   (format t"--------~%") 
	   (format t"--------Convergence=~3a, Iteration=~5d" convergence 
	iteration)
	   (format t"--------~%") 
	   (format t "Coefficients of fixed effect:~% ~g;~%~%" b1)
	   (dotimes (i c)
	    (setf bi (select b2 (iseq (nth i cq) (nth (+ 1 i) (- cq 1)))))
	    (format t "Coefficients of random effect ~2d:~% ~g;~%" (+ i 1) bi)
	   )
	   (format t "~%Estimate of Sigma-sq for fixed effect: ~g~%" sigx2)
	   (format t "~%Estimate of Sigma-sq for random effects:~% ~g~%" sigu2)
	   (format t "*****************************************************") 
         ) ))
))

