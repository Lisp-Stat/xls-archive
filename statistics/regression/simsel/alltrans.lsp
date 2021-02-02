(defun alltrans (y x alpha gamm gam.0 delta.0 eta.0)
"Finds optimal transformation and computes the L-criterion for all 
subsets of predictors under informative prior inputs

***********************************************************************
Version 1.1: November 15, 1996
 
This software is not formally maintained, but I would be happy to hear
from people who have problems with it.
 
Permission is hereby granted to StatLib to redistribute this software.
The software can be freely used for non-commercial purposes, and can
be freely distributed for non-commercial purposes only.
The copyright is retained by the developer.
Copyright 1996 Jennifer A. Hoeting
***********************************************************************

INPUTS
  y = response
  x = predictors (matrix format)
  alpha = starting values for transformation optimization,
          list of integers of length equal to the number of columns in x
  gamm, gam.0, delta.0, eta.0 = inputs for informative priors
"

   (let* ((n (array-dimension x 0))
	  (p.all (array-dimension x 1))
	  (n.models (- (round (exp (* p.all (log 2)))) 1))
	  (trans nil)
	  (trans.crit 0)

	  ;COMPUTE L.CRIT FOR NULL MODEL
	  (lc (null y gamm gam.0 delta.0 eta.0)))
					
	  (dotimes (i n.models 
		      (append (list lc) (list trans.crit) (list trans)))
               (let* (
		      ;GET BINARY VERSION OF i
		      (a (repeat (+ i 1) p.all))    
                      (b (floor (/ a (exp (* (iseq p.all) (log 2))))))
		      (c  (rem b 2))

		      ;SELECT APPROPRIATE COLUMNS OF X 
		      (e (select (column-list x) (which (= c 1)))) 
		      (f (combine e))
		      (p (sum c))
		      (g (matrix (list p n) f))
		      (xs (transpose g))
		      (alpha.mod (select alpha (which (= c 1))))
		      )
	 (labels ((ralphainf2 (alpha) ;USE FOR NEWTONMAX
 
		     (let* (
			    (y y)
			    (xs xs)
			    (n (array-dimension xs 0))
			    (p (array-dimension xs 1))
			    (gamm gamm)
			    (gam.0 gam.0)
			    (delta.0 delta.0)
			    (eta.0 eta.0)
			    (ones (repeat 1 n))
				    
			    ;TRANSFORM THE X MATRIX USING ALPHA
			    (temp1 (map-elements #'ifalpha alpha
						 (column-list xs)))
			    (temp2 (combine temp1))
			    (temp3 (matrix (list p n) temp2))
			    (xalpha (bind-columns ones  (transpose temp3)))
 
		      (PP.m (%* xalpha (inverse (%* (transpose xalpha) xalpha))
                                   (transpose xalpha)))
		      (p.m (%* (transpose (- y eta.0)) PP.m (- y eta.0)))
		      (q.m (%* (transpose y) (- (identity-matrix n) PP.m) y))
 
		      (lambda.m (/ (+ n (* (- 1 gamm) (+ p 1))) 
				   (+ n delta.0 -2)))
 
		      (t1 (* (+ 1 lambda.m) q.m))
		      (t2 (* gamm (+ gamm lambda.m) p.m))
		      (t3 (* lambda.m gam.0))
		      (crit (sqrt (+ t1 t2 t3)))
		      )
		    (* -1.0 (select crit 0 0))))
		    )

			  ;COMPUTE L.CRIT FOR GIVEN MODEL
			  (setf lc (combine lc 
				 (lcrit y xs gamm gam.0 delta.0 eta.0)))

			  ;FIND TRANS THAT MINIMIZE L.CRIT
			  (let* ((newt (nelmeadmax #'ralphainf2 alpha.mod
				  :epsilon .00001 :verbose nil))

                                 ;TRANSFORM THE X MATRIX USING ALPHA
                                 (temp1 (map-elements #'ifalpha newt
                                                 (column-list xs)))
                                 (temp2 (combine temp1))
                                 (temp3 (matrix (list p n) temp2))
				 (for.tc (lcrit y (transpose temp3)
						   gamm gam.0 delta.0 eta.0))
				 )

			    ;RETURN TRANSFORMATIONS FOR THIS MODEL
			    (setf trans (append trans (list newt)))

			    ;RETURN L.CRIT FOR TRANSFORMED MODEL
			    (setf trans.crit (combine trans.crit for.tc))
			  )
			  ;PRINT ITERATION NUMBER
			  (print (+ 1 i)) 
			  )
			 )
		 )
)
)  

