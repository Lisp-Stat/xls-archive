(defun allnoninf (y x alpha)
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
"

   (let* ((n (array-dimension x 0))
	  (p.all (array-dimension x 1))
	  (n.models (- (round (exp (* p.all (log 2)))) 1))
	  (trans nil)
	  (trans.crit 0)

	  ;COMPUTE L.CRIT FOR NULL MODEL
	  (lc (nullnon y n)))

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
		 (labels ((ralpha (alpha) ;USE FOR NEWTONMAX
				  (let* (
					 (y y)
					 (xs xs)
					 (n (array-dimension xs 0))
					 (p (array-dimension xs 1))
					 (ones (repeat 1 n))
 
					 ;TRANSFORM THE X MATRIX USING ALPHA
					 (temp1 (map-elements #'ifalpha alpha
						   (column-list xs)))
					 (temp2 (combine temp1))
					 (temp3 (matrix (list p n) temp2))
 
					 (xalpha (bind-columns ones 
						    (transpose temp3)))
					 (xalphaprime (transpose xalpha))
					 (svxalpha (sv-decomp xalpha))
					 (balpha (select svxalpha 2))
					 (balphaprime (transpose balpha))
					 (sqrtlambda (select svxalpha 1))
					 (lambda (* sqrtlambda sqrtlambda))
					 )
 
				    (setf (select lambda (which (/= 0 lambda)))
					  (/ 1.0 (select lambda 
						       (which (/= 0 lambda)))))
				    (let*  (
					    (lambdaminus (diagonal lambda))
					    (xprimexminus (%* balpha 
						      lambdaminus balphaprime))
					    (pxalpha (%* xalpha xprimexminus 
							 xalphaprime))
					    (qxalpha (- (identity-matrix n) 
							pxalpha))
					    (palpha (sum (diagonal pxalpha)))
					    (yQy (%* (transpose y) qxalpha y))
					    (yQynum (select yQy 0 0))
					    (df (- n palpha 2))
					    (crit (/ yQynum df))
					    )
				      (* -1.0 crit))))
			  )

			  ;COMPUTE L.CRIT FOR GIVEN MODEL
			  (setf lc (combine lc (lcritnon y xs)))

			  ;FIND TRANS THAT MINIMIZE L.CRIT
			  (let* ((newt (nelmeadmax #'ralpha alpha.mod
				  :epsilon .00001 :verbose nil))

                                 ;TRANSFORM THE X MATRIX USING ALPHA
                                 (temp1 (map-elements #'ifalpha newt
                                                 (column-list xs)))
                                 (temp2 (combine temp1))
                                 (temp3 (matrix (list p n) temp2))
				 (for.tc (lcritnon y (transpose temp3)))
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

