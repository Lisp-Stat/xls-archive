(defun lcritnon (y x)
"Computes l.crit for noninformative prior

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
  x = predictors (matrix format)"

    (let*  (
	   (n (array-dimension x 0))
	   (p (array-dimension x 1))
	   (ones (repeat 1 n))
           (x.mat (bind-columns ones x))
           (xprime (transpose x.mat))
           (svx (sv-decomp x.mat))
           (b (select svx 2))
           (bprime (transpose b))
           (sqrtlambda (select svx 1))
           (lambda (* sqrtlambda sqrtlambda))
	   )

           (setf  (select lambda (which (/= 0 lambda)))
                (/ 1.0 (select lambda (which (/= 0 lambda)))))
           (let*  (
		  (lambdaminus (diagonal lambda))
		  (xprimexminus (%* b lambdaminus bprime))
		  (px (%* x.mat xprimexminus xprime))
		  (qx (- (identity-matrix n) px))
		  (p (sum (diagonal px)))
		  (yQy (%* (transpose y) qx y))
		  (yQynum (select yQy 0 0))
		  (df (- n p 2))
		  (lm (/ yQynum df))
		  (lmsq (* 2 (- n 1) lm))
                  )
	     (sqrt lmsq)
)))
