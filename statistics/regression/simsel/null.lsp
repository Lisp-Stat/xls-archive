(defun null (y gamm gam.0 delta.0 eta.0)
"Computes L-criterion for informative prior for null model

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
  gamm, gam.0, delta.0, eta.0 = inputs for informative priors"

    (let*  (
	   (n (array-dimension y 0))
	   (ones (bind-columns (repeat 1 n)))

            (PP.m (%* ones (inverse (%* (transpose ones) ones))
                                   (transpose ones)))
           (p.m (%* (transpose (- y eta.0)) PP.m (- y eta.0)))
           (q.m (%* (transpose y) (- (identity-matrix n) PP.m) y))

	   (lambda.m (/ (+ n (* (- 1 gamm) )) (+ n delta.0 -2)))

	   (t1 (* (+ 1 lambda.m) q.m))
	   (t2 (* gamm (+ gamm lambda.m) p.m)) 
	   (t3 (* lambda.m gam.0))
	   )
      (sqrt (+ t1 t2 t3))
      )
 )
