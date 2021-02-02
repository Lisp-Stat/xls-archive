(defun nullnon (y n)
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
  n = number of observations
"

  (let*(
       (yQy (%* (transpose y) (- (diagonal (repeat 1 n)) (/ 1 n)) y))
       (df (- n 3))
       (temp (* 2 (- n 1) (/ yQy df) ))
       )
      (select (sqrt temp) 0 0)
  )
) 
