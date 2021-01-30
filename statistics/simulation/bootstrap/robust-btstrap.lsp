;;;; Course project:  STA450/2102S, University of Toronto, Spring 1993
;;;; Instructor: N. Reid
;;;; Student: G. Tomlinson
;;;; Project Title:  Bootstrap standard errors for robust regression coefficients



;CODE USED IN EXAMINING STACK LOSS DATA FOR OUTLIERS

(load-data "stack") 			   ; 	load in stack variables
(def sld (list (bind-columns air conc temp) loss)) ;	sld  = all data
(def sld2 (list (bind-columns air  temp) loss))	   ;	sld2 = data - conc 
(load "robustc") ; 	Tierney's robust regression coefficient function
(load "makewf") ;	Tierney's make-weight function: names are shortened
		;	for MS-DOS

;Function to return bootstrap sample: data is list of form (XMATRIX YVECTOR)
(defun bss (data)
  (let* ( (x (select data 0))
          (y (select data 1))
          (x ( if (matrixp  x) 
                  x
                  (apply #'bind-columns x )))
; r is the number of rows
          ( r ( array-dimension x 0))
; c is a vector naming all the columns
          ( c (iseq (array-dimension x 1) ))
; sel is a vector of length r, representing a random sample, with replacement
; from (0,1,...r-1)
          (sel (sample (iseq r)  r t)))
;returns bootstrap sample in the same format as it is given
    (list (select x sel c)
          (select y sel))))
;

; function to calculate mean and standard-deviation for each column of a 
; data matrix x.
(Defun desc (x)
  (let* ((r (iseq  (array-dimension x 0)))
         (c  (array-dimension x 1) )
; stats is a 2 x #columns array to hold mean and std dev of columns of x
         (stats  (make-array (list 2  c))))
   
    (dotimes (i c)
;sets the 1st row element to be the mean 
;and the 2nd row element to be the std dev. of the respective column of x
             (setf (aref stats 0 i) (mean (select x r i)))
             (setf (aref stats 1 i) (standard-deviation  (select x r i))))
    stats))

;calculates estimates of beta from 100 bootstrap samples and puts them in 
;an array  - p;  Out of the 100 bootstrap samples, 1 led to no convergence
;in the robust regression algorithm, even allowing for 40 iterations.  
;Unfortunately, I was not able to see an easy way to avoid this problem.

(def p (list nil nil nil ))

(dotimes ( i 100)
          (let* (( bsld (bss sld ))
                ( pars (robustc 
                 ( select bsld 0 )
                 ( select bsld 1 )
                 ( makewf 'huber ))))
           (setf p (bind-rows p pars)))
;do not return first row as it is (nil nil nil nil)
          (setf p (select p (iseq 1 100) (iseq 4))))

;sample usage of "desc"
(desc p)	; returns a matrix with row1 = mean, row 2 = std. devs. of
		; bootstrap data.

; code to calculate final weights from robust regression  
  (LET* ((X (BIND-COLUMNS AIR TEMP CONC))
         (BETA (ROBUSTC X LOSS (MAKEWF 'HUBER)))
         (FIT (+ (FIRST BETA) (MATMULT X (REST BETA))))
         (RESIDS  (- LOSS FIT))
         (SCALE (/ (MEDIAN (ABS RES)) 0.6745))
         (R (/ RESIDS SCALE)))
    (FUNCALL (MAKEWF 'HUBER) R))

 
