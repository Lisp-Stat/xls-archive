(setf *float-format* "%g")
(defun chisq-generator (df n)
  "chisq random generator, df are the degrees of freedom
   and n is the sample size"
  (let* ((ovrall (* df n))
	 (mys (iseq ovrall))
	 (rg1 (normal-generator (/ ovrall 2)))
	 (psample (split-list rg1 df))
	 (d1 (length psample))
	 (d2 (iseq d1))
	 (out (mapcar 
	       #'(lambda (xx)
		   (let* ((cur-sample (select psample xx))
			  (chisq-i (sum (^ cur-sample 2))))
		     chisq-i))
	       d2)))
    out))

(defun random-generator-1 (x y z n)
  (let* ((*ix* x)
	 (*iy* y)
	 (*iz* z))
	   (let* ((lout (mapcar #'(lambda (ii)
	      (let* ((tmp (setf *ix* (mod (* 171 *ix*) 30269)))
		     (tmp (setf *iy* (mod (* 172 *iy*) 30307))) 
		     (tmp (setf *iz* (mod (* 170 *iz*) 30323)))
		     (tmpo (mod (+ (float (/ *ix* 30269.0))
			      (float (/ *iy* 30307.0))
			      (float (/ *iz* 30323.0))) 1.0)))
	   tmpo)) (iseq n)))) lout)))

(defun random-generator-2 (x y z n)
  (let* ((*ix* x)
	 (*iy* y)
	 (*iz* z))
	   (let* ((lout (mapcar #'(lambda (ii)
	      (let* ((tmp (setf *ix* (mod (* 171 *ix*) 30269)))
		     (tmp (setf *iy* (mod (* 172 *iy*) 30307))) 
		     (tmp (setf *iz* (mod (* 170 *iz*) 30323)))
		     (tmpo (mod (+ (float (/ *ix* 30269.0))
			      (float (/ *iy* 30307.0))
			      (float (/ *iz* 30323.0))) 1.0)))
	   tmpo)) (iseq n)))) lout)))


(defun normal-generator (n)
  (let* ((l1 (random-generator-1 1 2 3 n))
	 (l2 (random-generator-2 3 4 5 n))
	 (mys (iseq n))
	 (out (mapcar 
	       #'(lambda (xx)
		   (let* ((U1i (select l1 xx))
			  (U2i (select l2 xx))
			  (R (sqrt  (- (* 2 (log U1i)))))
			  (theta  (* 2 pi U2i))
			  (x1 (* R (cos theta)))
			  (y1 (* R (sin theta))))
		     (list x1 y1)))
	       mys))
	 (out1 (combine out)))
    out1))

(defun random-generator (n)
  "random sample from U(0,1)"
  (let* ((*ix* 5)
	 (*iy* 11)
	 (*iz* 87))
	   (let* ((lout (mapcar #'(lambda (ii)
	      (let* ((tmp (setf *ix* (mod (* 171 *ix*) 30269)))
		     (tmp (setf *iy* (mod (* 172 *iy*) 30307))) 
		     (tmp (setf *iz* (mod (* 170 *iz*) 30323)))
		     (tmpo (mod (+ (float (/ *ix* 30269.0))
			      (float (/ *iy* 30307.0))
			      (float (/ *iz* 30323.0))) 1.0)))
	   tmpo)) (iseq n)))) lout)))

#| This one works with 3 global variables (a, b, c) and has the advantage that in each 
   iteration it produces different values. 
 
(defun random-generator (n)
  "random sample from U(0,1)"
  (let* ((lout (mapcar 
		#'(lambda (ii)
		    (let* ((tmp (setf *ix* (mod (* 171 *ix*) 30269)))
			   (tmp (setf *iy* (mod (* 172 *iy*) 30307))) 
			   (tmp (setf *iz* (mod (* 170 *iz*) 30323)))
			   (tmpo (mod (+ (float (/ *ix* 30269.0))
					 (float (/ *iy* 30307.0))
					 (float (/ *iz* 30323.0))) 1.0)))
		      tmpo)) (iseq n))))
    lout))

(defun init (a b c)
    (setf *ix* a )
    (setf *iy* b)
    (setf *iz* c))
|#


; Failure rate functions from chi square and standard lognormal distribution.
; use (plot-fail <args>) to plot the functions. The arguments are :
; 'c for chi square 'l for log normal. 

; sample run: (plot-fail (chisq-generator 10 1000) (rseq 2 30 80) 1 1 :type 'c)
; log case    (plot-fail (exp (normal-generator 500)) (rseq .1 6 80) 1 1 :type 'l)


(defun chisq-fail (xx df)
    (let* ((num (chisq-dens xx df))
	   (den (- 1 (chisq-cdf xx df)))
	   (hr (/ num den)))
      hr))

(defun log-failure (x)
  (let* ((num (/ (exp (/ (- (^ (log x) 2)) 2))
		 (* x (sqrt (* 2 pi)))))
	 (den (- 1 (normal-cdf (log x)))))
    (/ num den)))

(defun plot-fail (s xx h1 h2 &key (type 'c))
  (let* ((arg-1 (lambda-tilde s xx h1 h2))
	 (pic (plot-lines xx arg-1))
	 (tmp (setf a (send pic :add-lines xx (case type ('c (chisq-fail xx 10))
						         ('l (log-failure xx)))
			    :type 'dashed))))))

(defun biweight (x)
  (mapcar  #'(lambda (x) (cond ((< (abs x) 1) (* (/ 15 16)
						 (^ (- 1 (^ x 2)) 2)))
	(t 0))) x))

(defun epanechnikov (x)
   (mapcar #'(lambda (x) (cond ((< (abs x) (sqrt 5)) (*  (/ 3 4)
				    (/  (- 1 (* (/ 5) (^ x 2))) (sqrt 5))))
			       (t 0)))
	   x))

(defun gaussian (x) (normal-dens x))

(defun rectangular (x)
  (mapcar  #'(lambda (x) (cond ((< (abs x) 1) .5)
	(t 0))) x))

(defun triangular (x)
  (mapcar #'(lambda (x) (cond ((< (abs x) 1) (- 1 (abs x)))
      (t 0))) x))

; Variable bandwidth stuff

(defun lambda-hat (xin xout h1 &key type)
 "The auxilliary estimator (used to make the bandwith vary) defined by
W-L in H.A. I. To be used in lambda-tilde.
Args: xin is the sample (a list) xout is the points
  at which the estimate will be evaluated (a list also) and h is the
  bandwidth"
  (let* ((n (length xin))
         (nn (iseq n))
         (ord-xin (select xin (order xin)))  ;ordered data
	 (k-fun (case type ('b 'biweight) ('g 'gaussian) ('t 'triangular) ;choice of kernel
		           ('e 'epanechnikov) ('r 'rectangular))) ;according to 'type'
         (out (mapcar #'(lambda (i)
                          (sum (/ (funcall k-fun (/ (- i ord-xin) h1))
                                  (+ (- n nn) 1))))
                      xout)))
     (* (/ h1) out)))

(defun lambda-tilde (xin xout h1 h2 &key type )
 "The variable bandwith failure rate estimate. Uses lambda-hat as the
bandwith varying factor Args: 'b 'biweight, 'g gaussian, 't triangular
		              'e epanechnikov 'r rectangular. 
Args: xin is the sample (list)
      xout are the points at which the estimate will be evaluated
      h1 is the bandwith to be used by \hat \lambda(x_{(i)}
      h2 is the bandwith for \tilde \lambda(x)"
  (let* ((n (length xin))
         (nn (iseq n))
         (lhat (lambda-hat xin xin h1 :type type))
         (sqrt-lhat (sqrt lhat))
         (ord-xin (select xin (order xin)))
	 (k-fun (case type ('b 'biweight) ('g 'gaussian) ('t 'triangular)
		           ('e 'epanechnikov) ('r 'rectangular)))
         (out (mapcar #'(lambda (i)
                          (let* ((xout-i (select xout i))
                                 (out-i (sum (/ (* sqrt-lhat
						   (funcall k-fun 
							    (* (/ (- xout-i ord-xin) h2)
							       sqrt-lhat)))
						(+ (- n nn) 1)))))
                            out-i))
                      (iseq (length xout)))))
    (* (/ h2) out)))

(defun plot-all (sample x h1 h2 &key type)
  "This one plots the real failure, the adaptive estimate (dashed) and the transformed"
  (let* ((arg-1 (lambda-tilde sample x h1 h2 :type type))
	 (pic (plot-lines x (chisq-fail x 10)))
	 (tmp (setf b (send pic :add-lines x arg-1 :type 'dashed)))
	 (tmp (setf d (send pic :redraw))))))

; sample run

;  (def yy (chisq-generator 10 1000))
;  (plot-all yy (rseq 1.3 27 15) (pilot-width1 yy) .3 :type 'e)

(defun pilot-width1 (data)
  "returns Silverman's default width 1.06 A n^(-1/5)"
  (let* ((n (length data))
	 (sd (standard-deviation data))
	 (iqr (interquartile-range data))
	 (par (min sd (/ iqr 1.34)))
	 (d-w (* 1.06 par (^ n (/ -1 5)))))
    d-w))

(defun pilot-width3 (data)
  "returns Silverman's default width 0.9 A n^(-1/5)"
  (let* ((n (length data))
	 (sd (standard-deviation data))
	 (iqr (interquartile-range data))
	 (par (min sd (/ iqr 1.34)))
	 (d-w (* 0.9 par (^ n (/ -1 5)))))
    d-w))

; CROSS-VALIDATION RULE FOR VARIABLE BANDWIDTH ESTIMATOR (SILVERMAN)

(defun cross-val (sample h &key type)
  "calculates the score function \int \hat \lambda(h) - \frac{2}{n}\sum_i \lambda_{-i}(h)
pt-1 =  \int \hat \lambda(h)       and         pt-2 =  \frac{2}{n}\sum_i \lambda_{-i}(h)"
  (let* ((n (length sample))
	 (n1 (- n 1))
	 (nn (iseq n))
	 (nn1 (iseq n1))
	 (OrderedSample (select sample (order sample)))
	 ;(pilot-width (pilot-width1 sample)); !!put it outside this function when finish!!
	 ;(lhat (lambda-hat sample sample pilot-width  :type type)) ; xout =sample ?
         ;(sqrt-lhat (sqrt lhat))
	 (SampleUse (mapcar #'(lambda (xx)
				(remove (select OrderedSample xx) OrderedSample))
			    nn)) ;leave-one-out sample (xx is 1,2,3... in iterat. no. 1,2,3...)
	 (lambda-i (mapcar #'(lambda (ii)
			    (let* ((CurSample  (select SampleUse ii));leave-one-out sample(X_j)
				   (pilot-width (pilot-width1 CurSample))
				   (sqrt-lhat (sqrt ;pilot estimate: 2b used in b/width law
					       (lambda-hat CurSample CurSample
							   pilot-width  :type type))))
			      (lambda-tilde-i CurSample (select OrderedSample ii) sqrt-lhat
					      h n1 nn1 :type type))
			    )
			   nn))
	 (pt-2  (* (/ 2 n) (sum lambda-i))) ; \frac{2}{n}\sum_i \lambda_{-i}(h)
	 )
    pt-2  
    ))

(defun lambda-tilde-i (sample xout sqrt-lhat h n nn &key type)
  "\sum_i \lambda_{-i}"
  (let* ((k-fun (case type ('b 'biweight) ('g 'gaussian) ('t 'triangular)
		           ('e 'epanechnikov) ('r 'rectangular)))
	 (out-i (sum (/ (* sqrt-lhat
			   (funcall k-fun 
				    (* (/ (- xout sample) h)  sqrt-lhat)))
			(+ (- n nn) 1)))))
    (* (/ h) out-i)))

(defun simpson-int (f-list xout)
  "F-list is the values of the function and xout the points
   these values are evaluated at. Requires an even number of 
   arguments for both lists (2n), because the extended simpsons
   rule requires 2n points."
  (let* ((h (- (second xout) (first xout)))
	 (n (length f-list))
	 (nn (iseq  n ))
	 (f0 (first f-list))
	 (f2n (first (last f-list)))
	 (fodd (rest (select f-list (* 2 (iseq (/ n 2))))))
	 (feven (reverse (rest
			  (reverse (select f-list
					   (+ (* 2 (iseq (/ n 2))) 1))))))
	 (sumodd (sum fodd))
	 (sumeven (sum feven))
	 (res (* (/ h 3) (+ f0 (* 4 sumodd) (* 2 sumeven) f2n))))
    res))

			    
#|        ;MISE OPTIMAL BANDWIDTH BY PLUG-IN RULE
(defun MISE-band (sample &key type)
  (let* ((k-fun (case type ('b 'biweight) ('g 'gaussian) ('t 'triangular)
		           ('e 'epanechnikov) ('r 'rectangular))) |#
