; Variable Bandwidth failure rate estimation. The core algorithm is
; lambda-tilde. It represents the estimator: \frac{1}{h}\sum_{i=1}^n 
; \hat \lambda(x_{(i)})^{1/2}K\left (\frac{x-x_{(i)}}{n-i+1}\hat 
; \lambda(x_{(i)})^{1/2} \right ). The \hat \lambda(x_{(i)}) est.
; used above, is the estimate studied by W-L and T-W (hazard analysis 
; I etc.) according to the theory it shouldn't influence much the 
; prediction of the adaptive estimate. 

; This version uses standard samples and standard kernels. Expand it!!!!!!! 

(defun biweight (x)
  (mapcar  #'(lambda (x) (cond ((< (abs x) 1) (* (/ 15 16)
						 (^ (- 1 (^ x 2)) 2)))
	(t 0))) x))

(defun lambda-hat (xin xout h1 &key type)
 "The auxilliary estimator (used to make the bandwith vary) defined by
W-L in H.A. I. To be used in lambda-tilde.
Args: xin is the sample (a list) xout is the points
  at which the estimate will be evaluated (a list also) and h is the
  bandwidth"
  (let* ((n (length xin))
         (nn (iseq n))
         (ord-xin (select xin (order xin)))
	 (k-fun (case type ('b 'biweight) ('g 'gaussian) ('t 'triangular)
		           ('e 'epanechnikov) ('r 'rectangular)))
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
         (lhat (lambda-hat xin xout h1 :type type))
         (sqrt-lhat (sqrt lhat))
         (ord-xin (select xin (order xin)))
	 (k-fun (case type ('b 'biweight) ('g 'gaussian) ('t 'triangular)
		           ('e 'epanechnikov) ('r 'rectangular)))
         (out (mapcar #'(lambda (i)
                          (let* ((lhat-i (select sqrt-lhat i))
                                 (xout-i (select xout i))
                                 (out-i (sum (/ (* lhat-i (funcall k-fun (* (/ (- xout-i ord-xin) h2)
                                                                          lhat-i)))
                                              (+ (- n nn) 1)))))
                            out-i))
                      (iseq (length xout)))))
   (* (/ h2) out)))