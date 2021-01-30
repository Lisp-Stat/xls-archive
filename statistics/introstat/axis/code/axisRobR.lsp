;;;;
;;;;     AxisRobR.lsp © Robert A. Stine, 1992
;;;;                 Robust regression object.
#||
  27 Mar 94 ... Coefficient standard errors.
  29 Jul 93 ... Lots of little patches for summary, labels, filter.
   9 Jul 92 ... Patch the bootstrapping.
   8 Jul 92 ... Created from old regression methods.
||#
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisRobR")

;;;;

(require "axisRegr")

;;;;

#||

(setf rr (robust-regress income bush huber))

(send rr :robust-ests (send rr :x) (send rr :y) :count-limit 1)

||#
;;;;
;;;;     ROBUST REGRESSION ENHANCEMENT
;;;; 

(defun ROBUST-REGRESSION (x y method xNames yName &key case-labels included)
  (send robust-regression-proto :new
        x y xNames yName method
        :case-labels case-labels
        :included included))


(defproto ROBUST-REGRESSION-PROTO
  '(method    ;      symbol -> huber, cauchy, biweight
    c)        ;      tuning constant
  '()              
  axis-regression-proto)

(defmeth robust-regression-proto :ISNEW (x y xNames yName method
                                          &key case-labels included)
  (setf (slot-value 'method) method)
  (format t "Robust regression iterations using ~a.~%" (slot-value 'method))
  (call-next-method x y :predictor-names xNames :response-name yName
                    :case-labels case-labels
                    :included included)
  )
 

(defmeth robust-regression-proto :ESTIMATION-METHOD ()
  ("Robust"))

(defmeth robust-regression-proto :PRINT-TYPE ()
  (format t "Robust Estimates (~a, c=~a):~%"
          (slot-value 'method) (send self :tuning-constant))
  )
 

(defmeth robust-regression-proto :ROBUST-ESTS (x y
                              &key (tol 0.01) (count-limit 10))
  ; Returns both robust coefficients and weights.
  (let ((x   (if (matrixp x) x (apply #'bind-columns x)))
        (wts (repeat 1 (length y)))
        (wf  (send self :robust-weight-function))  )
    (labels ((as-list (x)
                      (coerce (compound-data-seq x) 'list))
             (rel-err (x y)
                      (mean (/ (abs (- x y)) (1+ (abs x)))) )
             (reg-coefs ()
                        (let* ((m (make-sweep-matrix x y wts))
                               (p (array-dimension x 1))  )
                          (as-list
                           (select (first (sweep-operator m (iseq 1 p)))
                                   (1+ p) (iseq 0 p)))))
             (fitvals (beta)
                      (+ (first beta) (matmult x (rest beta))))
             (improve-guess (beta)
                            (let* ((resids (- y (fitvals beta)))
                                   (scale  (/ (median (abs resids)) .6745)) )
                              (setf wts (funcall wf (/ resids scale)))  )
                            (reg-coefs))
             (good-enough? (last beta count)
                           (format t "#~a: Ests -> ~a~%" count beta)
                           (if (> count count-limit)
                               (format t "Iteration limit exceeded.~%")) ;4/2/94
                           (or (and last
                                    (< (rel-err beta last) tol))
                               (> count count-limit))
                           ))
      (do ((last   nil         beta)
           (count  0           (1+ count))
           (beta   (reg-coefs) (improve-guess beta))  )
          ((good-enough? last beta count) (list beta wts)))
      )))

(defmeth robust-regression-proto :ROBUST-WEIGHT-FUNCTION (&optional k)
  (unless (slot-value 'method)
          (setf (slot-value 'method) 'huber))
  (let ((k    (send self :tuning-constant)))
    #'(lambda (r)
        (let ((u (abs (/ r k))))
          (case (slot-value 'method)
            (biweight (^ (- 1 (^ (pmin u 1) 2)) 2))
            (cauchy   (/ 1 (+ 1 (^ u 2))))
            (huber    (/ 1 (pmax u 1)))
            )))))


(defmeth robust-regression-proto :COMPUTE ()             ; override
  ; Defines a new set of weights and then does WLS.        3/27/94
  (setf (slot-value 'weights)
        (second (send self :robust-ests
                      (send self :x) (send self :y))))
  (call-next-method))


(defmeth robust-regression-proto :PSI-DERIV-FUNCTION ()   ; 3/27/94
  (let ((c   (send self :tuning-constant))   )
    (case (slot-value 'method)
      (biweight #'(lambda (x) (let ((z2 (^ (/ x c) 2))  )
                                (if-else (< (abs x) c)
                                         (* (- 1 z2) (- 1 (* 5 z2)))
                                         0)))  )
      (cauchy   (format t "Cauchy stand error not done.~%"))
      (huber    #'(lambda (x) (if-else (< (abs x) c)  1 0))   )
      ))   )

(defmeth robust-regression-proto :PSI-FUNCTION ()         ; 3/27/94
  (let ((c   (send self :tuning-constant))   )
    (case (slot-value 'method)
      (biweight #'(lambda (x) (if-else (< (abs x) c)
                                       (* x (^ (- 1 (^ (/ x c) 2)) 2))
                                       0)))
      (cauchy   (format t "Cauchy stand error not done.~%"))
      (huber    #'(lambda (x) (if-else (< (abs x) c)
                                       x
                                       (if-else (< x 0)
                                                (- c) c))))
      ))   )

(defmeth robust-regression-proto :COEF-STANDARD-ERRORS ()
  "Returns robust estimated standard errors of coefficients."  ; 3/27/94
  (let* ((psi  (send self :psi-function))
         (psi* (send self :psi-deriv-function))
         (res  (send self :raw-residuals))
         (scl  (sqrt (/ (median (abs res)) .6745)))
         (n    (send self :num-cases))
         (x    (if (send self :intercept)
                   (bind-columns (repeat 1 n) (send self :x))
                   (send self :x)))
         (u    (/ res scl))    )
    (* scl
       (sqrt (* (/ (mean (^ (funcall psi  u) 2))
                   (^ (mean (funcall psi* u)) 2))
                (diagonal (inverse (matmult (transpose x) x)))
                )))))



(defmeth robust-regression-proto :METHOD  (&optional(m nil set))
  "Reads/sets estimation method."  ; 3/27/94
  (if set
      (progn 
       (send self :needs-computing t)
       (setf (slot-value 'method) m)  )
      (slot-value 'method)))


(defmeth robust-regression-proto :TUNING-CONSTANT (&optional(c nil set)) 
  "Reads/sets robust tuning constant." ; 3/27/94
  (if set
      (progn 
       (send self :needs-computing t)
       (setf (slot-value 'c) c)  )
      (if (slot-value 'c)
          (slot-value 'c)
          (case (slot-value 'method)
            (biweight 4.685)
            (cauchy   2.385)
            (huber    1.345))
          )))



;;;;
;;;;     BOOTSTRAP
;;;;

(defmeth robust-regression-proto :BOOTSTRAP-FUNCTION ()
  #'(lambda (xy)  ; compute robust coefs
      (first 
       (send self :robust-ests (first xy) (second xy))
       )))

(defmeth robust-regression-proto :BOOTSTRAP-FUNCTION (method)
  (if (eq 'random method) 
      #'(lambda (i)
          (let* ((y   (select (slot-value 'y) i))
                 (all (iseq (array-dimension (slot-value 'x) 1)))
                 (x (select (slot-value 'x) i all))  )
            (first (send self :robust-ests x y)) ; just coefs
            ))
      ; else fixed resampling from residuals
      #'(lambda (i) 
          (let* ((y (+ (send self :fit-values)
                       (select (send self :residuals) i)))
                 (x (slot-value 'x))    )
            (first (send self :robust-ests x y))
            ))
      ))