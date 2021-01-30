;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Straightforward accept-reject sampling from a density f-dens,
;; not necessarily normalized.
;;
;;  version 0.1 -- 06-14-95 -- barebones accept-reject
;; 
;;  version 0.2 -- 08-14-95 -- keep up with rejection-rate
;;                             some optimization, some examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun accept-reject (f-dens g-dens g-rand c n &key (verbose t))
  (let ((ff nil)
        (accept 0)
        (reject 0))
    (loop
     (let ((y (funcall g-rand)))
       (if (<= (random 1.0) (/ (funcall f-dens y) 
                    (* c (funcall g-dens y))))
           (progn
             (setf ff (append ff (list y)))
             (incf accept)) 
         (incf reject))
       (if (= n accept) 
           (progn
             (if verbose
                 (format t "Accept ~d Reject ~d" accept reject))
             (return ff)))
       )
     )
    )
  )

(provide "monte-carlo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The inversion method, with some examples (Devroye, p 28-29)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inversion-method (the-quant)
  (funcall the-quant (one-uniform-rand))
  )

(require "zero")

(defun bisect-inversion-method 
  (the-cdf low up &key (eps .0001) (jmax 40))
  (bisection 
   #'(lambda (x) (- (funcall the-cdf x) (one-uniform-rand)))
   low up :eps eps :jmax jmax))
 
(defun regula-inversion-method 
  (the-cdf low up &key (eps .0001) (maxit 40) (verbose nil))
  (regula-falsi 
   #'(lambda (x) (- (funcall the-cdf x) (one-uniform-rand)))
   low up :eps eps :maxit maxit :verbose verbose))

(defun secant-inversion-method 
  (the-cdf low up &key (eps .0001) (maxit 40)(verbose nil))
  (secant
   #'(lambda (x) (- (funcall the-cdf x) (one-uniform-rand)))
   low up :eps eps :maxit maxit :verbose verbose))

(defun ridder-inversion-method 
  (the-cdf low up &key (eps .0001) (maxit 40)(verbose nil))
  (ridder 
   #'(lambda (x) (- (funcall the-cdf x) (one-uniform-rand)))
   low up :eps eps :maxit maxit :verbose verbose))

(defmacro one-uniform-rand ()
  `(random 1.0)
  )



;;; Exponential  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun one-exponential-rand (&optional (lambda 1))
  (- (/ (log (one-uniform-rand)) lambda))
  )

(defun exponential-rand (n &optional (lambda 1))
  (mapcar #'(lambda (x) 
              (one-exponential-rand lambda)) 
          (make-list n))
  )

;;  Rayleigh  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-rayleigh-rand (&optional (sigma 1))
  (* sigma (sqrt (- (log (one-uniform-rand)))))
  )

(defun rayleigh-rand (n &optional (sigma 1))
  (mapcar #'(lambda (x)
              (one-rayleigh-rand sigma))
          (make-list n))
  )
              

;; Triangular ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-triangular-rand (&optional (a 1))
  (* a (- 1 (sqrt (one-uniform-rand))))
  )

(defun triangular-rand (n &optional (a 1))
  (mapcar #'(lambda (x)
              (one-triangular-rand a))
          (make-list n))
  )

;; Pareto ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun one-pareto-rand (&optional (a 1) (b 1))
  (/ b (expt (one-uniform-rand) (/ a)))
  )

(defun pareto-rand (n &optional (a 1) (b 1))
 (mapcar #'(lambda (x)
             (one-pareto-rand a b))
         (make-list n)))






