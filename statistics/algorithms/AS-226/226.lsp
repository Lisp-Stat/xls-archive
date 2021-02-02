;;; ===============================================================================
;;; lisp code for noncentral-t distribution
;;; by Russ Lenth, December, 1991
;;; based on Applied Statistics Alg AS 243

;;; Warning: non-vectorized version -  
;;; will accept lists for df and delta but will produce misleading results!
(defun *nct*cdf* (tval df delta)
  (if (< tval 0)  (- 1 (nct-cdf (- tval) df (- delta)))
      (let* ((x (/ (* tval tval) (+ df (* tval tval))))
             (d2 (* .5 delta delta))
             (b (* .5 df))
             (rxb (^ (- 1 x) b))
             (lbeta (+ (* .5 (log pi)) (- (log-gamma b) (log-gamma (+ .5 b))))))
        (do* ((a  .5  (+ a 1))
              (xodd  (beta-cdf x a b)  (- xodd godd))
              (xeven  (- 1 rxb)  (- xeven geven))
              (godd  (* 2 rxb (exp (- (* a (log x)) lbeta)))
                     (* godd x (/ (- (+ a b) 1) a)))
              (geven (* b x rxb)
                     (* geven x (/ (- (+ a b) .5) (+ a .5))))
              (pj  (* .5 (exp (- d2)))  (/ (* d2 pj) j))
              (qj  (* pj delta (sqrt (/ 2 pi)))  (/ (* d2 qj) (+ j .5)))
              (sumpj  (- .5 pj)  (- sumpj pj))
              (j  1  (+ j 1))
              (cdf  (+ (* pj xodd) (* qj xeven))
                    (+ cdf (* pj xodd) (* qj xeven)))
              (errbd  1 (* 2 sumpj (- xodd godd))))
             ((< errbd 1.0e-8) (+ cdf (normal-cdf (- delta))))))))

;;; Here is the properly vectorized function
(defun nct-cdf (tval df delta)
"Args: (tval df delta)
Cdf of the noncentral t(df;delta) distribution at tval.  Vectorized."
  (map-elements #'*nct*cdf* tval df delta))
  

;;; Warning: t-power is vectorized for delta, df, and alpha, but NOT for tail
(defun t-power (delta df &key (alpha .05) (tail 0))
"Args: (delta df &key (alpha .05) (tail 0))
Computes the power of a test on the t distribution at noncentrality delta.
Vectorized for all arguments, EXCEPT tail must be a scalar."
  (cond
    ((= tail 0) (+ (t-power delta df :alpha (/ alpha 2) :tail -1)
                   (t-power delta df :alpha (/ alpha 2) :tail 1)))
    ((> tail 0) (t-power (- delta) df :alpha alpha :tail -1))
    ((< tail 0) (nct-cdf (t-quant alpha df) df delta))))





;;; Noncentral beta cdf and related functions (based on APPL STAT Alg AS 226)

;;; non-vectorized version
(defun *ncbeta* (x a b lambda)
  (cond
    ((<= x 0) 0)
    ((>= x 1) 1)
    (t (do*
        ((n 0 (+ n 1))
         (c (/ lambda 2) c)
         (ix (beta-cdf x a b) (- ix gam))
         (gam (exp (+ (- (log-gamma (+ a b))
                         (log-gamma (1+ a)) (log-gamma b))
                      (* a (log x)) 
                      (* b (log (- 1 x)))))
              (/ (* x gam (+ a b n -1)) (+ a n)))
         (q (exp (- c)) (/ (* q c) n))
         (sumq (- 1 q) (- sumq q))
         (p (* q ix) (+ p (* q ix))))
        ((< (* sumq (- ix gam)) 1e-8) p)))))


;;; vectorized version
(defun ncbeta (x a b lambda)
"Noncentral beta cdf - Args: x a b lambda"
  (map-elements #'*ncbeta* x a b lambda))

;;; noncentral F distribution
(defun ncf (f df1 df2 lambda)
"Noncentral F cdf - Args: f df1 df2 lambda"
  (ncbeta (/ (* f df1) (+ (* f df1) df2)) (/ df1 2) (/ df2 2) lambda))




;;; =================================================================================
;;; Numerical inverse function
;;; Requires one starting value.  A secant method with expanding step-size factor is
;;; used to find a starting interval that encloses the solution.  Then the Illinois
;;; (modified regula falsi) algorithm is used to obtain the root.
;;; Bound(s) may be imposed using the :BOUNDS keyword argument; 
;;; a nil bound specifies that no bound is to be imposed.
;;; An :ACCURACY argument sets the accuracy for the solution
;;; A :PRINT argument, if set to t, displays the iterative steps.
;;;
;;; WARNING: This is vectorized, but beware of subtleties.  If f depends implicitly 
;;; on other parameters that could be list-valued, f(x) may return a list even when
;;; x is an atom.  In such cases, the safe thing is to use invert-param-function.

(defun invert-function (f f0 x &key (accuracy .001) (bounds (list nil nil)) (print nil))
"
   (invert-function  f                                ; function to be inverted
                     f0                               ; target value of (f x)
                     x                                ; starting value of x
                     &key (accuracy .001)             ; desired accuracy
                          (bounds (list nil nil))     ; bounds on x
                          (print nil))                ; show iterations

"
  (let ((lbound (first bounds))
        (ubound (second bounds)))
    (map-elements #'*inv*fcn* f f0 x accuracy lbound ubound print)))


;;; non-vectorized solver
(defun *inv*fcn* (f f0 x accuracy lbound ubound print)
  (flet ((fcn (x) (- (funcall f x) f0))
         (secant (x1 f1 x2 f2 factor) 
                 (- x2 (/ (* factor f2 (- x2 x1)) (- f2 f1))))
         (check-bounds (x)
                       (if lbound (setf x (max x lbound)))
                       (if ubound (setf x (min x ubound)) x))
         (check-danger (f1 f2)
                       (if (> (abs f2) (abs (* 1000 (- f1 f2))))
                           (error "invert-function: can't find enclosing interval"))))
;;; expanding secant search for enclosing interval (x1,x2)...
    (do* ((factor 0 (+ 1 factor))   ;; multiplier used in secant search
          (xnew nil (check-bounds (secant x1 f1 x2 f2 factor)))
          (x1 x x2)
          (f1 (fcn x) f2)
          (x2 (if (= x 0) (* 100 accuracy) (* .8 x))  xnew)
          (f2 (fcn x2) (fcn x2))
          (flag (check-danger f1 f2) (check-danger f1 f2)))
         ((<= (* f1 f2) 0)
;;; Illinois method for solving...
          (loop (setf xnew (secant x1 f1 x2 f2 1))
                (setf fnew (fcn xnew))
                (if (> (* fnew f2) 0)
                    (setf f1 (/ f1 2))
                    (list (setf x1 x2) (setf f1 f2)))
                (setf x2 xnew)
                (setf f2 fnew)
                (if print (format t "Illinois guess: ~g~%" xnew))
                (if (<= (abs (- x1 x2)) accuracy) (return x2))))
         (if print (format t "Secant guess: ~g~%" x2)))))



(defun invert-param-function (f f0 i parmlist 
                      &key (accuracy .001) (bounds (list nil nil)) (print nil))
"Like invert-function except solves for the i-th member of parmlist, holding
the other elements of parmlist constant.  f must take parmlist as an argument."
  (let ((xp parmlist))
    (setf (first xp) (+ (first xp) (- f0 f0)))      ;; makes xp a list if f0 is
    (setf xp (apply #'map-elements #'list xp))
    (flet ((g (f00 p00) 
              (*inv*p*fcn* f f00 i p00 
                    accuracy (first bounds) (second bounds) print)))
      (if (listp (first xp)) (map-elements #'g f0 xp)
          (g f0 parmlist)))))

(defun *inv*p*fcn* (f f0 i parmlist accuracy lbound ubound print)
  (flet ((g (x)
            (let ((p parmlist))
              (setf (select p i) x)
              (funcall f p))))
    (*inv*fcn* #'g f0 (select parmlist i) accuracy lbound ubound print)))




