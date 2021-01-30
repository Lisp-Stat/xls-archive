;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap routines for standard errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bootstrap-standard-error (x nboot func)
"Args: (x nboot func)
Returns bootstrap estimate of the standard error
os the statistic func for data x, based on nboot
samples. Data x can be any sequence (but not an array), 
func can have numbers, sequences, or arrays as values."
(let (
      (n (length x))
      (g (repeat nil nboot))
      )
  (dotimes (i nboot)
           (setf (elt g i) (funcall func (sample x n t))))
(sqrt (- (average (^ g 2)) (^ (average g) 2)))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap routines for bias estimation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bootstrap-bias (x nboot func)
"Args: (x nboot func)
Returns bootstrap estimate of the bias
of the statistic func for data x, based on nboot
samples. Data x can be any sequence (but not an array), 
func can have numbers, sequences, or arrays as values."
(let (
      (f (funcall func x))
      (n (length x))
      (g (repeat nil nboot))
      )
  (dotimes (i nboot)
           (setf (elt g i) (funcall func (sample x n t))))
(- (average g) f)
))

(defun better-bootstrap-bias (x nboot bfunc)
"Args: (x nboot bfunc)
Returns the better bootstrap estimate of the bias
of the statistic bfunc for data x, based on nboot
samples. The statistic has to be in resample form,
i.e. it must be a function of a vector of cell
proportions, where the cells have fixed values x.
Data x can be any sequence (but not an array),
bfunc can have numbers, sequences, or arrays as values."
(let* (
       (nn (length x))
       (p0 (repeat (/ nn) nn))
       (f0 (funcall bfunc x p0))
       (k0 (iseq nn))
       (ps (repeat 0 nn))
       (fs (repeat 0 nboot))
       )
(dotimes (i nboot)
(let* (
       (pb (/ (sample k0 nn t) nn))
       (fb (funcall bfunc x pb))
       )
(setf ps (+ ps pb))
(setf (elt fs i) fb)
))
(- (avarage fs) (funcall bfunc x (/ ps nboot)))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap routines for confidence interval estimation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bootstrap-percentile (x nboot func alpha)
"Args: (x nboot func alpha)
Returns the bootstrap percentile estimate of the 
central (1-2alpha) confidence interval for
the statistic func for data x, based on nboot
samples. Data x can be any sequence (but not an array),
func can have numbers, sequences, or arrays as values.
The confidence levels alpha can be a list of values."
(let (
      (n (length x))
      (g (repeat nil nboot))
      )
  (dotimes (i nboot)
           (setf (elt g i) (funcall func (sample x n t))))
(list (quantile g alpha) (quantile g (- 1 alpha)))
))

(defun bootstrap-t-method (x nboot mboot func alpha &key (sdfunc nil))
(let (
      (f (funcall func x))
      (s (if sdfunc (funcall sdfunc x)
              (bootstrap-standard-error x mboot func)))
      (g (repeat nil nboot))
      (n (length x))
      )
(dotimes (i nboot)
(let* (
       (xb (sample x n t))
       (fb (funcall func xb))
       (sb (if sdfunc (funcall sdfunc xb)
               (bootstrap-standard-error xb mboot func)))
       (tb (/ (- fb f) sb))
       )
(setf (elt g i) tb)))
(list 
 (- f (* s (quantile g (- 1 alpha))))
 (- f (* s (quantile g alpha))))
))

(defun bcanon (x nboot func alpha) 
(let (
      (f (funcall func x))
      (g (repeat nil nboot))
      (n (length x))
      (z (normal-quant alpha))
      )
  (dotimes (i nboot)
           (setf (elt g i) (funcall func (sample x n t))))
  (let* (
         (z0 (normal-quant (/ (length (which (< g f))) nboot)))
         (th (jackknife x func))
         (tm (mean th))
         (s1 (sum (^ (- tm th) 3)))
         (s2 (sum (^ (- tm th) 2)))
         (aa (/ s1 (* 6 (^ s2 1.5))))
         (a1 (normal-cdf (+ z0 (/ (+ z0 z) (1- (* aa (+ z0 z)))))))
         (a2 (normal-cdf (+ z0 (/ (- z0 z) (1- (* aa (- z0 z)))))))
         )
(list (quantile g a2) (quantile g a1))
)))

(defun abcnon (x bfunc alpha &key (eps .001))
"ARGS: X BFUNC ALPHA &OPTIONAL EPS
X must be a list of the data.  BFUNC is a function of two arguments
(X MASS) where X is the data list and MASS is a list of weights
of length the same as X and whose elements add to 1.  An example is 
the weighted mean:  #'(lambda (x y) (/ (sum (* x y)) (sum y))).  ALPHA
is the significance level.  A example function call is:
(abcnon x #'(lambda (x y) (/ (sum (* x y)) (sum y))) .05)"
(let* (
       (nn (length x))
       (kn (iseq nn))
       (n1 (repeat 1 nn))
       (n0 (repeat 0 nn))
       (p0 (repeat (/ nn) nn))
       (f0 (funcall bfunc x p0))
       (t1 (repeat nil nn))
       (t2 (repeat nil nn))
       )
(dotimes (i nn)
(let* (
       (un (if-else (= i kn) n1 n0))
       (pn (+ (* (- 1 eps) p0) (* eps un)))
       (pm (- (* (- 1 eps) p0) (* eps un)))
       (tn (funcall bfunc x pn))
       (tm (funcall bfunc x pm))
       )
(setf (elt t1 i) (/ (- tn tm) (* 2 eps)))
(setf (elt t2 i) (/ (- (+ tn tm) (* 2 f0)) (^ eps 2)))
))
(let* (
       (ss (sum (^ t1 2)))
       (sg (/ (sqrt ss) nn))
       (aa (/ (sum (^ t1 3)) (* 6 (^ ss 1.5))))
       (bb (/ (sum t2) (* 2 (^ nn 2))))
       (q0 (/ t1 (* (^ nn 2) sg)))
       (qn (funcall bfunc x (+ (* (- 1 eps) p0) (* eps q0))))
       (qm (funcall bfunc x (- (* (- 1 eps) p0) (* eps q0))))
       (cq (/ (- (+ qn qm) (* 2 f0)) (^ eps 2)))
       (gm (- (/ bb sg) cq))
       (z0 (- aa gm))
       (ww (+ z0 (normal-quant (- 1 alpha))))
       (lb (/ ww (^ (- 1 (* aa ww)) 2)))
       (xi (+ lb (* cq (^ lb 2))))
       )
(+ f0 (* sg xi))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jackknife routines for bias and standard error estimation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jackknife (x func)
(let* (
       (n (length x))
       (k (iseq n))
       (y (repeat nil n))
       )
  (dotimes (i n y)
           (setf (elt y i) (funcall func (select x (remove i k)))))
))

(defun jackknife-bias (x func)
(let (
      (n (length x))
      (g (jackknife x func))
      (f (funcall func x))
      )
(* (1- n) (- (mean g) f))
))

(defun jackknife-standard-error (x func)
(let* (
       (n (length x))
       (g (jackknife x func))
       (f (funcall func x))
       (m (average g))
       )
(sqrt (* (/ (1- n) n) (sum (^ (- g m) 2))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxilary functions needed for bootstrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun average (x)
 (let (
      (n (length x))
      (s 0)
      )
(dotimes (i n)
         (setf s (+ s (elt x i))))
(/ s n)
))

