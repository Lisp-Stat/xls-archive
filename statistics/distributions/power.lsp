(defproto family-proto '(parameter-names parameter-values num-sides power
                         message sample-size-names sample-size-values 
                         sample-ratio))
(defproto normal-family-proto () () family-proto)
(defproto exponential-family-proto () () family-proto)
(defproto binomial-family-proto () () family-proto)
(defproto poisson-family-proto () () family-proto)
(defproto survival-family-proto () () family-proto)
(defproto correlation-family-proto () () family-proto)


(defmacro normal-assessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(normal-assessor :parameter-names parameter-names family-proto)
(normal-assessor :parameter-values parameter-values family-proto)
(normal-assessor :sample-size-names sample-size-names family-proto)
(normal-assessor :sample-size-values sample-size-values family-proto)
(normal-assessor :sample-ratio sample-ratio family-proto)
(normal-assessor :num-sides num-sides family-proto)


(defmeth family-proto :message (&optional (val nil set))
 (if set (setf (slot-value 'message) val)
  (if (slot-value 'message) (format t "~%~a"
      (concatenate 'string "New Significance Level: "  (slot-value 'message)))
      (format t "~%"))))


(defmeth normal-family-proto :normal-1-sample ()
  (send self :parameter-names 
   (list "Mu (Ho)" "Mu (Ha)" "Sigma" "Alpha"))
  (send self :sample-size-names (list "N"))
 #'(lambda (mu0 mua sigma alpha n)
"Args: MU0 MUA SIGMA ALPHA N
MU0 is the value of Mu under the null hypothesis.  MUA is the value of Mu 
under the alternative hypothesis.  SIGMA is the true standard deviation.
N is the sample size."
  (let* (
         (num-sides (send self :num-sides))
         (ucrit (if (= num-sides 2)
                    (list (t-quant (/ alpha 2) (1- n)) 
                          (t-quant (- 1 (/ alpha 2)) (1- n)))
                    (if (< mu0 mua) 
                        (repeat (t-quant (- 1 alpha) (1- n)) 2)
                        (repeat (- (t-quant (- 1 alpha) (1- n))) 2))))
         (p1 (t-cdf (+ (first ucrit) 
                       (/ (- mu0 mua) (/ sigma (sqrt n)))) (1- n)))
         (p2 (- 1 (t-cdf (+ (second ucrit) 
                            (/ (- mu0 mua) (/ sigma (sqrt n)))) (1- n))))
        )
      (if (= num-sides 2) (+ p1 p2)
                          (if (> mu0 mua) p1 p2)))))


(defmeth normal-family-proto :normal-2-sample-equal-var ()
 (send self :parameter-names 
   (list "Mu-1" "Mu-2" "Sigma" "Alpha"))
 (send self :sample-size-names (list "N-1" "N-2"))
 (send self :sample-ratio #'(lambda (ntotal) 
                              (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
 #'(lambda (mu1 mu2 sigma alpha n1 n2)
"Args: MU1 MU2 SIGMA ALPHA N1 N2
MU1 is the true value of the mean of population 1.  MU2 is the true value of
the mean of population 2.  SIGMA is the common variance of both populations. 
ALPHA is the significance level.  N1 is the sample size from population 1.
N2 is the sample size from poplation 2."
    (let* (
           (num-sides (send self :num-sides))
           (df (+ n1 n2 (- 2)))
           (ucrit (if (= num-sides 2) 
                      (list (t-quant (- 1 (/ alpha 2)) df)
                            (t-quant (/ alpha 2) df))
                      (if (> mu1 mu2)
                          (repeat (t-quant (- 1 alpha) df) 2)
                          (repeat (t-quant alpha df) 2))))
           (obs (/ (- mu1 mu2) (* sigma (sqrt (+ (/ n1) (/ n2))))))
           (p1 (- 1 (t-cdf (- (first ucrit) obs) df)))
           (p2 (t-cdf (- (second ucrit) obs) df))
          )
      (if (= num-sides 2) (+ p1 p2) (if (> mu1 mu2) p1 p2)))))
          

(defmeth normal-family-proto :normal-2-sample-unequal-var ()
 (send self :parameter-names 
   (list "Mu-1" "Mu-2" "Sigma-1" "Sigma-2" "Alpha"))
 (send self :sample-size-names (list "N-1" "N-2"))
 (send self :sample-ratio #'(lambda (ntotal)
                           (let* (
                                  (params (send self :parameter-values))
                                  (sigma-1 (elt params 2))
                                  (sigma-2 (elt params 3))
                                 )
                          (ceiling
                           (list (* ntotal (/ sigma-1 (+ sigma-1 sigma-2)))
                                 (* ntotal (/ sigma-2 (+ sigma-1 sigma-2))))))))
#'(lambda (mu1 mu2 sigma1 sigma2 alpha n1 n2)
"Args: M1 M2 SIGMA-1 SIGMA-2 ALPHA N-1 N-2
M1 is the true value of the mean of population 1.  M2 is the true value of
the mean of population 2.  SIGMA-1 is the true variance of population 1.
SIGMA-2 is the true variance of population 2.  ALPHA is the significance level.
N1 is the sample size from population 1.  N2 is the sample size from poplation 
2."
     (let* (
            (num-sides (send self :num-sides))
            (obs (/ (- mu1 mu2) (sqrt (+ (/ (^ sigma1 2) n1)
                                    (/ (^ sigma2 2) n2)))))
            (k (+ (/ (^ sigma1 2) n1) (/ (^ sigma2 2) n2)))
            (f (ceiling (/ (+ (/ (^ sigma1 4) (* (^ k 2) (^ n1 2) (1- n1)))
                              (/ (^ sigma2 4) (* (^ k 2) (^ n2 2) (1- n2)))))))
            (ucrit (if (= num-sides 2)
                      (list (t-quant (- 1 (/ alpha 2)) f)
                            (t-quant (/ alpha 2) f))
                      (if (> mu1 mu2)
                          (repeat (t-quant (- 1 alpha) f) 2)
                          (repeat (t-quant alpha f) 2))))
            (p1 (- 1 (t-cdf (- (first ucrit) obs) f)))
            (p2 (t-cdf (- (second ucrit) obs) f))
           )
     (if (= num-sides 2) (+ p1 p2) (if (> mu1 mu2) p1 p2)))))



(defmeth normal-family-proto :lognormal-equal-var ()
 (send self :parameter-names
   (list "Mu-1" "Mu-2" "Nu" "Alpha"))
 (send self :sample-size-names (list "N-1" "N-2"))
 (send self :sample-ratio #'(lambda (ntotal)
                              (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (mu1 mu2 nu alpha n1 n2)
"Args: M1 M2 NU ALPHA N-1 N-2
M1 is the true value of the mean of population 1.  M2 is the true value of
the mean of population 2.  NU is the coefficient of variation.
ALPHA is the significance level.  N1 is the sample size from population 1.  
N2 is the sample size from poplation 2."
  (let* (
         (num-sides (send self :num-sides))
         (sigma-l (sqrt (1+ (log (^ nu 2)))))
         (nu-l1 (log (/ mu1 (sqrt (exp (^ sigma-l 2))))))
         (nu-l2 (log (/ mu2 (sqrt (exp (^ sigma-l 2))))))
         (df (+ n1 n2 (- 2)))
         (ucrit (if (= num-sides 2)
                    (list (t-quant (- 1 (/ alpha 2)) df)
                          (t-quant (/ alpha 2) df))
                    (if (> nu-l1 nu-l2)
                          (repeat (t-quant (- 1 alpha) df) 2)
                          (repeat (t-quant alpha df) 2))))
         (obs (/ (- nu-l1 nu-l2) (* sigma-l (sqrt (+ (/ n1) (/ n2))))))
         (p1 (- 1 (t-cdf (- (first ucrit) obs) df)))
         (p2 (t-cdf (- (second ucrit) obs) df))
        )
    (if (= num-sides 2) (+ p1 p2) (if (> nu-l1 nu-l2) p1 p2)))))



#|
(defmeth normal-family-proto :bivariate-normal ()
 (unless (send self :parameter-names)
 (send self :parameter-names
   (list "Mu-1" "Mu-2" "Nu" "Alpha"))
 (send self :sample-size-names (list "N-1" "N-2")))
 #'(lambda (mu1 mu2 sigma-1 sigma-2 alpha n1 n2)
    (let* (
           (bool (if (> mu1 mu2) t nil))
           (m1 (if bool mu1 mu2))
           (m2 (if bool mu2 mu1))
           (n-1 (if bool n1 n2))
           (n-2 (if bool n2 n1))
           (ucrit (normal-quant (- 1 alpha)))

|#




(defmeth exponential-family-proto :exponential-1-sample ()
 (send self :parameter-names
   (list "Mu (H0)" "Mu (Ha)" "Alpha"))
 (send self :sample-size-names (list "N"))
#'(lambda (mu0 mua alpha n)
"Args: MU0 MUA ALPHA N
MU0 is the value of mu under the null hypothesis.  MUA is the value of mu
under althernative hypothesis.  ALPHA is the significance level.  N is the
sample size."
 (let* (
        (num-sides (send self :num-sides))
        (df (* 2 n))
        (ucrit (if (= 2 num-sides) 
                   (* 2 mu0 (list (chisq-quant (/ alpha 2) df)
                                  (chisq-quant (- 1 (/ alpha 2)) df)))
                   (if (> mu0 mua) 
                    (repeat (* 2 mu0 (chisq-quant alpha df)) 2)
                    (repeat (* 2 mu0 (chisq-quant (- 1 alpha) df)) 2))))
        (p1 (chisq-cdf (/ (first ucrit) (* 2 mua)) df))
        (p2 (- 1 (chisq-cdf (/ (second ucrit) (* 2 mua)) df)))
       )
    (if (= num-sides 2) (+ p1 p2) 
                        (if (> mu0 mua) p1 p2)))))


(defmeth exponential-family-proto :exponential-2-sample ()
 (send self :parameter-names
   (list "Mu-1" "Mu-2" "Alpha"))
 (send self :sample-size-names (list "N-1" "N-2"))
 (send self :sample-ratio #'(lambda (ntotal)
                             (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (mu1 mu2 alpha n1 n2)
"Args: MU1 MU2 ALPHA N1 N2
MU1 is the true value of mu for population 1.  MU2 is the true value of
mu for population 2.  ALPHA is the significance level.  N1 is the size
of the sample from population 1.  N1 is the size of the sample from 
population 2."
  (let* (
         (num-sides (send self :num-sides))
         (phi-a (/ mu1 mu2))
         (dfs (repeat (list (list (* 2 n1) (* 2 n2))) 2))
         (ucrit (if (= 2 num-sides)
                    (list (apply #'f-quant (- 1 (/ alpha 2)) (first dfs))
                          (apply #'f-quant (/ alpha 2) (second dfs)))
                    (if (> phi-a 1)
                        (repeat (apply #'f-quant (- 1 alpha) (first dfs)) 2)
                        (repeat (apply #'f-quant alpha (second dfs)) 2))))
         (p1 (- 1 (apply #'f-cdf (/ (first ucrit) phi-a) (first dfs))))
         (p2 (apply #'f-cdf (/ (second ucrit) phi-a) (second dfs)))
       )
    (if (= num-sides 2) (+ p1 p2) (if (> phi-a 1) p1 p2)))))



(defmeth binomial-family-proto :binomial-1-sample ()
 (send self :parameter-names
   (list "Po" "Pa" "Alpha"))
 (send self :sample-size-names (list "N"))
#'(lambda (p0 pa alpha n)
"Args: P0 PA ALPHA N
P0 is the probability of success under the null hypothesis.  PA is the 
probability of success under the alternative hypothesis.  ALPHA is the
significance level and N is the sample size."
  (let* (
         (bq-0 (binomial-quant alpha n p0))
         (bq-a (binomial-quant (- 1 alpha) n p0))
         (b0-cdf (binomial-cdf bq-0 n p0))
         (ba-cdf (binomial-cdf bq-a n p0))
         (ucrit (if (> p0 pa) (if (> b0-cdf alpha) (1- bq-0) bq-0)
                              (if (< ba-cdf (- 1 alpha)) (1+ bq-a) bq-a)))
        )
    (send self :message 
          (if (> p0 pa) (format nil "~5,4f" (binomial-cdf ucrit n p0))
             (format nil "~5,4f" (- 1 (binomial-cdf ucrit n p0)))))

    (if (> p0 pa) (binomial-cdf ucrit n pa)
                  (- 1 (binomial-cdf ucrit n pa))))))


(defmeth binomial-family-proto :binomial-1-sample-arcsine ()
 (send self :parameter-names
   (list "Po" "Pa" "Alpha"))
 (send self :sample-size-names (list "N"))
#'(lambda (p0 pa alpha n)
"Args: P0 PA ALPHA N
P0 is the probability of success under the null hypothesis.  PA is the
probability of success under the alternative hypothesis.  ALPHA is the
significance level and N is the sample size."
  (let* (
         (num-sides (send self :num-sides))
         (delta (abs (* 2 (- (asin (sqrt pa)) (asin (sqrt p0))))))
         (ucrit (if (= num-sides 2)
                    (list (normal-quant (/ alpha 2))
                          (normal-quant (- 1 (/ alpha 2))))
                    (if (> delta 0)
                        (repeat (normal-quant (- 1 alpha)) 2)
                        (repeat (- (normal-quant (- 1 alpha))) 2))))
         (p1 (normal-cdf (+ (first ucrit) (* (sqrt n) delta)))
         (p2 (- 1 (normal-cdf (+ (second ucrit)  (* (sqrt n) delta)))))
        )
      (if (= num-sides 2) (+ p1 p2)
                          (if (> delta 0) p1 p2))))))


(defmeth binomial-family-proto :binomial-2-sample-arcsin ()
 (send self :parameter-names
   (list "P-1" "P-2" "Alpha"))
 (send self :sample-size-names (list "N-1" "N-2"))
 (send self :sample-ratio #'(lambda (ntotal)
                              (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (p1 p2 alpha n1 n2)
"Args: P1 P2 ALPHA N1 N2
P1 is the probability of success for population 1.  P2 is the
probability of success for population 2.  ALPHA is the significance level.
N1 is the sample size from population 1 and N2 is the sample size
from population 2."
  (let* (
         (num-sides (send self :num-sides))
         (delta (abs (* 2 (- (asin (sqrt p1)) (asin (sqrt p2))))))
         (obs (/ delta (sqrt (+ (/ n1) (/ n2)))))
         (ucrit (if (= num-sides 2)
                    (list (normal-quant (/ alpha 2))
                          (normal-quant (- 1 (/ alpha 2))))
                    (if (> delta 0)
                        (repeat (normal-quant (- 1 alpha)) 2)
                        (repeat (- (normal-quant (- 1 alpha))) 2))))
         (p1 (normal-cdf (- (first ucrit) obs)))
         (p2 (- 1 (normal-cdf (- (second ucrit) obs))))
        )
      (if (= num-sides 2) (+ p1 p2)
                          (if (> delta 0) p2 p1)))))


(defmeth binomial-family-proto :binomial-2-sample-median ()
 (send self :parameter-names
   (list "P0" "Pm" "Alpha"))
 (send self :sample-size-names (list "N"))
 (send self :sample-ratio #'(lambda (ntotal)
                              (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (po pm alpha n)
    (funcall (send self :binomial-2-sample-arcsin)
                (+ po pm) (- po pm) alpha (/ n 2) (/ n 2))))



(defmeth binomial-family-proto :fishers-exact-test ()
 (send self :parameter-names
   (list "P-1" "P-2" "Alpha"))
 (send self :sample-size-names (list "N"))
#'(lambda (p1 p2 alpha n)
    (let* (
           (num-sides (send self :num-sides))
           (pbar (/ (+ p1 p2) 2))
           (obs (sqrt (* .5 n pbar (- 1 pbar))))
           (obs-2 (sqrt (* .25 n (+ (* p1 (- 1 p1))
                                    (* p2 (- 1 p2))))))
           (obs-3 (* .5 n (- p1 p2)))
           (ucrit (if (= num-sides 2)
                      (list (+ .5 (* (normal-quant (- 1 (/ alpha 2))) obs))
                            (+ (- .5) (* (normal-quant (/ alpha 2)) obs)))
                      (if (> p1 p2)
                       (repeat (+ .5 (* obs (normal-quant (- 1 alpha)))) 2)
                       (repeat (+ (- .5) (* obs (normal-quant alpha))) 2))))
           (pr1 (- 1 (normal-cdf (/ (- (first ucrit) obs-3) obs-2))))
           (pr2 (normal-cdf (/ (- (second ucrit) obs-3) obs-2)))
          )
      (if (= num-sides 2) (+ pr1 pr2)
                          (if (> p1 p2) pr1 pr2)))))

(defun ask-vals (str)
(let* (
       (ask-vals (send text-item-proto :new (format nil "~a" str)))
       (yes-button (send modal-button-proto :new "Yes" :action #'(lambda () 
                     (send dialog :close) t)))
       (no-button (send modal-button-proto :new "No" :action #'(lambda () 
                     (send dialog :close) nil)))
       (dialog (send modal-dialog-proto :new
                   (list ask-vals (list yes-button no-button))))
      )
  (send dialog :modal-dialog)))

(defun get-vals (str)
 (let* (
        (ask (mapcar #'(lambda (x) (send text-item-proto :new 
                          (format nil "Z(~a)" x))) str)) 
        (get (mapcar #'(lambda (x) (send edit-text-item-proto :new 
                              "" :text-length 5)) (iseq 4)))
        (ok (send modal-button-proto :new "Ok" :action #'(lambda ()
               (mapcar #'(lambda (x) (str-to-num (send x :text))) get))))
        (dialog (send modal-dialog-proto :new 
                     (append (transpose (list ask get)) (list ok))))
       )
   (send dialog :modal-dialog)))

 
(defmeth binomial-family-proto :matched-pairs ()
(unless (send self :parameter-names)
 (send self :parameter-names (list "Delta" "Alpha"))
 (send self :sample-size-names (list "N")))
#'(lambda (delta alpha n)
 (unless (first (send self :slot-value 'sample-size-values))
    (let ((z-vals (ask-vals "Is Preliminary Sample Available?")))
   (if z-vals (send self :add-slot 'z-vals
                    (get-vals (list "1,1" "1,0" "0,1" "0,0")))
              (let ((theta-vals (ask-vals
                        "Are estimates of Theta1 and Theta2 Available?")))
                 (send self :add-slot 'theta-vals
                       (get-vals (list "Theta 1" "Theta 2"))))))
    (setf (slot-value 'sample-size-values) (list t)))
 (unless (send self :slot-value 'power)
    (send self :add-slot 'z-vals (get-vals (list "1,1" "1,0" "0,1" "0,0")))
    (setf (slot-value 'power) t))
 (let* (
        (z-vals (if (send self :has-slot 'z-vals)
                    (send self :slot-value 'z-vals) nil))
        (zbool (if z-vals t nil))
        (theta-vals (if zbool nil (send self :slot-value 'theta-vals)))
        (t-bool (if theta-vals t nil))
        (z_11 (if zbool (first z-vals) 
                        (if t-bool (apply #'* (combine n theta-vals))
                                   (apply #'* (combine n (list .9 .1))))))
        (z_10 (if zbool (second z-vals) 
                        (if t-bool (apply #'* (list n (first theta) 
                                                      (- 1 (second theta))))
                                   (apply #'* (combine n (list .9 .9))))))
        (z_01 (if zbool (third z-vals) 
                        (if t-bool (apply #'* (list n (- 1 (first theta))
                                                      (second theta)))
                                   (apply #'* (combine n (list .1 .1))))))
        (z_00 (if zbool (fourth z-vals) 
                        (if t-bool (apply #'* (list n (- 1 (first theta))
                                                      (- 1 (second theta))))
                                   (apply #'* (combine n (list .1 .9))))))
        (num-sides (send self :num-sides))
        (ucrit (if (= num-sides 2)
                   (list (normal-quant (/ alpha 2))
                         (normal-quant (- 1 (/ alpha 2))))
                       (repeat (normal-quant alpha) 2)))
        (zsum (+ z_00 z_01 z_10 z_11))
        (a-val (/ (+ z_10 z_01 (* (abs delta) (- z_10 z_01))) (* 2 zsum)))
        (phi (+ a-val (sqrt (- (^ a-val 2)
                               (/ (* (abs delta) (- z_10 z_01
                                                    (* delta (+ z_00 z_11))))
                                  zsum)))))
        (denom (sqrt (- (^ phi 2) (/ (* (^ delta 2) (+ 3 phi)) 4))))
        (pr1 (normal-cdf (/ (+ (* (first ucrit) phi)
                               (* (abs delta) (sqrt (* n phi)))) denom)))
        (pr2 (- 1 (normal-cdf (/ (+ (* (second ucrit) phi) 
                                    (* (abs delta) (sqrt (* n phi)))) denom))))
       )
    (if (= num-sides 2) (+ pr1 pr2) pr1))))



(defmeth binomial-family-proto :proportion-responders-cons-exp ()
 (send self :parameter-names
   (list "Pc" "Pe" "D-Crit" "Alpha"))
 (send self :sample-size-names (list "N-c" "N-e"))
 (send self :sample-ratio #'(lambda (ntotal)  
                             (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (pc pe dcrit alpha nc ne)
  (let ((ucrit (normal-quant (- 1 alpha))))

    (- 1 (normal-cdf (- ucrit (/ (- dcrit (- pe pc))
                                 (sqrt (+ (/ (* pc (- 1 pc)) nc)
                                          (/ (* pe (- 1 pe)) ne))))))))))


(defmeth binomial-family-proto :binomial-2-sample-historical ()
 (send self :parameter-names
   (list "P-c" "P-e" "Alpha"))
 (send self :sample-size-names (list "N-c" "N-e"))
 (send self :sample-ratio #'(lambda (ntotal) 
                                 (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (pc pe alpha nc ne)
(let* (
       (num-sides (send self :num-sides))
       (delta (* 2 (- (asin (sqrt (/ (+ (* pe ne) .375) (+ ne .75))))
                      (asin (sqrt (/ (+ (* pc nc) .375) (+ nc .75)))))))
       (ucrit (if (= num-sides 2)
                  (list (normal-quant (/ alpha 2))
                        (normal-quant (- 1 (/ alpha 2))))
                  (if (> delta 0)
                      (repeat (normal-quant (- 1 alpha)) 2)
                      (repeat (normal-quant alpha) 2))))
       (obs (/ delta (sqrt (+ (/ (+ nc .5)) (/ (+ ne .5))))))
       (pr1 (- 1 (normal-cdf (- (first ucrit) obs))))
       (pr2 (normal-cdf (- (second ucrit) obs)))
      )
   (if (= num-sides 2) (+ pr1 pr2) 
                       (if (> delta 0) pr1 pr2)))))


(defmeth binomial-family-proto :binomial-k-sample ()
(unless (send self :parameter-names)
 (let ((value-get (first (get-value-dialog "How many samples?" :initial 2))))
 (send self :parameter-names
   (append (mapcar #'(lambda (x) (format nil "P-~a" x)) (iseq 1 value-get))
           (list "Alpha")))
 (send self :sample-size-names 
   (mapcar #'(lambda (x) (format nil "N-~a" x)) (iseq 1 value-get)))))
(send self :sample-ratio #'(lambda (ntotal)
          (let ((p-values (butlast (butlast (send self :parameter-values)))))
                 (ceiling (* ntotal p-values)))))

#'(lambda (&rest args)
 (let* (
        (k (length (send self :sample-size-names)))
        (p-list (select args (iseq 0 (1- k))))
        (alpha (elt args k))
        (n-list (select args (iseq (1+ k) (1- (length args)))))
        (ucrit (chisq-quant (- 1 alpha) (1- k)))
        (sum-n (sum n-list))
        (sum-np (sum (* n-list p-list)))
        (ha1 (/ (* p-list n-list) sum-n))
        (ha2 (/ (* (- 1 p-list) n-list) sum-n))
        (h01 (/ (* sum-np n-list) (^ sum-n 2)))
        (h02 (/ (* (- 1 (/ sum-np sum-n)) n-list) sum-n))
        (lambda (* sum-n (sqrt (sum (+ (/ (^ (- ha1 h01) 2) h01)
                                       (/ (^ (- ha2 h02) 2) h02))))))
      )
   (- 1 (noncentral-chisq-cdf ucrit (1- k) lambda)))))

;;;

         
(defmeth binomial-family-proto :case-control ()
 (send self :parameter-names
   (list "F" "R" "Alpha"))
 (send self :sample-size-names (list "N-a" "N-o"))
 (send self :sample-ratio #'(lambda (ntotal) 
                                 (ceiling (list (/ ntotal 2) (/ ntotal 2)))))
#'(lambda (f r alpha na n0)
(let* (
       (num-sides (send self :num-sides))
       (delta (* 2 (- (asin (sqrt (/ (* f r) (1+ (* f (1- r))))))
                      (asin (sqrt f)))))
       (obs (/ delta (sqrt (+ (/ na) (/ n0)))))
       (ucrit (if (= num-sides 2)
                  (list (normal-quant (- 1 (/ alpha 2)))
                        (normal-quant (/ alpha 2)))
                  (if (> delta 0)
                   (repeat (normal-quant (- 1 alpha)) 2)
                   (repeat (normal-quant alpha) 2))))
       (pr1 (- 1 (normal-cdf (- (first ucrit) obs))))
       (pr2 (normal-cdf (- (second ucrit) obs)))
      )
   (if (= num-sides 2) (+ pr1 pr2)
                       (if (> delta 0) pr1 pr2)))))
 


 
(defmeth binomial-family-proto :matched-case-control ()
 (send self :parameter-names
   (list "F" "R" "Alpha"))
 (send self :sample-size-names (list "N"))
#'(lambda (f r alpha n)
 (let (
       (p0 .5)
       (pa (/ r (1+ r)))
       (n-star (* (+ (* f (- 1 (/ f (+ f (/ (- 1 f) r)))))
                     (* (/ f (+ f (/ (- 1 f) r))) (- 1 f))) n))
      )
  (funcall (send self :binomial-1-sample) p0 pa alpha (ceiling n-star)))))

(defmeth poisson-family-proto :poisson-1-sample ()
 (send self :parameter-names
   (list "Mu (H0)" "Mu (Ha)" "Time" "Alpha"))
#'(lambda (mu0 mua time alpha)
 (let* (
        (mu0-t (* mu0 time))
        (mua-t (* mua time))
        (pq-0 (poisson-quant alpha mu0-t))
        (pq-a (poisson-quant (- 1 alpha) mu0-t))
        (p0-cdf (poisson-cdf pq-0 mu0-t))
        (pa-cdf (poisson-cdf pq-a mu0-t))
        (c (if (> mu0 mua) (if (> p0-cdf alpha) (1- pq-0) pq-0)
                           (if (< pa-cdf (- 1 alpha)) (1+ pq-a) pq-a)))
       )
    (send self :message 
          (if (> mu0 mua) (format nil "~5,4f" (poisson-cdf c mu0-t))
             (format nil "~5,4f" (- 1 (poisson-cdf c mu0-t)))))

  (if (> mu0 mua) (poisson-cdf c mua-t)
                  (- 1 (poisson-cdf c mua-t))))))


(defmeth poisson-family-proto :poisson-2-sample ()
 (send self :parameter-names
   (list "Mu-1" "Mu-2" "T-1" "T-2" "Alpha"))
#'(lambda (m1 m2 t1 t2 alpha)
(let* (
       (mu1 (/ m1))
       (mu2 (/ m2))
       (n1 (* m1 t1))
       (n2 (* m2 t2))
       (num-sides (send self :num-sides))
       (phi-a (/ mu1 mu2))
       (dfs (repeat (list (list (* 2 n1) (* 2 n2))) 2))
       (ucrit (if (= 2 num-sides)
                  (list (apply #'f-quant (- 1 (/ alpha 2)) (first dfs))
                        (apply #'f-quant (/ alpha 2) (second dfs)))
                  (if (> phi-a 1)
                      (repeat (apply #'f-quant (- 1 alpha) (first dfs)) 2)
                      (repeat (apply #'f-quant alpha (second dfs)) 2))))
       (p1 (- 1 (apply #'f-cdf (/ (first ucrit) phi-a) (first dfs))))
       (p2 (apply #'f-cdf (/ (second ucrit) phi-a) (second dfs)))
       )
   (if (= num-sides 2) (+ p1 p2) (if (> phi-a 1) p1 p2)))))



(defmeth correlation-family-proto :correlation-coeff-outcome ()
 (send self :parameter-names
   (list "Rho (H0)" "Rho (Ha)" "Alpha"))
 (send self :sample-size-names (list "N"))
#'(lambda (rho-0 rho-a alpha n)
 (let* (
        (num-sides (send self :num-sides))
        (ucrit (if (= num-sides 2)
                   (list (normal-quant (/ alpha 2))
                         (normal-quant (- 1 (/ alpha 2))))
                   (if (> rho-0 rho-a)
                       (repeat (- (normal-quant (- 1 alpha))) 2)
                       (repeat (normal-quant (- 1 alpha)) 2))))
        (sqt (sqrt (- n 3)))
        (h0 (* sqt (log (/ (+ 1 rho-0) (- 1 rho-0))) (/ 2)))
        (ha (* sqt (log (/ (+ 1 rho-a) (- 1 rho-a))) (/ 2)))

        (p1 (normal-cdf (+ (first ucrit) h0 (- ha))))
        (p2 (- 1 (normal-cdf (+ (second ucrit) h0 (- ha)))))

       )
      (if (= num-sides 2) (+ p1 p2) (if (> rho-0 rho-a) p1 p2)))))


(defvar types (list
   (list 'NORMAL "1 Sample" "2 Sample-Equal Variances"
                 "2 Sample-Unequal Variances"
                 "Log Normal-Equal Variances")
   (list 'EXPONENTIAL "1 Sample" "2 Sample")
   (list 'BINOMIAL "1 Sample" "1 Sample Arcsine" "2 Sample-Arcsine"
                   "2 Sample-Median" "Fishers Exact Test" "Prop Responders"
                   "Case Control" "K Sample Binomial")
   (list 'POISSON "1 Sample" "2 Sample")
   (list 'CORRELATION "1 Sample-Correlaion")))

(defun pad-list (string len)
 (combine (list string (repeat "" (- len (length string))))))

(defun str-to-num (str)
  (read (make-string-input-stream str)))

(defun test-start-power ()
 (let* (
        (dist (mapcar #'first types))
        (ask-type (send text-item-proto :new
          "Pick a Model:"))
        (model-list (send list-item-proto :new (repeat "" 12)))
        (dist-list (send list-item-proto :new (mapcar #'(lambda (x)
                    (format nil "~a" x)) dist)
                    :action #'(lambda (x)
                        (let* (
                               (indlist (rest (assoc (elt dist
                                         (send self :selection)) types)))
                               (strlist (pad-list indlist 12)))
                       (mapcar #'(lambda (x y) (send model-list :set-text x y))
                                   (iseq (length strlist)) strlist)))))
        (ask-proc (send text-item-proto :new "Find:"))
        (get-proc (send choice-item-proto :new
                    (list "Power for a specified Sample Size"
                          "Sample Size for a specified Power")))
        (ask-sides (send text-item-proto :new "What type of test"))
        (get-sides (send choice-item-proto :new (list "1 Sided" "2 Sided")))
        (ok (send button-item-proto :new "Ok" :action #'(lambda ()
             (let* (
                    (family (send dist-list :selection))
                    (model (send model-list :selection))
                    (type (send get-proc :value))
                    (family-model (start-family family model))
                    (obj (send (symbol-value (first family-model)) :new))
                    (objfun (send obj (second family-model)))
                    (num-sides (1+ (send get-sides :value)))
                   )
                 (test-power-dialog obj objfun type num-sides)))))
       )
    (send dialog-proto :new (list ask-type (list dist-list model-list)
                            (list ask-proc get-proc) (list ask-sides get-sides)
                             ok) :title "Model Dialog")
  )
)


(defun test-power-dialog (obj objfun type num-sides)
 (let* (
        (param-names (send obj :parameter-names))
        (ss-names (send obj :sample-size-names))
        (n (length param-names))
        (ss-len (length ss-names))
        (text-items (mapcar #'(lambda (x) (send text-item-proto :new x))
                   (remove nil (if (= type 0) (append param-names ss-names)
                                      (append param-names (list "Power"))))))
        (text-values (mapcar #'(lambda (x) (send edit-text-item-proto :new x
                                  :text-length 5))
                       (repeat "" (if (= type 0)
                                      (+ n ss-len)
                                      (1+ n)))))
        (param-windows (if (= type 0)
                          (list (send text-item-proto :new "" :text-length 20))
                          (mapcar #'(lambda (x) (send text-item-proto :new ""
                                       :text-length 20))
                              (iseq ss-len))))
        (message-window (send text-item-proto :new "" :text-length 25))

        (ok (send button-item-proto :new "Calculate" :action #'(lambda ()
               (let ((params (mapcar #'(lambda (x) (str-to-num (send x :text)))
                                 text-values)))
                 (send obj :num-sides num-sides)
                 (send obj :parameter-values params)
                 (mapcar #'(lambda (x) (send x :text ""))
                      (combine param-windows message-window))
                (if (= type 0) 
                             (progn
                               (print (apply #'funcall objfun params))
                                (send (first param-windows) :text
                                 (format nil "The Power is:   ~6,5f"
                                 (apply #'funcall objfun params))))

                                (progn
                                  (mapcar #'(lambda (x y z) (send x :text
                                   (format nil "~a:  ~1,0f" y z)))
                                         param-windows ss-names

                                (if (= 1 ss-len)
                                    (1-sample-search objfun (butlast params)
                                         (first (last params)))
                                    (multi-sample-search
                                         objfun (send obj :sample-ratio)
                                         (butlast params)
                                         (first (last params)))))
                                (print (if (= 1 ss-len)
                                    (1-sample-search objfun (butlast params)
                                         (first (last params)))
                                    (multi-sample-search
                                         objfun (send obj :sample-ratio)
                                         (butlast params)
                                         (first (last params)))))))
                 (if (send obj :message) 
                     (send message-window :text (send obj :message))
                     (send message-window :text ""))))))
       )
 (send dialog-proto :new (append
          (transpose (list text-items text-values))
          (list param-windows message-window ok)))))















(defun start-power (family model type num-sides &rest args)
 (let* (
        (family-model (start-family family model))
        (obj (send (symbol-value (first family-model)) :new))
        (objfun (send obj (second family-model)))
       )
   (power-dialog obj objfun type num-sides (combine args))))

(defun power-dialog (obj objfun type num-sides params)
 (let* (
        (param-names (send obj :parameter-names))
        (ss-names (send obj :sample-size-names))
        (n (length param-names))
        (ss-len (length ss-names))
       )

   (send obj :num-sides num-sides)
   (send obj :parameter-values params)
   (setf *standard-output* *terminal-io*)
   (if (= type 1) (format t "~%The Power is:  ~6,4f"
                                 (apply #'funcall objfun params))
                   
                  (mapcar #'(lambda (y z) (format t "~%~a:  ~3,0f" y z))
                                              ss-names

                                (if (= 1 ss-len)
                             
                                    (1-sample-search objfun (butlast params)
                                         (first (last params)))
                                    (multi-sample-search 
                                         objfun (send obj :sample-ratio)
                                         (butlast params) 
                                         (first (last params))))))
                 (send obj :message)))


(defun multi-sample-search (fun sample-ratio-fun params power
                            &optional (ntotal 14) (step 1) (neg t) 
                                      (count 0) (oscnum 1)
                                      (eps 1e-03) (upper-bound 200))
 (let ((funval (apply #'funcall fun (append params 
                                    (funcall sample-ratio-fun ntotal)))))
;; function below specified power 
     (cond ((and (< count upper-bound) (< funval (- power eps)))

;; function previously below specified power
            (if neg (multi-sample-search fun sample-ratio-fun params power
                        (+ ntotal (* step (1+ (/ oscnum))))
                        (* step (1+ (/ oscnum))) t (1+ count) oscnum)
;; function previously above specified power
                    (multi-sample-search fun sample-ratio-fun params power
                        (+ ntotal 1) 1 t (1+ count) (1+ oscnum))))


;; function above specified power
           ((and (< count upper-bound) (> funval (+ power eps)))

;; function previously below specified power
            (if neg (multi-sample-search fun sample-ratio-fun params power
                         (- ntotal 1) 1 nil (1+ count) (1+ oscnum))

;; function previously above specified power
                    (multi-sample-search fun sample-ratio-fun params power
                         (- ntotal (* step (1+ (/ oscnum))))
                         (* step (1+ (/ oscnum)))
                         nil (1+ count) oscnum)))
           (t (funcall sample-ratio-fun ntotal)))))

(defun 1-sample-search (fun params power &optional (n 6) (step 1) (neg t)
                                                   (count 0) (bounded nil)
                                                   (osc nil) (eps 1e-03) 
                                                   (upper-bound 200))
 (let ((funval (apply #'funcall fun (append params (list n))))
       (slow (if (and bounded (> count 100)) t nil)))
   (if (< count upper-bound)
    (cond ((and (not osc) (< funval (- power eps)))
             (if neg (1-sample-search fun params power 
                         (if slow (1+ n) (+ n (* 2 step)))
                         (if slow 1 (* step 2)) t (1+ count) slow osc)
                     (1-sample-search fun params power 
                         (1+ n) 1 t (1+ count) t (if slow t nil))))
          ((and (not osc) (> funval (+ power eps)))
              (if neg (1-sample-search fun params power
                         (1- n) 1 nil (1+ count) t (if slow t nil))
                     (1-sample-search fun params power
                         (if slow (1- n) (- n (* step 2)))
                         (if slow 1 (* step 2)) nil (1+ count) slow osc)))
          (t (find-min-n fun params power n eps)))
     (find-min-n fun params power n eps))))



(defun find-min-n (fun params power n eps)
(let (funlist
      funv
      n-list
      (n (+ n 5)))
 (dolist (i (iseq 0 25))
  (setf funv (if (> (- n i) 5) 
                   (apply #'funcall fun (append params (list (- n i)))) 0))
  (setf funlist (append funlist (list funv)))
  (setf n-list (append n-list (list (- n i)))))
 (let* ((minlist (which (> funlist (- power eps))))
        (minfunlist (select funlist minlist)))
  (list (min (select n-list minlist))))))


(defun start-family (family model)
(let* (
       (family-name (case family (0 'normal-family-proto)
                                 (1 'exponential-family-proto)
                                 (2 'binomial-family-proto)
                                 (3 'poisson-family-proto)
                                 (4 'correlation-family-proto)))
       (model-name
          (case family (0 (case model (0 :normal-1-sample)
                                      (1 :normal-2-sample-equal-var)
                                      (2 :normal-2-sample-unequal-var)
                                      (3 :lognormal-equal-var)))
                       (1 (case model (0 :exponential-1-sample)
                                      (1 :exponential-2-sample)))
                       (2 (case model (0 :binomial-1-sample)
                                      (1 :binomial-1-sample-arcsine)
                                      (2 :binomial-2-sample-arcsin)
                                      (3 :binomial-2-sample-median)
                                      (4 :fishers-exact-test)
                                      (5 :proportion-responders-cons-exp)
                                      (6 :case-control)
                                      (7 :binomial-k-sample)))
                       (3 (case model (0 :poisson-1-sample)
                                      (1 :poisson-2-sample)))
                       (4 (case model (0 :correlation-coeff-outcome)))))
       )
  (list family-name model-name)))
                                         

;(start-power 0 0 1 1 2 5 5 .01 25)

;(family model type sides &rest args)





