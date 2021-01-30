(defproto binary-parametric-proto '(start-val-method start-vals maxiter max-grad-model max-grad-start estimates var-cov grad iterations std-errs model-menu model-menu-items est c std-err-c) () binary-models-proto)

(defmeth binary-parametric-proto :isnew
  (&optional (est t))
  (send self :est est)
  (send self :maxiter 25)
  (send self :max-grad-model .0001)
  (send self :max-grad-start .05)
  (send self :model-dialog-maker))

(defmeth binary-parametric-proto :est (&optional (bern nil set))
"Args: (&optional bern set)
Turns the estimation procedure on (t) and off (nil)"
  (when set (setf (slot-value 'est) bern))
  (slot-value 'est))

(defmeth binary-parametric-proto :estimate ()
  (when (send self :est)
        (case (send self :model-type)
          (0 (send self :logit
                   (send *binary-model-object* :y)
                   (send *binary-model-object* :x)
                   (send self :maxiter)
                   (send self :max-grad-model))
             (send (select
                    (send *binary-model-object* :visual-menu-items) 0)
                   :enabled t))
          (1 (send self :c-log-log
                   (send *binary-model-object* :y)
                   (send *binary-model-object* :x)
                   (send self :maxiter)
                   (send self :max-grad-model))
             (send (select
                    (send *binary-model-object* :visual-menu-items) 0)
                   :enabled t))
          (2 (send self :c-log-log
                   (send *binary-model-object* :y)
                   (send *binary-model-object* :x)
                   (send self :maxiter)
                   (send self :max-grad-model)
                   :comp nil)
             (send (select
                    (send *binary-model-object* :visual-menu-items) 0)
                   :enabled t))
          (3 (send self :3pl
                   (send *binary-model-object* :y)
                   (send *binary-model-object* :x)
                   (send self :start-vals)
                   (send self :maxiter)
                   (send self :max-grad-model))
             (send (select
                    (send *binary-model-object* :visual-menu-items) 0)
                   :enabled t))
          (4 (send self :3pl 
                   (send *binary-model-object* :y)
                   (send *binary-model-object* :x)
                   (send self :start-vals)
                   (send self :maxiter)
                   (send self :max-grad-model)
                   :up t)
             (send (select
                    (send *binary-model-object* :visual-menu-items) 0)
                   :enabled t)))))

(defmeth binary-parametric-proto :model-dialog-maker ()
"Args: none
Sets up the model type dialog for parametric binary regression"
  (let ((selection (choose-item-dialog
                    "Model Type"
                   (list 
                    "Logit"
                    "Complementary Log Log"
                    "Log Log"
                    "Non-zero Asymptote Logit"
                    "Non-one Asymptote Logit"))))
    (case selection
      (0 (send self :est t)
         (send self :model-type 0)
         (send self :estimate))
      (1 (send self :est t)
         (send self :model-type 1)
         (send self :estimate))
      (2 (send self :est t)
         (send self :model-type 2)
         (send self :estimate))
      (3 (send self :est t)
         (send self :model-type 3)
         (send self :sv-choose-meth))
      (4 (send self :est t)
         (send self :model-type 4)
         (send self :sv-choose-meth)))))


(defmeth binary-parametric-proto :sv-choose-meth ()
  (let ((sv-meth (choose-item-dialog "Start value selection method:"
                                   (list "Conditional maximum likelihood"
                                         "User supplied"))))
    (case sv-meth
      (0 (send self :3pl-sv
               (send *binary-model-object* :y)
               (send *binary-model-object* :x)
               (send self :model-type))
         (send self :estimate))
      (1 (send self :start-vals
               (let ((svs
                      (first
                       (get-value-dialog "Enter start values"))))
                 (if (= (send self :model-type) 4)
                     (cons (- 1 (car svs)) (- (cdr svs)))
                     svs)))
         (send self :estimate))
      (nil nil))))

(defmeth binary-parametric-proto :model-menu (&optional (menu nil set))
"Args: (&optional (menu nil set))
Sets or returns model menu"
  (when set (setf (slot-value 'model-menu) menu))
  (slot-value 'model-menu))

(defmeth binary-parametric-proto :model-menu-items (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns list of model menu items"
  (when set (setf (slot-value 'model-menu-items) list))
  (slot-value 'model-menu-items))
         
(defmeth binary-parametric-proto :estimates (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of ML estimates"
  (when set (setf (slot-value 'estimates) list))
  (slot-value 'estimates))

(defmeth binary-parametric-proto :maxiter (&optional (number nil set))
"Args: (&optional number)
Sets or returns maximum number of iterations for ML estimation"
  (when set (setf (slot-value 'maxiter) number))
  (slot-value 'maxiter))

(defmeth binary-parametric-proto :max-grad-model (&optional (number nil set))
"Args: (&optional number)
Sets or returns maximum absolute gradient value for convergence in final model fitting"
  (when set (setf (slot-value 'max-grad-model) number))
  (slot-value 'max-grad-model))

(defmeth binary-parametric-proto :max-grad-start (&optional (number nil set))
"Args: (&optional number)
Sets or returns maximum absolute gradient value for convergence in selecting start values"
  (when set (setf (slot-value 'max-grad-start) number))
  (slot-value 'max-grad-start))

(defmeth binary-parametric-proto :start-val-method
  (&optional (number nil set))
"Args: (&optional number)
Sets or returns start value selection method
    0: User Specified
    1: Conditional ML"
  (when set (setf (slot-value 'start-val-method) number))
  (slot-value 'start-val-method))

(defmeth binary-parametric-proto :start-vals (&optional (list nil set))
"Args: (&optional list)
Sets or returns start values"
  (when set (setf (slot-value 'start-vals) list))
  (slot-value 'start-vals))

(defmeth binary-parametric-proto :var-cov (&optional (matrix nil set))
"Args: (&optional matrix)
Sets or returns asymptotic parameter covariance matrix"
  (when set (setf (slot-value 'var-cov) matrix))
  (slot-value 'var-cov))

(defmeth binary-parametric-proto :grad (&optional (list nil set))
"Args: (&optional list)
Sets or returns convergence gradient"
  (when set (setf (slot-value 'grad) list))
  (slot-value 'grad))

(defmeth binary-parametric-proto :c (&optional (estimate nil set))
"Args: (&optional estimate)
Sets or returns c parameter estimate"
  (when set (setf (slot-value 'c) estimate))
  (slot-value 'c))

(defmeth binary-parametric-proto :std-err-c (&optional (estimate nil set))
"Args: (&optional estimate)
Sets or returns standard error of c parameter estimate"
  (when set (setf (slot-value 'std-err-c) estimate))
  (slot-value 'std-err-c))

(defmeth binary-parametric-proto :iterations (&optional (number nil set))
"Args: (&optional number)
Sets or returns number of iterations"
  (when set (setf (slot-value 'iterations) number))
  (slot-value 'iterations))

(defmeth binary-parametric-proto :std-errs (&optional (list nil set))
"Args: (&optional list)
Sets or returns parameter standard errors"
  (when set (setf (slot-value 'std-errs) list))
  (slot-value 'std-errs))

(defmeth binary-parametric-proto :3pl
  (y x estimates &optional (maxiter 25) (max-grad .001) &key (up nil))
  (flet ((obs-mult (x y) 
                   (apply #'bind-rows (* y (row-list x)))))
    (let  ((n (length y))
           (c (elt estimates 0))
           (beta (rest estimates))
           (logit nil)
           (pred-val nil)
           (yy (if up
                  (- 1 y)
                  y)))
      (do* ((i 1 (+ i 1))
            (foo (- 1 c) (- 1 c))
            (k1 (exp (matmult x beta)) (exp (matmult x beta)))
            (k2 (+ c k1) (+ c k1))
            (k3 (/ k1 (^ 2 k2)) (/ k1 (^ 2 k2)))
            (k4 (/ k1 (+ 1 k1)) (/ k1 (+ 1 k1)))
            (k5 (- (sum yy) n) (- (sum yy) n))
            (xy-tilde (obs-mult x (* yy k3)) (obs-mult x (* yy k3)))
            (h-beta 
             (matmult (transpose 
                       (- (* c xy-tilde)
                          (obs-mult x (* k4 (- 1 k4))))) x)
             (matmult (transpose 
                       (- (* c xy-tilde)
                          (obs-mult x (* k4 (- 1 k4))))) x))
            (h-c (- (/ k5 (^ foo 2)) (sum (/ yy (^ k2 2))))
                 (- (/ k5 (^ foo 2)) (sum (/ yy (^ k2 2)))))
            (h-c-beta (- (mapcar #'sum (column-list xy-tilde)))
                      (- (mapcar #'sum (column-list xy-tilde))))
            (q (combine (+ (sum (/ yy  k2) (/ k5 foo)))
                        (matmult (- (* yy (/ k1 k2)) k4) x))
               (combine (+ (sum (/ yy  k2) (/ k5 foo)))
                        (matmult (- (* yy (/ k1 k2)) k4) x)))
            (h (bind-columns (combine h-c h-c-beta)
                             (bind-rows h-c-beta h-beta))
               (bind-columns (combine h-c h-c-beta)
                             (bind-rows h-c-beta h-beta)))
            (inv-h (inverse h) (inverse h))
            (foo1 (- (combine c beta) (matmult inv-h q))
                  (- (combine c beta) (matmult inv-h q)))
            (beta (rest foo1) (rest foo1))
            (c (first foo1)(first foo1)))
           ((or (<= maxiter i)
                (>= max-grad (max (abs q)))
                (< 1 c))
            (cond 
              ((< 1 c)
               (message-dialog
                (format nil 
                        "Asymptote out of range ~a.
Try a different model type or better start values")))
              (t
               (let* ((estimates (if up (- beta) beta))
                      (pred-val (matmult x beta))
                      (logit (exp pred-val))
                      (pred-val-p (if up (- 1 (/ (+ c logit) (+ 1 logit)))
                                      (/ (+ c logit) (+ 1 logit))))
                      (c (if up (- 1 c) c))
                      (pred-val (if up (- pred-val) pred-val))
                      (var (* pred-val-p (- 1 pred-val-p)))
                      (xv (apply #'bind-rows (* (row-list x) (sqrt var))))
                      (trans-xv (transpose xv))
                      (hj (diagonal
                           (matmult xv 
                                    (inverse (matmult trans-xv xv))
                                    trans-xv)))
                      (resids-z (/ (- y pred-val-p) (sqrt var)))
                      (log1 (log pred-val-p))
                      (log2 (log (- 1 pred-val-p)))
                      (dj (- (* y (sqrt (* 2 (abs log1)))) 
                             (* (- 1 y) (sqrt (* 2 (abs log2))))))
                      (m1hj (- 1 hj))
                      (rsj (/ resids-z (sqrt m1hj)))
                      (delta-x-sq (^ rsj 2))
                      (var-cov (- inv-h))
                      (std-errs (sqrt (diagonal var-cov)))
                      (std-err-c (first std-errs))
                      (adm1 (- (array-dimension var-cov 1) 1))
                      (beta-seq (+ 1 (iseq adm1)))
                      (var-cov-beta (select var-cov beta-seq beta-seq))
                      (std-errs-beta (rest std-errs)))
                 (send self :estimates estimates)
                 (send self :var-cov var-cov)
                 (send self :grad q)
                 (send self :iterations i)
                 (send self :pred-val-p pred-val-p)
                 (send self :pred-val pred-val)
                 (send self :std-errs std-errs-beta)
                 (send self :c c)
                 (send self :std-err-c std-err-c)
                 (send self :hj hj)
                 (send self :delta-beta 
                       (/ (* delta-x-sq hj)
                          m1hj))
                 (send self :delta-x-sq delta-x-sq)
                 (send self :c-bar (* delta-x-sq hj))
                 (send self :dfbeta 
                       (/ (mapcar
                           #'(lambda (xx)
                               (* xx
                                  (/ (- y pred-val-p) m1hj)))
                           (column-list (matmult x var-cov-beta)))
                          std-errs-beta))
                 (send self :delta-d 
                       (+ (^ dj 2)
                          (/ (* (^ resids-z 2) hj)
                             m1hj)))
                 (send self :resids-z resids-z)
                 (send self :rsj rsj)
                 (send self :dj dj)
                 (send self :z (+ pred-val resids-z))
                 (send self :likelyhood (sum (+ (* y log1)
                                                (* (- 1 y) log2))))
                 (send self :resids-y (- y pred-val-p))))))))))

(defmeth binary-parametric-proto :3pl-sv (y x up-down)
  (let ((u-d (= up-down 4)))
    (when u-d (setf y (- 1 y)))
    (do* ((i 1 (+ i 1))
          (beta
           (send self :logit y x (send self :maxiter)
                 (send self :max-grad-start)  :starter t)
           (send self :3pl-beta beta c y x))
          (c (send self :3pl-c beta 0 y x)
             (send self :3pl-c beta c y x))
          (beta-foo (if u-d (- beta) beta)
                    (if u-d (- beta) beta))
          (c-foo (if u-d (- 1 c) c)
                 (if u-d (- 1 c) c)))
         ((or
           (< 1 (abs c-foo))
           (and
            (integerp (/ i 5))
            (not
             (ok-or-cancel-dialog
              (format nil
"Parameter estimates are now ~a for the asymptote
and ~a for the betas.
Do you want another iteration?" c-foo beta-foo)))))
          (cond 
            ((< 1 (abs c-foo))
             (message-dialog (format nil "asymptote out of range ~a" c-foo))
             (send self :est nil))
            (t (send self :start-vals (combine c beta))))))))

(defmeth binary-parametric-proto :3pl-beta
  (beta c y x &optional (maxiter 25) (max-grad .05))
  (flet ((obs-mult (x y) (apply #'bind-rows (* y (row-list x)))))
    (let ((n (length y))
          (k1 (exp (matmult x beta))))
      (do* ((i 1 (+ 1 i))
            (k2 (+ c k1) (+ c k1))
            (k3 (/ k1 (^ 2 k2)) (/ k1 (^ 2 k2)))
            (k4 (/ k1 (+ 1 k1)) (/ k1 (+ 1 k1)))
            (xy-tilde (obs-mult x (* y k3)) (obs-mult x (* y k3)))
            (h (matmult (transpose 
                         (- (* c xy-tilde)
                            (obs-mult x (* k4 (- 1 k4))))) x)
               (matmult (transpose 
                         (- (* c xy-tilde)
                            (obs-mult x (* k4 (- 1 k4))))) x))
            (q (matmult (- (* y (/ k1 k2)) k4) x)
               (matmult (- (* y (/ k1 k2)) k4) x))
            (beta (- beta (matmult (inverse h) q))
                  (- beta (matmult (inverse h) q))))
           ((or (>= i maxiter) (<= (abs q) max-grad)) beta)))))

  (defmeth binary-parametric-proto :3pl-c
  (beta c y x &optional (maxiter 25) (max-grad .05))
  (let ((n (length y))
        (k1 (exp (matmult x beta))))
    (do* ((i 1 (+ i 1))
          (foo (- 1 c) (- 1 c))
          (k2 (+ c k1) (+ c k1))
          (k5 (- (sum y) n) (- (sum y) n))
          (h (- (/ k5 (^ foo 2)) (sum (/ y (^ k2 2))))
             (- (/ k5 (^ foo 2)) (sum (/ y (^ k2 2)))))
          (q (+ (sum (/ y  k2) (/ k5 foo)))
             (+ (sum (/ y  k2) (/ k5 foo))))
          (c (- c (/ q h)) (- c (/ q h))))
         ((or (>= i maxiter) (<= (abs q) max-grad)) c))))

(defmeth binary-parametric-proto :c-log-log
  (y x &optional (maxiter 25) (max-grad .001) &key (comp t))
  (setf y (if comp y (- 1 y)))
  (flet ((new-raph 
          (x var resid)
          (let* ((x-prime (transpose x))
                 (inv-h (inverse 
                         (matmult x-prime 
                                  (apply #'bind-rows (* (row-list x) var)))))
                 (q (matmult x-prime resid)))
            (list (matmult inv-h q) inv-h q))))
    (do* ((iter 1 (+ iter 1))
          (var (- (* y .661303) 1)
               (- (* y
                     (- (/ exp1 one-exp2)
                        (/ (* (^ exp1 2) exp2)(^ one-exp2 2))))
                  exp1))
          (resid (- (* y 1.58198) 1)
                 (- (* y (/ exp1 one-exp2)) exp1))
          (ests (new-raph x var resid) (new-raph x var resid))
          (beta (first ests) (- beta (first ests)))
          (exp1 (exp (matmult x beta)) (exp (matmult x beta)))
          (exp2 (exp (- exp1)) (exp (- exp1)))
          (one-exp2 (- 1 exp2) (- 1 exp2)))
         ((or (>= iter maxiter)(>= max-grad (max (abs (third ests)))))
          (let* ((estimates (if comp beta (- beta)))
                 (pred-val (matmult x estimates))
                 (expp exp1)
                 (pred-val-p (if comp one-exp2 exp2))
                 (var (* pred-val-p (- 1 pred-val-p)))
                 (x-prime (transpose x))
                 (inv-h (inverse 
                         (matmult x-prime 
                                  (apply #'bind-rows (* (row-list x) var)))))
                 (q (matmult x-prime resid))
                 (xv (apply #'bind-rows (* (row-list x) (sqrt var))))
                 (trans-xv (transpose xv))
                 (hj (diagonal
                      (matmult xv 
                               (inverse (matmult trans-xv xv))
                               trans-xv)))
                 (resids-z (/ (- y pred-val-p) (sqrt var)))
                 (log1 (log pred-val-p))
                 (log2 (log (- 1 pred-val-p)))
                 (dj (- (* y (sqrt (* 2 (abs log1)))) 
                        (* (- 1 y) (sqrt (* 2 (abs log2))))))
                 (m1hj (- 1 hj))
                 (rsj (/ resids-z (sqrt m1hj)))
                 (delta-x-sq (^ rsj 2))
                 (var-cov inv-h)
                 (std-errs (sqrt (diagonal var-cov))))
            (send self :estimates estimates)
            (send self :var-cov var-cov)
            (send self :grad q)
            (send self :iterations iter)
            (send self :pred-val-p pred-val-p)
            (send self :pred-val pred-val)
            (send self :std-errs std-errs)
            (send self :hj hj)
            (send self :delta-beta 
                  (/ (* delta-x-sq hj)
                     m1hj))
            (send self :delta-x-sq delta-x-sq)
            (send self :c-bar (* delta-x-sq hj))
            (send self :dfbeta 
                  (/ (mapcar
                      #'(lambda (xx)
                          (* xx
                             (/ (- y pred-val-p) m1hj)))
                      (column-list (matmult x var-cov)))
                     std-errs))
            (send self :delta-d 
                  (+ (^ dj 2)
                     (/ (* (^ resids-z 2) hj)
                        m1hj)))
            (send self :resids-z resids-z)
            (send self :rsj rsj)
            (send self :dj dj)
            (send self :z (+ pred-val resids-z))
            (send self :likelyhood (sum (+ (* y log1)
                                           (* (- 1 y) log2))))
            (send self :resids-y (- y pred-val-p)))))))

(defmeth binary-parametric-proto :model-type (&optional (number nil set))
"Args: (&optional number)
Sets or returns model type
     0: Logit
     1: Complementary Log Log
     2: Log Log
     4: Non-Zero Asymptote Logit
     5: Non-One Asymptote Logit"
  (when set (setf (slot-value 'model-type) number))
  (slot-value 'model-type))
