;;;;
;;;;                  Fitting a Bradley-Terry Model with the
;;;;                          Lisp-Stat GLIM System

(require "glim")

;;;
;;; Function for fitting the model as a binomial regression model
;;;

(defun bradley-terry-model (counts &key labels)
  (let* ((n (round (sqrt (length counts))))
         (i (iseq 1 (- n 1)))
         (low-i (apply #'append (+ (* n i) (mapcar #'iseq i))))
         (p-names (if labels
                      (rest labels) 
                      (level-names (iseq n) :prefix "Choice"))))
    (labels ((tr (x)
               (apply #'append (transpose (split-list (coerce x 'list) n))))
             (lower (x) (select x low-i))
             (low-indicators (x) (mapcar #'lower (indicators x))))
      (let ((wins (lower counts))
            (losses (lower (tr counts)))
            (rows (low-indicators (repeat (iseq n) (repeat n n))))
            (cols (low-indicators (repeat (iseq n) n))))
        (binomialreg-model (- rows cols)
                           wins 
                           (+ wins losses)
                           :intercept nil
                           :predictor-names p-names)))))

;;;;
;;;;
;;;; Bradley-Terry Model Prototype
;;;;
;;;;

(defproto bradley-terry-proto () () binomialreg-proto)

;;;
;;; Constructor Function
;;;

(defun bradley-terry-model (counts &key labels)
  (let* ((n (round (sqrt (length counts))))
         (i (iseq 1 (- n 1)))
         (low-i (apply #'append (+ (* n i) (mapcar #'iseq i))))
         (p-names (if labels
                      (rest labels) 
                      (level-names (iseq n) :prefix "Choice"))))
    (labels ((tr (x)
               (apply #'append (transpose (split-list (coerce x 'list) n))))
             (lower (x) (select x low-i))
             (low-indicators (x) (mapcar #'lower (indicators x))))
      (let ((wins (lower counts))
            (losses (lower (tr counts)))
            (rows (low-indicators (repeat (iseq n) (repeat n n))))
            (cols (low-indicators (repeat (iseq n) n))))
        (send bradley-terry-proto :new
              :x (- rows cols)
              :y wins
              :trials (+ wins losses)
              :intercept nil
              :predictor-names p-names)))))

;;;;
;;;; Methods for Estimating Success Probabilities
;;;;

;;;
;;; Simplest version of :SUCCESS-PROB
;;;
(defmeth bradley-terry-proto :success-prob (i j)
  (let* ((phi (cons 0 (send self :coef-estimates)))
         (exp-logit (exp (- (select phi i) (select phi j)))))
    (/ exp-logit (+ 1 exp-logit))))

;;;
;;; Version with standard errors based on :SUCCESS-LOGIT
;;;
(defmeth bradley-terry-proto :success-prob (i j &optional stdev)
  (let* ((success-logit (send self :success-logit i j stdev))
         (exp-logit (exp (if stdev (first success-logit) success-logit)))
         (p (/ exp-logit (+ 1 exp-logit)))
         (s (if stdev (* p (- 1 p) (second success-logit)))))
    (if stdev (list p s) p)))

;;;
;;; Non-vectorized version of :SUCCESS-LOGIT
;;;
(defmeth bradley-terry-proto :success-logit (i j &optional stdev)
  (let ((coefs (send self :coef-estimates)))
    (flet ((lincomb (i j)
             (let ((v (repeat 0 (length coefs))))
               (if (/= 0 i) (setf (select v (- i 1)) 1))
               (if (/= 0 j) (setf (select v (- j 1)) -1))
               v)))
      (let* ((v (lincomb i j))
             (logit (inner-product v coefs))
             (var (if stdev (matmult v (send self :xtxinv) v))))
        (if stdev (list logit (sqrt var)) logit)))))

;;;
;;; Vectorized version of :SUCCESS-LOGIT
;;;
(defmeth bradley-terry-proto :success-logit (i j &optional stdev)
  (let ((coefs (send self :coef-estimates)))
    (flet ((lincomb (i j)
             (let ((v (repeat 0 (length coefs))))
               (if (/= 0 i) (setf (select v (- i 1)) 1))
               (if (/= 0 j) (setf (select v (- j 1)) -1))
               v))
           (matrix-or-list (x)
             (let ((x (coerce x 'list)))
               (if (numberp (first x)) x (apply #'bind-rows x))))
           (apply-sum (x)
             (if (matrixp x) (mapcar #'sum (row-list x)) (apply #'sum x))))
      (let* ((v (matrix-or-list (map-elements #'lincomb i j)))
             (xtxinv (if stdev (send self :xtxinv)))
             (logit (matmult v coefs))
             (var (if stdev (apply-sum (* (matmult v xtxinv) v)))))
        (if stdev (list logit (sqrt var)) logit)))))

;;;
;;; Baseball Data Example from Agresti
;;;

#|
(def wins-losses '( -  7  9  7  7  9 11
                    6  -  7  5 11  9  9
                    4  6  -  7  7  8 12
                    6  8  6  -  6  7 10
                    6  2  6  7  -  7 12
                    4  4  5  6  6  -  6
                    2  4  1  3  1  7  -))

(def teams '("Milwaukee" "Detroit" "Toronto" "New York"
             "Boston" "Cleveland" "Baltimore"))

(def wl (bradley-terry-model wins-losses :labels teams))
(send wl :success-prob 4 3)
|#
