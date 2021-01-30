;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Various Metropolis-Hastings methods for sampling from a (possibly
;;  non-normalized) density. Most of this stuff is inspired by
;;  Tierney, Markov Chains for Exploring Posterior Distributions,
;;  and by Chib and Greenberg, Understanding the Metropolis-Hastings
;;  Algorithm.
;;
;;  version 0.1 -- 06-14-95 -- barebones metropolis-hastings
;; 
;;  version 0.2 -- 06-15-95 -- variations on metropolis-hastings
;;                             some optimization
;;
;;  version 0.3 -- 06-16-95 -- simplified code by using macros
;;                             some simple optimizations
;;                             more variations
;;                             accept-reject counts
;;                             examples
;;
;;  version 0.4 -- 06-17-95 -- small bugs corrected
;;                             symmetric autoregression case
;;                             implemented rejection-sampling method
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "monte-carlo")

(defun metropolis-hastings-chain 
  (p-dens q-dens q-rand x n type
          &key
          (b (identity-matrix (length x)))   ;; only for auto-regressive
          (c (make-list (length x)))         ;; only for auto-regressive
          (r-dens #'(lambda ()))             ;; only for rejection-sampling
          (r-rand #'(lambda ()))             ;; only for rejection-sampling
          (bound 0)                          ;; only for rejection-sampling
          (verbose t))
  (let ((ff (make-list n))
        (accept 0)
        (reject 0))
    (dotimes (i n ff)
      (let* ((y (candidate type))
             (d (make-denominator type))
             (a (acceptance-probability type)))
        (cond ((<= (random 1.0) a) (incf accept) (setf x y))
              (t (incf reject)))
        (setf (elt ff i) x)
        (if verbose
            (format t "Accept ~d Reject ~d~%" accept reject))
        )
      )
    )
  )

(defmacro candidate (type)
  `(cond ((or (string-equal ,type "metropolis")
              (string-equal ,type "metropolis-hastings"))
          (funcall q-rand x))
         ((string-equal ,type "independence")
          (funcall q-rand))
         ((or (string-equal ,type "random-walk")
              (string-equal ,type "symmetric-random-walk"))
          (+ x (funcall q-rand)))
         ((or (string-equal ,type "auto-regression")
              (string-equal ,type "symmetric-auto-regression"))
             (+ c (matmult b (- x c)) (funcall q-rand)))
         ((string-equal ,type "rejection-sampling")
          (first (accept-reject p-dens r-dens r-rand bound 1 :verbose nil)))
         )
  )


(defmacro make-denominator (type)
  `(cond ((or (string-equal ,type "metropolis")
              (string-equal ,type "symmetric-random-walk")
              (string-equal ,type "symmetric-auto-regression"))
          (funcall p-dens x))
         ((or (string-equal ,type "metropolis-hastings")
              (string-equal ,type "independence"))
          (* (funcall p-dens x) (funcall q-dens x y))) 
         ((string-equal ,type "random-walk") 
          (* (funcall p-dens x) (funcall q-dens (- y x))))
         ((string-equal ,type "auto-regression")
          (* (funcall p-dens x) 
             (funcall q-dens (- y c (matmult b (- x c))))))
         ((string-equal ,type "rejection-sampling") nil)
         )
  )  

(defmacro make-numerator (type)
  `(cond ((or (string-equal ,type "metropolis")
              (string-equal ,type "symmetric-random-walk")
              (string-equal ,type "symmetric-auto-regression"))
          (funcall p-dens y))
         ((or (string-equal ,type "metropolis-hastings")
              (string-equal ,type "independence"))
          (* (funcall p-dens y) (funcall q-dens y x))) 
         ((string-equal ,type "random-walk") 
          (* (funcall p-dens y) (funcall q-dens (- x y))))
         ((string-equal ,type "auto-regression")
          (* (funcall p-dens y) 
             (funcall q-dens (- x c (matmult b (- y c))))))
         )
  )

(defmacro acceptance-probability (type)
  `(cond ((string-equal ,type "rejection-sampling")
          (let ((px (funcall p-dens x))
                (py (funcall p-dens y))
                (rx (funcall r-dens x))
                (ry (funcall r-dens y)))
            (cond ((<= px (* bound rx)) 1)
                  ((and (> px (* bound rx))
                        (<= py (* bound ry))) (/ (* bound rx) px))
                  (t (min 1 (/ (* py rx) (* px ry)))))))
         (t (if (> d 0)
                (min 1 (/ (make-numerator ,type) d)) 1))
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Stuff for examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinormal-dens (x mu inverse-sigma)
  (exp (* -.5 (matmult (- x mu) (matmult inverse-sigma (- x mu)))))
)

(defun independent-normal-dens (x mu sigma)
  (let* ((zz (/ (- x mu) (sqrt sigma)))
         (pp (/ (normal-dens zz) (sqrt sigma))))
    (prod pp)
    )
  )

(defun multiuniform-rand (d)
  (* d (1- (* 2 (mapcar #'random (repeat 1.0 (length d))))))
)

(defun multiuniform-dens (d)
  (/ (prod d))
)

(defun multinormal-rand (mu sigma)
  (let ((s (chol-decomp sigma)))
    (+ mu (matmult s (normal-rand (length mu)))))
  )

(defun independent-normal-rand (mu sigma)
  (+ mu (* (sqrt sigma) (normal-rand (length mu))))
  )

(defmeth scatterplot-proto :add-list-of-points (x)
  (send self :add-points
        (mapcar #'first x)
        (mapcar #'second x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Actual examples, from Chib and Greenberg, Understanding the Metropolis-
;; Hastings Algorithm. Just paste relevant parts into a running Xlisp-Stat 
;; process.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(def p-dens #'(lambda (x)
                (multinormal-dens x '(1 2)
                                  (inverse #2A((1 .9) (.9 1))))))

(def q-rand #'(lambda ()
                (multiuniform-rand '(.75 1))))

(def q-dens #'(lambda ()))

(def type "symmetric-random-walk")

(setf pp (plot-points nil nil))

(send pp :add-list-of-points
      (metropolis-hastings-chain 
       p-dens q-dens q-rand '(0 0) 4000 type))

(send pp :adjust-to-data)

|#

#|

(def p-dens #'(lambda (x)
                (multinormal-dens x '(1 2)
                                  (inverse #2A((1 .9) (.9 1))))))

(def q-rand #'(lambda ()
                (independent-normal-rand '(0 0) '(0.6 0.4))))

(def q-dens #'(lambda ()))

(def type "symmetric-random-walk")

(setf pp (plot-points nil nil))

(send pp :add-list-of-points
      (metropolis-hastings-chain 
       p-dens q-dens q-rand '(0 0) 4000 type))

(send pp :adjust-to-data)

|#

#|

(def p-dens #'(lambda (x)
                (multinormal-dens x '(1 2)
                                  (inverse #2A((1 .9) (.9 1))))))
(def r-dens #'(lambda (x)
                (independent-normal-dens x '(1 2) '(2 2))))

(def r-rand #'(lambda ()
                (independent-normal-rand '(1 2) '(2 2))))

(def bound .9)
                                  (def p-dens #'(lambda (x)
                (multinormal-dens x '(1 2)
                                  (inverse #2A((1 .9) (.9 1))))))
(def r-dens #'(lambda (x)
                (independent-normal-dens x '(1 2) '(2 2))))

(def r-rand #'(lambda ()
                (independent-normal-rand '(1 2) '(2 2))))

(def bound .9)
                                  
(def q-dens #'(lambda ()))

(def type "rejection-sampling")

(setf pp (plot-points nil nil))

(send pp :add-list-of-points
      (metropolis-hastings-chain 
       p-dens q-dens q-rand '(0 0) 4000 type :bound bound 
       :r-dens r-dens :r-rand r-rand ))

(send pp :adjust-to-data)

(def q-dens #'(lambda ()))

(def type "rejection-sampling")

(setf pp (plot-points nil nil))

(send pp :add-list-of-points
      (metropolis-hastings-chain 
       p-dens q-dens q-rand '(0 0) 4000 type :bound bound 
       :r-dens r-dens :r-rand r-rand ))

(send pp :adjust-to-data)

|#

#|

(def p-dens #'(lambda (x)
                (multinormal-dens x '(1 2)
                                  (inverse #2A((1 .9) (.9 1))))))

(def q-rand #'(lambda ()
                (multiuniform-rand '(1 1))))

(def q-dens #'(lambda ()))

(def type "symmetric-auto-regression")

(def b-matrix (- #2a((1 0) (0 1))))

(def c-vector '(1 2))

(setf pp (plot-points nil nil))

(send pp :add-list-of-points
      (metropolis-hastings-chain 
       p-dens q-dens q-rand '(0 0) 4000 type :b b-matrix :c c-vector))

(send pp :adjust-to-data)

|#




