;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the wild bootstrap of Liu, Haerdle, Mammen and others,
;; for functions of a vector of means. Compare, for instance,
;; Mammen, When does Bootstrap work ?, Springer, Lecture Notes in
;; Statistics no 77, 1992, or Liu, Annals of Statistics, 16, 1988,
;; 1696-1708.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wild-bootstrap (nboot stat func data weight)
"Args: (nboot stat func data weight)
This uses the \"wild bootstrap\" to compute NBOOT boostrap
replications of a possibly multivariate statistics of the
form FUNC(average(STAT(DATA_i))). It allows for various
choices of weight functions."
  (let* ((y (mapcar #'(lambda (x) (funcall stat x)) data))
         (n (length y))
         (s (/ (apply #'+ y) n))
         (v (funcall func s))
         ff
         (it 0))
    (loop
     (let ((w (funcall weight n))
           (z (- y (/ s n))))
       (setf ff (append ff (list (funcall func (apply #'+ (* w z))))))
       (incf it)
       (if (= it nboot) (return ff)))
     )
    )
  )

(defun normal-weights (n)
  (normal-rand n))

(defun poisson-weights (n)
  (1- (poisson-rand n 1))
  )

(defun golden-section-weights (n)
  (let ((v (uniform-rand n))
        (c (/ (1+ (sqrt 5)) (* 2 (sqrt 5))))
        (w1 (/ (- 1 (sqrt 5)) 2))
        (w2 (/ (+ 1 (sqrt 5)) 2)))
    (mapcar #'(lambda (x)
                (if (< x c) w1 w2)) v)
    )
  )

(defun normal-and-square-weights (n)
  (let ((v (normal-rand n)))
    (+ (/ v (sqrt 2)) (/ (1- (* v v)) 2))
    )
  )

(defun two-normal-weights (n)
  (let ((v1 (normal-rand n))
        (v2 (normal-rand n))
        (d1 (sqrt (+ 0.75 (/ (sqrt 17) 12))))
        (d2 (sqrt (- 0.75 (/ (sqrt 17) 12)))))
    (- (* (+ d1 (/ v1 (sqrt 2))) (+ d2 (/ v2 (sqrt 2)))) 
       (* d1 d2))
    )
  )
     