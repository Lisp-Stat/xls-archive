;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This maximizes a function under nonnegativity constraints. It's
;; engine is newtonmax, while it deals with the constraints using
;; an active set strategy (or, to use Zangwill's terminology,
;; manifold suboptimization).
;; 
;; Lisp-wise it is interesting that the function is recursive, and
;; uses a function that generates modified objhective functions on
;; the submanifold. 
;;
;; See, for instance, Zangwill, Nonlinear Programming, Prentice
;; Hall, 1969, section 8.3
;;
;; Version 1.0 -- 08/08/95 -- Seems to work
;;
;; Jan de Leeuw -- UCLA Center for Statistics
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nonneg-newtonmax 
  (func grad x &optional (ind (which (> x 0))) (eps 1e-8))
  (let* (flag
         (n (length x))
         (y (repeat 0 n)))
    (setf (select y ind) (select x ind))
    (if ind
        (let* ((z (select y ind))
               (g (restrict-function func n ind))
               (r (newtonmax g z :verbose nil)))
          (setf (select r (which (< (abs r) eps))) 0)
          (setf (select y ind) r))) 
    (if (>= (min y) 0)
        (let* ((h (funcall grad y))
               (s (max h)))
          (if (<= s 0) 
              (setf flag "done") 
              (setf flag "relax"
                    x y 
                    ind (append ind (list (position s h))))))
        (let* ((i (which (< r 0)))
               (u (select r i))
               (v (select z i))
               (w (- (/ u (- v u))))
               (l (max w)))
          (setf flag "restrict"
                x (+ y (* l (- x y))) 
                ind (which (> x 0))))
        )
    (if (string-equal flag "done") y
        (nonneg-newtonmax func grad x ind))
    )
  )
    
      
(defun restrict-function (f n ind)
  (lambda (z)
    (let ((y (repeat 0 n)))
      (setf (select y ind) z) 
      (funcall f y)
      )
    )
  )
  