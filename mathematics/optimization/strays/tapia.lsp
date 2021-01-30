;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These routines implement Tapia's Newton-like method for minimization
;; with nonlinear equality and inequality constraints. We need analytic
;; first and second derivatives for the objective function and the
;; constraints, and the routine mostly relies on the (fast) solve
;; built-in function from Xlisp-Stat.
;;
;; This seems a safe way to go, probably more efficient than pivoting
;; at the Lisp level.
;; 
;; References:
;;
;;     R.A. Tapia : A Stable Approach to Newton's Method for
;;                  General Mathematical Programming Problems in R^n
;;                  JOTA, 14, 1974, 453-476.
;;     R.A. Tapia : Newton's Method for Optimization Problems with
;;                  Equality Constraints
;;                  SIAM J Numer Anal, 11, 1974, 874-886
;;     R.A. Tapia : Newton's Method for Problems with Equality
;;                  Constraints
;;                  SIAM J Numer Anal, 11, 1974, 174-196
;;
;; Version 0.1 -- 09/02/95 -- Jan de Leeuw
;;                            Only equality constraints. Stop
;;                            criterion sort of dumb. No safeguards.
;; Version 0.2 -- 09/03/95 -- Better output, better stop.
;; Version 0.3 -- 09/03/95 -- Inequality constraints added.
;; Version 0.4 -- 09/04/95 -- Gutted the main routine, split it
;;                            into 5 defuns. Clean up. Corrected small
;;                            bug. Added examples.
;; Version 0.5 -- 09/07/95 -- Another bug fix. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
(defun tapia-newton 
  (x0 f g h 
      &key 
      (p (1- (length f))) ;; number of equality constraints
      (eps 1e-6)          ;; maximum change 
      (itmax 100)         ;; maximum # iterations
      (verbose t))        ;; are we verbose ?
"Args: (x0 f g h (p (1- (length f))) (eps 1e-6) (itmax 10)  (verbose t))
Iterations start at X0, a vector argument. The arguments F, G, and
H are lists of closures; giving function values, gradients, and
Hessians. There are M+1 functions. The first one is the function
to be maximized, the next P are the equality constraints, and the
final M-P the inequality constraints. By default P=M, i.e. there 
are only equality constraints. Arguments EPS and ITMAX
regulate iteration cut-off, while VERBOSE, if true, gives some
additional output. The program returns solution, functions values, 
gradients, and Hessians at the solution." 
  (let ((itel 0)
        (x-old x0)
        (y-old (if (< p (1- (length f))) 
                   (tapia-step-0 f x0 p))))
    (loop
     (let* ((fx (mapcar #'(lambda (ff)
                            (funcall ff x-old)) f))
            (gx (mapcar #'(lambda (gg)
                            (funcall gg x-old)) g))
            (hx (mapcar #'(lambda (hh)
                            (funcall hh x-old)) h))
            (bt (tapia-step-1 fx gx y-old p))
            (lm (tapia-step-2 gx hx bt y-old p))
            (dt (tapia-step-3 fx gx bt lm y-old p))
            (x-new (tapia-step-4 x-old lm dt bt y-old p))
            (y-new (if y-old (* y-old (- 1 (plus (drop dt p))))))
            (ch (max (abs (- x-new x-old))))
            (mr (max (abs (rest fx)))))
       (if verbose
           (format t "~3d ~15,10f ~15,10f~%" itel (first fx) mr))
       (if (or (> eps ch) (= itel itmax)) 
           (return (values x-old y-old fx gx hx))
           (setf itel (1+ itel) 
                 x-old x-new
                 y-old y-new))
           )
     )
    )
  )

(defun tapia-step-0 (f x0 p)
  (sqrt (* 2 (abs (mapcar 
                   #'(lambda (ff)
                       (funcall ff x0))
                   (drop f (1+ p))))))
  )
                       

(defun tapia-step-1 (fx gx y-old p)
  (let* ((f0 (first fx))
         (fr (rest fx))
         (g0 (first gx))
         (gr (rest gx))
         (yv (append (repeat 0 p) (if y-old (^ y-old 2))))
         (bb (+ (cross-products gr) (diagonal yv)))
         (be (+ (- fr (mapcar #'(lambda (gg) (sum (* g0 gg))) gr))
                (/ yv 2))))
    (solve bb be)
    )
  )

(defun tapia-step-2 (gx hx bt y-old p)
  (let* ((h0 (first hx))
         (hr (rest hx))
         (cc (+ h0
                (if (> p 0) (weighted-sum-of-matrices 
                             (take bt p) 
                             (take hr p)) 0)
                (if y-old (weighted-sum-of-matrices 
                           (plus (drop bt p)) 
                           (drop hr p)) 0))))
    (mapcar #'(lambda (gg) (solve cc gg)) gx)
    )
  )

(defun tapia-step-3 (fx gx bt lm y-old p)
  (let* ((l0 (first lm))
         (lr (rest lm))
         (f0 (first fx))
         (fr (rest fx))
         (g0 (first gx))
         (gr (rest gx))
         (yv (append (repeat 0 p) (if y-old (^ y-old 2))))
         (aa (append (take gr p)
                     (if y-old
                         (* (drop gr p) (plus (drop bt p))))))
         (bb (append (take fr p)
                     (if y-old 
                         (+ (drop fr p) (/ (^ y-old 2) 2)))))
         (dd (transpose (+ (cross-products aa lr) (diagonal yv))))
         (de (- bb (mapcar #'(lambda (gg)
                               (sum (* gg l0))) gr))))
    (solve dd de)
    )
  )

(defun tapia-step-4 (x-old lm dt bt y-old p)
  (let ((l0 (first lm))
        (lr (rest lm))
        (zz (append (take dt p)
                    (if y-old
                        (* (plus (drop dt p))
                           (plus (drop bt p)))))))
    (- x-old l0 (apply #'+ (* zz lr)))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cross-products (x &optional (y (copy-list x)))
  (let* ((m (length x))
         (n (length (first x)))
         (a (make-array (list m n) :initial-contents x))
         (b (make-array (list m n) :initial-contents y)))
    (matmult a (transpose b))
    )
  )

(defun weighted-sum-of-matrices (w a)
  (apply #'+ (* w a)))

(defun drop (x i)
"Args: (x i)
Drops the first I elements of the list X"
  (select x (which (<= i (iseq (length x)))))
  )

(defun take (x i)
"Args: (x i)
Takes the first I elements of the list X"
  (select x (iseq i))
  )

(defun plus (x)
  (mapcar #'(lambda (z)
              (max 0 z)) x)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Examples
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

This is the problem of minimizing
            f(x) = \sum x^2
under the conditions
            x_1      ? 1
            \sum x   ? 0
where ? can be <= or =.

(setf funcs (list
               #'(lambda (x) (sum (^ x 2)))
               #'(lambda (x) (1- (aref x 0)))
               #'(lambda (x) (sum x))))

(setf grads (list
               #'(lambda (x) (* 2 x))
               #'(lambda (x) (let ((z (make-array (length x) 
                                                  :initial-element 0)))
                               (setf (aref z 0) 1) z))
               #'(lambda (x) (repeat 1 (length x)))))

(setf hesss (list
             #'(lambda (x) (* 2 (identity-matrix (length x))))
             #'(lambda (x) (let ((n (length x)))
                             (make-array (list n n)
                                         :initial-element 0)))
             #'(lambda (x) (let ((n (length x)))
                             (make-array (list n n)
                                         :initial-element 0)))))

|#

#|

This is the problem of minimizing
            f(x) = 1/2 [(x-1)^2 + (y-1)^2]
under the conditions
            x+y ? 1
            -x  ? 0
            -y  ? 0
where ? can be <= or =.

(setf funcs (list
             #'(lambda (x) (/ (sum (^ (1- x) 2)) 2))
             #'(lambda (x) (1- (sum x)))
             #'(lambda (x) (- (aref x 0)))
             #'(lambda (x) (- (aref x 1)))))

(setf grads (list
             #'(lambda (x) (1- x))
             #'(lambda (x) #(1 1))
             #'(lambda (x) #(-1 0))
             #'(lambda (x) #(0 -1))))

(setf hesss (list
             #'(lambda (x) (identity-matrix 2))
             #'(lambda (x) (make-array '(2 2) :initial-element 0))
             #'(lambda (x) (make-array '(2 2) :initial-element 0))
             #'(lambda (x) (make-array '(2 2) :initial-element 0))))
            	
|#

#|

This is the problem of minimizing x'Ax under the conditions
x'x ? 1, where ? is either <= or =.

(setf a #2A((4 -1 -1) (-1 6 -2) (-1 -2 4)))

(setf funcs (list
             #'(lambda (x) (sum (* x (matmult a x))))
             #'(lambda (x) (- (sum (* x x)) 1))))

(setf grads (list
             #'(lambda (x) (* 2 (matmult a x)))
             #'(lambda (x) (* 2 x))))

(setf hesss (list
             #'(lambda (x) (* 2 a))
             #'(lambda (x) (* 2 (identity-matrix 3))))) 

|#

#|

Tapia's example: minimize f(x)=x^2 over x >= 1
             or: minimize f(x)=x^2 over x <= 1

(setf funcs (list 
             #'(lambda (x) (sum (* x x)))
             #'(lambda (x) (- 1 (aref x 0)))))

(setf grads (list
             #'(lambda (x) (* 2 x))
             #'(lambda (x) (vector -1))))

(setf hesss (list
             #'(lambda (x) (make-array '(1 1) :initial-element 2))
             #'(lambda (x) (make-array '(1 1) :initial-element 0))))

or

(setf funcs (list 
             #'(lambda (x) (sum (* x x)))
             #'(lambda (x) (1- (aref x 0)))))

(setf grads (list
             #'(lambda (x) (* 2 x))
             #'(lambda (x) (vector 1))))

(setf hesss (list
             #'(lambda (x) (make-array '(1 1) :initial-element 2))
             #'(lambda (x) (make-array '(1 1) :initial-element 0))))


|#

#|

Monotone regression: minimize (x-y)'(x-y) over Kx <= 0.

(setf y #(1 2 1 4 2))

(setf funcs (list
             #'(lambda (x) (sum (^ (- x y) 2)))
             #'(lambda (x) (- (aref x 0) (aref x 1)))
             #'(lambda (x) (- (aref x 1) (aref x 2)))
             #'(lambda (x) (- (aref x 2) (aref x 3)))
             #'(lambda (x) (- (aref x 3) (aref x 4)))))

(setf grads (list
             #'(lambda (x) (* 2 (- x y)))
             #'(lambda (x) #(1 -1 0 0 0))
             #'(lambda (x) #(0 1 -1 0 0))
             #'(lambda (x) #(0 0 1 -1 0))
             #'(lambda (x) #(0 0 0 1 -1))))

(setf hesss (list
             #'(lambda (x) (* 2 (identity-matrix 5)))
             #'(lambda (x) (make-array '(5 5) :initial-element 0))
             #'(lambda (x) (make-array '(5 5) :initial-element 0))
             #'(lambda (x) (make-array '(5 5) :initial-element 0))
             #'(lambda (x) (make-array '(5 5) :initial-element 0))))

|#

