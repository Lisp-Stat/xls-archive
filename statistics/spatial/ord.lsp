;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file implements the spatial interaction models
;;; described by Keith Ord in JASA, 70, 1975, 120-126.
;;; In educational statistics these are known as Erbring
;;; and Young models. I think they are cute. The Xlisp
;;; is rather boring, but it works. I still have to do
;;; the standard errors.
;;;
;;;                          Jan de Leeuw, 11-25-94
;;;
;;; Jason Bond added standard errors, 02-26-95 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ord-spatial-interaction-model (y w &key (epsilon 1e-8))
(let* (
       (yl (matmult w y))
       (ll (eigenvalues w))
       (s1 (sum (* y y)))
       (s2 (sum (* y yl)))
       (s3 (sum (* yl yl)))
       (rh (ord-newton-find-rho s1 s2 s3 ll))
       )
(list rh (/ (sum (^ (- y (* rh yl)) 2)) (length ll)))
))

(defun ord-mixed-model (x y w &key (epsilon 1e-8))
(let* (
      (ll (eigenvalues w))
      (yl (matmult w y))      
      (my (regression-model x y))
      (ry (send my :residuals))
      (ml (regression-model x yl))
      (rl (send ml :residuals))
      (s1 (sum (* ry ry)))
      (s2 (sum (* ry rl)))
      (s3 (sum (* rl rl)))
      (rh (ord-newton-find-rho s1 s2 s3 ll))
      (zz (- y (* rh yl)))
      (mz (regression-model x z))
      (rz (send mz :residuals))
      )
(list rh (/ (sum (* rz rz)) (length ll)) (send mz :coef-estimates))
))

(defun ord-autoregressive-model (x y w &key (epsilon 1e-8))
(let* (
       (rr (regression-model x y))
       (rs (send rr :residuals))
       (bo (send rr :coef-estimates))
       (ll (eigenvalues w))
       (yl (matmult w y))
       (xl (matmult w x))
       )
(loop
(let* (
       (rl (matmult w rs))
       (s1 (sum (* rs rs)))
       (s2 (sum (* rs rl)))
       (s3 (sum (* rl rl)))
       (rh (ord-newton-find-rho s1 s2 s3 ll))
       (zz (- y (* rh yl)))
       (xx (- x (* rh xl)))
       (zr (regression-model xx zz))
       (bn (send zr :coef-estimates))
       )
(setf rs (send zr :residuals))
(if (> epsilon (max (abs (- bn bo)))) 
    (return (list rh (/ (sum (* rs rs)) (length ll)) bn))
    (setf bo bn))
))))

(defun ord-newton-find-rho (s1 s2 s3 ll &key (old 0) (epsilon 1e-8))
(loop
(let* (
      (q (- 1 (* old ll)))
      (p (sum (log q)))
      (r (/ 2 (length ll)))
      (s (+ s1 (- (* 2 old s2)) (* old old s3)))
      (b (/ (- (* old s3) s2) (^ s 2)))
      (a (sum (/ ll q)))
      (u (sum (/ (^ ll 2) (^ q 2))))
      (v (* 4 (^ b 2)))
      (w (/ (* 2 s3) (^ s 2)))
      (f (- (log s) (* r p)))
      (g (+ (* r a) (* 2 b)))
      (h (+ (* r u) w (- v)))
      (new (- old g h))
      )
(format t "~,8f ~,8f ~,8f ~,8f ~,8f ~%" old new f g h)
(if (> epsilon (abs (- old new))) 
    (return new) (setf old new))))
)

(defun ml-se (y w rho-hat sigma-2)
  (let* (
         (n (array-dimension w 0))
         (eig (eigenvalues w))
         (alpha (sum (^ (/ eig (- 1 (* rho-hat eig))) 2)))
         (bmatrix (matmult (inverse (- (identity-matrix n)
                        (* rho-hat w))) w))
         (a12 (* sigma-2 (sum (diagonal bmatrix))))
         (yl-yl (* sigma-2 (sum (diagonal 
                   (matmult (transpose bmatrix) bmatrix)))))
        )
    (* sigma-2 (inverse (matrix (list 2 2) (combine 
                  (list (/ n 2) a12 a12 (- (* sigma-2 yl-yl)
                        (* alpha (^ sigma-2 2))))))))
  )
)

(defun mixed-se (x y w rho-hat sigma-2 beta-hat)
  (let* (
         (n (array-dimension w 0))
         (p (length beta-hat))
         (eig (eigenvalues w))
         (alpha (sum (^ (/ eig (- 1 (* rho-hat eig))) 2)))
         (amatrix (inverse (- (identity-matrix n) (* rho-hat w))))
         (bmatrix (matmult (inverse 
                             (- (identity-matrix n) (* rho-hat w))) w))
         (a1 (* sigma-2 (inner-product (transpose x) bmatrix x beta-hat)))
         (a2 (* sigma-2 (sum (diagonal bmatrix))))
         (a3 (+ (* sigma-2 
                   (sum (diagonal (matmult (transpose bmatrix) bmatrix))))
                (inner-product a1 a1)))
         (x-prime-x (* sigma-2 (matmult (transpose x) x)))
        )
    (* sigma-2 (inverse 
                (bind-rows
                 (matrix (list 2 (+ 2 p)) (combine 
                    (list (/ n 2) a2 (repeat 0 p) a2 
                          (- (* sigma-2 a3) (* alpha (^ sigma-2 2)))
                          (* sigma-2 (combine a1)))))
                  (bind-columns (matrix (list p 1) (repeat 0 p))
                                (matrix (list p 1) (combine a1))
                                x-prime-x))))
  )
)

(defun auto-reg-se (x y w rho-hat sigma-2 beta-hat)
  (let* (
         (n (array-dimension w 0))
         (p (length beta-hat))
         (eig (eigenvalues w))
         (alpha (sum (^ (/ eig (- 1 (* rho-hat eig))) 2)))
         (amatrix (inverse (- (identity-matrix n) (* rho-hat w))))
         (bmatrix (matmult amatrix w))
         (zmatrix (matmult w (- y (inner-product x beta-hat))))
         (a12 (* sigma-2 (sum (diagonal bmatrix))))
         (a22 (* (^ sigma-2 2) 
                 (- (sum (diagonal
                           (matmult (transpose bmatrix) bmatrix)))
                    alpha)))
         (a33 (* sigma-2 (matmult (transpose (matmult amatrix x))
                          (matmult amatrix x))))
        )
    (* sigma-2
       (inverse 
        (bind-rows (matrix (list 2 (+ p 2)) (combine 
                 (list (/ n 2) a12 (repeat 0 p) a12 a22 (repeat 0 p))))
               (bind-columns (matrix (list p 2) (repeat 0 (* 2 p))) a33))))
  )
)
                         


