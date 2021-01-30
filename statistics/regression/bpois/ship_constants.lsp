;;;
;;; This code can be used to estimate the constants of 
;;; proportionality for Normal priors.
;;; It is all set to go with the ship data.
;;; With the ship data, be prepared for a wait for a while.

(format t "Remember, this could take a while...~%")

(defvar dconst (/ 1 (sqrt (* 2 pi))))
(defvar ldconst (log dconst))

(def d1 (read-data-columns "ship1.out" 9))
(def d2 (read-data-columns "ship2.out" 9))
(def d3 (read-data-columns "ship3.out" 9))
(def d4 (read-data-columns "ship4.out" 9))

(def data (list d1 d2 d3 d4))

(def m (length data)) ; the no of chains.

(def hypers (list (list 
		   (list -10.0 10.0)
		   (list -3.0 9.0)
		   (list -3.0 9.0)
		   (list -3.0 8.0)
		   (list -3.0 8.0)
		   (list 3.0 9.0)
		   (list 3.0 9.0)
		   (list 3.0 9.0)
		   (list 3.0 8.0))
		  (list
		   (list -10.0 10.0)
		   (list -3.0 9.0)
		   (list 3.0 9.0)
		   (list 3.0 8.0)
		   (list 3.0 8.0)
		   (list 3.0 9.0)
		   (list 3.0 9.0)
		   (list 3.0 9.0)
		   (list 3.0 8.0))
		  (list
		   (list 5.0 10.0)
		   (list 3.0 9.0)
		   (list 3.0 9.0)
		   (list 3.0 8.0)
		   (list 3.0 8.0)
		   (list -3.0 9.0)
		   (list -3.0 9.0)
		   (list -3.0 9.0)
		   (list -3.0 8.0))
		  (list
		   (list 5.0 10.0)
		   (list 3.0 9.0)
		   (list -3.0 9.0)
		   (list -3.0 8.0)
		   (list -3.0 8.0)
		   (list -3.0 9.0)
		   (list -3.0 9.0)
		   (list -3.0 9.0)
		   (list -3.0 8.0))))

(def npar (length (select hypers 0))) ; the number of Betas.

(defun priordens (i j)  ; The prior measure at i-th point in j-th chain. 
  (let ((d (get-data i j))
	(result 1.0))
    (dotimes (l npar)
	     (let ((h (get-hypers j l)))
	       (setf result (* result (ndens (select d l) (select h 0) (select h 1))))))
    result))

(defun get-data (i j) ; the i-th DATA POINT from j-th chain.
  (mapcar #'(lambda(x) (select (select (select data j) x) i)) (iseq npar)))

(defun get-hypers (j k) ; hyper for the k-th param for j-th chain.
  (select (select hypers j) k))

(defun ndens (x mu sigma) ; The normal density.
  (let ((y (/ (- x mu) sigma)))
    (/ (* dconst (exp (* -0.5 y y))) sigma)))

(defun lndens (x mu sigma)  ; The log of the normal density
  (let ((y (/ (- x mu) sigma)))
    (- ldconst (log sigma) (* 0.5 y y))))

(defun p (i j c) ; Prob that i-th point came from j-th chain given c.
  (let ((s (* 0.25 (sum (* (priordens i 0) (select c 0))  ; .25 is just 1/m.
			(* (priordens i 1) (select c 1))
			(* (priordens i 2) (select c 2))
			(* (priordens i 3) (select c 3))))))
    (/ (* (priordens i j) (select c j)) s)))

(defun lp (i j c) ; Log p(i j c).
  (let ((s (p i j c)))
    (if (> s 0.0)
        (log s)
      -1e10)))

(defun bigll (c) ; Log quasi likelihood.
  (setf w (cons 1.0 c))
  (let ((ll0 (reduce #'+ (mapcar #'(lambda(x) (lp x 0 w)) (iseq 500))))
	(ll1 (reduce #'+ (mapcar #'(lambda(x) (lp x 1 w)) (iseq 500))))
	(ll2 (reduce #'+ (mapcar #'(lambda(x) (lp x 2 w)) (iseq 500))))
	(ll3 (reduce #'+ (mapcar #'(lambda(x) (lp x 3 w)) (iseq 500)))))
    (+ ll0 ll1 ll2 ll3)))

(format t "On to maximization..~%")

;; Maximize once.
(def z (newtonmax #'bigll '(1 2 3)))
  
(def z (/ z))

(format t "The estimates of the constants are 1.0, ~g~% ~g, ~g~%" 
	(select z 0) (select z 1) (select z 2))

