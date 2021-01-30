;;;
;;; This program calculates estimates of the c's
;;; for the pump data. 
;;;

(format t "Remember, this could take a while...~%")
(def d1 (read-data-columns "pump1.out" 12))
(def d2 (read-data-columns "pump2.out" 12))
(def d3 (read-data-columns "pump3.out" 12))

(def data (list (list (select d1 10) (select d1 11))
                (list (select d2 10) (select d2 11))
                (list (select d3 10) (select d3 11))))

(def m (length data)) ; the no of chains.

(def hypers (list (list (list 50.0) (list 0.5 0.5))
		  (list (list 90.0) (list 0.25 1.5))
		  (list (list 0.5) (list 0.8 0.08))))

(defun priordens (i j) ; The prior measure at i-th point in j-th chain. 
  (let* ((d (get-data i j))
	 (alpha (select d 0))
	 (beta (select d 1))
	 (h1 (get-hypers j 0))
	 (h2 (get-hypers j 1)))
    (* (/ (select h1 0)) (^ (select h2 1) (select h2 0))
       (exp (- 0.0 
	       (log-gamma (select h2 0))
	       (/ alpha (select h1 0))
	       (* beta (select h2 1))))
       (^ beta (- (select h2 0) 1.0)))))

(defun get-data (i j)  ; the i-th DATA POINT from j-th chain.
  (list (select (select (select data j) 0) i) 
	(select (select (select data j) 1) i)))

(defun get-hypers (j k) ; hyper for the k-th param for j-th chain.
  (select (select hypers j) k))

(defun p (i j c) ; Prob that i-th point came from j-th chain given c.
  (let ((s (* 0.333333333 (sum (* (priordens i 0) (select c 0))
		 	(* (priordens i 1) (select c 1))
			(* (priordens i 2) (select c 2))))))
    (/ (* (priordens i j) (select c j)) s)))

(defun lp (i j c) ; Log p(i j c).
  (let ((s (p i j c)))
    (if (> s 0.0)
	(log s)
      -1e10)))

(defun bigll (c)  ; Log quasi likelihood.
  (setf w (cons 1.0 c))  ; We are assuming one constant is 1.0.
  (let ((ll0 (reduce #'+ (mapcar #'(lambda(x) (lp x 0 w)) (iseq 500))))
	(ll1 (reduce #'+ (mapcar #'(lambda(x) (lp x 1 w)) (iseq 500))))
	(ll2 (reduce #'+ (mapcar #'(lambda(x) (lp x 2 w)) (iseq 500)))))
    (+ ll0 ll1 ll2)))

  
(format t "On to maximization...~%")

;; Maximize once.
(def z (newtonmax #'bigll '(3.0 .1)))
  
(def z (/ z))

(format t "The estimates of the constants are 1.0, ~g~% ~g\n" 
        (select z 0) (select z 1))

    
