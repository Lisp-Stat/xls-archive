;;;test-examples for the torus algorithm
(in-package "USER")


(defun rosenbrock2-1 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(1001 1001)))

(defun rosenbrock2-2 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(1001 -999)))

(defun rosenbrock2-3 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(-999 -999)))

(defun rosenbrock2-4 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(-999 1001)))

(defun rosenbrock2-5 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(1443 1)))

(defun rosenbrock2-6 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(1 1443)))

(defun rosenbrock2-7 ()
  (control-internal '(double double) '(-2000 -2000)
		    '(2000 2000) '(.0000001 .0000001)
		    'rosenbrock2a 'min '(1.2 1)))


(defun rosenbrock2a (x1 x2)
  (let* ((y1 (- x2 (* x1 x1)))
	 (y2 (1- x1)))
    (+ (* 100 y1 y1) (* y2 y2))))







(defun rosenbrock4-1 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(101 101 101 101)))

(defun rosenbrock4-2 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(101 101 101 -99)))

(defun rosenbrock4-3 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(101 101 -99 -99)))

(defun rosenbrock4-4 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(101 -99 -99 -99)))

(defun rosenbrock4-5 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(-99 -99 -99 -99)))

(defun rosenbrock4-6 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(-99 101 -99 101)))

(defun rosenbrock4-7 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(101 -99 101 -99)))

(defun rosenbrock4-8 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(201 0 0 0)))

(defun rosenbrock4-9 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(1 201 1 1)))

(defun rosenbrock4-10 ()
  (control-internal '(double double double double)
		    '(-200 -200 -200 -200) '(200 200 200 200)
		    '(.0000001 .0000001 .0000001 .0000001)
		    'rosenbrock4a 'min '(1 1 1 201)))

(defun rosenbrock4-11 ()		; to test for final convergence
  (control-internal  '(double double double double)
		     '(-2 -2 -2 -2) '(2 2 2 2)
		     '(.0000001 .0000001 .0000001 .0000001)
		     'rosenbrock4a
		     'min '(1 1.2 1.4 1.6)))


(defun rosenbrock4a (x1 x2 x3 x4)
  (let* ((y1 (- x2 (* x1 x1)))
	 (y2 (- x3 (* x2 x2)))
	 (y3 (- x4 (* x3 x3)))
	 (y4 (1- x1))
	 (y5 (1- x2))
	 (y6 (1- x3)))
    (+ (* 100 y1 y1) (* 100 y2 y2) (* 100 y3 y3)
       (* y4 y4) (* y5 y5) (* y6 y6))))






(defun corana2-1 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(1000 888)))

(defun corana2-2 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(-999 1001)))

(defun corana2-3 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(-999 -889)))

(defun corana2-4 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(1001 -998)))

(defun corana2-5 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(1441 3)))

(defun corana2-6 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(-10 -1410)))

(defun corana2-7 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(-1100 850)))

(defun corana2-8 ()
  (control-internal '(double double) '(-10000 -10000)
		    '(10000 10000) '(.0001 .0001)
		    'corana2a 'min '(850 -1100)))



(defun corana2a (x1 x2)
  (let* ((x (list x1 x2))
	 (d '(1 1000))
	 (s .2)
	 (tt .05)
	 (c .15)
	 (k (mapcar #'(lambda (y)
			(mapcar #'abs
				(multiple-value-list (round y s))))
		    x))
	 (k0 (apply #'* (mapcar #'car k)))
	 (k1 (apply #'* (mapcar #'(lambda (yk)
				    (cond ((< (cadr yk) tt) 1)
					  (t 0))) k)))
	 (z (mapcar #'(lambda (yk yx yd)
			(let ((z1 (- (* (car yk) s) tt)))
			  (cond ((or (zerop k0) (zerop k1))
				 (* yd yx yx))
				(t (* c yd z1 z1))))) k x d)))
    (apply #'+ z)))



(defun corana4-1 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(-999 -999 -9999 -1000)))

(defun corana4-2 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(999 1000 1001 -998)))

(defun corana4-3 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(1000 -1000 10000 -10000)))

(defun corana4-4 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(-999 -999 -998 -1000)))

(defun corana4-5 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(1000 999 999 998)))

(defun corana4-6 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(1000 -1000 -9999 9999)))

(defun corana4-7 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(1000 -1000 998 1000)))

(defun corana4-8 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(0 0 1 2001)))

(defun corana4-9 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(1998 3 10 -13)))

(defun corana4-10 ()
  (control-internal '(double double double double)
		    '(-10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000) '(.0001 .0001 .0001 .0001)
		    'corana4a 'min '(1234 -1234 560 -334)))
				   
			      
(defun corana4a (x1 x2 x3 x4)
  (let* ((x (list x1 x2 x3 x4))
	 (d '(1 1000 10 100))
	 (s .2)
	 (tt .05)
	 (c .15)
	 (k (mapcar #'(lambda (y)
			(mapcar #'abs
				(multiple-value-list (round y s))))
		    x))
	 (k0 (apply #'* (mapcar #'car k)))
	 (k1 (apply #'* (mapcar #'(lambda (yk)
				    (cond ((< (cadr yk) tt) 1)
					  (t 0))) k)))
	 (z (mapcar #'(lambda (yk yx yd)
			(let ((z1 (- (* (car yk) s) tt)))
			  (cond ((or (zerop k0) (zerop k1))
				 (* yd yx yx))
				(t (* c yd z1 z1))))) k x d)))
    (apply #'+ z)))









(defun corana10-1 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(1000 1000 1000 1000 1000 1000 1000 1000 1000 1000)))

(defun corana10-2 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(-1000 1000 -1000 1000 -1000 1000 -1000 1000 -1000 1000)))

(defun corana10-3 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(-999 -999 -999 -999 -999 -999 -999 -999 -999 -999)))

(defun corana10-4 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(999 999 999 999 999 -999 -999 -999 -999 -999)))

(defun corana10-5 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(-999 1000 -999 1000 -999 1000 -999 1000 -999 1000)))

(defun corana10-6 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(3000 4 20 40 120 -3 -6 0 0 100)))

(defun corana10-7 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(1000 -999 1000 -999 1000 -999 1000 -999 1000 -999)))


(defun corana10-8 ()
  (control-internal '(double double double double double
			     double double double double double)
		    '(-10000 -10000 -10000 -10000 -10000
			     -10000 -10000 -10000 -10000 -10000)
		    '(10000 10000 10000 10000 10000
			    10000 10000 10000 10000 10000)
		    '(.0001 .0001 .0001 .0001 .0001
			    .0001 .0001 .0001 .0001 .0001)
		    'corana10a 'min
		    '(1000 -999 1000 -999 1000 -999 1000 -999 1000 -999)))


(defun corana10a (x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
  (let* ((x (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10))
	 (d '(1 1000 10 100 1 10 100 1000 1 10))
	 (s .1)
	 (tt .04)
	 (c .15)
	 (k (mapcar #'(lambda (y)
			(mapcar #'abs
				(multiple-value-list (round y s))))
		    x))
	 (k0 (apply #'* (mapcar #'car k)))
	 (k1 (apply #'* (mapcar #'(lambda (yk)
				    (cond ((< (cadr yk) tt) 1)
					  (t 0))) k)))
	 (z (mapcar #'(lambda (yk yx yd)
			(let ((z1 (- (* (car yk) s) tt)))
			  (cond ((or (zerop k0) (zerop k1))
				 (* yd yx yx))
				(t (* c yd z1 z1))))) k x d)))
    (apply #'+ z)))
