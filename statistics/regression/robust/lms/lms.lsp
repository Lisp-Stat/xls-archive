;; Here is a xlisp implementation of the lms routine in (Stromberg 1993).
;; It is set up to work thru the example in table 3.1 of that paper.  It
;; calculates a slightly different lms slope than Stromberg gives, but
;; chooses the same points in acheiving the best Chebyshev fit.
;; 
;; This algorithm requires two auxillary files, which follow:
;; 
;;     1.  combinations.lsp - an combination generator object.
;;     2.  stromberg.data - data for a test case.

(load "combinations.lsp")

(defun lms (x y xtx)
  (let (m
	beta
	residues
	sum-of-sq-residues
	sum-of-abs-residues
	signs-of-residues
	epsilon
	beta-c
	lms-residues)
    (setf m (matmult (inverse xtx) (transpose x)))
    (setf beta (matmult m y))
    (setf residues (- y (matmult x beta)))
    (setf sum-of-sq-residues
	  (aref (matmult (transpose residues) residues) 0 0))
    (setf sum-of-abs-residues
	  (let ((sum 0))
	    (dotimes (i (array-dimension residues 0) sum)
		     (setf sum (+ sum (abs (aref residues i 0)))))))
    (setf signs-of-residues
	  (let ((result (make-array (array-dimensions residues))))
	    (dotimes (i (array-dimension residues 0) result)
		     (setf (aref result i 0)
			   (cond ((> (aref residues i 0) 0) 1)
				 ((< (aref residues i 0) 0) -1)
				 (t 0))))))
    (cond ((< sum-of-abs-residues 1e-50)
	   (setf beta-c beta))
	  (t
	   (setf epsilon (/ sum-of-sq-residues sum-of-abs-residues))
	   (setf beta-c (matmult m (- y (* epsilon signs-of-residues))))))))

(defun select-sample (index-list x)
  (let ((n-samples (length index-list))
	(x-samp (make-array (list (length index-list) (array-dimension x 1)))))
    (dotimes (i n-samples x-samp)
	     (dotimes (j (array-dimension x 1) nil)
		      (setf (aref x-samp i j)
			    (aref x (select index-list i) j))))))

(defun lms-sample (x y)
  (let (iter n-samples sample-list x-samp y-samp best-beta best-lms
	     trial trial-lms lms-residues h i xtx seq comb)
    (setf best-lms #.POSITIVE-INFINITY)
    (setf n-samples (+ (array-dimension x 1) 1))
    (setf h
	  (+ -1
             (ceiling (/ (array-dimension x 0) 2))
	     (ceiling (/ (+ (array-dimension x 1) 1) 2))))
    (setf seq (iseq (array-dimension x 0)))
    (setf comb (send comb-generator :new
		     (array-dimension x 0) (+ (array-dimension x 1) 1)))
    (do ((i 0 (+ i 1)))
        (nil nil)
        (setf sample-list (send comb :next-comb))
        (cond ((eq sample-list nil) (return (list best-lms best-beta)))
              ( t (setf sample-list (- sample-list 1))))
	(setf x-samp (select-sample sample-list x))
	(setf y-samp (select-sample sample-list y))
	(setf xtx (matmult (transpose x-samp) x-samp))
	(cond ((< (determinant xtx) 1e-6)
	       (format *standard-output* "Singular point ~a~%" sample-list))
	      (t
	       (setf trial (lms x-samp y-samp xtx))
	       (setf lms-residues (^ (- (matmult x trial) y) 2))
	       (setf trial-lms (select (sort-data lms-residues) h))
	       (cond ((< trial-lms best-lms)
		      (setf best-lms trial-lms)
		      (setf best-beta trial)
		      (format *STANDARD-OUTPUT*
			      "best-lms=~a best-beta=~a Sample=~a~%"
			      best-lms best-beta sample-list))))))))

(setf x nil)
(setf y nil)

(with-open-file (f "stromberg.data")
  (let (tmp)
    (do ((line (read-line f nil nil) (read-line f nil nil)))
      ((not line) nil)
      (setf tmp (read-from-string (format nil "( ~a )" line)))
      (print tmp)
      (setf y (append y (list (list (first tmp)))))
      (setf x (append x (list (cdr tmp)))))))

(setf x (apply #'bind-rows x))
(setf y (apply #'bind-rows y ))

(lms-sample x y)

