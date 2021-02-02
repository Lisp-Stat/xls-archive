;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dykstra (JASA, 78, 1983, 837-842) discussed this algorithm to
;; project on the intersection of r closed convex sets. Further
;; details and references are in Mathar, Cyclic Projections in
;; Data Analysis, Operations Research Proceedings 1988, Spinger,
;; 1989.
;;
;; Version 1.0 -- 07-14-95 -- Jan de Leeuw. As an application we
;;                            give a monotone regression algorithm.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dykstra-projection (x0 projections &key (eps 1e-6))
  (let* ((r (length projections))
         (p (length x0))
         (m (repeat (list (repeat 0 p)) r))
         (x (repeat (list x0) (1+ r))))
    (loop
      (setf (elt x 0) (elt x r))
      (dotimes (i r)
        (let ((k (1+ i))
              (f (elt projections i)))
          (setf
           (elt x k) (funcall f (- (elt x i) (elt m i)))
           (elt m i) (- (elt x k) (- (elt x i) (elt m i))))))
      (if (> eps (max (abs (- (elt x 0) (elt x r)))))
          (return (elt x r))))))

(defun make-order (i)
  (lambda (x)
    (let ((j (1+ i)))
      (if (>= (elt x i) (elt x j))
          (let ((m (/ (+ (elt x i) (elt x j)) 2)))
            (setf (elt x i) m (elt x j) m))))
    x))

(defun monotone-regression (x &key (eps 1e-6))
  (let* ((n (length x))
         (f (mapcar #'make-order (iseq (1- n)))))
    (dykstra-projection x f :eps eps)))

