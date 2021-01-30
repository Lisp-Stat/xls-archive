(defun dist (x)
(abs (outer-product x x #'-)))

(defun d-eq (n)
(- (make-array (list n n) :initial-element 1) (identity-matrix n))
)

(defun stress (x)
(sum (^ 2 (- *dist* (dist x)))))