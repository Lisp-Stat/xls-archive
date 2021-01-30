;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "g-single")

(require "g-cat")

;;
;;     Computational methods for gifi-single-proto
;;

(defmeth gifi-single-proto :mgy ()
  (matmult
    (* (select (slot-value 'quant-scores) (slot-value 'coded-data) 0)
      (send self :m))
    (slot-value 'loadings)))

(defmeth gifi-single-proto :gy ()
  (matmult (select (slot-value 'quant-scores) (slot-value 'coded-data) 0)
           (slot-value 'loadings)))

(defmeth gifi-single-proto :gcentroid ()
  (select (slot-value 'centroid)
          (slot-value 'coded-data)
          (iseq (send (slot-value 'center) :p))))

(defmeth gifi-single-proto :update-quant-scores ()
  (setf (slot-value 'centroids) (send self :centroids))
  (send self :normal-quants)
  (send self :normal-loadings))

(defmeth gifi-single-proto :normal-quants ()
  (setf (slot-value 'quant-scores)
    (let* ((a (transpose (slot-value 'loadings)))
           (y-tilde (matmult (slot-value 'centroids)
                      (/ a (sum (* a a)))))
           (d (slot-value 'diagonal))
           (norm (aref (matmult (transpose y-tilde)
                         (pre-diag-mult d y-tilde)) 0 0))
           (n (send (slot-value 'center) :n)))
      (* y-tilde (sqrt (/ n norm))))))

(defmeth gifi-single-proto :normal-loadings ()
  (setf (slot-value 'loadings)
    (matmult (transpose (pre-diag-mult (slot-value 'diagonal) 
                          (slot-value 'quant-scores)))
      (/ (slot-value 'centroids)
        (send (slot-value 'center) :n)))))

(defmeth gifi-single-proto :quant-scores ()
  (matmult (slot-value `quant-scores) (slot-value 'loadings)))

(defmeth gifi-single-proto :total-fit ()
  (apply #'+ (row-list (^ (slot-value 'loadings) 2))))

(defmeth gifi-single-proto :single-fit ()
  (combine (send self :total-fit)))

(defmeth gifi-single-proto :multiple-fit ()
  (let ((d (/ (slot-value 'diagonal)
              (send (slot-value 'center) :n)))
        (qnt2 (^ (slot-value 'centroids) 2)))
    (mapcar #'(lambda (col) (sum (* d col)))
      (column-list qnt2))))

(defmeth gifi-single-proto :permute-quants (perm-ind)
  (setf (slot-value 'loadings)
    (select (slot-value 'loadings) 0 perm-ind)))

;;
;;    Coding methods for gifi-single-proto 
;;

(defmeth gifi-single-proto :coding-message ()
  (let ((level "single nominal")
        (label (slot-value 'var-label)))
    (setf (slot-value 'var-level) level)
    (format t "Coding ~a as ~a...~%" label level)))

;;
;;    Accessor methods for gifi-single-proto
;;

(defmeth gifi-single-proto :loadings ()
  (combine (slot-value 'loadings)))

;;
;;    Report methods for gifi-single-proto
;;

(defmeth gifi-single-proto :report-coding (&optional report)
  (let* ((stream (if report (send (slot-value 'center)
                              :slot-value 'stream)))
         (qnt-lbl (slot-value 'quant-labels))
         (diag (slot-value 'diagonal))
         (quants (combine (slot-value 'quant-scores)))
         (num-mis (sum (if-else (slot-value 'active-values) 0 1))))
    (dormat stream "~%~a is coded at the ~a level.~%~%"
      (slot-value 'var-label) (slot-value 'var-level))
    (dormat stream
      "    Category descriptor   Weighted freq.    Quantifications~%")
    (dormat stream
      "------------------------------------------------------------~%")
    (dotimes (i (length qnt-lbl))
      (dormat stream "       ~24a ~10g       ~10,5g~%"
        (elt qnt-lbl i) (elt diag i) (elt quants i)))
    (dormat stream
      "------------------------------------------------------------~%")
    (dormat stream
      "Number of missing (passive) values:  ~g~%" num-mis)))

(defmeth gifi-single-proto :sub-report-loadings (&optional stream)
  (dormat stream "    ~16a          " (slot-value 'var-label))
  (dolist (l (combine (slot-value 'loadings)))
    (dormat stream " ~9,5g" l))
  (dormat stream "~%"))

;(defmeth gifi-single-proto :sub-report-single-fit (&optional stream)
;  (let ((fit (send self :single-fit)))
;    (dormat stream "    ~16a" (slot-value 'var-label))
;    (dormat stream " ~9,5g" (sum fit))
;    (dolist (f fit)
;      (dormat stream " ~9,5g" f))
;    (dormat stream "~%")))
        


;;
;;    Computational methods for gifi-single-ordinal-proto
;;

(defmeth gifi-single-ordinal-proto :normal-quants ()
  (let* ((rls (slot-value 'restricted-levels))
         (inds (iseq rls))
         (y-tilde (combine (call-next-method)))
         (y (select y-tilde inds))
         (w (select (slot-value 'diagonal) inds))
         (init (repeat 1 rls)))
    (setf (select y-tilde inds) (monor (list y w init)))
    (setf (slot-value 'quant-scores) (bind-columns y-tilde))))

;; monor provided by Jan Deleeuw.
(defun monor (x)
"ARGS: x
This takes a list of three sequences of equal length. It then
computes the monotone regression on the first sequence, using
weights in the second sequence. The third sequence in the list X
is a work array indicating size of the blocks in the monotone
regression. In the call this is usually just a vector of ones,
but it changes in the recursive calls to the function."
(let* (
       (values (coerce (elt x 0) 'list))
       (weights (coerce (elt x 1) 'list))
       (blocks (coerce (elt x 2) 'list))
       (n (length values))
       (last-right (if (= n 1) nil 
                       (position t (< (difference values) 0))))
       )
(cond 
    ((not last-right) (setf result ()) 
     (dotimes (i n result)
        (setf result 
              (append result (repeat (elt values i) (elt blocks i))))))
    (t (let* (
            (first-wrong (1+ last-right))
            (head (iseq last-right))
            (tail (if (= n (1+ first-wrong)) nil
                         (iseq (1+ first-wrong) (1- n))))
            (u (elt values last-right))
            (v (elt values first-wrong))
            (a (elt weights last-right))
            (b (elt weights first-wrong))
            (p (elt blocks last-right))
            (q (elt blocks first-wrong))
            (r (+ a b))
            (h (+ p q))
            (s (/ (+ (* a u) (* b v)) r))
            )
(setf values (append (select values head) (list s) (select values tail)))
(setf weights (append (select weights head) (list r) (select weights tail)))
(setf blocks (append (select blocks head) (list h) (select blocks tail)))
(monor (list values weights blocks))
)))))

;;
;;    Coding methods for gifi-single-ordinal-proto
;;

(defmeth gifi-single-ordinal-proto :code-restricted-levels
  (&optional rls)
  (let* ((levels (slot-value 'raw-values))
         (n (sum (if-else (mapcar #'numberp levels) 1 0)))
         (max (length levels)))
    (setf (slot-value 'restricted-levels)
      (if (if rls (and (> rls 2) (< rls max)) nil) rls n))))

(defmeth gifi-single-ordinal-proto :coding-message ()
  (let ((level "single ordinal")
        (label (slot-value 'var-label)))
    (setf (slot-value 'var-level) level)
    (format t "Coding ~a as ~a...~%" label level)))

;;
;;    Methods for gifi-single-numerical-proto
;;

(defmeth gifi-single-numerical-proto :normal-quants ()
  (let* ((rls (slot-value 'restricted-levels))
         (inds (iseq rls))
         (y-tilde (combine (call-method gifi-single-proto :normal-quants)))
         (y (select y-tilde inds))
         (w (select (slot-value 'diagonal) inds))
         (raw (select (slot-value 'raw-values) inds))
         (z (- raw (/ (sum (* w raw)) (sum w))))
         (b (/ (sum (* z w y)) (sum (* z w z)))))
    (setf (select y-tilde inds) (* b z))
    (setf (slot-value 'quant-scores) (bind-columns y-tilde))))

(defmeth gifi-single-numerical-proto :coding-message ()
  (let ((level "single numerical")
        (label (slot-value 'var-label)))
    (setf (slot-value 'var-level) level)
    (format t "Coding ~a as ~a...~%" label level)))




