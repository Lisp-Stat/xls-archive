;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "g-var")

(require "gifi-gardens")
(require "g-center")

;;  
;;    Computational methods for gifi-var-proto
;;

(defmeth gifi-var-proto :m ()
  (let ((act (slot-value 'active-values))
        (w (send (slot-value 'center) :obj-weights))
        (n (send (slot-value 'center) :slot-value 'n)))
    (* (if act (if-else act 1 0) 1)
       (if w w (repeat 1 n)))))

(defmeth gifi-var-proto :mgy ()
  (pre-diag-mult (send self :m)
    (matmult (slot-value 'coded-data) (slot-value 'quant-scores))))

(defmeth gifi-var-proto :gy ()
  (matmult (slot-value 'coded-data) (slot-value 'quant-scores)))

(defmeth gifi-var-proto :gcentroid ()
  (send self :gy))

(defun pre-diag-mult (d m)
  (apply #'bind-rows 
    (* d (row-list m))))

(defmeth gifi-var-proto :update-quant-scores ()
  (setf (slot-value 'quant-scores)
    (send self :centroids)))

(defmeth gifi-var-proto :centroids ()
  (matmult (slot-value 'd-inv-g-prime) 
    (send self :obj-scores)))

;This should be deleted after testing.
(defmeth gifi-var-proto :total-loss2 ()
  (let* ((obj (send self :obj-scores))
         (qnt (matmult (slot-value 'coded-data)
                       (slot-value 'quant-scores)))
         (diff (^ (- obj qnt) 2))
         (m (/ (send self :m) 
               (send (slot-value 'center) :slot-value 'n))))
    (mapcar #'(lambda (col) (sum (* col m)))
      (column-list diff))))

(defmeth gifi-var-proto :total-loss ()
  (- (send self :total-variance)
     (send self :total-fit)))

(defmeth gifi-var-proto :total-variance ()
  (let ((m (/ (send self :m)
              (send (slot-value 'center) :slot-value 'n)))
        (obj2 (^ (send self :obj-scores) 2)))
    (mapcar #'(lambda (col) (sum (* m col)))
      (column-list obj2))))

(defmeth gifi-var-proto :total-fit ()
  (let ((d (/ (slot-value 'diagonal)
              (send (slot-value 'center) :slot-value 'n)))
        (qnt2 (^ (slot-value 'quant-scores) 2)))
    (mapcar #'(lambda (col) (sum (* d col)))
      (column-list qnt2))))

(defmeth gifi-var-proto :multiple-fit ()
  (send self :total-fit))

(defmeth gifi-var-proto :single-fit ()
  (repeat 0.0 (send (slot-value 'center) :p)))

(defmeth gifi-var-proto :permute-quants (perm-ind) ())


;;
;;    Coding methods for gifi-var-proto
;;

(defmeth gifi-var-proto :default-knots (n)
  (let ((data (remove nil (coerce (send self :raw-data) 'list))))
    (rseq (min data) (max data) n)))

(defmeth gifi-var-proto :encode (&optional knots &rest args)
  (send self :crisp knots)
  (send self :code-diagonal)
  (send self :code-d-inv-g-prime)
  (send self :code-active)
  (send self :update-quant-scores)
  (send self :slot-value 'var-level "multiple nominal--crisp"))

(defmeth gifi-var-proto :crisp (&optional knots)
  (let* ((data (coerce (send self :raw-data) 'list))
         (knots (cond 
                  ((null knots) (send self :default-knots 5))
                  ((listp knots) knots)
                  ((and (integerp knots) (> knots 1))
                   (send self :default-knots knots))
                  (t (send self :default-knots 5))))
         (k (1- (length knots)))
         (eps (combine (repeat 0 k) (* (max knots) machine-epsilon)))
         (knots (+ knots eps)))
    (flet ((code (x)
             (if (null x) (repeat 0 k)
               (difference (if-else (< x knots) 1 0))))
           (label (i) 
             (format nil "~,2g to ~,2g" 
               (elt knots i) (elt knots (1+ i)))))
      (setf (slot-value 'coded-data)
        (apply #'bind-rows
          (mapcar #'code data)))
      (setf (slot-value 'quant-labels)
        (mapcar #'label (iseq k))))))

(defmeth gifi-var-proto :code-diagonal ()
  (let ((g (slot-value 'coded-data))
        (m (diagonal (send self :m))))
    (setf (slot-value 'diagonal)
      (diagonal
        (matmult (transpose g) (matmult m g))))))

(defmeth gifi-var-proto :code-d-inv-g-prime ()
  (let ((d-inv (mapcar
                 #'(lambda (x) (if (> x 0) (/ x) 0))
                 (slot-value 'diagonal)))
        (g-prime (transpose (slot-value 'coded-data)))
        (m (diagonal (send self :m))))
    (setf (slot-value 'd-inv-g-prime)
      (matmult (diagonal d-inv) 
        (matmult g-prime m)))))         

(defmeth gifi-var-proto :code-active ()
  (let* ((data (mapcar #'sum (row-list (slot-value 'coded-data))))
         (m (send self :m))
         (vals (mapcar #'not (= 0 (* data m)))))
    (setf (slot-value 'active-values) vals)))

;;
;;    Analysis altering methods for gifi-var-proto
;;

(defmeth gifi-var-proto :recast (&rest args)
  (apply #'send (slot-value 'center) :cast self args))

(defmeth gifi-var-proto :active (&optional (active t set))
  (let* ((center (slot-value 'center))
         (in-analysis (send center :in-analysis))
         (act (send center :active))
         (ind (car (which 
               (mapcar #'(lambda (x) (equal x self))
                 in-analysis)))))
    (if set (list
               (setf (elt act ind) active)
               (send center :active act)
               (send center :var-summary)))
    (elt (send center :active) ind)))

;;
;;    Accessor methods for gifi-var-proto
;;

(defmeth gifi-var-proto :var-label (&optional (new-label nil set))
  (if set (setf (slot-value 'var-label) new-label)
    (slot-value 'var-label)))

(defmeth gifi-var-proto :quant-labels (&optional (new-labels nil set))
  (if set (setf (slot-value 'quant-labels) new-labels)
    (slot-value 'quant-labels)))

(defmeth gifi-var-proto :var-level ()
  (slot-value 'var-level))

(defmeth gifi-var-proto :coded-data (&optional (new-data nil set))
  (if set (setf (slot-value 'coded-data) new-data)
    (slot-value 'coded-data)))

(defmeth gifi-var-proto :diagonal ()
  (slot-value 'diagonal))

(defmeth gifi-var-proto :active-values
  (&optional (active-values nil set))
  (if set 
    (setf (slot-value 'active-values) 
      (cond
        ((null active-values) active-values)
        ((= (length active-values) (send (slot-value 'center) :n))
         active-values)
        (t (format t "Error: length of active-values must be 0 or n.")))))
  (slot-value 'active-values))

(defmeth gifi-var-proto :quant-scores ()
  (slot-value 'quant-scores))

(defmeth gifi-var-proto :obj-scores ()
  (send (slot-value 'center) :slot-value 'obj-scores))

(defmeth gifi-var-proto :raw-data ()
  (send (slot-value 'center) :raw-data (slot-value 'column-ind)))

(defmeth gifi-var-proto :loadings () ())

;;
;;    Report methods for gifi-var-proto
;;

(defmeth gifi-var-proto :report-coding (&optional report)
  (let* ((stream (if report (send (slot-value 'center)
                              :slot-value 'stream)))
         (qnt-lbl (slot-value 'quant-labels))
         (diag (slot-value 'diagonal))
         (num-mis (sum (if-else (slot-value 'active-values) 0 1))))
    (dormat stream "~%~a is coded at the ~a level.~%~%"
      (slot-value 'var-label) (slot-value 'var-level))
    (dormat stream
      "    Category descriptor   Weighted frequency~%")
    (dormat stream
      "------------------------------------------------------------~%")
    (dotimes (i (length qnt-lbl))
      (dormat stream "       ~24a ~10g ~%"
        (elt qnt-lbl i) (elt diag i)))
    (dormat stream
      "------------------------------------------------------------~%")
    (dormat stream
      "Number of missing (passive) values:  ~g~%" num-mis)))

(defmeth gifi-var-proto :sub-report-loadings (&optional stream)
  ())

(defmeth gifi-var-proto :sub-report-multiple-fit (&optional stream)
  (let ((fit (send self :multiple-fit)))
    (dormat stream "    ~16a" (slot-value 'var-label))
    (dormat stream "~9,5f " (sum fit))
    (dolist (f fit)
      (dormat stream " ~9,5f" f))
    (dormat stream "~%")))

(defmeth gifi-var-proto :sub-report-single-fit (&optional stream)
  (let ((fit (send self :single-fit)))
    (dormat stream "    ~16a" (slot-value 'var-label))
    (dormat stream "~9,5f " (sum fit))
    (dolist (f fit)
      (dormat stream " ~9,5f" f))
    (dormat stream "~%")))




