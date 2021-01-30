;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "g-fuzzy")

(require "g-var")

;;
;;     Computational methods for gifi-fuzzy-proto
;;

(defmeth gifi-fuzzy-proto :total-fit ()
  (let* ((d (/ (send self :diagonal)
               (send (slot-value 'center) :n)))
         (qnt (send self :quant-scores))
         (d-qnt (matmult d qnt)))
    (mapcar #'sum
      (* (column-list qnt) (column-list d-qnt)))))

;;
;;     Coding methods for gifi-fuzzy-proto
;;

(defmeth gifi-fuzzy-proto :encode (&optional knots (order 2))
  (setf (slot-value 'knots) 
    (cond 
      ((null knots) (send self :default-knots 5))
      ((listp knots) knots)
      ((and (integerp knots) (> knots 2))
       (send self :default-knots knots))
      (t (send self :default-knots 5))))
  (setf (slot-value 'order) 1)
  (send self :crisp (send self :knots))
  (dotimes (i (1- order)) (send self :up-bspline-order nil))
  (send self :encode-arbitrary)
  (send self :slot-value 'var-level
    (format nil "B-spline--order ~d" (send self :order)))
  (send self :quant-labels
    (mapcar
      #'(lambda (i) (format nil "~,2g to ~,2g"
                      (elt-alt (send self :knots) (- i (send self :order)))
                      (elt-alt (send self :knots) i)))
      (iseq 1 (+ (length (send self :knots))
                 (send self :order)
                 -2)))))


(defmeth gifi-fuzzy-proto :encode-arbitrary
  (&optional g-matrix)
  (if g-matrix
    (setf (slot-value 'coded-data) g-matrix))
  (send self :code-diagonal)
  (send self :code-d-inv-g-prime)
  (send self :code-active)
  (send self :update-quant-scores)
  (setf (slot-value 'graph-raw) nil)
  (send self :slot-value 'var-level "Fuzzy"))

(defmeth gifi-fuzzy-proto :code-diagonal ()
  (let ((g-matrix (send self :coded-data))
        (m (diagonal (send self :m))))
    (setf (slot-value 'diagonal)
      (matmult (transpose g-matrix)
        (matmult m g-matrix)))))

(defmeth gifi-fuzzy-proto :code-d-inv-g-prime ()
  (let* ((w (diagonal (sqrt (send self :m))))
         (g-matrix (send self :coded-data))
         (qr (qr-decomp (matmult w g-matrix)))
         (q-prime-w (matmult (transpose (car qr)) w))
         (r (cadr qr))
         (ind (which (/= (diagonal r) 0)))
         (r-inv (* 0 r)))
    (setf (select r-inv ind ind) (inverse (select r ind ind)))
    (setf (slot-value 'd-inv-g-prime)
      (matmult r-inv q-prime-w))))

(defmeth gifi-fuzzy-proto :up-bspline-order (&optional (alter t))
  (let* ((g (send self :coded-data))
         (data (coerce (send self :raw-data) 'list))
         (x (if-else (mapcar #'numberp data) data 0))
         (kk (send self :knots))
         (p (1+ (send self :order))))
    (send self :coded-data (update g x kk p))
    (setf (slot-value 'order) p))
  (if alter
    (progn
      (send self :encode-arbitrary)
      (send self :slot-value 'var-level
        (format nil "B-spline--order ~d" (send self :order)))
      (send self :quant-labels
        (mapcar
          #'(lambda (i) (format nil "~,2g to ~,2g" 
                          (elt-alt (send self :knots) 
                            (- i (send self :order)))
                          (elt-alt (send self :knots) i)))
          (iseq 1 (+ (length (send self :knots)) 
                     (send self :order) 
                     -2)))))))


; update and elt-alt are provided by Jan de Leeuw.

(defun update (g x kk p) ; kk includes exterior knots. (g x k p)
"Args: basis points knots order
Takes a B-spline BASIS of order p-1 defined at
POINTS for given KNOTS, and upgrades it to order p."
(let* (
      (n (length x))
      (m (- (length kk) 2)) ;(m (length k))
;      (ma (max x))
;      (mb (min x))
      (au (repeat 0 n))
      (gg (column-list g))
;      (kk (concatenate 'list (list mb) k (list ma)))
      (h (make-array (list n (+ p m)) :initial-element 0))
      )
(dotimes (j (+ p m))
(let* (
     (la (elt-alt kk (1+ (- j p))))
     (lb (elt-alt kk j))
     (lc (elt-alt kk (1+ j)))
     (ld (elt-alt kk (+ 2 (- j p))))
     (ca (if (= 0 j) au (/ (- x la) (- lb la))))
     (cb (if (= (1- (+ p m)) j) au (/ (- lc x) (- lc ld))))
     (ga (if (= 0 j) au (elt gg (1- j))))
     (gb (if (= (1- (+ p m)) j) au (elt gg j)))
     (aa (+ (* ca ga) (* cb gb)))
     )
(setf (select h (iseq n) (list j))
      (make-array (list n 1) :displaced-to (coerce aa 'vector)))
))
h))

(defun elt-alt (x i)
"Args: sequence index
Modification of elt function. If INDEX is larger than length(SEQUENCE),
the function returns the last element of SEQUENCE, if INDEX is negative it retur
ns the first element."
(let (
     (n (length x))
     )
(cond
  ((< i 0) (elt x 0))
  ((>= i n) (elt x (1- n)))
  (t (elt x i)))))



;;
;;     Accessor methods for gifi-fuzzy-proto
;;

(defmeth gifi-fuzzy-proto :knots ()
  (slot-value 'knots))

(defmeth gifi-fuzzy-proto :order ()
  (slot-value 'order))

;;
;;    Report methods for gifi-fuzzy-proto
;;

(defmeth gifi-fuzzy-proto :report-coding (&optional report)
  (let* ((stream (if report (send (slot-value 'center)
                              :slot-value 'stream)))
         (qnt-lbl (slot-value 'quant-labels))
         (diag (slot-value 'diagonal))
         (num-mis (sum (if-else (slot-value 'active-values) 0 1))))
    (dormat stream "~%~a is coded as ~a level.~%~%"
      (slot-value 'var-label) (slot-value 'var-level))
    (dormat stream
      "    Category descriptor   Diagonal matrix~%")
    (dormat stream
      "------------------------------------------------------------~%")
    (dotimes (i (length qnt-lbl))
      (dormat stream "     ~12a" (elt qnt-lbl i))
      (dotimes (j (length qnt-lbl))
        (dormat stream " ~7,2f" (aref diag i j)))
      (dormat stream "~%"))
    (dormat stream
      "------------------------------------------------------------~%")
    (dormat stream
      "Number of missing (passive) values:  ~g~%" num-mis)))

;;
;;     Some graphing methods for gifi-fuzzy-proto
;;

(defmeth gifi-fuzzy-proto :init-graph-data (&optional (points 20))
  (let* ((knots (slot-value 'knots))
         (order (slot-value 'order))
         (c (1- (length knots)))
         (raw (remove-duplicates 
               (combine
                (mapcar #'(lambda (a b) (rseq a b (1+ points)))
                  (select knots (iseq c))
                  (cdr knots)))))
         (freq (+ points (repeat '(0 1) (list (1- c) 1))))
         (ind (repeat (iseq c) freq))
         (mat (select (identity-matrix c) ind (iseq c))))
    (setf (slot-value 'graph-raw) raw)
    (setf (slot-value 'graph-data)
      (if (= order 1) mat
        (dolist (p (iseq 2 order) mat)
          (setf mat (update mat raw knots p)))))))

(defmeth gifi-fuzzy-proto :quant-curve-data ()
  (unless (slot-value 'graph-raw) (send self :init-graph-data))
  (matmult (slot-value 'graph-data) (send self :quant-scores)))




