;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "g-cat")

(require "g-var")

;;
;;     Computational methods for gifi-cat-proto
;;

(defmeth gifi-cat-proto :mgy ()
  (let ((m (diagonal (send self :m)))
        (row (iseq (send (slot-value 'center) :p))))
    (matmult m
      (select (slot-value 'quant-scores)
        (slot-value 'coded-data) row))))

(defmeth gifi-cat-proto :gy ()
  (let ((row (iseq (send (slot-value 'center) :p))))
    (select (slot-value 'centroid) (slot-value 'coded-data) row)))

(defmeth gifi-cat-proto :centroids ()
  (let* ((obj-rows (row-list (send self :obj-scores)))
         (m (send self :m))
         (wt-rows (* m obj-rows)))
    (flet ((centroid (inds)
             (let ((num (apply #'+ (select wt-rows inds)))
                   (denom (sum (select m inds))))
               (if (= 0 denom) (* 0 num) (/ num denom)))))
      (apply #'bind-rows
        (mapcar #'centroid (slot-value 'd-inv-g-prime))))))

;;
;;     Coding functions and methods for gifi-cat-proto
;;

(defmeth gifi-cat-proto :encode 
  (&optional raw-values restricted-levels)
  (send self :coding-message)
  (if raw-values 
    (setf (slot-value 'raw-values) raw-values)
    (send self :code-raw-values))
  (send self :code-restricted-levels restricted-levels)
  (send self :code-data)
  (send self :code-active)
  (send self :code-diagonal)
  (send self :code-d-inv-g-prime)
  (send self :update-quant-scores)
  (send self :code-quant-labels))

(defmeth gifi-cat-proto :coding-message ()
  (let ((level "multiple nominal")
        (label (slot-value 'var-label)))
    (setf (slot-value 'var-level) level)
    (format t "Coding ~a as ~a...~%" label level)))

(defmeth gifi-cat-proto :code-raw-values ()
  (let* ((bar1 (remove-duplicates (send self :raw-data)))
         (numbers (select bar1 
                      (which (mapcar #'numberp bar1))))
         (bar2 (if (null numbers) bar1
                 (remove-duplicates
                   (combine (sort-data numbers) bar1)))))
    (setf (slot-value 'raw-values) (remove nil bar2))))

(defmeth gifi-cat-proto :code-restricted-levels 
  (&optional restricted-levels)
  ())   ; This is a dummy method for future protos

(defmeth gifi-cat-proto :code-data ()
  (let ((bar (slot-value 'raw-values)))
    (setf (slot-value 'coded-data)
      (mapcar #'(lambda (x) (where-in x bar))
        (send self :raw-data)))))

(defun where-in (x bar)
  (do ((bart bar (cdr bart))
       (index 0 (1+ index)))
      ((if (null bart) t
         (if (equal x (car bart)) t
           (if (listp (car bart)) (member x (car bart)))))
       index)))

(defmeth gifi-cat-proto :code-active ()
  (let ((k (length (slot-value 'raw-values))))
    (setf (slot-value 'active-values)
      (< (slot-value 'coded-data) k))))

(defmeth gifi-cat-proto :code-diagonal ()
  (let* ((coded (slot-value 'coded-data))
         (m (send self :m))
         (range (iseq 0 (max coded))))
    (flet ((count (i) 
             (sum (* m (if-else (= i coded) 1 0)))))
      (setf (slot-value 'diagonal) (mapcar #'count range)))))

(defmeth gifi-cat-proto :code-d-inv-g-prime ()
  (let* ((coded (slot-value 'coded-data))
         (range (iseq 0 (max coded))))
    (flet ((group (i) (which (= i coded))))
      (setf (slot-value 'd-inv-g-prime)
        (mapcar #'group range)))))

(defun string-it (x)
  (cond
    ((null x) "MISSING")
    ((listp x) (if (= 1 (length x)) (string-it (car x))
                 (strcat (string-it (car x)) "-" (string-it (cdr x)))))
    ((stringp x) x)
    ((numberp x) (format nil "~g" x))
    ((symbolp x) (string-downcase (symbol-name x) :start 1))
    (t "cOnFuSeD")))

(defmeth gifi-cat-proto :code-quant-labels ()
  (let ((labels (mapcar #'string-it (slot-value 'raw-values)))
        (m (length (slot-value 'diagonal))))
    (setf (slot-value 'quant-labels)
      (if (< (length labels) m) 
        (combine labels (repeat "PASSIVE" (- m (length labels))))
        labels))))

;;
;;    Accessor methods for gifi-cat-proto
;;

(defmeth gifi-cat-proto :raw-data ()
  (coerce (call-next-method) 'list))

;;
;;    Report methods for gifi-cat-proto
;;

;(defmeth gifi-cat-proto :report-coding (&optional report)
;  (let* ((stream (if report (send (slot-value 'center)
;                              :slot-value 'stream)))
;         (qnt-lbl (slot-value 'quant-labels))
;         (diag (slot-value 'diagonal))
;         (num-mis (sum (if-else (slot-value 'active-values) 0 1))))
;    (dormat stream "~%~a is coded at the ~a level.~%~%"
;      (slot-value 'var-label) (slot-value 'var-level))
;    (dormat stream
;      "    Category descriptor   Weighted frequency~%")
;    (dormat stream
;      "------------------------------------------------------------~%")
;    (dotimes (i (length qnt-lbl))
;      (dormat stream "       ~24a ~10g ~%"
;        (elt qnt-lbl i) (elt diag i)))
;    (dormat stream
;      "------------------------------------------------------------~%")
;    (dormat stream
;      "Number of missing (passive) values:  ~g~%" num-mis)))




