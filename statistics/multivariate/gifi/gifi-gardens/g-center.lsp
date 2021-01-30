;;;
;;;     Gifi-Gardens, a place to grow nonlinear multivariate
;;;     analyses of the Gifi variety, by James Hilden-Minton.
;;;

(provide "g-center")

(require "gifi-gardens")

;;    
;;    Computational methods for gifi-center-proto
;;

(defmeth gifi-center-proto :update-obj-scores (&optional pivot)
  (let* ((z (apply #'+
             (mapcar #'(lambda (gv) (send gv :mgy))
                     (send self :active-gifi-vars))))
         (m (send self :m-star))
         (mh (sqrt m))
         (z-tilde (- (matmult (diagonal mh) z)
                (outer-product (/ mh (sum m))
                  (matmult m z))))
         (mhi (mapcar #'(lambda (x) (if (> x 0) (/ x) 0)) 
                mh))
         (qrd (qr-decomp z-tilde pivot)))
    (setf (slot-value 'obj-scores)
      (* (sqrt (* (slot-value 'n)
                  (sum (if-else (slot-value 'active) 1 0))))
         (matmult (diagonal mhi) (car qrd))))
    (if pivot 
      (mapcar #'(lambda (gv) (send gv :permute-quants (caddr qrd)))
        (send self :active-in-analysis)))))

(defmeth gifi-center-proto :active-gifi-vars ()
  (select (slot-value 'in-analysis) 
    (if (slot-value 'active) (which (slot-value 'active)))))

(defmeth gifi-center-proto :m-star ()
  (flet ((decode (x) (if x x (repeat 1 (slot-value 'n)))))
    (* (apply #'+ (mapcar 
         #'(lambda (gv) 
           (decode (if-else (send gv :slot-value 'active-values) 1 0)))
         (send self :active-gifi-vars)))
       (decode (slot-value 'obj-weights)))))

(defmeth gifi-center-proto :update-quant-scores (&optional all)
  (let ((gvs (if all (slot-value 'in-analysis)
               (send self :active-gifi-vars))))
    (dolist (gv gvs)
      (send gv :update-quant-scores))))

(defmeth gifi-center-proto :update-loss-history (&optional forget)
  (let* ((active (send self :active-gifi-vars))
         (p (send self :p))
         (loss (- p 
                 (/ (sum (mapcar 
                           #'(lambda (gv) (send gv :total-fit))
                           active))
                    (length active)))))
    (setf (slot-value 'loss-history)
      (if forget (list loss)
        (cons loss (slot-value 'loss-history))))))

(defmeth gifi-center-proto :iterate-once
  (&optional pivot all forget)
  (progn
    (send self :update-obj-scores pivot)
    (send self :update-quant-scores all)
    (send self :update-loss-history forget)))

(defmeth gifi-center-proto :minimize-loss
  (&key (pivot t) (all t) forget (precision 0.00001) report)
  (let ((stream (if report (slot-value 'stream))))
    (dormat stream "Minimizing loss...~%~%")
    (do* ((loss-hist (send self :iterate-once pivot nil forget)
                     (send self :iterate-once pivot nil nil))
          (count (length loss-hist) (1+ count))
          (diff 1 (- (cadr loss-hist) (car loss-hist))))
      ((< (- machine-epsilon) diff precision)
       (dormat stream "   Iteration ~3d: ~10,5f~%" count (car loss-hist))
       (dormat stream "Final iteration...~10,5f~%"
         (car (send self :iterate-once pivot all nil))))
      (dormat stream "   Iteration ~3d: ~10,5f~%" count (car loss-hist)))))

(defmeth gifi-center-proto :random-obj-scores ()
  (let ((n (slot-value 'n))
        (p (slot-value 'p)))
    (setf (slot-value 'obj-scores)
      (make-array (list n p)
        :initial-contents 
        (split-list (normal-rand (* n p)) p)))
    (send self :update-quant-scores nil)))


;;
;;    Analysis altering methods for gifi-center
;;

(defmeth gifi-center-proto :cast 
  (gv-col level &rest args &key (var-label nil set-vl) 
    (active-values nil) (active nil set-act))
  (let* ((proto (case level
                  ('crisp gifi-var-proto)
                  ((multiple-nominal mnom) gifi-cat-proto)
                  ((single-nominal snom) gifi-single-proto)
                  ((ordinal single-ordinal sord) gifi-single-ordinal-proto)
                  ((numerical nume) gifi-single-numerical-proto)
                  ((spline bspline fuzzy) gifi-fuzzy-proto)
                  (t (error "Not an available level of analysis - ~a"
                       level))))
         (gv (cond
               ((integerp gv-col) 
                  (send proto :new self gv-col))
               ((member gv-col (slot-value 'in-analysis))
                  (send gv-col :retype proto self 
                    (send gv-col :slot-value 'column-ind)))
               (t (error "Not a column index or gifi-var in the analysis - ~a"
                    gv-col))))
         (key-inds (if args
                    (remove nil 
                     (mapcar 
                       #'(lambda (x) 
                         (which (mapcar #'(lambda (y) (equal x y)) args)))
                       '(:var-label :active-values :active)))
                    nil))
         (coding-args (if key-inds (select args (iseq (min key-inds)))
                        args))
         (key-args (if key-inds 
                     (select args 
                       (iseq (min key-inds) 
                         (1- (length args))))))
         (read (apply #'(lambda (&key (var-label nil set-vl)
                                      (active-values nil)
                                      (active nil set-act))
                          (list set-vl var-label 
                            active-values set-act active))
                 key-args)))
    (if (elt read 0) (send gv :var-label (elt read 1)))
    (send gv :active-values (elt read 2))
    (apply #'send gv :encode coding-args)
    (send gv :report-coding)
    (if (elt read 3) (send gv :active (elt read 4)))
    gv))

(defmeth gifi-center-proto :p (&optional (new-p nil set))
  (let ((old-p (slot-value 'p)))
    (if set
      (prog1
        (setf (slot-value 'p) new-p)
        (cond 
          ((null old-p) (send self :random-obj-scores))
          ((< new-p old-p)
           (list
             (setf (slot-value 'obj-scores)
               (select (slot-value 'obj-scores)
                 (iseq (slot-value 'n))
                 (iseq new-p)))
             (send self :update-quant-scores)))
          ((> new-p old-p)
           (let* ((n (slot-value 'n))
                  (d (- new-p old-p))
                  (cont (split-list (normal-rand (* n d)) d)))  
             (setf (slot-value 'obj-scores)
               (bind-columns (slot-value 'obj-scores)
                 (make-array (list n d) :initial-contents cont)))
             (send self :update-quant-scores)))
          (t t)))
      (slot-value 'p))))

(defmeth gifi-center-proto :add-gifi-var (gifi-var &key active)
"For internal use only. Called by :isnew of gifi-var-proto."
  (setf (slot-value 'in-analysis)
    (append (slot-value 'in-analysis) (list gifi-var)))
  (setf (slot-value 'active)
    (append (slot-value 'active) (list active))))

(defmeth gifi-center-proto :active (&optional (new nil set))
  (if set (setf (slot-value 'active) new)
    (slot-value 'active)))

(defmeth gifi-center-proto :kill-var (ind)
  (let ((keep (remove ind (iseq (length (slot-value 'active))))))
    (setf (slot-value 'active) (select (slot-value 'active) keep))
    (setf (slot-value 'in-analysis)
      (select (slot-value 'in-analysis) keep)))
    (send self :var-summary))



;;
;;    Accessor methods for gifi-center
;;

(defmeth gifi-center-proto :title (&optional (new-title nil set))
  (if set (setf (slot-value 'title) new-title)
    (slot-value 'title)))

(defmeth gifi-center-proto :raw-data (&optional (data nil set))
  (if set 
    (if (matrixp data) 
      (setf (slot-value 'raw-data) (column-list data))
      (select (slot-value 'raw-data) data))
    (slot-value 'raw-data)))

(defmeth gifi-center-proto :n (&optional reset)
  (if reset 
    (setf (slot-value 'n) (length (car (slot-value 'raw-data))))
    (slot-value 'n)))

(defmeth gifi-center-proto :obj-scores (&optional (new nil set))
  (if set (setf (slot-value 'obj-scores) new)
    (slot-value 'obj-scores)))

(defmeth gifi-center-proto :obj-labels (&optional (new-labels nil set))
  (if set (setf (slot-value 'obj-labels) new-labels))
  (let* ((lab (slot-value 'obj-labels))
         (l (cond 
              ((listp lab) lab)
              ((integerp lab) (coerce (send self :raw-data lab) 'list))
              (t nil))))
    (cond 
      ((null l) nil)
      ((stringp (car l)) l)
      ((numberp (car l))
       (mapcar #'(lambda (x) (format nil "~g" x)) l))
      ((symbolp (car l))
       (mapcar #'symbol-name l))
      (t nil))))

(defmeth gifi-center-proto :in-analysis ()
  (slot-value 'in-analysis))

(defmeth gifi-center-proto :obj-weights (&optional (new-weights nil set))
  (if set 
    (cond
      ((null new-weights) (setf (slot-value 'obj-weights) nil))
      ((= (length new-weights) (slot-value 'n))
       (setf (slot-value 'obj-weights) new-weights))
      (t (format t "Error: list must be empty or of length n.")))
    (slot-value 'obj-weights)))

(defmeth gifi-center-proto :loss-history ()
  (slot-value 'loss-history))

(defmeth gifi-center-proto :graphs ()
  (slot-value 'graphs))


;;
;;    Report methods for gifi-center-proto
;;

(defmeth gifi-center-proto :open-file (file-name)
  (if (stringp file-name)
    (setf (slot-value 'stream)
      (open file-name :direction :output))))

(defmeth gifi-center-proto :close-file ()
  (close (slot-value 'stream))
  (setf (slot-value 'stream) nil))

(defun dormat (stream &rest args)
  (apply #'format (cons stream args))
  (apply #'format (cons t args)))

(defmeth gifi-center-proto :var-summary (&optional report)
  (let ((stream (if report (slot-value 'stream)))
        (active (send self :active-gifi-vars))
        (passive (select (slot-value 'in-analysis)
                   (which (mapcar #'null (slot-value 'active))))))
    (flet ((report (gv)
             (dormat stream "   ~12a coded as  ~24a --~a~%"
               (send gv :var-label) (send gv :var-level)
               (symbol-name (send gv :slot-value 'proto-name)))))
      (dormat stream 
        "~%The following variables are active in analysis ~a:~%~%"
        (slot-value 'title))
      (dolist (gv active) (report gv))
      (if passive 
        (dormat stream "~%And these are not active:~%~%"))
      (dolist (gv passive) (report gv)))))

(defmeth gifi-center-proto :report-fit (&optional report)
  (let ((stream (if report (slot-value 'stream)))
        (active (send self :active-gifi-vars)))
    (dormat stream "~%Analysis of fit for ~a~%" (slot-value 'title))
    (dormat stream "~%    Multiple Fit~%")
    (dormat stream "    Variable          Sum             Dimensions~%")
    (dormat stream 
      "----------------------------------------------------------------------~%")
    (dolist (gv active) (send gv :sub-report-multiple-fit stream))
    (dormat stream "~%    Single Fit~%")
    (dormat stream
      "----------------------------------------------------------------------~%"
)
    (dolist (gv active) (send gv :sub-report-single-fit stream))))




