(defproto binary-smooth-proto
  '(n-points k setup plot-x-vals plot-smooth-vals dim-smooth
             var-names n-ivs x x-estimate likelihood dim-vals
             col-x resids-z adj-dv)
  () binary-models-proto)

(defmeth binary-smooth-proto :isnew
  ()
  (send self :x (send *binary-model-object* :x-use))
  (send self :col-x (column-list (send self :x)))
  (send self :x-names (remove '"Intercept"
                              (send *binary-model-object* :x-names)
                              :test 'equalp))
  (send self :n-ivs (length (send self :x-names)))
  (send self :nobs (send *binary-model-object* :nobs))
  (when (send self :select-k)
        (when (send self :select-n-points)
              (send self :setup-setup 
                    (send *binary-model-object* :y)
                    (send self :x)
                    (send self :k)
                    (send self :n-points))
              (send self :local-scoring 
                    (send *binary-model-object* :y)
                    (send self :x)
                    (send self :setup))
              (send (select
                     (send *binary-model-object* :visual-menu-items) 1)
                    :enabled t))))
  
(defmeth binary-smooth-proto :select-k ()
  (let ((k (get-value-dialog
            "Smoothing window size"
            :initial .4))
        (nobs (send *binary-model-object* :nobs)))
    (when (and k (select k 0))
          (let ((k (select k 0)))
            (if (> k 1)
                (send self :k (min (round k) nobs))
                (send self 
                      :k (round (* k nobs))))))))

(defmeth binary-smooth-proto :k (&optional (number nil set))
"Args: (&optional number)
Sets or returns smoothing window size"
  (when set (setf (slot-value 'k) number))
  (slot-value 'k))

(defmeth binary-smooth-proto :n-ivs (&optional (number nil set))
"Args: (&optional number)
Sets or returns number of ivs to be smoothed"
  (when set (setf (slot-value 'n-ivs) number))
  (slot-value 'n-ivs))

(defmeth binary-smooth-proto :likelihood (&optional (number nil set))
"Args: (&optional number)
Sets or returns value of likelihood"
  (when set (setf (slot-value 'likelihood) number))
  (slot-value 'likelihood))

(defmeth binary-smooth-proto :x-names (&optional (number nil set))
"Args: (&optional number)
Sets or returns names of ivs to be smoothed"
  (when set (setf (slot-value 'x-names) number))
  (slot-value 'x-names))

(defmeth binary-smooth-proto :x (&optional (matrix nil set))
"Args: (&optional matrix)
Sets or returns iv matrix"
  (when set (setf (slot-value 'x) matrix))
  (slot-value 'x))

(defmeth binary-smooth-proto :col-x (&optional (list nil set))
"Args: (&optional list)
Sets or returns colunmn list of iv matrix"
  (when set (setf (slot-value 'col-x) list))
  (slot-value 'col-x))

(defmeth binary-smooth-proto :adj-dv (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of adjusted dependent variables"
  (when set (setf (slot-value 'adj-dv) list))
  (slot-value 'adj-dv))

(defmeth binary-smooth-proto :setup (&optional (list nil set))
"Args: (&optional list)
Sets or returns the list of lists of setup values"
  (when set (setf (slot-value 'setup) list))
  (slot-value 'setup))

(defmeth binary-smooth-proto :dim-vals (&optional (list nil set))
"Args: (&optional list)
Sets or returns the list dimension values"
  (when set (setf (slot-value 'dim-vals) list))
  (slot-value 'dim-vals))

(defmeth binary-smooth-proto :dim-smooth (&optional (list nil set))
"Args: (&optional list)
Sets or returns the list of dimension smooths"
  (when set (setf (slot-value 'dim-smooth) list))
  (slot-value 'dim-smooth))

(defmeth binary-smooth-proto :resids-z (&optional (list nil set))
"Args: (&optional list)
Sets or returns the list of standardized residuals"
  (when set (setf (slot-value 'resids-z) list))
  (slot-value 'resids-z))

(defmeth binary-smooth-proto :x-estimate (&optional (list nil set))
"Args: (&optional list)
Sets or returns the list of list of x's to be estimates"
  (when set (setf (slot-value 'x-estimate) list))
  (slot-value 'x-estimate))

(defmeth binary-smooth-proto :n-points (&optional (number nil set))
"Args: (&optional number)
Sets or returns number of estimated points"
  (when set (setf (slot-value 'n-points) number))
  (slot-value 'n-points))

(defmeth binary-smooth-proto :select-n-points ()
  (let ((n (get-value-dialog 
            "Number of points estimated" 
            :initial 12)))
    (when (and n (select n 0))
          (send self :n-points
                (select n 0)))))

(defmeth binary-smooth-proto :solve (y x &optional w)
  (let ((x-prime (transpose x)))
    (if w
        (matmult (inverse (matmult x-prime
                                   (apply #'bind-rows (* w (row-list x)))))
                 x-prime (* w y))
        (matmult (inverse (matmult x-prime x)) x-prime y))))

(defmeth binary-smooth-proto :local-scoring (y x setup-junk)
  (let* ((val (array-dimension x 1))
         (dim-vals (repeat 0 val))
         (dim-smooth (repeat 0 val))
         (adj-dv (repeat 0 val)))
    (labels
      ((linear-smoother 
        (y junk w dim)
        (let* ((y-s (select junk 0))
               (x-s (select junk 1))
               (k (send self :k))
               (npoints (send self :n-points))
               (est-points-short (select junk 2))
               (nobs (send self :nobs))
               (order-x (select junk 3))
               (obs-to-estimate (select junk 4))
               (rcx (mapcar #'rank (send self :col-x)))
               (selector (select junk 5))
               (resp-points-short (repeat 0 npoints))
               (resp-points (repeat 0 nobs))
               (maxsel (max selector))
               (numhighs (length (member maxsel selector)))
               (numlows (length (member 0 (reverse selector))))
               (point nil))
          (do* ((j 0))
               ((= j npoints)
                (setf (select dim-vals dim) resp-points-short)
                (setf (select dim-smooth dim)
                      (select resp-points 
                              (select rcx dim)))
                resp-points)
               (let ((i (select selector j)))
                 (cond
                   ((= i 0)
                    (let* ((seq1 (iseq numlows))
                           (seq2 (iseq
                                  (max 1
                                       (select obs-to-estimate
                                               (- numlows 1)))))
                           (max-seq2 (select (last seq2) 0))
                           (beta (send self :solve
                                       (select y (select y-s 0))
                                       (select x-s 0)
                                       (select w (select y-s 0)))))
                      (setf (select resp-points-short seq1)
                            (+ (select beta 0)
                               (* (select est-points-short seq1)
                                  (select beta 1))))
                      (setf (select resp-points seq2)
                            (+ (select beta 0) (* (select order-x seq2)
                                                  (select beta 1))))
                      (setf point (list (select order-x max-seq2)
                                        (select resp-points max-seq2)))
                      (setf j (+ numlows j))))
                   ((= i maxsel)
                    (let* ((n1 (select obs-to-estimate j))
                           (n2 (select obs-to-estimate (- j 1)))
                           (seq1 (+ (iseq numhighs) j))
                           (seq2 (iseq n1 (- nobs 1)))
                           (seq3 (iseq n2 (- n1 1)))
                           (beta (send self :solve 
                                       (select y (select y-s maxsel))
                                       (select x-s maxsel)
                                       (select w (select y-s maxsel)))))
                      (setf (select resp-points-short seq1)
                            (+ (select beta 0)
                               (* (select est-points-short seq1)
                                  (select beta 1))))
                      (setf (select resp-points seq2)
                            (+ (select beta 0) (* (select order-x seq2)
                                                  (select beta 1))))
                      (let* ((x1 (select point 0))
                             (y1 (select point 1))
                             (x2 (select est-points-short  j))
                             (y2 (select resp-points-short j))
                             (beta1 (/ (- y2 y1) (- x2 x1)))
                             (alpha (- y1 (* beta1 x1))))
                        (setf (select resp-points seq3) 
                              (+ alpha (* (select order-x seq3) beta1))))
                      (setf j npoints)))
                   (t (let* ((seq (iseq (select obs-to-estimate (- j 1))
                                        (- (select obs-to-estimate j) 1)))
                             (sel-y-s (select y-s i))
                             (beta (send self :solve 
                                    (select y sel-y-s)
                                    (select x-s i)
                                    (select w sel-y-s)))
                             (this-point-x (select est-points-short j))
                             (this-point-y
                              (+ (select beta 0)
                                 (* this-point-x (select beta 1)))))
                        (setf (select resp-points-short j) this-point-y)
                        (let* ((x1 (select point 0))
                               (y1 (select point 1))
                               (beta1 (/ (- this-point-y y1)
                                         (- this-point-x x1)))
                               (alpha (- y1 (* beta1 x1))))
                          (setf (select resp-points seq) 
                                (+ alpha (* (select order-x seq) beta1))))
                        (setf j (+ 1 j))
                        (setf point (list this-point-x this-point-y)))))))))
       (backfit-smooth 
        (y x w setup-junk)
        (let* ((col-x (column-list x))
               (nvars (length col-x))
               (order-x (mapcar #'order col-x))
               (rank-x (mapcar #'rank col-x))
               (seq (iseq (+ 1 nvars)))
               (dem-vals (append (list y) (repeat 0 nvars))))
          (do* ((iter 1 (+ iter 1))
                (pred-vals 
                 (apply #'+ (rest dem-vals))
                 (apply #'+ (rest dem-vals)))
                (sse-diff 
                 5
                 (- sse (sum (^ (- y pred-vals) 2))))
                (sse 
                 (sum (^ (- y pred-vals) 2))
                 (- sse sse-diff)))
               ((or (= iter 20) (< sse-diff 3))
                (apply #'+ (rest dem-vals)))
               (if (= iter 1)
                   (dotimes (i nvars)
                            (if (= i 0)
                                (setf (select dem-vals 1) 
                                      (select
                                       (linear-smoother 
                                        (select y (select order-x 0))
                                        (select setup-junk 0)
                                        (select w (select order-x 0))
                                        0)
                                       (select rank-x 0)))
                                (setf (select dem-vals (+ i 1))
                                      (select
                                       (linear-smoother
                                        (select
                                         (apply #'- 
                                                (select dem-vals
                                                        (iseq (+ i 1))))
                                         (select order-x i))
                                        (select setup-junk i)
                                        (select w (select order-x i)) i)
                                       (select rank-x i)))))
                   (dotimes (i nvars)
                            (setf (select dem-vals (+ i 1))
                                  (select
                                   (linear-smoother
                                    (select
                                     (apply #'-
                                            (select dem-vals
                                                    (remove (+ i 1) seq)))
                                     (select order-x i))
                                    (select setup-junk i)
                                    (select w (select order-x i)) i)
                                   (select rank-x i)))))))))
      (do* ((eta 0 (backfit-smooth z x w setup-junk))
            (exp (exp eta) (exp eta))
            (p (/ exp (+ 1 exp)) (/ exp (+ 1 exp)))
            (w (repeat (* p (- 1 p)) (length y)) (* p (- 1 p)))
            (z (+ eta (/ (- y p) w)) (+ eta (/ (- y p) w)))
            (iter 1 (+ iter 1))
            (likelihood-diff
             -5 (- likelihood (sum (+ (* y (log p))
                                      (* (- 1 y) (log (- 1 p)))))))
            (likelihood (sum (+ (* y (log p)) (* (- 1 y) (log (- 1 p)))))
                        (- likelihood likelihood-diff)))
           ((or (= iter 15) (> likelihood-diff -1)) 
            (send self :pred-val eta)
            (send self :pred-val-p p)
            (send self :likelihood likelihood)
            (send self :dim-vals dim-vals)
            (send self :dim-smooth dim-smooth)
            (dotimes (i val)
                     (let ((sum (- z (apply #'+ dim-smooth))))
                       (setf (select adj-dv i)
                             (+ sum (select dim-smooth i)))))
            (send self :adj-dv adj-dv)
            (send self :resids-z (/ (- y p)
                                    (sqrt (* p (- 1 p))))))))))
  
(defmeth binary-smooth-proto :setup-setup (y x k npoints)
  (let* ((x-col (column-list x))
         (nvars (length x-col))
         (return-list nil))
    (dotimes (i nvars)
             (setf return-list
                   (append return-list 
                           (list 
                            (send self :local-scoring-setup
                                  y (select x-col i) k npoints)))))
                   (send self :setup return-list)))
         
(defmeth binary-smooth-proto :local-scoring-setup (y x k npoints)
  (let* ((k (* 2 (round (/ k 2))))
         (k2 (/ k 2))
         (x-col (coerce x 'list))
         (nobs (length x-col))
         (ones (repeat 1  k))
         (order (order x-col))
         (y (select y order))
         (order-x (select x-col order))
         (order-y (select y order))
         (x-list nil)
         (y-list nil)
         (selector nil)
         (x-estimate (rseq (select order-x 0)
                           (select order-x (- nobs 1)) npoints))
         (obs-to-estimate
          (mapcar #'(lambda (x) (select (rank (cons x order-x)) 0))
                  x-estimate))
         (seq1 (iseq k))
         (low-list-x (select order-x seq1)))
    (setf (select obs-to-estimate (- npoints 1)) (- nobs 1))
    (send self :x-estimate
          (append (send self :x-estimate) (list x-estimate)))
    (do ((j 0))
        ((= j  npoints)
         (list y-list x-list x-estimate
               order-x obs-to-estimate selector y))
        (let* ((i (select obs-to-estimate j))) 
          (cond
            ((= j 0)
             (setf selector (list 0))
             (setf y-list (list seq1))
             (setf x-list (list (bind-columns ones low-list-x)))
             (setf j 1))
            ((<= i (+ 1 k2))
             (setf selector (append selector (list 0)))
             (setf j (+ 1 j)))
            ((>= i (- nobs k2))
             (let ((seq2 (iseq (- nobs k) (- nobs 1))))
               (setf x-list (append
                             x-list
                             (list (bind-columns
                                    ones
                                    (select order-x seq2)))))
               (setf y-list (append y-list (list seq2)))
               (setf selector
                     (append selector
                             (repeat (+ 1 (max selector))
                                     (- npoints (length selector)))))
               (setf j npoints)))
            (t (let ((seq3 (iseq (- i k2) (+ i (- k2 1)))))
                 (setf selector
                       (append selector
                               (list (length y-list))))
                 (setf x-list
                       (append x-list
                               (list (bind-columns
                                      ones (select order-x seq3)))))
                 (setf y-list (append y-list (list seq3)))
                 (setf j (+ 1 j)))))))))