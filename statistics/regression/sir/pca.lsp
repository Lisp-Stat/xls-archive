
(defproto pca-model-proto
  '(x nobs dim scale scores variances principal-directions)
  ()
  *object*
  "Principal Component Model")

(defun pca-model (x &key 
                    (print T))
  (let (
        (m (send pca-model-proto :new))
        )
    (send m :x x)
    (send m :compute)
    (if print (send m :display))
    m))

(defmeth pca-model-proto :isnew () (send self :needs-computing t))

(defmeth pca-model-proto :save ()
  `(pca-model',(send self :x)
                     ))

(defmeth pca-model-proto :compute ()
  (let*(
        (x (send self :x))
        (pcaout (pca-no-out x))
        )
    (setf (slot-value 'nobs) (nth 1 pcaout))
    (setf (slot-value 'dim) (nth 2 pcaout))
    (setf (slot-value 'scale) (nth 3 pcaout))
    (setf (slot-value 'scores) (nth 4 pcaout))
    (setf (slot-value 'variances) (nth 5 pcaout))
    (setf (slot-value 'principal-directions) (nth 6 pcaout))
    ))

(defmeth pca-model-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'scale) nil))
  (null (slot-value 'scale)))

(defmeth pca-model-proto :display ()
  (let*(
        (scale (send self :scale))
        (scale-type (nth scale (list "Correlations" "Covariances" "Range")))
        (eigen-vectors (send self :principal-directions))
        (variances (send self :variances))
        (var-prop (/ variances (sum variances)))
        )
    (format t "~2%==============================================================")
    (format t "~2%        *** Principal Component Analysis ***~2%")
    (format t "Scale type:                  ~s~2%" scale-type)
    (format t "Number of observations:            ~s~2%" (send self :nobs))
    (format t "Number of variables:               ~s~2%" (send self :dim))
    (format t "the first principal component:~%")
    (format t "~s" (nth 0 eigen-vectors))
    (format t "~2%the second principal component:~%")
    (format t "~S"(nth 1 eigen-vectors))
    (format t "~2%the third principal component:~%")
    (format t "~s"(nth 2 eigen-vectors))
    (format t "~2%the companion output variances of PCA:~%")
    (format t "~s" variances)
    (format t "~2%the proportions of all variances:~%")
    (format t "~s" var-prop)
    (format t "~2%==============================================================~%")
    (send self :plot1)
    ))


(defmeth pca-model-proto :x (&optional new-x)
  (when new-x
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth pca-model-proto :nobs ()
  (slot-value 'nobs))

(defmeth pca-model-proto :dim ()
  (slot-value 'dim))

(defmeth pca-model-proto :scale ()
  (slot-value 'scale))

(defmeth pca-model-proto :scores ()
  (slot-value 'scores))

(defmeth pca-model-proto :variances ()
  (slot-value 'variances))

(defmeth pca-model-proto :principal-directions ()
  (slot-value 'principal-directions))

(defmeth pca-model-proto :plot1 ()
  (setf plot1 (spin-plot (select (send self :scores) (list 0 1 2))
                         :variable-labels (list "pc1" "pc2" "pc3")
                         :title "Pricipal-Components"))
  (send plot1 :linked t)
  )

;==========================================================

(defun PCA-no-out (x)
  (let*(
        (scale (choose-item-dialog 
                "the scale used in  this analysis is based on :"
                '("Correlation"
                  "Covariance"
                  "Range")
                ))
        (dim (length x))
        (nobs (length (car x)))
        (sqrt-n-1 (sqrt (- nobs 1)))
        (cm (mapcar 'mean x))
        (x-cm (- x cm))
        cs
        sv
        scores
        variances
        eigen-vectors
        )
    (when (= scale 0)
          (setf cs (mapcar 'standard-deviation x-cm))
          (setf x-cm/cs (/ x-cm cs))
          (setf mx (transpose (make-array (list dim nobs) 
                                          :initial-contents x-cm/cs)))
          )
    (when (= scale 1)
          (setf mx (transpose (make-array (list dim nobs) 
                                          :initial-contents x-cm)))
          )
    (setf sv (good-sv-decomp mx))
    (setf scores (transpose (coer (* sqrt-n-1 (nth 0 sv)) )))
    (setf variances (^ (/ (nth 1 sv) sqrt-n-1) 2))
    (setf eigen-vectors (coer (transpose (nth 2 sv)) ))
    (list x nobs dim scale scores variances eigen-vectors)
    ))
        
;===============================================================

(defun good-sv-decomp (data &key (matrix nil))
"Args: (matrix &key (matrix nil))
Takes an arbitrary rectangular matrix and returns its singular value
decomposition with results ordered according to the singular values.
When :matrix nil the singular values are returned as a list.
When :matrix T the singular values are returned as a diagonal matrix."
    (let* (
           (svd (sv-decomp data) )
           (u (select svd 0) )
           (w (select svd 1) )
           (v (select svd 2) )
           (flag (select svd 3) )
           (order (reverse (order w)) )
           (svals (if matrix (diagonal (select w order))
                             (select w order)) )
           (lsingvects (apply #'bind-columns (select (column-list u) order)) )
           (rsingvects (apply #'bind-columns (select (column-list v) order)) )
          )
     (list lsingvects svals rsingvects flag)
  ))

(defun coer (x)
  (split-list (apply #'append
               (mapcar #'(lambda (x) (coerce x 'list))
                     (row-list x))) (nth 1 (array-dimensions x))))


