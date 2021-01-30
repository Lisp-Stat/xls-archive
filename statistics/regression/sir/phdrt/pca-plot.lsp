(defun pca-plot (x)
  (let*(
        (scale 0)
        (dim (length x))
        (nobs (length (car x)))
        (sqrt-n-1 (sqrt (- nobs 1)))
        (cm (mapcar 'mean x))
        (x-cm (- x cm))
        cs
        maxl
        minl
        range
        sv
        scores
        variances
        eigen-vectors
        )
    (when (= scale 0)
          (setf cs (mapcar 'standard-deviation x-cm))
          (setf x-cm (/ x-cm cs))
          )
    (when (= scale 2)
          (setf maxl (mapcar 'max x-cm))
          (setf minl (mapcar 'min x-cm))
          (setf range (- maxl minl))
          (setf x-cm (/ x-cm range))
          )
    (setf mx (transpose (make-array (list dim nobs) 
                                    :initial-contents x-cm)))          
    (setf sv (good-sv-decomp mx))
    (setf scores (transpose (coerce (* sqrt-n-1 (nth 0 sv)) 'list)))
    (setf variances (^ (/ (nth 1 sv) sqrt-n-1) 2))
    (setf eigen-vectors (coerce (transpose (nth 2 sv)) 'list))
;    (def pca-w (send :new :title "Principal Components"
;                       :location '(318 216) :size '(295 83)))

    (format t "Scale type:  ~s~2%" (nth scale (list "Covariance"
                                   "Correlation" "Range")))
    (format t "==============================================================")
    (format t "~2%      **** Principal Component Analysis ****")
    (format t "~2%Number of observations:      ~s~2%" nobs)
    (format t "Number of variables:         ~s~2%" dim)
    (format t "the first principal component:~%")
    (format t "~s" (nth 0 eigen-vectors))
    (format t "~2%the second principal component:~%")
    (format t "~S"(nth 1 eigen-vectors))
    (format t "~2%the third principal component:~%")
    (format t "~s"(nth 2 eigen-vectors))
    (format t "~2%the companion output variances of PCA:~%")
    (format t "~s" variances)
    (format t "~2%the proportions of all variances:~%")
    (format t "~s" (/ variances (sum variances)))
(format t "~2%==============================================================~%")
    (select scores (list 0 1 2))
    ))

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
