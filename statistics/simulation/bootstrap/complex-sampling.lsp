(defun stratified-bwo-method (yy nn bb func)
"Args: (y n b func)
Y is a list of lists, which each sub-list containing 
the observations in a stratum, and the number of lists 
in Y equal to the number of strata. The strata sizes are in N.
We apply Sitter's without-replacement bootstrap for
stratified samples B times, for the statistic computed
by FUNC. The list of bootstrap values is returned."
(let* (
       (ll (length yy))
       (wh (/ nn (sum nn)))
       (nh (mapcar #'length yy))
       (fh (/ nh nn))
       (kh (/ (* nn (- 1 (/ (- 1 fh) nh))) nh))
       (k1 (ceiling kh))
       (k2 (floor kh))
       (n1 (1- nh))
       (n2 nh)
       (a1 (/ (* k1 (- 1 (/ n1 (* nh k1)))) (* n1 (- (* nh k1) 1))))
       (a2 (/ (* k2 (- 1 (/ n2 (* nh k2)))) (* n2 (- (* nh k2) 1))))
       (pp (/ (- (/ (- 1 fh) (* nh (1- nh))) a2) (- a1 a2)))
       (bbb (iseq bb))
     )
(dotimes (b bb bbb)
(let* (
      (rr (uniform-rand ll))
      (tr (<= pp rr))
      (kr (if-else tr k1 k2))
      (nr (if-else tr n1 n2))
      (kn (* kr nr))
      (fk (map-elements #'repeat yy kr))
      (fl (map-elements #'sample fk nr))
      )
(setf (elt bbb b)  
      (funcall func fl kn))
))
))

(defun stratified-jackknife (yy nn func)
(let* (
      (ll (length yy))
      (nh (mapcar #'length yy))
      (fh (/ nh nn))
      (wh (/ (* (- 1 fh) (1- nh)) nh))
      (th (funcall func yy nn))
      )
(dotimes (i ll)
(dotimes (j (elt nh i))
(setf (elt yy i) jack-list (elt yy i))
(sum (* wh (mapcar #'(lambda (k) 
            (sum (^ (-  th) 2))) (iseq ll))))
))     



(defun stratified-rescaling-bootstrap (yy nn bb func &optional (replace t) (formula 1))
"Args: (yy nn bb func &optional (stratified t) (formula 1))
YY is a list of lists, which each sub-list containing 
the observations in a stratum. The number of lists 
in YY is equal to the number of strata. The strata sizes are in NN.
We apply Rao and Wu's rescaling method for stratified
samples with or without replacement BB times, using the statistic computed
by FUNC (which must be a function of the stratum means). 
FORMULA selects bootstrap sample sizes. The list of bootstrap values is returned."
(let* (
       (ll (length yy))
       (wh (/ nn (sum nn)))
       (nh (mapcar #'length yy))
       (yh (mapcar #'mean yy))
       (fh (if replace (/ nh nn) 0))
       (ns (cond ((= formula 0) (/ (^ (- nh 2) 2) (1- nh)))
                 ((= formula 1) (- nh 3))
                 ((= formula 2) nh)
                 ((= formula 3) (* (/ (^ (- nh 2) 2) (1- nh))
                                   (/ (- 1 fh) (^ (- 1 (* 2 fh)) 2))))
                 (t (1- nh))))
       (ch (sqrt (/ (* ns (- 1 fh)) (1- nh))))
       (bbb (iseq bb))
       )
(dotimes (b bb bbb)
(let* (
       (ys (map-elements #'sample yy ns t))
       (ym (mapcar #'mean ys))
       (yt (+ yh (* ch (- ym yh))))
      )
(setf (elt bbb b) (funcall func yt)) 
))
))

(defun stratified-mean (yy nn)
(let (
     (ll (length yy))
     (wh (/ nn (sum nn)))
     (nh (mapcar #'length yy))
     (yh (mapcar #'mean yy))
     )
(sum (* wh yh))
))

(defun stratified-dispersion (yy nn)
(let* (
     (ll (length yy))
     (wh (/ nn (sum nn)))
     (nh (mapcar #'length yy))
     (fh (/ nh nn))
     (sh (mapcar #'variance yy))
     )
(sum (* (^ wh 2) sh (/ (- 1 fh) nh)))
))

(defun variance (x)
(let (
     (n (length x))
     (m (mean x))
     )
(/ (sum (^ (- x m) 2)) (1- n))
))

(defun jack-list (x)
"Args: X
Returns n copies of list, with each of the elements
removed in turn."
(let (
     (n (length x))
     )
(map-elements #'select (make-list n :initial-element x)
              (map-elements #'remove (iseq n) 
                            (make-list n :initial-element (iseq n))))
))