;;  alternating    cate-count
;;  column-means           cov             covw            covariance-matrix1        
;;  covariance-matrix2     eigen-decomp    g-normal        g-normal-us        ;;  g-uniform           
;;  index-list                       
;;  inversep       inversep1       inversep2       
;;  list-to-matrix  matrix-c   matrix-exam     matrix-to-list
;;  multislice     normal-us        print-list
;;  projection     projection1     rmv             random-prior
;;  row-mean       split           split1          SQRT-SSX 
;;  stand          STANDARD        STANDARD1       STANDARD2       
;;  standardize    subsampling                         
;;  xy-order        xy-sort      xy-sort1

(defun alternating (x)
  (let* (
         (nobs (length (car x)))
         (dim (length x))
         (tx (transpose x))
         (ntx (copy-list tx))
         )
    (if (oddp nobs)
        (setf nobs1 (- nobs 1))
        (setf nobs1 nobs))
    (dotimes (i nobs1)
             (setf j (+ i 1))
             (if (evenp i)
                 (setf (nth i ntx) (nth j tx))
                 (setf (nth i ntx) (nth (- i 1) tx))))
    (if (oddp nobs) (setf (nth (- nobs 1) ntx) (nth (- nobs 1) tx)))
    (transpose ntx)
    ))

(defun cate-count (y b)
"Args: (y b)
Y   - a list of lists of variables to be grouped by b
B   - a list of categories (class index)
Return list of # for each category of b"
   (let* (
           (k (length b))
           (l (repeat 0 k))
         )
     (dotimes (i k)
              (setf (nth i l) (length (which (= y (nth i b)))))
              )
     l))

(defun column-means (x)
"Args: (x)
X           - matrix of list of variables (in matrix form)
returns the column means of the matrix x."
(mapcar #'mean (column-list x)))

(defun cov (x)
"Args: (x)
X           - list of variables (list of lists)
Return the un-weighted covariance matrix of X"
(let* ( (nobs (length (nth 0 x)))
        (p (length x))
        (m1 (make-array (list p nobs) :initial-contents x))
        (m (transpose m1))
        (cm (mapcar 'mean (column-list m)))        
        (m-c (- m (outer-product (repeat 1 nobs) cm)))
        (m-c-1 (* m-c (repeat (repeat 1 nobs) (repeat p nobs))))
       )
(/ (%* (transpose m-c-1) m-c) (- nobs 1))
  ))


(defun covariance-matrix1 (&rest args)
"Args: (&rest args)
Returns the sample covariance matrix (nobs=n) of the data columns in ARGS. ARGS may
consist of lists, vectors or matrices."
  (let ((columns (apply #'append 
                        (mapcar (lambda (x) 
                                  (if (matrixp x) (column-list x) (list x)))
                                args))))
    (/ (cross-product (apply #'bind-columns 
                             (- columns (mapcar #'mean columns))))
       (length (car columns)))
    ))


(defun covariance-matrix2 (&rest args)
"Args: (&rest args)
Returns the sample covariance matrix (nobs=n) of the data columns in ARGS. ARGS may
consist of lists, vectors or matrices."
  (let ((columns (apply #'append 
                        (mapcar (lambda (x) 
                                  (if (matrixp x) (column-list x) (list x)))
                                args))))
    (cross-product (apply #'bind-columns 
                          (- columns (mapcar #'mean columns))))
    ))


(defun covw (x w)
"Return the weighted covariance matrix of X"

(let* ( (nobs (length (nth 0 x)))
        (p (length x))
        (m1 (make-array (list p nobs) :initial-contents x))
        (m (transpose m1))
        (cm (mapcar 'mean (column-list m)))        
        (m-c (- m (outer-product (repeat 1 nobs) cm)))
        (m-c-w (* m-c (repeat w (repeat p nobs))))
       )
(/ (%* (transpose m-c-w) m-c) (- nobs 1))
  ))


(defun eigen-decomp (m1 m2)
"eigenvalue decompsition of m1 with respect to m2;
based on singular value decompsotion so
the sign of eigenvales are not available"
(let* ((s-m2(nth 0 (chol-decomp m2)))
        (inv-s-m2 (inversep2 s-m2))
        (m3 (matmult (matmult inv-s-m2 m1) (transpose inv-s-m2)))
         (sing(sv-decomp m3))
        eig-value
        eig-vect)
       (setf eig-value (nth 1 sing ))
       (setf eig-vect (matmult (transpose (nth 0 sing))  inv-s-m2))
       
(list eig-vect eig-value)
      ))


(defun g-normal  (p n) 
"generate a list of p independent standard normal rvs each with n observations"
  (let*   (data-x)
    (setf data-x (iseq 0 (- p 1))) 
    (dotimes (i p)
             (setf (select data-x i)
                   (normal-rand n) ) )
    data-x))

(defun g-normal-us (p n u s)
"generate a list of p independent normal rvs (mean=u, std=s) each
with n observations"
  (let*(
        (out (repeat (list nil) p))
        )
    (dotimes (i p)
             (setf (nth i out) (+ u (* s (normal-rand n))))
             )
    out))


(defun g-uniform  (p n) 
"generate a list of p independent uniform rvs each with n observations"
  (let*   (data-x)
    (setf data-x (iseq 0 (- p 1))) 
    (dotimes (i p)
             (setf (select data-x i)
                   (uniform-rand n) ) )
    data-x))


(defun index-list (y)
"return a list of all different categories used in y"
  (let* (
         lt
         )
    (do (i)
        ((eql i 99))
        (setf lt (append lt (list (car y))))
        (setf y (remove (car (last lt)) y))
        (when (null y) (setf i 99))
        )
    (sort-data lt)
    ))

(defun inversep (m)
"Take a squae-matrix m and return the inverse-matrix of m.
 before finding the inverse, check if any diagonal item of m is 0.
 if 0, leave it out and put it back after the inverse operation."
  (let* (
         (col-list-m (column-list m))
         (dim (car (array-dimensions m)))
         (diag (diagonal m))
         (non-zero-elem (which (/= 0 diag)))
         (zero-elem (which (= 0 diag)))
         (len-non-zero (length non-zero-elem))
         (nm (select m NON-ZERO-ELEM NON-ZERO-ELEM))
         (inv-nm (inverse nm))
         (mx (repeat 0 dim))
         r
         c
         inv-mx
         )
    (dotimes (i dim) 
           (setf (nth i mx) 
                 (coerce (nth i col-list-m) 'list)))
    (dotimes (i len-non-zero)
             (setf r (nth i non-zero-elem))
             (dotimes (j len-non-zero)
                      (setf c (nth j non-zero-elem))
                      (setf (nth c (nth r mx))
                            (select inv-nm i j))))
    (setf inv-mx (make-array (list dim dim) :initial-contents mx))
    (cons inv-mx zero-elem)))

(defun inversep1 (m)
"Take a squae-matrix m and return the inverse-matrix of m.
 before finding the inverse, check if any diagonal item of m is 0.
 if 0, leave it out and put it back after the inverse operation."
  (let* (
         (col-list-m (column-list m))
         (dim (car (array-dimensions m)))
         (diag (diagonal m))
         (zero-elem (which (= 0 diag)))
         (non-zero-elem (which (/= 0 diag)))
         (len-non-zero (length non-zero-elem))
         (nm (select m NON-ZERO-ELEM NON-ZERO-ELEM))
         (inv-nm (inverse nm))
         (mx (repeat 0 dim))
         r
         c
         )
    (dotimes (i dim) 
           (setf (nth i mx) 
                 (coerce (nth i col-list-m) 'list)
             ))
    (print nm)
    (print inv-nm)
    (print (%* nm inv-nm))
    (dotimes (i len-non-zero)
             (setf r (nth i non-zero-elem))
           (dotimes (j len-non-zero)
                    (setf c (nth j non-zero-elem))
                    (setf (nth j (nth i mx))
                          (select inv-nm i j)
                          )))
    (make-array (list dim dim) :initial-contents mx)
    ))

(defun inversep2 (m)
"Take a squae-matrix m and return the inverse-matrix of m.
 before finding the inverse, check if any diagonal item of m is 0.
 if 0, leave it out and put it back after the inverse operation."
  (let* (
         (col-list-m (column-list m))
         (dim (car (array-dimensions m)))
         (diag (diagonal m))
         (non-zero-elem (which (/= 0 diag)))
         (zero-elem (which (= 0 diag)))
         (len-non-zero (length non-zero-elem))
         (nm (select m NON-ZERO-ELEM NON-ZERO-ELEM))
         (inv-nm (inverse nm))
         (mx (repeat 0 dim))
         r
         c
         inv-mx
         )
    (dotimes (i dim) 
           (setf (nth i mx) 
                 (coerce (nth i col-list-m) 'list)))
    (dotimes (i len-non-zero)
             (setf r (nth i non-zero-elem))
             (dotimes (j len-non-zero)
                      (setf c (nth j non-zero-elem))
                      (setf (nth c (nth r mx))
                            (select inv-nm i j))))
    (setf inv-mx (make-array (list dim dim) :initial-contents mx))
    inv-mx))

(defun list-to-matrix (x)
"Returns X (list of lists) in matrix form"
  (let* (
         (dim (length x))
         (nob (length (car x)))
         )
    (transpose (make-array (list dim nob) :initial-contents x))
    ))


(defun matrix-c (x)
"Returns x in matrix form. x is a list of lists, each giving a column."
(let* ( 
        (l (length x))
        (x-mat (nth 0 x)))
  (dotimes (i (- l 1))         
           (setf x-mat (bind-columns x-mat (nth (+ i 1) x))))
  x-mat))

(defun matrix-exam (m e)
"Take a matix m, check if any element of this matrix is too small (< e)
 but different from 0, return a matrix with those small element replace
 by 0."
(let* (
       (dim (array-dimensions m))
       (dim1 (nth 0 dim))
       (dim2 (nth 1 dim))
       element
       )
  (dotimes (i dim1)
           (dotimes (j dim2)
                    (setf element (select m i j))
                    (if (<= (abs element) e)
                        (setf (select m i j) 0)
                      )
                    ))
m))  


(defun matrix-to-list (m)
"Return m (a matrix) in a list (list of lists) form"
  (transpose (coerce m 'list))
  )


(defun multislice (y u)
"Args: (y u)
Y           - list of original independent variables (class-index)
U           - a list of lists of directions to be used for further
              slicing
Slice # for : u1...uk  -  input from dialog, should be a list of
                          # of slices to be used for each additional
                          variable in the slicing procedure
Returns a new list (with length = nobs) of slice index for each
observation to be used in further slicing."
  (let* (
         (ly (length y))
         (y1 (copy-list y))
         (y2 (copy-list y))
         (n (iseq 0 (- ly 1)))
         (b (index-list y))
         (lb (length b))
         (k (length u))
         (index 1)
         (sk (car (get-value-dialog "Slice # for : u1...uk")))
         (ap (list nil))
         su1
         n1
         lbb
         rb
         sk1
         n22
         n11
         h11
         h22
         j1
         )
    (dotimes (q k)
             (setf y1 (copy-list y2))
             (if (= q 0) (setf index1 lb) (setf index1 (- index 1)))
             (setf index 1)
             (dotimes (i index1)
                      (if (= q 0) (setf rb b) (setf rb (iseq 1 index1)))
                      (setf su1 (select (nth q u) (which (= (nth i (iseq 1 index1)) y1))))
                      (setf n1 (select n (which (= (nth i (iseq 1 index1)) y1))))
                      (setf n1 (reverse (xy-order n1 su1)))
                      (setf lbb (length n1))
                      (setf sk1 (nth q sk))
                      (setf n22 (floor (/ lbb sk1)))
                      (setf h11 (- lbb (* sk1 n22)))
                      (setf h22 (- sk1 h11))
                      (setf n11 (+ n22 1))
                      (setf j1 0)
                      (if (< (length n1) sk1)
                          (dotimes (j lbb)
                                   (setf (nth (nth j n1) y2) index)
                                   (setf index (+ index 1))
                                   )
                          (dolist (j (repeat (list n11 n22) (list h11 h22)))
                                  (dolist (m (iseq j1 (- (+ j1 j) 1)))
                                          (setf (nth (nth m n1) y2) index)
                                          )
                                  (setf index (+ index 1))
                                  (setf j1 (+ j1 j))
                                  ))))
    y2))


(defun normal-us (u s &optional n)
"generate a N(u,s) rv, u & s can be a num or a list without n ,
the # of observations is 1 or the length of u (or s)."
  (let*(
        out
        )
    (if (numberp u)
        (setf nu 1)
        (setf nu (length u))
        )
    (if (numberp s)
        (setf ns 1)
        (setf ns (length s))
        )
    (when n (setf n (max (list n nu ns))))
    (unless n (setf n (max (list nu ns))))
    (setf out (+ u (* s (normal-rand n))))
    out))

(defun print-list (x)
  (let* (
         (dim (length x))
         (nob (length (car x)))
         (mx (transpose (make-array (list dim nob) :initial-contents x)))
         )
    (print-matrix mx)
    ))

(defun projection (b x)
"Args: (b x)
B  - vector to be used for transformation (in list form)
X  - a list of lists of original variables (list of lists)
Returns a new variable (%* b x) in list form (with length = nobs)"
  (let* (
         (p (length b))
         (n (length (car x)))
         (mx (make-array (list p n) :initial-contents x))
         )
    (setf nx (%* b mx))
    nx))


(defun projection1 (datax p n b c)
"provide the criterion variable f= c+ b^t   datax, "
(let* ((f( repeat c n)))
  (dotimes (i p)
           (setf f (+ f (* (nth i b) (select datax i))))
           )
  f))

(defun random-prior (n b)
"Args: (n b)
N  - # of random numbers to be generated (a number)
B  - a list of categories to be generated (a list)
Returns a list of randomly repeated numbers from b with length = n"
  (let* (
         (b1 (/ b (sum b)))
         (u (uniform-rand n))
         (m (length b))
         (bb (repeat 0 m))
         (c (repeat 0 n))
         )
    (setf (nth 0 bb) (nth 0 b1))
    (dotimes (i (- m 1))
             (setf (nth (+ i 1) bb) (+ (nth i bb) (nth (+ i 1) b1)))
             )
    (dotimes (i n)
             (setf (nth i c) (+ 1 (car (which (<= (- (nth i u) bb) 0))))))
    c))

(defun rmv (data k)
"remove the  k-th variable, k=0,..."
  (let* (dim)
    (setf dim (length data)) 
    (select data  (remove k (iseq 0 (- dim 1))))))


(defun row-mean (x)
" yield mean of rows of x, x is a list of variables"
(let*( (obs (length (nth 0 x)))
        (r-mean-x (list (mean  (subsampling x (list 0))) )))
  (dotimes (i (- obs 1))
(setf r-mean-x (append r-mean-x (list (mean (subsampling x (list (+ i 1)))))))
)
r-mean-x))


(defun split (x b)
"Args: (x b)
X   - a list of lists of variables to be grouped by b
      (car x) = y
B   - a list of categories (class index)
Return grouped x (include y) by the categories of b"
   (let* ( (mx (transpose x))
           (k (length b))
           (l (repeat 0 k))
           (s (repeat 0 k))
         )
     (dotimes (i k)
              (setf (nth i l) (which (= (car x) (nth i b))))
              (setf (nth i s) (transpose (append (select mx (nth i l)))))
              )
     s))

(defun split1 (x y b)
"Return grouped x (without y) by the categories of b"
   (let* ( (mx (transpose x))
           (k (length b))
           (l (repeat 0 k))
           (s (repeat 0 k))
         )
     (dotimes (i k)
              (setf (nth i l) (which (= y (nth i b))))
              (setf (nth i s) (transpose (append (select mx (nth i l)))))
              )
     s))

(defun SQRT-SSX (x )
"Return the SQRT-SSX matrix of X"
(let* ( (nobs (length (nth 0 x)))
        (p (length x))
        (m1 (make-array (list p nobs) :initial-contents x))
        (m (transpose m1))
        (cm (mapcar 'mean (column-list m)))        
        (m-c (- m (outer-product (repeat 1 nobs) cm)))
        (cov (/ (%* (transpose m-c) m-c) (- nobs 1)))
        (l (car (chol-decomp cov)))
        )
  l))


(defun stand (x)
  (let* (
         (dim (length x))
         (xs (repeat 0 dim))
         )
         (dotimes (i dim)
                     (setf (nth i xs) (standard-deviation (nth i x)))
                     )
    xs))

(defun STANDARD (x )
"Return the STANDARDIZED matrix of X"

(let* ( (nobs (length (nth 0 x)))
        (p (length x))
        (x1 (mapcar '- x (mapcar 'mean x)))
        (m1 (make-array (list p nobs) :initial-contents x1))
        
        (cov (/ (%* m1 (transpose m1)) (- nobs 1)))
        (l (car (chol-decomp cov)))
        (newx (coerce (%* (inverse l) m1 ) 'list))
        )
  newx))

(defun STANDARD1 (x )
"Return the STANDARDIZED matrix of X"

(let* ( (nobs (length (nth 0 x)))
        (p (length x))
        (x1 (mapcar '- x (mapcar 'mean x)))
        (m1 (make-array (list p nobs) :initial-contents x1))
        
        (cov (/ (%* m1 (transpose m1)) (- nobs 1)))
        (l (car (chol-decomp cov)))
        (newx (coerce (%* (inverse l) m1 ) 'list))
        (out (cons newx l))
        )
  out))

(defun STANDARD2 (x )
"Return the STANDARDIZED matrix of X"

(let* ( (nobs (length (nth 0 x)))
        (p (length x))
        (m1 (make-array (list p nobs) :initial-contents x))
        (m (transpose m1))
        (cm (mapcar 'mean (column-list m)))        
        (m-c (- m (outer-product (repeat 1 nobs) cm)))
        (cov (/ (%* (transpose m-c) m-c) nobs))
        (l (car (chol-decomp cov)))
        (newx (coerce (%* (inverse l) (transpose m-c) ) 'list))
        (out (cons newx l))
        )
  out))


(defun standardize (x)
  (let* (
        (p (length x))
        (n (length (car x)))
        (cov (covw x (repeat 1 n)))
        (l (car (chol-decomp cov)))
        (xm (make-array (list p n):initial-contents x))
        (newx (coerce (%* (inverse l) xm) 'list))
        )
    newx))


(defun subsampling (x case)
" selecting case from x, x is a list of variables, case is a list of
cases to be selected"
(let* ( (dim (length x))
        (x-out (list (select (nth 0 x) case))))
   (dotimes (i (- dim 1))
    (setf x-out (append x-out (list (select (nth (+ i 1) x) case)))))
    x-out))


(defun xy-order (x y)
"orders the elements of x according to the orderof y"
  (let* ( (x-order '()))
    (dolist (i (order y))
        (setf x-order (cons (nth i x) x-order)))
    x-order))


(defun xy-sort (x y)
"Return sorted x & y by the order of y"
   (let* ( (n (length y))
           (p (length x))
           (x-order '())
           (m (make-array (list p n) :initial-contents x))
           (m1 (transpose m))
           (m2 (coerce m1 'list))
           (y-order (sort-data y))
           m3
           m4
           m5           
           )
         (dolist (i (reverse (order y)))
               (setf x-order (cons (nth i m2) x-order))
         )
         (setf m3 (make-array (list n p) :initial-contents x-order))
         (setf m4 (transpose m3))
         (setf m5 (coerce m4 'list))
     (cons m5 y-order)
     ))

(defun xy-sort1 (x y)
"Return sorted y & x by the order of y"
   (let* ( (n (length y))
           (p (length x))
           (x-order '())
           (m (make-array (list p n) :initial-contents x))
           (m1 (transpose m))
           (m2 (coerce m1 'list))
           (y-order (sort-data y))
           m3
           m4
           m5           
           )
         (dolist (i (reverse (order y)))
               (setf x-order (cons (nth i m2) x-order))
         )
         (setf m3 (make-array (list n p) :initial-contents x-order))
         (setf m4 (transpose m3))
         (setf m5 (coerce m4 'list))
     (cons y-order m5)
     ))

