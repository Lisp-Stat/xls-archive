;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Factor Analysis code, written on a boring trip from LA to Delaware
;; December 15-17, 1994.
;;
;; Still to come:
;;
;;          -- Derflinger Algorithms for LS and ML
;;          -- Factor Score Estimation
;;          -- Standard Errors (although you could bootstrap)
;;          -- Implementation of EM (just for the heck of it)
;;
;; This module will not be extended to exploratory factor analysis,
;; I'll save that for a general SEM module.
;;
;; Revisions: 
;;      cut out some crap, completed Swain module (02-25-95, Durham, NC)
;;      completed Minimum Trace FA, added abbrevs (02-26-95, Durham, NC)
;;
;; Jan de Leeuw (deleeuw@stat.ucla.edu)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maximum-likelihood-factor-analysis 
  (c p &key (epsilon 1e-6) (nmax0 20) (nmax1 100) (verbose t))
"Args: (c p)
Exploratory ML Factor Analysis of the covariance
or correlation matrix c. Extracts p factors. Uses
the Hemmerle refactoring method. Starts with LS 
Factor Analysis solution."
(format t "~s~%" "We start with a LS Factor Analysis")
(let* (
       (ls (least-squares-factor-analysis c p :epsilon epsilon :nmax nmax0))
       (aa (first ls))
       (uu (second ls))
       (nn (length uu))
       (r0 (+ (determinant c) nn))
       (ca (+ (diagonal uu) (matmult aa (transpose aa))))
       (rr (+ (determinant ca) (sum (* (inverse ca) c))))
       (it 0)
       )
(format t "~s~%" "We now switch to ML Factor Analysis")
(loop
(let* (
       (vv (/ (sqrt uu)))
       (cc (- (* c (outer-product vv vv)) (identity-matrix nn)))
       (dd (truncated-eigen-decomposition cc p))
       )
(setf it (1+ it))
(setf aa (* dd (outer-product (/ vv) (repeat 1 p))))
(setf uu (diagonal (- c (matmult aa (transpose aa)))))
(setf ca (+ (diagonal uu) (matmult aa (transpose aa))))
(setf ss (+ (determinant ca) (sum (* (inverse ca) c))))
(if verbose (format t "~d ~,8f ~,8f ~%" it (- rr r0) (- ss r0)))
(if (or (> epsilon (max (abs (- rr ss)))) (= it nmax1))
    (return (list aa uu))
    (setf rr ss))))
)) 

(defun least-squares-factor-analysis 
  (c p &key (epsilon 1e-6) (nmax 100) (verbose t))
"Args: (c p)
Exploratory LS Factor Analysis of the covariance
or correlation matrix C. Extracts p factors. Uses
Alternating Least Squares. Starts with SMC uniqueness
estimates."
(let* (
       (u (uniqueness-bound c))
       (d (- c (diagonal u)))
       (a (truncated-eigen-decomposition d p))
       (s (matmult a (transpose a)))
       (r (sum (^ (- d s) 2)))
       (it 0)
       )
(loop
(setf it (1+ it))
(setf u (- (diagonal c) (diagonal s)))
(setf d (- c (diagonal u)))
(setf a (truncated-eigen-decomposition d p))
(setf s (matmult a (transpose a)))
(setf v (sum (^ (- d s) 2)))
(if verbose (format t "~d ~,8f ~,8f ~%" it r v))
(if (or (> epsilon (max (abs (- r v)))) (= it nmax))
    (return (list a u))
    (setf r v))) 
))

(defun uniqueness-bound (c)
(/ (diagonal (inverse c))))

(defun truncated-eigen-decomposition (c p)
"Args (c p)
Approximates a symmetric matrix by a positive 
semi-definite matrix of rank p which is closest 
in the least squares sense."
  (let* (
         (n (first (array-dimensions c)))
         (x (eigen c))
         (g (select (first x) (iseq p)))
         (h (/ (+ g (abs g)) 2))
         (y (select (second x) (iseq p)))
         (a (make-array (list p n) :initial-contents
                        (mapcar #'(lambda (x) (coerce x 'list)) y)))
         )
(matmult (transpose a) (diagonal (sqrt h)))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the FlagShip of Exploratory Factor Analysis.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun factor-analysis-a-la-swain 
  (c p &key (f #'ml-func) (epsilon 1e-6) (nmax0 20) (nmax1 100) (verbose t))
"Args: (c p &key (f #'ml-func) (epsilon 1e-6) (nmax 100))
Exploratory Factor Analysis of the covariance
or correlation matrix c using the generalized Swain
(Psychometrika, 1975, p 315-335) loss function. 
Extracts p factors. Uses Newton's method. 
Starts with LS Factor Analysis solution."
(format t "~s~%" "We start with a LS Factor Analysis")
(let* (
       (ls (least-squares-factor-analysis c p :epsilon epsilon :nmax nmax0))
       (un (second ls))
       (ss (matrix-function c #'(lambda (x) (/ (sqrt x)))))
       (nn (length un))
       (ii (identity-matrix nn))
       (im (which (>= (iseq nn) p)))
       (it 0)
       )
(format t "~s~%" "We now switch to Swain-Newton")
(loop
(let* (
       (vv (matmult ss (matmult (diagonal un) ss)))
       (eig (eigen vv))
       (evl (first eig))
       (evc (apply #'bind-columns (second eig)))
       (fev (funcall f evl))
       (ff (first fev))
       (gg (second fev))
       (hh (third fev))
       (uu (matmult ss evc))
       (grad (matmult (^ (select uu (iseq nn) im) 2) 
                    (select gg im)))
       (hess (make-array (list nn nn)))
       (bb (+ (diagonal hh) 
              (* 2 (matmult (diagonal gg)
                            (- (/ (+ ii 
                                     (outer-product evl evl #'-))) ii)))))
       (bs (select bb im (iseq nn)))
       )
(dotimes (r nn)
(dotimes (s nn)
(let 
     (
     (ur (select uu r (iseq nn)))
     (us (select uu s (iseq nn)))
     )
(setf (aref hess r s)
(sum (* bs (outer-product (select ur 0 im) ur) 
        (outer-product (select us 0 im) us))))
)))
(let (
     (um (matmult (inverse hess) grad))
     )
(if (or (< (max (abs um)) epsilon) (= (incf it) nmax1))
    (return un)
    (setf un (- un um))))
(if verbose (format t "~d ~,8f ~%" it (sum (select ff im))))
))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Candidate functions for the Swain criterion. Return function
;; value, first-derivative value, second-derivative value.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ml-func (x)
(list
(+ (log x) (/ x) (- 1))
(+ (/ x) (- (/ (* x x))))
(+ (- (/ (* x x))) (/ 2 (* x x x)))))

(defun gls-func (x)
(list
(/ (^ (- x 1) 2) 2)
(- x 1)
(repeat 1 (length x))))

(defun james-func (x)
(list
(/ (^ (log x) 2) 2)
(/ (log x) x)
(/ (- 1 (log x)) (* x x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Image Analysis
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun image-analysis (x &key (image nil) (anti-image nil)
                         (image-dispersion nil) (anti-image-dispersion nil)
                         (determination t))
"Args: (x)
This computes the images and anti-images and their dispersions."
(let* (
       (c (matmult (transpose x) x))
       (d (inverse c))
       (s (/ (diagonal d)))
       (ai (matmult(matmult x d) (diagonal s)))
       (im (- x ai))
       (ca (covariance-matrix ai))     
       (ci (covariance-matrix im))
       (rt (list im ai ci ca s))
       )
(if image (print-matrix im))
(if anti-image (print-matrix ai))
(if image-dispersion (print-matrix ci))
(if anti-image-dispersion (print-matrix ca))
(if determination (print s))
nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Minimum Trace Factor Analysis
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minimum-trace-factor-analysis
  (c &key (epsilon 1e-6) (nmax 100) (verbose t))
(let* (
      (nn (first (array-dimensions c)))
      (mm (iseq nn))
      (uu (make-array nn :initial-element 0))
      (vv (make-array nn :initial-element 0))
      (cc (copy-array c))
      (ff 0)
      (fg 0)
      (it 0)
      )
(loop
(dotimes (i nn)
(let* (
      (kk (which (/= i mm)))
      (c1 (aref cc i i))
      (c2 (select cc i kk))
      (c3 (moore-penrose-inverse (select cc kk kk)))
      (th (- c1 (sum (* c3 (outer-product c2 c2)))))
      )
(setf (aref vv i) th)
))
(setf vv (/ vv nn))
(incf uu vv)
(decf cc (diagonal vv))
(setf fg (sum uu))
(if verbose
    (format t "~d ~,8f ~,8f ~%" it ff fg))
(if (or (< (- fg ff) epsilon) (= it nmax))
    (return (list uu (- 1 (/ fg (sum (diagonal c))))))
  (progn (setf ff fg) (incf it)))
)))

(defun minimum-trace-factor-analysis-a-la-bentler 
  (c &key (epsilon 1e-6) (nmax 100) (verbose t))
(let* (
      (nn (first (array-dimensions c)))
      (mm (iseq nn))
      (fm (identity-matrix nn))
      (ff (sum (diagonal c)))
      (fg (sum (diagonal c)))
      (it 0)
      )
  (loop
   (dotimes (i nn)
     (let* (
           (kk (which (/= i mm)))
           (s1 (aref c i i))
           (s2 (make-array (1- nn) :displaced-to (select c kk i)))
           (f3 (select fm kk mm))
           (fh (- (/ (matmult (transpose f3) s2) s1)))
           (fs (sum (* fh fh)))
           )
       (setf (select fm i mm) (make-array (list 1 nn) :displaced-to
                                          (if (>= fs 1) fh
                                            (/ fh (sqrt fs)))))
             ))
     (setf fg (sum (* c (matmult fm (transpose fm)))))
     (if verbose
         (format t "~d ~,8f ~,8f ~%" it ff fg))
     (if (or (< (abs (- ff fg)) epsilon) (= it nmax))
         (return (- 1 (/ fg (sum (diagonal c)))))
       (progn (setf ff fg) (incf it)))
     )))
     
(defun weighted-minimum-trace-factor-analysis (c))     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bunch of utilities, some of general use.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replace-diagonal (a b)
"Args: (a b)
Takes a rectangular matrix a, and replaces
the diagonal elements with the elements
of a sequence b. Used only for this
side-effect."
(let (
     (k (iseq (min (array-dimensions a))))
     )
(mapcar #'(lambda (i)
            (setf (aref a i i) (elt b i))) k)
))

(defun matrix-function (a f)
"Args: (a f)
If the eigen-decomposition of a is KDK',
then this function returns Kf(D)K'."
(let* (
       (x (eigen a))
       (g (first x))
       (n (length g))
       (h (funcall f g))
       (y (second x))
       )
(apply #'+ (mapcar #'(lambda (i)
                        (* (elt h i) (outer-product (elt y i) (elt y i))))
                    (iseq n)))
))

(defun moore-penrose-function (x)
(if (numberp x) (if (= 0 x) 1 (/ x))
(map-elements #'(lambda (x) (if (= 0 x) 1 (/ x))) x))
)

(defun moore-penrose-inverse (c)
(matrix-function c #'moore-penrose-function))

(defun make-factor-covariance-matrix (n m p)
(let (
     (f (make-array (list n p) :displaced-to
                    (coerce (normal-rand (* n p)) 'vector)))
     (a (make-array (list p m) :displaced-to
                    (coerce (round (uniform-rand (* p m))) 'vector)))
     (z (make-array (list n m) :displaced-to
                       (coerce (normal-rand (* n m)) 'vector)))
     )
(covariance-matrix (+ z (matmult f a)))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This last part from "On Lisp" by Paul Graham, page 214-215
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro abbrev (short long)
`(defmacro ,short (&rest args)
   `(,',long ,@args)))

(defmacro abbrevs (&rest names)
`(progn
  ,@(mapcar #'(lambda (pair)
                `(abbrev ,@pair))
            (group names 2))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                      (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
             
(abbrevs mlfa maximum-likelihood-factor-analysis
         teid truncated-eigen-decomposition
         lsfa least-squares-factor-analysis
         faas factor-analysis-a-la-swain
         mtfa minimum-trace-factor-analysis
         mtfb minimum-trace-factor-analysis-a-la-bentler
         wmtf weighted-minimum-trace-factor-analysis
         iman image-analysis
         mfcm make-factor-covariance-matrix)
        
