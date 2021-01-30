;;;; Course project:  STA450/2102S, University of Toronto, Spring 1993
;;;; Instructor: N. Reid
;;;; Student: J. Pogue
;;;; Project Title:  
;;;;
;;;; Repeated Measures with missing data using the EM Algorithm
;;;; based on the article, Maximum Likelihood Computations With Repeated Measures:
;;;; Application of the EM Algorithm
;;;; Laird, N., Lange, N., & Stram, D.
;;;; JAMA, 1987.


;;;;
;;;; First a repeated measures data prototype is created from the regression-model-proto.
;;;; The data slots :X and :Y can be easily modified for this data
;;;;

(defproto repeated-measures-proto '(title degree alpha theta) () regression-model-proto)

;;;;
;;;; Here, slots that are not needed are removed from the new proto.
;;;; Methods are written for the used slots.
;;;;

(send repeated-measures-proto :delete-slot 'residual-sum-of-squares)
(send repeated-measures-proto :delete-slot 'total-sum-of-squares)
(send repeated-measures-proto :delete-slot 'included)
(send repeated-measures-proto :delete-slot 'weights)
(send repeated-measures-proto :delete-slot 'basis)
(send repeated-measures-proto :delete-slot 'sweep-matrix)
(send repeated-measures-proto :delete-slot 'intercept)
(send repeated-measures-proto :delete-slot 'case-labels)

(defmeth repeated-measures-proto :title (&optional (title nil set))
    (if set (setf (slot-value 'title) title)
        (setf (slot-value 'title) "A Repeated Measures Data Set"))
    (slot-value 'title))

;;;;
;;;; Both :X and :Y slots make sure that new data are arrays.
;;;; This proto assumes a data set with one variable which is repeatedly measured
;;;; over a number of time-points. Missing data is allowed.
;;;; :X is a single 1 by T (time) array
;;;; :Y is a row array N by T, where each row contains the dependent measurements 
;;;; for a single subject
;;;;

(defmeth repeated-measures-proto :x (&optional (x nil set))
    (if set (setf (slot-value 'x) 
                  (if (arrayp x) x (make-array (list 1 (length x))
                              :initial-contents (list (iseq 1 (length x)))))))
        (slot-value 'x))

(defmeth repeated-measures-proto :y (&optional (y nil set))
    (if set (setf (slot-value 'y) 
                  (if (arrayp y) y (apply #'bind-rows y))))
        (slot-value 'y))
;;;;
;;;; :DEGREE holds the number of trend analysis parameters for the model.
;;;; The program assumes that an intercept is always included and the default (1)
;;;; fits a linear term. Other terms that can be included are quadratic (2),
;;;; cubic (3), etc..
;;;;

(defmeth repeated-measures-proto :degree (&optional (degree nil set))
    (if set (setf (slot-value 'degree) degree))
    (slot-value 'degree))
;;;;
;;;; :ALPHA holds the final ML parameter estimates
;;;;
;;;;

(defmeth repeated-measures-proto :alpha (&optional (alpha nil set))
    (if set (setf (slot-value 'alpha) alpha))
    (slot-value 'alpha))

;;;;
;;;; :THETA holds the final ML error and covariance matrix 
;;;; parameter estimates. Here the covariance matrix is assumed to have an
;;;; arbitrary structure. A more complete version of this program would
;;;; allow for other types of structure such as compound symmetry, AR(1),
;;;; etc.
;;;;

(defmeth repeated-measures-proto :theta (&optional (theta nil set))
    (if set (setf (slot-value 'theta) theta))
    (slot-value 'theta))
;;;;
;;;;
;;;; Misc Function which are used by the main function GROWTH-EM
;;;;
;;;;

;;;;
;;;; TIME-MISSING
;;;; Takes the :Y and :X values and produces two row vectors (yi xi)
;;;; containing only those non-missing values, where a missing measurement
;;;; is indicated by a -1 in the y row array. These two array represent a single
;;;; subject. This function takes a copy of the x array (time) and copies to it any -1
;;;; values from the y (response) array, in the matching time point. Then -1 values 
;;;; are stripped off both arrays. This lists missing and present contain the time points
;;;; missing and present for that individual.
;;;;

(defun time-missing (yterm xterm jk)
    (def missing '(-1))
    (def present '(-1))
    (dotimes (j (select (array-dimensions yterm) 1))
                 (if (= (select yterm jk j) -1)
                     (setf missing 
                           (concatenate 'list missing (list j)))
                     (setf present 
                           (concatenate 'list present (list j)))))
     (setf missing (remove -1 missing))
     (setf present (remove -1 present))
     (def yi (select yterm jk present))
     (def xi (select xterm 0 present)))

;;;;
;;;; Y-MISS is used to calculate the total number of non-missind observations 
;;;; in the data set from the :Y data. A row array is produced with all missing values stripped
;;;; off. This is used to calculate the sum-of-squares of the responses, which is
;;;; used on the diagonal of the covariance matrix as an initial estimate.
;;;;

(defun y-miss (ymat)
    (def tot-pres '(-1))
      (dotimes (j1 (select (array-dimensions ymat) 1))
               (if (not (= (select ymat 0 j1) -1))
                   (setf tot-pres (concatenate 'list 
                                               tot-pres (list j1)))))
    (setf tot-pres (remove -1 tot-pres))
    (def y-pres (select ymat 0 tot-pres)) y-pres)

;;;;
;;;; DIAG produces an identity matrix of size dimen x dimen
;;;;
;;;;

(defun diag (dimen)
    (setf s-mat (make-array (list dimen dimen) :initial-element 0))
    (dotimes (j2 (select (array-dimensions s-mat) 0))
             (setf (select s-mat j2 j2) 1)) s-mat)

;;;;
;;;; GOOD-ENOUGH-VECTOR-P
;;;; The DIFF array contains the difference between old and new THETA values.
;;;; If all the values contained in it are less than a value the function is true.
;;;; Here the value is 100, which will cause the program to converge quickly and
;;;; therefore, is good to demonstrate the program. For real use, a value of .001
;;;; or less should be used.
;;;;

(defun good-enough-vector-p (diff)
    (def mark (make-array (list (select (array-dimensions diff) 1) 1)
                           :initial-element 0))
    (dotimes (j3 (select (array-dimensions diff) 1))
             (if (> (select diff j3 0) 100)
                 (setf (select mark j3 0) 1)))
    (= (sum mark) 0))

;;;;
;;;; MAKE-X 
;;;; Takes the x array of times, adds a column of ones and powers of x to match
;;;; degree of model (e.g. (^ x 2)
;;;;

(defun make-x (xr degree)
     (def xc (transpose xr))
     (if (> degree 1)
         (dotimes (j4 (- degree 1))
                  (setf xc (bind-columns xc (^ (transpose xr) (+ j4 2))))))
     (setf xc (bind-columns (repeat 1 (select (array-dimensions xc) 0)) xc)) xc)

;;;;
;;;; TRACE-M
;;;; Sums the values on the diagonal of a square matrix
;;;;

(defun trace-m (m)
     (def trISW 0)
     (dotimes (j5 (select (array-dimensions m) 0))
              (def trISW (+ trISW (select m j5 j5)))) trISW)
;;;;
;;;; TRACE-M-LIST
;;;; Produces a list of values on diagonal of square matrix
;;;;

(defun trace-m-list (m)
     (def trlist '(-1))
     (dotimes (j5 (select (array-dimensions m) 0))
              (def trlist (concatenate 'list trlist (list (select m j5 j5))))) (rest trlist))

;;;;
;;;;
;;;; GROWTH-EM
;;;; 
;;;;

(defun growth-EM (x y &key (degree 1))
;;;;
;;;; x - time points, if not in an array will be converted when sent to proto
;;;; y - responses for each time point, each row represents one subject
;;;;     missing values are coded as -1
;;;; :DEGREE - intercept is assumed and a linear component is the default (1)
;;;;           can include quadratic (2), cubic (3), etc.
;;;;
;;;; First, the data is sent to the proto
;;;;    
    (def model (send repeated-measures-proto :new))
    (send model :x x)
    (send model :y y)
    (send model :degree degree)
    (send model :title "MLE and EM algorithm")
;;;;
;;;; Next, a number of variables are initialized
;;;; 1. The covariance matrix is a identity matrix with sy2 on the diagonal, to begin with
;;;; 2. Z is a matrix containing the design on time, meaning it is an identity matrix
;;;;    of dimension time
;;;; 3. The error is assumed to be 0 to start
;;;; 4. alpha-current (model coeficients) are calculated using default values of covariance
;;;;    matrix and error term, these are recalculated. New values are plugged back in, and the process is 
;;;;    iterated until the theta (columns of error and covariance parameters) converges.
;;;;
    (let* ((time (select (array-dimensions (send model :y)) 1))
           (subj (select (array-dimensions (send model :y)) 0))
           (y-row (make-array (list 1 (* time subj)) 
                              :displaced-to (send model :y)))
           (y-pres (y-miss y-row))
           (nobs (select (array-dimensions y-pres) 1))
           (sy2 (/ (sum (^ (- y-pres (mean y-pres)) 2))
                       (- nobs 1)))
           (D (diag time))
           (D-current (* D sy2))
           (Z (diag time))
           (error 0)
           (error-current 0)
           (i (diag time))
           (deg (send model :degree))
           (XWX-current (make-array (list (+ deg 1) (+ deg 1))
                                  :initial-element 0))
           (XWY-current (make-array (list (+ deg 1) 1)
                           :initial-element 0))
           (alpha-current (make-array (list (+ deg 1) 1) 
                                  :initial-element 0))
           (theta (make-array (list (+ (^ time 2) 1) 1) 
                                  :initial-element 10000000))
           (theta-current (bind-rows '(0) (make-array (list (^ time 2) 1)
                                  :displaced-to D-current)))
           (diff (abs (- theta-current theta))))
;;;;
;;;; Set up large DO loop with test for convergence and instructions to
;;;; print results.
;;;;
    (do* ((j6 1 (+ j6 1)))
         ((good-enough-vector-p diff)
          (send model :theta theta-current)
          (send model :alpha alpha-current)
          (format t "This is a ~a~%" (send model :title))
          (format t "Parameter estimates are ~g~%" (send model :alpha))
          (format t "Standard errors are ~g~%" 
                    (trace-m-list (sqrt (inverse (select store 0)))))
          (format t "Error & Covariance matrix elements ~g~%" 
          (send model :theta)))
         (setf XWX-current (* XWX-current 0))
         (setf XWY-current (* XWY-current 0))
         (setf D D-current)
         (setf error error-current)
         (setf theta theta-current)
;;;;
;;;; Compute alpha-current  in terms of default values of theta
;;;; For each subject the arrays xi, yi, zi, wi are calculated containing only non-missing
;;;; time points.
;;;;
         (def t1 (dotimes (j7 subj store)                      
                      (time-missing (send model :y y) (send model :x x) j7)
                      (let* ((tni (select (array-dimensions yi) 1))
                             (zi (select z present (iseq 0 (- time 1))))
                             (ii (diag tni))
                             (sigmai (+ (* error ii)
                                        (matmult (matmult zi D) (transpose zi))))
                             (wi (inverse sigmai))
                             (XX (make-x xi deg))
                             (XWX-current (+ XWX-current 
                                            (matmult (matmult (transpose XX) wi) XX)))
                             (XWY-current (+ XWY-current 
                                            (matmult (matmult (transpose XX) wi) (transpose yi)))))
                         (def store (compound-data-seq (list XWX-current XWY-current))))))
               (setf alpha-current (matmult (inverse (select t1 0)) (select t1 1)))
               (def errori 0)
               (def Di (make-array (list time time) 
                                         :initial-element 0))
;;;;
;;;; Re-calculate theta-current to be tested against theta
;;;; Again, for each subject the arrays xi, yi, zi, wi are calculated containing only non-missing
;;;; time points.
;;;; An improvement to the program would store these values from the last DOTIMES loop. However,
;;;; this might take large amount of memory for a big sample.
;;;;
         (def t2 (dotimes (j8 subj store2)
                     (time-missing (send model :y y) (send model :x x) j8)
                     (let* ((tni2 (select (array-dimensions yi) 1))
                            (zi2 (select z present (iseq 0 (- time 1))))
                            (ii2 (diag tni2))
                            (sigmai2 (+ (* error ii2)
                                (matmult (matmult zi2 D) (transpose zi2))))
                            (wi2 (inverse sigmai2))
                            (XX2 (make-x xi deg))
                            (ri (- yi (matmult XX2 alpha-current)))
                            (bi (matmult (matmult (matmult D (transpose zi2))
                                          wi2) (transpose ri)))
                            (fir (- (transpose ri) (matmult zi2 bi)))
                            (sec (- ii2 (* error wi2)))
                            (errori (+ errori (+ (matmult (transpose fir) fir)
                                        (* error (trace-m sec)))))
                            (Di (+ Di (/ (+ (matmult bi (transpose bi)) 
                                   (matmult D (- i (matmult 
                                       (matmult (matmult (transpose zi2) wi2) zi2) D)))) tni2))))
                         (def store2 (compound-data-seq (list errori Di))))))
         (setf error-current (/ (sum (select t2 0)) nobs))
         (setf D-current  (select t2 1))         
         (setf theta-current (bind-rows (list error-current) (make-array (list (^ time 2) 1)
                                         :displaced-to D-current)))
         (setf diff (abs (- theta-current theta))))))

