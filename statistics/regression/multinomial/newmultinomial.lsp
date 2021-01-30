;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;  Multinomial Regression Function
;;
;;  Args: pred crit &key ord-unord dist
;;  
;;  
;;  crit - The Y's in the form of a list (nx1)
;;  pred - The X's in the form of a matrix (nxm).
;;  ord-unord - 'ord or 'unord depending on whether categories are to bo 
;;              ordered or not.
;;  dist - The distribution to use it ord-unord is 'ord
;;         Types of distributions are: 'logistic, 'normal, 'cacuhy, 'laplace,
;;                                     or 'fisher-tippett.
;;  Example function Calls:  
;;  (def mymulti (multinomial-regression response covariates :ord-unord 'ord
;;                              :dist 'normal))
;;
;;  or for a dialog:
;;  (def mymulti (multinomial-regression response covariates))
;;
;;  To get parameter estimates and/or standard errors send the following
;;  messages to your prototype:
;;
;;  (send mymulti :parameter-estimates)
;;  - returned is a list of 2 lists.  The first is the alphas, the second
;;    is the betas.
;;
;;  (send mymulti :parameter-standard-errors)
;;  - returned is a list of 2 lists.  The first is the standard-errors
;;    of the alphas, the second is the standard-errors of the betas.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defproto multinomial-proto '(pred crit q0 type cdf pdf idf
                              parameter-estimates parameter-standard-errors))


(defmacro normal-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(normal-accessor :pred pred multinomial-proto)
(normal-accessor :crit crit multinomial-proto)
(normal-accessor :type type multinomial-proto)
(normal-accessor :cdf cdf multinomial-proto)
(normal-accessor :pdf pdf multinomial-proto)
(normal-accessor :idf idf multinomial-proto)
(normal-accessor :parameter-standard-errors 
                  parameter-standard-errors multinomial-proto)
(normal-accessor :parameter-estimates parameter-estimates multinomial-proto)



(defmeth multinomial-proto :isnew (pred crit &key ord-unord dist)
  (send self :pred pred)
  (send self :crit crit)
  (if ord-unord
      (progn
        (if (equalp ord-unord 'ord)
            (send self :collect-cdf (cond ((equalp dist 'logistic) 0)
                                          ((equalp dist 'normal) 1)
                                          ((equalp dist 'cauchy) 2)
                                          ((equalp dist 'laplace) 3)
                                          ((equalp dist 'fisher-tippett) 4)))
            (progn
             (let ((result (send self :umml)))
               (send self :parameter-estimates result)))))
      (send self :pick-type))
  self)


(defun mycoerce (mat)
    (mapcar #'(lambda (x) (coerce x 'list)) (row-list mat)))


(defmeth multinomial-proto :pick-type ()
 (let* (
        (type-header (send text-item-proto :new "Type of analysis"))
        (ok (send button-item-proto :new "OK"
               :action #'(lambda () (send self :type (send type :value))
                                    (send self :collect-type))))
        (cancel (send button-item-proto :new "Cancel"
               :action #'(lambda () (send type-dialog :close))))
        (type (send choice-item-proto :new
                     (list "Ordered" "Unordered")))
        (type-dialog  (send dialog-proto :new
                         (list
                          type-header type (list ok cancel)
                          ))))))

(defmeth multinomial-proto :pick-cdf ()
 (let* ((cdf-header (send text-item-proto :new "Choose your cdf"))
        (cancel (send modal-button-proto :new "Cancel"))
        (cdf (send choice-item-proto :new 
              (list "Logit" "Probit" "Cauchy" "Laplace"
                    "Fisher-Tippett") :value 0))
        (ok (send modal-button-proto :new "OK"
             :action #'(lambda () (send self :collect-cdf (send cdf :value)))))
        (cdf-dialog (send modal-dialog-proto :new
                         (list
                          (list (list cdf-header cdf))
                          (list ok cancel)
                          ))))
(send cdf-dialog :modal-dialog)
))


(defmeth multinomial-proto :collect-type ()
  (let ((k (send self :type)))
    (cond ((= k 0) (send self :pick-cdf))
          ((= k 1) (send self :umml)))))





(defmeth multinomial-proto :collect-cdf (k)
(cond 
  ((= k 0) (send self :cdf #'(lambda (x) (logistic-cdf x)))
           (send self :pdf #'(lambda (x) (logistic-dens x)))
           (send self :idf #'(lambda (x) (logistic-quant x))))
  ((= k 1) (send self :cdf #'(lambda (x) (normal-cdf x)))
           (send self :pdf #'(lambda (x) (normal-dens x)))
           (send self :idf #'(lambda (x) (normal-quant x))))
  ((= k 2) (send self :cdf #'(lambda (x) (cauchy-cdf x)))
           (send self :pdf #'(lambda (x) (cauchy-dens x)))
           (send self :idf #'(lambda (x) (cauchy-quant x))))

  ((= k 3) (send self :cdf #'(lambda (x) (laplace-cdf x)))
           (send self :pdf #'(lambda (x) (laplace-dens x)))
           (send self :idf #'(lambda (x) (laplace-quant x))))

  ((= k 4) (send self :cdf #'(lambda (x) (fisher-tippett-cdf x)))
           (send self :pdf #'(lambda (x) (fisher-tippett-dens x)))
           (send self :idf #'(lambda (x) (fisher-tippett-quant x)))))
(let* ((sol (send self :omml))
       (ord (send self :hit-ord (first sol))))))




(defmeth multinomial-proto :omml ()
(let* ((pred (send self :pred))
       (crit (send self :crit))
       (cdf (send self :cdf))
       (pdf (send self :pdf))
       (par0 (send self :init-ord))
       (n (length crit))
       (l (- (length (remove-duplicates crit)) 1))
       (mymax (newtonmax #'(lambda (par)
         (let* (
                (alp (select par (iseq l)))
                (bet (select par (+ l (iseq (- (length par) l)))))
                (h (matmult pred bet))
                (j (outer-product h alp (function +)))
                (s (funcall cdf j))
                (v (bind-columns (repeat 0 n) s (repeat 1 n)))
                (u (mapcar (function difference) (mycoerce v)))
                (z (matrix-select u crit))
                (a (sum (log z)))
                )
#|
            (if (= ana 1) 
                (let* ((b (funcall pdf j))
                       (w (bind-columns (repeat 0 n) b (repeat 0 n)))
                       (y (mapcar (function difference) (mycoerce w)))
                       (x (matrix-select y crit))
                       (g (double-select b crit))
                       (h (concatenate 'vector 
                              (matmult (/ z) g) (matmult (/ x z) pred))))
                   (setf a (list a h))))
|#
          a)) par0 :return-derivs 't)))
   (send self :parameter-estimates (list
        (select (first mymax) (iseq l))
        (select (first mymax) (iseq l (1- (length (first mymax)))))))
   (send self :parameter-standard-errors (list
     (select (sqrt (- (diagonal (inverse (first (last mymax)))))) (iseq l))
     (select (sqrt (- (diagonal (inverse (first (last mymax))))))
              (iseq l (1- (length (first mymax)))))))
   mymax))

     




(defmeth multinomial-proto :init-ord ()
"We compute an initial estimate for the 
ordered multinomial regression model."
(let* (
       (pred (send self :pred))
       (crit (send self :crit))
       (idf (send self :idf))
       (n (length crit))
       (l (- (length (remove-duplicates crit)) 1))
       (m (elt (array-dimensions pred) 1))
       )
(append (funcall idf (/ (cumsum (select (marginals crit) (iseq l))) n))
        (repeat 0 m))))

(defmeth multinomial-proto :hit-ord (pars)
"Args: pars
PARS is a vector of arguments containing the
cutting points and the regression coefficients."
(let* (
      (pred (send self :pred))
      (crit (send self :crit))
      (cdf (send self :cdf))
      (n (length crit))
      (l (- (length (remove-duplicates crit)) 1))
      (alp (select pars (iseq l)))
      (bet (select pars (+ l (iseq (- (length pars) l)))))
      (h (matmult pred bet))
      (s (funcall cdf (outer-product h alp (function +))))
      (v (bind-columns (repeat 0 n) s (repeat 1 n)))
      (u (mapcar (function difference) (mycoerce v)))
      (w (mapcar (function max) u))
      (z (matrix-select u crit))
      (y (if-else (= w z) (repeat 1 n) (repeat 0 n)))
      )
(list (exp (/ (sum (log z)) n)) (/ (sum y) n))
))



(defmeth multinomial-proto :umml ()
"Args: pars
PARS is a list of arguments containing
the intercepts and the regression coefficients."
(let* ((pred (send self :pred))
       (crit (send self :crit))
       (n (length crit))
       (m (elt (array-dimensions pred) 1))
       (l (length (remove-duplicates crit)))
       (q0 (combine (/ (marginals crit) n) (repeat 0 (* m l))))
       (mymax (newtonmax #'(lambda (pars) 
              (let* ((alp (select pars (iseq l)))
                     (bet (select pars (+ l (iseq (- (length pars) l)))))
                     (tet (transpose (make-array (list l m) :displaced-to
                          (coerce bet 'vector))))
                     (h (+ (matmult pred tet) (outer-product (repeat 1 n) alp)))
                     (u (mycoerce (row-percentage (exp h)))))
                 (sum (log (matrix-select u crit))))) q0 :return-derivs 't)))
   (send self :parameter-estimates (list 
        (select (first mymax) (iseq l))
        (select (first mymax) (iseq l (1- (length (first mymax)))))))
   (send self :parameter-standard-errors (list 
      (select (sqrt (- (diagonal (inverse (first (last mymax)))))) (iseq l))
      (select (sqrt (- (diagonal (inverse (first (last mymax))))))
              (iseq l (1- (length (first mymax)))))))))
      




(defun row-percentage (x)
(let ( 
     (r (mapcar #'sum (row-list x)))
     (n (length (elt (mycoerce x) 0)))
     )
(/ x (outer-product r (repeat 1 n)))
))
       
(defun matrix-select (x y)
"Args: x y
For each k we select the y(k)-th element in row k
of x. If x is n x m, then k=1,...,n and
1<=y(k)<=m."
(let* ((n (length y))
       (z (repeat 0 n)))
(dotimes (i n)
         (setf (select z i) (select (elt x i) (elt (- y 1) i))))
z))

(defun double-select (x y)
(let* (
       (n (length y))
       (unique-y (sort-data (remove-duplicates y)))
       (m (length unique-y))
       (z (make-array (list n (- m 1)) :initial-element 0))
      )
(dotimes (i n)
(let ((k (position (elt y i) unique-y)))
(cond ((= 0 k)
         (setf (aref z i 0) (select x i 0)))
      ((= (- m 1) k)
         (setf (aref z i (- m 2)) (- (select x i (- m 2)))))
      (t 
         (setf (aref z i k) (select x i k))
         (setf (aref z i (- k 1)) (- (select x i (- k 1))))))))
z))

(defun logistic-cdf (x)
"Args: x
Computes standard logistic cdf at x."
(/ (+ 1 (exp (- x)))))

(defun logistic-dens (x)
"Args: x
Computes standard logistic density at x."
(let (
      (y (exp (- x)))
      )
(/ y (^ (+ 1 y) 2))
))

(defun logistic-quant (x)
"Args: x
Compute standard logistic quantile function at x,
i.e. the logit of x."
(log (/ x (- 1 x)))
)

(defun fisher-tippett-cdf (x)
"Args: x
Computes type I extreme value cdf at x."
(- 1 (exp (- (exp x)))))

(defun fisher-tippett-dens (x)
"Args: x
Computes type I extreme value density at x."
(let (
     (y (exp x))
     )
(* y (exp (- y)))
))

(defun fisher-tippett-quant (x)
"Args: x
Computes type I extreme value quantile function at x."
(log (- (log (- 1 x))))
)

(defun laplace-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'laplace-dens x mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (/ (exp (- (abs (/ (- x mu) sigma)))) (* 2 sigma)))
    )
  )


(defun laplace-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'laplace-cdf x mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (let ((y (/ (exp (- (abs (/ (- x mu) sigma)))) 2)))
        (if (<= x mu) y (- 1 y))))
    )
  )

(defun laplace-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'laplace-quant p mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1"))
      (if (<= p .5)
          (+ mu (* sigma (log (* 2 p))))
        (- mu (* sigma (log (* 2 (- 1 p)))))))
    )
  )



(defun marginals (x)
"Args: first
FIRST is a sequence of numbers or strings.
Different entries are sorted and counted."
(mapcar #'sum (column-list (make-indicator x))))

(defun make-indicator (x)
"Args: sequence
Elements of SEQUENCE are either numbers or strings. 
Returns a dummy with sorted category values."
(let* (
      (y (sort-data (remove-duplicates x :test 'equal)))
      (m (length y))
      (n (length x))
      (a (make-array (list n m) :initial-element 1))
      (b (make-array (list n m) :initial-element 0))
      )
(if-else (outer-product x y #'equal) a b)
))

(defun expand-table (x)
"Args: table value
The rows of the table X correspond with values of the treatment variable,
the columns are the reponse categories. This is expanded
to a categorical predictor and criterion variable."
(let (
     (r (row-sum x))
     (n (elt (array-dimensions x) 0))
     (m (elt (array-dimensions x) 1))
     (p nil)
     (c nil)
     )
(dotimes (i n)
(setf p (concatenate 'vector p (repeat i (elt r i))))
(dotimes (j m)
(setf c (concatenate 'vector c (repeat j (aref x i j))))))
(list p c)
))

(defun row-sum (x)
  (mapcar #'sum (row-list x)))


(defun multinomial-regression (pred crit &key ord-unord dist)
  (let ((proto (send multinomial-proto :new pred crit :ord-unord ord-unord
                                       :dist dist)))
   proto))


(setf pr (make-array '(10 2) :displaced-to
      (coerce (normal-rand 20) 'vector)))

;; Criterion
(setf cr '(1 2 3 4 1 2 3 4 2 3))

;; Starting Values of intercepts and regression coefficients
;(setf myq0 '(1 2 3 4 1 2 2 1 .1 .5 -.5 .3))

#|
;; Regressors
(setf pred (make-array '(10 2) :displaced-to
      (coerce (normal-rand 20) 'vector)))

;; Criterion
(setf crit '(1 2 3 4 1 2 3 4 2 3))

;; Starting Values of intercepts and regression coefficients
(setf q0 '(1 2 3 4 1 2 2 1 .1 .5 -.5 .3))

|#


;  (def mymulti (multinomial-regression pr cr))





