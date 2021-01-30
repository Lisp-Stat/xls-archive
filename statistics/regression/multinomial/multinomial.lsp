(setf pred (make-array '(10 2) :displaced-to
      (coerce (normal-rand 20) 'vector)))
(setf crit '(1 2 3 4 1 2 3 4 2 3))
(setf q0 '(1 2 3 4 1 2 2 1 .1 .5 -.5 .3))



(defun mycoerce (mat)
    (mapcar #'(lambda (x) (coerce x 'list)) (row-list mat)))


(defun pick-type ()
  (let (
        (type-header (send text-item-proto :new "Pick the type of analysis"))
        (deri-header (send text-item-proto :new "First derivatives"))
        (ok (send modal-button-proto :new "OK"
               :action #'collect-type))
        (cancel (send modal-button-proto :new "Cancel"))
        )
    (setf type (send choice-item-proto :new
                     (list "Ordered" "Unordered")))
    (setf deri (send choice-item-proto :new
                     (list "Numerical" "Analytical")))
    (setf type-dialog  (send modal-dialog-proto :new
                         (list
                          (list
                          (list type-header type)
                          (list deri-header deri))
                          (list ok cancel)
                          )))
(send type-dialog :modal-dialog)
))

(defun pick-cdf ()
  (let (
        (cdf-header (send text-item-proto :new "Choose your cdf"))
        (ok (send modal-button-proto :new "OK"
               :action #'collect-cdf))
        (cancel (send modal-button-proto :new "Cancel"))
        )
(setf cdf (send choice-item-proto :new 
              (list "Logit" "Probit" "Cauchy" "Laplace"
                    "Fisher-Tippett") :value 0))
(setf cdf-dialog (send modal-dialog-proto :new
                         (list
                          (list (list cdf-header cdf))
                          (list ok cancel)
                          )))
(send cdf-dialog :modal-dialog)
))

(defun collect-type ()
  (let (
        (k (send type :value))
        )
    (setf ana (send deri :value))
    (cond 
           ((= k 1) (newtonmax #'umml q0))
           ((= k 0) (send type-dialog :close) (pick-cdf))
           )
    )
  )

(defun collect-cdf ()
  (let
      (
      (k (send cdf :value))
      )
(cond 
  ((= k 0) (defun cdf (x) (logistic-cdf x))
           (defun pdf (x) (logistic-dens x))
           (defun idf (x) (logistic-quant x)))
  ((= k 1) (defun cdf (x) (normal-cdf x))
           (defun pdf (x) (normal-dens x))
           (defun idf (x) (normal-quant x)))
  ((= k 2) (defun cdf (x) (cauchy-cdf x))
           (defun pdf (x) (cauchy-dens x))
           (defun idf (x) (cauchy-quant x)))
  ((= k 3) (defun cdf (x) (laplace-cdf x))
           (defun pdf (x) (laplace-dens x))
           (defun idf (x) (laplace-quant x)))
  ((= k 4) (defun cdf (x) (fisher-tippett-cdf x))
           (defun pdf (x) (fisher-tippett-dens x))
           (defun idf (x) (fisher-tippett-quant x)))
)
(send cdf-dialog :close)
;(break)
(setf sol (newtonmax #'omml (init-ord) :return-derivs t))
(print sol)
(hit-ord (first sol))
))

(defun omml (pars)
(let* (
      (n (length crit))
      (l (- (max crit) 1))
      (alp (select pars (iseq l)))
      (bet (select pars (+ l (iseq (- (length pars) l)))))
      (h (matmult pred bet))
      (j (outer-product h alp (function +)))
      (s (cdf j))
      (v (bind-columns (repeat 0 n) s (repeat 1 n)))
      (u (mapcar (function difference) (mycoerce v)))
      (z (matrix-select u crit))
      (a (sum (log z)))
      )
  (if (= ana 1) (let* 
             (
              (b (pdf j))
              (w (bind-columns (repeat 0 n) b (repeat 0 n)))
              (y (mapcar (function difference) (mycoerce w)))
              (x (matrix-select y crit))
              (g (double-select b crit))
              (h (concatenate 'vector 
                              (matmult (/ z) g) (matmult (/ x z) pred)))
              )
             (setf a (list a h))))
a))

(defun init-ord ()
"We compute an initial estimate for the 
ordered multinomial regression model."
(let (
      (n (length crit))
      (l (- (max crit) 1))
      (m (elt (array-dimensions pred) 1))
      )
(append (idf (/ (cumsum (select (marginals crit) (iseq l))) n))
        (repeat 0 m))
))

(defun hit-ord (pars)
"Args: pars
PARS is a vector of arguments containing the
cutting points and the regression coefficients."
(let* (
      (n (length crit))
      (l (- (max crit) 1))
      (alp (select pars (iseq l)))
      (bet (select pars (+ l (iseq (- (length pars) l)))))
      (h (matmult pred bet))
      (s (cdf (outer-product h alp (function +))))
      (v (bind-columns (repeat 0 n) s (repeat 1 n)))
      (u (mapcar (function difference) (mycoerce v)))
      (w (mapcar (function max) u))
      (z (matrix-select u crit))
      (y (if-else (= w z) (repeat 1 n) (repeat 0 n)))
      )
(list (exp (/ (sum (log z)) n)) (/ (sum y) n))
))


(defun umml (pars)
"Args: pars
PARS is a list of arguments containing
the intercepts and the regression coefficients."
(let* (
      (n (length crit))
      (l (max crit))
      (m (elt (array-dimensions pred) 1))
      (alp (select pars (iseq l)))
      (bet (select pars (+ l (iseq (- (length pars) l)))))
      (tet (transpose (make-array (list l m) :displaced-to
           (coerce bet 'vector))))
      (h (+ (matmult pred tet) (outer-product (iseq n) alp #'+)))
      (u (coerce (row-percentage (exp h)) 'list))
      )
(sum (log (matrix-select u crit)))
))

(defun row-percentage (x)
(let ( 
     (r (mapcar #'sum (row-list x)))
     (n (length (elt (coerce x 'list) 0)))
     )
(/ x (outer-product r (repeat 1 n)))
))
       
(defun matrix-select (x y)
"Args: x y
For each k we select the y(k)-th element in row k
of x. If x is n x m, then k=1,...,n and
1<=y(k)<=m."
(let* (
      (n (length y))
      (z (repeat 0 n))
      )
(dotimes (i n)
         (setf (select z i) (select (elt x i) (elt (- y 1) i))))
z))

(defun double-select (x y)
(let*  (
       (n (length y))
       (m (max y))
       (z (make-array (list n (- m 1)) :initial-element 0))
       )
(dotimes (i n)
(let (
     (k (elt y i))
     )
(cond ((= 1 k) (setf (aref z i 0) (select x i 0)))
      ((= m k) (setf (aref z i (- m 2)) (- (select x i (- m 2)))))
      (t (setf (aref z i (- k 1)) (select x i (- k 1)))
         (setf (aref z i (- k 2)) (- (select x i (- k 2))))))
))
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

(defun laplace-cdf (x)
"Args: x
Compute laplace or double-exponential cdf at x"
(cond ((< x 0) (/ (exp x) 2))
  ((>= x 0) (- 1 (/ (exp (- x)) 2))))
)

(defun laplace-dens (x)
"Args: x
Compute laplace or double-exponential density at x"
(/ (exp (- (abs x))) 2)
)

(defun laplace-quant (x)
"Args: x
Compute laplace or double-exponential quantile function at x"
(cond ((< x .5) (log (* 2 x)))
  ((>= x .5) (- (log (* 2 (- 1 x))))))
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

(pick-type)


