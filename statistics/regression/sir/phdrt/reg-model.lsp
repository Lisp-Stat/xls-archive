;;;;
;;;; regression.lsp XLISP-STAT regression model proto and methods
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;; Incorporates modifications suggested by Sandy Weisberg.
;;;;
;;;;
;;;; Regresion Model Prototype
;;;;

(defproto reg-model-proto
          '(x y intercept sweep-matrix basis weights included
              total-sum-of-squares residual-sum-of-squares)
          ()
          *object*
          "Normal Linear Regression Model")

(defun reg-model (x y &key 
                           (intercept T) 
                           (print T) 
                           weights
                           (included (repeat t (length y)))
                           )
"Args: (x y &key (intercept T) (print T) weights 
          included predictor-names response-name case-labels)
X           - list of independent variables or X matrix
Y           - dependent variable.
INTERCEPT   - T to include (default), NIL for no intercept
PRINT       - if not NIL print summary information
WEIGHTS     - if supplied should be the same length as Y; error variances are
               assumed to be inversely proportional to WEIGHTS
INCLUDED    - if supplied should be the same length as Y, with elements nil
              to skip a in computing estimates (but not in residual analysis). 
Returns a reg model object. To examine the model further assign the
result to a variable and send it messages."

  (let ((x (cond 
             ((matrixp x) x)
             ((vectorp x) (list x))
             ((and (consp x) (numberp (car x))) (list x))
             (t x)))
        (m (send reg-model-proto :new)))
    (send m :x (if (matrixp x) x (apply #'bind-columns x)))
    (send m :y y)
    (send m :intercept intercept)
    (send m :weights weights)
    (send m :included included)
    m))

(defmeth reg-model-proto :isnew () (send self :needs-computing t))

(defmeth reg-model-proto :save ()
"Message args: ()
Returns an expression that will reconstruct the regression model."
  `(reg-model ',(send self :x)
                     ',(send self :y)
                     :intercept ',(send self :intercept)
                     :weights ',(send self :weights)
                     :included ',(send self :included)))
                       
;;;
;;; Computing and Display Methods
;;;

(defmeth reg-model-proto :compute ()
"Message args: ()
Recomputes the estimates. For internal use by other messages"
  (let* ((included (if-else (send self :included) 1 0))
         (x (send self :x))
         (y (send self :y))
         (intercept (send self :intercept))
         (weights (send self :weights))
         (w (if weights (* included weights) included))
         (m (make-sweep-matrix x y w))
         (n (array-dimension x 1))
         (p (- (array-dimension m 0) 1))
         (tss (aref m p p))
         (tol (* .0001 (mapcar #'standard-deviation (column-list x))))
         (sweep-result
          (if intercept
              (sweep-operator m (iseq 1 n) tol)
              (sweep-operator m (iseq 0 n) (cons 0.0 tol)))))
    (setf (slot-value 'sweep-matrix) (first sweep-result))
    (setf (slot-value 'total-sum-of-squares) tss)
    (setf (slot-value 'residual-sum-of-squares) 
          (aref (first sweep-result) p p))
    (setf (slot-value 'basis)
          (let ((b (remove 0 (second sweep-result))))
            (if b 
                (- (reverse b) 1)
                (error "no columns could be swept"))))))

(defmeth reg-model-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'sweep-matrix) nil))
  (null (slot-value 'sweep-matrix)))
  
;;;
;;; Slot accessors and mutators
;;;

(defmeth reg-model-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument
NEW-X sets the x matrix to NEW-X and recomputes the estimates."
  (when (and new-x (matrixp new-x))
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth reg-model-proto :y (&optional new-y)
"Message args: (&optional new-y)
With no argument returns the y sequence as supplied to m. With an argument
NEW-Y sets the y sequence to NEW-Y and recomputes the estimates."
  (when (and new-y (or (matrixp new-y) (sequencep new-y)))
        (setf (slot-value 'y) new-y)
        (send self :needs-computing t))
  (slot-value 'y))

(defmeth reg-model-proto :intercept (&optional (val nil set))
"Message args: (&optional new-intercept)
With no argument returns T if the model includes an intercept term, nil if
not. With an argument NEW-INTERCEPT the model is changed to include or
exclude an intercept, according to the value of NEW-INTERCEPT."
  (when set 
        (setf (slot-value 'intercept) val)
        (send self :needs-computing t))
  (slot-value 'intercept))

(defmeth reg-model-proto :weights (&optional (new-w nil set))
"Message args: (&optional new-w)
With no argument returns the weight sequence as supplied to m; NIL means
an unweighted model. NEW-W sets the weights sequence to NEW-W and
recomputes the estimates."
  (when set 
        (setf (slot-value 'weights) new-w) 
        (send self :needs-computing t))
  (slot-value 'weights))

(defmeth reg-model-proto :total-sum-of-squares ()
"Message args: ()
Returns the total sum of squares around the mean."
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'total-sum-of-squares))

(defmeth reg-model-proto :residual-sum-of-squares () 
"Message args: ()
Returns the residual sum of squares for the model."
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'residual-sum-of-squares))

(defmeth reg-model-proto :basis ()
"Message args: ()
Returns the indices of the variables used in fitting the model."
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'basis))

(defmeth reg-model-proto :sweep-matrix ()
"Message args: ()
Returns the swept sweep matrix. For internal use"
  (if (send self :needs-computing) (send self :compute))
  (slot-value 'sweep-matrix))

(defmeth reg-model-proto :included (&optional new-included)
"Message args: (&optional new-included)
With no argument,  NIL means a case is not used in calculating estimates, and non-nil means it is used.  NEW-INCLUDED is a sequence of length of y of nil and t to select cases.  Estimates are recomputed."
  (when (and new-included 
             (= (length new-included) (send self :num-cases)))
        (setf (slot-value 'included) (copy-seq new-included)) 
        (send self :needs-computing t))
  (if (slot-value 'included)
      (slot-value 'included)
      (repeat t (send self :num-cases))))
;;;
;;; Other Methods
;;; None of these methods access any slots directly.
;;;

(defmeth reg-model-proto :num-cases ()
"Message args: ()
Returns the number of cases in the model."
  (length (send self :y)))

(defmeth reg-model-proto :num-included ()
"Message args: ()
Returns the number of cases used in the computations."
  (sum (if-else (send self :included) 1 0)))

(defmeth reg-model-proto :num-coefs ()
"Message args: ()
Returns the number of coefficients in the fit model (including the
intercept if the model includes one)."
  (if (send self :intercept)
      (+ 1 (length (send self :basis)))
      (length (send self :basis))))

(defmeth reg-model-proto :df ()
"Message args: ()
Returns the number of degrees of freedom in the model."
  (- (send self :num-included) (send self :num-coefs)))
  
(defmeth reg-model-proto :x-matrix ()
"Message args: ()
Returns the X matrix for the model, including a column of 1's, if
appropriate. Columns of X matrix correspond to entries in basis."
  (let ((m (select (send self :x) 
                   (iseq 0 (- (send self :num-cases) 1)) 
                   (send self :basis))))
    (if (send self :intercept)
        (bind-columns (repeat 1 (send self :num-cases)) m)
        m)))

(defmeth reg-model-proto :fit-values ()
"Message args: ()
Returns the fitted values for the model."
  (matmult (send self :x-matrix) (send self :coef-estimates)))

(defmeth reg-model-proto :raw-residuals () 
"Message args: ()
Returns the raw residuals for a model."
  (- (send self :y) (send self :fit-values)))

(defmeth reg-model-proto :residuals () 
"Message args: ()
Returns the raw residuals for a model without weights. If the model
includes weights the raw residuals times the square roots of the weights
are returned."
  (let ((raw-residuals (send self :raw-residuals))
        (weights (send self :weights)))
    (if weights (* (sqrt weights) raw-residuals) raw-residuals)))

(defmeth reg-model-proto :sum-of-squares () 
"Message args: ()
Returns the error sum of squares for the model."
  (send self :residual-sum-of-squares))

(defmeth reg-model-proto :sigma-hat ()
"Message args: ()
Returns the estimated standard deviation of the deviations about the 
regression line."
  (let ((ss (send self :sum-of-squares))
        (df (send self :df)))
    (if (/= df 0) (sqrt (/ ss df)))))

;; for models without an intercept the 'usual' formula for R^2 can give
;; negative results; hence the max.
(defmeth reg-model-proto :r-squared ()
"Message args: ()
Returns the sample squared multiple correlation coefficient, R squared, for
the regression."
  (max (- 1 (/ (send self :sum-of-squares) (send self :total-sum-of-squares)))
       0))

(defmeth reg-model-proto :coef-estimates ()
"Message args: ()
Returns the OLS (ordinary least squares) estimates of the regression
coefficients. Entries beyond the intercept correspond to entries in basis."
  (let ((n (array-dimension (send self :x) 1))
        (indices (if (send self :intercept) 
                     (cons 0 (+ 1 (send self :basis)))
                     (+ 1 (send self :basis))))
        (m (send self :sweep-matrix)))
    (coerce (compound-data-seq (select m (+ 1 n) indices)) 'list)))

(defmeth reg-model-proto :plot-residuals (&optional x-values)
"Message args: (&optional x-values)
Opens a window with a plot of the residuals. If X-VALUES are not supplied 
the fitted values are used. The plot can be linked to other plots with the 
link-views function. Returns a plot object."
  (plot-points (if x-values x-values (send self :fit-values))
               (send self :residuals)
               :title "Residual Plot"))
