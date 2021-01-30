(defun binary-model
  (data var-names &optional obs-names)
"Args: (data &key (print t) var-names obs-names)
DATA: Matrix with at least one binary response and
      whos other columns are predictor variables
VAR-NAMES: List of k character strings naming the variables
OBS-NAMES: List of n character strings naming the observations"
  (def *binary-model-object*
       (send binary-models-proto :new 
             data var-names obs-names)))

(defproto binary-models-proto '(data var-names nobs nvars dv y y-name x x-names ivs x-use x-use-names obs-names pred-val-p pred-val resids-y resids-z likelyhood model-type model-menu n-ivs z hj rsj delta-beta delta-d delta-x-sq dj c-bar dfbeta order-y int visual-menu visual-menu-items x-mat-menu var-unnames full-x full-x-names x-selector x-out linked-vars trans-names transformations trans-selector linked-vars-out linked-vars-way-out))

(defmeth binary-models-proto :isnew
  (data var-names &optional obs-names)
  (send self :data (column-list data))
  (send self :var-names var-names)
  (send self :nobs (array-dimension data 0))
  (send self :obs-names (if obs-names obs-names
                            (mapcar #'(lambda (x) (format nil "~a" x))
                                    (iseq (send self :nobs)))))
  (send self :nvars (length var-names))
  (when (send self :dv-chooser)
        (when (send self :iv-chooser)
              (send self :model-menu-maker)
              (send self :visualize-menu-maker)
              (send self :x-mat-menu-maker))))

(defmeth binary-models-proto :iv-chooser ()
  (let* ((seq (remove (send self :dv)
                      (iseq (send self :nvars))))
         (pot-x (select (send self :data) seq))
         (pot-names (append (list "Intercept")
                            (select (send self :var-names) seq)))
         (ivs (select (choose-subset-dialog
                      "Independent variables:"
                      pot-names) 0))
         (x-use
          (when ivs (apply #'bind-columns
                           (select
                            (cons
                             (repeat 1 (send self :nobs))
                             (mapcar #'(lambda (xx)
                                         (select xx
                                                 (send self :order-y)))
                                     pot-x))
                                     ivs))))          
         (x-use-names (when ivs (select pot-names ivs))))
    (if (member 0 ivs)
        (send self :int 1)
        (send self :int 0))
    (send self :full-x 
          (mapcar #'(lambda (x) (coerce x 'list))
                  (column-list x-use)))
    (send self :x-selector (iseq (length ivs)))
    (send self :full-x-names x-use-names)
    (send self :n-ivs (length ivs))))

(defmeth binary-models-proto :dv-chooser ()
  (do* ((dv-test (choose-item-dialog
                  "Dependent variable:" 
                  (send self :var-names))
                 (choose-item-dialog
                  "Dependent variable:" 
                  (send self :var-names)))
        (y-test (coerce (select (send self :data) dv-test) 'list)
                (coerce (select (send self :data) dv-test) 'list))
        (test (send self :binaryp y-test) (send self :binaryp y-test))
        (flag (when (and dv-test (not test))
                    (ok-or-cancel-dialog
                     "Variable is not binary, choose another or cancel"))
              (when (not test)
                    (ok-or-cancel-dialog
                     "Variable is not binary, choose another or cancel"))))
       ((when (or test (not flag)(not dv-test)))
        (when (and test (send self :y (send self :code-y y-test)))
              (send self :y-name (select (send self :var-names) dv-test))
              (send self :dv dv-test)
              (send self :order-y (order (send self :y)))
              (send self :y (select (send self :y) (send self :order-y)))))))

(defmeth binary-models-proto :bi-resid-x-names ()
  (let ((list (append (send self :x-selector)
          (mapcar #'second (send self 
                                :linked-vars))))
        (names (send self :full-x-names)))
    (if (equalp (send self :int) 0)
        (select names list)
        (select names (rest list)))))

(defmeth binary-models-proto :bi-resid-x ()
  (let ((list (append (send self :x-selector)
          (mapcar #'second (send self 
                                :linked-vars))))
        (col-x (send self :full-x)))
    (if (equalp (send self :int) 0)
        (select col-x list)
        (select col-x (rest list)))))

(defmeth binary-models-proto :model-type (&optional (number nil set))
"Args: (&optional number)
Sets or returns model type
     0: Smooth
     1: Parametric"
  (when set (setf (slot-value 'model-type) number))
  (slot-value 'model-type))

(defmeth binary-models-proto :visual-menu (&optional (menu nil set))
"Args: (&optional (menu nil set))
Sets or returns visualization menu"
  (when set (setf (slot-value 'visual-menu) menu))
  (slot-value 'visual-menu))

(defmeth binary-models-proto :x-mat-menu (&optional (menu nil set))
"Args: (&optional (menu nil set))
Sets or returns x matrix menu"
  (when set (setf (slot-value 'x-mat-menu) menu))
  (slot-value 'x-mat-menu))

(defmeth binary-models-proto :linked-vars (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns list of linked variables"
  (when set (setf (slot-value 'linked-vars) list))
  (slot-value 'linked-vars))

(defmeth binary-models-proto :linked-vars-out (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns list of out linked variables"
  (when set (setf (slot-value 'linked-vars-out) list))
  (slot-value 'linked-vars-out))

(defmeth binary-models-proto :linked-vars-way-out (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns list of way out linked variables"
  (when set (setf (slot-value 'linked-vars-way-out) list))
  (slot-value 'linked-vars-way-out))

(defmeth binary-models-proto :trans-names (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns list of transformed variable names"
  (when set (setf (slot-value 'trans-names) list))
  (slot-value 'trans-names))

(defmeth binary-models-proto :transformations (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns list of transformed variables"
  (when set (setf (slot-value 'transformations) list))
  (slot-value 'transformations))

(defmeth binary-models-proto :visual-menu-items
  (&optional (list nil set))
"Args: (&optional (list nil set))
Sets or returns visualization menu items"
  (when set (setf (slot-value 'visual-menu-items) list))
  (slot-value 'visual-menu-items))

(defmeth binary-models-proto :data (&optional (matrix nil set))
"Args: (&optional matrix)
Sets or returns data matrix"
  (when set (setf (slot-value 'data) matrix))
  (slot-value 'data))

(defmeth binary-models-proto :full-x (&optional (matrix nil set))
"Args: (&optional matrix)
Sets or returns full data matrix"
  (when set (setf (slot-value 'full-x) matrix))
  (slot-value 'full-x))

(defmeth binary-models-proto :int (&optional (bern nil set))
"Args: (&optional bern)
Sets or returns presence (t) or absence (nil) of intercept"
  (when set (setf (slot-value 'int) bern))
  (slot-value 'int))

(defmeth binary-models-proto :hj (&optional (diagonal nil set))
"Args: (&optional diagonal)
Sets or returns diagonal of hat matrix"
  (when set (setf (slot-value 'hj) diagonal))
  (slot-value 'hj))

(defmeth binary-models-proto :delta-beta (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of standardized beta - beta-j"
  (when set (setf (slot-value 'delta-beta) list))
  (slot-value 'delta-beta))

(defmeth binary-models-proto :order-y (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of the order of the dependent variable"
  (when set (setf (slot-value 'order-y) list))
  (slot-value 'order-y))

(defmeth binary-models-proto :delta-d (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of deviance - deviance-j"
  (when set (setf (slot-value 'delta-d) list))
  (slot-value 'delta-d))

(defmeth binary-models-proto :delta-x-sq (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of x-sq - x-sq-j"
  (when set (setf (slot-value 'delta-x-sq) list))
  (slot-value 'delta-x-sq))

(defmeth binary-models-proto :c-bar (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of c bar influence statistics"
  (when set (setf (slot-value 'c-bar) list))
  (slot-value 'c-bar))

(defmeth binary-models-proto :dfbeta (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of changes for each beta with the elimination
of each observation"
  (when set (setf (slot-value 'dfbeta) list))
  (slot-value 'dfbeta))

(defmeth binary-models-proto :dj (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of deviance residuals"
  (when set (setf (slot-value 'dj) list))
  (slot-value 'dj))

(defmeth binary-models-proto :rsj (&optional (list nil set))
"Args: (&optional list)
Sets or returns list of standardized residuals"
  (when set (setf (slot-value 'rsj) list))
  (slot-value 'rsj))

(defmeth binary-models-proto :z (&optional (list nil set))
"Args: (&optional list)
Sets or returns adjusted dependent variable"
  (when set (setf (slot-value 'z) list))
  (slot-value 'z))

(defmeth binary-models-proto :n-ivs (&optional (number nil set))
"Args: (&optional matrix)
Sets or returns number of independent variables"
  (when set (setf (slot-value 'n-ivs) number))
  (slot-value 'n-ivs))

(defmeth binary-models-proto :obs-names (&optional (list nil set))
"Args: (&optional list)
Sets or returns observation names list"
  (when set (setf (slot-value 'obs-names) list))
  (slot-value 'obs-names))

(defmeth binary-models-proto :nobs (&optional (number nil set))
"Args: (&optional number)
Sets or returns number of observations"
  (when set (setf (slot-value 'nobs) number))
  (slot-value 'nobs))

(defmeth binary-models-proto :var-names (&optional (string-list nil set))
"Args: (&optional string-list)
Sets or returns included variable names"
  (when set (setf (slot-value 'var-names) string-list))
  (slot-value 'var-names))

(defmeth binary-models-proto :x-unnames ()
   (select (send self :full-x-names) 
           (if (send self :x-out)
               (append (sort-data (send self :x-out))
                       (mapcar #'second (send self :linked-vars-out))
                       (mapcar #'second (send self :linked-vars-way-out)))
               (append (mapcar #'second (send self :linked-vars-out))
                       (mapcar #'second (send self :linked-vars-way-out))))))

(defmeth binary-models-proto :nvars (&optional (number nil set))
"Args: (&optional number)
Sets or returns number of variables"
  (when set (setf (slot-value 'nvars) number))
  (slot-value 'nvars))

(defmeth binary-models-proto :dv (&optional (number nil set))
"Args: (&optional number)
Sets or returns dependent variable index"
  (when set (setf (slot-value 'dv) number))
  (slot-value 'dv))

(defmeth binary-models-proto :y (&optional (vector nil set))
"Args: (&optional vector)
Sets or returns dependent variable observation list"
  (when set (setf (slot-value 'y) vector))
  (slot-value 'y))

(defmeth binary-models-proto :y-name (&optional (string nil set))
"Args: (&optional name)
Sets or returns dependent variable name"
  (when set (setf (slot-value 'y-name) string))
  (slot-value 'y-name))

(defmeth binary-models-proto :x-selector (&optional (list nil set))
"Args (&optional list)
Sets or returns indices for variables to be modeled"
  (when set (setf (slot-value 'x-selector) list))
  (slot-value 'x-selector))

(defmeth binary-models-proto :x-out (&optional (list nil set))
"Args (&optional list)
Sets or returns indices for variables not to be modeled"
  (when set (setf (slot-value 'x-out) list))
  (slot-value 'x-out))

(defmeth binary-models-proto :full-x-names (&optional (list nil set))
"Args  (&optional list)
Sets or returns full list of independent variables"
  (when set (setf (slot-value 'full-x-names) list))
  (slot-value 'full-x-names))

(defmeth binary-models-proto :x ()
  (apply #'bind-columns
         (select (send self :full-x)
                 (sort-data
                  (append (send self :x-selector)
                          (mapcar #'second (send self :linked-vars)))))))

(defmeth binary-models-proto :x-names ()
  (select (send self :full-x-names)
           (append (sort-data (send self :x-selector))
                   (mapcar #'second (send self :linked-vars)))))

(defmeth binary-models-proto :x-use ()
  (if (equalp (send self :int) 0)
      (apply #'bind-columns
             (select (send self :full-x) 
                     (sort-data (send self :x-selector))))
      (apply #'bind-columns
             (select (send self :full-x)
                     (rest (sort-data (send self :x-selector)))))))

(defmeth binary-models-proto :x-use-names ()
  (if (equalp (send self :int) 0)
      (select (send self :full-x-names) (sort-data (send self :x-selector)))
      (rest (select (send self :full-x-names)
                    (sort-data (send self :x-selector))))))

(defmeth binary-models-proto :ivs (&optional (indices nil set))
"Args: (&optional indices)
Sets or returns independent variables indices"
  (when set (setf (slot-value 'ivs) indices))
  (slot-value 'ivs))

(defmeth binary-models-proto :pred-val (&optional (list nil set))
"Args: (&optional list)
Sets or returns model predicted logit values"
  (when set (setf (slot-value 'pred-val) list))
  (slot-value 'pred-val))

(defmeth binary-models-proto :pred-val-p (&optional (list nil set))
"Args: (&optional list)
Sets or returns model predicted probability values"
  (when set (setf (slot-value 'pred-val-p) list))
  (slot-value 'pred-val-p))

(defmeth binary-models-proto :likelyhood (&optional (number nil set))
"Args: (&optional number)
Sets or returns likelyhood"
  (when set (setf (slot-value 'likelyhood) number))
  (slot-value 'likelyhood))

(defmeth binary-models-proto :resids-y (&optional (list nil set))
"Args: (&optional list)
Sets or returns model residuals from observed response"
  (when set (setf (slot-value 'resids-y) list))
  (slot-value 'resids-y))

(defmeth binary-models-proto :resids-z (&optional (list nil set))
"Args: (&optional list)
Sets or returns model residuals from adjusted dependent variable"
  (when set (setf (slot-value 'resids-z) list))
  (slot-value 'resids-z))

(defmeth binary-models-proto :binaryp (list)
  (= 2 (length (remove-duplicates list :test 'equalp))))

(defmeth binary-models-proto :code-y (list)
  (let* ((members (remove-duplicates list :test 'equalp))
         (mem1 (select members 0))
         (mem2 (select members 1))
         (code-num nil)
         (response nil))
    (when (not (stringp mem1)) (setf mem1 (format nil "~a" mem1)))
    (when (not (stringp mem2)) (setf mem2 (format nil "~a" mem2)))
    (setf code-num (choose-item-dialog
                     "Which response is to be coded 1?"
                     (list mem1 mem2)))
    (setf response (select members code-num))
    (when code-num 
          (mapcar #'(lambda (x) (if (equalp x response) 1 0)) list))))
