;;;
;;;  additional methods to make compatible with glim-proto
;;;
(defmeth regression-model-proto :pweights (&optional (new nil set))
  (if set (send self :weights new) (send self :weights)))

(defmeth regression-model-proto :deviance ()
  (send self :residual-sum-of-squares))

(defmeth regression-model-proto :verbose (&rest args ) ())

(defmeth regression-model-proto :recycle (&optional set) ())

(defmeth regression-model-proto :yvar (&optional new-y) (send self :y new-y))

;;;
;;;  Compute a linear combination of estimates and its standard error
;;;

(defmeth regression-model-proto :lin-combination (x)
"
Message args: (x)
Computes x'betahat and its standard-error.  x must be a list or a vector."
  (let* ((b (send self :coef-estimates))
         (s (send self :sigma-hat))
         (a (send self :xtxinv)))
        (list  (sum (* x b)) (* s (sqrt (matmult x a x))))))

;;;  
;;; squared residuals for use in heteroscedasticity score test
;;;

(defmeth regression-model-proto :residuals-squared ()
"Message args: ()
Returns n*res^2/sum(res^2)."
  (/ (* (send self :num-included) (^ (send self :residuals) 2))
     (send self :residual-sum-of-squares)))

;;
;;  quantiles with some points not included
;;

(defmeth regression-model-proto :quantiles (&key (x (send self :yvar))
  (quantile-function #'normal-quant))
"
Message args: (&key (x (send self :yvar))
                         (quantile-function #'normal-quant))
Returns the quantile-function quantiles of x, ordered according
to x, with cases with included nil set to a small value."
  (let* ((ind (which (send self :included)))
         (z (rank (select x ind)))
         (z (funcall quantile-function
                     (/ (1+ z) (+ (length z) 1))))
         (plot-pos (repeat (funcall quantile-function 
                                  (/ .625 (+ (length z) 5))) 
                           (send self :num-cases))))
    (setf (select plot-pos ind) z)
    plot-pos))

;;
;; toggle cases method
;;

(defmeth regression-model-proto :toggle-cases (&optional (idnum nil))
"
Message args: idnum, a list of case numbers
Deletes/restores specified cases.  If idnum is nil, all cases are restored."
(let* (included)
  (cond
    ((null idnum)
     (setf included (make-list (send self :num-cases) :initial-element t)))
    (t
     (setf included (send self :included))
     (dolist (i (coerce idnum 'list))
             (setf (nth i included) (not (nth i included))))))
  (send self :included included)))


;;
;; methods for accessing graphs
;;

(defmeth regression-model-proto :delete-graph (graph)
  (setf (slot-value 'graphs) (remove graph (slot-value 'graphs))))

(defmeth regression-model-proto :graphs (&optional graph)
    (if graph (setf (slot-value 'graphs) 
                   (cons graph (slot-value 'graphs))))
    (slot-value 'graphs))

(defmeth regression-model-proto :last-graph () (first (send self :graphs)))

;;
;;  basic plotting methods
;;
(defmeth regression-model-proto :plot (&rest args) 
"
Message args: (data &key (plot-controls t))
Installs a plot of any dimension, histogram, 2-D, 3-D or more-D, in a regression model.  Data is a list giving instructions for creating the data to be plotted."
;;;NOTE:  The keyword, if present must be at the END of the Message
  (when (null (send self :has-slot 'graphs)) (send self :make-first-plot))
  (let* ((n (length args))
         (keyword (cond ((> n 1) (eq (nth (- n 2) args) ':plot-controls))
                    (t nil)))
         (plot-controls (if keyword (car (last args)) t))
         (data (cond ((= n 0) (slot-value 'default-plot))
                 ((= n 1) (if (listp (first args)) (first args) 
                                     (list (first args))))
                 (t (let* ((d (select args 
                                      (iseq (- n (if keyword 2 0))))))
                      (if (and (= (length d) 1)
                               (listp (first d))) (first d) d)))))
         (p (length data))
         (axes (iseq 0 (1- p)))
         (f (transpose (mapcar #'(lambda (data) 
                                 (send self :return-function data)) data)))
         (functions (second f))
         (labels (if (> p 2)
                     (mapcar #'(lambda (a) (substr a 0 5)) (first f))
                     (first f)))
         (title (cond
                ((= p 1) (first labels))
                ((= p 2)
                 (format nil "~a vs ~a"
                         (substr (first labels) 0 10)
                         (substr (second labels) 0 10))) 
                ((= p 3)
                 (format nil "X:~a Y:~a Z:~a" (first labels) (second labels)
                         (third labels)))
                (t  (format nil "~a Dimensional Plot" p))))
         (plot (send self :make-plot functions labels 
                   :title title
                   :matrix-plot nil
                   :replot-type "adjust"
                   :use-buffering nil
                   :plot-updating t
                   :plot-controls plot-controls)))
  plot))


(defmeth regression-model-proto :scatterplot-matrix (&rest args) 
"
Message args: (data &key (plot-controls t))
Installs a scatterplot matrix in a regression model.  Data is a list 
giving instructions for creating the data to be plotted."
;;;NOTE:  The keyword, if present must be at the END of the Message
  (when (null (send self :has-slot 'graphs)) (send self :make-first-plot))
  (let* ((n (length args))
         (keyword (cond ((> n 1) (eq (nth (- n 2) args) ':plot-controls))
                    (t nil)))
         (plot-controls (if keyword (car (last args)) t))
         (data (cond ((= n 0) 
                      (cons 'y (iseq (array-dimension (send self :x) 1))))
                 ((= n 1) (error "Need at least 2 quantities"))
                 (t (let* ((d (select args 
                                      (iseq (- n (if keyword 2 0))))))
                      (if (and (= (length d) 1)
                               (listp (first d))) (first d) d)))))
         (p (length data))
         (axes (iseq 0 (1- p)))
         (f (transpose (mapcar #'(lambda (data) 
                                 (send self :return-function data)) data)))
         (functions (second f))
         (labels (first f))
         (title "Scatterplot Matrix")
         (plot (send self :make-plot functions labels 
                   :title title
                   :replot-type "adjust"
                   :matrix-plot t
                   :use-buffering nil
                   :plot-updating t
                   :plot-controls plot-controls)))
  plot))

(defmeth regression-model-proto :make-plot 
  (functions labels 
             &key (title "Regression Plot") 
             (matrix-plot nil)
             (replot-type "redraw")
                     (use-buffering nil)
             (plot-updating t)
             (plot-controls nil))
"
Internal function to draw plots.  Generally not to be accessed by the user."
;;;data is either (1) a list of functions.  Evaluation of the j-th function
;;;                   gives the data for axis j; or
;;;               (2) a function that, when evaluated gives all axes.  
;;;  For a general plot, data is created by the call to the :plot method.
;;;  For other plots, e.g. :ares plots, it is created by a call to the :ares
;;;     method
;;;The keyword tell if the plot should use buffering, plot controls, and
;;;plot updating when the model is changed.
  (when (null (send self :has-slot 'graphs)) (send self :make-first-plot))
  (let* ((data (cond 
                 ((equal (type-of functions) 'cons) 
                  (mapcar 'funcall functions))
                 ((equal (type-of functions) 'closure)
                  (funcall functions))))
         (ids (iseq (send self :num-cases)))
         (num (length data))
         (graph (cond
                ((= num 1)
                 (histogram data 
                            :variable-labels labels :title title))
                (matrix-plot
                  (scatterplot-matrix data :variable-labels 
                                      (mapcar #'(lambda (a) (substr a 0 7))
                                                        labels)
                                      :title title))
                ((= num 2)
                 (plot-points data
                              :variable-labels labels :title title))
                ((= num 3)
                   (spin-plot data :variable-labels 
                       (mapcar #'(lambda (c) (substr c 0 5)) labels)
                            :title title ))
                 (t
                  (scatterplot-matrix data :variable-labels 
                                      (mapcar #'(lambda (a) (substr a 0 7))
                                                        labels)
                                      :title title)))))
    (send self :graphs graph)
;;;the next line selects points not 'included in the fit
    (when (= (length (send self :graphs)) 1)
          (send graph :point-selected (iseq (send self :num-cases))
                (mapcar 'not (send self :included))))
    (send graph :linked t)
    (send graph :point-label ids (send self :case-labels))
    (when use-buffering (send graph :start-next-frame
                            #'(lambda () (send graph :start-buffering)))
          (send graph :finish-next-frame 
          #'(lambda () (send graph :buffer-to-screen))))
    (send graph :finish-next-frame #'(lambda () 
           (if (equal replot-type "adjust") 
               (send graph :adjust-to-data)
               (send graph :redraw-content))))
;;;The next when statement gives my linking strategy:  point symbol, state
;;;and color are inherited to later plots, except symbols are not inherited
;;;from a > 2D plot.
    (when (> (length (send graph :links)) 1)
          (let ((plot (select (send graph :links) 1)))
            (send graph :point-color ids (send plot :point-color ids))
            (send graph :point-state ids (send plot :point-state ids))
            (when (and (< num 3) (= (send plot :num-point-variables) 2))
                  (send graph :point-symbol ids 
                        (send plot :point-symbol ids)))))
    (send graph :add-slot 'owner self)
    (send graph :add-slot 'functions functions)
    (send graph :add-slot 'axes (iseq num))
    (send graph :add-slot 'plot-updating plot-updating)
    (if plot-controls (send graph :plot-controls))
    (when plot-updating (send graph :menu-delete-points))
    (defmeth graph :close ()
      (send (slot-value 'owner) :delete-graph self)
      (call-next-method))
    graph))


;;
;; modify compute and needs-computing methods
;;

(defmeth regression-model-proto :make-first-plot ()
"Message args: ()
Creates two slots and override methods for compute and needs-computing to
automatically update graphs if present whenever :needs-computing is set to
t."
  (send self :add-slot 'graphs)
;;;the next slot is for compatibility with glim-proto
  (send self :add-slot 'now-computing nil)
  (send self :add-slot 
        'default-plot (list 'fit-values 'studentized-residuals))
  (defmeth self :compute ()
    (setf (slot-value 'now-computing) t)
    (call-next-method)
    (setf (slot-value 'now-computing) nil)
    (when (send self :has-slot 'ir) (send self :ir-needs-computing t))
    (send self :update-graphs))
  (defmeth self :needs-computing (&optional set)
    (cond ((and set (send self :graphs-need-updating)
                (null (slot-value 'now-computing)))
           (send self :compute))
      (set (setf (slot-value 'sweep-matrix) nil)))
    (null (slot-value 'sweep-matrix))))

(defmeth regression-model-proto :graphs-need-updating ()
"
Returns t if there are graphs to update, and nil otherwise."
  (> (length (slot-value 'graphs)) 0))

(defmeth regression-model-proto :update-graphs ()
"
Updates the graphs."
  (when (send self :graphs-need-updating)
        (dolist (graph (send self :graphs)) 
                (when (send graph :slot-value 'plot-updating)
                      (let* ((fun (send graph :slot-value 'functions))
                             (data (cond
                                    ((equal (type-of fun) 'cons)
                                     (mapcar 'funcall fun))
                                    ((equal (type-of fun) 'closure)
                                     (funcall fun)))))
                             (send graph :point-showing 
                                (iseq (send self :num-cases))
                                (send self :included))
                             (send graph :draw-next-frame
                                (send graph :slot-value 'axes) data))))))

;;
;;  translate a function name into a variable label and 
;;  a function that when funcalled computes a vector
;;
(defmeth regression-model-proto :return-function (name &key add)
"
Message args: (name &key add)
This internal function sets up an association list mapping names
of functions to functions.  Used in plotting.  If the keyword add is true,
then name is added to the association list.  It is assumed to be of the form
(list 'keyword (list quoted-label function)).  The 'keyword is used to name
the quantity to be plotted.  The quoted-label is the label used on the graph.
The function, when funcalled, computes the data."
   (when (null (send self :has-slot 'function-list))
         (send self :add-slot 'function-list
              (list
               (list 'response (list (send self :response-name)
                     #'(lambda () (send self :yvar))))
               (list 'fit-values          (list "Fit-values"
                     #'(lambda () (send self :fit-values))))
               (list 'residuals    (list 
                     (if (send self :pweights) 
                         "Residuals weighted" "Residuals")
                     #'(lambda () (send self :residuals))))
               (list 'studentized-residuals (list "Studentized residuals"
                     #'(lambda () (send self :studentized-residuals))))
               (list 'jackknife-residuals 
                     (list "Ext. Stud. Residuals" #'(lambda () 
                     (send self :externally-studentized-residuals))))
               (list 'leverages (list "Leverages" #'(lambda () 
                     (send self :leverages))))
               (list 'cooks-distances (list "Cook's Distance"
                     #'(lambda () (send self :cooks-distances))))
               (list 'sqrt-cooks-distances (list "Sqrt Cook's Dist."
                     #'(lambda () (sqrt (send self :cooks-distances)))))
               (list 'case-numbers (list "Case numbers" #'(lambda () 
                  (iseq 0 (1- (send self :num-cases))))))
               (list 'local-inf (list "Local inf. dir. cosine" #'(lambda ()
                  (second (send self :local-inf :absolute t)))))
               (list 'signed-local-inf 
                     (list "Signed local inf. dir. cosine" #'(lambda ()
                  (second (send self :local-inf :absolute nil)))))
               (list 'local-inf-scale (list "Local inf. dir. cosine"
                   #'(lambda () (second (send self :local-inf :scale t)))))
               (list 'local-influence (list "Local inf. dir. cosine"
                   #'(lambda () (second (send self :local-inf :absolute t)))))
               (list 'signed-local-influence 
                     (list "Signed local inf. dir. cosine"
                   #'(lambda () (second 
                                 (send self :local-inf :absolute nil)))))
               (list 'local-influence-scale (list "Local inf. dir. cosine"
                   #'(lambda () (second (send self :local-inf :scale t)))))
               (list 'residuals-squared (list "Residuals^2" #'(lambda ()
                  (send self :residuals-squared))))
               (list 'normal-scores (list "Normal Scores" #'(lambda ()
                  (send self :quantiles))))
               (list 'yvar (list (send self :response-name)
                     #'(lambda () (send self :yvar))))
               (list 'y (list (send self :response-name)
                     #'(lambda () (send self :y))))
               (list 'externally-studentized-residuals 
                     (list "Ext. Stud. Residuals" #'(lambda ()
                     (send self :externally-studentized-residuals))))
               (list 'raw-residuals (list 
                     (if (send self :pweights) 
                         "Residuals unweighted" "Raw Residuals")
                     #'(lambda () (send self :raw-residuals))))
               (list 'distances       (list "Cook's Distance"
                     #'(lambda () (send self :cooks-distances))))
			)))
  (cond 
	((null name) (slot-value 'function-list))
   (add (setf (slot-value 'function-list) 
              (cons name (slot-value 'function-list)))
        name)
   (t
    (let* (col
          (predictor-names (send self :predictor-names))
          (*function-list* (slot-value 'function-list))
          (function (second (assoc name *function-list*))))
    (cond
      (function
       (setf (select function 0) (format nil "~a" (select function 0))))
      ((or (numberp name) (stringp name))
       (setf col (send self :convert-name name :basis nil))
       (setf function #'(lambda ()
              (select (coerce (transpose (send self :x)) 'list) col)))
       (setf text-label  (select predictor-names col))
       (setf function (list text-label function)))
      ((or (vectorp name) (listp name))
       (setf function (list " " #'(lambda () name))))
      ((equal (type-of name) 'closure)
       (setf function (list "" name)))
      (t (error "Axis choice is not a list or a closure")))
    function))))

(defmeth regression-model-proto :convert-name (x &key (basis t))
"
Message args: (x)
x is an integer or a predictor-name.  Returns an integer.  If basis is t, the x-variable must be included in the basis.  Used by return-function."
  (let* ((b (if basis (send self :basis) 
                (iseq (array-dimension (send self :x) 1))))
         (names (send self :predictor-names)))
    (cond
      ((numberp x)
       (if (intersection (list x) b)
                  x
                  (error "No such predictor in the model")))
      ((stringp x)
       (if (member x names :test 'equal)
           (position x names :test 'equal)
           (error "Not a predictor name")))
      ((not x) ())
      (t (error "Input neither an integer nor a string")))))
