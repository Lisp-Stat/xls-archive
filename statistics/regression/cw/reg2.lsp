;;; various plot methods for regression-model-proto

(defmeth regression-model-proto :qq-plot 
            (&key (x 'studentized-residuals)
                  (quantile-function #'normal-quant)
                  (plot-controls t))
"
Message args: (&key (x 'studentized-residuals)
                    (quantile-function #'normal-quant) 
                    (plot-controls t))
values is an expression that when evaluated give the quantities to be plotted, 
or else it is a keyword from the list in :return-function.  Quantile function, 
if supplied, returns the quantiles for the null distribution.  For example, to
get a qq-plot the response y against chi-squared quantiles with 5 df, 
use the message:
(send reg :qq-plot :x 'y :quantile-function #'(lambda (p) (chisq-quant p 5)))"

(let* ((data (send self :return-function x))
       (rankits (list "Quantiles" #'(lambda () (send self :quantiles 
                       :x (funcall (second data)) 
							  :quantile-function quantile-function))))
       (title (if (> (length (first data)) 18)
                  (first data) (format nil "QQ Plot: ~a" (first data))))
       (plot (send self :make-plot 
                  (list (second rankits) (second data))
                  (list (first rankits)  (first data))
                  :title title
                   :replot-type "redraw"
                   :plot-updating t
                   :use-buffering nil
                   :plot-controls plot-controls)))
  (send plot :point-showing (iseq (send self :num-cases)) 
        (send self :included))
  plot))

(defmeth regression-model-proto :ares ()
"Produces and ARES plot (Cook and Weisberg, 1989) Technometrics, p 279."
  (flet ((get-base-model (basis names)
      (let ((selection (select-list-dialog
              (select names basis)
              :title "Choose Base Model Predictors (X1)"
              :left-title "Predictors"
              :right-title "Base Model")))
        (unless selection (return-from :ares))
        (if (= (length basis) (length (second selection)))
       (error "Can't put all variables in the base model"))
        (select basis (second selection))))
    (get-added-variables (basis names base-model)
      (let* ((choices (sort-data (set-difference basis base-model)))
        (selection
         (select-list-dialog (select names choices)
                   :title "Select added predictors"
                   :left-title "Remaining Predictors"
                   :right-title "Added predictors (X2)")))
        (unless selection (return-from :ares))
        (unless (second selection)
           (error "Must add at least one variable."))
        (select choices (second selection))))
    (get-group (added-variables)
      (if (= (length added-variables) 1)
          t
          (let ((selection (choose-item-dialog 
             "Variables to be added"
             '("as a group" "sequentially"))))
       (unless selection (return-from :ares))
       (select '(t nil) selection)))))
    (let* ((basis (send self :basis))
      (names (send self :predictor-names))
      (base-model (get-base-model basis names))
      (added-variables (get-added-variables basis names base-model))
      (group (get-group added-variables))
      (y (send self :yvar))
      (firstp (length base-model))
      (fn (+ firstp (if (send self :intercept) 0 -1)))
      (secondp (length added-variables))
      p
      yhat
      wt
      (data #'(lambda ()
           (setf wt (if (send self :pweights) 
              (sqrt (send self :pweights)) 1))
           (setf p
            (project 
             (select (send self :x)
                (iseq 0 (1- (send self :num-cases)))
                (append base-model added-variables))
             :intercept (send self :intercept)
             :included (send self :included)
             :weights (send self :pweights)))
           (setf yhat
            (* (column-list (send p :q)) (send p :qty y)))
           (setf yhat
            (mapcar #'(lambda(a) (coerce a 'list)) yhat))
           (dolist (j (iseq 1 (1- (length yhat))))
             (setf (nth j yhat) 
              (+ (nth (1- j) yhat) (nth j yhat))))
           (setf yhat 
            (mapcar #'(lambda (yhat) (/ yhat wt)) yhat)) 
           (list (nth fn yhat) (- y (nth fn yhat)))))
      (title (format nil "~a + ~a"
           (select names base-model)
           (select names added-variables)))
      (ares-plot (send self :make-plot
             data
             '("Fitted values" "Residuals")
             :title title :plot-controls t))
      (action #'(lambda(slider) 
             (let* ((num (round (cond
                  ((= slider 0) 0)
                  ((= slider (floor slider))
                   (- slider 1))
                  (t (floor slider)))))
               (lam (- slider num)) 
               (yh (cond 
               (group (first (last yhat)))
               ((= num secondp) (first (last yhat)))
               (t (nth (+ fn num 1) yhat))))
               (yh1 (cond
                (group (nth fn yhat))
                ((= num 0) (nth fn yhat))
                (t (nth (+ fn num) yhat))))
               (yhatlam (- yh (* (- 1 lam) (- yh yh1))))
               (ehatlam (- y yhatlam)))
          (send ares-plot :draw-next-frame '(0 1) 
                (list yhatlam ehatlam)))))
      (xmin (min (list (send ares-plot :range 0) 
             (first (last yhat)))))
      (xmax (max (list (send ares-plot :range 0)
             (first (last yhat)))))
      (ymin (min (list (send ares-plot :range 1) 
             (- y (first (last yhat))))))
      (ymax (max (list (send ares-plot :range 1) 
             (- y (first (last yhat))))))
      (seq (* .01 
         (round (* 100 
              (rseq 0 (if group 1 secondp) 
               (if group 21 (+ 1 (* 10 secondp))))))))
      (slider (send slider-control-proto :new seq
          :location (send ares-plot :locate-next-control)
          :graph ares-plot
          :length (send ares-plot :slider-width)
          :title "lambda")))
      (send ares-plot :range 0 xmin xmax)
      (send ares-plot :range 1 ymin ymax)
      (defmeth slider :do-action (lam) (funcall action lam))
      ares-plot)))


(defmeth regression-model-proto :spin-residuals ()
"Message args: ()
Produces a 3D plot {y2.1/a, e/b, y1/c} where the groups of 
variables are specified in dialog boxes: 1 is given first, 2 second and the 
remaining variables are given last.  This 3D plot is described in Cook and 
Weisberg, 1989, Technometrics, page 289.  Uses Project Proto."
  (flet ((get-x1 (names basis)
      (let ((selection (select-list-dialog 
              (select names basis)
              :title "Base Model Predictors (X1)..."
              :left-title "Candidates"
              :right-title "Selection")))
        (unless selection (return-from :spin-residuals))
        (if (= (length (second selection)) (length basis))
       (error "Can't put all variables in the base model"))
        (select basis (second selection))))
    (get-x2 (names basis x1)
      (let* ((choices (sort-data (set-difference basis x1)))
        (selection (select-list-dialog 
               (select names choices)
               :title "Added Predictors (X2)..."
               :left-title "Candidates"
               :right-title "Selection")))
        (unless selection (return-from :spin-residuals))
        (unless (second selection)
           (error "Must add at least one variable."))
        (select choices (second selection)))))
    (let* ((basis (send self :basis))
      (names (send self :predictor-names))
      (x1 (get-x1 names basis))
      (firstp (if x1 (length x1) 0))
      (x2 (get-x2 names basis x1))
      (secondp (+ firstp (length x2)))
      (x3 nil)
      (big-model (append x1 x2 x3))
      (data #'(lambda ()
           (let* ((p (project 
            (select (send self :x)
               (iseq (send self :num-cases))
               big-model)
            :intercept (send self :intercept)
            :included (send self :included)
            :weights (send self :pweights)))
             (y (send self :yvar))
             (data (list (send p :p2.1 y firstp secondp)
               (send p :residuals y)
               (send p :project y firstp)))) 
             (if x3
            (append data (list (send p :p2.1 y secondp)))
            data))))
;;;Setup plot title & plot.  
      (title (format nil "~a + ~a" (select names x1) (select names x2)))
      (graph (send self :make-plot data (if (null x3) 
                   '("y21" "e" "y1") 
                        '("y21" "e" "y1" "y312"))
         :title title :plot-controls nil))) 
      (send graph :spin-resid-controls)
      graph)))


 
(defmeth regression-model-proto :avsp ()
"Args: ()
Produces a 3D added variable plot (Cook and Weisberg, 1989
Technometrics, p 284).  Uses project-proto."
  (flet ((get-base (names basis)
      (let ((selection (choose-subset-dialog "Base  Predictors..."
                    (select names basis))))
        (unless selection (return-from :avsp))
        (unless (<= 2 (- (length basis) (length (first selection))))
           (error "Must leave out at least two variables"))
        (select basis (first selection))))
    (get-add (which names basis used)
      (let* ((choices (sort-data (set-difference basis used)))
        (selection (choose-item-dialog 
               (format nil "~a added variable..." which)
               (select names choices))))
        (unless selection (return-from :avsp))
        (select choices selection))))
    (let* ((basis (send self :basis))
      (names (send self :predictor-names))
      (id (iseq (length basis))) ;; *** is this needed?
      (y (send self :yvar))
      (base (get-base names basis))
      (x0 (cond (base
            (coerce (transpose (select (send self :x)
                   (iseq (send self :num-cases))
                   base))
               'list))
           ((send self :intercept) (repeat 1 (send self :num-cases)))
           (t (error "You must have a base model or an intercept"))))
      (firstp (if base (length base) 0))
      (add1 (get-add "First" names basis base))
      (x1 (first (coerce (transpose (select (send self :x)
                   (iseq (send self :num-cases))
                   add1))
               'list)))
      (add2 (get-add "Second" names basis (append base (list add1))))
      (x2 (first (coerce (transpose (select (send self :x)
                   (iseq (send self :num-cases))
                   add2))
               'list)))
      (data #'(lambda ()
           (let* ((p (project x0
               :intercept (send self :intercept)
               :included (send self :included)
               :weights (send self :pweights)))
             (xa (send p :residuals x1))
             (ya (send p :residuals y ))
             (za (send p :residuals x2)))
             (list xa ya za))))
      (plot (send self :make-plot data '("e2.1" "ey.1" "e3.1")
             :title (format nil "Old-AVsP 1:~a 2:~a Base:~a"
                  (select names add1)
                  (select names add2)
                  (select names base))
             :plot-controls t)))
      (send plot :variable-label (list 0 1 2) (list "e2.1" "ey.1" "e3.1"))
      plot)))

(defmeth regression-model-proto :avp ()
"Args: ()
Produces an added variable plot (Cook and Weisberg, 1989
Technometrics, p 283). Uses project-proto."
  (flet ((get-base (names)
      (let ((selection (choose-subset-dialog "Base Model Predictors..."
                    names)))
        (unless selection (return-from :avp))
        ;; *** is this first check really needed?
        (unless (first selection)
           (error "Must select at least one variable"))
        (unless (<= 1 (- (length names) (length (first selection))))
           (error "Must leave out at least one variable"))
        (first selection)))
    (get-add (names)
      (let ((selection (choose-subset-dialog "Added predictors..."
                    names)))
        (unless selection (return-from :avp))
        ;; *** error check
        (first selection))))
    (let* ((names (send self :predictor-names))
      (id (iseq 0 (1- (array-dimension (send self :x) 1))))
      (y (send self :yvar))
      (base (get-base names))
      (firstp (length base))
      (add1 (select (rmel base id) (get-add (rmel base names))))
      (mod (append base add1))
      (data #'(lambda ()
           (let* ((p (project 
            (select (send self :x)
               (iseq 0 (1- (send self :num-cases))) 
               mod)
            :intercept (send self :intercept)
            :included (send self :included)
            :weights (send self :pweights))))
             (list (send p :p2.1 y firstp)
              (send p :residuals y firstp)))))
      (plot (send self :make-plot  data '("Y2-1" "e1" )
             :title (format nil "AVP Base:~a Added:~a"
                  (select names base)
                  (select names add1))
             :plot-controls t)))
      (send plot :use-color t)
      (send plot :point-color (iseq 0 (1- (send plot :num-points))) 'red)
      (send plot :avp-controls)
      plot)))

; In the X11 version, the function find-menu is not defined until the first
; menu is created in the call to make-fake-menubar.  However, the
; :graphics-menu message calls find menu before the first menu is created.
; this creates a dummy function that will be overridded by the real thing
; later.

#+X11 (defun find-menu (a))

(defmeth regression-model-proto :graphics-menu (&optional (title "Graphics"))
 (when (find-menu title) (send (find-menu title) :remove))
 (let* ((graphics (send menu-proto :new title))
       (yhat (send menu-item-proto :new "Stud. res. vs yhat" :action
                   #'(lambda () (send self :plot ))))
       (ares (send menu-item-proto :new "ARES plot" :action
                   #'(lambda () (send self :ares))))
       (avp (send menu-item-proto :new "AVP -- 2-D" :action
                   #'(lambda () (send self :avp))))
       (avsp (send menu-item-proto :new "AVsP -- 3-D" :action
                   #'(lambda () (send self :avsp))))
       (spin (send menu-item-proto :new "Spin residuals, 3-D" :action
                   #'(lambda () (send self :spin-residuals))))
       (inf1 (send menu-item-proto :new "Local influence--absolute" :action
                   #'(lambda () (send self :plot 'case-numbers 'local-inf))))
       (inf2 (send menu-item-proto :new "Local influence--signed" :action
                   #'(lambda () (send self :plot 'case-numbers 
                                      'signed-local-inf))))
       (qq (send menu-item-proto :new "Normal-plot" :action
                   #'(lambda () (send self :qq-plot))))
       (hetero (send menu-item-proto :new "Non-constant Var. Score" :action
                   #'(lambda () (send self :hetero-score))))
       (dr (send menu-item-proto :new "Dynamic-rankits" :action
                   #'(lambda () (send self :dynamic-rankits))))
       (plot-mat (send menu-item-proto :new "Scatterplot Matrix" :action
                       #'(lambda () (send self :scatterplot-matrix))))
       (plot-dialog (send menu-item-proto :new "Other Plot"  :action
                 #'(lambda () (send self :plot-dialog))))
       (disp (send menu-item-proto :new "Display fit" :action
                   #'(lambda () (send self :display))))
       (dash0 (send dash-item-proto :new))
       (dispose (send menu-item-proto :new "Remove graphics Menu" :action
                      #'(lambda () (send graphics :dispose)))))
  (send graphics :append-items yhat ares avp avsp 
        spin inf1 inf2 qq hetero dr 
        plot-mat plot-dialog disp
        dash0 dispose)
  (send graphics :install)))

(defmeth regression-model-proto :plot-dialog ()
(let* ((items (first (transpose (send self :return-function nil))))
		 (predictors (send self :predictor-names))
		 (selector (select-list-dialog 
						(combine predictors (mapcar #'string items))
												 :title "Choose quantities to plot"
												 :left-title "Candidates"
												 :right-title "Selected Axes")))
  (cond
	((null selector) nil)
	((null (second selector)) nil)
	(t (apply #'send self :plot (select (combine predictors items)
													(second selector)))))))

(defmeth regression-model-proto :local-inf (&key (scale nil) (absolute nil))
"Args: (&optional (scale nil))
Computes the maximum local influence Cmax and the direction vector lmax 
for case weight perturbation, as in Cook (1986), eqs 29 and 30."
  (let* ((included (which (send self :included)))
         (r (send self :residuals))
         (ss (send self :residual-sum-of-squares) )
         (s2 (/ ss (send self :num-included)))
         (p (project (select (send self :x)
                                  (iseq 0 (1- (send self :num-cases)))
                                  (send self :basis))
                                    :intercept (send self :intercept)
                                    :included (send self :included)
                                    :weights (send self :weights)))
         (q1 (sweep (send p :q) 'cols #'(lambda (a) (* r a))))
         (q2 (/ (^ (send self :residuals) 2)
                (sqrt (* 2 ss))))
         (d (sv-decomp (select
                        (if scale (bind-columns q1 q2) q1) included
                        (iseq 0 (1- (+ (array-dimension q1 1) 
                                       (if scale 1 0)))))))
         (cmax (max (* 2 (^ (second d) 2) (/ s2))))
         (ind (position (max (second d)) (coerce (second d) 'list)))
         (lmax (repeat 0 (send self :num-cases))))
    (setf (select lmax included) 
          (coerce (nth ind (column-list (first d))) 'list))
        (print (format t "Maximum Curvature = ~a~%" cmax))
        (list cmax (if absolute (abs lmax) lmax))))

(defmeth regression-model-proto :bcp (c p)
  (let*  ((inc (which (send self :included)))
          (z (+ c (select (send self :yvar) inc)))
          (gmean (geometric-mean z))
          (ans (make-list (length (send self :yvar)) :initial-element 0))
          (bc (cond (gmean
                     (if (< (abs p) .0001)
                          (* gmean (log z))
                          (/ (- (^ z p) 1) (* p (^ gmean (- p 1))))))
                         nil)))  
         (if gmean (setf (select ans inc) bc) (setf ans nil))
         ans))
               
(defmeth regression-model-proto :dynamic-rankits
  (&key (residuals 'studentized-residuals) (shift 0) 
        (lambda '(-1.5 -1 -.5 -.33 -.25 0 .25 .33 .5 1 1.5))
        (y-transform #'(lambda (c lam) 
                               (let ((ans (send self :bcp c lam)))
                                    (if ans ans (error "Negative y")))))
        (x-transform #'(lambda (x) x) ) (quantile-function #'normal-quant))
"
Message Args:   (&key (residuals 'studentized-residuals) (shift 0) 
        (lambda '(-1.5 -1 -.5 -.33 -.25 0 .25 .33 .5 1 1.5))
        (y-transform #'(lambda (c lam) (send self :bcp c lam)) sety)
        (x-transform #'(lambda (x) x) setx) (quantile-function #'normal-quant))
Returns a confidence curve (Cook & Weisberg, 1990 JASA, p. 544) of the log 
likelihood for transformations applied to the response and predictors 
in the model indexed by lam in the lambda-range.  The transformation to y 
must be normalized to have Jacobian |dy/dlam| = 1.  Also produces a dynamic 
slider probability plot with quantiles generated by quantile function for 
the transformed model.  See Cook and Weisberg, 1989, Technometrics.  
This is very old code, and it does not work with case deletion."
     (let* ((pow lambda)
            (y (send self :yvar))
            (x (funcall x-transform (select (send self :x)
                                            (iseq (send self :num-cases))
                                            (send self :basis))))
            (proj (project x :intercept (send self :intercept)
                           :included (send self :included)
                           :weights (send self :pweights)))
            (f-lik #'(lambda (lam) (* -.5 (send self :num-included)
                           (log (send proj :residual-sum-of-squares y-t)))))
            log-lik
            y-t
            data
            rankits
            (y-name (if (equal residuals 'studentized-residuals)
                        "Studentized Residuals" "Residuals"))
            (not-inc (which (if-else (send self :included) nil t)))
            (get-values #'(lambda (y)
                            (let ((ans (if (equal residuals
                                               'studentized-residuals)
                                        (send proj :studentized-residuals y)
                                           (send proj :residuals y))))
                              (when not-inc
                                    (setf (select ans not-inc)
                                          (make-list (length not-inc)
                                                     :initial-element 0)))
                              ans))))
       (dolist (lam pow)
            (setf y-t (funcall y-transform shift lam))
            (setf log-lik (cons (funcall f-lik lam) log-lik))
            (setf data (cons (funcall get-values y-t) data))
            (setf rankits (cons (send self :quantiles :x (first data)
                  :quantile-function quantile-function) rankits)))
    (setf data (reverse data))
    (setf rankits (reverse rankits)) 
    (plot-lines (sqrt (* 2 (- (max log-lik) (reverse log-lik)))) pow
                :variable-labels '("z-value" "Power")
                :title "Confidence Curve") 
    (send self :dynamic-plot rankits data "Dynamic Rankit Plot"
          (list "Probability Scale" y-name) pow)))

(defmeth regression-model-proto :dynamic-plot 
  (data-x data-y plot-title plot-labels pow)
"Sets up a dynamic plot."
  (let* ((data #'(lambda () (list (list (min data-x) (max data-x))
                            (list (min data-y) (max data-y)))))
         (plot (send self :make-plot data plot-labels
                     :plot-updating nil 
                     :title plot-title
                     :plot-controls t
                     :replot-type "redraw")))
    (send plot :dynamic-plot-control (iseq (length pow)) pow)
    (send plot :use-color t)
    (send plot :add-slot 'data-x data-x)
    (send plot :add-slot 'data-y data-y) 
    (send plot :x-axis t t 5)
    (send plot :y-axis t t 5)
    (send plot :clear :draw nil)
    (send plot :add-points (select data-x 0) (select data-y 0)) 
    (send plot :point-showing (iseq (length (send self :included)))
          (send self :included))
    (send plot :point-color (iseq (send plot :num-points)) 'red)
    (send plot :redraw)
    plot))

(defmeth regression-model-proto :hetero-score ()
  (let* ((x-mod (send self :fit-values))
         (lab '("Fit-values"))
         (score #'(lambda ()
                    (let* ((test (* .5 (- (send reg :total-sum-of-squares)
                            (send reg :residual-sum-of-squares))))
                           (df (length (send reg :basis))))
                      (list (* .01 (round (* 100 test)))
                            df 
                       (* .001 (round (* 1000 (- 1 (chisq-cdf test df)))))))))
         (base-mod self)
         reg
         (data #'(lambda () 
                   (setf reg (regression-model x-mod 
                                 (send base-mod :residuals-squared)
                                 :included (send base-mod :included)
                                 :weights (send base-mod :pweights)
                                 :print nil))
                   (list (send reg :fit-values) (send reg :yvar))))
         (graph (send self :make-plot data 
                      '("Est. Direction" "Squared Residuals")
                  :title "N.C. Var. Score Plot" :plot-controls t))
         (control2 (send hetero2-control-proto :new
                         (send graph :locate-next-control)))
         (control (send hetero-control-proto :new
                        (send graph :locate-next-control) 
                        (format nil "Score test: ~a  ~a" 
                                lab (funcall score)))))
    (send graph :add-slot 'res (send graph :point-coordinate 1 
                                     (iseq (send graph :num-points))))
    (send graph :finish-next-frame #'(lambda ()
         (send control :slot-value 'text
               (format nil "Score test: ~a  ~a"  lab (funcall score)))
         (send graph :adjust-to-data)                       
         (send control :redraw)))
    (defmeth graph :update-res (selected)
     (let* ((n (iseq (send self :num-points)))
            (label (if selected "Absolute Residuals" "Squared Residuals"))
            (values (if selected (sqrt (slot-value 'res)) (slot-value 'res))))
       (send self :variable-label 1 label)
       (send self :draw-next-frame '(1) (list values))))
    (defmeth graph :hetero-change-model ()
      (flet ((get-x-list (names basis)
          (let ((selection
            (choose-subset-dialog 
             "Variance Model..."
             (append '("Fit values") (select names  basis)))))
       (unless selection (return-from :hetero-change-model))
       (unless (first selection)
          (error "Must include at least one term"))
       (first selection))))
        (let* ((basis (send base-mod :basis))
          (names (send base-mod :predictor-names))
          (x-list (get-x-list names basis)))
     (cond 
      ((member 0 x-list)
       (setf lab '("Fit-values"))
       (setf x-mod (send base-mod :fit-values)))
      (t
       (setf x-list (select basis (1- x-list)))
       (setf x-mod (select (send base-mod :x)  
                                (iseq (send base-mod :num-cases)) x-list))
       (setf lab (select names x-list))))
     (send self :draw-next-frame '(0 1) (funcall data)))))
    (send graph :add-control control2)
    (send graph :add-control control)
    graph))
