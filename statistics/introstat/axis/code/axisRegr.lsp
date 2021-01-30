
;;;;     AxisRegr.lsp © Robert A. Stine, 1992
;;;;                 Correlation object.

#||
    9 Apr 94 ... Adjust to work with timeAxis; runs test.
   15 Mar 94 ... Display of regression checks calc status.
   27 Feb 94 ... Regress parses booleans.
   23 Feb 94 ... Plotting function externally supplied.
   17 Jan 94 ... Fix predict for bivariate model. Add spinner.
    5 Jan 94 ... VIF dynamic slider plot; pvalues in regression.
    7 Jul 92 ... Created from correlation and regression files.
||#

;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisRegr")

;;;;

(require "axisUtil")
(require "axisBoot")

#||

(setf rm (regress (year gnp sales (* sales 2)) budget))  ; macro

(send rm :value)                     ; value of slope
(send rm :ci .9)                     ; for each slope
(send rm :plot-residuals)
(send rm :plot-diagnostics)          ; linked diagnostic plots
(send rm :plot-partial-residuals)
(send rm :plot-partial-regr)
(send rm :bootstrap 10 'random)    ; each slope

(setf rc (send regr-cmd-proto :new))

||#
;;;;
;;;;     EXTENSIONS TO REGRESSION OBJECT THAT DOES THE WORK
;;;;

(defun REGRESS (&rest args)    ; 2/27/94
  (let ((x (if (listp (first args))
               (first args)
               (list (first args)))) )
    ;  (format t "First elements ~a~%" (mapcar #'first x))
    (apply #'send axis-regression-proto :new
           (if (arrayp (first x)) (first x)
               (mapcar #'(lambda (var)
                           (if (member (first var) (list t nil)) ; boolean
                               (mapcar #'(lambda (x) (if x 1 0)) var)
                               var))
                       x))
           (rest args))))


(defproto AXIS-REGRESSION-PROTO
 '(rBoot fBoot
   vif
   plotFunc)
  ()
  regression-model-proto)

(defmeth axis-regression-proto :ISNEW (&rest args)
  (apply #'call-next-method args)
  )

(defmeth axis-regression-proto :PLOT-FUNCTION-IS (f)   ; 2/24/94
  (setf (slot-value 'plotFunc) f))

(defmeth axis-regression-proto :PLOT (&rest args)      ; 2/24/94
  (if (slot-value 'plotFunc)
      (apply (slot-value 'plotFunc) args)
      (apply #'regression-scatterplot args)
      ))
      

(defmeth axis-regression-proto :PRINT-TYPE ()
  (if (send self :weights) (format t "Weighted "))
  (format t "Least Squares Estimates for ~a:~%" (send self :response-name)))


(defmeth axis-regression-proto :DISPLAY ()                    ;  OVERRIDE
  (when (send self :needs-computing)              ; 3/15/94
        (format t "Recomputing estimates...~%")
        (send self :compute))
  (send self :print-type)
  (let* ((coefs   (coerce (send self :coef-estimates) 'list))
         (se-s    (send self :coef-standard-errors))
         (tStat   (/ coefs se-s))
         (pvals   (* 2 (- 1 (t-cdf (abs tStat) (send self :df)))))
         (x       (send self :x))
         (p-names (select (send self :predictor-names) (send self :basis))) )
    (print-matrix (bind-columns
                   (if (send self :intercept)
                       (combine (list "Variable" "Constant") p-names)
                       (cons "Variable" p-names)  )
                   (cons "Estimate" coefs)
                   (cons "Std Err " (/ (round (* 1000 se-s)) 1000))
                   (cons "t-Ratio"  (/ (round (* 100 tStat)) 100))
                   (cons "p-value"  (/ (round (* 1000 pvals)) 1000))  ))
    (format t "R Squared:             ~10,3f~%" (send self :r-squared))
    (format t "Sigma hat:             ~10g~%" (send self :sigma-hat))
    (format t "Number of cases:       ~10d~%" (send self :num-cases))
    (if (/= (send self :num-cases) (send self :num-included))
        (format t "Number of cases used:  ~10d~%" (send self :num-included)))
    (format t "~%")  (send self :print-anova)
    (format t "~%")))



;;;    Regression Model Extensions

(defmeth axis-regression-proto :VALUE ()      ; Standardize names
  (send self :coef-estimates))

(defmeth axis-regression-proto :N ()
  (send self :num-cases))

(defmeth axis-regression-proto :SE ()
  (send self :coef-standard-errors))

(defmeth axis-regression-proto :ESTIMATION-METHOD ()
  (if (send self :weights) "WLS" "OLS"))

(defmeth axis-regression-proto :CI (cc)
  (format t "~a% intervals for coefficients using " (* 100 cc))
  (if (send self :weights) 
      (format t "weighted least squares estimates:~2%")
      (format t "least squares estimates:~2%"))
  (let* ((b     (send self :value))
         (names (send self :predictor-names))
         (k     (length names))
         (tVal  (- (t-quant (* .5 (- 1 cc)) (send self :df))))
         (wid   (* tVal (send self :se)))
         (int   (mapcar #'(lambda (l u) (list l u))
                       (- b wid) (+ b wid)))   )
    (when (send self :intercept)
          (format t "~15a  ~10g   ~a ~%"
                  "Constant" (car b) (car int))
          (setf b   (cdr b))
          (setf int (cdr int))  )
    (dotimes (i k) 
             (cond 
               ((member i (send self :basis))
                (format t "~15a ~10g   ~a~%"
                        (select names i) (car b) (car int)   )
                (setf b (cdr b) int (cdr int))   )
               (t (format t "~15a    aliased~%" (select names i)))  ))
    ))

(defmeth axis-regression-proto :PLOT-RESIDUALS ()   ; OVERRIDE
  (let ((p (send self :plot
                 (send self :fit-values) (send self :residuals)
                 "Fitted" "Residuals"
                 :cases (send self :case-labels)))   )
    (send p :abline 0 0)
    p
    ))

(defmeth axis-regression-proto :SPIN-PLOT ()
  "Arguments: none.  Show spin plot for 2 covariate model."
  (if (= 2 (- (send self :num-coefs) (if (send self :intercept) 1 0)))
      (flet ((range (x) (list (min x) (max x))))
        (let* ((x  (column-list (send self :x)))
               (xNames (send self :predictor-names))
               (y (send self :y))
               (b (send self :coef-estimates))
               (p (spin-plot (list (first x) (second x) y)
                             :variable-labels (list
                                               (first xNames)
                                               (second xNames)
                                               (send self :response-name)
                                               )))
               )
          (send p :title "Regression Function")
          (send p :draw-axes)
          (send p :depth-cuing nil)
          (apply #'send p :abcplane  (if (send self :intercept) 
                                         b
                                         (cons  0 b) ))
          p
          ))))

; (def p (send rm :spin-plot))


(defmeth axis-regression-proto :PREDICT (x &key cc (nobs 1))
  "Arguments: x &key cc (nobs 1)
   Prediction, standard error/pred interval for nobs future values.
   If cc supplied, then gives prediction interval (low, pred, hi). 
   If nobs = 0, then gives the confidence interval for the mean."
  (unless (listp x) (setf x (list x)))
  (when (send self :has-slot 'rho)
        (format t "Using time series predictor...~%"))
  (if (= (if (send self :has-slot 'rho) (1+ (length x)) (length x))  ; rho?
         (if (send self :intercept)
             (1- (send self :num-coefs))
             (send self :num-coefs)))
      (let* ((x     (if (send self :has-slot 'rho)  ; kludge for AR
                        (cons (slot-value 'lasty)
                              (- x (* (slot-value 'rho) (slot-value 'lastx))))
                        x))
             (xf    (if (send self :intercept) (cons 1 x) x))
             (pred  (matmult xf (send self :coef-estimates)))
             (xtxi  (send self :xtxinv))
             (sig   (send self :sigma-hat))
             (fac   (if (= 0 nobs) 0 (/ 1 nobs)))
             (se    (* sig (sqrt (+ fac (matmult xf xtxi xf)))))   )
        (if cc
            (let* ((cc    (+ cc (/ (- 1 cc) 2)))
                   (tval  (t-quant cc (send self :df)))
                   (int   (* se tval))   )
              (list (- pred int) pred (+ pred int))   )
            (list pred se)))
      (format t "Incorrect number of covariates.~%")
      ))


(defmeth axis-regression-proto :TEST (h &key cc)
  "Arguments: h &key cc.  Test Ho h'beta = 0."
 (flet ((format-lc (h)
            (let ((vec (coerce h 'vector))
                  (res " "))
              (dotimes (i (length vec))
                       (if (not (= 0 (aref vec i)))
                           (setf res (strcat res (format nil "+ ~a b(~a) "
                                                         (aref vec i) i))))
                       )
              res))    )
   (if (= (length h) (send self :num-coefs))
       (let* ((coef  (send self :coef-estimates))
              (xtx   (send self :xtxinv))
              (sig   (send self :sigma-hat))
              (val   (matmult h coef))
              (se    (* sig (sqrt (matmult h xtx h))))
              (tStat (/ val se))
              (df    (send self :df))
              (pval  (* 2 (- 1 (t-cdf (abs tstat) df))))  )
         (format t "Test of linear combination: ~a = ~a~%"
                 val (format-lc h))
         (format t "   S.E. = ~a t = ~5,2f (df = ~a, p = ~5,3f) ~%"
                 se tStat df pval)
         (if cc
             (let ((half (* se (t-quant (+ cc (/ (- 1 cc) 2)) df))))
               (format t "~a Confidence interval: ~%" cc)
               (format t "   [ ~a , ~a ] ~%" (- val half) (+ val half))
               ))   )
       (format t "Incorrect number of terms.~%")
       )))
;;;;
;;;;     Save Dialog
;;;;

(defmeth axis-regression-proto :SAVE-ALIST ()
  ; Association list of method-selectors and names for save dialog.
 (let ((aList (list
               '(residual  . :residuals)
               '(fitted    . :fit-values)
               '(stanres   . :studentized-residuals)
               '(StudRes   . :externally-studentized-residuals)
               '(Leverage  . :leverages)
               '(CookD     . :cooks-distances) ))
       (wts (when (send self :weights) (list '(weights . :weights))))  )
   (append aList wts)))

                                       

;;;;
;;;;     Added Variable Plots
;;;;

(defmeth axis-regression-proto :PARTIAL-REGR-COEFS ()
  ; Array of regression coefs from deleting one carrier. Sweeping
  ; on swept columns removes that variable from the regression.
  (let* ((row    (send self :num-coefs))
         (p      (1- row))
         (sm     (send self :sweep-matrix))
         (yxCoef (make-array (list (1+ p)  p)))  ; Y on all but one X
         (xxCoef (make-array (list (1+ p)  p)))  ; X on other X's
         (cols   (iseq (1+ p)))
         (newSM  nil)  )
    (dolist (c (iseq 1 p))
            (setf newSM (first (sweep-operator sm (list c))))
            (setf (select yxCoef cols (1- c) )    ; delete one X
                  (transpose (select newSM   row   cols)))
            (setf (aref yxCoef c (1- c)) 0)      ; zero element for X
            (setf (select xxCoef cols (1- c) )  
                  (transpose (select newSM   c     cols)))
            (setf (aref xxCoef c (1- c)) 0)
            )
    (list xxCoef yxCoef)
    ))

; (send m :partial-regr-coefs)


(defmeth axis-regression-proto :PLOT-PARTIAL-REGR (&key (linked t))
  ; Partial regression residual plots, one for each carrier.
  (if (> 3 (send self :num-coefs))
      (message-dialog "Partial regression only useful for multiple regression.") 
      (let* ((ests   (send self :coef-estimates))
             (pc     (send self :partial-regr-coefs))
             (xxCoef (first pc))
             (yxCoef (second pc))
             (x      (send self :x))    (y      (send self :y))
             (n      (array-dimension x 0))
             (x      (bind-columns (repeat 1 n) x))
             (rows   (iseq n))
             (p      (1- (send self :num-coefs)))
             (cols   (iseq (1+ p)))
             (names  (slot-value 'predictor-names))
             (labels (send self :case-labels))
             (loc    50)  )
        (flet ((plot (k)
                     (let* ((ry (- y (matmult x (select yxCoef cols (1- k)))))
                            (rx (- (select x rows k)
                                   (matmult x (select xxCoef cols (1- k)))))
                            (name (select names (1- k)))
                            (p    (send self :plot
                                        (combine rx) (combine ry)
                                        (strcat "Partial " name) ""
                                        :cases labels)))
                       (send p :abline 0 (nth k ests))
                       (send p :title (format nil "Partial Regr: ~a" name))
                       (send p :location 10 loc)
                       (setf loc (+ 25 loc))
                       (send p :adjust-to-data)
                       (if linked (send p :linked t))
                       p
                       )))
          (mapcar #'plot (iseq 1 p))
          ))))


(defmeth axis-regression-proto :PLOT-PARTIAL-RESIDUALS (&key (linked t))
  ;  Partial regression residual plots, one for each carrier.
  (let* ((ests   (send self :coef-estimates))
         (x      (column-list (send self :x)))
         (names  (slot-value 'predictor-names))
         (resids (send self :residuals))
         (labels (send self :case-labels))
         (loc    50) )
    (flet ((plot (k)
                 (let* ((b    (select ests (1+ k)))
                        (name (select names k))
                        (p  (send self :plot ; 2/23/94
                             (select x k)
                             (+ resids (* b (select x k)))
                             (format nil "~a" name) "" :cases labels))  )
                   (send p :abline 0 b)
                   (send p :title (format nil "Partial Resid: ~a" name))
                   (send p :location 260 loc)
                   (setf loc (+ 25 loc))
                   (send p :adjust-to-data)
                   (if linked (send p :linked t))
                   p
                   )))
      (mapcar #'plot (iseq 0 (- (length ests) 2)))
      )))

;;;;
;;;;  ___________________  Variance Inflation Factors  ____________________
;;;;

(defmeth axis-regression-proto :VIF (&key (print t))
  ; Vector of variance inflation factors.
  (unless (send self :intercept)
          (format t "Use VIF only for models with an intercept.")
          (return))
  (unless (slot-value 'vif) (send self :calc-vif))
  (let ((vif (slot-value 'vif))   )
    (when print
          (format t "Variables and Square Roots of VIF's.~%")
          (mapcar #'(lambda (n v) (format t "~a --> ~a~%" n v))
                  (send self :predictor-names) (sqrt vif))   )
    vif))

  
(defmeth axis-regression-proto :CALC-VIF ()
  (setf (slot-value 'vif)
        (if (= 2 (send self :num-coefs))
            (list 1)
            (flet ((ss (x) (let ((dev (- x (mean x))))
                             (sum (* dev dev)))))
              (let* ((s     (send self :sigma-hat))
                     (coefs (cdr (iseq (send self :num-coefs))))
                     (ss    (mapcar #'ss (column-list (send self :x))))
                     (se  (cdr (send self :coef-standard-errors))) ) ; \ XXi
                (/ (* se se ss) (* s s))
                )))  ))


(defmeth axis-regression-proto :VIF-PLOT ()
  "Arguments: none. Builds the slider dialog for VIF."
  (if (< (send self :num-coefs) 3)
      (message-dialog "VIF plot only useful for multiple regression.")
      (let* ((var (send self :vif-dialog))
             (xName (select (send self :predictor-names) var))
             (yName (send self :response-name))
             (vif (select (send self :vif :print nil) var))
             (r2  (/ (- vif 1) vif))
             (coef (select (send self :coef-estimates) (1+ var)))
             (rows (iseq (length (send self :y))))
             (fit (send self :partial-fit var))
             (fit (- fit (mean fit)) 'list)
             (res (send self :residuals))
             (x   (first (column-list (select (send self :x) rows var))))
             (x   (coerce (- x (mean x)) 'list))
             (comp (+ res (* x coef)))
             (p   (plot-points x comp
                               :title "Partial Slider Plot"
                               :variable-labels (list xName yName)
                               :point-labels (send self :case-labels)))
             (s   (interval-slider-dialog
                   (list 0 1)
                   :title "VIF Lambda Slider" :initial 0 :nice nil :points 101
                   :action #'(lambda (lam)
                               (send p :point-coordinate
                                     0 rows (- x (* lam fit)))
                               (send p :point-coordinate
                                     1 rows (- comp (* (* lam coef) fit)))
                               (send p :title-is
                                     (format nil "VIF=~4,1f @ ~d%"
                                             (/ 1 (- 1 (* lam r2)))
                                             (floor (* 100 lam))   ))
                               (send p :redraw)
                               )))    )      
        (send p :add-subordinate s) (send p :size 250 200)
        (send p :add-slot 'title "VIF= 1.0 @ 0%")        ; kludge
        (defmeth p :title-is (title) (setf (slot-value 'title) title))
        (defmeth p :redraw ()
          (call-next-method)
          (send self :draw-string (slot-value 'title )
                (- (floor (* .5 (first (send self :size)))) 25) 25)   )
        (send p :abline 0 coef)  (def *vifPlot* p)
        (send p :showing-labels t)
        )))
#||
; (send p :point-symbol (which (< 0 fit)) 'cross) ; code up/dn
            ; (mapcar #'(lambda (a b c d)
            ;             (send p :add-lines (list (list a b) (list c d)))  )
            ;         x (- x fit) comp (- comp (* coef fit)))
||#

(defmeth axis-regression-proto :PARTIAL-FIT (xIndex)
  ; Computes fitted values regressing x_j on other x's.
  (let* ((xIndex  (1+ xIndex))               ; 1-based origin for constant
         (lastRow (send self :num-coefs))
         (cols    (iseq lastRow))      ; next holds coefs of Y and X on all but X
         (sm      (send self :sweep-matrix)) ; get original sweep matrix
         (newSM   (first (sweep-operator sm (list xIndex)))) ; remove this x
         (xCoef   (select newSM (list xIndex) cols))
         (j1      (setf (select xCoef 0 xIndex) 0))
         (n       (length (send self :y)))
         (x       (bind-columns (repeat 1 n)  (send self :x)))  )
    (first (column-list (matmult x (transpose xCoef))))
    ))
    

(defmeth axis-regression-proto :VIF-DIALOG ()
  ; Dialog for determining which variable to use.
  (let* ((titleItem (send text-item-proto :new "Select predictor for VIF plot."))
         (vif       (send self :vif :print nil))
         (itemList  (send choice-item-proto :new 
                          (mapcar #'(lambda (n v) (format nil "~15a  ~5,1f" n v))
                                  (send self :predictor-names) vif)))
         (canButton (send modal-button-proto :new "Cancel"))
         (okButton  (send modal-button-proto :new "OK" :action
                          #'(lambda () (send itemList :value)))) 
         (dialog (send modal-dialog-proto :new
                       (list titleItem itemList (list okButton canButton)))  )   )
    (if vif
        (send dialog :modal-dialog))
    ))


(defmeth axis-regression-proto :DURBIN-WATSON (&key (print t))
  ;Durbin-Watson statistic from current residuals."
  (let* ((r  (send self :residuals))
         (dw (/ (sum (^ (- (cdr r) (butlast r)) 2))
                (sum (* r r))))    )
    (when print 
          (format t "Durbin-Watson statistic = ~a (rho^ = ~6,3f)~%"
                  dw (/ (- 2 dw) 2)  ))
    dw))

(defmeth axis-regression-proto :RUNS-TEST (&key (print t))  ; 4/11/94
  (let* ((r    (send self :residuals))
         (n    (length r))
         (sign (if-else (< 0 r) 1 0))
         (n+   (count 1 sign))
         (n-   (count 0 sign))
         (nRun (1+ (count 1 (abs (difference sign))))) ; 1+num chg
         (eRun (- (/ (* 2 n+ n-) n) 1))
         (vRun (/ (* 2 n+ n- (- (* 2 n+ n-) n))
                  (* (* n n) (- n 1))))
         ( z   (/ (- nRun eRun) (sqrt vRun)))      )
    (when print
        (format t "~d runs with z = ~a (p=~6,3f)~%"
                nRun z (* 2 (- 1 (normal-cdf (abs z)))))  )
    z))
    

(defmeth axis-regression-proto :PLOT-BOX-COX ()
  "Arguments: none.  Builds Box-Cox transformation plot."
  (if (< (min (send self :Y)) .00001)
      (format t "Cannot use Box-Cox with negative responses.~%")
      (let* ((X     (matmult (send self :x-matrix) (send self :xtxinv)))
             (y     (coerce (send self :y) 'vector))
             (n     (length y))
             (gm    (^ (prod y) (/ 1 n)))
             (Xt      (transpose (send self :x-matrix)))
             (powers  (list -2 -1 -0.5 -0.25 0 .1 .25 .5 .75 1 1.25 1.5 1.75 2))
             (plot  nil)    )
        (labels ((projErr (lam)
                          (let* ((ty  (if (= 0 lam)
                                          (* (log y) gm)
                                          (/ (- (^ y lam) 1)
                                             (* lam (^ gm (- lam 1))))))
                                 (err (- ty (matmult X Xt ty)))  )
                            (- (* n (log (sum (* err err)))))  ))
                 (addLines (plot y)
                           (let ((max (max y))
                                 (cv  (chisq-quant .9 1))  )
                             (send plot :abline (- max cv) 0)
                             ))  )  
          (setf toPlot (mapcar #'projErr powers))
          (setf plot   (plot-points powers toPlot))
          (send plot :title (format nil "Box-Cox:~a" (send self :response-name)))
          (send plot :variable-label '(0 1) (list "Lambda" "2 Log Likelihood"))
          (addLines plot toPlot)
          (send plot :adjust-to-data)
          ))))


(defmeth axis-regression-proto :PLOT-DIAGNOSTICS (&key (linked t))
  (let* ((n   (length (send self :y)) )
         (lab (send self :case-labels))
         (p1 (send self :plot
              (send self :leverages) (send self :studentized-residuals)
              "Leverage" "Studentized Residual"
              :cases lab))
         (p2 (send self :plot
              (iseq n) (send self :cooks-distances)
              "Case Number" "Cook's D"
              :cases lab))   )
    (send p1 :title "Residual on Leverage")
    (send p1 :location 0 50)
    (send p1 :abline 0 0)
    (send p1 :showing-labels t)
    (send p1 :adjust-to-data)
    (send p1 :linked linked)
    (send p2 :title "Cooks D")
    (send p2 :location 300 50)
    (send p2 :adjust-to-data)
    (send p2 :linked linked)
    (list p1 p2)
    ))

;;;;
;;;;     BOOTSTRAP
;;;;

(defmeth axis-regression-proto :BOOTSTRAP-FUNCTION (method)
   (if (eq 'random method) 
      #'(lambda (i)
          (let* ((y   (select (send self :slot-value 'y) i))
                 (all (iseq (array-dimension (send self :slot-value 'x) 1)))
                 (x (select (send self :slot-value 'x) i all))
                 (m (make-sweep-matrix x y))
                 (p (array-dimension x 1))  )
            (coerce (compound-data-seq
                     (select (first (sweep-operator m (iseq 1 p)))
                             (1+ p) (iseq 0 p))) 'list)
            ))
      ; else fixed resampling from residuals
      #'(lambda (i) 
          (let* ((y (+ (send self :fit-values)
                       (select (send self :residuals) i)))
                 (x (send self :slot-value 'x))
                 (m (make-sweep-matrix x y) )
                 (p (array-dimension (send self :slot-value 'x) 1))   )
            (coerce (compound-data-seq
                     (select (first (sweep-operator m (iseq 1 p)))
                             (1+ p) (iseq 0 p))) 'list)
            ))
      ))



(defmeth axis-regression-proto :BOOTSTRAP (b method)
  (format t "~a resampling method.~%" method)
  (let* ((n      (length (send self :y)))
         (name   (strcat (if (eq method 'random) "Random" "Fixed")
                              " Regr"))   )
      (bootstrap name
                 (list (send self :bootstrap-function method))
                 `(resample (iseq ,n))
                 (send *active-icon-window* :dataset)
                 b 
                 :labels (cons 'Constant
                               (mapcar
                                #'(lambda (n)
                                    (string-to-symbol n :prefix "Coef-"))
                                (send self :predictor-names)  )))
    ))
