(provide 'RXridge)

(setf *sim-reg* nil)

;; *****************************************************************************
;; ************************* The Menu Definition *******************************
;; *****************************************************************************
(defun SRX-menu()
"Function Args: ()
Creates a menu to interactively create a regression-model object."
  (let* ((srx-menu  (send menu-proto :new " softRXridge ")) ; create new menu
         (load-comp (send menu-item-proto                   ; first menu-item
                          :new "Load Data and Compute"     
                          :action #'RXridge-load-compute))
         (kill-menu (send menu-item-proto                  ; second menu-item
                          :new "Remove Menu"
                          :action #'(lambda() (send srx-menu :remove))))
         (dash0     (send dash-item-proto :new))
        )
         ; arrange menu items
        (send srx-menu :append-items dash0 load-comp dash0 kill-menu)
        (send srx-menu :install)                                     
        srx-menu                           ; give back menu-object
  )
)

(defun RXridge-load-compute()
"Function Args: ()
Interactively asks for data-file, response index, regressor indices and
creates a regression object and the RXridge regression menu."
  (let* ((dat-fil (open-file-dialog))                    ; get data file name
         (dat-fil (if (probe-file dat-fil) 
                      dat-fil
                      (block tmp (message-dialog (format nil "Datei \"~a\" existiert nicht !"
                                                             dat-fil)) 
                                 (top-level nil))))
         (num-col (count-file-columns dat-fil))          ; get number of columns in datafile
         (nam-fil (concatenate 'string (subseq dat-fil 0 (- (length dat-fil) 3)) 
                          "nam"))                        ; replace 'dat' by 'nam' gives
                                                         ; names-file name
         (names   (when (probe-file nam-fil)
                        (read-data-file nam-fil)))       ; read names (optionally)
         (names   (if names
                      (map 'list #'(lambda(n) (format nil "~A" n)) names)
                      (map 'list #'(lambda(n) (format nil "Var~d" n)) 
                           (iseq num-col))))             ; make list of namestrings
         (data    (read-data-columns dat-fil num-col))   ; read data columns from file
         (resp    (choose-item-dialog "Choose Response" names))   ; ask for response index
         (y       (select data resp))                    ; save response data
         (ynam    (select names resp))                   ; save response name
         (data    (remove y data))                       ; leave only regressors in data ...
         (names   (remove ynam names))                   ; ... and names
         (preds   (if resp 
                      (first (choose-subset-dialog "Choose Predictors" names
                                :initial (iseq (- num-col 1)))
                      )
                      nil
                  ))                                     ; choose the regressors
         (srx-model nil)                                 ; just a variable
        )
        (when preds
          (setf srx-model (regression-model (select data preds) y   ; create the regression
                                  :response-name ynam               ; object ...
                                  :predictor-names (select names preds)
                                  ))
                                                                        
          (send srx-model :RXridge-menu (subseq dat-fil 0 (- (length dat-fil) 4)))
                                             ; ... and send the RXridge-menu message to it
          (send srx-model :RXridge-compute-summary) ; show principal axis summary
        )
        srx-model                            ; give back the regression object
   )
)

(defmeth regression-model-proto :RXridge-menu(&optional (title "RXridgeREG"))
  (let* ((RXridge   (send menu-proto :new title))
         (set-rscal (send menu-item-proto
                          :new "Choice of Regressor-Response Scaling "
                          :action #'(lambda ()
                                       (send self :RXridge-set-rscale))))
         (summary   (send menu-item-proto
                          :new "Compute Principal Axis Summary "
                          :action #'(lambda ()
                                       (send self :RXridge-compute-summary))))
         (dash0     (send dash-item-proto :new))
         (crlq      (send menu-item-proto
                          :new "Identify Most Likely Path Q-shape "
                          :action #'(lambda ()
                                       (send self :RXridge-compute-crlq))))
         (set-path  (send menu-item-proto
                          :new "Select Shrinkage Path Shape "
                          :action #'(lambda () 
                                       (send self :RXridge-pshape))))
         (set-steps (send menu-item-proto
                          :new "Set Number of STEPS per Shrinkage Unit "
                          :action #'(lambda ()
                                       (send self :RXridge-set-steps))))
         (max-likl  (send menu-item-proto
                          :new "Identify Most Likely Shrinkage M-extent "
                          :action #'(lambda ()
                                       (send self :RXridge-compute-max-likelihood))))
         (traces    (send menu-item-proto
                          :new "Display Shrinkage TRACE Plots "
                          :action #'(lambda ()
                                       (send self :RXridge-traces))))
         (trace-dat (send menu-item-proto
                          :new "List Shrinkage Trace Details "
                          :action #'(lambda ()
                                       (send self :RXridge-compute-trace-dat t))))
         (vrr       (send menu-item-proto 
                          :new "Visual Re-Regression and Influence "
                          :action #'(lambda () 
                                       (send self :RXridge-plot-vrr))))
         (sizsig    (send menu-item-proto
                          :new "Plot Component Size vs. Significance "
                          :action #'(lambda ()
                                       (send self :RXridge-plot-sizsig))))
         (true-parm (send menu-item-proto
                          :new "Specify True Values of Parameters "
                          :action #'(lambda ()
                                       (send self :RXridge-true-parms))))
         (true-dat (send menu-item-proto
                          :new "List True Shrinkage Details "
                          :action #'(lambda ()
                                       (send self :RXridge-true-risks t))))
         (true-risk (send menu-item-proto
                          :new "Display Expected Traces, True MSE Risks "
                          :action #'(lambda ()
                                       (send self :RXridge-risks))))
         (simu-dat (send menu-item-proto
                          :new "List Shrinkage Simulation Details "
                          :action #'(lambda ()
                                       (send self :RXridge-loss-simulation t))))
         (simu      (send menu-item-proto
                          :new "Display Simulated Traces, True SE Losses "
                          :action #'(lambda ()
                                       (send self :RXridge-losses))))
         (killmenu  (send menu-item-proto :new "Remove this MENU "
                          :action #'(lambda () 
                                       (send self :delete-slot 'rx-qp)
                                       (send self :delete-slot 'rx-steps)
                                       (send self :delete-slot 'rx-scale)
                                       (send self :delete-slot 'rx-shape)
                                       (send self :delete-slot 'rx-xyscale)
                                       (send self :delete-slot 'rx-summary)
                                       (send self :delete-slot 'rx-crx)
                                       (send self :delete-slot 'rx-trace-dat)
                                       (send self :delete-slot 'rx-truegs)
                                       (send self :delete-slot 'rx-true-dat)
                                       (send self :delete-slot 'rx-simu-dat)
                                       (setf *sim-reg* nil)
                                       (send RXridge :remove)
                                 )))
        )
        (send RXridge :append-items dash0
                                  set-rscal summary dash0
                                  crlq set-path set-steps max-likl dash0
                                  trace-dat traces dash0
                                  vrr sizsig dash0
                                  true-parm true-dat true-risk simu-dat simu dash0
                                  killmenu)
        (send RXridge :install)
        (send self :add-slot 'rx-qp 0)          ; for Parameter Q
        (send self :add-slot 'rx-steps 4)       ; for number of steps
        (send self :add-slot 'rx-scale 1)       ; for type of scaling
        (send self :add-slot 'rx-shape 0)       ; for type of path shape
        (send self :add-slot 'rx-xyscale nil)   ; list of scaling factors
        (send self :add-slot 'rx-summary nil)   ; for OLS (comp tstat
                                               ;          ols ols-risk rho)
        (send self :add-slot 'rx-crx nil)       ; scaled/centered X-matrix
        (send self :add-slot 'rx-trace-dat nil) ; for RXridge (mcal bstar risk
                                               ;            exev infd  delta)
        (send self :add-slot 'rx-truegs nil)    ; for TRUE gammas and sigma
        (send self :add-slot 'rx-true-dat nil)  ; for RXridge (mcal beta risk
                                               ;            exev infd  delta)
        (send self :add-slot 'rx-simu-dat nil)  ; for RXridge (mcal beta loss
                                               ;            delta)
        RXridge
  )
)

;; *****************************************************************************
;; ******************* The Access Methods for Slot Values **********************
;; *****************************************************************************

(defmeth regression-model-proto :RXridge-qp (&optional qp)
"Message Args: ()
Without argument, gets parameter q, else sets it to qp."
  (when qp
    (setf (slot-value 'rx-qp) qp))
  (slot-value 'rx-qp)
)

(defmeth regression-model-proto :RXridge-steps (&optional steps)
"Message Args: (&optional steps)
Without argument gets the number of steps, else sets it to steps."
  (when steps
    (setf (slot-value 'rx-steps) steps))
  (slot-value 'rx-steps)
)

(defmeth regression-model-proto :RXridge-scale (&optional scale)
"Message Args: (&optional scale)
Without argument gets the type of scaling
(0=just centered, 1=centered/scaled, 2=1 with rescaled results),
else sets it to scale."
  (when scale
    (setf (slot-value 'rx-scale) scale))
  (slot-value 'rx-scale)
)

(defmeth regression-model-proto :RXridge-shape (&optional shape)
"Message Args: (&optional shape)
Without argument, gets the type of path shape
(0=Q-shape, 1=CROSR, 2=Garrote, 3=Lasso),
else sets it to shape."
  (when shape
    (setf (slot-value 'rx-shape) shape))
  (slot-value 'rx-shape)
)

(defmeth regression-model-proto :RXridge-xyscale (&optional xyscale)
"Message Args: (&optional yxscale)
Without argument gets (list xscale yscale), the list xscale of scaling 
factors of X and the scaling factor yscale of y. Else sets them to xyscale."
  (when xyscale
    (setf (slot-value 'rx-xyscale) xyscale))
  (slot-value 'rx-xyscale)
)

(defmeth regression-model-proto :RXridge-summary (&optional (summary nil set))
"Message Args: (summary)
If summary is nil, the summary for next steps will have to be recomputed.
Else the RXridge summary is stored."
  (if set
      (setf (slot-value 'rx-summary) summary)
      (slot-value 'rx-summary)
  )
)

(defmeth regression-model-proto :RXridge-crx (&optional (crx nil set))
"Message Args: (summary)
If crx is nil, get scaled/centered X-matrix, else set it to crx."
  (if set
      (setf (slot-value 'rx-crx) crx)
      (slot-value 'rx-crx)
  )
)

(defmeth regression-model-proto :RXridge-truegs (&optional (true-parm nil set))
"Message Args: (true-parm)
If true-parm is nil, the true-parm values & seed/state need to be respecified.
Else the RXridge true-parm values are stored."
  (if set
      (setf (slot-value 'rx-truegs) true-parm)
      (slot-value 'rx-truegs)
  )
)

(defmeth regression-model-proto :RXridge-comp()
"Message Args: ()
Get uncorrelated components of OLS"
  (when (send self :RXridge-summary) (first  (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-tstat()
"Message Args: ()
Get t statistics for Hypothesis: true gamma equal 0"
  (when (send self :RXridge-summary) (second (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-svdx()
"Message Args: ()
Get Singular Value Decompostion (list H S G) of X=H*S*G'."
  (when (send self :RXridge-summary) (third  (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-ols()
"Message Args: ()
Get OLS estimates."
  (when (send self :RXridge-summary) (nth 3  (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-ols-risk()
"Message Args: ()
Get diagonal Scaled Mean Square Error for OLS"
  (when (send self :RXridge-summary) (nth 4  (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-rho()
"Message Args: ()
Get principal correlations of y with principal coordinates."
  (when (send self :RXridge-summary) (nth 5  (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-ols-s2()
"Message Args: ()
Get unbiased estimator of the residual variance."
  (when (send self :RXridge-summary) (nth 6  (slot-value 'rx-summary))))

(defmeth regression-model-proto :RXridge-trace-dat (&optional (trace-dat nil set))
"Message Args: (trace-dat)
If trace is nil, RXridge Trace Data for next plots will have to be recomputed.
Else the RXridge Trace Data are stored."
  (if set
      (setf (slot-value 'rx-trace-dat) trace-dat)
      (slot-value 'rx-trace-dat)
  )
)

(defmeth regression-model-proto :RXridge-mcal()
"Message Args: ()
Get list of Multicollinearity Allowances."
  (when (send self :RXridge-trace-dat) (first  (slot-value 'rx-trace-dat))))

(defmeth regression-model-proto :RXridge-bstar()
"Message Args: ()
Get list of Shrunken Coefficients for Q-shape path at MCAL-extent."
  (when (send self :RXridge-trace-dat) (second (slot-value 'rx-trace-dat))))

(defmeth regression-model-proto :RXridge-risk()
"Message Args: ()
Get list of Scaled MSE Risk for Shrunken Coefficients"
  (when (send self :RXridge-trace-dat) (third  (slot-value 'rx-trace-dat))))

(defmeth regression-model-proto :RXridge-exev()
"Message Args: ()
Get list of Excess Eigenvalues for Shrunken Coefficients"
  (when (send self :RXridge-trace-dat) (nth 3  (slot-value 'rx-trace-dat))))

(defmeth regression-model-proto :RXridge-infd()
"Message Args: ()
Get list of Inferior Direction Cosines for Shrunked Coefficients"
  (when (send self :RXridge-trace-dat) (nth 4  (slot-value 'rx-trace-dat))))

(defmeth regression-model-proto :RXridge-delta()
"Message Args: ()
Get list of Shrinkage Factors"
  (when (send self :RXridge-trace-dat) (nth 5  (slot-value 'rx-trace-dat))))

(defmeth regression-model-proto :RXridge-trug()
"Message Args: ()
Get TRUE values for uncorrelated components of OLS"
  (when (send self :RXridge-truegs) (first  (slot-value 'rx-truegs))))

(defmeth regression-model-proto :RXridge-trus()
"Message Args: ()
Get TRUE value for the residual standard deviation."
  (when (send self :RXridge-truegs) (second (slot-value 'rx-truegs))))

(defmeth regression-model-proto :RXridge-soption()
"Message Args: ()
Get random number generator seed/state option, 0=saved, 1=current, 2=new."
  (when (send self :RXridge-truegs) (third (slot-value 'rx-truegs)))
)

(defmeth regression-model-proto :RXridge-seed (&optional seed)
"Message Args: ()
Without argument, gets saved random number generator seed/state;
With argument, sets saved seed to seed argument."
  (when seed
     (setf (nth 3 (slot-value 'rx-truegs)) seed))
  (when (send self :RXridge-truegs)
     (nth 3 (slot-value 'rx-truegs)))
)

(defmeth regression-model-proto :RXridge-true-dat (&optional (true-dat nil set))
"Message Args: (true-dat)
If true is nil, RXridge True Data for next plots will have to be recomputed.
Else the RXridge True Data are stored."
  (if set
      (setf (slot-value 'rx-true-dat) true-dat)
      (slot-value 'rx-true-dat)
  )
)

(defmeth regression-model-proto :RXridge-mtrue()
"Message Args: ()
Get list of Multicollinearity Allowances = MCAL-extents of Shrinkage."
  (when (send self :RXridge-true-dat) (first  (slot-value 'rx-true-dat))))

(defmeth regression-model-proto :RXridge-btrue()
"Message Args: ()
Get list of Expected Coefficients for Q-shape path at MCAL-extent."
  (when (send self :RXridge-true-dat) (second (slot-value 'rx-true-dat))))

(defmeth regression-model-proto :RXridge-rtrue()
"Message Args: ()
Get list of True Scaled MSE Risks for Shrunken Coefficients"
  (when (send self :RXridge-true-dat) (third  (slot-value 'rx-true-dat))))

(defmeth regression-model-proto :RXridge-etrue()
"Message Args: ()
Get list of True Excess Eigenvalues for Shrunken Coefficients"
  (when (send self :RXridge-true-dat) (nth 3  (slot-value 'rx-true-dat))))

(defmeth regression-model-proto :RXridge-itrue()
"Message Args: ()
Get list of True Inferior Direction Cosines for Shrunked Coefficients"
  (when (send self :RXridge-true-dat) (nth 4  (slot-value 'rx-true-dat))))

(defmeth regression-model-proto :RXridge-dtrue()
"Message Args: ()
Get list of Shrinkage Factors applied to Principal Axes"
  (when (send self :RXridge-true-dat) (nth 5  (slot-value 'rx-true-dat))))

(defmeth regression-model-proto :RXridge-simu-dat (&optional (simu-dat nil set))
"Message Args: (simu-dat)
If simu is nil, RXridge Simu Data for next plots will have to be recomputed.
Else the RXridge Simu Data are stored."
  (if set
      (setf (slot-value 'rx-simu-dat) simu-dat)
      (slot-value 'rx-simu-dat)
  )
)

(defmeth regression-model-proto :RXridge-msimu()
"Message Args: ()
Get list of Multicollinearity Allowances = MCAL-extents of Shrinkage."
  (when (send self :RXridge-simu-dat) (first  (slot-value 'rx-simu-dat))))

(defmeth regression-model-proto :RXridge-bsimu()
"Message Args: ()
Get list of Simulated Coefficients for Q-shape path at MCAL-extent."
  (when (send self :RXridge-simu-dat) (second (slot-value 'rx-simu-dat))))

(defmeth regression-model-proto :RXridge-lsimu()
"Message Args: ()
Get list of Simulated Scaled SE Losses for Shrunken Coefficients"
  (when (send self :RXridge-simu-dat) (third  (slot-value 'rx-simu-dat))))

(defmeth regression-model-proto :RXridge-dsimu()
"Message Args: ()
Get list of Shrinkage Factors applied to Principal Axes"
  (when (send self :RXridge-simu-dat) (nth 3  (slot-value 'rx-simu-dat))))

(defmeth regression-model-proto :RXridge-ysimu()
"Message Args: ()
Get list of Simulated Response Values"
  (when (send self :RXridge-simu-dat) (nth 4  (slot-value 'rx-simu-dat))))

;; *****************************************************************************
;; ********* Some user-friendly methods for setting slot values ****************
;; *****************************************************************************

(defmeth regression-model-proto :RXridge-set-qp (&optional (qp nil))
"Message Args: (&optional qp)
With argument sets the global Parameter Q to 'qp', else calls Slider-Dialog"
  (if qp
      (send self :RXridge-qp qp)
      (let* ((action #'(lambda (q) 
                         (send self :RXridge-qp (/ (- q 10) 2))
                         (send self :RXridge-trace-dat nil)
                         (send self :RXridge-true-dat nil)
                         (send self :RXridge-simu-dat nil)
                         (pause 10)
                         ; with other Q all trace data must be recomputed
                       ))
            )
            (send (sequence-slider-dialog (iseq 0 20) 
                     :title "Set Q" 
                     :text "Q = " 
                     :display (rseq -5 5 21) 
                     :action action )
                  :value (round  (* 2 (+ (send self :RXridge-qp) 5))))
     )
  )
)

(defmeth regression-model-proto :RXridge-set-steps (&optional (steps nil))
"Message Args: (&optional steps)
With argument sets the Number of Iterations to 'steps', else calls Slider-Dialog"
  (if steps                                 
      (send self :RXridge-steps steps)
      (let* ((action #'(lambda (steps) 
                         (send self :RXridge-steps steps)
                         (send self :RXridge-trace-dat nil)
                         (send self :RXridge-true-dat nil)
                         (send self :RXridge-simu-dat nil)
                         (pause 10)
                         ; for other numbers of steps, all trace
                         ; data must be recomputed
                       ))
            )
            (send (sequence-slider-dialog (iseq 1 40) 
                     :title "Set Steps" 
                     :text "Steps = " 
                     :action action)
                  :value (- (send self :RXridge-steps) 1))
      )
  )
)

(defmeth regression-model-proto :RXridge-set-rscale ()
"Message Args: ()
Dialogbox to choose type of scaling"
  (let* ((rold   (send self :RXridge-scale))
         (rscale (choose-item-dialog "Choose Type of Scaling"
                    (list "center only "
                          "center and scale"
                          "center and scale with rescaled output")
                    :initial rold
                 ))
        )
        (when rscale                           ; if scale type is choosen
           (when (not (= rscale rold))         ; and different from old one,
             (send self :RXridge-scale rscale)   ; set new rscale.
             (send self :RXridge-trace-dat nil)  ; Upon a scale change, tstat,
             (send self :RXridge-true-dat  nil)  ; comp,and also all trace
             (send self :RXridge-simu-dat  nil)  ; data must be recomputed.
             (send self :RXridge-summary   nil)  ; Also, trug and trus must
             (send self :RXridge-truegs    nil)  ; be respecified.
           )
        )
  )
)

(defmeth regression-model-proto :RXridge-pshape ()
"Message Args: ()
Dialogbox to choose basic type of path shape"
  (let* ((rold   (send self :RXridge-shape))
         (rshape (choose-item-dialog "Choose Shape of Shrinkage Path"
                    (list "2 parameter family: set Q-shape..."
                          "Constant Ratio of Shrinkage Ratios"
                          "Garrote: Breiman-like path"
                          "Lasso: Tibshirani-like path")
                    :initial rold
                 ))
        )
        (when rshape                           ; if shape-type is choosen
           (when (not (= rshape rold))         ; and different from old one,
             (send self :RXridge-shape rshape)   ; set new rshape.
             (send self :RXridge-trace-dat nil)
             (send self :RXridge-true-dat nil)
             (send self :RXridge-simu-dat nil)
             (pause 10)
             ; with other path shapes, all trace data must be recomputed
           )
           (when (= rshape 0)                  ; when shape is zero,
             (send self :RXridge-set-qp)         ; set path Q-shape.
           )
        )
  )
)

;; *****************************************************************************
;; ******************* Auxiliary functions *************************************
;; *****************************************************************************

(defun pp-matrix (a &key (size 12) (acc 7) 
                         (stream *standard-output*)  )
"Args: (matrix &key (size 10) (acc 7) (stream *standard-output*))
Prints MATRIX to STREAM in a nice form with length size and accuracy acc 
for floats."
  (unless (matrixp a) (error "not a matrix - ~a" a))
  (dolist (x (row-list a))
          (let ((n (length x)))
            (dotimes (i n)
                     (let ((y (aref x i)))
                       (cond
                         ((integerp y) (format stream "~vd" size y))
                         ((floatp y)  (if (= y 0.0)
                                          (format stream "~v,vf" size  acc y)
                                          (if (< (1+ (floor (/ (log (abs y)) (log 10)))) 
                                                 (- size acc))
                                              (format stream "~v,vf" size  acc y)
                                              (format stream "~v,ve" size  (- acc 3) y)
                                          )))
                         (t (format stream "~vf" size y))))
                     (if (< i (- n 1)) (format stream " "))
            )
            (format t "~%")
          )
  )
  (format t "~%") 
  nil
)

(defun plot-matrix(xvec ymat &key (title "Matrix-Plot") 
                                  variable-labels
                                  point-labels)
"Plot a matrix in different colors"
  (let* ((colors (do ((col (color-symbols))) 
                     ((> (length col) (first (array-dimensions ymat))) col) 
                     (setf col (append col (color-symbols)))
                 ))
         (colors (remove 'WHITE colors))
         (colors (remove 'YELLOW colors))
         (labs   point-labels)
         (plot (plot-points xvec (first (row-list ymat))
                :title title
                :variable-labels variable-labels
                :point-labels (first point-labels)
                :color (first colors)))
        )
        ; (send plot :size 400 300)
        (send plot :range 1 (min ymat)  (max ymat))
        (send plot :showing-labels t)
        (send plot :add-lines xvec (first (row-list ymat))
                   :color (first colors))
        (send plot :linked t)
        (map 'list #'(lambda(y lab c) 
                      (send plot :add-points xvec y
                                 :point-labels lab
                                 :color c)
                      (send plot :add-lines  xvec y :color c)
                     )
                   (rest (row-list ymat)) (rest point-labels) (rest colors)
        )
        plot
  )
)

(defun get-number-dialog (s v)
"Function Args: (string-label value-initial)
Display string and request modification/acceptance of default value."
  (let* ((answer nil)
        )
        (setf answer (get-string-dialog
                          (format nil "~a: " s) :initial v))
        (setf answer (when answer (read-from-string answer)))
  )
)

(defun trace-label(m n)
"Function Args: (MCAL-extent curve-number)
Define labels for points on Shrinkage/Ridge TRACE plots."
  (map 'list #'(lambda(mi) (format nil "(~d)m=~2,3f" n mi)) m)
)

(defun lev-cutoff ( n p &optional (sig_niv 0.95))
"Function Args: (n-obs p-vars (sig-level))
Position warning line for leverages ...Chatterjee and Hadi(1988), page 32."
   (let* ((c (/ (- n p) (- p 1)))
          (f (f-quant sig_niv (- p 1) (- n p)))
         )
         (/ (+ f (/ c n)) (+ f c))
     )
)

(defun mstep(mobj kinc p qp pshape rho eqm1)
"Function Args: (mcal-objective k-extent p-vars q-shape path-shape prin-corr eigvals-to-power)
Compute k-extent corresponding to a given mcal-objective by Newtonian descent."
 (if (>= mobj p)
      (list " inf" (make-list p :initial-element 0))
      (let ((funs (- mobj p))
            (pc2 (make-list p :initial-element 0.000001))
            (pca (make-list p :initial-element 0.001))
            (dfac (make-list p :initial-element 1))
           )
           (dotimes (i p)
             (setf (select pc2 i) (max (^ (select rho i) 2) 0.000001))
             (setf (select pca i) (max (abs (select rho i)) 0.001))
           )
           (cond (  (<= mobj 0)          ;Least Squares Solution
                    (list 0 dfac)
                 )
                 (  (= pshape 1)         ;CROSR Path
                    (do ((derivs 0)
                       )
                       ((<= (abs funs) 0.000001) kinc)
                       (setf funs (+ (- mobj p)
                                     (sum (/ 1 (+ 1 (/ kinc pc2))))))
                       (setf derivs (sum (/ (/ 1 pc2) (^ (+ 1 (/ kinc pc2)) 2))))
                       (setf kinc (+ kinc (/ funs derivs)))
                    )
                    (list kinc (/ 1 (+ 1 (/ kinc pc2))))
                 )
                 (  (= pshape 2)         ;GARROTE Path
                    (do ((derivs 0)
                       )
                       ((<= (abs funs) 0.0001) kinc)
                       (setf funs (- mobj p))
                       (setf derivs 0)
                       (dotimes (i p)
                         (cond ((< kinc (select pc2 i))
                                (setf derivs (+ derivs (/ 1 (select pc2 i))))
                                (setf (select dfac i) (- 1 (/ kinc
                                  (select pc2 i))))
                                (setf funs (+ funs (select dfac i)))
                               )
                               (t (setf (select dfac i) 0) )
                         )
                       )
                       (setf kinc (+ kinc (/ funs derivs)))
                    )
                    (list kinc dfac)
                 )
                 (  (= pshape 3)         ;LASSO Path
                    (do ((derivs 0)
                       )
                       ((<= (abs funs) 0.0001) kinc)
                       (setf funs (- mobj p))
                       (setf derivs 0)
                       (dotimes (i p)
                         (cond ((< kinc (select pca i))
                                (setf derivs (+ derivs (/ 1 (select pca i))))
                                (setf (select dfac i) (- 1 (/ kinc
                                  (select pca i))))
                                (setf funs (+ funs (select dfac i)))
                               )
                               (t (setf (select dfac i) 0) )
                         )
                       )
                       (setf kinc (+ kinc (/ funs derivs)))
                    )
                    (list kinc dfac)
                 )
                 (t (if (= qp 1)
                      (setf kinc (/ (- mobj) funs))
                      (do ((derivs 0)
                         )
                         ((<= (abs funs) 0.000001) kinc)
                         (setf funs (+ (- mobj p)
                                       (sum (/ 1 (+ 1 (* kinc eqm1))))))
                         (setf derivs (sum (/ eqm1 (^ (+ 1 (* kinc eqm1)) 2))))
                         (setf kinc (+ kinc (/ funs derivs)))
                      )
                    )
                    (list kinc (/ 1 (+ 1 (* kinc eqm1))))
                 )
           )
      )
  )
) 

;; *****************************************************************************
;; ******************* The Methods for Computing Results ***********************
;; *****************************************************************************

(defmeth regression-model-proto :RXridge-compute-summary ()
"Message Args: ()
Computes Summary for Shrinkage/Ridge Regression"

  (format t "~2%")
  (format t "*********************** Shrinkage/Ridge Regression ********************~2%")
  (format t "  softRX freeware (c) by Bob Obenchain for XLisp-Stat 2.1 Release 3+. ~%");
  (format t "  Version 9605...Vast majority of Lisp code by Bernhard Walter(1994). ~2%");
  (format t "***********************************************************************~2%")

  (let* ((rscale (send self :RXridge-scale))
         (incl   (send self :included))                 ; which cases are included
         (xmat   (apply #'bind-rows 
                   (select (row-list (send self :x)) (which incl))))
                    ; get the regressor-matrix and use just included cases
         (yvec   (select (send self :y) (which incl)))
         (n      (send self :num-included))    ; number of included cases
         (p      (- (send self :num-coefs) 1)) ; number of non-constant coeffs

         (xscale (if (= rscale 0) 
                     (make-list p :initial-element 1)
                     (map 'list #'standard-deviation (column-list xmat))
                 ))       ; rscale = 0 ==> xscale = list of 1's
                          ; else       ==> xscale = list of stdev(x_i)'s
         (crx    (apply #'bind-columns 
                    (map 'list 
                       #'(lambda(xi mi si) (/ (- xi mi) si)) 
                                (column-list xmat)                   
                                (map 'list #'mean (column-list xmat))
                                 xscale)))  ; (X - mean(X)) / xscale
                    ; ==> crx is the centered/scaled design matrix

         (yscale (if (= rscale 0)
                     1
                     (standard-deviation yvec) ; rscale=0 ==> yscale = 1
                 ))                            ; else     ==> yscale = stdev(y)
         (cry    (coerce (/ (- yvec (mean yvec)) yscale) 'vector))
                                 ; ==> cry is the centered rescaled response

         (sx     (sv-decomp crx))            ; SVD of crx = H' * L^(-1/2) * G
         (sv     (coerce (second sx) 'list)) ; list of sing. values L_i^(-1/2)

         (smse   (matmult (third sx) (diagonal (/ 1 (^ sv 2)))
                     (transpose (third sx))))  ; G'*L-inv*G = scaled MSE
         (risk   (diagonal smse))
         (tsmse  (sum risk))

         (Hty    (matmult (transpose (first sx)) cry))       ; = H'y
         (comp   (matmult (diagonal (/ 1 (second sx))) Hty)) ; = c = L^(-1/2) * H'y
                                                             ; uncorr. components
         (b0     (matmult (third sx) comp))                  ; = G*c = OLS

         (ssy    (sum (^ cry 2)))                    ; = y'y 
         (rho    (/ Hty (sqrt ssy)))                 ; = H'y / y'y = princ. correl.'s
         (res    (- cry (matmult crx b0)))           ; = y - X'b   = OLS residuals
         (s2     (/ (sum (^ res 2)) (- n p 1)))      ; unbiased estim. of resid.
                                                     ; variance in uncorrel.
                                                     ; homosced. model
         (varrho (/ s2 ssy))                         ; sample variance of rho_i
         (tstat  (/ rho (sqrt varrho)))              ; t-statistic for 
                                                     ; H_0: \gamma = 0
        )

        (when (= rscale 2)
          (setf b0    (*    (/ yscale xscale)     b0   ))
          (setf risk  (* (^ (/ yscale xscale) 2)  risk ))
        )

        (format t "Principal Axis Summary Statistics of Ill-Conditioning~2%")
        (format t "(Regressors and Response are centered")
        (cond ( (= rscale 0) (format t ") ~2%") )
              ( (= rscale 1) (format t "/scaled) ~2%") )
              ( (= rscale 2) (format t "/scaled and output is rescaled) ~2%"))
        )
        (format t "    Eigenvalue  Sing.Value  Uncorr.Comp.  Princ.Corr.")
        (format t "      t-stat~%")
        (dotimes (i p)
           (format t "~3d ~10,5f  ~10,5f    ~10,5f   ~10,5f  ~10,5f ~%" 
                     (+ 1 i) (^ (select sv i) 2) (select sv i) (select comp i)
                     (select rho i) (select tstat i))
        )

        (format t "~2%")
        (format t "Adjusted Response Sum-of-Squares: ~10,5f ~2%" ssy)
        (format t "Residual Mean Square for Error:   ~10,5f ~2%" s2)
        (format t "Estimate for Residual Std. Error: ~10,5f ~2%" (sqrt s2))


        (send self :RXridge-summary (list comp tstat sx b0 risk rho s2))
        (send self :RXridge-xyscale (list xscale yscale))
        (send self :RXridge-crx crx)

        (list crx cry)
  )
)

(defmeth regression-model-proto :RXridge-compute-trace-dat (&optional (print nil))
"Message Args: (&optional (print nil))
Computes TRACE Statistics for Shrinkage/Ridge Regression.
If argument print is not nil, then results are printed."

  (when (not (send self :RXridge-summary))   ; if necessary
        (send self :RXridge-compute-summary) ; compute comp,tstat,...
  )

  (format t "~%Compute RXridge TRACE Statistics ~2%")

  (let* ((svdx   (send self :RXridge-svdx))   ; Get SVD of X
         (G      (third svdx))                ; Direction Cosine G
         (sv     (second svdx))               ; singular values
         (ev     (coerce (^ sv 2) 'list))     ; the eigenvalues
         (tstat  (send self :RXridge-tstat))
         (p      (length sv))
         (n      (send self :num-included))
         (steps  (send self :RXridge-steps))
         (rscale (send self :RXridge-scale))
         (scale  (send self :RXridge-xyscale))
         (pshape (send self :RXridge-shape))
         (rho    (send self :RXridge-rho))
         (qp     (min (max (send self :RXridge-qp) -5) 5))
         (eqm1   (if (= qp 1)
                     (make-list p :initial-element 1)    ; (1....1)        or
                     (^ ev (- qp 1))                     ; \lambda_i^(Q-1)
         ))                                              ; used for \delta_i
         (mobj   nil)
         (iter   nil)
         (kinc   0)
         (binc   nil)
         (vecr   nil)
         (compr  nil)
         (lowr   nil)
         (smse   nil)
         (lowb   nil)
         (rinc   nil)
         (tinc   nil)
         (emse   nil)
         (sfac   nil)
         (eig    nil)
         (einc   nil)
         (cinc   nil)
         (dinc   nil)
         (bstar  (send self :RXridge-ols))              ; OLS estimate
         (risk   (send self :RXridge-ols-risk))         ; Diagonal SMSE of OLS
         (exev   (make-list p :initial-element 0))
         (infd   (make-list p :initial-element 0))
         (delta  (make-list p :initial-element 1))
         (cold   (make-list p :initial-element 1))
         (tsmse  (list (sum risk)))
         (konst  '(0.0))
         (mcal   '(0.0))
        )

        (format t "Steps: ")
        (dotimes (inc (* p steps))           ; inc = 0 .... p*steps-1
          (format t "~3d  " inc)
          (when (= (mod (+ 1 inc) 8) 0) (format t "~%       "))
          (setf mobj  (/ (+ inc 1) steps))
          (setf iter  (mstep mobj kinc p qp pshape rho eqm1))
          (setf kinc  (first iter))
          (setf dinc  (coerce (second iter) 'list)) ; list of delta shrinkage factors
          (setf binc  (matmult G                          ; G *
                               (diagonal dinc)            ; D *
                               (send self :RXridge-comp)))  ; c = Shrink. Est.
          (setf vecr  (bind-columns (* (- 1 dinc) (/ tstat sv))))
          (setf compr (+ (* (/ (- n p 3) (- n p 1))
                            (matmult vecr (transpose vecr)))
                         (diagonal (/ (- (* 2 dinc) 1) ev))))  ; \hat{T} {6.15}
          (setf lowr  (/ (^ dinc 2) ev))  ; lower bound on scaled MSE(D*c)
          (setf compr (+ (- compr (diagonal (diagonal compr)))
                         (diagonal (pmax lowr (diagonal compr))))) ; T^* {6.19}
          (setf smse  (matmult G compr (transpose G)))
          (setf lowb  (matmult G (diagonal lowr) (transpose G)))
          (setf rinc  (pmax (diagonal smse) (diagonal lowb)))
          (setf tinc  (sum rinc))

          (setf emse  (- (diagonal (/ 1 ev)) compr))
          (setf sfac  (max (/ (min (abs (diagonal emse))) 100) 0.01))
          (setf eig   (eigen (/ emse sfac)))
          (setf einc  (sort-data (coerce (* (first eig) sfac) 'list)))
          (setf cinc  (if (< (first einc) -0.0001)
                        (let ((ctmp (matmult G (first (last (second eig)))))
                             )
                             (when (= rscale 2)
                               (setf ctmp (/ (matmult ctmp
                                                      (diagonal (first scale)))
                                             (sqrt (sum (^ ctmp 2)))))
                             )
                             (when (< (sum (* cold ctmp)) 0)
                               (setf ctmp (- ctmp))
                             )
                             (setf cold ctmp)
                             (coerce ctmp 'list)
                        )
                        (make-list p :initial-element 0)
                      ))
          (when (= rscale 2)
            (setf binc (matmult (diagonal (/ (second scale) (first scale)))
                                binc))
            (setf rinc (matmult (diagonal (^ (/ (second scale) (first scale)) 2))
                                rinc))
          )
          (setf bstar (bind-columns bstar binc))
          (setf risk  (bind-columns risk  rinc))
          (setf exev  (bind-columns exev  einc))
          (setf infd  (bind-columns infd  cinc))
          (setf delta (bind-columns delta dinc))
          (setf tsmse (append       tsmse (list tinc)))
          (setf konst (append       konst (list kinc)))
          (setf mcal  (append       mcal  (list (- p (sum dinc)))))
        )
        (format t "~2%")
        (when print
          (format t "Matrix BSTAR =~%")
          (pp-matrix (transpose bstar))
          (format t "Matrix RISK =~%")
          (pp-matrix (transpose risk))
          (format t "Matrix EXEV =~%")
          (pp-matrix (transpose exev))
          (format t "Matrix INFD =~%")
          (pp-matrix (transpose infd))
          (format t "Matrix DELTA =~%")
          (pp-matrix (transpose delta))
          (format t "Matrix (TSMSE Konst MCAL) =~%")
          (pp-matrix (bind-columns tsmse konst mcal))
        )

        (send self :RXridge-trace-dat (list mcal bstar risk exev infd delta))
        nil
    )
)

(defmeth regression-model-proto :RXridge-traces()
"Message Args: ()
Tests that all TRACE data are computed and calls plot method."
  (when (not (send self :RXridge-trace-dat))
        (send self :RXridge-compute-trace-dat)
  )
  (send self :RXridge-plot-traces (send self :RXridge-mcal )
                                (send self :RXridge-bstar)
                                (send self :RXridge-risk )
                                (send self :RXridge-exev )
                                (send self :RXridge-infd )
                                (send self :RXridge-delta)
                                nil)
)

(defmeth regression-model-proto :RXridge-compute-max-likelihood ()
"Message Args: ()
Computes Classical, Empirical Bayes and Random Coefficient Likelihoods
in Shrinkage/Ridge Regression."

  (when (not (send self :RXridge-trace-dat))     ; if necessary
        (send self :RXridge-compute-trace-dat)   ; compute dinc,...
  )

  (format t 
    "~%Compute Classical, Empirical Bayes and Random Coefficient Likelihoods ~2%")

  (if (< (sum (^ (send self :RXridge-rho) 2)) 1) ; RSQUARE must be smaller than 1.0
      (let* ((rho   (abs (send self :RXridge-rho)))
             (tstat (abs (send self :RXridge-tstat)))
             (rsq   (sum (^ rho 2)))
             (delta (rest (column-list (send self :RXridge-delta))))
             (mcal  (send self :RXridge-mcal))
             (steps (send self :RXridge-steps))
             (p     (- (send self :num-coefs) 1)) ; number of non-constant coeffs
             (n     (send self :num-included))
             (s2    (send self :RXridge-ols-s2))
             (svdx  (send self :RXridge-svdx))
             (bstar (send self :RXridge-bstar))
             (pshape (send self :RXridge-shape))

             (C     nil)
             (E     nil)
             (R     nil)
             (omd   nil)
             (ddomd nil)
             (rxi   nil)
             (slik  nil)
             (clik  nil)
             (ebay  nil)
             (sr2d  nil)
             (rcof  nil)
             (minC  nil)
             (minE  nil)
             (minR  nil)
            )
            (format t "Steps: ")
            (dotimes (inc (* p steps))           ; inc = 0 .... p*steps-1
    
              (format t "~3d  " inc)
              (when (= (mod (+ 1 inc) 8) 0) (format t "~%       "))
              (setf omd   (pmax (- 1 (select delta inc)) 1.0e-13))    ; Arrgghhhh !
              (setf ddomd (/ (coerce (select delta inc) 'list) omd))
              (setf rxi   (sum (* rho (sqrt ddomd))))
              (setf slik  (/ 2.0 (+ rxi (sqrt (+ (* 4 n) (^ rxi 2))))))
              (setf clik  (- (+ (* 2 n (log slik)) (sum ddomd))
                             (/ rxi slik)
                             (* n (log (/ (- 1 rsq) n)))))
              (setf ebay  (sum (- (* (^ tstat 2) omd) (log omd))))
              (setf sr2d  (sum (* (select delta inc) (^ rho 2))))
              (setf rcof  (- (* n (log (/ (- 1 sr2d) (- 1 rsq))))
                             (sum (log omd))))
              (setf C     (append C (list clik)))
              (setf E     (append E (list ebay)))
              (setf R     (append R (list rcof)))
            )
            (setf minC (map 'list #'(lambda(m) (if m "<<<" "   ")) (= C (min C))))
            (setf minE (map 'list #'(lambda(m) (if m "<<<" "   ")) (= E (min E))))
            (setf minR (map 'list #'(lambda(m) (if m "<<<" "   ")) (= R (min R))))
            (format t "~2%")
            (format t " MCAL       CLIK             EBAY            RCOF~%")
            (format t " 0.00       inf              inf             inf~%")
            (dotimes (i (* p steps))
               (format t "~5,2f     ~10,5f ~2A   ~10,5f ~2A  ~10,5f ~2A~%"
                         (coerce (select mcal (+ i 1)) 'float)
                         (select C i) (select minC i)
                         (select E i) (select minE i)
                         (select R i) (select minR i) )
            )
            (format t " MCAL       CLIK             EBAY            RCOF~2%")
            (format t "The shrinkage M-extent = MCAL most likely to be optimal along~%")
            (cond ((= pshape 0)
                   (format t "the path of shape Q=~3,1f within the 2-parameter family can~%"
                      (send self :RXridge-qp))
                  )
                  ((= pshape 1)
                   (format t "the >> Constant-Ratio-Of-Shrinkage-Ratios path << can~%")
                  )
                  ((= pshape 2)
                   (format t "the >>>> GARROTE shape (Breiman-like) path <<<< can~%")
                  )
                  (t
                   (format t "the >>>> LASSO shape (Tibshirani-like) path <<<< can~%")
                  )
            )
            (format t "depend upon whether one uses a Classical (CLIK), Empirical Bayes~%")
            (format t "(EBAY) or Random Coefficient (RCOF) criterion.  In each case,~%")
            (format t "the objective is to minimize the minus-two-log-likelihood~%")
            (format t "statistics listed above.~2%")
            (format t "(Regressors and Response are centered")
            (if  (= (send self :RXridge-scale) 0)
                 (format t ") ~2%")
                 (format t " and scaled) ~2%")
            )
            t
      )
      (format t 
       "Maximum Likelihood ridge shrinkage cannot be applied when RSQUARE = 1.0~2%"
      )
  )
)

(defmeth regression-model-proto :RXridge-compute-crlq
                                   (&optional (qmin -5) (qmax 5) (nq 21))
"Message Args: (&optional (qmin -5) (qmax 5) (nq 21))
Computes classical maximum likelihood choice of shrinkage path Q-shape at
nq values within [qmin,qmax] as well as the M-extent along each path."

  (when (not (send self :RXridge-summary))       ; if necessary
        (send self :RXridge-compute-summary)     ; compute sv, rho, ...
  )

  (format t 
    "~%Classical Max.Likelihood choice of Shrinkage Q-shape and M-extent ~2%")

  (let* ((sv    (second (send self :RXridge-svdx)))
         (p     (length sv))
         (n     (send self :num-included))
         (rho   (abs (send self :RXridge-rho)))
         (r2    (sum (^ rho 2)))
         (ctmp  0)
         (ktmp  0)
         (qnow  0)
         (sqm1  0)
         (r2c2  0)
         (qvec  nil)
         (crlq  nil)
         (mvec  nil)
         (kvec  nil)
         (chisq nil)
         (mCHI  nil)
        )
        (dotimes (i nq)
          (setf qnow (/ (- (+ (* (- qmin qmax) (+ 1 i))
                              (* nq qmax)) 
                           qmin)
                        (- nq 1)))
          (setf qvec (append qvec (list qnow)))
          (if (= qnow 1.0)
              (setf s1mq (make-list p :initial-element 1))
              (setf s1mq (^ sv (- 1 qnow)))
          )
          (setf sq2 (sum (^ s1mq 2)))
          (setf ctmp (/ (sum (* s1mq rho)) (sqrt (* r2 sq2))))
          (setf crlq (append crlq (list ctmp)))
          (setf r2c2 (* r2 (^ ctmp 2)))
          (setf ktmp (/ (* sq2 (- 1 r2c2)) (* n r2c2)))
          (setf kvec (append kvec (list ktmp)))
          (setf s1mq (* ktmp (^ s1mq -2)))
          (setf mvec (append mvec (list (sum (/ s1mq (+ 1 s1mq))))))
          (setf chisq (append chisq (list (* n (log (/ (- 1 r2c2) (- 1 r2)))))))
        )
        (setf mCHI (map 'list #'(lambda(m) (if m "<<<" "   "))
              (= chisq (min chisq))))

        (format t "(Regressors and Response are centered")
        (if  (= (send self :RXridge-scale) 0)
             (format t ") ~2%")
             (format t " and scaled) ~2%")
        )
    
        (format t "   Q        CRL(Q)      M           K         CHISQ ~%")
        (dotimes (i nq)
            (format t " ~5,2f  ~10,5f  ~7,3f     ~11,5g ~7,2f ~2A~%"
                         (select qvec i)  (select crlq i)
                         (select mvec i)  (select kvec i)
                         (select chisq i) (select mCHI i) )
            )
        (format t "   Q        CRL(Q)      M           K         CHISQ ~2%")
        (format t "The shrinkage path Q-shape most likely to lead to overall~%")
        (format t "minimum Mean-Squared-Error (MSE) risk is the Q-shape that~%")
        (format t "maximizes CRL(Q) and minimizes CHISQ in the above listing.~2%")
  )
)

;; *****************************************************************************
;; *********************** The Plot Methods and functions **********************
;; *****************************************************************************

(defmeth regression-model-proto :RXridge-plot-sizsig()
"Message Args: ()
Plots absolute values of uncorrelated components vs statistical significance"
  (when (not (send self :RXridge-summary))
        (send self :RXridge-compute-summary)
  )
  (let* ((comp  (send self :RXridge-comp))
         (tstat (send self :RXridge-tstat))
         (p     (length comp))
         (plot (plot-points (abs tstat) (abs comp) 
                           :title "Num. Size vs Stat. Sig."
                           :variable-labels '("abs t-stat" "abs. size")
                           :point-labels (map 'list #'num-to-string (iseq 1 p))))
        )
        ; (send plot :size 400 350)
        (send plot :add-lines (append '(0) (coerce (abs tstat) 'list))
                              (append '(0) (coerce (abs comp)  'list)))
        plot
  )
)

(defmeth regression-model-proto :RXridge-plot-traces(mcal bstar risk exev infd delta true)
"Message Args: (bstar risk exev infd delta)
Asks for wanted trace-plots and plots them."

  (let* ((wish-list (if true
                        (choose-subset-dialog "Which TRACES should be plotted?"
                          (list "MCAL vs Expected Coefficients "   ; mcal <-> bstar
                                "MCAL vs Scaled True MSE       "   ; mcal <-> risk
                                "MCAL vs True Excess Eigenval. "   ; mcal <-> exev 
                                "MCAL vs True Inferior Direct. "   ; mcal <-> infd 
                                "MCAL vs Shrinkage Pattern     "   ; mcal <-> delta
                          )
                          :initial (iseq 5)
                        )
                        (choose-subset-dialog "Which TRACES should be plotted?"
                          (list "MCAL vs Ridge Coefficients"   ; mcal <-> bstar
                                "MCAL vs Scaled MSE Risk   "   ; mcal <-> risk
                                "MCAL vs Excess Eigenvalues"   ; mcal <-> exev 
                                "MCAL vs Inferior Direction"   ; mcal <-> infd 
                                "MCAL vs Shrinkage Pattern "   ; mcal <-> delta
                          )
                          :initial (iseq 5)
                        )
                    )
         )
         (p        (- (send self :num-coefs) 1)) ; number non-constant coeffs
         (qp       (send self :RXridge-qp))
         (pshape   (send self :RXridge-shape))
         (anot     (cond ((= pshape 0)
                         (format nil "Q=~3,1f" qp)
                        )
                        ((= pshape 1)
                         (format nil "CROSR")
                        )
                        ((= pshape 2)
                         (format nil "GARROTE")
                        )
                        (t
                         (format nil "LASSO")
                        )
                  )
         )
         (labs     (map 'list
                        #'(lambda(i) 
                             (trace-label mcal i)
                        )
                        (iseq 1 p)
                   ))
        )
        (when (member 4 (first wish-list))
          (plot-matrix mcal delta
                       :point-labels labs
                       :title (format nil "MCAL vs. Delta (~a)" anot))
        )
        (when (member 3 (first wish-list))
          (send
            (plot-matrix mcal infd
                         :point-labels labs
                         :title (format nil "MCAL vs. InferiorDIR (~a)" anot))
            :abline 0 0)
        )
        (when (member 2 (first wish-list))
          (send
            (plot-matrix mcal exev
                         :point-labels labs
                         :title (format nil "MCAL vs. ExcessEV's (~a)" anot))
            :abline 0 0)
        )
        (when (member 1 (first wish-list))
          (plot-matrix mcal risk
                       :point-labels labs
                       :title (format nil "MCAL vs. SMSE (~a)" anot))
        )
        (when (member 0 (first wish-list))
          (send 
            (plot-matrix mcal bstar
                         :point-labels labs
                         :title (format nil "MCAL vs. Coeffs (~a)" anot))
            :abline 0 0)
        )
 )
)

(defmeth regression-model-proto :RXridge-plot-vrr ()
"Message Args: ()
Plot shrinkage pred. vs y and leverage vs stan.resid.^2"

  (when (not (send self :RXridge-trace-dat))     ; if necessary
        (send self :RXridge-compute-trace-dat)   ; compute dinc,...
  )

  (let* ((yscale   (second (send self :RXridge-xyscale)))
         (incl     (send self :included))               ; which cases are included
         (yvec     (select (send self :y) (which incl)))
         (my       (mean yvec))
         (cry      (coerce (/ (- yvec my) yscale) 'vector))

         (qp       (send self :RXridge-qp))
         (pshape   (send self :RXridge-shape))
         (anot     (cond ((= pshape 0)
                         (format nil "Q=~3,1f" qp)
                        )
                        ((= pshape 1)
                         (format nil "CROSR")
                        )
                        ((= pshape 2)
                         (format nil "GARROTE")
                        )
                        (t
                         (format nil "LASSO")
                        )
                  )
         )
         (mcal     (send self :RXridge-mcal))
         (bstar    (column-list (send self :RXridge-bstar)))
         (delta    (column-list (send self :RXridge-delta)))
         (H        (first (send self :RXridge-svdx)))
         (Hty      (coerce (matmult (transpose H) cry) 'list))
         (lev      (map 'list #'(lambda(hi) (sum (* hi hi)))
                                   (row-list H))) ; Leverages
         (n        (send self :num-included))
         (s2       (sqrt (send self :RXridge-ols-s2)))
         (D        (first delta))
         (p        (length D))

         (fit      (matmult H (coerce (* D Hty) 'vector)))
         (xstd     (standard-deviation fit))
         (xscale   (if (> xstd 0) xstd 1))
         (xstar    (/ (- fit (mean fit)) xscale))
         (res      (- cry fit))
         (st-res   (map 'list #'(lambda(ri Hi)
                     (/ (^ ri 2)
                            (* s2
                                 (- (/ (- n 1) n)
                                    (sum (* (- (* 2 D) (^ D 2))
                                            (^ Hi 2)))))))
                     res (row-list H)))

         (vrrplot (plot-points xstar cry
                    :title (format nil "Visual Re-Regression, ~a, M=~5,2f" anot 0)
                    :point-labels (map 'list #'num-to-string (iseq 1 n))
                    :variable-labels '("Predicted" "Response" )))

         (resplot (plot-points lev st-res
                    :title (format nil "Leverages / Outliers, ~a, M=~5,2f" anot 0)
                    :point-labels (map 'list #'num-to-string (iseq 1 n))
                    :variable-labels '("Leverage" "Stan.Resid.^2" )))

         (re-plot #'(lambda(m)
              (let* ((D2   (select Delta m))
                     (fit2 (matmult H (coerce (* D2 Hty) 'vector)))
                     (xstd2   (standard-deviation fit2))
                     (xscale2 (if (> xstd2 0) xstd2 1))
                     (xstar2  (/ (- fit2 (mean fit2)) xscale2))
                     (res2 (- cry fit2))
                     (mxx (max xstar2))
                     (mnx (min xstar2))
                     (shr-res (map 'list #'(lambda(ri Hi)
                                 (/ (^ ri 2)
                                        (* s2
                                             (- (/ (- n 1) n)
                                                (sum (* (- (* 2 D2) (^ D2 2))
                                                        (^ Hi 2)))))))
                               res2 (row-list H)))
                     (shr-lev (map 'list #'(lambda(hi) (sum (* hi D2 hi)))
                                              (row-list H))) ; Shrinkage Leverages
              
                     (vrr-sel   (send vrrplot :points-selected))
                     (res-sel   (send resplot :points-selected))
                    )
                    (send vrrplot :clear-points :draw nil)
                    (send vrrplot :clear-lines :draw nil)
                    (send vrrplot :add-points xstar2 cry
                                  :point-labels (map 'list
                                      #'num-to-string (iseq 1 n)))
                    (if (> (- mxx mnx) 0.01)
                        (let* ((abl (regression-model xstar2 cry
                                 :print nil))
                               (fty (send abl :fit-values))
                               (maxy (max fty))
                               (miny (min fty))
                              )
                              (send vrrplot :add-lines (list mnx mxx)
                                   (list miny maxy) :color 'RED)
                              (send vrrplot :add-lines (list mnx mxx)
                                   (list (* mnx xscale2) (* mxx xscale2))
                                   :color 'BLUE)
                        )
                        (send vrrplot :add-lines (list (min xstar) (max xstar))
                                   (list 0 0) :color 'BLUE)
                    )
                    (send vrrplot :title (format nil
                               "Visual Re-Regression, ~a, M=~5,2f"
                                anot (select mcal m)))
                    (send vrrplot :points-selected vrr-sel)
              
                    (send resplot :clear-points :draw nil)
                    (send resplot :add-points shr-lev shr-res
                                  :point-labels (map 'list #'num-to-string
                                                    (iseq 1 n)))
                    (send resplot :title (format nil
                               "Leverages / Outliers, ~a, M=~5,2f"
                               anot (select mcal m)))
                    (send resplot :points-selected res-sel)
              ))
         )
         (infl-cont #'(lambda(i)
                       (let* ((in1 (rseq
                                 (/ i (second (send resplot :range 0)))
                                 (second (send resplot :range 1)) 25) )
                              (lv2 (rseq
                                 (/ i (second (send resplot :range 1)))
                                 (second (send resplot :range 0)) 25) )
                              (in2 (map 'list #'(lambda(x) (/ i x)) lv2))
                              (infl (sort-data (append in1 in2)))
                              (ilev (map 'list #'(lambda(x) (/ i x)) infl))
                             )
                             (send resplot :clear-lines :draw nil)
                             (send resplot :add-lines ilev infl :color 'BLUE)
                             (send resplot :add-lines (send resplot :range 0)
                                           '(4 4)
                                           :type 'dashed)
                             (send resplot :add-lines (list (lev-cutoff n p)
                                                            (lev-cutoff n p))
                                           (send resplot :range 1)
                                           :type 'dashed)
                       )
                      )
         )
        )
        ; (send vrrplot :size 300 300)
        (send vrrplot :linked t)
        (send vrrplot :showing-labels t)
        (let* ((maxx (max xstar))
               (minx (min xstar))
              )
              (if (> maxx minx)
                  (let* ((abl (regression-model xstar cry
                              :print nil))
                         (fty (send abl :fit-values))
                         (maxy (max fty))
                         (miny (min fty))
                        )
                        (send vrrplot :add-lines (list minx maxx)
                                (list miny maxy) :color 'RED)
                        (send vrrplot :add-lines (list minx maxx)
                                (list (* minx xscale) (* maxx xscale))
                                :color 'BLUE)
                   )
              )
        )
        ; (send resplot :size 300 300)
        (send resplot :linked t)
        (send resplot :showing-labels t)
        (send resplot :range 1 0 (max  5 (max st-res)))
        (send resplot :add-lines (send resplot :range 0) '(4 4)
                      :type 'dashed)
        (send resplot :add-lines (list (lev-cutoff n p) (lev-cutoff n p))
                                 (send resplot :range 1)
                      :type 'dashed)

        (sequence-slider-dialog (rseq 0.0001 (* (max st-res) (max lev)) 99)
                                :title   "Overall Influence"
                                :text    "INFL ="
                                :action   infl-cont)

        (sequence-slider-dialog (iseq (length mcal))
                                :display (map 'list #'(lambda(x)
                                            (/ (float (round (* x 1000))) 1000)
                                                      ) mcal)
                                :title   "Visual-Re-Regression"
                                :text    "MCAL ="
                                :action   re-plot)
        nil
  )
)

(defmeth regression-model-proto :RXridge-true-parms ()
"Message Args: ()
Specify values of true regressor components and true error standard deviation."
  (let* ((temp (if (send self :RXridge-truegs)
                  (send self :RXridge-trus)
                  (sqrt (send self :RXridge-ols-s2))
               )
         )
         (trug (if (send self :RXridge-truegs)
                  (send self :RXridge-trug)
                  (send self :RXridge-comp)
               )
         )
         (cseed (make-random-state nil))
         (schoice (if (send self :RXridge-truegs)
                      (send self :RXridge-soption)
                      0
                  )
         )
         (sseed (if (send self :RXridge-truegs)
                    (send self :RXridge-seed)
                    (make-random-state t)
                )
         )
         (trus (get-number-dialog "Specify TRUE Error Standard Deviation"
                     (format nil "~6,3f" temp)))
         (i 0)
        )
        (format t "TRUE Parameter Values...~2%")
        (format t "True Error Standard Deviation = ~f ~%" trus)
        (dotimes (i (length trug))
            (setf temp (select trug i))
            (setf (select trug i)
               (get-number-dialog
                 (format nil "Specify TRUE Uncorrelated Component No. ~d" (+ 1 i))
                 (format nil "~6,3f" temp)
               )
            )
            (format t "True Uncorrelated Component No. ~d = ~f ~%"
               (+ 1 i) (select trug i)
            )
        )
        (format t "Saved   Random Number Generator Seed/State. ~s~%"
            sseed)
        (format t "Current Random Number Generator Seed/State. ~s~%"
            cseed)
        (setf schoice (choose-item-dialog "Random Number Generation"
                        (list "retain saved seed/state"
                              "use  current seed/state"
                              "generate new seed/state")
                              :initial schoice
                        ))
        (if (= schoice 1) (setf sseed cseed))
        (format t "~2%")
        (send self :RXridge-truegs (list trug trus schoice sseed))
  )
)

(defmeth regression-model-proto :RXridge-risks()
"Message Args: ()
Tests that all TRUE data are computed and calls plot method."
  (when (not (send self :RXridge-true-dat))
        (send self :RXridge-true-risks)
  )
  (send self :RXridge-plot-traces (send self :RXridge-mtrue )
                                (send self :RXridge-btrue )
                                (send self :RXridge-rtrue )
                                (send self :RXridge-etrue )
                                (send self :RXridge-itrue )
                                (send self :RXridge-dtrue )
                                t)
)

(defmeth regression-model-proto :RXridge-true-risks (&optional (print nil))
"Message Args: (&optional (print nil))
Given true regressor components and true error standard deviation, compute
true MSE risks implied by shrinkage.
If argument print is not nil, then results are printed."

(when (not (send self :RXridge-summary))   ; if necessary
      (send self :RXridge-compute-summary) ; compute comp,tstat,...
)
(when (not (send self :RXridge-truegs))    ; if necessary
      (send self :RXridge-true-parms)      ; specify TRUE values...
)

  (let* ((trus (send self :RXridge-trus))
         (trug (send self :RXridge-trug))
        )
        (when (and trus trug)
          (if (<= (first (last (coerce (second (send self :RXridge-svdx)) 'list))) 0)
              ;; then
              (format t "Simulation requires XMAT to have Full Column Rank.~%")
              ;; else
              (let* ((svdx   (send self :RXridge-svdx)) ; Get SVD of X
                     (G      (third svdx))              ; Direction Cosine G
                     (sv     (second svdx))             ; singular values
                     (ev     (coerce (^ sv 2) 'list))   ; eigenvalues
                     (rscale (send self :RXridge-scale))
                     (steps  (send self :RXridge-steps))
                     (scale  (send self :RXridge-xyscale))
                     (xscale (first  scale))
                     (qp     (min (max (send self :RXridge-qp) -5) 5))
                     (pshape (send self :RXridge-shape))
                     (rho    (send self :RXridge-rho))
                     (p      (length sv))
                     (n      (send self :num-included))
                     (maxinc (* p steps))
                     (risk   (send self :RXridge-ols-risk))
                     (tsmse  (sum risk))
                     (eqm1   (if (= qp 1)
                                 (make-list p :initial-element 1)    ; (1....1)     or
                                 (^ ev (- qp 1))                     ; \lambda_i^(Q-1)
                     ))                                              ; used for \delta_i
                     (fact   (+ (^ trus 2) 
                                (/ (sum (* trug ev trug)) 
                                   (1- n)
                                )
                             ))
                     (when (> rscale 0)
                           (trug   (/ trug (sqrt fact)))
                                       ; rescale, preserving signal/noise ratios
                           (trus   (/ trus (sqrt fact)))
                                       ; expected response sum-of-squares = n-1
                     )
                     (beta   (matmult G trug)) ; BETA, true regression coefficients
                     (when   (= rscale 2)
                             (setf beta (/ beta xscale))
                             (setf risk (/ risk (^ xscale 2)))
                     )
                     (idty   (identity-matrix p))
                     (delta  (make-list p :initial-element 1))
                                       ; COLUMN VECTOR OF DELTA FACTORS
                     (d      (diagonal delta)) ; DIAGONAL SHRINKAGE FACTORS
                     (mobj   nil)
                     (iter   nil)
                     (kinc   0)
                     (binc   nil)
                     (rbias  nil)
                     (compr  nil)
                     (smse   nil)
                     (rinc   nil)
                     (tinc   nil)
                     (emse   nil)
                     (sfac   nil)
                     (eig    nil)
                     (einc   nil)
                     (cinc   nil)
                     (dinc   nil)
                     (exev   (make-list p :initial-element 0))
                     (infd   (make-list p :initial-element 0))
                     (delta  (make-list p :initial-element 1))
                     (cold   (make-list p :initial-element 1))
                     (tsmse  (list (sum risk)))
                     (konst  '(0.0))
                     (mcal   '(0.0))
                   )

                   (when print
                     (format t "~%True Risks in Shrinkage Regression ~2%")
                     (when (> rscale 0)
                           (format t "Rescale response/regressors, preserving all signal/noise ~%")
                           (format t "ratios, so that expected response sum-of-squares = n-1. ~2%")
                     )
                     (format t "(Regressors and Response are centered")
                     (cond ( (= rscale 0) (format t ") ~2%") )
                           ( (= rscale 1) (format t "/scaled) ~2%") )
                           ( (= rscale 2) (format t "/scaled and output is rescaled) ~2%"))
                     )
                     (format t "Principal Axis Summary Statistics of Ill-Conditioning...~2%")
                     (format t "Eigenval Sing.Val   Uncorr.Comp.    Beta ~%")
                     (dotimes (i (length ev))
                        (format t "~8,4f   ~8,4f   ~8,4f   ~8,4f ~%"
                             (elt ev i) (elt sv i) (elt trug i) (elt beta i))
                     )
                     (format t "~2%")
                     (format t "True Risks for the TWO HYPERPARAMETER ridge family ~%")
                     (format t "with SHAPE fixed at Q = ~3,2f are being calculated.~2%" qp)
                   )
                   (format t "Steps: ")        
                   (dotimes (inc maxinc) 
                      (format t "~3d " inc)
                      (when (= (mod (+ 1 inc) 8) 0) (format t "~%       "))
                      (setf mobj  (/ (+ inc 1) steps))
                      (setf iter  (mstep mobj kinc p qp pshape rho eqm1))
                      (setf kinc  (first iter))
                      (setf dinc  (coerce (second iter) 'list))
                                          ; list of \delta_i (shrink.factors)
                      (setf binc  (matmult G (diagonal dinc) trug))
                                          ; G * D * true gamma
                      (setf rbias (matrix (list p 1)
                                          (/ (* (- dinc 1) trug) trus)))
                      (setf compr (+ (matmult rbias (transpose rbias))
                                     (diagonal (/ (^ dinc 2) ev))))
                      (setf smse  (matmult G compr (transpose G)))  ;
                      (setf rinc  (diagonal smse)) ;
                      (when (= rscale 2)
                            (setf binc (/ binc xscale))
                            (setf rinc (/ rinc (^ xscale 2)))
                      )
                      (setf tinc  (sum rinc))
                      (setf emse  (- (diagonal (/ 1 ev)) compr))
                      (setf sfac  (max (/ (min (abs (diagonal emse))) 100) 0.01))
                      (setf eig   (eigen (/ emse sfac)))
                      (setf einc  (sort-data (coerce (* (first eig) sfac) 'list)))
                      (setf cinc  (if (< (first einc) -0.0001)
                                      (let ((ctmp (matmult G (first (last (second eig)))))
                                           )
                                           (when (= rscale 2)
                                             (setf ctmp (/ ctmp (sqrt (sum (^ ctmp 2)))))
                                           )
                                           (when (< (sum (* cold ctmp)) 0)
                                             (setf ctmp (- ctmp))
                                           )
                                           (setf cold ctmp)
                                           (coerce ctmp 'list)
                                      )
                                      (make-list p :initial-element 0)
                                  ))
                      (when (= rscale 2)
                            (setf binc (matmult (diagonal (second scale)) binc))
                            (setf rinc (matmult (diagonal (^ (second scale) 2)) rinc))
                      )
                      (setf beta  (bind-columns beta  binc))
                      (setf risk  (bind-columns risk  rinc))
                      (setf exev  (bind-columns exev  einc))
                      (setf infd  (bind-columns infd  cinc))
                      (setf delta (bind-columns delta dinc))
                      (setf tsmse (append       tsmse (list tinc)))
                      (setf konst (append       konst (list kinc)))
                      (setf mcal  (append       mcal  (list (- p (sum dinc)))))
                   )
                   (format t "~2%")

                   (send self :RXridge-true-dat (list mcal beta risk exev infd delta))

                   (when print
                     (format t "Matrix BETA =~%")
                     (pp-matrix (transpose beta))
                     (format t "Matrix RISK =~%")
                     (pp-matrix (transpose risk))
                     (format t "Matrix EXEV =~%")
                     (pp-matrix (transpose exev))
                     (format t "Matrix INFD =~%")
                     (pp-matrix (transpose infd))
                     (format t "Matrix DELTA =~%")
                     (pp-matrix (transpose delta))
                     (format t "Matrix (TSMSE Konst MCAL) =~%")
                     (pp-matrix (bind-columns tsmse konst mcal))
                   )

                   nil
                )
        )
    )
  )
)

(defmeth regression-model-proto :RXridge-loss-simulation (&optional (print nil))
"Message Args: (&optional (print nil))
Given true regressor components and true error standard deviation,
simulate responses and compute losses implied by shrinkage.
If argument print is not nil, then results are printed."

(when (not (send self :RXridge-summary))   ; if necessary
      (send self :RXridge-compute-summary) ; compute comp, tstat, ...
)
(when (not (send self :RXridge-truegs))    ; if necessary
      (send self :RXridge-true-parms)      ; specify TRUE values...
)

  (let* ((trus    (send self :RXridge-trus))
         (trug    (send self :RXridge-trug))
         (schoice (send self :RXridge-soption))
         (seed    (send self :RXridge-seed))
        )
        (cond ((< schoice 1) (format t "Random State Reset to Saved ~s~%"
                               seed))
              ((= schoice 1) (setf seed (make-random-state nil))
                             (format t "Using Current Random State ~s~%"
                               seed))
              ((> schoice 1) (setf seed (make-random-state t))
                             (format t "New Random State Generated ~s~%"
                               seed))
        )
        (setf *random-state* (make-random-state seed))
        (when (and trus trug)
          (if (<= (first (last (coerce (second (send self :RXridge-svdx)) 'list))) 0)
              ;; then
              (format t "Simulation requires XMAT to be of Full Column Rank.~%")
              ;; else
              (let* ((svdx   (send self :RXridge-svdx))   ; Get SVD of X
                     (G      (third svdx))                ; Direction Cosine G
                     (sv     (second svdx))               ; singular values
                     (ev     (coerce (^ sv 2) 'list))     ; the eigenvalues
                     (tstat  (send self :RXridge-tstat))
                     (rscale (send self :RXridge-scale))
                     (steps  (send self :RXridge-steps))
                     (scale  (send self :RXridge-xyscale))
                     (xscale (first scale))
                     (crx    (send self :RXridge-crx))
                     (qp     (min (max (send self :RXridge-qp) -5) 5))
                     (pshape (send self :RXridge-shape))
                     (p      (length sv))
                     (n      (send self :num-included))
                     (eqm1   (if (= qp 1)
                                 (make-list p :initial-element 1)
                                 (^ ev (- qp 1))                 
                     ))      ; (1....1) or \lambda_i^(Q-1) used for \delta_i
                     (fact   (+ (^ trus 2)
                                (/ (sum (* trug ev trug))
                                   (1- n)
                                )
                             ))
                     (trug   (/ trug (sqrt fact)))
                                       ; rescale, preserving signal/noise ratios
                     (trus   (/ trus (sqrt fact)))
                                       ; expected response sum-of-squares = n-1
                     (beta   (matmult G trug)) ; BETA, true regression coeffs
                     (expy   (matmult crx beta)) ; expected response vector
                     (error  (* (normal-rand n) trus))
                                       ; n x 1 vector of normal (0,trus) errors
                     (yvec   (+ expy (- error (mean error))))
                     (yscale (if (= rscale 0) 1  ; rscale=0 ==> yscale = 1
                             (standard-deviation yvec)
                             ))        ; else, yscale = stdev(y)
                     (cry    (coerce (/ (- yvec (mean yvec)) yscale) 'vector))
                                       ; ==> cry is the centered and rescaled
                     (ssy    (sum (^ cry 2)))
                     (Hty    (matmult (transpose (first svdx)) cry)) ; = H'y
                     (comp   (matmult (diagonal (/ 1 sv)) Hty))      ; = c = L^(-1/2) * H'y
                     (bstar  (matmult G comp))                       ; = G*c = OLS
                     (loss   (/ (^ (- bstar beta) 2) (^ trus 2)))    ; exact scaled squared error losses
                     (res    (- cry (matmult crx bstar)))            ; LEAST SQUARES RESIDUAL VECTOR
                     (when (= rscale 2)
                           (setf bstar (*    (/ yscale xscale)     bstar))
                           (setf loss  (* (^ (/ yscale xscale) 2)  loss ))
                     )
                     (totlos (sum loss))                             ; exact total scaled loss
                     (maxinc (* p steps))
                     (delta  (make-list p :initial-element 1))       ; COLUMN VECTOR OF DELTA FACTORS
                     (d      (diagonal delta))  ; DIAGONAL SHRINKAGE FACTOR MATRIX
                     (rho    (/ (* sv comp) (sqrt ssy)))   ; RHO CONTAINS CORRELATIONS OF Y WITH
                              ; THE PRINCIPAL COORDINATES (RATHER THAN GIVEN COORDINATES) OF X
                     (s2     (/ (sum (^ res 2)) (- n p 1))) ; S2 IS THE UNBIASED
                                     ; ESTIMATOR OF THE RESIDUAL VARIANCE IN A MODEL
                                     ; WITH UNCORRELATED AND HOMOSCEDASTIC OBSERVATIONS
                     (varrho (/ s2 ssy))  ; VARRHO IS THE SAMPLE VARIANCE OF EACH RHO
                     (trat   (/ rho (sqrt varrho))) ; VECTOR OF T-STATISTICS FOR TESTS
                            ; OF THE HYPOTHESES THAT THE TRUE GAMMA COMPONENTS ARE ZERO
                     ; setup for calculation of step-by-step shrinkage estimates.
                     (konst  '(0.0))
                     (mcal   '(0.0))
                     (totlos (list totlos))
                     (kinc   0)
                     (dinc   0)
                     (binc   0)
                     (linc   0)
                     (minc   0)
                     (tinc   0)
                   )
                   (when print
                     (format t "~%Shrinkage Regression Loss Simulation ~2%")
                     (format t "Shrinkage Path Shape = Q =~4,1f~2%" qp)
                     (format t "Rescale response/regressors, preserving all signal/noise ~%")
                     (format t "ratios, so that expected response sum-of-squares = n-1. ~2%")
                     (format t "(Regressors and Response are centered")
                     (cond ( (= rscale 0) (format t ") ~2%") )
                           ( (= rscale 1) (format t "/scaled) ~2%") )
                           ( (= rscale 2) (format t "/scaled and output is rescaled) ~2%"))
                     )
                     (format t "Simulated Response Values and True Expected Values...~2%")
                     (format t "  YVEC       EXPY ~%")
                     (dotimes (i (length yvec))
                        (format t "~8,4f   ~8,4f ~%" (elt yvec i) (elt expy i))
                     )
                     (format t "~2%")
                     (format t "Principal Axis Summary Statistics of Ill-Conditioning...~2%")
                     (format t "Eigenval Sing.Val   Uncorr.Comp.    Beta ~%")
                     (dotimes (i (length ev))
                        (format t "~8,4f   ~8,4f   ~8,4f   ~8,4f ~%" (elt ev i) (elt sv i)
                                                   (elt comp i) (elt bstar i))
                     )
                     (format t "~2%")
                     (format t "Simulated Sq.Losses for the TWO HYPERPARAMETER ridge family ~%")
                     (format t "with SHAPE fixed at Q = ~3,2f are being calculated.~2%" qp)
                   )
                   (format t "Steps: ")
                   (dotimes (inc maxinc)
                      (format t "~3d " inc)
                      (when (= (mod (+ 1 inc) 8) 0) (format t "~%       "))
                      (setf iter (mstep (/ (1+ inc) steps) kinc p qp pshape rho eqm1))
                      (setf kinc (first iter))
                      (setf dinc (second iter))
                      (setf binc (matmult G (diagonal dinc) comp))
                                          ; fitted RXridge coefficients
                      (setf linc (/ (^ (- binc beta) 2) (^ trus 2)))
                                          ; exact scaled squared error losses
                      (when (= rscale 2)
                            (setf binc (matmult (diagonal (/ (second scale) (first scale))) binc))
                            (setf rinc (matmult (diagonal (^ (/ (second scale) (first scale)) 2)) linc))
                      )
                      (setf tinc (sum linc))
                      (setf minc (- p (sum dinc)))      ;; multicollinearity allowance
                      (setf bstar (bind-columns bstar binc))
                      (setf loss (bind-columns loss linc))
                      (setf delta (bind-columns delta dinc))
                      (setf totlos (append totlos (list tinc)))
                      (setf konst  (append konst  (list kinc)))
                      (setf mcal   (append mcal   (list minc)))
                   )
                   (format t "~2%End of LOSS Simulation ...~2%")
                   (send self :RXridge-simu-dat (list mcal bstar loss delta yvec))

                   (when print
                     (format t "~%Simulated Ridge Coefficients...~2%")
                     (pp-matrix (transpose bstar))
                     (format t "~%Exact Scaled Losses...~2%")
                     (pp-matrix (transpose loss))
                     (format t "~%Ridge Shrinkage Delta Factors...~2%")
                     (pp-matrix (transpose delta))
                     (format t "~%Shrinkage Pattern Statistics...~2%")
                     (format t "   TLOSS        KONST         MCAL~%")
                     (pp-matrix (bind-columns totlos konst mcal))
                   )
              )
       )
    )
  )
)

(defmeth regression-model-proto :RXridge-losses()
"Message Args: ()
Tests that all SIM data are computed and calls plots."
  (when (not (send self :RXridge-simu-dat))
        (send self :RXridge-loss-simulation)
  )

  (let* ((crx     (send self :RXridge-crx ))
         (mcal    (send self :RXridge-msimu ))
         (bstar   (send self :RXridge-bsimu ))
         (loss    (send self :RXridge-lsimu ))
         (delta   (send self :RXridge-dsimu ))
         (yvec    (send self :RXridge-ysimu ))
         (p       (- (send self :num-coefs) 1)) ; number non-constant coeffs
         (qp      (send self :RXridge-qp))
         (pshape  (send self :RXridge-shape))
         (anot    (cond ((= pshape 0)
                         (format nil "Q=~3,1f" qp)
                        )
                        ((= pshape 1)
                         (format nil "CROSR")
                        )
                        ((= pshape 2)
                         (format nil "GARROTE")
                        )
                        (t
                         (format nil "LASSO")
                        )
                  )
         )
         (labs    nil)
         (replace 0)
        )
        (setf labs (map 'list
                        #'(lambda(i)
                             (trace-label mcal i)
                        )
                        (iseq 1 p)
                   ))

        (send (plot-matrix mcal delta
                      :title (format nil "MCAL vs. Delta (~a)" anot)
                      :point-labels labs)
              :abline 0 0)

        (send (plot-matrix mcal bstar
                           :title (format nil "MCAL vs. Coeff (~a)" anot)
                           :point-labels labs)
              :abline 0 0)

        (send (plot-matrix mcal loss
                      :title (format nil "MCAL vs. Scaled Sq.Loss (~a)" anot)
                      :point-labels labs)
              :abline 0 0)

        (when *sim-reg*
             (setf replace (choose-item-dialog "Simulation Options"
                              (list "Overwrite Current Data"
                                    "  Retain  Current Data")
                              :initial replace
                           )
             )
        )
        (when (= replace 0)
             (def sim-reg (regression-model crx yvec :response-name "SIM"))
             (format t "(The simulated response model object is 'sim-reg') ~%")
             (if (not *sim-reg*)
                 (send sim-reg :RXridge-menu "SIM")
             )
        )
        (setf *sim-reg* t)
  )
)