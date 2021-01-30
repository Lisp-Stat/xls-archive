
;;;;     axis.lsp © Robert A. Stine, 1992
;;;;                 Menus for managing the data/objects of an analysis.

#||
    17 Apr 94 ... Add jitter toggle to plot.
    25 Mar 94 ... Clean up the cmd item structure, again.
    26 Feb 94 ... Item agents to link regr plots to dataset; saver.
    19 Feb 94 ... :add-menu-items removes dataset dependence from scatter.
     9 Feb 94 ... Revised correlation dialog. Fix spin-plot ordering.
     2 Feb 94 ... Begin revising cmd items to use new form structure.
    22 Jul 93 ... Update menus and add evaluation item.
     2 Jul 93 ... Patch the scatter command (args reversed).
     2 Jul 92 ... Created; add data and popup (3 Jul)
||#

;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(require "patch")   ; alters arg passing in regression, other bugs

(require "axisUtil")
(require "axisCmd")
(require "axisData")
(require "axisDens")
(require "axisScat")
(require "axisCorr")
(require "axisRegr")
(require "axisRobR")
(require "axisSE")

(require "compare")

#||
   (debug)
   (setf *tracenable* t)
   (setf *tracelimit* 3)
||#
;;;;     GLOBALS

(setf *statistics-menu* nil)

;      (install-statistics-menu)
;      (remove-statistics-menu)
;;;;

(defproto STAT-MENU-ITEM-PROTO
  ()
  ()
  menu-item-proto)

(defmeth stat-menu-item-proto :ISNEW (name proto)
  (call-next-method name
                    :action #'(lambda ()
                                (send proto :new))))

(defmeth stat-menu-item-proto :UPDATE ()
  (send self :enabled
        (if *active-icon-window* 
            t nil)))

;;;;

(defun INSTALL-STATISTICS-MENU ()
  (if *statistics-menu* (send *statistics-menu* :remove))
  (setf *statistics-menu* (send menu-proto :new "Statistics"))
  (let ((evalItem (send stat-menu-item-proto :new
                        "Evaluate" eval-cmd-proto))
        (descItem (send stat-menu-item-proto :new
                        "Describe" desc-cmd-proto))
        (densItem (send stat-menu-item-proto :new
                        "Density"  density-cmd-proto))
        (compItem (send stat-menu-item-proto :new
                        "Compare" compare-cmd-proto))
        (scatItem (send stat-menu-item-proto :new
                        "Scatter Plot" scatter-cmd-proto))
        (scatMatItem (send stat-menu-item-proto :new
                           "Scatterplot Matrix"  scatMat-cmd-proto))
        (spinItem (send stat-menu-item-proto :new
                        "Spin Plot" spin-cmd-proto))
        (corrItem (send stat-menu-item-proto :new
                        "Correlation" corr-cmd-proto))
        (regrItem (send stat-menu-item-proto :new
                        "Regression" regr-cmd-proto))
        (robregrItem (send stat-menu-item-proto :new
                           "Robust Regression" robust-regr-cmd-proto))
        (seItem (send stat-menu-item-proto :new
                      "Structural Eqns" se-cmd-proto))
        (bootItem (send stat-menu-item-proto :new
                           "Bootstrap" uniboot-cmd-proto))
        )
    (send *statistics-menu* :append-items
          evalItem descItem
          (send dash-item-proto :new)
          densItem compItem scatItem scatmatItem spinItem
          (send dash-item-proto :new)
          corrItem regrItem robRegrItem seItem
          (send dash-item-proto :new)
          bootItem)  ; removed regrbootitem
    (send *statistics-menu* :install)))
  

(defun REMOVE-STATISTICS-MENU ()
  (if *statistics-menu*
      (send *statistics-menu* :remove)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;     EVALUATION COMMAND PROTOTYPE             ;;
;;                                  22 Jul 93   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto EVAL-CMD-PROTO
  '()
  '(classIDCode)       ;; required for every "type" of command
  commando-proto)


(defmeth eval-cmd-proto :ISNEW ()
  (send self :init-slots
        "Evaluate"
        nil
        "Expression ->")
  (call-next-method)
  (setf (slot-value 'target) (send self :dataset))
  )

(defmeth eval-cmd-proto :DOIT ()          ; override
  (let ((form (first (send self :form)))  )
    (format t "Evaluation of ~a:~% ~a~%" 
            form
            (send (send self :dataset) :evaluate form)   ))
  )

; (def ec (send eval-cmd-proto :new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;             DESCRIBE                         ;;
;;                                  26 Jul 93   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto DESC-CMD-PROTO
  '()
  '(classIDCode)       ;; required for every "type" of command
  commando-proto)

(defmeth desc-cmd-proto :ISNEW ()
  (send self :init-slots
        "Describe"
        'describe
        "Variable ->")
  (call-next-method))

(defmeth desc-cmd-proto :COMMAND ()
  (append (call-next-method)
          (send self :text))  )

; (def dc (send desc-cmd-proto :new))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;             DENSITY                          ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto DENSITY-CMD-PROTO
  '()
  '(classIDCode)       ;; required for every "type" of command
  commando-proto)

(defmeth density-cmd-proto :ISNEW ()
  (send self :init-slots
        "Density"
        'plot-density
        "Expression ->")
  (call-next-method))

(defmeth density-cmd-proto :COMMAND ()
  (append (call-next-method)
          (send self :text)) )

; (def d (send density-cmd-proto :new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;             COMPARE                          ;;
;;                                30 Jun 93     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto compare-cmd-proto
  '(vToggle)
  '(classIDCode)  
  commando-proto )

(defmeth compare-cmd-proto :ISNEW ()
  (send self :init-slots
        "Compare"
        'compare
        (list (list "Variables ->"))   )   ; 3/25/94, gets a list form item
  (call-next-method))                      ; double indicates list-form

(defmeth compare-cmd-proto :COMMAND ()
  (cons
   (slot-value 'cmdForm)
   `((list ,@(first (send self :form)))   ; get first and wrap with list
     :labels ',@(send self :text)
     :vertical? ,(send (slot-value 'vToggle) :value)
     )))
 

(defmeth compare-cmd-proto :TOGGLE-ITEMS ()
  (let ((ti (send toggle-item-proto :new "Vertical" :value nil))   )
    (setf (slot-value 'vToggle ) ti)
    (list ti)
    ))

; (setf c (send compare-cmd-proto :new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;              SCATTERPLOT                     ;;
;;                                    25 Apr 92 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto scatter-cmd-proto
  '(joinToggle yToggle xToggle)
  '(classIDCode)  
  commando-proto)

(defmeth scatter-cmd-proto :ISNEW ()
  (send self :init-slots
        "Scatter Plot"
        'regression-scatterplot
         (list "Plot y data ->"
               "Plot x data ->"))
  (call-next-method))

(defmeth scatter-cmd-proto :COMMAND ()        ; 4/17/94
  (let ((cmd (call-next-method))
        (jx  (send (slot-value 'xToggle) :value))
        (jy  (send (slot-value 'yToggle) :value))  )
    `(,(first cmd)
      ,(if jx (list 'jitter (select cmd 2)) (select cmd 2))
      ,(if jy (list 'jitter (select cmd 1)) (select cmd 1))
      ,@(reverse (send self :text))
      :cases ^cases
      )))

(defmeth scatter-cmd-proto :DOIT ()
  (call-next-method)
  (def *scatter* (slot-value 'target))
  (let ((ds  (send self :dataset))
        (tar (send self :target))   )
    (send *scatter* :add-menu-items                 ; 2/19/94
          (list
           (messenger-item tar ds nil)
           (saver-item tar ds)                      ; 2/26/94
           (send menu-item-proto :new "Add codes"   ; 2/26/94
                 :action #'(lambda ()
                             (let ((f (get-form-dialog "Code variable"))  )
                               (if f
                                   (send tar :set-symbols
                                         (send ds :evaluate f)
                                         (format nil "~a" f))))))
           ))
    (send self :close)))  ; 2/19/94
        

(defmeth scatter-cmd-proto :TOGGLE-ITEMS ()          ; 4/17/94
  (let ((ty (send toggle-item-proto :new "Jitter Y" :value nil))
        (tx (send toggle-item-proto :new "Jitter X" :value nil))
        )
    (setf (slot-value 'xToggle ) tx)
    (setf (slot-value 'yToggle ) ty)
    (list ty tx)
    ))


#|
 (def sc (send scatter-cmd-proto :new))    
 (send sc :command)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;            SCATTERPLOT-MATRIX                ;;
;;                                    8 Jul 92  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto scatMat-cmd-proto
  '()
  '(classIDCode)  
  commando-proto
  "Controller for a scatterplot window." )

(defmeth scatMat-cmd-proto :ISNEW ()
  (send self :init-slots
        "Scatterplot Matrix"
        'scatterplot-matrix
        (list (list "Variables ->"))    )    ; 2/2/94
  (call-next-method))

(defmeth scatMat-cmd-proto :COMMAND ()
  (cons
   (slot-value 'cmdForm)
   `((list ,@(reverse (first (send self :form))))
     :variable-labels ',(reverse (first (send self :text))))
   ))

(defmeth scatMat-cmd-proto :DOIT ()
  (call-next-method)
  (send (send self :dataset) :label-points (slot-value 'target)) 
  )

; (setf sc (send scatMat-cmd-proto :new))
; (send sc :text)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;             SPIN-PLOT                        ;;
;;                                    8 Jul 92  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto spin-cmd-proto
  '()
  '(classIDCode)  
  commando-proto
  "Controller for a spin-plot window." )

(defmeth spin-cmd-proto :ISNEW ()
  (send self :init-slots
        "Spin Plot"
        'spin-plot
        (list (list "3 Variables ->"))   )  ; 2/2/94
  (call-next-method))

(defmeth spin-cmd-proto :COMMAND ()
  (let ((syms (first (send self :form)))
        (names (first (send self :text)))
        (order '(1 2 0))   )                ; 7/26/94 for regr surface
    (if (= 3 (length syms))
        (cons
         (slot-value 'cmdForm)
         `((list ,@(select syms order))
           :variable-labels ',(select names order))  )
        (message-dialog "Spinner requires three variables.")
        )))

(defmeth spin-cmd-proto :DOIT ()
  ; Evaluates, labels, and adds method for showing regr surface.
  (call-next-method)
  (send (send self :dataset) :label-points (slot-value 'target))
  (send *target* :depth-cuing nil)
  (defmeth *target* :show-regression ()
    (let* ((indx (iseq (send self :num-points)))
           (m  (regression-model
                (list (send self :point-coordinate 0 indx)
                      (send self :point-coordinate 1 indx))
                (send self :point-coordinate 2 indx) :print nil))  )
      (format t "Regr coefs are ~a~%" (send m :coef-estimates))
      (apply #'send self :abcplane (send m :coef-estimates)) 
      ))
  )

#||
   (setf sc (send spin-cmd-proto :new))
   (send sc :command)
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;            CORRELATION                       ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto corr-cmd-proto
  '()
  '(classIDCode)  
  commando-proto)

(defmeth corr-cmd-proto :ISNEW ()
  (send self :init-slots
        "Correlation"
        'correlate
         (list (list "Variables ->")))
  (call-next-method))

(defmeth corr-cmd-proto :COMMAND ()
  (cons
   (slot-value 'cmdForm)
   `((list ,@(first (send self :form)))
     ',@(send self :text))
  ))

(defmeth corr-cmd-proto :DOIT ()
  (format t "Computing correlation...~%")
  (call-next-method)
  (send (slot-value 'target) :print-summary)
  )

(defmeth corr-cmd-proto :BUTTON-ITEMS ()
  (let ((print-but (send button-item-proto :new "Print"
                         :action #'(lambda ()
                                     (send (slot-value 'target)
                                           :print-summary))))
        (boot-but (send button-item-proto :new "Bootstrap"
                        :action #'(lambda ()
                                    (let ((b (get-pos-integer-dialog
                                              "Enter bootstrap B."
                                              :init 50))   )
                                      (if b
                                          (send (slot-value 'target)
                                                :bootstrap b))
                                      ))))
        )
    (list (list print-but boot-but))
    ))

#||
  (setf c   (send corr-cmd-proto :new))
  (send c :form)
  (send c :text)
  (send c :command)

  (setf ds  (send c :dataset))
  (setf tar (send c :slot-value 'target))

  (send tar :value)
  (setf  f  (send tar :bootstrap-function))
  (funcall f '(1 3 2 4))

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;               OLS REGRESSION                 ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto regr-cmd-proto
  '()
  '(classIDCode)  
  commando-proto)

(defmeth regr-cmd-proto :ISNEW (&optional name cmd)
  (send self :init-slots
        (if name name "OLS Regression")
        (if cmd cmd 'regress)
        (list "Response   ->" (list "Covariates ->"))  ) ; 2/2/94
  (call-next-method))

(defmeth regr-cmd-proto :COMMAND ()
  (let* ((txt  (send self :text))      ; list of lists
         (form (send self :form))
         ( x   (select form 1))    )
    (cons
     (slot-value 'cmdForm)
     `( (list ,@x)
        ,(select form 0)
        :response-name ,(first txt)
        :predictor-names (list ,@(second txt))
        :case-labels ^cases
        :included ^filter)
     )))


(defmeth regr-cmd-proto :DOIT ()
  (let ((ds  (send *active-icon-window* :dataset))   )
    (call-next-method)
    (setf *regression* (slot-value 'target))
    (send *regression* :plot-function-is                      ; 2/23/94
          #'(lambda (&rest args)                      
              (let ((thePlot (apply #'regression-scatterplot args))  )
                (send thePlot :add-menu-items
                      (list  (messenger-item thePlot ds nil)
                             (saver-item thePlot ds)
                             ))
                thePlot)))))
 

#|
 (def  r  (send regr-cmd-proto :new))

 (send r :form)
 (def fl (send r :slot-value 'formList))
 (def xf (second fl))
 (send xf :text) ; ok
 (send xf :form) ; bugged...

 (send r :text :asTokens? t)
 (send r :command)

 (send r :show-window)

 (def  rm (send r :slot-value 'target))
 (def  b  (send rm :bootstrap-function 'random))
 (funcall b '(1 2 3 2 1 2 3))
 (bootstrap "REGR" (list b) `(resample (iseq 10)) (first *datasets*) 5)
|#

(defmeth regr-cmd-proto :BOOT-DIALOG ()
  (let* ((prompt (send text-item-proto :new "Enter bootstrap B:"))
         (nReps  (send edit-text-item-proto :new "50" :text-length 5))
         (method (send choice-item-proto :new 
                       (list "Random X" "Fixed X") :value 0))
         (ok     (send modal-button-proto :new "OK"
                       :action #'(lambda ()
                                   (list (read (make-string-input-stream
                                                (send nReps :text)) nil)
                                         (select (list 'random 'fixed)
                                                 (send method :value)))) ))
         (cancel (send modal-button-proto :new "Cancel"
                       :action #'(lambda () nil)))
         (dialog  (send modal-dialog-proto :new 
                        (list (list prompt nReps)
                              (list method)
                              (list ok cancel))
                        :title "Bootstrap Settings"
                        :default-button ok))    )
    (send dialog :modal-dialog)
    ))


(defmeth regr-cmd-proto :BUTTON-ITEMS () 
  (let* ((print-but (send commando-button-proto :new "Print"       ; 7/11
                          :action #'(lambda (x) (send x :display))))
         (ci-but    (send commando-button-proto :new "Conf Int"
                          :action #'(lambda (x) (send x :ci .9))))
         (boot-but (send commando-button-proto :new "Bootstrap"
                         :action #'(lambda (x)
                                     (let ((resp (send self :boot-dialog)))
                                       (if resp (apply #'send x :bootstrap resp)
                                           )))))
         (resid-but (send commando-button-proto :new "Plot Resids"
                            :action #'(lambda (x) (send x :plot-residuals))))
         (vif-but   (send commando-button-proto :new "VIF Plot"
                          :action #'(lambda (x) (send x :vif-plot)))) 
         (diag-but  (send commando-button-proto :new "Diagnostics"
                          :action #'(lambda (x)
                                      (send x :vif)
                                      (send x :durbin-watson)
                                      (send x :runs-test)
                                      (send x :plot-diagnostics))))
         (pRegr-but (send commando-button-proto :new "Partial Regr"
                          :action #'(lambda (x)
                                      (send x :plot-partial-regr))))
         (pRes-but  (send commando-button-proto :new "Partial Resid"
                          :action #'(lambda (x)
                                      (send x :plot-partial-residuals))))
         (save-but  (send commando-button-proto :new "Save Data"
                          :action #'(lambda (x)
                                      (saver x (send self :dataset)))  ))
         )
    (list (list print-but ci-but save-but)
          (list resid-but diag-but pRegr-but pRes-but)
          (list vif-but boot-but)  )
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;            ROBUST REGRESSION                 ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto robust-regr-cmd-proto
  '()  
  '(classIDCode)  
  regr-cmd-proto)

(defmeth robust-regr-cmd-proto :ISNEW ()
  (call-next-method "Robust Regression" 'robust-regression))
  
(defmeth robust-regr-cmd-proto :COMMAND ()
  (let* ((txt  (send self :text)) ; list of lists
         (form (send self :form))
         ( x   (select form 1))
         )
    (cons
     (slot-value 'cmdForm)
     `( (list ,@x)
        ,(select form 0)
        huber                        ; 3/27/94
        (list ,@(second txt))
        ,(first txt)                 ; 2/2/94
        :case-labels ^cases
        :included ^filter
        ))))

(defmeth robust-regr-cmd-proto :METHOD-DIALOG ()
  (let ((ch (choose-item-dialog "Estimation method"
                                '("Huber" "Cauchy" "Biweight")))  )
    (if ch
        (send (slot-value 'target) :method
              (select '(huber cauchy biweight) ch))
        )))
  
(defmeth robust-regr-cmd-proto :BUTTON-ITEMS ()
  (let ((meth-but (send button-item-proto :new "Method"
                         :action #'(lambda () (send self :method-dialog))))
        (buttons  (call-next-method))  )
    (list (first buttons) 
          (second buttons)            ; 4/2/94
          (cons meth-but (rest (select buttons 2)))  )
    ))
    


#||
  (def rc (send robust-regr-cmd-proto :new))
  (def  rm (send rc :slot-value 'target))
  (def  b  (send rm :bootstrap-function 'random))
  (funcall b '(1 2 3 2 1 2 3))
  (bootstrap "REGR" (list b) `(resample (iseq 10)) (first *datasets*) 5)
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;           STRUCTURAL EQUATION                ;;
;;                                   21 Jul 93  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (setf cmd (send se-cmd-proto :new))
; (send cmd :command)

(defproto se-cmd-proto
  '()
  '(classIDCode)  
  commando-proto)

(defmeth se-cmd-proto :ISNEW ()
  (let ((nEqns (get-pos-integer-dialog "Enter number of equations" :init 2)))
    (when (and nEqns (< 1 nEqns))
          (send self :init-slots
                "Structural Equations"
                'se-model
                (combine
                 (mapcar #'(lambda (i) 
                             (list 
                              (format nil "Endogenous #~d" i)
                              (format nil "     Equation #~d" i))  )
                        (1+ (iseq nEqns)))  )
                )))
  (call-next-method))

(defmeth se-cmd-proto :COMMAND ()
  `(,(slot-value 'cmdForm)
                 ',(split-list (send self :form) 2)
                 ^dataset
                 ))


(defmeth se-cmd-proto :DOIT ()
  (call-next-method)
  (setf *model* (slot-value 'target))
  )


(defmeth se-cmd-proto :INITIAL-FORMS (nForms)
  ; Override so that it does not pick up the dataset names.
  (repeat "" nForms))


(defmeth se-cmd-proto :BUTTON-ITEMS ()
  (let ((boot-but (send button-item-proto :new "Bootstrap"
                        :action #'(lambda ()
                                    (let ((b (get-pos-integer-dialog
                                              "Enter bootstrap B."
                                              :init 50))   )
                                      (if b
                                          (send (slot-value 'target)
                                                :bootstrap b))))
                        ))    )
    (list boot-but)
    ))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;            BOOTSTRAP COMPARISON              ;;
;;                        26 Apr 92 - 30 Jun 93 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto uniboot-cmd-proto
  '(nEsts)
  '(classIDCode)  
  commando-proto)

(defmeth uniboot-cmd-proto :ISNEW (&optional nEsts)
  (setf (slot-value 'nEsts)
        (if nEsts  nEsts
            (get-pos-integer-dialog "Number to compare?")))
  (when (slot-value 'nEsts)    ; 2/2/94
        (send self :init-slots
              "Bootstrap Comparison"
              'bootstrap
              (combine (mapcar #'(lambda (i) (format nil "Estimator #~a " i))
                               (1+ (iseq (slot-value 'nEsts))))
                       (list "Sampling Rule" "Number of Trials"))  )
        (call-next-method))    
  )

(defmeth uniboot-cmd-proto :COMMAND ()
  (let* ((form  (send self :form))
         (nEsts (slot-value 'nEsts))
         (ops   (select form (iseq nEsts)))        
         )
    (cons
     (slot-value 'cmdForm)
     `( "Univariate"               ; name default (rest adds bootstrap)
        (list ,@ops)
        ',(select form nEsts)      ; generator form
        ^dataset
        ,(select form (1+ nEsts))  ; nReps
        ))))

(defmeth uniboot-cmd-proto :INITIAL-FORMS (nForms)
  ; Override so that it does not pick up the dataset names.
  (repeat "" nForms))


#||

  (setf ds (first *datasets*))
  (BOOTSTRAP (LIST 'MEAN) (NORMAL-RAND 10) ds 20)

  (def b (send uniboot-cmd-proto :new))    

||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Fire it up                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if *statistics-menu*
    (remove-statistics-menu))
(install-statistics-menu)

