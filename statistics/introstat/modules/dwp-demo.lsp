(defun start ()
  (let* (
         (data (read-data-columns "DWP.data"))
         (n (length (first data)))
         (newreg t)
         (varind (repeat nil 7))
         (newplot1 t)
         (newplot2 t)
         (newplot3 t)
         (update nil)
         (goodpts (iseq (length (first data))))
        )
    (let* (
           (deptell (send text-item-proto :new "Dependant Variable:  BSAAM"))
           (indepask (send text-item-proto :new "Pick Regressors: "))
           (interceptask (send toggle-item-proto :new "Intercept"))
           (dateask (send toggle-item-proto :new "YEAR"))
           (apmamask (send toggle-item-proto :new "APMAM"))
           (apsabask (send toggle-item-proto :new "APSAB"))
           (apslakeask (send toggle-item-proto :new "APSLAKE"))
           (opbpcask (send toggle-item-proto :new "OPBPC"))
           (oprcask (send toggle-item-proto :new "OPRC"))
           (opslakeask (send toggle-item-proto :new "OPSLAKE"))
           (reprintask (send toggle-item-proto :new "Reprint Plots?"))
           (includeask (send toggle-item-proto :new "Include All Points?"))
           (plotask (send text-item-proto :new "Choose Diagnostics: "))
           (indepvsdepask (send toggle-item-proto :new 
                                    "Dependant vs. Independant"))
           (residvsyask (send toggle-item-proto :new "Residual vs. BSAAM"))
           (residvstimeask (send toggle-item-proto :new "Residual vs. Time"))
           (cpksask (send toggle-item-proto :new "CP vs. Number of Regressors"))
           (varlist (list "Year" "Apmam" "Apsab" "Apslake" "Opbpc" "Oprc" 
                          "Opslake"))
           (do-it (send button-item-proto :new "Compute Regression"
                    :action #'(lambda ()
                               (let* (
                                      (interceptans (send interceptask :value))
                                      (dateans (send dateask :value))
                                      (apmamans (send apmamask :value))
                                      (apsabans (send apsabask :value))
                                      (apslakeans (send apslakeask :value))
                                      (opbpcans (send opbpcask :value))
                                      (oprcans (send oprcask :value))
                                      (opslakeans (send opslakeask :value))
                                      (indepvsdep (send indepvsdepask :value))
                                      (residvsy (send residvsyask :value))
                                      (residvstime (send residvstimeask :value))
                                      (reprintans (send reprintask :value))
                                      (includeans (send includeask :value))
                                      (numreg (length (which (list dateans
                                            apmamans apsabans
                                            apslakeans opbpcans oprcans 
                                            opslakeans))))
                                      (newplot1 (if (= 7 (length (which
                                            (map-elements #'(lambda (x y)
                                              (equalp x y)) varind 
                                               (list dateans apmamans apsabans
                                                     apslakeans opbpcans 
                                                     oprcans opslakeans)))))
                                                nil t))
                                      (goodpts (cond (includeans 
                                            (repeat t n))
                                           ((and (not newreg)
                                           (not (= 0 (length (variables)))))
                                           (send (elt 
                                           (mapcar #'symbol-value (variables))
                                               (first (which (mapcar #'objectp
                                             (mapcar #'symbol-value 
                                                (variables))))))
                                                   :point-showing (iseq n)))
                                           (t (repeat t n))))
                                      (newplot2 newplot1)
                                      (newplot3 newplot2)
                                      (update (if (or includeans
                                            (/= (length (which goodpts)) n))
                                             t nil))
                                     )
               (setf varind (list dateans apmamans apsabans
                                           apslakeans
                                           opbpcans oprcans opslakeans))

               (cond (reprintans (setf newplot1 t) (setf newplot2 t) 
                                 (setf newplot3 t)))
               (cond (newreg (setf reg1 (regression-model 
                                     (select data (which varind)) (elt data 7) 
                           :intercept interceptans :print nil)))

                     (t (send reg1 :intercept interceptans)
                        (send reg1 :x (matrix (list n numreg)
                             (combine (transpose 
                                   (select data (which varind))))))
                        (send reg1 :included goodpts)
                        (send reg1 :compute)))
               (send reg1 :predictor-names (select varlist (which varind)))
               (send reg1 :response-name "Bsaam")
               (send reg1 :display)
               (setf newreg nil)
               (cond ((and newplot1 indepvsdep)
                                 (cond (dateans
                                  (def dateplot (plot-points
                                    (elt data 0) (elt data 7)
                                    :title "BSAAM vs. TIME"
                                    :variable-labels (list "TIME" "BSAAM")))
                                  (send dateplot :linked t)))
                                 (cond (apmamans 
                                  (def apmamplot (plot-points 
                                    (elt data 1) (elt data 7)
                                    :title "BSAAM vs. APMAM"
                                    :variable-labels (list "APMAM" "BSAAM")))
                                   (send apmamplot :linked t)))
                                 (cond (apsabans 
                                  (def apsabplot (plot-points 
                                    (elt data 2) (elt data 7)
                                    :title "BSAAM vs. APSAB"
                                    :variable-labels (list "APSAB" "BSAAM")))
                                  (send apsabplot :linked t)))
                                 (cond (apslakeans 
                                  (def apslakeplot (plot-points 
                                    (elt data 3) (elt data 7)
                                    :title "BSAAM vs. ASPLAKE"
                                    :variable-labels (list "APSLAKE" "BSAAM")))
                                  (send apslakeplot :linked t)))
                                 (cond (opbpcans 
                                  (def opbpcplot (plot-points 
                                    (elt data 4) (elt data 7)
                                    :title "BSAAM vs. OPBPC"
                                    :variable-labels (list "OPBPC" "BSAAM")))
                                  (send opbpcplot :linked t)))
                                 (cond (oprcans 
                                  (def oprcplot (plot-points 
                                    (elt data 5) (elt data 7)
                                    :title "BSAAM vs. OPRC"
                                    :variable-labels (list "OPRC" "BSAAM")))
                                  (send oprcplot :linked t)))
                                 (cond (opslakeans 
                                  (def opslakeplot (plot-points 
                                    (elt data 6) (elt data 7)
                                    :title "BSAAM vs. OPSLAKE"
                                  :variable-labels (list "OPSLAKE" "BSAAM")))
                                  (send opslakeplot :linked t)))))
               (cond ((and indepvsdep update)
                      (if dateans (refresh-plot (elt data 0) (elt data 7)
                                           goodpts dateplot))
                      (if apmamans (refresh-plot (elt data 1) (elt data 7)
                                           goodpts apmamplot))
                      (if apsabans (refresh-plot (elt data 2) (elt data 7)
                                           goodpts apsabplot))
                      (if apslakeans (refresh-plot (elt data 3) (elt data 7)
                                           goodpts apslakeplot))
                      (if opbpcans (refresh-plot (elt data 4) (elt data 7)
                                           goodpts opbpcplot))
                      (if oprcans (refresh-plot (elt data 5) (elt data 7)
                                           goodpts oprcplot))
                      (if opslakeans (refresh-plot (elt data 6) (elt data 7)
                                           goodpts opslakeplot))))

               (cond (residvsy
               (cond ((and newplot2 residvsy)
                       (def residvsyplot 
                             (plot-points (select (elt data 7) (which goodpts))
                                          (send reg1 :residuals)
                                   :title "Residuals vs. BSAAM"
                                   :variable-labels 
                                        (list "BSAAM" "Residuals")))
                       (send residvsyplot :linked t)
                       (setf newplot2 nil))
                       (update
                               (let (
                                     (x (select (elt data 7) (which goodpts)))
                                     (y (select (send reg1 :residuals)
                                          (which goodpts)))
                                    )
                                 (send residvsyplot :clear-points)
                                 (send residvsyplot :add-points x y)
                                 (send residvsyplot :adjust-to-data)
                               )))))

               (cond (residvstime 
               (cond ((and newplot3 residvstime) 
                        (def rvstimeplot 
                            (plot-points (select (elt data 0) (which goodpts))
                                            (send reg1 :residuals)
                                 :title "Residuals vs. TIME"
                                 :variable-labels (list "Time" "Residuals")))
                        (send rvstimeplot :linked t)
                        (setf newplot3 nil))
                       (update (let (
                                     (x (select (elt data 0) (which goodpts)))
                                     (y (select (send reg1 :residuals)
                                                (which goodpts)))
                                    )
                                 (send rvstimeplot :clear-points)
                                 (send rvstimeplot :add-points x y)
                                 (send rvstimeplot :adjust-to-data)
                               )))))))))
             )
             (send dialog-proto :new (list (list deptell) 
                                           (list indepask)
                                           (list interceptask)
                                           (list dateask)
                                           (list apmamask) 
                                           (list apsabask)
                                           (list apslakeask) 
                                           (list opbpcask) 
                                           (list oprcask) (list opslakeask)
                                           (list plotask)
                                           (list indepvsdepask)
                                           (list residvsyask)
                                           (list residvstimeask) 
                                           (list do-it)
                                           (list reprintask)
                                           (list includeask)))
     )
 )
)

(defmeth dialog-proto :close ()
 (exit)
)

(start)


(defmeth scatterplot-proto :close ()
    (call-next-method)
    (let (
          (vars (variables))
          (varplot (first (which (mapcar #'(lambda (x) (equalp x self)) 
                       (mapcar #'symbol-value (variables))))))
         )
   
     (undef (elt vars varplot))
    )
)

(defun refresh-plot (data1 data2 goodpts plot)
 (let (
       (x (select data1 (which goodpts)))
       (y (select data2 (which goodpts)))
      )
   (send plot :clear-points)
   (send plot :add-points x y)
   (send plot :adjust-to-data)
 )
)

