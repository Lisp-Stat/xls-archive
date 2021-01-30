;;;;
;;;;     Bootstrap Simulation Objects _ Robert A. Stine
#||
     26 Jul 94 ... Bit of a kludge to build global *bs* object.
     29 Mar 94 ... Adjustments.
     30 Jun 93 ... From old bootsim.
     29 Apr 92 ... Use recorders.
     25 Apr 92 ... Cut from simobjs.
||#
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisBoot")

;;;;

(require "axisUtils")
(require "axisData")

;;;;


#||

; several functions
(def ops     (list #'mean #'median #'standard-deviation))
(def genForm '(normal-rand 10))
(def  bs (send bootstrap-proto :new ops genForm
               (first *datasets*)
               :labels '(mean median sd)
               :name "Bootstrap Univariate"  ))
(send bs :run 10)
(send bs :replications)


; one vector function
(def op    (list #'fivnum))
(def  boot (send bootstrap-proto :new op genForm (first *datasets*)))
(send boot :run 10)
(send boot :replications)


(let ((ops '(mean median)))
  (eval (cons 'list
        (mapcar #'(lambda (f) `(function ,f))
                ops))))


 (def b
      (bootstrap "Test"
                 (list 'mean 'median)
                 '(normal-rand 10)
                 (first *datasets*)
                 10))
 (send b :run 20)  ; more reps

||#

(defun BOOTSTRAP (name ops genForm srcDataset b &key labels)
  ;(format t
  ;        "BS: ~a~%ops    ~a~%form   ~a~% ds    ~a~% b     ~a~%labels ~a~%" 
  ;        name ops genForm (send srcDataset :name) b labels)
  ;(format t "Test eval ~a~%"
  ;        (funcall (first ops) (send srcDataset :evaluate genForm)))
  (let ((boot (send bootstrap-proto :new
                    (mapcar #'(lambda (f)
                                (if (functionp f)
                                    f (eval `(function ,f))))
                            ops)
                    genForm srcDataset
                    :labels (if labels labels ops)
                    :name name)))
    (send boot :run b)
    (def *bs* boot)
    boot))

(defun RESAMPLE (x &optional n)
  (sample x (if n n (length x)) t))


;;;     _____  Prototype for bootstrap models  _____


(defproto bootstrap-proto
  '(name          ; name string
    operators     ; list of the functions to call
    labels        ; names of the results
    bsReps        ; bootstrap results

    sourceDS      ; context dataset
    destDS        ; where to put things
    
    newSample?    ; same samples for each operator, or new for each?

    trialData     ; current trial data
    trialDataForm ; form to gen common trial data

    thetaHat      ; full sample estimate of the statistic (list)
    a             ; estimated acceleration constant (list)

    seedInit      ; initial seed at start of simulation
    seedCurr      ; seed to use when simulation is resumed
    ))

;;     __________  Creation  __________

(defmeth bootstrap-proto :ISNEW (operators
                                 trialDataForm
                                 sourceData
                                 &key newsample? name labels)
  (format t "-------> ~a "
  (setf (slot-value 'bsReps)        nil)
  (setf (slot-value 'operators)     (if (listp operators)
                                        operators (list operators)))
  (setf (slot-value 'labels)   (mapcar ; 3/29/94
                                  (if (eq 'resample (first trialDataForm))
                                      #'(lambda (s) (string-to-symbol 
                                                     (strcat (string s) "_b")))
                                      #'(lambda (s) (string-to-symbol 
                                                     (strcat (string s) "_s")))   )
                                       (if (listp labels)
                                           labels (list labels)))  )
  (setf (slot-value 'trialDataForm) trialDataForm)
  (setf (slot-value 'sourceDS)      sourceData)
  (setf (slot-value 'name)          name)
  (setf (slot-value 'newSample?)    newsample?)
  (setf (slot-value 'trialData)     nil)
  (let ((seed (make-random-state    t)))
    (setf (slot-value 'seedInit)    (make-random-state seed))
    (setf (slot-value 'seedCurr)    (make-random-state seed))
    ))
  )

(defmeth bootstrap-proto :RESET-SEED (&key newSeed)
  (setf (slot-value 'seedCurr)
        (if newSeed newSeed
            (make-random-state (slot-value 'seedInit))
            ))
  )

(defmeth bootstrap-proto :RESET ()
  (setf (slot-value 'trialData) nil)
  (setf (slot-value 'a) nil)
  (setf (slot-value 'thetaHat) nil)  )


;;     __________  Accessors  __________

(defmeth bootstrap-proto :INITIAL-SEED ()
  (make-random-state (slot-value 'seedInit))
  )

(defmeth bootstrap-proto :CURRENT-SEED ()
  (make-random-state (slot-value 'seedCurr))
  )

(defmeth bootstrap-proto :NAME ()
  (slot-value 'name))

(defmeth bootstrap-proto :LABELS ()
  (if (slot-value 'labels)
      (slot-value 'labels)
      (mapcar #'(lambda (l) (gensym 'bs))
              (iseq (length (first (slot-value 'bsreps))))
              )))


(defmeth bootstrap-proto :REPLICATIONS (&key asVars?)
  (if asVars?
      (let ((data (slot-value 'bsReps))  )
        (if (= 1 (length (first data)))  ))
       ; else as observations
      (slot-value 'bsReps))
  )

(defmeth bootstrap-proto :NEW-SAMPLE?-IS (tf)
  (send self :reset)
  (setf (slot-value 'newSample?) tf))

(defmeth bootstrap-proto :TRIAL-DATA-FORM-IS (form)
  (send self :reset)
  (setf (slot-value 'trialDataForm) form))

(defmeth bootstrap-proto :GEN-TRIAL-DATA ()
  (setf (slot-value 'trialData)
        (send (slot-value 'sourceDS) :evaluate
              (slot-value 'trialDataForm))))

(defmeth bootstrap-proto :TRIAL-DATA ()
  (slot-value 'trialData))

(defmeth bootstrap-proto :B ()
  (length (slot-value 'bsReps)))

(defmeth bootstrap-proto :SIZE ()   ; does anything call this?
  (length (slot-value 'bsReps)))


(defmeth bootstrap-proto :INPUT-DATA ()
  ; Recovers the original input data.            7/28/94
  (let ((form (slot-value 'trialDataForm))  )
    (if (eq 'resample (first form))
        (send (slot-value 'sourceDS) :evaluate (second form))  ; get data
        (iseq (length (slot-value 'trialData)))                ; gen indices
        )))

(defmeth bootstrap-proto :THETA-HAT ()
  ; Computes the original estimate from the full sample.
  (if (slot-value 'thetaHat)
      (slot-value 'thetaHat)
      (setf (slot-value 'thetaHat)
            (let* ((funcs  (slot-value 'operators))
                   (data   (send self :input-data))   )
              (if (= 1 (length funcs))
                  (let ((z (funcall (first funcs) data))  )
                    (if (listp z) z (list z)))
                  (mapcar #'(lambda (f)
                              (funcall f data))
                          funcs)
                  )))))


(defmeth bootstrap-proto :JACKKNIFE ()          ;    7/25/94
  ; Returns the leave-one-out values of the estimator arranged
  ; as a list of columns, one column for each estimator.
  (let* ((data  (send self :input-data))
         (n     (length data))
         (indx  (iseq (length data)))
         (funcs (send self :slot-value 'operators))
         (ui    ())    )
    (if (= 1 (length funcs))
        (let ((f (first funcs))  )
          (dotimes (trial n)
                   (push 
                    (funcall f (select data (remove trial indx)))
                    ui)))
        (dotimes (trial n)
                 (let ((subset (select data (remove trial indx)))  )
                   (push
                    (mapcar #'(lambda (f) (funcall f subset))
                            funcs)
                    ui)
                   )))
    (if (listp (first ui))
        (transpose ui)
        (list ui))))
 

(defmeth bootstrap-proto :A ()
  ; Acceleration constant
  (if (slot-value 'a) (slot-value 'a)
      (setf (slot-value 'a)
            (mapcar #'(lambda (u)
                        (let ((dev (- (mean u) u))  )
                          (/ (sum (^ dev 3))
                             (* 6 (^ (sum (^ dev 2)) 1.5))  )))
                    (send self :jackknife)
                    ))))
                   
           
;;
;;     Bootstrap Intervals
;;

(defmeth bootstrap-proto :INTERVALS(cc)
  ; Produces all three types      7/26/94
  (terpri)
  (send self :percentile-interval cc)
  (terpri)
  (send self :bias-corrected-interval cc)
  (terpri)
  (send self :accelerated-interval cc) )

(defmeth bootstrap-proto :PERCENTILE-INTERVAL (cc)           ; 7/25/94
  ; Returns two-sided 1-alpha interval for whatever.
  (let* ((lo  (/ (- 1 cc) 2))
         (hi  (+ cc lo))    )
    (format t "Bootstrap Percentile Intervals (~a%)~%" (* 100 cc))
    (mapcar #'(lambda (txt x)
                (let ((q (quantile x (list lo hi)))  )
                  (format t "~10a  [ ~a, ~a ]~%" txt (first q) (second q))  ))
            (send self :labels)
            (if (listp (first (slot-value 'bsReps)))
                (transpose (slot-value 'bsReps))
                (list (slot-value 'bsReps)))
            ))
  (format t "----------------------~%")
  )

  
(defmeth bootstrap-proto :Z0 (bsReps est)                    ; 7/25/94
  ; The bias-correction constant based on the BS reps about est
  (normal-quant
   (/ (length (which (< bsReps est))) 
      (length bsReps)
      )))


(defmeth bootstrap-proto :BIAS-CORRECTED-INTERVAL (cc)       ; 7/25/94
  ; Returns two-sided 1-alpha bias-corrected BS interval for whatever.
  (let* ((lo  (/ (- 1 cc) 2))
         (hi  (normal-quant (+ cc lo)))
         (lo  (normal-quant lo))     )
    (format t "Bias-Corrected BS Percentile Intervals (~a%)~%" (* 100 cc))
    (mapcar #'(lambda (txt x* x^)
                (let ((z0 (send self :z0 x* x^))  )
                  (format t "Z0 = ~a~%" z0)
                  (let* ((pct (normal-cdf (+ (* 2 z0)
                                             (list lo hi)))  )
                         (q (quantile x* pct))   )
                    (format t "~10a  [ ~a, ~a ] using percentiles ~a.~%"
                            txt (first q) (second q) pct)  )))
            (send self :labels)
            (if (listp (first (slot-value 'bsReps)))
                (transpose (slot-value 'bsReps))
                (list (slot-value 'bsReps)))
            (send self :theta-hat))
    )
  (format t "----------------------~%")
  )

(defmeth bootstrap-proto :ACCELERATED-INTERVAL (cc)       ; 7/26/94
  ; Returns two-sided 1-alpha BCa BS interval for whatever.
  (let* ((thetaHat (send self :theta-hat))
         (lo  (/ (- 1 cc) 2))
         (hi  (normal-quant (+ cc lo)))
         (lo  (normal-quant lo))     )
    (format t "Accelerated BS Intervals (~a%)~%" (* 100 cc))
    (mapcar #'(lambda (txt a x* x^)
                (let* ((z0  (send self :z0 x* x^))
                       (z   (+ (list lo hi) z0))
                       (pct (normal-cdf (+ z0 (/ z (- 1 (* a z)))) ))
                       (q   (quantile x* pct))   )
                  (format t "~10a  [ ~a, ~a ] using percentiles ~a.~%"
                          txt (first q) (second q) pct)  ))
            (send self :labels) (send self :a)
            (if (listp (first (slot-value 'bsReps)))
                (transpose (slot-value 'bsReps))
                (list (slot-value 'bsReps)))
            thetaHat))
  (format t "----------------------~%")
  )
  

;;
;;

(defmeth bootstrap-proto :RUN (nTrials)
  (setf *random-state* (slot-value 'seedCurr))
  (let ((funcs    (send self :slot-value 'operators))
        (progress (progress-indicator nTrials "Bootstrap"))     )
    (if (= 1 (length funcs))
        ; apply single function to samples from generate form
        (let ((f (first funcs))  )
          (dotimes (trial nTrials)
                   (send progress :at (1+ trial))
                   (push (funcall f (send self :gen-trial-data))
                         (slot-value 'bsReps))
                   ))
        ; else map over the input functions
        (dotimes (trial nTrials)
                 (send progress :at (1+ trial))
                 (send self :gen-trial-data)  ; one trial gen for all funcs
                 (push
                  (mapcar #'(lambda (f)
                              (funcall f (slot-value 'trialData)))
                          funcs)
                  (slot-value 'bsReps)
                  ))   )
    (setf (slot-value 'seedCurr) (make-random-state nil))
    (send self :store-results)
    nil))
 
(defmeth bootstrap-proto :PRINT-SEEDS ()
  (format t "  Seed 0 ~a ~%" (send self :initial-seed))
  (format t "  Seed n ~a ~%" (send self :current-seed))
  )

(defmeth bootstrap-proto  :STORE-RESULTS ()
  ; Labels each item, splitting up a single one if one op.
  ; (format t "results ... ~a~%" (slot-value 'bsreps))
  (let* ((bs (slot-value 'bsReps))      ; wrap in list if one
         (x  (if (listp (first bs)) (transpose bs) (list bs)))   )
    (if (slot-value 'destDS)
        (send (slot-value 'destDS) :data-is x) ; direct reset of data
        (setf (slot-value 'destDS)
              (make-dataset-from-values
               (concatenate 'string
                            (if (eq 'resample
                                    (first (slot-value 'trialDataForm)))
                                "Bootstrap - "
                                "Simulate - ")
                            (slot-value 'name))
               (send self :labels)
               x
               :features (list
                        (cons "# Trials" 
                              (list  ; assumes str parsed by get-form
                               #'(lambda (str) (send self :run str))
                                                     )))
               )))))
            
;        (mapcar #'(lambda (label data)
;               (send (slot-value 'destDS) :add-variable label data))
;               (send self :labels) x)

  

  