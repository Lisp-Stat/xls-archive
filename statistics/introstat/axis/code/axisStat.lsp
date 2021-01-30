;;;;
;;;;     AxisStats.lsp © Robert A. Stine, 1992
;;;;                 Macros/Objects for various statistics.
;;;;
;;;;     3 Jul 92 ... Created
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(require "axisutil")

;;;;

(provide "axisStat"

;;;;

(defproto statistic-proto
  '(type           ;  as in histogram, mean,...
    name           ;  individual identifier
    dataForm       ;  input data as a lisp form
    n              ;  number obs used in last evaluation
    optionList     ;  association list of options
    parameterList  ;  association list of parameters with most recent ests
    bootstrap      ;  bootstrap recorde
    jackknife      ;  jackknife results
   ))


(defmeth statistic-proto :TYPE ()
  (slot-value 'type))

(defmeth statistic-proto :NAME ()
  (slot-value 'name))

(defmeth statistic-proto :NAME-IS (name)
  (setf (slot-value 'name) name))


(defmeth statistic-proto :INPUT-DATA ()
  (eval (slot-value 'dataForm))  )

(defmeth statistic-proto :N ()
  (slot-value 'n))

(defmeth statistic-proto :OPTIONS ()
  (if (slot-value 'option-list)
      (mapcar #'car (slot-value 'optionList) )))

(defmeth statistic-proto :OPTION-VALUE (opt)
  (cdr (assoc opt list)))

(defmeth statistic-proto :OPTION-IS (option value)
  (if (slot-value 'optionList)
      (replace-if #'(lambda (x) (eql (car x) option))
                  (slot-value 'optionList)
                  (cons option value))
      ))

(defmeth statistic-proto :DIALOG ()                ; OVERRIDE
  () )


(defmeth statistic-proto :UPDATE ()                ; OVERRIDE
  () )

(defmeth statistic-proto :CALCULATE ()
  "Returns the raw results of calculation as an assoc list."
  () )

(defmeth statistic-proto :CALC-FUNCTION ()         ; OVERRIDE
  "Returns function that does evaluation with data as input."
  nil )

(defmeth statistic-proto :OUTPUT-DATA ()           ; OVERRIDE
  nil )


(defmeth statistic-proto :PARAMETERS ()
  (mapcar #'car (slot-value 'parameterList))  )

(defmeth statistic-proto :PARAMETER-LIST ()
  (slot-value 'parameterList) )

(defmeth statistic-proto :PARAMETER-EST (parm)
  (cdr (assoc parm (slot-value 'parameterList)))  )

(defmeth statistic-proto :PARAMETER-SE (parm)        ; OVERRIDE
  nil )


(defmeth statistic-proto :PARAMETER-CI (parm cc &key (method 'tStat))  
  (cond                                              ; OVERRIDE ?
    ((eql method 'tStat)
     (let* ((loc   (send self :parameter-est parm))
            (tVal  (t-cdf (+ cc (* .5 (- 1 cc)))))
            (wid   (* tVal (send self :parameter-se)))   )
        (list (- loc wid) (+ loc wid))  ))
    ((eql method 'bsPct)
     (if (slot-value 'bootstrap)
         (let* ((sorted (sort-data (slot-value 'bootstrap)))
                (B      (length (slot-value 'bootstrap)))
                (endPct (* .5 (- 1 cc)))
                (index  (list (floor   (* B endPct))
                              (ceiling (* B (- 1 endPct)))))   )
           (select sorted index)   )))
    ))
                

(defmeth statistic-proto :PRINT ()                   ; OVERRIDE
  (format t "~%Statistic ~a:~a -->~%" 
          (slot-value 'type) (slot-value 'name))
  )

(defmeth statistic-proto :PLOT (&key addTo)           ; OVERRIDE
  () )

(defmeth statistic-proto :DIAGNOSE ()                 ; OVERRIDE
  () )


(defmeth statistic-proto :DF ()                       ; OVERRIDE ?
  (slot-value 'n))

(defmeth statistic-proto :TEST (parm value &key (method 'tStat))
  "Returns p-value a of test of the hypothesis parm=value."
  (cond
    ((eql method 'tStat)
     (let* ((est (send self :parameter-est parm))
            (se  (send self :parameter-se  parm))
            (ts  (/ est se))
            (pv  (* 2 (t-cdf (abs ts) (send self :df))))   )
       (list pv ts)  ))
    ))
            

(defmeth statistic-proto :BOOTSTRAP (bLim &key method)
  (let ((stat (send self :calc-function))
        (data (eval (slot-value 'dataForm)))
        (reps () )    )
    (dotimes (b bLim)
             (push (funcall stat data) reps)   )
    (if (and (slot-value 'bootstrap)
             (ok-or-cancel-dialog "Append to prior results?")  )
        (push reps (slot-value 'bootstrap))
        (setf (slot-value 'bootstrap) reps)
        )))


(defmeth statistic-proto :BOOTSTRAP-OF (parm)
  (cdr (assoc parm (slot-value 'bootstrap)))  )

(defmeth statistic-proto :JACKKNIFE ()
  (setf (slot-value 'jackknife) nil)
  (let* ((stat (send self :calc-function))
         (data (eval (slot-value 'dataForm)))
         (n    (length data))   
         (reps () )    )
    (dotimes (i n)
             (push (funcall stat (select data (remove i (iseq n))))
                   (slot-value 'jackknife))
             )))

(defmeth statistic-proto :JACKKNIFE-OF (parm)
  (cdr (assoc parm (slot-value 'jackknife)))  )

