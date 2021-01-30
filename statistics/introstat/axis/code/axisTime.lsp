;;;;
;;;;     axisTime.lsp © Robert A. Stine, 1994
;;;;
;;;;             Add in module for augmenting Axis with some features
;;;;             for handling time series operations.
#||
     7 Apr 94 ... Added as Axis module.
    17 Apr 92 ... Created as original stand-alone code.
||#
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(require "axisRegr")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;         Add time commands to the statistics menu.       ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ADD-TIME-SERIES-ITEMS ()
  (send *statistics-menu* :append-items
        (send stat-menu-item-proto :new
              "Time Plot" time-plot-cmd-proto)
        (send stat-menu-item-proto :new
              "Autocorrelation" autocorr-cmd-proto))
  )

(defun GDIFF (x rho)
  (- (rest x) (* rho (butlast x))  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                     TIME PLOT                           ;;
;;                                              7 Apr 94   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto TIME-PLOT-CMD-PROTO
  '(joinToggle)
  '(classIDCode)       ;; required for every "type" of command
  commando-proto)

(defmeth time-plot-cmd-proto :ISNEW ()
  (send self :init-slots
        "Time Plot"
        'time-plot
        (list (list "Series ->") "Time ->"))
  (call-next-method))

(defmeth time-plot-cmd-proto :COMMAND ()
  (let ((form (send self :form))
        (text (send self :text))  )
    (cons
     (slot-value 'cmdForm)
     `((list ,@(first form))   ; get first and wrap with list
       ,(second form)
       ',(first text)
       ',(second text)
       ,(send (slot-value 'joinToggle) :value)
       ))))


(defmeth time-plot-cmd-proto :TOGGLE-ITEMS ()
  (let ((ti (send toggle-item-proto :new "Join" :value t))   )
    (setf (slot-value 'joinToggle ) ti)
    (list ti)
    ))

; (def tpc (send time-plot-cmd-proto :new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                    AUTOCORR                             ;;
;;                                              7 Apr 94   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defproto AUTOCORR-CMD-PROTO
  ()
  '(classIDCode)       ;; required for every "type" of command
  commando-proto)

(defmeth autocorr-cmd-proto :ISNEW ()
  (send self :init-slots
        "Autocorr"
        'auto-corr-plot
        "Series ->" )
  (call-next-method))

(defmeth autocorr-cmd-proto :COMMAND ()
  (append (call-next-method)
          (send self :text)) )



; (def ac (send autocorr-cmd-proto :new))


           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;         Special function for time series analysis.      ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun TIME-PLOT (series time yNames timeName join?)
  ; Plot of multiple series on the same time axis
  (let* ((colors '(red blue magenta cyan))
         (f  (if join? #'plot-lines #'plot-points))
         (fa (if join? :add-lines  :add-points))
         (p  (funcall f time (first series)
                      :variable-labels (list timeName (first yNames))))  )
    (when (< 1 (length series))
          (send p :use-color t)
          (mapcar #'(lambda (y c) (send p fa time y :color c))
                      (rest series)
                      colors)
          (send p :adjust-to-data)
          )
    p))
  
; (time-plot (list (uniform-rand 20) (uniform-rand 20)) (iseq 20)
;            (list "Name1" "Two") "Seq" t)
           
(defun AUTO-CORR-PLOT (y name &optional nLags)
  (let* ((n     (length y))
         (nLags (if nLags nLags (ceiling (/ n 5))))
         (z     (- y (mean y)))
         (var   (inner-product z z))
         (z+    (copy-list z))
         (x     (iseq 0 nLags))
         (acf   (list 1))  )
    (dotimes (i nLags)
             (setf z  (butlast z))
             (setf z+ (rest z+))
             (push (/ (inner-product z z+) var) acf)   )
    (let* ((acf (reverse acf))
           (p   (plot-points x acf
                             :variable-labels "Lag" name)))
      (send p :abline 0 0)
      (send p :title "Autocorrelation"
            (send p :add-lines x acf))
      (format t "Autocorrelations of ~a:~% ~a~%" name acf)
      p)))
     
; (auto-corr-plot (uniform-rand 100) "ubb" 10)        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;          Added regression methods                       ;;
;;                                              7 Apr 94   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmeth axis-regression-proto :RHO-IS (rho) 
  (unless (send self :has-slot 'rho)
          (send self :add-slot 'rho))
  (setf (slot-value 'rho) rho))

(defmeth axis-regression-proto :LASTX-IS (last)  ; big-time kludge
  (unless (send self :has-slot 'lastX)
          (send self :add-slot 'lastX))
  (setf (slot-value 'lastX) (coerce last 'list)))


(defmeth axis-regression-proto :LASTY-IS (last)  ; big-time kludge
  (unless (send self :has-slot 'lastY)
          (send self :add-slot 'lastY))
  (setf (slot-value 'lastY) last))


(defmeth axis-regression-proto :AUTOCORR (z)
  (let* ((num (sum (* (cdr z) (butlast z))))
         (den (sum (* z z)))  )
    (/ num den)))

(defmeth axis-regression-proto :LAG-RESIDUAL-PLOT ()
  (let* ((z    (send self :residuals))
         (lagz (butlast z))
         (z    (rest z)) 
         (p    (plot-points lagz z))  )
  (send p :variable-label '(0 1) (list "Lag Residual" "Residual"))
  (send p :adjust-to-data)
  p))

(defmeth axis-regression-proto :GDIFFS (rho &key (all t))
  "Compute generalized diffs of x and y based on input rho."    ; 4/12/94
  (let* ((y   (coerce (send self :y) 'vector))
         (n   (length y))
         (x   (column-list (send self :x)))
         (k   (sqrt (- 1 (^ rho 2))))
         (lead (iseq 1 (- n 1)))
         (lag  (iseq 0 (- n 2)))   ) 
    (flet ((diff (v)
                 (setf (select v lead)
                       (- (select v lead) (* rho (select v lag))))
                 (if all
                     (setf (aref v 0) (* k (aref v 0)))
                     (setf v (select v lead)))
                 v))
      (list
       (apply #'bind-columns (mapcar #'diff x))
       (diff y))
      )))


(defmeth axis-regression-proto :ESTIMATE-AUTOCORR ()
  (let ((final 0)
        (res (send self :residuals))
        (y   (slot-value 'y))
        (dim (array-dimension (slot-value 'x) 1))
        (x   (slot-value 'x))  )
    (labels ((coIter (r)
                     (format t "Iterating, rho = ~6,3f ~%" r )
                     (let* ((xy (send self :gdiffs r))
                            (sm (apply #'make-sweep-matrix xy))
                            (c  (select (first
                                         (sweep-operator sm (iseq 1 dim)))
                                        (1+ dim) (iseq 0 dim)))
                            (c   (coerce (compound-data-seq c) 'list))
                            (z   (- y (+ (first c) (matmult x (rest c)))))
                            (rho (send self :autocorr z))  )
                      (if (> (abs (- rho r)) .02)  ;continue
                          (coIter rho)
                          (progn
                           (format t "End C-O, rho=~6,3f @ ~a~%" rho c)
                           (setf final rho)  )) 
                      )))
      (coIter (send self :autocorr (send self :residuals)))
      final
    )))

(defmeth axis-regression-proto :FORM-PREDICTOR-MODEL ()
  "Rearrange model terms for prediction."
  (let* ((rho    (send self :estimate-autocorr))
         (y      (send self :y))
         (lagy   (butlast y))
         (x      (bind-columns lagy (first (send self :gdiffs rho :all nil))))
         (xnames (cons "LAG Y" (mapcar #'(lambda (s) (strcat "GDIFF-" s))
                                       (send self :predictor-names))))   )
    (send self :rho-is    rho)
    (send self :lastx-is  (first (last (row-list (send self :x)))))
    (send self :lasty-is  (first (last y)))
    (send self :x x)
    (send self :y (rest y))
    (send self :predictor-names xnames)
    (send self :display)
    ))

(defmeth axis-regression-proto :COCHRAN-ORCUTT ()
  "Computes the Cochran-Orcutt fit."
  (let* ((rho    (send self :estimate-autocorr))
         (gd     (send self :gdiffs rho))
         (xnames (mapcar #'(lambda (s) (strcat "GDIFF-" s))
                         (send self :predictor-names)))
         (yname  (strcat "GDIFF-" (send self :response-name)))
         (model  (regress (first gd) (second gd) 
                          :predictor-names xNames :response-name yname))
         )
    (send model :rho-is    rho)
    (send model :lastx-is  (first (last (row-list (send self :x)))))
    (send model :lasty-is  (first (last y)))
    model
    ))

;;;;;;;;

(add-time-series-items)

