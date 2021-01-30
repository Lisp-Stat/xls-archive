


;;;;
;;;;  ___________________  Variance Inflation Factors  ____________________
;;;;
;;;;
;;;;     vif.lsp © Robert A. Stine, 1994
;;;;
;;;;     6 Jan 94 ... Created
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use


#||

; After loading this file, try out the following example using the
; stack loss data distributed by Tierney with Lisp-Stat.  This file adds
; several methods to the regression model object, but does not define
; anything so elaborate as a "mix-in" or new class of regression models.
; It merely extends the set of messages used by regression models.

  (def loss '(42 37 37 28 18 18 19 20 15 14 14 13 11 12  8  7  8  8  9 15 15))
  (def air  '(80 80 75 62 62 62 62 62 58 58 58 58 58 58 50 50 50 50 50 56 70)) 
  (def temp '(27 27 25 24 22 23 24 24 23 18 18 17 18 19 18 18 19 19 20 20 20))
  (def conc '(89 88 90 87 87 87 93 93 87 80 89 88 82 93 89 86 72 79 80 82 91))

  (def rm (regression-model (list temp conc air) loss
                            :predictor-names (list "Temp" "Conc" "Air")))

  (send rm :vif-plot)

||#

(defmeth regression-model-proto :VIF (&key (print t))
  ; Vector of variance inflation factors.
  (unless (send self :intercept)
          (message-dialog "Use VIF only for models with an intercept.")
          (return))
  (unless (send self :has-slot 'vif) 
          (send self :add-slot 'vif)
          (send self :calc-vif)  )
  (let ((vif (slot-value 'vif))   )
    (when print
          (format t "Variables and Square Roots of VIF's.~%")
          (mapcar #'(lambda (n v) (format t "~a --> ~a~%" n v))
                  (send self :predictor-names) (sqrt vif))   )
    vif))

  
(defmeth regression-model-proto :CALC-VIF ()
  (setf (slot-value 'vif)
        (if (= 2 (send self :num-coefs))
            #(1)
            (flet ((ss (x) (let ((dev (- x (mean x))))
                             (sum (* dev dev)))))
              (let* ((s     (send self :sigma-hat))
                     (coefs (cdr (iseq (send self :num-coefs))))
                     (ss    (mapcar #'ss (column-list (send self :x))))
                     (se  (cdr (send self :coef-standard-errors)))    ) ; diag XXi
                (/ (* se se ss) (* s s))
                )))  ))


(defmeth regression-model-proto :VIF-PLOT ()
  ; Queries for variable to use, then builds the assciated slider dialog.
  (let ((var (send self :vif-dialog))  )
    (when var
          (let* ((xName (select (send self :predictor-names) var))
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
            ))))

#|| --- some options for fun

            ; (send p :point-symbol (which (< 0 fit)) 'cross) ; code up/dn movers

            ; (mapcar #'(lambda (a b c d)                     ; draw line segments
            ;             (send p :add-lines (list (list a b) (list c d)))  )
            ;         x (- x fit) comp (- comp (* coef fit)))

||#

(defmeth regression-model-proto :PARTIAL-FIT (xIndex)
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
    

(defmeth regression-model-proto :VIF-DIALOG ()
  ; Dialog for determining which variable to use.
  (when (= 2 (send self :num-coefs))
        (format t "Only useful in multiple regression.~%")
        (return))
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


