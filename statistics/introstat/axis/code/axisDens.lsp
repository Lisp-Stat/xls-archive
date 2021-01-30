;;;;
;;;;     AxisDens.lsp © Robert A. Stine
;;;;
;;;;               Density command dialog and plot object.
;;;;
;;;;      3 Nov 94 ... Update for 3.39; fix width in kernel under trans.
;;;;        Jan 94 ... Add transformation slider, symmetry plot.
;;;;     25 Jul 93 ... Add QQ plot option.
;;;;      6 Jan 93 ... Some light tuning.  Add label.  Slider item.
;;;;      5 Jul 92 ... Extracted from commando.lsp
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

;;;;

(provide "axisDens")

;;;;

(require "axisUtil")  ; slider things
(require "labelPlt")

;;;;

#||

;     Build the density by hand
(setf dp (plot-density '(1 2 3 4 5 3 2  2 3 4 5) "Test"))
(send dp :slot-value 'width)

;     Build the density from dialog
(setf dcp (send density-cmd-proto :new))

||#



;;
;;     DENSITY PLOT
;;

(defun PLOT-DENSITY (variable label)
  (send density-plot-proto :new variable :kernel? t :dots? t :label label)
  )


(defproto DENSITY-PLOT-PROTO
  '(var                       ; variable being plotted
    label                     ; label for item shown
    xGrid                     ; grid for plots
    dots? kernel? box? gauss? ; booleans for contents of plot
    dItem kItem bItem gItem   ; menu items
    sItem                     ; slider menu item
    tItem power               ; transformation menu item, assoc power                     
    width                     ; for kernel smooth
    os mean sd                ; reference stats
    kernel                    ; most recent kernel 
    qqPlot symPlot            ; slots for ref to assoc plots            
    )
  '()
  labelled-plot-proto
  )

(defmeth density-plot-proto :ISNEW (var &key label
                    (kernel? t) (dots? t) box? gauss?)
  (call-next-method 2)
  (send self :title "Density Plot")
  (send self :main-label-is (concatenate 'string "Density of " label))
  (send self :size 300 150)
  (send self :margin 10 10 10 10)
  (setf (slot-value 'label) label)
  (setf (slot-value 'var) var)
  (if kernel? (send self :do-draw-kernel))
  (if dots?   (send self :do-draw-dots))
  (if box?    (send self :do-draw-boxplot))
  (send self :adjust-to-data)
  (let* ((rng (send self :range 0))
         (pad (* .05 (- (second rng) (first rng))))
         (low (- (first rng) pad))
         (hi  (+ (second rng) pad))    )
    (setf (slot-value 'xGrid) (rseq low hi 50))
    (send self :range 0 low hi)  )
  (let* ((rng (send self :range 1))
         (pad (* .05 (- (second rng) (first rng)))) )
    (send self :range 1 0 (+ (second rng) pad))  )
  (send self :x-axis t nil 5)
  (send self :y-axis t nil 2)
  )


(defmeth density-plot-proto :MENU-TEMPLATE ()
  (send self :menu-title "Density")
  (let ((qqItem (send menu-item-proto :new "QQ Plot"
                      :action #'(lambda () (send self :qq-plot))))
        (syItem (send menu-item-proto :new "Symmetry Plot"
                      :action #'(lambda () (send self :symmetry-plot))))
        (kItem (send menu-item-proto :new "Show Kernel"
                     :action #'(lambda () (send self :do-draw-kernel))))
        (dItem (send menu-item-proto :new "Show Data"
                     :action #'(lambda () (send self :do-draw-dots))))
        (bItem (send menu-item-proto :new "Show Boxplot"
                     :action #'(lambda () (send self :do-draw-boxplot))))
        (gItem (send menu-item-proto :new "Show Normal"
                     :action #'(lambda () (send self :do-draw-gaussian))))
        (sItem (send slider-menu-item-proto :new 
                     :target self
                     :label "Kernel Slider"
                     :action #'(lambda () (send self :kernel-slider))))
        (tItem (send slider-menu-item-proto :new 
                     :target self
                     :label "Transformation Slider"
                     :action #'(lambda () (send self :transform-slider))))
        (pItem (send menu-item-proto :new "Print Summary"
                     :action #'(lambda () (send self :print-summary))))
        )
    (setf (slot-value 'kItem) kItem)
    (setf (slot-value 'dItem) dItem)
    (setf (slot-value 'gItem) gItem)
    (setf (slot-value 'bItem) bItem)
    (setf (slot-value 'sItem) sItem)
    (setf (slot-value 'tItem) tItem)
    (list 'link 'color 'symbol 'redraw 
          qqItem syItem
          'dash
          kItem dItem bItem gItem 'dash sItem tItem 'dash pItem)
    ))


(defmeth density-plot-proto :UPDATE-MENU ()
  (send (slot-value 'bItem) :mark (slot-value 'box?))
  (send (slot-value 'gItem) :mark (slot-value 'gauss?))
  (send (slot-value 'dItem) :mark (slot-value 'dots?))
  (send (slot-value 'kItem) :mark (slot-value 'kernel?))
  (send (slot-value 'sItem) :enabled (slot-value 'kernel?))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Quantile Plots
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth density-plot-proto :QQ-PLOT ()
  ; Normal quantile-quantile plot with line from median and iqr
  (let* ((data (send self :data))
         (q (normal-quant (/ (iseq 1 (length data)) (1+ (length data)))))
         (p (labelled-plot q (sort-data data) 
                           (concatenate 'string "Quantile plot of "
                                        (slot-value 'label))))   )
    (setf (slot-value 'qqPlot) p)
    (send p :abline (send self :median)  (* .6745 (send self :iqr)))
    (send p :title "Quantile Plot")
    (send p :variable-label '(0 1) (list "Std Normal" "Data Scale"))
    (send p :adjust-to-data)
    p))

(defmeth density-plot-proto :REDRAW-QQ-PLOT ()   ; Separate prototype...
  (let ((qqPlot (slot-value 'qqPlot))  )
    (if (send qqPlot :allocated-p)
        (let* ((data   (send self :data))
               (q      (normal-quant (/ (iseq 1 (length data)) 
                                        (1+ (length data)))))  )
          (send qqPlot :clear-lines)
          (send qqPlot :clear-points)
          (send qqPlot :add-points (list q (sort data #'<)))
          (send qqPlot :abline (send self :median) (* .6745 (send self :iqr)))
          (send qqPlot :adjust-to-data)    )
        (setf (slot-value 'qqPlot) nil)
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Symmetry Plots
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth density-plot-proto :SYMMETRY-PLOT ()
  ; Symmetry plot with reference 45 deg line
  (let* ((data (sort-data (- (send self :data) (send self :median))))
         (n    (length data))
         (half (/ n 2))
         (p (labelled-plot (abs (select data (iseq (ceiling half))))
                           (select data (iseq (1- n) (floor half)))
                           (concatenate 'string "Symmetry plot of "
                                        (slot-value 'label))))   )
    (setf (slot-value 'symPlot) p)
    (send p :abline 0 1)
    (send p :title "Symmetry Plot")
    (send p :variable-label '(0 1) (list "Below Median" "Above Median"))
    (send p :adjust-to-data)
    p))

(defmeth density-plot-proto :REDRAW-SYMMETRY-PLOT ()
  (let ((sPlot (slot-value 'symPlot))  )
    (if (send sPlot :allocated-p)
        (let* ((data  (sort-data (- (send self :data) (send self :median))))
               (n     (length data))
               (half  (/ n 2))  )
          (send sPlot :clear-points) (send sPlot :clear-lines)
          (send sPlot :add-points
                (list (abs (select data (iseq (ceiling half))))
                      (select data (iseq (1- n) (floor half))) )  )
          (send sPlot :adjust-to-data)
          (send sPlot :abline 0 1)  )
        (setf (slot-value 'symPlot) nil)
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Data and Summary Statistics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       
(defmeth density-plot-proto :DATA ()
  ; If no power or power is one, var. Otherwise transforms.
  ; (format t "Reading data with power = ~a~%" (slot-value 'power))
  (let ((power (slot-value 'power)))
    (if power
        (if (= power 0)
            (log (slot-value 'var))
            (/ (- (^ (slot-value 'var) power) 1) power)  )
        (slot-value 'var)
        )))


(defmeth density-plot-proto :PRINT-SUMMARY ()
  (format t "Summary of ~a.~%" (send self :title))
  (format t "  Mean     S.D.      Min      25%     Median     75%     Max~%")
  (let ((m   (send self :mean))
        (sd  (send self :sd))
        (os  (float (send self :order-stats)))  )
    (format t "~8,3g ~8,3g ~8,3g ~8,3g ~8,3g ~8,3g ~8,3g~%"
            m sd (aref os 0) (aref os 1) (aref os 2)
            (aref os 3) (aref os 4))
  ))

(defmeth density-plot-proto :POWER-IS (p)                       ; 3 Nov 94
  ; Sets the associated transformation power, clearing all stat slots.
  ; Prepares display for new power.
  (let ((oldiqr (send self :iqr))    )
    (setf (slot-value 'power) p)
    (setf (slot-value 'kernel) nil)
    (setf (slot-value 'os)     nil)
    (setf (slot-value 'mean)   nil)
    (setf (slot-value 'sd)     nil)
    (send self :clear-lines)
    (send self :clear-points)
    (let* ((os     (send self :order-stats))
           (min    (select os 0))
           (max    (select os 4))
           (newIqr (- (select os 3) (select os 1)))
           (pad    (* .2 newIqr))  )
      (send self :range 0 (- min pad) (+ max pad))
      (setf (slot-value 'xGrid) (rseq (- min pad) (+ max pad) 50))
      (when (slot-value 'dots?)    (send self :draw-dots))
      (when (slot-value 'kernel?)
            (setf (slot-value 'width) (* (slot-value 'width)
                                         (/ newIqr oldIqr)))
            (send self :draw-kernel))   )
    (send self :adjust-to-data)  ; call prior to boxplots
    (when (slot-value 'box?)     (send self :draw-boxplot))
    (when (slot-value 'gauss?)   (send self :draw-gaussian))
  ))
      
(defmeth density-plot-proto :ORDER-STATS ()
  (if (slot-value 'os)
      (slot-value 'os)
      (setf (slot-value 'os) (coerce (fivnum (send self :data)) 'vector))
      ))

(defmeth density-plot-proto :MEAN ()
  (if (slot-value 'mean)
      (slot-value 'mean)
      (setf (slot-value 'mean) (mean (send self :data)))
      ))

(defmeth density-plot-proto :MEDIAN ()
  (select (send self :order-stats) 2))

(defmeth density-plot-proto :SD ()
  (if (slot-value 'sd)
      (slot-value 'sd)
      (setf (slot-value 'sd) (standard-deviation (send self :data)))
      ))

(defmeth density-plot-proto :IQR ()
  (let ((os (send self :order-stats))  )
    (- (select os 3) (select os 1))   ))
 
(defmeth density-plot-proto :WIDTH ()
  "Computes kernel width using sample data, if not done already."
  (if (slot-value 'width)
      (slot-value 'width)
      (setf (slot-value 'width) (send self :sd))
      ))

(defmeth density-plot-proto :KERNEL (&optional width)
  ;Retrieves kernel at built-in width if requested, otherwise calcs.
  (if (and (slot-value 'kernel)     ; got one and no change requested
           (or (not width) (= width (send self :width)))  )
      (slot-value 'kernel)
      (progn
       (if width (setf (slot-value 'width) width))
       (setf (slot-value 'kernel)
             (kernel-dens (send self :data)
                          :width (slot-value 'width)
                          :xvals (slot-value 'xGrid)))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Sliders
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmeth density-plot-proto :KERNEL-SLIDER ()
  (let ((slider (interval-slider-dialog (list 0 (* 3 (send self :sd)))
                      :action #'(lambda(wid)
                                  (send self :clear-lines)
                                  (send self :draw-kernel wid)
                                  (if (send self :slot-value 'box?)
                                      (send self :draw-boxplot))
                                  (if (send self :slot-value 'gauss?)
                                      (send self :draw-gaussian))   ))))
    (send slider :title "Kernel Slider")
    (send slider :add-slot 'menuitem)
    (send slider :menu-item-is (slot-value 'sItem))
    (send slider :value (slot-value 'width))
    (send self :add-subordinate slider)
    (send (slot-value 'sItem) :slider-is slider)
    ))


(defmeth density-plot-proto :TRANSFORM-SLIDER ()
  (let ((slider (interval-slider-dialog (list -2 3)
                      :action #'(lambda(power)
                                  (send self :power-is power)
                                  (if (send self :slot-value 'qqPlot)
                                      (send self :redraw-qq-plot))
                                  (if (send self :slot-value 'symPlot)
                                      (send self :redraw-symmetry-plot))  ))))
    (send slider :title "Power Slider")
    (send slider :add-slot 'menuitem)
    (send slider :menu-item-is (slot-value 'tItem))
    (send slider :value 1)
    (send self :add-subordinate slider)
    (send (slot-value 'tItem) :slider-is slider)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     DO-DRAW methds
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmeth density-plot-proto :DO-DRAW-DOTS ()   
  (if (slot-value 'dots?)
      (send self :clear-points)
      (send self :draw-dots))
  (setf (slot-value 'dots?) (not (slot-value 'dots?)))
  (send self :update-menu)
  )

(defmeth density-plot-proto :DO-DRAW-BOXPLOT ()
  (if (slot-value 'box?)
      (progn (send self :clear-lines)
             (if (slot-value 'kernel?) (send self :draw-kernel))
             (if (slot-value 'gauss?) (send self :draw-gaussian))  )
      (send self :draw-boxplot))
  (setf (slot-value 'box?) (not (slot-value 'box?)))
  (send self :update-menu)
  )

(defmeth density-plot-proto :DO-DRAW-KERNEL ()   
  (if (slot-value 'kernel?)
      (progn (send self :clear-lines)
             (when (slot-value 'subordinates)  ; kill the slider too
                   (send (first (slot-value 'subordinates)) :close)
                   (send self :delete-subordinate
                         (first (slot-value 'subordinates))))
             (if (slot-value 'box?) (send self :draw-boxplot))
             (if (slot-value 'gauss?) (send self :draw-gaussian))  )
      (send self :draw-kernel (send self :width) ))
  (setf (slot-value 'kernel?) (not (slot-value 'kernel?)))
  (send self :update-menu)
  )

(defmeth density-plot-proto :DO-DRAW-GAUSSIAN ()   
  (if (slot-value 'gauss?)
      (progn (send self :clear-lines)
             (if (slot-value 'box?) (send self :draw-boxplot))
             (if (slot-value 'kernel?) (send self :draw-kernel))  )
      (send self :draw-gaussian))
  (setf (slot-value 'gauss?) (not (slot-value 'gauss?)))
  (send self :update-menu)
  )




(defmeth density-plot-proto :DRAW-DOTS ()
  (let ((data (send self :data))  )
    (send self :add-points (list data (repeat 0 (length data))))
    ))

(defmeth density-plot-proto :DRAW-BOXPLOT ()
  "Horizontal boxplots just about offsets."
  (let* ((x    (send self :order-stats))
         (rng  (send self :range 1))
         (size (- (second rng) (first rng)))
         (wid  (* .2 size))
         (d    (+ (first rng) (* .1 size)))
         (m    (+ d (* .5 wid)))
         (u    (+ d wid))  )
    (send self :add-lines (list
                           (select x '(0 1 1 3 3 4 3 3 2 2 2 1 1))
                           (list       m m u u m m m d d u d d m)))
    ))

(defmeth density-plot-proto :DRAW-KERNEL (&optional width)
  (send self :add-lines (send self :kernel width))  )

(defmeth density-plot-proto :DRAW-GAUSSIAN ()
  (let* ((x   (slot-value 'xGrid))
         (sx  (/ (- x (send self :mean)) (send self :sd)))  )
    (send self :add-lines (list x (/ (normal-dens sx) (send self :sd)))
          :color 'red)
    ))
              

          
