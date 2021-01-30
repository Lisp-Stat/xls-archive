;;;;
;;;;     Comparison Plots © Robert A. Stine
;;;;
;;;;           25 Jul 93 ... Compare and scale; remove build menu.
;;;;           28 Apr 92 ... Built to hold bootstrap results.
;;;;           28 May 92 ... Adjust menu, source call-back, selection.
;;;;           17 Jun 93 ... Some menu adjustments.
;;;;

;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use


(provide "compare")

;;;;

                     
#||         EXAMPLE

(def cp (send comparison-plot-proto :new
              (list
               (uniform-rand 10) (+ 10 (normal-rand 10)) (normal-rand 10))
              nil))

(send cp :center)


(slot-value cp 'boxplots?)

||#               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun COMPARE (data &key recoverFunc vertical? labels title)
  (send comparison-plot-proto :new
        data recoverFunc
        :vertical? vertical?
        :labels labels
        :title title))

; (setf cp (compare (list (iseq 10) (iseq 10)) :labels '("One" "Two")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto labelled-graph-proto
  '(
    labels     ; strings for labels
    pts        ; position in data coordinates
    )
  ()
  (list graph-proto))

(defmeth labelled-graph-proto :ADD-LABELS (labels pts)
  (setf (slot-value 'labels) labels)
  ;  (format t "label points ~a~%" pts)
  (setf (slot-value 'pts) pts))

(defmeth labelled-graph-proto :DRAW-LABELS ()
  (if (slot-value 'labels)
      (mapcar #'(lambda (l c) 
                  (let ((pt (send self :scaled-to-canvas
                                  (first c) (second c))))
                    (send self :draw-string l (first pt) (second pt))  ))
              (slot-value 'labels) (slot-value 'pts))
        ))

(defmeth labelled-graph-proto :REDRAW ()
  (call-next-method)
  (send self :draw-labels))


;;     _____  PROTOTYPE  _____

(defproto comparison-plot-proto     ;;;;  FIELD OR MIXIN???
  '(
    data os               ; data and order stats of data
    recoverFun            ; recoverFcn recovers source data for point
                          ; via callback-source; scalar func of index
    nFrames               ; nFrames is the number of cols to plot
    labels                ; frame labels
    vertical? min max     ; extremes for old scale if not vertical
    sep offsets           ; sep is coordinate size, offsets are starts
    kernels? dots? boxplots?
    location              ; these are non-nil if data have been shifted
    scale                 ; when viewed in vertical mode
    )
  ()
  (list labelled-graph-proto))


;;     _____  CREATION  _____


(defmeth comparison-plot-proto :ISNEW (data recoverFun
          &key (vertical? t) labels title kernels? dots? (boxplots? t))
  (call-next-method 2)
  (send self :size 300 200)
  (setf (slot-value 'data) data)
  (setf (slot-value 'recoverFun) recoverFun)
  (setf (slot-value 'sep) 50)
  (setf (slot-value 'nFrames) (length data))
  (setf (slot-value 'offsets) (* (slot-value 'sep)
                                 (iseq (slot-value 'nFrames))))
  (setf (slot-value 'vertical?) vertical?)
  (unless vertical?
          (setf (slot-value 'min) (mapcar #'min data))
          (setf (slot-value 'max) (mapcar #'max data)) 
          (send self :standardize)   ); reset for new x scale
  (setf (slot-value 'os) (mapcar #'fivnum (slot-value 'data)))
  (send self :title (format nil "~a ~a" 
                            (if title title "Comparing")
                            (length (first data))))
  (if kernels?  (send self :do-draw-kernels))
  (if dots?     (send self :do-draw-dots))
  (if boxplots? (send self :do-draw-boxplots))
  (send self :add-labels 
        (if labels labels     ; make sure have some
            (mapcar #'(lambda (i) (format nil "#~a" i))
                    (iseq (length data)))))
  (send self :adjust)
  )


;;     _____  ACCESSORS  _____


(defmeth comparison-plot-proto :N-FRAMES ()
  (slot-value 'nFrames))

(defmeth comparison-plot-proto :VERTICAL? ()
  (slot-value 'vertical?))


;;     _____  LABELLING  _____

(defmeth comparison-plot-proto :ADD-LABELS (labels)
  "Call after intializing plot scales to place labels."
  (if (slot-value 'vertical?)
      (let* ((x    (first (send self :visible-range 0)))
             (coor (row-list (bind-columns
                              (repeat x (length labels))
                              (+ (/ (slot-value 'sep) 2)
                                 (slot-value 'offsets)))))
             (coor (mapcar #'(lambda (c) (coerce c 'list)) coor)) )
        (call-next-method labels coor))
      (let* ((n     (slot-value 'nFrames))   ;horizontal
             (sep   (slot-value 'sep))  (off   (- (slot-value 'offsets) 5))
             (top   (repeat sep n))     (bot   (repeat (- (* .15 sep)) n))
             (minMx (combine (slot-value 'min) (slot-value 'max)))
             (lab   (combine labels
                            (mapcar #'(lambda(m) (format nil "~6,2g" m))
                                   minMx))) 
             (coor (row-list (bind-columns
                              (combine off off (+ off (* .6 sep)))
                              (combine top bot bot))))
             (coor (mapcar #'(lambda (c) (coerce c 'list)) coor)) )
        (call-next-method lab coor))   )
    )

  
(defmeth comparison-plot-proto :ADJUST ()
  "Adjust the scales and draw in labels."
  (let* ((v?    (send self :vertical?))
         (sep   (slot-value 'sep))
         (width (/ sep 5))
         (size  (+ width (* (slot-value 'nFrames) sep)))   )
    (if v?
        (send self :x-axis t nil 4))
    (send self :adjust-to-data)
    (send self :range 1 (- (* 2 width))
          (if v?   size   (* 1.2 sep)))
    (unless v? (send self :range 0 (- width) (- size (* 2 width))))
    ))


;;     _____  SCALING  _____


(defmeth comparison-plot-proto :STANDARDIZE ()
  ;  Using the min and max slots of original data.
  (let ((data (slot-value 'data))
        (sep  (* .7 (slot-value 'sep)))   ; use only 70% of range
        (min  (slot-value 'min))  (max  (slot-value 'max))
        (off  (slot-value 'offsets))  )
    (setf data (mapcar #'(lambda (v min max i)
                           (let* ((fac (/ sep (- max min)))    )
                             (setf v (+ (select off i) 
                                        (* fac (- v min))))
                             ))
                       data min max (iseq (length data))))
    (setf (slot-value 'data) data)  ))
          

(defmeth comparison-plot-proto :CENTER ()
  ;  Only done in vertical mode.
  (send self :alter 'location #'median #'(lambda (x m) (- x m)))
  )

(defmeth comparison-plot-proto :SCALE ()
  ;  Only done in vertical mode.
  (send self :alter 'scale #'interquartile-range #'(lambda (x m) (/ x m)))
  )

(defmeth comparison-plot-proto :ALTER (slot estFunc adjFunc)
  (setf (slot-value slot)
        (mapcar estFunc (slot-value 'data))  )
  (setf (slot-value 'os) (mapcar adjFunc
                                 (slot-value 'os) (slot-value slot)))
  (setf (slot-value 'pts) (mapcar #'(lambda (p m)
                                      (list (funcall adjFunc (first p) m)
                                            (second p)))
                                  (slot-value 'pts) (slot-value slot)))
  (setf (slot-value 'data)
        (mapcar adjFunc
                (slot-value 'data) (slot-value slot) ))
  (send self :clear-lines)
  (send self :clear-points)
  (if (slot-value 'kernels? ) (send self :draw-kernels))
  (if (slot-value 'dots?    ) (send self :draw-dots))
  (if (slot-value 'boxplots?) (send self :draw-boxplots))
  (send self :adjust)  )

;;     _____  MENUS AND HANDLERS  _____

(defproto cp-menu-item-proto
  '(
    plot
    slotName
    )
  ()
  menu-item-proto)

(defmeth cp-menu-item-proto :ISNEW (plot slot &rest args)
  (setf (slot-value 'plot) plot)
  (setf (slot-value 'slotName) slot)
  (apply #'call-next-method args))

(defmeth cp-menu-item-proto :UPDATE ()
  (send self :mark
        (if (send (slot-value 'plot) :slot-value (slot-value 'slotName))
            t nil)))
             
;----       

(defmeth comparison-plot-proto :MENU-TEMPLATE ()
  (send self :menu-title "Compare")
  (let ((center (send cp-menu-item-proto :new self 'location "Center"
                     :action #'(lambda () (send self :center))))
        (scale  (send cp-menu-item-proto :new self 'scale "Scale"
                     :action #'(lambda () (send self :scale))))
        (kItem (send cp-menu-item-proto :new self 'kernels? "Show Kernels"
                     :action #'(lambda () (send self :do-draw-kernels))))
        (dItem (send cp-menu-item-proto :new self 'dots? "Show Data"
                     :action #'(lambda () (send self :do-draw-dots))))
        (bItem (send cp-menu-item-proto :new self 'boxplots? "Show Boxplots"
                     :action #'(lambda () (send self :do-draw-boxplots))))
        (sItem (send menu-item-proto :new "Scatterplot Matrix"
                     :action #'(lambda () (send self :scatmat))))
        (cItem (send menu-item-proto :new "Show source"
                     :action #'(lambda () (send self :callback-source))))
        (pItem (send menu-item-proto :new "Print Summary"
                     :action #'(lambda () (send self :print-summary))))
        )
    (defmeth center :update () (call-next-method)
      (send self :enabled
            (send (slot-value 'plot) :vertical?)))
    (defmeth scale  :update ()  (call-next-method)
      (send self :enabled
            (send (slot-value 'plot) :vertical?)))
    (list 'color 'symbol 'link 'redraw 'dash
          center scale 'dash
          kItem dItem bItem 'dash sItem cItem pItem)
    ))
    


;;     _____  MENU HANDLERS  _____

(defmeth comparison-plot-proto :CALLBACK-SOURCE ()
  "Call source func to display data leading to selected elements."
  (let ((selected (send self :selection)))
    (if (and selected (slot-value 'recoverFun))
        (let* ((n      (length (first (slot-value 'data))))
               (who    (remove-duplicates (mod selected n)))  )
          (mapc #'(lambda (i) (funcall (slot-value 'recoverFun) i)) 
                who)  )
        (format t (if selected
                      "Missing recovery function.~%"
                      "None are selected.~%"))
        )))

(defmeth comparison-plot-proto :ADJUST-POINTS-IN-RECT (x y w h s)
  "Handle multiple views of same sample point."
  (if (eq s 'selected)
      (let ((p (send self :points-in-rect x y w h)))
        (if p
            (let* ((n   (length (first (slot-value 'data))))
                   (off (* n (iseq (slot-value 'nFrames))))
                   (mp  (mod p n))     ; base values
                   (all (combine (mapcar #'(lambda (i) (+ i off)) mp))))
              ;  (format t "all ~a~%" all)
              (send self :selection all)  )))  ; select in all frames
      (call-next-method x y w h s)
      ))


(defmeth comparison-plot-proto :PRINT-SUMMARY ()
  (format t "Summary of ~a.~%" (send self :title))
  (format t "                   Moments                   Percentiles~%")
  (format t "   Label     Mean    S.D.      5%       10%      50%      90%        95%~%")
  (let* ((labels (slot-value 'labels))
         (data (slot-value 'data))
         (m   (mapcar #'mean data))
         (sd  (mapcar #'standard-deviation data))
         (pct (mapcar #'(lambda (x) (quantile x '(.05 .1  .5  .90 .95)))
                      data))  )
    (unless (send self :vertical?)
            (let* ((min (slot-value 'min)) (max (slot-value 'max))
                   (scl (/ (- max min) (* .7 (slot-value 'sep))))
                   (off (slot-value 'offsets)) )
              (flet ((adjust (x) (+ min (* scl (- x off)))))
                (setf m   (adjust m))
                (setf sd  (* sd scl))
                (setf pct (adjust pct))  )))
    (mapcar #'(lambda (l m sd pct)
                (format t "~8a ~8,3g ~8,3g ~8,3g ~8,3g ~8,3g ~8,3g ~8,3g~%"
                        l m sd (first pct) (second pct) (select pct 2)
                        (select pct 3) (select pct 4)))
            labels m sd pct)
    ))


(defmeth comparison-plot-proto :DO-DRAW-DOTS ()   
  (if (slot-value 'dots?)
      (send self :clear-points)
      (send self :draw-dots))
  (send self :draw-labels)
  (setf (slot-value 'dots?) (not (slot-value 'dots?)))   )

(defmeth comparison-plot-proto :DO-DRAW-BOXPLOTS ()   
  (if (slot-value 'boxplots?)
      (progn (send self :clear-lines)
             (if (slot-value 'kernels?) (send self :draw-kernels)))
      (send self :draw-boxplots))
  (send self :draw-labels)
  (setf (slot-value 'boxplots?) (not (slot-value 'boxplots?)))   )

(defmeth comparison-plot-proto :DO-DRAW-KERNELS ()   
  (if (slot-value 'kernels?)
      (progn (send self :clear-lines)
             (if (slot-value 'boxplots?)
                        (send self :draw-boxplots)))
      (send self :draw-kernels))
  (send self :draw-labels)
  (setf (slot-value 'kernels?) (not (slot-value 'kernels?)))   )


(defmeth comparison-plot-proto :DRAW-DOTS ()   
  "Dot plot just below heights given in initial offsets."
  (mapcar #'(lambda (x v)
              (send self :add-points (list x (repeat v (length x)))))
          (slot-value 'data)
          (if (send self :vertical?)
              (- (slot-value 'offsets) 1)
              (repeat -2 (slot-value 'nFrames))  )
          ))

(defmeth comparison-plot-proto :SCATMAT ()
  (scatterplot-matrix (slot-value 'data)
                      :variable-labels (slot-value 'labels))
  )

  
;;     _____  DRAWING  _____

(defmeth comparison-plot-proto :BOX-WIDTH ()
  (floor (/ (slot-value 'sep) (if (slot-value 'kernels?) 20 7.5))))

(defmeth comparison-plot-proto :DRAW-BOXPLOTS ()
  "Horizontal boxplots just about offsets."
  (let  ((width   (send self :box-width))  )
    (mapcar #'(lambda (os v) (let ((x  (coerce os 'vector))
                                   (u  (+ v width))
                                   (d  (- v width))  )
                               (send self :add-lines (list
                                    (select x '(0 1 1 3 3 4 3 3 2 2 2 1 1))
                                    (list       v v u u v v v d d u d d v)))
                               ))
            (slot-value 'os)
            (if (send self :vertical?)
                (+ width (slot-value 'offsets))
                (repeat (/ (slot-value 'sep) 10) (slot-value 'nFrames))   )
            )))


(defmeth comparison-plot-proto :DRAW-KERNELS ()
  (let  ((width   (send self :box-width))
         (sep     (slot-value 'sep))  )
    (mapcar #'(lambda (x v)
                (let* ((ker (kernel-dens x))
                       (y   (second ker))
                       (y   (if (send self :vertical?)
                                (+ v (* y (/ sep (* 2 (max y)))))
                                (* y sep 7)))   )
                  (send self :add-lines (list (first ker) y))))
            (slot-value 'data) (slot-value 'offsets)
            )))
                
