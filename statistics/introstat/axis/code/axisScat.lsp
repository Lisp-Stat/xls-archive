;;;;
;;;;     AxisScat.lsp © Robert A. Stine
;;;;
;;;;               Density command dialog and plot object.
#||
    26 Feb 94 ... Legends and :set-symbols for coded plots.
    23 Feb 94 ... Allow external menu items on creation.
    19 Feb 94 ... Remove data filter and save data to create indicator.
    24 Jan 94 ... Export a data filter.  Add messenger.
     5 Jul 93 ... Tune up menu interactions, defaults. Print lines.
     9 Jul 92 ... Selection status and lines. Moving a point
     5 Jul 92 ... Extracted from commando.lsp
||#
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "axisScat")

;;;;

(require "axisRobR")
(require "axisUtil")  ; slider items

;;;;

#||

(setf p (plot-points (iseq 10) (uniform-rand 10)))

||#


;;
;;     REGRESSION SCATTERPLOT EXTENTIONS 
;;

(defun REGRESSION-SCATTERPLOT (x y labelx labely &key cases menuItems)
  (let ((p (send regr-scatter-proto :new x y labelx labely menuItems))   )
    (when cases
          (send p :point-label (iseq (length x)) cases)
          (send p :showing-labels t))
    p
    ))
        

(defproto regr-scatter-proto
  '(
    b0 b1        ; ols regr parms  -- also used to signal need to draw
    r0 r1        ; rob regr parms
    smthPct      ; how much to lowess
    sliderItem   ; slider menu item controlling smoother
    menuItems    ; externally supplied menu items
    )        
  '()
  scatterplot-proto
  )

(defmeth regr-scatter-proto :ISNEW (x y labelx labely menuItems)
  (if menuItems (send self :add-menu-items menuItems :new nil))
  (call-next-method 2 :title "Scatter Plot")
  (send self :add-points (list x y))
  (send self :add-mouse-mode 'move-point :title "Move point"
        :cursor 'hand :click :do-move-point)
  (send self :add-mouse-mode 'drag-point :title "Drag point"
        :cursor 'hand :click :do-drag-point)
  (setf (slot-value 'smthPct) .4)
  (send self :variable-label '(0 1) (list labelx labely))
  (send self :x-axis t nil 3)
  (send self :y-axis t nil 3)
  (send self :adjust-to-data)
  )

(defmeth regr-scatter-proto :SET-SMOOTH-PCT (sp)
  (setf (slot-value 'smthPct) sp))


(defmeth regr-scatter-proto :PRINT-LINES ()
  (if (or (slot-value 'b0) (slot-value 'r0))
       (let ((lab (send self :variable-label '(0 1)))  )
        (format t "Scatterplot regression lines...~%")
         (if (slot-value 'b0)
             (format t "  OLS  regression: ~a = ~a + ~a ~a~%"
                     (select lab 1) (slot-value 'b0) (slot-value 'b1)
                     (select lab 0))   )
         (if (slot-value 'r0)
             (format t "Robust regression: ~a = ~a + ~a ~a~%"
                     (select lab 1) (slot-value 'r0) (slot-value 'r1)
                     (select lab 0))   )
         )))
            


;;  Mouse Modes

(defmeth regr-scatter-proto :DO-MOVE-POINT (x y s o)  ; x y shift opt
  (let ((p (send self :drag-point x y)))
    (when p
          (when (slot-value 'b0) 
                (setf (slot-value 'b0) nil) ; force new line
                (send self :draw-ols-line))
          (when (slot-value 'r0)
                (setf (slot-value 'r0) nil) ; force new line
                (send self :draw-robust-line))
          (format t "Point ~d has been moved.~%" p)
          )))

(defmeth regr-scatter-proto :DO-DRAG-POINT (x y s o)
  (let ((p (first (send self :points-in-rect (- x 5) (- y 5) 10 10)))  )
    (when p
          (let ((save  (send self :point-coordinate '(0 1) p))
                (draw? (slot-value 'b0))  )
            (send self :while-button-down 
                  #'(lambda (x y)
                      (let ((xy (send self :canvas-to-real x y))  )
                        (send self :point-coordinate '(0 1) p xy)
                        (when draw? 
                              (setf (slot-value 'b0) nil) ; force new line
                              (send self :clear-lines)
                              (send self :draw-ols-line))
                        )))
            (send self :point-coordinate '(0 1) p save)
            (when draw? 
                  (setf (slot-value 'b0) nil) ; force new line
                  (send self :clear-lines)
                  (send self :draw-ols-line))
            ))))
        

(defmeth regr-scatter-proto :FIT-INDICES ()
  ;Indices of non-invisible points for fitting lines.
  (let ((all (iseq (send self :num-points)))  )
    (select all (which
                 (mapcar #'(lambda (s) (not (eq 'invisible s)))
                         (send self :point-state all))
                 ))))


(defmeth regr-scatter-proto :STATE ()                 ; 2/19/94
  (copy-list
   (send self :point-state
         (iseq (send self :num-points)))
   ))


(defmeth regr-scatter-proto :SAVE-ALIST ( )           ; 2/19/94
  ; Association list of method-selectors and names for save dialog.
  (list
   '(point-state  . :state)))


(defmeth regr-scatter-proto :ADD-MENU-ITEMS (items &key (new t))   ; 2/23/94
  (let ((items (if (listp items) items (list items)))  )
    (setf (slot-value 'menuItems)
          (if (slot-value 'menuItems)
              (append items (slot-value 'menuItems))
              (append items (list 'dash))   ))
    (when new
          (send self :new-menu "Scatter"
                (send self :menu-template)))
    ))

(defmeth regr-scatter-proto :MENU-TEMPLATE ()
  (send self :menu-title "Scatter")
  (let ((sItem (send slider-menu-item-proto :new 
                     :target self
                     :label "Lowess Slider"
                     :action #'(lambda () (send self :build-slider)))) )
    (setf (slot-value 'sliderItem) sItem)
    (append (slot-value 'menuItems)
            (call-next-method)
            (list 'dash
                  (send menu-item-proto :new "Clear lines"
                        :action #'(lambda ()
                                    (send self :clear-lines)
                                    (setf (slot-value 'b0) nil)
                                    (setf (slot-value 'r0) nil)))
                  (send menu-item-proto :new "Show OLS" 
                        :action #'(lambda ()
                                    (send self :draw-ols-line)))
                  (send menu-item-proto :new "Show Robust"
                        :action #'(lambda ()
                                    (send self :draw-robust-line)))
                  (send menu-item-proto :new "Print lines"
                        :action #'(lambda () (send self :print-lines)))
                  'dash
                  sItem
                  (send menu-item-proto :new "Show Smooth"
                        :action #'(lambda () (send self :draw-smooth)))
                  (send menu-item-proto :new "Bootstrap Smooth"
                        :action #'(lambda ()
                                    (send self :bootstrap-smooth)) )
                  ))))
  

;;  Override these two to make sure the regression lines are there

(defmeth regr-scatter-proto :ERASE-SELECTION ()      ; OVERRIDE
  (call-next-method)
  (setf (slot-value 'b0) nil)
  (setf (slot-value 'r0) nil))

(defmeth regr-scatter-proto :SHOW-ALL-POINTS ()      ; OVERRIDE
  (call-next-method)
  (setf (slot-value 'b0) nil)
  (setf (slot-value 'r0) nil))

;;  Each of the calc functions calls fit-indices to locate valid points

(defmeth regr-scatter-proto :CALC-OLS-LINE ()
  (let* ((index (send self :fit-indices))
         (x  (send self :point-coordinate 0 index))
         (y  (send self :point-coordinate 1 index))
         (mx (mean x))
         (my (mean y))
         (dx (- x mx))
         (b1 (/ (inner-product dx y) (inner-product dx dx)))
         (b0 (- my (* b1 mx)))  )
    (setf (slot-value 'b0) b0)
    (setf (slot-value 'b1) b1)
    ))

(defmeth regr-scatter-proto :DRAW-OLS-LINE ()
    (unless (slot-value 'b0)
            (send self :calc-ols-line))
    (send self :abline (slot-value 'b0) (slot-value 'b1))
    )

(defmeth regr-scatter-proto :CALC-ROBUST-LINE ()
  (let* ((index (send self :fit-indices))
         (b  (first (send robust-regression-proto :robust-ests
                          (list (send self :point-coordinate 0 index))
                          (send self :point-coordinate 1 index))))  )
    (setf (slot-value 'r0) (first b))
    (setf (slot-value 'r1) (second b))
    ))


(defmeth regr-scatter-proto :DRAW-ROBUST-LINE ()
  (unless (slot-value 'r0)
          (send self :calc-robust-line)  )
  (let ((x (send self :range 0))
        (r0 (slot-value 'r0))
        (r1 (slot-value 'r1))   )
    (send self :add-lines (list x (+ r0 (* r1 x))) :color 'red)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;     Smoothing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth regr-scatter-proto :BUILD-SLIDER ()
  (let ((slider (interval-slider-dialog (list 0 1)
                  :action #'(lambda(pct)
                              (send self :set-smooth-pct pct)
                              (send self :draw-smooth)
                              )))   )
    (send slider :add-slot 'menuitem)
    (send slider :menu-item-is (slot-value 'sliderItem))
    (send slider :value (slot-value 'smthPct))
    (send self :add-subordinate slider)
    (send (slot-value 'sliderItem) :slider-is slider)
    ))
       

(defmeth regr-scatter-proto :DRAW-SMOOTH ()
   (let* ((index (send self :fit-indices))
          (x  (send self :point-coordinate 0 index))
          (y  (send self :point-coordinate 1 index))   )
     (send self :clear-lines)
     (when (slot-value 'b0) (send self :draw-ols-line))
     (when (slot-value 'r0) (send self :draw-robust-line))
     (send self :add-lines (lowess x y :f (slot-value 'smthPct)))
     ))

(defmeth regr-scatter-proto :BOOTSTRAP-SMOOTH ()
  (let* ((bLim (get-pos-integer-dialog "How many samples?"))
         (indx (send self :fit-indices))
         (x    (send self :point-coordinate 0 indx))
         (y    (send self :point-coordinate 1 indx))
         (pct  (send self :slot-value 'smthPct))   )
    (dotimes (b bLim)
             (let* ((sample (resample indx))
                    (x*     (select x sample))
                    (y*     (select y sample))  )
               (send self :add-lines (lowess x* y* :f pct) :color 'blue)
               ))
    ))


(defmeth regr-scatter-proto :SET-SYMBOLS (codes &optional title)  ; 2/26/94
  ; Sets the point-symbols as determined by codes.
  (let* ((v 2)
         (uniq   (remove-duplicates codes))
         (nCodes (length uniq))
         (syms   '(disk cross x square diamond wedge1)) )
    (if (< (length syms) nCodes)
        (message-dialog "Too many different codes.")
        (let ((leg (send legend-proto :new 50 v
                         title
                         (mapcar #'(lambda (a b) (list a b))
                                 (select syms (iseq nCodes))
                                 (mapcar #'(lambda (x)
                                             (format nil "~a" x))
                                         uniq))))  )
          (send self :add-overlay leg)
          (send self :margin 0 (+ v (send leg :depth)) 0 0)
          (send self :point-symbol
                (iseq (length codes))
                (select syms (mapcar #'(lambda (c) (position c uniq))
                                     codes))   )
          (send self :redraw)
          ))))
                      
; (send p :set-symbols (combine (repeat 0 23) (repeat 1 23)))                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     LEGENDS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#||
  (def p *scatter*)
  (def leg (send legend-proto :new 50 5 "Title"
               (list  '(dot "One") '(cross "Two"))))
  (send p :add-overlay leg)
  (send p :margin 0 (+ 5 (1+ (send leg :depth))) 0 0)
  (send p :delete-overlay leg)
||#

(defproto LEGEND-PROTO
  '(x y       ; upper left corner of the bounding box
    aList     ; symbol.string pair assoc list
    title     ; labelling string
    width     ; computed width
    offset    ; space around symbol
    )
  ()
  graph-overlay-proto)


(defmeth legend-proto :ISNEW (x y title aList)
  (setf (slot-value 'offset) 10)
  (setf (slot-value 'x) x)
  (setf (slot-value 'y) y)
  (setf (slot-value 'title) title)
  (setf (slot-value 'aList) aList)
  )

(defmeth legend-proto :REDRAW ()
  (let* ((x (slot-value 'x))
         (y (slot-value 'y))
         (g (send self :graph))
         (tit (slot-value 'title))
         (h (+ (send g :text-ascent) (send g :text-descent)))
         (off (slot-value 'offset))
         (off/2 (floor (/ off 2)))   )
    (send g :frame-rect x y 
          (send self :width) (- (send self :depth) 1)  )
    (setf y (+ y h))
    (when tit
          (send g :draw-string tit (+ 2 x) y)
          (setf x (+ x 5 (send g :text-width tit))))
    (mapcar #'(lambda (pair)
                (send g :draw-symbol (first pair) nil (+ x off/2) (- y 3))
                (send g :draw-string  (second pair) (+ x off) y)
                (setf y (+ y h))
                )
            (slot-value 'aList))
      ))


(defmeth legend-proto :WIDTH ()
  (if (slot-value 'width)
      (slot-value 'width)
      (let ((g (send self :graph))  )
        (setf (slot-value 'width)
              (+ (floor (* 1.5 (slot-value 'offset)))
                 (if (slot-value 'title) 
                     (+ 5 (send g :text-width (slot-value 'title)))
                     0)
                 (max (mapcar #'(lambda (p)
                                  (send g :text-width (second p)))
                              (slot-value 'aList)))
                 )))))

(defmeth legend-proto :DEPTH ()
  (let ((g (send self :graph))  )
    (+ 4 (* (length (slot-value 'aList))
           (+ (send g :text-ascent) (send g :text-descent))
           ))))
