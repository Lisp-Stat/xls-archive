;;;;
;;;;     Labelled  Plots © Robert A. Stine
;;;;
;;;;     15 Jan 94 ... Defun function added.      
;;;;      6 Jan 93 ... Extracted from Simulator compare.
;;;;
;;;; Additions to
;;;; XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney
;;;; Xlisp 2.0 Copyright (c) 1985, 1987 by David Michael Betz
;;;;    All Rights Reserved
;;;;    Permission is granted for unrestricted non-commercial use

(provide "labelPlt")

;;;;

(defun labelled-plot (x y label)
  (let ((p (send labelled-plot-proto :new 2))   )
    (send p :add-points (list x y))
    (send p :x-axis t t 5)
    (send p :y-axis t t 5)
    (send p :adjust-to-data)
    (send p :main-label-is label)
    p))

;;;;

(defproto labelled-plot-proto
  '(
    mainLabel  ; top of plot "title" (in the view, not the border)
    subLabel   ; a subtitle string
    labels     ; strings for additional labels
    pts        ; position in data coordinates
    )
  ()
  (list graph-proto))

(defmeth labelled-plot-proto :TITLE-IS (title)
  (send self :main-label-is title)
  (send self :title title))

(defmeth labelled-plot-proto :MAIN-LABEL-IS (main)
  (setf (slot-value 'mainLabel) main))

(defmeth labelled-plot-proto :SUB-LABEL-IS (sub)
  (setf (slot-value 'subLabel) sub))

(defmeth labelled-plot-proto :ADD-LABELS (labels pts)
  (setf (slot-value 'labels) labels)
  (setf (slot-value 'pts) pts))

(defmeth labelled-plot-proto :DRAW-LABELS ()
  (let ((mid (floor (/ (first (send self :size)) 2))))
    (if (slot-value 'mainLabel)                       ; center above point
        (send self :draw-text (slot-value 'mainLabel)  mid 10 1 0)) 
    (if (slot-value 'subLabel)
        (send self :draw-text (slot-value 'subLabel) mid 25 1 0))  )
  (if (slot-value 'labels)
      (mapcar #'(lambda (l c) 
                  (let ((pt (send self :scaled-to-canvas
                                  (first c) (second c))))
                    (send self :draw-string l (first pt) (second pt))  ))
              (slot-value 'labels) (slot-value 'pts))
      ))

(defmeth labelled-plot-proto :REDRAW ()
  (call-next-method)
  (send self :draw-labels))

                     
#||

