;;;
;;; @(#)$Header$
;;;
;;; Copyright (C) 1994 B. Narasimhan, naras@euler.bd.psu.edu
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

(require "utils")
(require "button")
(provide "distproto")
(defparameter *min-probability* 1e-7)
(defparameter *max-probability* (- 1 1e-7))
(defparameter *num-points* 50)
(defparameter *shade-color* 
  (if (screen-has-color)
     'magenta
    'black))
(defparameter *quantile-print-format* "~,3f")
(defparameter *probability-print-format* "~,3f")
(defproto dist-plot-proto 
          '(dens cdf icdf params l-point r-point shade-color
                 num-points important-abscissae
                 params-print-format params-display-loc
                 answer answer-display-loc 
                 min-probability max-probability)
           () scatterplot-proto
"The distribution plot prototype. The slots dens, cdf, and icdf
respectively hold the function that calculate the density, 
cumulative distribution and inverse of the cumulative distribution
functions. The slot params holds the parameters for the 
distribution. L-point and r-point hold values between which the
plot must be shaded under the density with shade-color. Num-points
indicates how many points must be used to plot the density. The slot
important-abscissae is a list of points that need to be highlighted.
The strings params-print-format and answer indicate how
the parameters and answers should be printed on the plot while the
loc-slots hold the location on the plot where they should be 
printed. Min-probability and max-probability indicate the
practical support of the density.")
(defmeth dist-plot-proto :params (&optional params)
  "Method args: (&optional params)
Sets or retrieves the parameters for the distribution."
  (if params
      (setf (slot-value 'params) params)
    (slot-value 'params)))
(defmeth dist-plot-proto :answer (&optional str)
  "Method args: (&optional str)
Sets or retrieves the answer string."
  (if str
      (setf (slot-value 'answer) str)
    (slot-value 'answer)))
(defmeth dist-plot-proto :answer-display-loc (&optional loc)
  "Method args: (&optional loc)
Sets or retrieves the answer-display-loc slot."
  (if loc
     (setf (slot-value 'answer-display-loc) loc)
  (slot-value 'answer-display-loc)))
(defmeth dist-plot-proto :params-print-format (&optional str)
  "Method args: (&optional str)
Sets or retrieves the parameter-print-format string."
  (if str
      (setf (slot-value 'params-print-format) str)
    (slot-value 'params-print-format)))
(defmeth dist-plot-proto :params-display-loc (&optional loc)
  "Method args: (&optional loc)
Sets or retrieves the params-display-loc slot."
  (if loc
     (setf (slot-value 'params-display-loc) loc)
  (slot-value 'params-display-loc)))
(defmeth dist-plot-proto :dens (&optional dens)
  "Method args: (&optional dens)
Sets or retrieves the density."
  (if dens
      (setf (slot-value 'dens) dens)
    (slot-value 'dens)))
(defmeth dist-plot-proto :cdf (&optional cdf)
  "Method args: (&optional cdf)
Sets or retrieves the CDF."
  (if cdf
      (setf (slot-value 'cdf) cdf)
    (slot-value 'cdf)))
(defmeth dist-plot-proto :icdf (&optional icdf)
  "Method args: (&optional icdf)
Sets or retrieves the Inverse CDF."
  (if icdf
      (setf (slot-value 'icdf) icdf)
    (slot-value 'icdf)))
(defmeth dist-plot-proto :num-points (&optional num-points)
  "Method args: (&optional num-points)
Sets or retrieves the slot num-points."
  (if num-points
      (setf (slot-value 'num-points) num-points)
    (slot-value 'num-points)))
(defmeth dist-plot-proto :important-abscissae (&optional list)
  "Method args: (&optional list)
Sets or retrieves the list of important abscissae."
  (if list
      (setf (slot-value 'important-abscissae) list)
    (slot-value 'important-abscissae)))
(defmeth dist-plot-proto :shade-color (&optional color)
  "Method args: (&optional color)
Sets or retrieves the shading color."
  (if color
      (setf (slot-value 'shade-color) color)
    (slot-value 'shade-color)))
(defmeth dist-plot-proto :l-point (&optional (point nil supplied-p))
  "Method args: (&optional (point nil supplied-p))
Sets or retrieves the slot l-point."
  (if supplied-p
      (setf (slot-value 'l-point) point)
    (slot-value 'l-point)))
(defmeth dist-plot-proto :r-point (&optional (point nil supplied-p))
  "Method args: (&optional (point nil supplied-p))
Sets or retrieves the slot r-point."
  (if supplied-p
      (setf (slot-value 'r-point) point)
    (slot-value 'r-point)))
(defmeth dist-plot-proto :min-probability (&optional val) 
  "Method args: (&optional val) 
Sets or retrieves the minimum probability for which quantiles 
can be calculated."  
  (if val 
      (setf (slot-value 'min-probability) val)
    (slot-value 'min-probability)))
(defmeth dist-plot-proto :max-probability (&optional val) 
  "Method args: (&optional val) 
Sets or retrieves the maximum probability for which quantiles 
can be calculated."  
  (if val
      (setf (slot-value 'max-probability) val)
    (slot-value 'max-probability)))
(defmeth dist-plot-proto :xmin ()
  "Method args: None
Returns the minimum value of x used in plotting."
  (apply (slot-value 'icdf) (send self :min-probability) 
         (send self :params)))
(defmeth dist-plot-proto :xmax ()
  "Method args: None
Returns the maximum value of x used in plotting."
  (apply (slot-value 'icdf) (send self :max-probability) 
         (send self :params)))
(defmeth dist-plot-proto :dens-at (x)
  "Method args: x 
Returns the value of the density at x."
  (apply (slot-value 'dens) x (send self :params)))
(defmeth dist-plot-proto :cdf-at (x)
  "Method args: x 
Returns the value of the cdf at x."
  (apply (slot-value 'cdf) x (send self :params)))
(defmeth dist-plot-proto :icdf-at (x)
  "Method args: x 
Returns the value of the icdf at x."
  (apply (slot-value 'icdf) x (send self :params)))
(defmeth dist-plot-proto :display-params ()
  "Method args: (None)
Displays the parameters on the plot."
   (when (send self :params-display-loc)
      (let ((str (apply #'format nil (send self :params-print-format)
                        (send self :params))))
        (apply #'send self :draw-string str 
               (send self :params-display-loc)))))
(defmeth dist-plot-proto :display-answer ()
  "Method args: (None)
Displays the answer centered horizontally on the plot."
  (let ((y (send self :answer-display-loc))
        (answer (send self :answer)))
     (when answer
        (let ((x (round (* 0.5 (- (send self :canvas-width)
                                  (send self :text-width answer))))))
           (send self :draw-string answer x y)))))
(defmeth dist-plot-proto :draw-vert-arrow (a b c)
  "Method args: (a b c)
Draws a vertical arrow from (a b) ending at (a c). The arrow head will
be at (a c). The coordinates must be canvas coordinates."
  (send self :draw-line a b a c)
  (let* ((p (+ b (round (* 0.8 (- c b)))))
         (x1 (- a 10))
         (x2 (+ a 10)))
    (send self :paint-poly (list (list x1 p) (list x2 p) (list a c)))))
(defmeth dist-plot-proto :highlight-important-abscissae ()
  "Method args: None
Draws a vertical arrows highlighting the abscissae in the slot
important-abscissae." 
  (when (send self :important-abscissae)
        (let* ((list (send self :important-abscissae))
               (ascent (send self :text-ascent))
               (descent (send self :text-descent))
               (ht (send self :canvas-height))
               (arrow-start-y (- ht (round (* 1.5 (+ ascent descent)))))
               (str-start-y (- ht (round (* 0.35 (+ ascent descent))))))
           (dolist (val list)
             (let* ((coord (send self :real-to-canvas val 0.0))
                    (x (select coord 0))
                    (y (select coord 1))
                    (str (format nil *quantile-print-format* val))
                    (str-wid (send self :text-width str)))
               (send self :draw-string str 
                 (- x (round (* 0.5 str-wid))) str-start-y)
               (send self :draw-vert-arrow x arrow-start-y y))))))
(defmeth dist-plot-proto :shade-under-plot ()
  "Shades the region under the curve determined by l-point and r-point.
If both are non-nil, shades between, otherwise to the left or right
as the case may be. Does nothing if both l-point, r-point are
nil. Note that a must be < b if both are non-nil."
  (when (or (send self :l-point) (send self :r-point))
     (let ((x (mapcar #'(lambda(x) (send self :linestart-coordinate 0 x))
                      (iseq (send self :num-lines))))
           (y (mapcar #'(lambda(x) (send self :linestart-coordinate 1 x))
                        (iseq (send self :num-lines))))
           (a (send self :l-point))
           (b (send self :r-point)))
        (cond
         ((and a b) ; We need to shade between.
          (let ((m+1 (position a x :test #'<=))
                (n (position b x :test #'>= :from-end t)))
            (unless (and m+1 n (>= n m+1))
                 (error "Bad left and right end points."))
            (let ((v-list (list
                           (send self :real-to-canvas a 0)
                           (send self :real-to-canvas a 
                                 (send self :dens-at a))))
                  (middle 
                   (select (mapcar #'(lambda(x y)
                            (send self :real-to-canvas x y)) x y)
                           (iseq m+1 n)))
                  (end (list
                        (send self :real-to-canvas b 
                              (send self :dens-at b))
                        (send self :real-to-canvas b 0)))
                  (dc (send self :draw-color)))
              (setf v-list (append v-list middle))
              (setf v-list (append v-list end))
              (send self :draw-color (send self :shade-color))
              (send self :paint-poly v-list)
              (send self :draw-color dc))))
         (a ; We need to shade to the left.
          (let ((m+1 (position a x :test #'>= :from-end t)))
            (when m+1
              (let ((v-list 
                     (list 
                       (send self :real-to-canvas (select x 0) 0.0)))
                    (middle 
                     (select (mapcar #'(lambda(x y)
                               (send self :real-to-canvas x y)) x y)
                       (iseq m+1)))
                     (end (list
                           (send self :real-to-canvas a 
                                 (send self :dens-at a))
                           (send self :real-to-canvas a 0.0)))
                    (dc (send self :draw-color)))
                (setf v-list (append v-list middle))
                (setf v-list (append v-list end))
                (send self :draw-color (send self :shade-color))
                (send self :paint-poly v-list)
                (send self :draw-color dc)))))
         (b ; We need to shade to the right.
          (let ((n (position b x :test #'<)))
            (when n
              (let ((v-list 
                     (list
                       (send self :real-to-canvas b 0)
                       (send self :real-to-canvas b (send self
                       :dens-at b))))
                    (end
                     (select (mapcar #'(lambda(x y)
                                (send self :real-to-canvas x y)) x y)
                             (iseq n (1- (send self :num-lines)))))
                    (dc (send self :draw-color)))
                (setf v-list (append v-list end))
                (send self :draw-color (send self :shade-color))
                (send self :paint-poly v-list)
                (send self :draw-color dc)))))))))
(defmeth dist-plot-proto :isnew (&key dens cdf icdf params 
         prob-dialog quant-dialog
         (shade-color *shade-color*)
         (params-print-format "Parameters: ~g")
         (num-points *num-points*)
         (max-probability *max-probability*)
         (min-probability *min-probability*)
         (title "Probability Density"))
  (setf (slot-value 'dens) dens)
  (setf (slot-value 'cdf) cdf)
  (setf (slot-value 'icdf) icdf)
  (setf (slot-value 'shade-color) shade-color)
  (setf (slot-value 'params) params)
  (setf (slot-value 'params-print-format) params-print-format)
  (setf (slot-value 'num-points) num-points)
  (setf (slot-value 'min-probability) min-probability)
  (setf (slot-value 'max-probability) max-probability)
  (call-next-method 2 :title title)
  (when (screen-has-color) (send self :use-color t))
  (let* ((em (send self :text-width "m"))
         (ascent (send self :text-ascent))
         (descent (send self :text-descent))
         (prob (send button-overlay-proto :new em ascent
                     "Find Probability" 
                     #'(lambda() (funcall prob-dialog self))))
         (cw (send self :canvas-width))
         (qtw (send self :text-width "Find Quantile"))
         (quant (send button-overlay-proto :new (- cw em qtw em) ascent
                      "Find Quantile"
                       #'(lambda() (funcall quant-dialog self)))))
    (let* ((sz (send self :size))
           (ht (select sz 1))
           (top-margin (round (+ ascent ascent descent em
                                 (* 1.5 (+ ascent descent))
                                 (* 1.5 (+ ascent descent)))))
           (bot-margin (round (* 1.5 (+ ascent descent)))))
       (send self :size (select sz 0) (+ ht top-margin bot-margin))
       (send self :margin 0 top-margin 0 bot-margin))
    (send self :add-overlay prob)
    (send self :add-overlay quant)
    (send self :params-display-loc 
          (list em (+ ascent ascent descent em ascent descent)))
    (setf (slot-value 'answer-display-loc) 
          (+ ascent ascent descent em 
             (round (* 1.5 (+ ascent descent)))
             ascent descent))))
(defmeth dist-plot-proto :redraw-background ()
  (call-next-method)
  (send self :display-params)
  (send self :display-answer))
(defmeth dist-plot-proto :redraw-content ()
  "Method args: none
Redraws the content of the plot and the background."
  (call-next-method)
  (send self :clear-lines :draw nil)
  (let* ((x (rseq (send self :xmin) (send self :xmax)
                  (send self :num-points)))
         (y (mapcar #'(lambda(w) (send self :dens-at w)) x)))
    (send self :add-lines x y :draw nil))
  (send self :adjust-to-data :draw nil)
  (send self :shade-under-plot)
  (send self :highlight-important-abscissae))
