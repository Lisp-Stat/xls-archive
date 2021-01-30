;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   polish-plot.lsp
;
;AUTHOR:  David L. Daniel
;         Dept. of Experimental Statistics
;         New Mexico State University
;         ddaniel@nmsu.edu
;
;UPDATED:  5/30/96
;
;COMMENTS:
;   If you have suggestions or comments about this code, or if you
;   make useful changes to it, please let me know via email.  Also,
;   if you find this code useful, I'd appreciate your letting me know
;   that as well.  Thanks, -David
;
;
;
;OVERVIEW:
;   The primary function in the code below is "polish-plot" which
;   enhances the capabilities of the typical two dimensional plots
;   available in Xlisp-Stat.  The main objective of this code is
;   to create plots which are better suited for final output to a
;   printer.  The function polish-plot adds the following
;   attributes to two dimensional plots.
;
;ADVANTAGES:
;o  Enables log-linear, linear-log, and log-log scale plotting.
;   The logarithm base may be chosen by the user.
;
;o  A title and multiple labels may be placed inside the plot
;   window, and will be redrawn automatically when the plot
;   is redrawn.
;
;o  Function curves may be added which are represented by line
;   segments, as with the :add-function method, but whose line
;   segment end-points are equally spaced in the X direction
;   even when the X axis is log scaled.
;
;o  Formats for labeling the major tick marks can be set.
;
;o  Minor tick marks can be included on linear or log scaled
;   axes, and the number of minor ticks can be specified.
;
;o  The minimum X or Y value for a plot may be offset by a
;   specified amount from the axes' intersection so that points
;   and curves are not plotted immediately at an axis line.
;
;o  X and Y axis tick marks are labeled with horizontal strings
;   so that chunky bitmapped images are not obtained when plots
;   are sent to a printer.
;
;DISADVANTAGES (known):
;o  Brushing does not work on log scaled plots.
;   Note:  This was a conscious decision between two alternatives -
;   I chose this consequence in order to retain the ability to
;   retrieve the original data values with standard xlisp-stat
;   messages.  Brushing could be implemented if the original
;   data values were stored in a new slot, but the user would
;   have to access these values with non-standard commands.  One
;   option would be to allow the user to switch between these
;   two different modes of operation by sending the plot a
;   message.  Let me know if there is interest in this.
;
;
;Also included below are several support functions and an
;example.  Two of the support functions (fix-zeros and
;fix-non-pos) are for converting zero and non-positive values
;to small positive values.  These facilitate working with the
;log-scale plotting.  One of the support functions
;(plot-log-function) allows the user to initialize a plot by
;plotting a function with a log-scaled axis.
;

;;;;;;;;;;;here are some support functions

(defun fix-zeros (x value)

"Args:  (X VALUE)
Returns the list X with zero elements replaced by VALUE.
Nondestructive."

    (let* ((y (copy-list x))
           )
        (setf (select y (which (= y 0))) (repeat value (length (select y
(which (= y 0))))))
        y)
    )
;;;;;;;;;;

(defun fix-non-pos (x value)

"Args:  (X VALUE)
Returns the list X with non-positive elements replaced by VALUE.
Nondestructive."

    (let* ((y (copy-list x))
           )
        (setf (select y (which (<= y 0))) (repeat value (length (select y
(which (<= y 0))))))
        y)
    )
;;;;;;;;;;

(defun plot-log-function (func range &key (base 10) (axis 0) (num-points 50))

"Args:  (FUNC RANGE &key (BASE 10) (AXIS 0) (NUM-POINTS 50))
Plots a specified function FUNC over the domain range RANGE.  Plots:
     X axis on a log scale     (if AXIS=0 or 2)
     Y axis on a log scale     (if AXIS=1 or 2).
The base for the log scaling is given by BASE.  The number of points
calculated for plotting is given by NUM-POINTS, and these are chosen
so that they are equispaced along the X axis whether it is linear or
log scaled."

    (let* (

           ;;;define the plot window
           (plot (send scatterplot-proto :new 2))

           ;;;define the log function for the specified base
           (fn #'(lambda (x) (/ (log x) (log base))))

           ;;;get the range of the curve to be plotted
           (range (mapcar fn range))

           ;;;determine line segment endpoints to represent the curve
           (x (if (member axis '(0 2))
                    (^ base (rseq (first range) (second range) num-points))
                    (rseq (^ base (first range)) (^ base (second range))
num-points)
                    ))
           (y (mapcar func x))
           )

        ;;;add the line segments to the plot window
        (send plot :add-lines x y)
        (send plot :adjust-to-data)

        ;;;scale the plot and return the plot object as the functions
        ;;;return value
        (log-scale plot axis :base base)
        plot
        ))
;;;;;;;;;;


;;;here is the multi-enhancement function which "polishes" up
;;;the usual 2D scatterplots in xlisp-stat
(defun polish-plot (plot axis &key (base 10))

"Args:  (PLOT AXIS &key (BASE 10))
For a two dimensional scatterplot, PLOT, this function 'polishes' the
plot by adding more capabilities to it.  The user can transform one or
both axes to a log scale according to the following specification of
AXIS:
     X axis on a log scale     (if AXIS=0 or 2)
     Y axis on a log scale     (if AXIS=1 or 2).
The log base is given by BASE, but defaults to 10.  This function
also adds several methods to the plot object, including:
    :WINDOW-TITLE and :ADD-LABEL
        which draw titles and labels in the plot window canvas.
        These titles and labels will be automatically redrawn when
        the plot gets redrawn;
    :ADD-LOG-FUNCTION
        which adds plots of functions whose line segments are equally
        spaced the X axis, even with log scaling;
    :SET-AXIS
        which returns or sets the formatting of the axes;
    :LOG-BASE
        which returns or sets the base of the log transformation;
    :MINOR-TICKS
        which returns or sets the number of X or Y minor tick marks;
    :OFF-AXIS
        which returns or sets the distance from the axes' intersection
        that the minimum X or Y value will occur so that points are
        not plotted on top of an axis line.
This function prints Y tick mark labels horizontally to avoid
bitmapped characters.  This function may be reapplied to a plot object
multiple times in order to:
    (1)  change the log tranformation characteristics of the axes, and
    (2)  erase all labels which have been placed in the window canvas.
"

  ;;;include code to add labels to the window

  ;;;add slots to hold the label strings, locations, locations in
  ;;;the canvas
  ;;;coordinates, and justification parameters
  (send plot :add-slot 'window-labels '())
  (send plot :add-slot 'window-labels-loc '())
  (send plot :add-slot 'canvas-labels-loc '())
  (send plot :add-slot 'window-labels-just '())
;;;;;;;;;;;


  ;;;define a method for actually drawing the labels in the plot
  ;;;window
  (defmeth plot :draw-labels ()
    (let* (

           ;;;get the number of labels to be drawn
           (nwl (length (slot-value 'window-labels)))

           ;;;define the log function for the specified base
           (fn #'(lambda (x) (/ (log x) (log base))))

           ;;;set parameters indicating which axis will be log-scaled
           (xaxis (if (member axis '(0 2)) t nil))
           (yaxis (if (member axis '(1 2)) t nil))
           )

      ;;;initialize the slot value for the canvas coordinates of
      ;;;the labels
      (slot-value 'canvas-labels-loc '())

      ;;;process each label
      (dotimes (i nwl nil)
            (let* (

                   ;;;get the X and Y locations
                   (x-loc (first (nth i (slot-value 'window-labels-loc))))
                   (y-loc (second (nth i (slot-value 'window-labels-loc))))
                 
                   ;;;if the X location is NIL, set it to the middle
                   ;;;of the plot
                   (x-loc (if x-loc x-loc
                              (if xaxis (^ base (mean (send self :range 0)))
                                  (mean (send self :range 0))
                                  )))

                   ;;;if the Y location is NIL, set it to the top of
                   ;;;the plot
                   (y-loc (if y-loc y-loc
                              (if yaxis (^ base (second (send self :range 1)))
                                  (second (send self :range 1))
                                  )))

                   ;;;log transform the coordinates if appropriate
                   (x-loc (if xaxis (funcall fn x-loc) x-loc))
                   (y-loc (if yaxis (funcall fn y-loc) y-loc))
                   )

               ;;;get the "canvas" coordinates and add them to the
               ;;;slot value list of canvas coordinates
               (slot-value 'canvas-labels-loc
                           (append (slot-value 'canvas-labels-loc)
                                   (list (+ '(0 -5)
                                            (send self :real-to-canvas
                                                  x-loc y-loc)))))

               ;;;draw the text in the plot window at the specified
               ;;;location
               (send self :draw-text (nth i (slot-value 'window-labels))
                     (first (nth i (slot-value 'canvas-labels-loc)))
                     (second (nth i (slot-value 'canvas-labels-loc)))
                     (first (nth i (slot-value 'window-labels-just)))
                     (second (nth i (slot-value 'window-labels-just)))
                     )))
      nil))
;;;;;;;;;;;


  ;;;define a method to add a title to the top of the plot window
  (defmeth plot :window-title (title)
"Method args:  (TITLE)
Draws the title TITLE at the top of the canvas."
    (let* (

           ;;;set the location values to nil, indicating the top
           ;;;center of the window
           (x-loc nil)
           (y-loc nil)
           )

      ;;;add the title string, location, and justification to the
      ;;;appropriate slot values
      (slot-value 'window-labels
                  (append (slot-value 'window-labels) (list title)))
      (slot-value 'window-labels-loc
                  (append (slot-value 'window-labels-loc)
                                      (list (list x-loc y-loc))))
      (slot-value 'window-labels-just
                  (append (slot-value 'window-labels-just)
                                      (list (list 1 0))))

      ;;;redraw the plot
      (send self :redraw)
      ))
;;;;;;;;;;;


  ;;;define a method for adding a generic label to the plot window
  (defmeth plot :add-label (label x-loc y-loc
                                  &optional (h-just 0) (v-just 0))
"Method args:  (LABEL X-LOC Y-LOC &optional (H-JUST 0) (V-JUST 0))
Draws the label LABEL at the 'real-valued' coordinates (X-LOC,YLOC).
The label is justified in accord with :draw-text."

    ;;;add the label string, location, and justification to the
    ;;;appropriate slot values
    (slot-value 'window-labels
                (append (slot-value 'window-labels) (list label)))
    (slot-value 'window-labels-loc
                (append (slot-value 'window-labels-loc)
                                    (list (list x-loc y-loc))))
    (slot-value 'window-labels-just
                (append (slot-value 'window-labels-just)
                                    (list (list h-just v-just))))

    ;;;redraw the plot
    (send self :redraw)
    )
;;;;;;;;;;;


;;;;;;;;;;;here's the code to draw the log-scale axes

  ;;;define a method for setting the distance the minimum plot values
  ;;;will occur from the axes' intersection
  (defmeth plot :off-axis (axis &optional (dist nil))

"Args:  (AXIS &optional (DIST))
Changes the distance from the axes' intersection that the minimum
X or Y value occurs, so that points are not plotted on top of an axis
line.  AXIS is 0 for X axis and 1 for Y axis.  DIST is the distance
of the offset and is specified as a fraction of the distance between
major tick marks.  For example DIST=0.5 indicates half the distance
between major tick marks.  If DIST is not supplied, the offset value
is just returned."

    ;;;
    (when dist
          (cond
             ((= axis 0) (setf (slot-value 'x-offset) dist))
             ((= axis 1) (setf (slot-value 'y-offset) dist))
             )
          (send self :redraw)
          )
    (cond
       ((= axis 0) (slot-value 'x-offset))
       ((= axis 1) (slot-value 'y-offset))
       (t nil)
       )
    )
;;;;;;;;;;;


  ;;;define a method for setting the range and number of major ticks
  (defmeth plot :set-axis (axis &optional (min nil) (max nil) (ticks nil)
(format nil))

"Args:  (AXIS &optional (MIN nil) (MAX nil) (TICKS nil) (FORMAT nil))
Changes the axis tick mark parameters.  AXIS is 0 for x axis and 1 for
y axis.  Any values which are not to be changed should be specified as
NIL.  FORMAT controls the formatting of the tick mark labels and
is a string which would be appropriate for the FORMAT command."

    ;;;format the X axis
    (when (= axis 0)

          ;;;get the min, max, number of ticks, and tick
          ;;;label formatting
          (let* ((min (if min min (first (slot-value 'x-axis))))
                 (max (if max max (second (slot-value 'x-axis))))
                 (ticks (if ticks ticks (third (slot-value 'x-axis))))
                 (format (if format format (fourth (slot-value 'x-axis)))) )

              ;;;put the axis formatting into the slot value
              (setf (slot-value 'x-axis) (list min max ticks format)))
          )

    ;;;format the Y axis
    (when (= axis 1)

          ;;;get the min, max, number of ticks, and tick
          ;;;label formatting
          (let* ((min (if min min (first (slot-value 'y-axis))))
                 (max (if max max (second (slot-value 'y-axis))))
                 (ticks (if ticks ticks (third (slot-value 'y-axis))))
                 (format (if format format (fourth (slot-value 'y-axis)))) )

              ;;;put the axis formatting into the slot value
              (setf (slot-value 'y-axis) (list min max ticks format)))

          )

    ;;;if changes were made, then redraw the plot
    (when (or min max ticks format) (send self :redraw))

    ;;;return the axis formatting
    (cond
        ((= axis 0) (slot-value 'x-axis))
        ((= axis 1) (slot-value 'y-axis))
        (t nil)
        )
    )
;;;;;;;;;;;


  ;;;define a method for redrawing the plot BACKGROUND
  (defmeth plot :redraw-background ()
    (let* (

           ;;;get the axis defaults
           (xax (send self :x-axis))
           (yax (send self :y-axis))

           ;;;define the log function for the specified base
           (fn #'(lambda (x) (/ (log x) (log base))))

           ;;;get the number of points and lines in the plot
           (npt (send self :num-points))
           (nln (send self :num-lines))

           ;;;set parameters indicating which axis will be log-scaled
           (xaxis (if (member axis '(0 2)) t nil))
           (yaxis (if (member axis '(1 2)) t nil))

           ;;;get the extreme coordinate values
           (xpt (if (> npt 0) (send self :point-coordinate 0 (iseq npt)) nil))
           (xln (if (> nln 0) (send self :linestart-coordinate 0 (iseq
nln)) nil))
           (ypt (if (> npt 0) (send self :point-coordinate 1 (iseq npt)) nil))
           (yln (if (> nln 0) (send self :linestart-coordinate 1 (iseq
nln)) nil))
           (xmin (min (append xpt xln)))
           (xmax (max (append xpt xln)))
           (ymin (min (append ypt yln)))
           (ymax (max (append ypt yln)))
           (dummy (unless (slot-value 'x-axis)
                          (setf (slot-value 'x-axis)
                                (if xaxis
                                    (list xmin xmax (- xmax xmin -1) "~,3g")
                                    (append (get-nice-range xmin xmax 4)
(list "~,3g"))
                                    ))))
           (dummy (unless (slot-value 'y-axis)
                          (setf (slot-value 'y-axis)
                                (if yaxis
                                    (list ymin ymax (- ymax ymin -1) "~,3g")
                                    (append (get-nice-range ymin ymax 4)
(list "~,3g"))
                                    ))))

           ;;;get the extreme coordinate values in the plot's
           ;;;coordinate system
           (xklow (if xaxis
                        (floor (/ (log (first (slot-value 'x-axis))) (log
base)))
                        (first (slot-value 'x-axis))))
           (xkhigh (if xaxis
                        (ceiling (/ (log (second (slot-value 'x-axis)))
(log base)))
                        (second (slot-value 'x-axis))))
           (yklow (if yaxis
                        (floor (/ (log (first (slot-value 'y-axis))) (log
base)))
                        (first (slot-value 'y-axis))))
           (ykhigh (if yaxis
                        (ceiling (/ (log (second (slot-value 'x-axis)))
(log base)))
                        (second (slot-value 'y-axis))))

           ;;;set the final "pretty" extreme coordinate values
           (xmin (if xaxis (^ base xklow) xklow))
           (xmax (if xaxis (^ base xkhigh) xkhigh))
           (ymin (if yaxis (^ base yklow) yklow))
           (ymax (if yaxis (^ base ykhigh) ykhigh))

           ;;;determine what the major tick values will be and get
           ;;;text strings for them
           (xmajor-ticks (if xaxis (iseq xklow xkhigh)
                                   (rseq xmin xmax (third (slot-value
'x-axis)))))
           (ymajor-ticks (if yaxis (iseq yklow ykhigh)
                                   (rseq ymin ymax (third (slot-value
'y-axis)))))
           (xmajor-values (mapcar #'(lambda (x) (format nil (fourth
(slot-value 'x-axis))
                                                (if xaxis (^ base x) x)))
xmajor-ticks))
           (ymajor-values (mapcar #'(lambda (y) (format nil (fourth
(slot-value 'y-axis))
                                                (if yaxis (^ base y) y)))
ymajor-ticks))

           ;;;set the left margin to accomodate the Y tick labels
           (dummy (send self :margin
                    (+ (max (mapcar #'(lambda (x) (send self :text-width
x)) ymajor-values)) 0) 0 0 0 :draw nil))

           ;;;get the scaling values for X and Y
           ;;;XDIFF is in:  (trasformed units) / (major tick)
           ;;;XSCALE & YSCALE are in:  (pixels) / (trasformed units)
           ;;;where "transformed units" are the final units for an
           ;;;axis after any log tranformation which may have been
           ;;;requested (but could be untransformed)
           (xdiff (/ (- (first (reverse xmajor-ticks)) (first
xmajor-ticks)) (- (length xmajor-ticks) 1)))
           (ydiff (/ (- (first (reverse ymajor-ticks)) (first
ymajor-ticks)) (- (length ymajor-ticks) 1)))

           ;;;reset the low plot coordinate values to account for
           ;;;the axis offsets
           (xklow (- xklow (* (slot-value 'x-offset) xdiff)))
           (yklow (- yklow (* (slot-value 'y-offset) ydiff)))

           ;;;initialize the scaled axes
           (dummy (send self :range 0 xklow xkhigh :draw nil))
           (dummy (send self :range 1 yklow ykhigh :draw nil))
           (dummy (send self :x-axis (first xax) (second xax) 0 :draw nil))
           (dummy (send self :y-axis (first yax) (second yax) 0 :draw nil))

           (scale (/ (- (send self :real-to-canvas xkhigh ykhigh)
                        (send self :real-to-canvas xklow yklow)
                        )
                     (- (list xkhigh ykhigh) (list xklow yklow))))
           (xscale (first scale))
           (yscale (second scale))
           )

        ;;;draw the usual background features
        (call-next-method)

        ;;;draw the X tick marks
        (let* (

               ;;;get the needed extremes of the axis
               (minx (first (send self :real-to-canvas (+ xklow (* (slot-value 'x-offset) xdiff)) 0)))
               (maxx (first (send self :real-to-canvas xkhigh 0)))
               (miny (+ (second (send self :real-to-canvas 0 (first (send
self :range 1)))) 1))

               ;;;determine the distance between each minor tick
               (dist (if xaxis (/ (- (* base base) base) (slot-value
'xminor-ticks))
                               0))
               (minors (if xaxis
                           (cumsum (- (funcall fn (+ base (* dist (iseq 1
(slot-value 'xminor-ticks)))))
                                  (funcall fn (+ base (* dist (iseq
(slot-value 'xminor-ticks)))))
                                    ))
                           (cdr (reverse (cdr (reverse (rseq 0 xdiff (+
(slot-value 'xminor-ticks) 2))))))
                           ))
               )

            ;;;draw the major ticks
            (dotimes (i (length xmajor-values) nil)
                (send self :draw-line (+ minx (round (* i xscale xdiff)))
miny (+ minx (round (* i xscale xdiff))) (+ miny 5))
                (send self :draw-text (nth i xmajor-values) (+ minx (round
(* i xscale xdiff))) (+ miny 12) 1 1)
                )

            ;;;draw the minor ticks
            (dotimes (i (- (length xmajor-values) 1) nil)
                (dolist (j (iseq (slot-value 'xminor-ticks)) nil)
                    (send self :draw-line (+ minx (round (* (+ (* i xdiff)
(nth j minors)) xscale))) miny
                                          (+ minx (round (* (+ (* i xdiff)
(nth j minors)) xscale))) (+ miny 3))
                    )
                )
            )

        ;;;draw the Y tick marks
        (let* (

               ;;;get the needed extremes of the axis
               (miny (second (send self :real-to-canvas 0 (+ yklow (*
(slot-value 'y-offset) ydiff)))))
               (maxy (second (send self :real-to-canvas 0 ykhigh)))
               (minx (- (first (send self :real-to-canvas (first (send self
:range 0)) 0)) 1))

               ;;;determine the distance between each minor tick
               (dist (if yaxis (/ (- (* base base) base) (slot-value
'yminor-ticks))
                               0))
               (minors (if yaxis
                           (cumsum (- (funcall fn (+ base (* dist (iseq 1
(slot-value 'yminor-ticks)))))
                                      (funcall fn (+ base (* dist (iseq
(slot-value 'yminor-ticks)))))
                                      ))
                           (cdr (reverse (cdr (reverse (rseq 0 ydiff (+
(slot-value 'yminor-ticks) 2))))))
                           ))
               )

            ;;;draw the major ticks
            (dotimes (i (length ymajor-values) nil)
                (send self :draw-line minx (+ miny (round (* i yscale
ydiff))) (- minx 5) (+ miny (round (* i yscale ydiff))))
                (send self :draw-text (nth i ymajor-values) (- minx 8) (+
miny (round (* i yscale ydiff)) -5) 2 1)
                )

            ;;;draw the minor ticks
            (dotimes (i (- (length ymajor-values) 1) nil)
                (dolist (j (iseq (slot-value 'yminor-ticks)) nil)
                    (send self :draw-line minx (+ miny (round (* (+ (* i
ydiff) (nth j minors)) yscale)))
                                          (- minx 3) (+ miny (round (* (+
(* i ydiff) (nth j minors)) yscale))))
                    )
                )
            )
    nil))
;;;;;;;;;;;


  ;;;define a method for redrawing the plot CONTENT
  (defmeth plot :redraw-content ()
    (let* (

           ;;;define the log function for the specified base
           (fn #'(lambda (x) (/ (log x) (log base))))

           ;;;get the number of points and lines in the plot
           (npt (send self :num-points))
           (nln (send self :num-lines))

           ;;;set parameters indicating which axis will be log-scaled
           (xaxis (if (member axis '(0 2)) t nil))
           (yaxis (if (member axis '(1 2)) t nil))

           ;;;save the original coordinates
           (xpt (if (> npt 0) (send self :point-coordinate 0 (iseq npt)) nil))
           (xln (if (> nln 0) (send self :linestart-coordinate 0 (iseq
nln)) nil))
           (ypt (if (> npt 0) (send self :point-coordinate 1 (iseq npt)) nil))
           (yln (if (> nln 0) (send self :linestart-coordinate 1 (iseq
nln)) nil))

           ;;;initialize a local storage variable
           (temp '())
           )

        ;;;set the coordinates to the transformed values
        (when (and (> npt 0) xaxis) (send self :point-coordinate 0 (iseq
npt) (funcall fn xpt) :redraw nil))
        (when (and (> npt 0) yaxis) (send self :point-coordinate 1 (iseq
npt) (funcall fn ypt) :redraw nil))
        (when (and (> nln 0) xaxis) (send self :linestart-coordinate 0
(iseq nln) (funcall fn xln) :redraw nil))
        (when (and (> nln 0) yaxis) (send self :linestart-coordinate 1
(iseq nln) (funcall fn yln) :redraw nil))

        ;;;draw the points and lines in the content area
        (call-next-method)

        ;;;draw any window labels (scale and reset stored label
        ;;;locations)
        (send self :draw-labels)

        ;;;restore the original coordinates
        (when (and (> npt 0) xaxis) (send self :point-coordinate 0 (iseq
npt) xpt :redraw nil))
        (when (and (> npt 0) yaxis) (send self :point-coordinate 1 (iseq
npt) ypt :redraw nil))
        (when (and (> nln 0) xaxis) (send self :linestart-coordinate 0
(iseq nln) xln :redraw nil))
        (when (and (> nln 0) yaxis) (send self :linestart-coordinate 1
(iseq nln) yln :redraw nil))
        )
    nil)
;;;;;;;;;;;


    ;;;define a method for adding a function to a log-scaled plot
    (defmeth plot :add-log-function (func &key (range nil) (num-points 50))
        (let* (

               ;;;get the log base and define the corresponding
               ;;;transformation function
               (base (slot-value 'base))
               (fn #'(lambda (x) (/ (log x) (log base))))

               ;;;get the range of the curve in transformed (log)
               ;;;coordinates
               (range (if range range (^ base (send self :range 0))))
               (range (mapcar fn range))

               ;;;determine X values which will be equally spaced
               ;;;in the transformed scale
               (x (^ base (rseq (first range) (second range) num-points)))

               ;;;get the function values at the determined X values
               (y (mapcar func x))
               )

            ;;;add the line segments representing the curve to
            ;;;the plot
            (send self :add-lines x y)

            ;;;remove any previous axis settings and redraw the plot
            (setf (slot-value 'x-axis) nil)
            (setf (slot-value 'y-axis) nil)
            (send plot :redraw)
            ))
;;;;;;;;;;;


    ;;;define a method for adding a function to a log-scaled plot
    (defmeth plot :log-base (&optional (base nil))
        (when base (slot-value 'base base)
                   (send self :redraw))
        (slot-value 'base)
        )
;;;;;;;;;;;


    ;;;define a method for changing the number of minor tick marks
    (defmeth plot :minor-ticks (axis &optional (ticks nil))

"Args:  (AXIS &optional (TICKS))
Returns the number of minor tick marks used for the X axis (if AXIS=0)
or the Y axis (if AXIS=1).  If TICKS is supplied, then minor tick
marks is set to TICKS.  Note that for log-scaled axes TICKS represents
the number of minor *partitions*, whereas for linear scaled axes TICKS
represents the number of minor *tick marks*."

        (when ticks
              (when (= axis 0)
                    (slot-value 'xminor-ticks ticks)
                    (send self :redraw)
                    )
              (when (= axis 1)
                    (slot-value 'yminor-ticks ticks)
                    (send self :redraw)
                    )
              )
        (cond ((= axis 0) (slot-value 'xminor-ticks))
              ((= axis 1) (slot-value 'yminor-ticks))
              (t nil))
        )
;;;;;;;;;;;


    ;;;initialize some new slots for the log-scaling functions
    ;;;and methods

    ;;;slots for the number of minor ticks
    (send plot :add-slot 'xminor-ticks (if (member axis '(0 2)) (- base 1) 0))
    (send plot :add-slot 'yminor-ticks (if (member axis '(1 2)) (- base 1) 0))

    ;;;slots for the axis settings (min, max, number of ticks,
    ;;;and tick label format)
    (send plot :add-slot 'x-axis nil)
    (send plot :add-slot 'y-axis nil)

    ;;;slots for the offsets for the minimum X and Y values
    (send plot :add-slot 'x-offset 0)
    (send plot :add-slot 'y-offset 0)

    ;;;slot for the base of the log function to be used
    (send plot :add-slot 'base base)

    ;;;Redraw the plot
    (send plot :redraw)
    )
;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;
;Here is an example.
;It may be instructive to execute some of the lines
;one at a time to see the intermediate results.


(def concentration '(0 0 0 0 0 0 1 1 1 1 1 1 2 2 2 2 2 2
                     4 4 4 4 4 4 8 8 8 8 8 8 16 16 16 16 16 16))
(def height '(22.27 9.18 20.4 15.13 15.3 16.66 21.25 20.23 20.23
              16.66 18.36 19.72 15.98 9.35 14.45 18.7 15.3 11.9
              6.12 5.44 7.99 8.67 11.22 13.94 9.52 7.82 8.5 11.73
              9.35 7.65 4.08 8.16 9.18 5.27 6.46 4.93))

(defun nlinx (x)
"Args:  (X)
Returns the value of the nonlinear dose function described by
Brain and Cousens (1989):
                    D - C + Fx
        y = -------------------------------- + C
            1 + exp{ B [log(x) + log(i50)] }

It assumes the parameter list, PARAMS, (containing B, C, D, F, and i50)
has already been defined globally."
(let* ((num (- (+ (third params) (* (fourth params) x)) (second params)))
       (den (1+ (exp (* (first params) (log (/ x (fifth params)))))))
       )
      (+ (/ num den) (second params))))

(def params '(1.80129687 6.20440300 16.48982920 26.99715679 0.58941316))

(def pp (plot-points (fix-zeros (copy-list concentration) 1e-3) height))
(send pp :x-axis t t 5)
(send pp :variable-label 0 "Concentration Level")
(polish-plot pp 0)
(send pp :window-title "Plant Height vs. Fertilizer Concentration")
(send pp :add-log-function #'nlinx :range '(1e-3 16))
(send pp :off-axis 0 .2)
(send pp :off-axis 1 .2)
(send pp :size 400 300)
(send pp :minor-ticks 1 4)
;;;;;;;;;;;






