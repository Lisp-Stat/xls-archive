(provide "kde-ovls")

(defproto kde-overlay-proto '(location title message show) () 
  graph-overlay-proto 
  "The overlay prototype. Location is where a box would be located on
the graph and should be a list of 4 integers, x, y, width and height.
Title is a string that will be drawn next to the box.  Message is
the message that will be sent to the plot when the box is clicked.
Show is t or nil, according to whether the overlay is seen or not.")

(defmeth kde-overlay-proto :isnew (title location message)
  "Method args: title location message
Title is a string, location is a list of 4 integers, and message is
the message that needs to be sent to the graph window when clicked."
  (setf (slot-value 'location) location)
  (setf (slot-value 'title) title)
  (setf (slot-value 'message) message)
  (setf (slot-value 'show) t)
  (call-next-method))

(defmeth kde-overlay-proto :location (&optional loc)
  "Method args: &optional loc
Accessor method for the slot 'location."
  (if loc
      (setf (slot-value 'location) loc)
    (slot-value 'location)))

(defmeth kde-overlay-proto :toggle-show ()
  "Method args: none.
Toggles whether the overlay shows or not."
  (setf (slot-value 'show) (not (slot-value 'show)))
  (send (send self :graph) :redraw))

(defmeth kde-overlay-proto :redraw ()
  "Method args: none.
This method redraws the overlay."
  (when (slot-value 'show)
	(let* ((loc (send self :location))
	       (x (select loc 0))
	       (y (select loc 1))
	       (box (select loc 2))
	       (string-x (select loc 3))
	       (graph (send self :graph)))
	  (send graph :frame-rect x (- y box) box box)
	  (send graph :draw-string (slot-value 'title) string-x y))))

(defmeth kde-overlay-proto :do-click (x y m1 m2)
  "Method args: x y m1 m2
This method checks whether the mouse was clicked in the overlay box.
If so, it sends the graph the message located in its message slot."
  (if (and (slot-value 'show) (slot-value 'message))
      (let* ((loc (send self :location))
	     (box (third loc))
	     (left (first loc))
	     (top (- (second loc) box))
	     (right (+ left box))
	     (bottom (+ top box))
	     (graph (send self :graph)))
	(when (and (< left x right) (< top y bottom))
	      (send self :toggle-show)
	      (send graph (slot-value 'message))
	      t))))



