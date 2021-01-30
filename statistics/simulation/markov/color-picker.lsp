;;; cpicker.lsp, A tool for choosing and creating colors.
;;; Copyright (C) 1993 B. Narasimhan
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(defproto color-choice-window-proto '(loc cbox-side color red green blue) 
  () graph-window-proto)

(defmeth color-choice-window-proto :modal-window (&optional (remove t))
  (send self :modal-window remove))

(defmeth color-choice-window-proto :modal-window-return (value)             
  (let ((target (send (slot-value 'modal-dialog) 
		      :slot-value 'modal-throw-target)))
    (if target (throw target value))))

(defmeth color-choice-window-proto :modal-dialog-return (value)             
"Method Args: (value)
Ends modal dialog loop and has :modal-dialog return VALUE."
  (let ((target (slot-value 'modal-throw-target)))
    (if target (throw target value))))

(defmeth color-choice-window-proto :isnew ()

    (setf (slot-value 'color) 'black)
    (send self :delete-method :do-motion)
    (send self :title "Color Picker")
    (setf (slot-value 'red) 0)
    (setf (slot-value 'green) 0)
    (setf (slot-value 'blue) 0)
    (let* ((menu (send menu-proto :new "Menu"))
	   (custom-color-item (send menu-item-proto :new "Custom Color"
				    :action
				    #'(lambda () (create-rgb-sliders self))))
	   (name-color-item (send menu-item-proto :new "Rename Color"
				  :action
				  #'(lambda () 
				      (let ((name 
					     (get-value-dialog 
					      "Enter new name" 
					      :initial (slot-value 'color))))
					(when name
					      (free-color (slot-value 'color))
					      (setf (slot-value 'color) 
						    (select name 0))
					      (make-color 
					       (slot-value 'color)
					       (slot-value 'red)
					       (slot-value 'green)
					       (slot-value 'blue))
					      (send self :redraw))))))
	   (free-color-item (send menu-item-proto :new "Free color"
				  :action
				  #'(lambda () 
				      (free-color (slot-value 'color))
				      (setf (slot-value 'color) 
					    (select (color-symbols) 0))
				      (send self :redraw)))))
				      
      
      (send menu :append-items custom-color-item name-color-item
	    free-color-item)
      (send self :menu menu))
    (call-next-method :show nil)
    (send self :redraw)
    (send self :show-window))
    
(defmeth color-choice-window-proto :redraw ()
  (let* ((ascent (send self :text-ascent))
	 (descent (send self :text-descent))
	 (cbox-side (+ ascent descent))
	 (em (send self :text-width "m"))
	 (row-sep (round (* .5 cbox-side)))
	 (col-sep (* 2 em))
	 (colors (color-symbols))
	 (color-names (map 'list #'(lambda (x) (string-downcase x :start 1))
			   (map 'list #'string colors)))
	 (max-name-width (max (map 'list #'(lambda (x)
					     (send self :text-width x))
				   color-names)))
	 (item-width (+ cbox-side em max-name-width))
	 (y-offset (+ 50 row-sep))
	 (n (length colors))
	 (c (floor (sqrt n)))
	 (r (floor (/ n c)))
	 (ww (+ (* c (+ col-sep item-width)) col-sep))
	 (wh (+ y-offset (if (eql (* c r) n)
			     (* r (+ row-sep cbox-side))
			   (* (+ r 1) (+ row-sep cbox-side))) row-sep))
	 (tmp nil))
    
    (setf (slot-value 'cbox-side) cbox-side)
    (send self :size ww wh)    
    (send self :draw-color 'white)
    (send self :paint-rect 0 0 ww wh)
    (setf (slot-value 'cbox-side) cbox-side)
    (dotimes (i n)
             (let ((y (+ (* (floor (/ i c)) (+ cbox-side row-sep)) 
			 row-sep y-offset))
		   (x (+ (* (mod i c) (+ item-width col-sep)) col-sep)))
               (setf tmp
		     (append tmp (list (list x y))))))
    (setf (slot-value 'loc) tmp)
    (send self :draw-color (slot-value 'color))
    (send self :paint-rect (- (round (* .5 ww)) 50) row-sep 100 50)
    (send self :draw-color 'black)
    (dotimes (j n)
	     (let* ((myloc (select tmp j))
		    (x (first myloc))
		    (name (select color-names j))
		    (y (second myloc)))
	       (send self :frame-rect x y cbox-side cbox-side)
	       (send self :draw-text name (+ x cbox-side em) (+ y ascent) 0 0)
	       (when (eql (select colors j) (slot-value 'color))
		     (send self :draw-line x y (+ x cbox-side) (+ y cbox-side))
		     (send self :draw-line x (+ y cbox-side) (+ x cbox-side) y))))))

(defmeth color-choice-window-proto :do-click (x y m1 m2)
  (let* ((loc (slot-value 'loc))
	 (box-side (slot-value 'cbox-side))
	 (n (length (slot-value 'loc)))
	 (val (catch 'my-click
		(dotimes (j n)
			 (let* ((myloc (select loc j))
				(left-x (first myloc))
				(bot-y (second myloc))
				(rt-x (+ left-x box-side))
				(top-y (+ bot-y box-side)))
			   (if (and (< left-x x rt-x) (< bot-y y top-y))
			       (throw 'my-click j)))))))
    (when val
	  (setf (slot-value 'color) (select (color-symbols) val))
	  (send self :redraw))))

(defun create-rgb-sliders (w) 
  (let* ((r-text (send text-item-proto :new "Red Intensity"))
	 (r-value (send text-item-proto :new "" :text-length 5))
	 (r-scroll (send interval-scroll-item-proto :new
			 (list 0 1)
			 :points 256
			 :text-item r-value
			 :action #'(lambda (x)
				     (send w :change :red x))
			 :initial 0))
	 
	 (g-text (send text-item-proto :new "Green Intensity"))
	 (g-value (send text-item-proto :new "" :text-length 5))
	 (g-scroll (send interval-scroll-item-proto :new
			 (list 0 1)
			 :points 256
			 :text-item g-value
			 :action #'(lambda (x)
				     (send w :change :green x))
			 :initial 0))
	 
	 (b-text (send text-item-proto :new "Blue Intensity"))
	 (b-value (send text-item-proto :new "" :text-length 5))
	 (b-scroll (send interval-scroll-item-proto :new
			 (list 0 1)
			 :points 256
			 :text-item b-value
			 :action #'(lambda (x)
				     (send w :change :blue x))
			 :initial 0))
	 (d (send dialog-proto :new (list (list r-text r-value) r-scroll
					  (list g-text g-value) g-scroll
					  (list b-text b-value) b-scroll))))
    (send w :add-subordinate d)))

	
(defmeth color-choice-window-proto :change (&key red green blue)
  (if red (setf (slot-value 'red) red))
  (if green (setf (slot-value 'green) green))
  (if blue (setf (slot-value 'blue) blue))
  (let ((ww (first (send self :size)))
	(row-sep (round (* .5 (slot-value 'cbox-side)))))
    (make-color 'custom (slot-value 'red) 
		(slot-value 'green) (slot-value 'blue))
    (setf (slot-value 'color) 'custom))
  (send self :redraw))
    
(defmeth color-choice-window-proto :give-rgb-values ()
  (parse-color (slot-value 'color)))



(def z (send color-choice-window-proto :new))




