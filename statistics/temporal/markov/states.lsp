;;; states.lsp, Part of the dmc.lsp package.
;;; Copyright (C) 1993 B. Narasimhan & B. I. MacAlpine
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
;;;
;;; For complete details on this program, see 
;;; the technical report 
;;; "Discrete Finite State Markov Chains in Lisp-Stat"
;;; by B. Narasimhan and Brett I. MacAlpine.
;;;         

(provide "states")

;;; The individual State objects.

(defproto state-proto '(title state-no loc-and-size no-of-visits 
			      pvec))

(defmeth state-proto :isnew (name sno vec)
  "Method args: (title state prob-vec)
   Creates a new instance of state-proto. Title is used
   for the button title, state is the number of the state,
   and prob-vec gives the transition probabilities for the
   state."
  (let ((n (length vec)))
    (dolist (j (iseq 1 (- n 1)))
	    (setf (elt vec j) (+ (elt vec (- j 1)) (elt vec j))))
    (setf (slot-value 'pvec) vec)
    (setf (elt (slot-value 'pvec) (- n 1)) 1))
  (setf (slot-value 'no-of-visits) 0)
  (setf (slot-value 'title) name)
  (setf (slot-value 'state-no) sno))

(defmeth state-proto :loc-and-size (&optional loc-size)
  "Method args: (&optional loc-size)
  Sets or retrieves the location and size of box.
  Loc-size should be a list of 4 items, x, y, width, height."
  (if loc-size
      (setf (slot-value 'loc-and-size) loc-size)
    (slot-value 'loc-and-size)))

(defmeth state-proto :prob-vec ()
  "Method args: ()
   Returns the probability vector associated with the state."
  (let ((n (length (slot-value 'pvec)))
	(tmp (copy-vector (slot-value 'pvec))))
    (dolist (j (iseq (- n 1) 1))
	    (setf (elt tmp j) (- (elt tmp j) (elt tmp (- j 1)))))
    tmp))

(defmeth state-proto :no-of-visits ()
  "Method args: ()
   Returns the number of visits to the state so far."
  (slot-value 'no-of-visits))

(defmeth state-proto :you-are-current-state (w)
  "Method args: (window)
   Arranges for the state to become current state. Window
   should be the window in which the state sits."
  (let ((nv (slot-value 'no-of-visits)))
    (setf (slot-value 'no-of-visits) (+ nv 1)))
  (send self :redraw w))

(defmeth state-proto :you-are-not-current-state (w)
  "Method args: (window)
   Arranges for a current state to become a normal state.
   Window should be the window in which the state sits."
  (send self :redraw w))

(defmeth state-proto :redraw (window)
  "Method args: (window)
   Redraws the state in window."
  (let* ((dc (send window :draw-color))
	 (cs (eql (send window :current-state) 
		  (slot-value 'state-no)))
	 (tc (if cs
		 *current-state-text-color*
	       *normal-state-text-color*))
	 (bc (if cs
		 *current-state-color*
	       *normal-state-color*))
	 (ls (send self :loc-and-size))
	 (x (first ls))
	 (y (second ls))
	 (w (third ls))
	 (h (fourth ls))
	 (cx (+ x (round (* .5 w))))
	 (cy (+ y (round (* .5 h)))))
    (send window :draw-color bc)
    (send window :paint-oval x y w h)
    (send window :draw-color tc)
    (send window :draw-text 
	  (format nil "~d" (slot-value 'state-no))
	  cx (+ cy (round (* .5 (send window :text-ascent)))) 
	  1 0)
    (send window :draw-color dc)
    (send window :frame-oval x y w h)
    nil))

(defmeth state-proto :my-click (x y m1 m2)
  "Method args: (x y m1 m2)
   Returns t if mouse is clicked in box, else nil."
  (let* ((ls (send self :loc-and-size))
	 (left-x (first ls))
	 (bot-y (second ls))
	 (rt-x (+ left-x (third ls)))
	 (top-y (+ bot-y (fourth ls))))
    (if (and (< left-x x rt-x) (< bot-y y top-y))
	t
      nil)))

(defmeth state-proto :do-click (w)
  "Method args: (window)
   Pops up a message box with some statistics."
  (let ((nv (send self :no-of-visits))
	(time (send w :time)))
    (if (eql time 0)
	(message-dialog (format nil 
				"State ~s~%No. of visits ~d~%"
				(send self :title) nv))
      (message-dialog (format nil 
			      "State ~s~%No. of visits ~d~%~
                                Proportion ~7,5f~%"
				(send self :title) 
				nv (/ nv time))))))

(defmeth state-proto :reset ()
  "Method args: ()
   Resets the state."
  (setf (slot-value 'no-of-visits) 0))

(defmeth state-proto :title (&optional title)
  "Method args: (&optional title)
   Sets and retrieves the title."
  (if title 
      (setf (slot-value 'title) title)
  (slot-value 'title)))

(defmeth state-proto :describe (&optional time)
  "Method args: (&optional time)
   Prints some statistics.  If time is given, prints
   the proportion of time in state also."
  (let ((nv (send self :no-of-visits)))
    (if (> nv 0)
	(if time
	    (format t "~3d         ~7d       ~1,5f~%"
		    (slot-value 'title) nv (/ nv time))
	  (format t "State ~a, No of visits: ~g~%" 
		  (slot-value 'state-no) nv)))))
 
(defmeth state-proto :next-state ()
  "Method args: ()
   Returns the next state the Markov Chain will goto 
   from this state."
  (let* ((u (select (uniform-rand 1) 0))
	 (pvec (slot-value 'pvec))
	 (state (catch 'index
		  (dotimes (j n)
			   (if (< u (select pvec j))
			       (throw 'index j))))))
    state))










