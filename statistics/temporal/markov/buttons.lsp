;;; buttons.lsp, Part of the dmc.lsp package.
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

(require "but-opts")
(provide "buttons")

(defproto button-obj-proto '(boss title loc-and-size 
				  click-action)) 

(defmeth button-obj-proto :isnew (title loc-and-size action)
  (setf (slot-value 'title) title)
  (setf (slot-value 'loc-and-size) loc-and-size)
  (setf (slot-value 'click-action) action))

(defmeth button-obj-proto :title (&optional title)
  (if title
      (setf (slot-value 'title) title)
    (slot-value 'title)))

(defmeth button-obj-proto :do-click ()
  (funcall (slot-value 'click-action)))

(defmeth button-obj-proto :loc-and-size (&optional loc-size)
  "Sets or retrieves the location and size of box."
  (if loc-size
      (setf (slot-value 'loc-and-size) loc-size)
    (slot-value 'loc-and-size)))

(defmeth button-obj-proto :redraw (window)
  "Redraws the button with the specified color in window."
  (let* ((dc (send window :draw-color))
         (ls (send self :loc-and-size))
         (x (first ls))
         (y (second ls))
         (w (third ls))
         (h (fourth ls))
         (cx (+ x (round (* .5 w))))
         (cy (+ y (round (* .5 h)))))

    (send window :draw-color *button-back-color*)
    (send window :paint-rect x y w h)
    (send window :draw-color *button-text-color*)
    (send window :draw-text
          (slot-value 'title)
          cx (+ cy (round (* .5 
			     (send window :text-ascent)))) 1 0)
    (send window :draw-color dc)
    (send window :frame-rect x y w h)
    nil))

(defmeth button-obj-proto :my-click (x y m1 m2)
  "Returns t if mouse is clicked in box, else nil."
  (let* ((ls (send self :loc-and-size))
         (left-x (first ls))
         (bot-y (second ls))
         (rt-x (+ left-x (third ls)))
         (top-y (+ bot-y (fourth ls))))
    (if (and (< left-x x rt-x) (< bot-y y top-y))
        t
      nil)))











