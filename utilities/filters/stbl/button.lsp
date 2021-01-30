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

(provide "button")
(defproto button-overlay-proto '(llx ury urx lly title action) ()
  graph-overlay-proto
  "The button overlay prototype. Title is the title displayed on the
  button, and action is the function that is called when the mouse is
  clicked in the box. The slots llx and ury hold the lower left and
  upper right coordinates of the box.")
(defmeth button-overlay-proto :isnew (llx ury title action)
  "Method args: (llx ury title action)
Title is a string, action is a function that is invoked when
the mouse is clicked in the box, llx and ury are the coordinates of
the lower left and upper right x and y respectively. "
  (setf (slot-value 'action) action)
  (setf (slot-value 'title) title)
  (setf (slot-value 'llx) llx)
  (setf (slot-value 'ury) ury)
  (call-next-method))
(defmeth button-overlay-proto :redraw ()
  "Method args: none.
This method redraws the overlay."
  (let* ((graph (send self :graph))
         (em (send graph :text-width "m"))
         (gap (round (* .5 em)))
         (title (slot-value 'title))
         (llx (slot-value 'llx))
         (ury (slot-value 'ury))
         (tw (send graph :text-width title))
         (wid (+ gap tw gap))
         (ht (+ gap (send graph :text-ascent) (send graph :text-descent)
                gap)))
    (setf (slot-value 'urx) (+ llx wid))
    (setf (slot-value 'lly) (+ ury ht))
    (send graph :draw-string title (+ llx gap)  (+ ury (- ht gap)))
    (send graph :frame-rect llx ury wid ht)))
(defmeth button-overlay-proto :do-click (x y m1 m2)
  "Method args: x y m1 m2
This method invokes the function in the action slot when the mouse is
clicked on the button."
  (let ((llx (slot-value 'llx))
        (ury (slot-value 'ury))
        (urx (slot-value 'urx))
        (lly (slot-value 'lly)))
    (when (and (< llx x urx) (< ury y lly))
          (funcall (slot-value 'action))
          t)))
