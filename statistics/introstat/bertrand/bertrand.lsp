;;;
;;; @(#)$Header: /usr/people/naras/xlisp/Teaching/bertrand.lsp,v 1.1 1994/10/30 15:49:04 naras Exp $
;;;
;;; Copyright (C) 1994 B. Narasimhan, naras@euler.bd.psu.edu
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
;;; Demonstrates Bertrand's paradox to my probability students.
;;; B. Narasimhan
;;; 

(require "slider-control")

(defparameter *n* 100)
(defconstant root-three (sqrt 3))
(setf *relative-slidebar-length* 1.0)
(setf *current-choice* -1)

;;;
;;; My stuff really begins here.
;;;

(defun method (type)
  (cond
   ((= type 0) (method-1))
   ((= type 1) (method-2))
   (t (method-3))))

(defun method-1 ()
  "Generate the chord by choosing rectangular coordinates of its 
midpoint uniformly in [-1, 1]."
  (let ((u (- (* 2 (uniform-rand 2)) 1)))
    (loop
     (when (<= (sum (* u u)) 1.0)
	   (return))
     (setf u (- (* 2 (uniform-rand 2)) 1)))
    (let* ((x0 (select u 0))
	   (y0 (select u 1))
	   (dist (+ (* x0 x0) (* y0 y0)))
	   (disc-root (* (sqrt (- (/ dist) 1)) y0))
	   (x1a (+ x0 disc-root))                     
	   (x1b (- x0 disc-root))
	   (y1a (+ (/ (+ (* (- x0) x1a) (* y0 y0) (* x0 x0)) y0)))
	   (y1b (+ (/ (+ (* (- x0) x1b) (* y0 y0) (* x0 x0)) y0))))
      (when (> (* 2.0 (sqrt (- 1.0 dist))) root-three)
	    (setf *success-count* (1+ *success-count*)))
      (list (list x1a x1b) (list y1a y1b)))))

(defun method-2 ()
  "Generate the chord by choosing the polar coordinates of its midpoint
uniformly.  But this is the same as choosing r uniformly while theta 
is fixed, say at pi/2."
  (let* ((y (- (* 2 (select (uniform-rand 1) 0)) 1))
	 (x (sqrt (- 1 (* y y)))))
    (when (> (* 2.0 (sqrt (- 1.0 (* y y)))) root-three)
	  (setf *success-count* (1+ *success-count*)))
    (list (list x (- x)) (list y y))))

(defun method-3 ()
  "Generate the chord by choosing the polar coordinates (1,alpha) 
and (1,beta) of the end points of the chord randomly. But this is the
same as fixing beta at, say (1,0) and choosing alpha randomly."
  (let* ((alpha (* 2 pi (select (uniform-rand 1) 0)))
	 (x (cos alpha))
	 (y (sin alpha)))
    (when (> (sqrt (* 2.0 (- 1.0 x))) root-three)
	  (setf *success-count* (1+ *success-count*)))
    (list (list x 1.0) (list y 0.0))))

(defun run-simulation (g choice)
  (if (= choice *current-choice*)
      (setf *total-count* (+ *total-count* *n*))
    (progn
      (setf *current-choice* choice)
      (send g :start-buffering)
      (send g :clear :draw nil)
      (send g :add-function #'(lambda(x) (sqrt (- 1 (* x x)))) -1 1)
      (send g :add-function #'(lambda(x) (- (sqrt (- 1 (* x x))))) -1 1)
      (send g :adjust-to-data)
      (send g :buffer-to-screen)
      (setf *total-count* *n*)
      (setf *success-count* 0)))
  (let ((m (mapcar #'(lambda (x) (method choice)) (iseq *n*)))
	(o (select (send g :slot-value 'overlays) 0)))
    (send g :start-buffering)
    (dolist (x m)
	    (send g :add-lines (select x 0) (select x 1)))
    (send g :buffer-to-screen)
    (send o :draw-indicator 
	  (floor (* 1000 (/ *success-count* *total-count*))))))

(defun bertrands-paradox ()
  "Graphically illustrates Bertrand's paradox."
  (let* ((g (plot-function #'(lambda(x) (sqrt (- 1 (* x x)))) -1 1))
	 (sz (send g :size)))
    (send g :title "Bertrand's Paradox")
    (send g :add-function #'(lambda(x) (- (sqrt (- 1 (* x x))))) -1 1)
    (send g :margin 150 0 0 0)
    (send g :size (+ (select sz 0) 150) (select sz 1))
    (send g :add-overlay
	  (send slider-control-proto :new (iseq 1001)
		:title "Probability"
		:length (send g :slider-width)
		:display (rseq 0 1 1001)))
    (send g :adjust-to-data)
    (let* ((text (send text-item-proto :new 
		       (format nil
			       "Choose one of the following three methods~%~
                               for generating the random chord in the ~
                               unit circle~%~
                               and press the Run button. You will see that the~%~
                               Probability changes depending on what you~%~
                               mean by random!~%")))
	   (choices (send choice-item-proto :new 
			  (list "Random Rectangular coordinates of Midpoint"
				"Random Polar coordinates of Midpoint"
				"Random Polar coordinates of Endpoints")
			  :value 0))
	   (run (send button-item-proto :new "Run 100 Simulations"
		      :action #'(lambda() 
				  (run-simulation g (send choices :value)))))
	   (d (send dialog-proto :new 
		    (list (list text) (list choices) (list run)))))
      (send d :add-subordinate g)
      g)))
  







