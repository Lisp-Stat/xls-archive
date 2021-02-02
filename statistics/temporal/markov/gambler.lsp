;;; gambler.lsp, Part of the dmc.lsp package.
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

(require "dmc")

(def n 26)                      ; Gambler's Maximum Fortune is $25.
(def *prob-win* .5)

;;; Override the :next-state method with a new one for speed.

(defmeth state-proto :next-state ()
  "Method args: ()
   Returns the next state the Markov Chain will goto 
   from this state.
   NOTE: This is a local change for speeding up
         multiple runs."
  (let ((cs (slot-value 'state-no)))
    (if (or (eql cs 0) (eql cs (- n 1)))
	cs
      (let ((u (select (uniform-rand 1) 0)))
	(if (< u *prob-win*)
	    (+ cs 1)
	  (- cs 1))))))


;;; Create the Transition Matrix.

(def m (identity-matrix n))
(dotimes (i (- n 1))
	 (when (> i 0)
	       (setf (aref m i i) 0)
	       (setf (aref m i (+ i 1)) *prob-win*)
	       (setf (aref m i (- i 1)) (- 1 *prob-win*))))

(def initial-state 12)         ; Gambler's initial money is $12.

(setf z (send dmc-proto :new m initial-state "Gambler's Ruin")) 
					; Create Markov Chain.

(defun test-gambler (number)
  (let ((w 0)
	(l 0)
	(tmp nil))
    (dotimes (j number)
;;	     (format t "Game: ~d~%" j)
	     (send z :reset)
	     (send z :run-dmc-silently 500)
	     (setf tmp (send z :current-state))
	     (if (eql tmp 25)
		 (setf w (+ w 1))
	       (if (eql tmp 0)
		   (setf l (+ l 1)))))
    (format t "Wins: ~d, Losses: ~d, ~
               Decided games: ~d, ~
               Proportion wins: ~d~%"
	    w l (+ w l) (/ w (+ w l)))))

	     





















