;;; stochast.lsp, Part of the dmc.lsp package.
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

(provide "stochast")

(defun random-stochastic-matrix (n)
  " Returns a Random Stochastic matrix of dimension n."
  (let* ((tmp (uniform-rand (* n n)))
	 (m (row-list (matrix (list n n) tmp))))
    (dotimes (j n)
	     (setf (select m j)
		   (/ (select m j) 
		      (sum (select m j))))
	     (setf (select (select m j) (- n 1)) 
		   (- 1 (sum 
			 (select (select m j)
				 (iseq 0 (- n 2)))))))
    (matrix (list n n) (combine m))))

(defun square-matrixp (m)
  "Returns t if matrix m is square."
  (unless (matrixp m) 
	  (error "not a matrix - ~a" m))
  (let ((dim (array-dimensions m)))
    (eql (select dim 0) (select dim 1))))

(defun stochastic-matrixp (m)
  "Returns t if matrix m is stochastic."
  (unless (square-matrixp m)
	  (return))
  (dolist (row (row-list m) t)
	  (unless (< (abs (- (sum row) 1)) 
		     machine-epsilon)
		  (return))))

(defun doubly-stochastic-matrixp (m)
  "Returns t if matrix m is doubly-stochastic."
  (and (stochastic-matrixp m) 
       (stochastic-matrixp (transpose m))))

