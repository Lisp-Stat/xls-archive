;;; thm.lsp, Part of the dmc.lsp package.
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
(def *time-window* 0)
(def n 15)                           ; no of states you want.
(def m (random-stochastic-matrix n)) ; a random transition matrix. 

(def initial-state 7)                    ; Initial State.

(setf z (send dmc-proto :new m initial-state)) ; Create Markov Chain.

