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

(require "dists")
(def z 
  (send dist-plot-proto :new
        :dens #'gaussian-density
        :cdf #'gaussian-cdf
        :icdf #'gaussian-icdf
        :params (list 0 1)
        :params-print-format "Parameters: Mean = ~d, Std. Dev. = ~d"
        :prob-dialog #'normal-prob-dialog
        :quant-dialog #'normal-quant-dialog
        :num-points 50
        :title "Normal Distribution"))
(defmeth z :close () (send self :remove) (exit))
(send z :l-point -2)
(send z :r-point 2)
(send z :important-abscissae '(-2 2))
(send z :answer "The shaded area is 95%")
(send z :redraw)
