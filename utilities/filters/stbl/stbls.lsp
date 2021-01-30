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
(provide "stbls")
(defun stbl ()
"Method args: none
 Installs statistical tables in a menu."
  (let ((menu (send menu-proto :new "Tables")))
    (send menu :append-items
          (send menu-item-proto :new "Normal Distribution"
                :action #'normal-distribution)
          (send menu-item-proto :new "T Distribution"
                :action #'t-distribution)
          (send menu-item-proto :new "Chi-square Distribution"
                :action #'chisq-distribution)
          (send menu-item-proto :new "F Distribution"
                :action #'F-distribution))
    (send menu :install)))
(stbl)
