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

(provide "utils")
(defun probability-p (x)
" Method args: (x)
Returns true if x is a number between 0 and 1, end-points included."
  (and (>= x 0.0) (<= x 1.0)))

(defun strict-probability-p (x)
" Method args: (x)
Returns true if x is a number between 0 and 1, end-points not included."
  (and (> x 0.0) (< x 1.0)))

(defun nonzero-probability-p (x)
" Method args: (x)
Returns true if x is a number between 0 and 1, 0 not included."
  (and (> x 0.0) (<= x 1.0)))

(defun nonunit-probability-p (x)
" Method args: (x)
Returns true if x is a number between 0 and 1, 1 not included."
  (and (>= x 0.0) (< x 1.0)))
(defun new-xlispstat ()
" Method args: none
Returns true if the version of xlispstat is 3.xx or greater."
  (and (boundp 'xls-major-release) (>= xls-major-release 3)))
(defun val-of (str)
"Method args: (str)
Returns the value of str. If empty or invalid string, returns nil."
  (if (= (length str) 0)
      nil
    (if (new-xlispstat)
        (ignore-errors (read-from-string str))
      (unwind-protect
          (read (make-string-input-stream str))))))
(defun get-values-from (list)
" Method args: (list)
List should be list of edit-text-items. A list of values from each of
them is returned."
  (mapcar #'(lambda(x) (val-of (send x :text))) list))
(defun get-numbers-from (list)
" Method args: (list)
List should be list of edit-text-items. A list of values from each of
them is returned.  If any of them is invalid, an error is signalled."
  (let ((vals (mapcar #'(lambda(x) (val-of (send x :text))) list)))
     (unless (every #'numberp vals)
        (error "Non-numeric values in some items."))
     vals))
(defun quantile-answer (probability quantile)
"Method args: (probability quantile)
Returns a string for a quantile answer."
  (let ((fstr (concatenate 'string
               "ANSWER: The " *probability-print-format* "-quantile is "
               *quantile-print-format* ".")))
     (format nil fstr probability quantile)))
(defun probability-answer (probability x1 x2)
"Method args: (probability x1 x2)
Returns a string for a probability answer."
  (if (and x2 (not x1))
     (let ((fstr (concatenate 'string
                "ANSWER: The probability to the right of "
                *quantile-print-format* " is "
                *probability-print-format* ".")))
                (format nil fstr x2 probability))
    (if (and x1 x2)
       (let ((fstr (concatenate 'string
                   "ANSWER: The probability between "
                   *quantile-print-format* " and "
                   *quantile-print-format* " is "
                   *probability-print-format* ".")))
             (format nil fstr x1 x2 probability))
     (let ((fstr (concatenate 'string
                   "ANSWER: The probability to the left of "
                   *quantile-print-format* " is "
                  *probability-print-format* ".")))
          (format nil fstr x1 probability)))))
(defmeth text-item-proto :width (&optional width)
"Method args: (&optional wid)
Sets or retrieves the width of a text-item."
  (if width
      (let ((sz (slot-value 'size)))
        (setf (slot-value 'size) (list width (select sz 1))))
    (select (slot-value 'size) 0)))
(defmeth edit-text-item-proto :width (&optional width)
"Method args: (&optional wid)
Sets or retrieves the width of a edit-text-item."
  (if width
      (let ((sz (slot-value 'size)))
        (setf (slot-value 'size) (list width (select sz 1))))
    (select (slot-value 'size) 0)))
(defmeth interval-scroll-item-proto :width (&optional width)
"Method args: (&optional width)
Sets or retrieves the width of an interval-scroll-item."
  (if width
      (let ((sz (slot-value 'size)))
        (setf (slot-value 'size) (list width (select sz 1))))
    (select (slot-value 'size) 0)))
