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

(require "distproto")
(provide "dists")
(defun gaussian-density (x mu sigma)
  "Method args: (x mu sigma)
Returns the normal density with mean mu and std. dev. sigma at x."
  (/ (normal-dens (/ (- x mu) sigma)) sigma))
(defun gaussian-cdf (x mu sigma)
  "Method args: (x mu sigma)
Returns the normal cdf with mean mu and std. dev. sigma at x."
  (normal-cdf (/ (- x mu) sigma)))
(defun gaussian-icdf (x mu sigma)
  "Method args: (x mu sigma)
Returns the x-th normal quantile with mean mu and std. dev. sigma."
  (+ (* sigma (normal-quant x)) mu))
(defun normal-quant-dialog (dist)
  "Dialog for the quantiles of the Normal Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find Normal Distribution Quantiles,~%~
                     complete all fields and press OK.~%")))
         (mean-label (send text-item-proto :new "Mean"))
         (mean-val (send edit-text-item-proto :new
                         (format nil "~a" (select params 0)) :text-length 10))
         (sd-label (send text-item-proto :new "Std. Dev."))
         (sd-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 1)) :text-length 10))
         (prob-label (send text-item-proto :new "Probability"))
         (prob-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list mean-label mean-val sd-label sd-val 
                      prob-label prob-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-numbers-from 
                                      (list mean-val sd-val prob-val)))
                             (params (select inputs '(0 1)))
                             (prob (select inputs 2)))
                         (send dist :params params)
                         (send dist :l-point (send dist :icdf-at prob))
                         (send dist :important-abscissae 
                               (list (send dist :l-point)))
                         (send dist :r-point nil)
                         (send dist :answer
                              (quantile-answer prob (send dist :l-point)))
                         (send dist :adjust-to-data)
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list mean-label sd-label)
                           (list mean-val sd-val)
                           (list prob-label prob-val)
                           (list ok cancel))
                :title "Normal Quantile Dialog") :modal-dialog)))
(defun normal-prob-dialog (dist)
  "Dialog for the probabilities of the Normal Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find Normal Distribution Probabilities,~%~
                     complete appropriate fields and press OK.~%~
                     For probability to the left, use Left Point;~%~
                     for probability to the right, use Right Point;~%~
                     for probability between, use both.~%")))
         (mean-label (send text-item-proto :new "Mean"))
         (mean-val (send edit-text-item-proto :new
                         (format nil "~a" (select params 0)) :text-length 10))
         (sd-label (send text-item-proto :new "Std. Dev."))
         (sd-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 1)) :text-length 10))
         (l-label (send text-item-proto :new "Left Point"))
         (l-val (send edit-text-item-proto :new "" :text-length 10))
         (r-label (send text-item-proto :new "Right Point"))
         (r-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list mean-label mean-val sd-label sd-val
                      l-label l-val r-label r-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-values-from 
                                      (list mean-val sd-val l-val r-val)))
                             (params (select inputs '(0 1)))
                             (lx (select inputs 2))
                             (rx (select inputs 3))
                             (between (and rx lx))
                             (left (and lx (not rx)))
                             (prob (if between
                                      (- (send dist :cdf-at rx)
                                         (send dist :cdf-at lx))
                                     (if left
                                        (send dist :cdf-at lx)
                                       (- 1 (send dist :cdf-at rx))))))
                         (send dist :params params)
                         (send dist :l-point lx)
                         (send dist :r-point rx)
                         (send dist :important-abscissae
                               (if between
                                   (list lx rx)
                                 (if left
                                     (list lx)
                                   (list rx))))
                         (send dist :answer
                              (probability-answer prob lx rx))
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list mean-label sd-label)
                           (list mean-val sd-val)
                           (list l-label l-val)
                           (list r-label r-val)
                           (list ok cancel))
                :title "Normal Probability Dialog") :modal-dialog)))
(defun normal-distribution ()
  (send dist-plot-proto :new
        :dens #'gaussian-density
        :cdf #'gaussian-cdf
        :icdf #'gaussian-icdf
        :params (list 0 1)
        :params-print-format "Parameters: Mean = ~,3f, Std. Dev. = ~,3f"
        :prob-dialog #'normal-prob-dialog
        :quant-dialog #'normal-quant-dialog
        :num-points 50
        :title "Normal Distribution"))
(defun t-quant-dialog (dist)
  "Dialog for the quantiles of the T Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find T-Distribution Quantiles,~%~
                     complete all fields and press OK.~%")))
         (df-label (send text-item-proto :new "Degrees of Freedom"))
         (df-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 0)) :text-length 10))
         (prob-label (send text-item-proto :new "Probability"))
         (prob-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list df-label df-val prob-label prob-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-numbers-from 
                                      (list df-val prob-val)))
                             (params (select inputs '(0)))
                             (prob (select inputs 1)))
                         (send dist :params params)
                         (send dist :l-point (send dist :icdf-at prob))
                         (send dist :important-abscissae 
                               (list (send dist :l-point)))
                         (send dist :r-point nil)
                         (send dist :answer
                              (quantile-answer prob (send dist :l-point)))
                         (send dist :adjust-to-data)
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list df-label prob-label)
                           (list df-val prob-val)
                           (list ok cancel))
                :title "T Quantile Dialog") :modal-dialog)))
(defun t-prob-dialog (dist)
  "Dialog for the probabilities of the T Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find T-Distribution Probabilities,~%~
                     complete appropriate fields and press OK.~%~
                     For probability to the left, use Left Point;~%~
                     for probability to the right, use Right Point;~%~
                     for probability between, use both.~%")))
         (df-label (send text-item-proto :new "Degrees of Freedom"))
         (df-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 0)) :text-length 10))
         (l-label (send text-item-proto :new "Left Point"))
         (l-val (send edit-text-item-proto :new "" :text-length 10))
         (r-label (send text-item-proto :new "Right Point"))
         (r-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list df-label df-val l-label l-val r-label r-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-values-from 
                                      (list df-val l-val r-val)))
                             (params (select inputs '(0)))
                             (lx (select inputs 1))
                             (rx (select inputs 2))
                             (between (and rx lx))
                             (left (and lx (not rx)))
                             (prob (if between
                                      (- (send dist :cdf-at rx)
                                         (send dist :cdf-at lx))
                                     (if left
                                        (send dist :cdf-at lx)
                                       (- 1 (send dist :cdf-at rx))))))
                         (send dist :params params)
                         (send dist :l-point lx)
                         (send dist :r-point rx)
                         (send dist :important-abscissae
                               (if between
                                   (list lx rx)
                                 (if left
                                     (list lx)
                                   (list rx))))
                         (send dist :answer
                              (probability-answer prob lx rx))
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list df-label df-val)
                           (list l-label l-val)
                           (list r-label r-val)
                           (list ok cancel))
                :title "T Probability Dialog") :modal-dialog)))
(defun t-distribution ()
  (send dist-plot-proto :new
        :dens #'t-dens
        :cdf #'t-cdf
        :icdf #'t-quant
        :params (list 15)
        :params-print-format "Parameter: Degrees of Freedom = ~,1f"
        :prob-dialog #'t-prob-dialog
        :quant-dialog #'t-quant-dialog
        :num-points 50
        :title "T-Distribution"))
(defun chisq-quant-dialog (dist)
  "Dialog for the quantiles of the Chi-square Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find Chi-square Distribution Quantiles,~%~
                     complete all fields and press OK.~%")))
         (df-label (send text-item-proto :new "Degrees of Freedom"))
         (df-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 0)) :text-length 10))
         (prob-label (send text-item-proto :new "Probability"))
         (prob-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list df-label df-val prob-label prob-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-numbers-from 
                                      (list df-val prob-val)))
                             (params (select inputs '(0)))
                             (prob (select inputs 1)))
                         (send dist :params params)
                         (send dist :l-point (send dist :icdf-at prob))
                         (send dist :important-abscissae 
                               (list (send dist :l-point)))
                         (send dist :r-point nil)
                         (send dist :answer
                              (quantile-answer prob (send dist :l-point)))
                         (send dist :adjust-to-data)
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list df-label prob-label)
                           (list df-val prob-val)
                           (list ok cancel))
                :title "Chi-square Quantile Dialog") :modal-dialog)))
(defun chisq-prob-dialog (dist)
  "Dialog for the probabilities of the Chi-square Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find Chisq-Distribution Probabilities,~%~
                     complete appropriate fields and press OK.~%~
                     For probability to the left, use Left Point;~%~
                     for probability to the right, use Right Point;~%~
                     for probability between, use both.~%")))
         (df-label (send text-item-proto :new "Degrees of Freedom"))
         (df-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 0)) :text-length 10))
         (l-label (send text-item-proto :new "Left Point"))
         (l-val (send edit-text-item-proto :new "" :text-length 10))
         (r-label (send text-item-proto :new "Right Point"))
         (r-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list df-label df-val l-label l-val r-label r-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-values-from 
                                      (list df-val l-val r-val)))
                             (params (select inputs '(0)))
                             (lx (select inputs 1))
                             (rx (select inputs 2))
                             (between (and rx lx))
                             (left (and lx (not rx)))
                             (prob (if between
                                      (- (send dist :cdf-at rx)
                                         (send dist :cdf-at lx))
                                     (if left
                                        (send dist :cdf-at lx)
                                       (- 1 (send dist :cdf-at rx))))))
                         (send dist :params params)
                         (send dist :l-point lx)
                         (send dist :r-point rx)
                         (send dist :important-abscissae
                               (if between
                                   (list lx rx)
                                 (if left
                                     (list lx)
                                   (list rx))))
                         (send dist :answer
                              (probability-answer prob lx rx))
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list df-label df-val)
                           (list l-label l-val)
                           (list r-label r-val)
                           (list ok cancel))
                :title "Chi-square Probability Dialog") :modal-dialog)))
(defun chisq-distribution ()
  (send dist-plot-proto :new
        :dens #'chisq-dens
        :cdf #'chisq-cdf
        :icdf #'chisq-quant
        :params (list 15)
        :params-print-format "Parameter: Degrees of Freedom = ~,1f"
        :prob-dialog #'chisq-prob-dialog
        :quant-dialog #'chisq-quant-dialog
        :num-points 50
        :title "Chi-Square Distribution"))
(defun f-quant-dialog (dist)
  "Dialog for the quantiles of the F Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find F Distribution Quantiles,~%~
                     complete all fields and press OK.~%")))
         (ndf-label (send text-item-proto :new "Numerator df"))
         (ndf-val (send edit-text-item-proto :new
                         (format nil "~a" (select params 0)) :text-length 10))
         (ddf-label (send text-item-proto :new "Denominator df"))
         (ddf-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 1)) :text-length 10))
         (prob-label (send text-item-proto :new "Probability"))
         (prob-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list ndf-label ndf-val ddf-label ddf-val
                      prob-label prob-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-numbers-from 
                                      (list ndf-val ddf-val prob-val)))
                             (params (select inputs '(0 1)))
                             (prob (select inputs 2)))
                         (send dist :params params)
                         (send dist :l-point (send dist :icdf-at prob))
                         (send dist :important-abscissae 
                               (list (send dist :l-point)))
                         (send dist :r-point nil)
                         (send dist :answer
                              (quantile-answer prob (send dist :l-point)))
                         (send dist :adjust-to-data)
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list ndf-label ddf-label)
                           (list ndf-val ddf-val)
                           (list prob-label prob-val)
                           (list ok cancel))
                :title "F Quantile Dialog") :modal-dialog)))
(defun f-prob-dialog (dist)
  "Dialog for the probabilities of the F Distribution."
  (let* ((params (send dist :params))
         (prompt (send text-item-proto :new
                  (format nil 
                    "To find F istribution Probabilities,~%~
                     complete appropriate fields and press OK.~%~
                     For probability to the left, use Left Point;~%~
                     for probability to the right, use Right Point;~%~
                     for probability between, use both.~%")))
         (ndf-label (send text-item-proto :new "Numerator df"))
         (ndf-val (send edit-text-item-proto :new
                         (format nil "~a" (select params 0)) :text-length 10))
         (ddf-label (send text-item-proto :new "Denominator df"))
         (ddf-val (send edit-text-item-proto :new
                       (format nil "~a" (select params 1)) :text-length 10))
         (l-label (send text-item-proto :new "Left Point"))
         (l-val (send edit-text-item-proto :new "" :text-length 10))
         (r-label (send text-item-proto :new "Right Point"))
         (r-val (send edit-text-item-proto :new "" :text-length 10))
         (olist (list mean-label mean-val sd-label sd-val
                      l-label l-val r-label r-val))
         (mwid (max (mapcar #'(lambda(x) (send x :width)) olist)))
         (ok (send modal-button-proto :new "OK"
                   :action
                   #'(lambda ()
                      (let* ((inputs (get-values-from 
                                      (list ndf-val ddf-val l-val r-val)))
                             (params (select inputs '(0 1)))
                             (lx (select inputs 2))
                             (rx (select inputs 3))
                             (between (and rx lx))
                             (left (and lx (not rx)))
                             (prob (if between
                                      (- (send dist :cdf-at rx)
                                         (send dist :cdf-at lx))
                                     (if left
                                        (send dist :cdf-at lx)
                                       (- 1 (send dist :cdf-at rx))))))
                         (send dist :params params)
                         (send dist :l-point lx)
                         (send dist :r-point rx)
                         (send dist :important-abscissae
                               (if between
                                   (list lx rx)
                                 (if left
                                     (list lx)
                                   (list rx))))
                         (send dist :answer
                              (probability-answer prob lx rx))
                         (send dist :redraw)))))
         (cancel (send button-item-proto :new "Cancel"
                       :action
                       #'(lambda()
                           (send (send cancel :dialog)
                                 :modal-dialog-return nil)))))
    (dolist (x olist)
            (send x :width mwid))
    (send (send modal-dialog-proto
                :new (list prompt
                           (list ndf-label ddf-label)
                           (list ndf-val ddf-val)
                           (list l-label l-val)
                           (list r-label r-val)
                           (list ok cancel))
                :title "F Probability Dialog") :modal-dialog)))
(defun f-distribution ()
  (send dist-plot-proto :new
        :dens #'f-dens
        :cdf #'f-cdf
        :icdf #'f-quant
        :params (list 20 20)
        :params-print-format "Parameters: Num. df. = ~,1f, Den. df. = ~,1f"
        :prob-dialog #'f-prob-dialog
        :quant-dialog #'f-quant-dialog
        :num-points 50
        :title "FDistribution"))
