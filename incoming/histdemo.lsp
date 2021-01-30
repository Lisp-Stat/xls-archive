;;;
;;; A simple demo of histograms and kernel density estimation
;;; 

;;  Kjetil Halvorsen, La Paz, Bolivia, May 1998
;;     Copyright: GPL

;; Tu use: Simply load this file, arrange the tree windows. Appears menu
;;   "HistDemo", can use to change sample size, Choose new sample (new sample size in
;;      effect only after this choice) or remove menu AND all windows.
;;      sliders for changing number of bins in histogram, and window width in
;;      superposed kernel density estimation. Uses gaussian kernel.

;; The data is in *data*
;; sample size is *n*

(def *n* 100)
(def *data* (normal-rand *n*))
(def pause 50)




(def hist (histogram *data*))
(send hist :use-color t)
(send hist :size 400 200)
(send hist :redraw)
(send hist :title "Normal  (100)")


(def bin-slider (sequence-slider-dialog (iseq 2 50)
                                        :text "Bins:"
                                        :title "Numero de bins"
                                        :action #'(lambda (nbins)
                             (send hist :num-bins nbins))))

(def *widths* (list 0.1 2))

(def dens-slider (interval-slider-dialog *widths*
                                         :text "Ventana:"
                                         :title "Estimacion Nucleo:"
                                         :action #'(lambda (w)
   (send hist :clear-lines :draw nil)
   (send hist :add-lines (kernel-dens 
                           *data* :xvals 100 :width w :type 'g)
         :color 'red))))
                                      
(send hist :add-subordinate bin-slider)
(send hist :add-subordinate dens-slider)

;;;
;;;  A simple menu for the demo:
;;;

(defun get-n ()
  (setq *n* (car (get-value-dialog "Tamano de la muestra"                                
                   :initial 100))))

(defun get-distribution ()
(let ((dist (choose-item-dialog "Distribucion"
                                (list "Normal" "Gamma"
                                      "Uniform" "Triangular"))))
  (when dist
        (cond ((eq dist 0)
               (setq *data* (normal-rand *n*))
               (send hist :title (format nil "~a   (~d) " "Normal" *n*)))
              ((eq dist 1)
               (setq *data* (gamma-rand *n* 0.5))
               (send hist :title (format nil "~a   (~d) " "Gamma" *n*)))
              ((eq dist 2)
               (setq *data* (uniform-rand *n*))
               (send hist :title (format nil "~a   (~d) " "Uniform" *n*)))
              ((eq dist 3)
               (setq *data* (+ (uniform-rand *n*)
                               (uniform-rand *n*)))
               (send hist :title (format nil "~a   (~d) " "Triangular" *n*)))
              (t
                (princ "*** Argumento illegal ***"))))))

(defun install-new-distribution ()
(send hist :clear-points)
(send hist :add-points *data*)
(send hist :clear-lines)
(send hist :adjust-to-data))

(defun animate ()
(dotimes (i 50)
         (send hist :num-bins i)
         (pause pause)))



(setq histdemo-menu (send menu-proto :new "HistDemo"))

(setq distribution-item (send menu-item-proto :new "Nueva muestra"
                              :action #'(lambda () 
                                          (get-distribution)
                                          (install-new-distribution))))

(setq n-item (send menu-item-proto :new "Tamano de la muestra"
                   :action #'get-n))

(setq remove-item (send menu-item-proto :new "Remove"
                        :action #'(lambda ()
                                    (send histdemo-menu :remove)
                                    (send histdemo-menu :dispose)
                                    (send hist :remove)
                                    (undef 
       '(*data* *n* *widths* hist bin-slider dens-slider pause)))))

(setq animate-item (send menu-item-proto :new "Pelicula"
                         :action #'animate))

(setq pause-item (send menu-item-proto :new "Pausa"
                       :action #'(lambda ()
                                   (setq pause (car (get-value-dialog "Pause"
                                                                      :initial pause))))))

(send histdemo-menu :install)
(send histdemo-menu :append-items  distribution-item
                                   n-item
                                   animate-item
                                   pause-item
                                   remove-item)


;;;    EOF    ;;;;



                       
