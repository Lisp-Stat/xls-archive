;;;; clt.lsp                    B. Narasimhan
;;;;
;;;;  This is a little tool I use in my intro classes to demonstrate
;;;;  the clt. 

;;;; Load this file and type (clt-demo) to get the 
;;;; demo.

(provide "clt")

(require "statistics")
(require "graphics")
(require "help")

(def c (/ 1 (sqrt (* 2 pi))))
(def nxbar 10)
(def n 10)
;(def pop (map 'list #'random (repeat 100 500)))
(def pop (gamma-rand 500 4))
(def pop-mean (mean pop))
(def pop-sigma (standard-deviation pop))

(def sigma (/ pop-sigma (sqrt n)))
(def mean pop-mean)
(defun f (x)
  (* (/ c sigma)
     (exp (* -0.5 (^ (/ (- x mean) sigma) 2)))))

(defun sample (size)
  (select pop (map 'list #'random (repeat 500 size))))

(defun xbar-sample (k)
  (map 'list #'mean (map 'list #'sample (repeat n k))))

(def xbars (xbar-sample nxbar))
(def xbar-mean (mean xbars))
(def xbar-stddev (standard-deviation xbars))


(defun clt-demo ()
  (let* ((pop-hist (histogram pop :title "Pop. Histogram"))
	 (hist (histogram xbars :title "Sample Mean Hist."))
	 (more-button (send button-item-proto :new "Add sample"
			    :action 
			    #'(lambda ()
				(send hist :add-sample))))
	 (restart-button (send button-item-proto :new "Restart"
			       :action 
			       #'(lambda ()
				   (send hist :restart))))
	 (dialog (send dialog-proto :new (list more-button
					       restart-button))))
    (send hist :num-bins 15)
    (send hist :add-function 
	  #'f (- mean (* 3 sigma)) (+ mean (* 3 sigma)))

    (send dialog :add-subordinate pop-hist)
    (send dialog :add-subordinate hist)
    (defmeth hist :add-sample ()
      (let ((point (mean (sample n))))
	(setf xbars (append xbars (list point)))
	(send self :add-points (list point))))))

    










