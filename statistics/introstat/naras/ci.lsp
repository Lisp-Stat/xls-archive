;;;;
;;;; ci.lsp                    B. Narasimhan
;;;;
;;;;  This is a little tool I use in my intro classes to 
;;;;  demonstrate the meaning of Confidence level.  You sample
;;;;  from a given population everytime and the population 
;;;;  happens to be finite here.  However, it is easy to
;;;;  extend this further.

;;;;  Load this file and type 
;;;;  (ci-demo) to get the demonstration.
;;;;  A histogram of the population along with a plot of the
;;;;  confidence interval is shown.
;;;;  The dialog box controls the window.
;;;;  The Vertical line in the centre of the plot shows the
;;;;  true value of the mean.

(provide "ci")

(require "statistics")
(require "graphics")
(require "help")

(def n 30)
(def inc .1)
(def count 0)
(def confidence .95)
;(def pop (map 'list #'random (repeat 100 500)))
(def pop (gamma-rand 500 4))
(def pop-mean (mean pop))
(def pop-sigma (standard-deviation pop))

(def sigma (/ pop-sigma (sqrt n)))
(def mean pop-mean)

(defun sample (size)
  (select pop (map 'list #'random (repeat 500 size))))

(defun ci-demo ()
  (let* ((pop-hist (histogram pop :title "Pop. Histogram"))
	 (samp (sample n))
	 (xbar (mean samp))
	 (s (standard-deviation samp))
	 (len (/ (* (normal-quant confidence) s) (sqrt n)))
	 (l (- xbar len))
	 (r (+ xbar len))
	 (x (list l r))
	 (y (list 0 0))
	 (ci-plot (plot-points x y))
	 (next-button (send button-item-proto :new "New Sample"
		     :action 
		     #'(lambda ()
			 (send ci-plot :add-ci))))
	 (restart-button (send button-item-proto :new "Restart"
			       :action 
			       #'(lambda ()
				   (send ci-plot :clear :draw nil)
				   (send ci-plot :redraw))))
	 (dialog (send dialog-proto :new (list next-button
					       restart-button))))

    (send ci-plot :range 1 -1 1)
    (send ci-plot :add-lines (list (list pop-mean pop-mean) (list -1 1)))
    (send ci-plot :add-lines (list x y))
    (send dialog :add-subordinate pop-hist)
    (send dialog :add-subordinate ci-plot)
    (defmeth ci-plot :add-ci ()
      (let* (
	     (samp (sample n))
	     (xbar (mean samp))
	     (s (standard-deviation samp))
	     (len (/ (* (normal-quant confidence) s) (sqrt n)))
	     (l (- xbar len))
	     (r (+ xbar len))
	     (x (list l r))
	     (y (+ (list 0 0) (* inc (+ count 1)))))
	(setf count (+ count 1))
	(setf y (+ y (* inc count)))
	(send self :add-points (list x y))
	(send self :add-lines (list x y))
	(send self :range 1 -1 (max y))
	(send self :add-lines (list 
			       (list pop-mean pop-mean) (list -1 (max y))))
	(send self :adjust-to-data)))))



    










