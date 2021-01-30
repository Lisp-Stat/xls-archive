;;;;
;;;; ci2.lsp                    B. Narasimhan
;;;;
;;;;  This is a little tool I use in my intro classes to 
;;;;  demonstrate the effect of changing the confidence level
;;;;  and sample size on the length of the confidence interval.
;;;;  

;;;;  Load this file and type 
;;;;  (ci2-demo) to get the demonstration.
;;;;  A histogram of the population along with a plot of the
;;;;  confidence interval is shown.
;;;;  The dialog box controls the window.
;;;;  The Vertical line in the centre of the plot shows the
;;;;  true value of the mean.

(provide "ci2")

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

(defun ci2-demo ()
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
         (n-label (send text-item-proto :new "Sample Size:"))
         (n-val (send text-item-proto :new "" :text-length 5))
         (n-scroll (send sequence-scroll-item-proto :new
                         (iseq 0 100)
                         :text-item n-val
                         :action
                         #'(lambda(x) 
                             (def n x)
                             (send ci-plot :add-ci))))

         (c-label (send text-item-proto :new "Confidence:"))
         (c-val (send text-item-proto :new "" :text-length 5))
         (c-scroll (send interval-scroll-item-proto :new
                         '(0 1)
                         :text-item c-val
                         :action
                         #'(lambda(x) 
                             (def confidence x)
                             (send ci-plot :add-ci))))
	 
         (dialog (send dialog-proto :new (list (list n-label n-val)
                                               (list n-scroll)
                                               (list c-label c-val)
                                               (list c-scroll)))))

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
	(send self :adjust-to-data)))
    (send ci-plot :range 1 -1 1)
    (send ci-plot :add-lines (list (list pop-mean pop-mean) (list -1 1)))
    (send ci-plot :add-lines (list x y))
    (send dialog :add-subordinate pop-hist)
    (send dialog :add-subordinate ci-plot)
    (send n-scroll :value n)
    (send c-scroll :value confidence)))



    










