;;;;
;;;; norm.lsp                    B. Narasimhan
;;;;
;;;;  This is a little tool I use in my intro classes to show
;;;;  the normal dist function.

;;;; Load this file and type (norm-density) to get the demo.
;;;;

(provide "norm")

(require "statistics")
(require "graphics")
(require "help")

(def c (/ 1 (sqrt (* 2 pi))))
(def mu 0)
(def sigma 1)

(defun f (x)
  (* (/ c sigma)
     (exp (* -0.5 (^ (/ (- x mu) sigma) 2)))))

(defun norm-density ()
  (let* ((w (plot-function #'f -5 5))
	 (mu-label (send text-item-proto :new "Mean:"))
	 (mu-val (send text-item-proto :new "" :text-length 5))
	 (mu-scroll (send interval-scroll-item-proto :new
			  '(-3 3)
			  :text-item mu-val
			  :action
			  #'(lambda(x) 
			      (def mu x)
			      (send w :clear :draw nil)
			      (send w :add-function #'f
				    -3 3 :draw t))))
	 (s-label (send text-item-proto :new "Sigma:"))
	 (s-val (send text-item-proto :new "" :text-length 5))
	 (s-scroll (send interval-scroll-item-proto :new
			 '(0.5 4.5)
			 :text-item s-val
			 :action
			 #'(lambda(x) 
			     (def sigma x)
			     (send w :clear :draw nil)
			     (send w :add-function #'f
				   -5 5 :draw t))))
	 (dialog (send dialog-proto :new (list (list mu-label mu-val)
					       (list mu-scroll)
					       (list s-label s-val)
					       (list s-scroll)))))
    (send mu-scroll :value mu)
    (send s-scroll :value sigma)
    (send w :title "Normal Density")
    (send dialog :add-subordinate w)))











