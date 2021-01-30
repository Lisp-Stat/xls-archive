#|

ex-mdlm.lsp

A more substantial example to build and compute a big 
multivariate DLM model.
See the BAYES-LIN documentation for an explanation.

|#

;; First make sure BAYES-LIN is loaded
(require "bayeslin")

;; Define some constants
(def mydata (read-data-columns "ex-mdlm.dat"))
(def data (make-array '(6 35) :initial-contents mydata))

(def v
#2a(
    (  2420.36       387.330       20.3907       165.274       44.5645       58.6081    )
    (  387.330       263.850       3.85480       71.5054       23.4794       3.26543    )
    (  20.3907       3.85480       30.7910       3.58376       1.25836       5.98943    )
    (  165.274       71.5054       3.58376       139.715       23.1193       11.3268    )
    (  44.5645       23.4794       1.25836       23.1193       50.0087       4.78345    )
    (  58.6081       3.26543       5.98943       11.3268       4.78345       44.2135    )
   )
)

(def w
#2a(
    (  1112.49       272.473       22.5176       66.4472       31.5611       27.8440    )
    (  272.473       195.499       11.5298       30.0701       18.5065       15.3737    )
    (  22.5176       11.5298       29.6411       5.53985       4.66797       6.27591    )
    (  66.4472       30.0701       5.53985       78.9076       14.0421       8.03411    )
    (  31.5611       18.5065       4.66797       14.0421       40.5035       7.32038    )
    (  27.8440       15.3737       6.27591       8.03411       7.32038       32.9728    )
   )
)


(def e0 (coerce (repeat 50 6) 'array))
(def ee0 (coerce (append (repeat 50 12) (repeat 0 6)) 'array))
(def w0 (diagonal (repeat 900 6)))
(def zero66 (diagonal (repeat 0 6)))

;; Create the "Type B" cliques
(format t "~&Creating the B cliques")
(dolist (i (iseq 1 35))
	 (create-tree-node
	  (intern (format nil "b~a" i))
	  (list (intern (format nil "x1.~a" i))
		(intern (format nil "x2.~a" i))
		(intern (format nil "x3.~a" i))
		(intern (format nil "x4.~a" i))
		(intern (format nil "x5.~a" i))
		(intern (format nil "x6.~a" i))
		(intern (format nil "theta1.~a" i))
		(intern (format nil "theta2.~a" i))
		(intern (format nil "theta3.~a" i))
		(intern (format nil "theta4.~a" i))
		(intern (format nil "theta5.~a" i))
		(intern (format nil "theta6.~a" i))
		(intern (format nil "nu1.~a" i))
		(intern (format nil "nu2.~a" i))
		(intern (format nil "nu3.~a" i))
		(intern (format nil "nu4.~a" i))
		(intern (format nil "nu5.~a" i))
		(intern (format nil "nu6.~a" i))
		)
	  ee0
	  (bind-rows (bind-columns
		      (+ w0 (* w i) v)
		      (+ w0 (* w i))
		      v)
		     (bind-columns
		      (+ w0 (* w i))
		      (+ w0 (* w i))
		      zero66)
		     (bind-columns
		      v
		      zero66
		      v)
		     )
	  (if (= i 35)
	      (list (intern (format nil "a~a" i)))
	    (list (intern (format nil "a~a" i))
		  (intern (format nil "a~a" (+ i 1))))
	    )

))

;; Create the "Type A" cliques
(format t "~&Creating the A cliques")
(dolist (i (iseq 1 35))
	 (create-tree-node
	  (intern (format nil "a~a" i))
	  (list (intern (format nil "theta1.~a" (- i 1)))
	        (intern (format nil "theta2.~a" (- i 1)))
	        (intern (format nil "theta3.~a" (- i 1)))
	        (intern (format nil "theta4.~a" (- i 1)))
	        (intern (format nil "theta5.~a" (- i 1)))
	        (intern (format nil "theta6.~a" (- i 1)))
	        (intern (format nil "theta1.~a" i))
	        (intern (format nil "theta2.~a" i))
	        (intern (format nil "theta3.~a" i))
	        (intern (format nil "theta4.~a" i))
	        (intern (format nil "theta5.~a" i))
	        (intern (format nil "theta6.~a" i))
		(intern (format nil "omega1.~a" i))
		(intern (format nil "omega2.~a" i))
		(intern (format nil "omega3.~a" i))
		(intern (format nil "omega4.~a" i))
		(intern (format nil "omega5.~a" i))
		(intern (format nil "omega6.~a" i))
		)
	  ee0
	  (bind-rows (bind-columns
		      (+ w0 (* w (- i 1)))
		      (+ w0 (* w (- i 1)))
		      zero66)
		     (bind-columns
		      (+ w0 (* w (- i 1)))
		      (+ w0 (* w i))
		      w)
		     (bind-columns
		      zero66
		      w
		      w)
		     )
	  (if (= i 1)
	      (list (intern (format nil "b~a" i)))
	    (list (intern (format nil "b~a" i))
		  (intern (format nil "b~a" (- i 1))))
	    )
))


;; Now create the moral graph nodes
(format t "~&Creating the moral graph nodes")
;; number of moral nodes to create and plot
(def plotnum 8)
(create-moral-node (intern "theta.0")
		   (list (intern "theta1.0")
			 (intern "theta2.0")
			 (intern "theta3.0")
			 (intern "theta4.0")
			 (intern "theta5.0")
			 (intern "theta6.0"))
		   (intern "a1")
		   "Theta(0)"
		   (list (intern "theta.1")
			 (intern "omega.1"))
		   )
(send (symbol-value (intern "theta.0")) :location (list (/ 1 (+ plotnum 2)) 0.4))
(dolist (i (iseq 1 plotnum))
  ;; create the theta node
  (create-moral-node (intern (format nil "theta.~a" i))
		    (list (intern (format nil "theta1.~a" i))
			  (intern (format nil "theta2.~a" i))
			  (intern (format nil "theta3.~a" i))
			  (intern (format nil "theta4.~a" i))
			  (intern (format nil "theta5.~a" i))
			  (intern (format nil "theta6.~a" i)))
		    (intern (format nil "b~a" i))
		    (format nil "Theta(~a)" i)
		    (if (< i plotnum)
			(list (intern (format nil "omega.~a" i))
			      (intern (format nil "nu.~a" i))
			      (intern (format nil "x.~a" i))
			      (intern (format nil "theta.~a" (- i 1)))
			      (intern (format nil "theta.~a" (+ i 1)))
			      (intern (format nil "omega.~a" (+ i 1)))
			      )
		      (list (intern (format nil "omega.~a" i))
			    (intern (format nil "nu.~a" i))
			    (intern (format nil "x.~a" i))
			    (intern (format nil "theta.~a" (- i 1)))
			    )
		      )
		    )
  (send (symbol-value (intern (format nil "theta.~a" i))) :location
	(list (* (+ i 1) (/ 1 (+ plotnum 2))) 0.4)
	)
  ;; create the omega node
  (create-moral-node (intern (format nil "omega.~a" i))
		     (list (intern (format nil "omega1.~a" i))
			   (intern (format nil "omega2.~a" i))
			   (intern (format nil "omega3.~a" i))
			   (intern (format nil "omega4.~a" i))
			   (intern (format nil "omega5.~a" i))
			   (intern (format nil "omega6.~a" i)))
		     (intern (format nil "a~a" i))
		     (format nil "Omega(~a)" i)
		     (list (intern (format nil "theta.~a" i))
			   (intern (format nil "theta.~a" (- i 1))))
		     )
  (send (symbol-value (intern (format nil "omega.~a" i))) :location
	(list (* (+ i 0.5) (/ 1 (+ plotnum 2))) 0.2)
	)
  ;; create the nu node
  (create-moral-node (intern (format nil "nu.~a" i))
		     (list (intern (format nil "nu1.~a" i))
			   (intern (format nil "nu2.~a" i))
			   (intern (format nil "nu3.~a" i))
			   (intern (format nil "nu4.~a" i))
			   (intern (format nil "nu5.~a" i))
			   (intern (format nil "nu6.~a" i)))
		     (intern (format nil "b~a" i))
		     (format nil "Nu(~a)" i)
		     (list (intern (format nil "theta.~a" i))
			   (intern (format nil "x.~a" i)))
		     )
  (send (symbol-value (intern (format nil "nu.~a" i))) :location
	(list (* (+ i 0.5) (/ 1 (+ plotnum 2))) 0.6)
	)
  ;; create the x node
  (create-moral-node (intern (format nil "x.~a" i))
		     (list (intern (format nil "x1.~a" i))
			   (intern (format nil "x2.~a" i))
			   (intern (format nil "x3.~a" i))
			   (intern (format nil "x4.~a" i))
			   (intern (format nil "x5.~a" i))
			   (intern (format nil "x6.~a" i)))
		     (intern (format nil "b~a" i))
		     (format nil "X(~a)" i)
		     (list (intern (format nil "theta.~a" i))
			   (intern (format nil "nu.~a" i)))
		     )
  (send (symbol-value (intern (format nil "x.~a" i))) :location
	(list (* (+ i 1) (/ 1 (+ plotnum 2))) 0.8)
	)
)

;; create the plot
(create-moral-plot 'myplot)
(create-global-moral-plot 'myplot2)


;; Sequentially introduce the data
(format t "~&Adding some info")
;(dolist (i (iseq 1 35))
(dolist (i (iseq 1 6))
(format t "~&Data for week ~a" i)
(send (symbol-value (intern (format nil "b~a" i))) :observe
      (list 
            (intern (format nil "x1.~a" i))
            (intern (format nil "x2.~a" i))
            (intern (format nil "x3.~a" i))
            (intern (format nil "x4.~a" i))
            (intern (format nil "x5.~a" i))
            (intern (format nil "x6.~a" i))
	    )
      (select (column-list data) (- i 1))
      )
(send myplot :record)
(send (symbol-value (intern (format nil "b~a" i))) :absorb)
)

; (break)

;; Now extract some additional information
(format t "~&Extracting the info")
(def nu-list nil)
(def omega-list nil)
(dolist (i (iseq 1 35))
	(setf nu-list (append nu-list (list
	   (send (symbol-value (intern (format nil "b~a" i))) :ex
	      (list
	       (intern (format nil "nu1.~a" i))
	       (intern (format nil "nu2.~a" i))
	       (intern (format nil "nu3.~a" i))
	       (intern (format nil "nu4.~a" i))
	       (intern (format nil "nu5.~a" i))
	       (intern (format nil "nu6.~a" i))
		)))))	
	(setf omega-list (append omega-list (list
	   (send (symbol-value (intern (format nil "a~a" i))) :ex
	      (list
	       (intern (format nil "omega1.~a" i))
	       (intern (format nil "omega2.~a" i))
	       (intern (format nil "omega3.~a" i))
	       (intern (format nil "omega4.~a" i))
	       (intern (format nil "omega5.~a" i))
	       (intern (format nil "omega6.~a" i))
		)))))				       
)

;; Now analyse and plot the residuals
(def nu-mat (apply #'bind-columns nu-list))
(def omega-mat (apply #'bind-columns omega-list))
(def nu-pepsi
     (coerce (select (row-list nu-mat) 3) 'list))
(def omega-pepsi
     (coerce (select (row-list omega-mat) 3) 'list))
(def nu-plot
     (plot-lines (iseq 1 35) nu-pepsi)
     )
(send nu-plot :title "Nu")
(def omega-plot
     (plot-lines (iseq 1 35) omega-pepsi)
     )
(send omega-plot :title "Omega")

(terpri)

;; end

