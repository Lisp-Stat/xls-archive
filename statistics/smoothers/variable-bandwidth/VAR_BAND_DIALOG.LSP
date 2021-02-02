(defun var-band-dialog ()
  (let* ((sample (chisq-generator 10 1000))
	 (xx (rseq 2 20 50))
	 (i 0) (a1-l ()) (p1-l ())
	 (dil-1 (initial-dialog)) (k-1 (first dil-1)) (d-1 (second dil-1))
	 (pic (cond ((= d-1 0) (plot-lines xx (chisq-fail xx 10)))
		    (t (plot-lines xx (log-failure xx)))));(tmp (setf a234 (print (chisq-fail xx 10))))
	 (ker-sym (find-kernel k-1))
	 (s0 (send text-item-proto :new "Graphical Display"))
	 (s1 (send text-item-proto :new "Choose the bandwidths for the pilot and the
adaptive estimator. Then click Start to start
the simulation or cancel to quit"))
	 (p-band (send text-item-proto :new "Pilot estimate bandwidth:"))
	 (a-band (send text-item-proto :new "Adaptive estimate bandwidth:"))
	 (p1-scroll (send interval-scroll-item-proto :new  '(.1 2.5)
			  :text-item p-band))
	 (a1-scroll (send interval-scroll-item-proto :new  '(.1 2.5)
			  :text-item a-band :selection))
	 (s2 (send button-item-proto
		   :new "Start Simulation"
		   :action #'(lambda ()
			       (let* ((arg-1
				       (lambda-tilde sample xx 
						     (send p1-scroll :value)
						     (send a1-scroll :value)
						     :type ker-sym))(tmp (setf a234 (print arg-1)))
				      (tmp (setf a (d-or-n pic xx arg-1 i)))
				      (tmp (setf v1 (send p1-scroll :value))) 
				      (tmp (setf v2 (send a1-scroll :value)))
				      (tmp (setf p1-l (cons v1 p1-l)))
				      (tmp (setf a1-l (cons v2 a1-l)))
				      (tmp (send pic :use-color t))
				      (nl (send pic :num-lines))
				      (tmp (setf i (+ i 1)))
				      (tmp (setf ss (clr i nl pic)))	       
				      (tmp (setf d (send pic
							 :redraw-content))))))))
	 (Cancel (send button-item-proto :new "Cancel"
		       :action 
		       #'(lambda ()
			   (let ((dialog (send cancel :dialog))
				 (a-1 (end-msg p1-l a1-l i)))
			     (send dialog :modal-dialog-return
				   (message-dialog a-1))))))
	 (s-dialog (send modal-dialog-proto :new
			 (do-layout s0 s1 p-band p1-scroll a-band
				    a1-scroll s2 cancel))))
    (send s-dialog :modal-dialog )))

(defun do-layout (s0 s1 p-band p1-scroll a-band a1-scroll s2 cancel)
  (list s0
	(list 
	 (list 
	  (list s1)
	  (list p-band p1-scroll )
	  (list a-band a1-scroll)
	  ) )
	(list s2  Cancel)))

(defun find-kernel (x)
  "The default is set to biweight"
  (cond ((= x 0) 'b)
	((= x 1) 'e)
	((= x 2) 'g)
	((= x 3) 'r)
	((= x 4) 't)
	(t 'b)))

(defun collect-values (kernel-items distrib-items)
  (list  kernel-items  distrib-items))


(defun initial-dialog ()
  (let* ((kernels-label (send text-item-proto :new "Kernels"))
	 (distrib-label (send text-item-proto :new "Distributions"))
	 (kernel-items (send choice-item-proto :new 
			     (list "Biweight" "Epanechnikov" "Gaussian"
				   "Rectangular" "Triangular")
			   :value 1  )) ;edo eixe :value 1 alla to evgala
	 (distrib-items (send choice-item-proto :new 
			      (list "Chi square" "Log normal")
			      ));edo eixe :value 1 alla to evgala
         (ok (send modal-button-proto :new "OK" :action 
		 #'(lambda () (setf ai-1 (list (send kernel-items :value)
					       (send distrib-items :value))))))
	 (cancel (send button-item-proto :new "Cancel" 
		       :action #'(lambda () 
				   (let ((dialog (send cancel :dialog)))
				     (send dialog :modal-dialog-return nil)))))
	 (in-dialog (send modal-dialog-proto :new 
			  (list 
			   (list 
			   (list kernels-label kernel-items)
			   (list distrib-label distrib-items))
			   (list ok cancel))))
	 (dial (send in-dialog :modal-dialog)))
  ai-1))

(defun end-msg (a1-l p1-l i)
  (format nil "the bandwidths of the estimates are: ~%
               adaptive estimate: ~,3g ~%
               pilot estimate:    ~,3g ~%
               colors: (in order of appearance):
               ~,3g" a1-l p1-l (select (color-symbols) (iseq i))))

(defun d-or-n (pic xx arg i)
  (if (equal (integerp (/ i 2)) T)  (send pic :add-lines xx arg :type 'dashed)
    (send pic :add-lines xx arg)))

(defun clr (i nl pic)
  (send pic :linestart-color (iseq (* i (/ nl (+ i 1))) (- nl 1 ))
	(select (color-symbols) i)))
	
	;; TEST RUN: (var-band-dialog)