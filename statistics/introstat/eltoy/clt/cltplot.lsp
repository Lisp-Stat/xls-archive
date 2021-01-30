;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This creates a menu of distribution display tools for a variety of
;;; distributions.  It is started by the function (launch-dist-toy).
;;;

(require :el-dist-lib (strcat ElToY-directory "Dist-Lib" *d-sep*  "Dist-lib.lsp"))
(require :el-clt-dialog (strcat ElToY-directory "clt" *d-sep*  "cltdialog.lsp"))
(require :el-clt-families (strcat ElToY-directory "clt" *d-sep*  "clt-families.lsp"))

(def x-normal (rseq -4 4 100))
(def y-normal (normal-dens x-normal))




(defun make-clt-demo (&key (model #'normal-rand) (n 500) 
			   (name model))
  (let* ((h-data (apply model (list n)))
	(h (histogram h-data :size '(500 300)
		      :title (format nil "CLT Demonstration: ~S "
				name)     
		      )))
    (send h :add-slot 'h-data)
    (send h :slot-value 'h-data h-data)
    (send h :add-slot 'sample-size)
    (send h :slot-value 'sample-size n)
    (send h :add-slot 'reps)
    (send h :slot-value 'reps 1)
    (send h :num-bins 25)
    (send h :add-slot 'model)
    (send h :slot-value 'model model)
    (send h :add-slot 'qqplot)
    (send h :slot-value 'qqplot nil)
    (send h :add-slot 'explanation)
    (send h :slot-value 'explanation (send clt-dialog-proto :new h
					   :name name))
    (let ((q-item (send menu-item-proto :new "Add QQ-plot"
			:action #'(lambda () (clt-demo-add-qqplot h)))))
      (send h :add-slot 'q-item)
      (send h :slot-value 'q-item q-item)
      (send (send h :menu) :append-items q-item))
    (let ((mn (mean h-data))
	  (std (standard-deviation h-data)))
      (apply #'send h :range 0 (+ (* '(-4 4) std) mn))
      (send h :add-lines (list (+ (* x-normal std) mn) (/ y-normal std))))
    (send h :add-method :add-h-data #'clt-add-data)
    (send h :add-method :more #'clt-demo-more)
    h
    ))



(defun clt-add-data (h new-data)
  (let* ((h-data (+ (send h :slot-value 'h-data) new-data))
	 (mn (mean h-data))
	 (std (standard-deviation h-data)))
    (send h :slot-value 'reps (1+ (send h :slot-value 'reps)))
    (send h :slot-value 'h-data h-data)
    (apply #'send h :range 0 (+ (* '(-4 4) std) mn))
    (send h :clear :draw nil)
;    (send h :clear-points :draw t)
    (send h :add-points h-data :draw nil)
;    (send h :clear-lines :draw t)
    (send h :adjust-to-data :draw nil)
    (send h :num-bins 25 :draw t)
    (send h :add-lines (list (+ (* x-normal std) mn) (/ y-normal std))
	    :draw t)
    (when (send h :slot-value 'qqplot)
	  (send (send h :slot-value 'qqplot) :clear-points :draw nil)
	  (send (send h :slot-value 'qqplot) :add-points
		(list (send h :slot-value 'q-points) (sort-data h-data))
		:draw nil)
	  (send (send h :slot-value 'qqplot) :adjust-to-data :draw t))
    (send (send h :slot-value 'explanation) :add-points)
    ))

(defun clt-demo-add-qqplot (h)
  (send (send h :slot-value 'q-item) :enabled nil)
  (send h :add-slot 'q-points)
  (let* ((n (length (send h :slot-value 'h-data)))
	 (q-points (normal-quant (/ (- (iseq 1 n) .5) n))))
    (send h :slot-value 'q-points q-points)
    (send h :slot-value 'qqplot
	  (plot-points q-points (sort-data (send h :slot-value 'h-data))
		       :title (format nil "Quantile plot for ~A"
				      (send h :title))
		       :variable-labels '("Quantiles of Standard Normal"
					  "Data")))
    ))


(defun clt-demo-more (h &key (reps 10)
			&aux (n (send h :slot-value 'sample-size)))
  (dotimes (i reps)
	   (send h :add-h-data (apply (send h :slot-value 'model)
				    (list n)))))


(defvar *clt-toy* (send menu-proto :new "CLT Toy")
  "Menu of available Central Limit Theorem toys.")


(send *clt-toy* :append-items
      (send menu-item-proto :new "---Distribution---" 
	    :function #'sysbeep))


(defun toggle-clt-toy (family)
; (declare (type Family family))
  "Toggles presence/absense of dist-toy for distribuition <family>"
  (let ((clt-toy (get '*clt-toy* (send family :name))))
    (if clt-toy (send clt-toy :more)
      (progn
	(putprop '*clt-toy*
		 (make-clt-demo :model #'(lambda (n) (send family :rand n))
				:name family)
		 (send family :name))))))


(defun launch-clt-toy ()
  (mapcar #'add-clt-toy *CLT-Families*)
  (send *clt-toy* :install))



(defun add-clt-toy (family)
  (send *clt-toy* :append-items
	(send menu-item-proto :new (symbol-name (send family :name))
	      :action (function (lambda ()
				  (toggle-clt-toy family))))))

(launch-clt-toy)


(new-provide :el-clt)


