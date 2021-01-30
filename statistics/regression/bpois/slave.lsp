(require "slave-opts")
(require "kde")

(when (screen-has-color)
      (send kde-proto :use-color t)
      (send kde-proto :back-color 'blue)
      (send kde-proto :draw-color 'yellow))

(defproto slave-proto '(master param-label param-index superimpose
			       stats-labels 
			       stats-messages
			       stats-print-formats)
  () kde-proto 
  "The Slave prototype.  Under control of the master prototype; not
usually manipulated by the user.")

(defmeth slave-proto :how-many-chains ()
  (send (send self :master) :how-many-chains))

;;;
;;; Override the data method for kde-proto.
;;;

(send kde-proto :delete-method :data)

(defmeth slave-proto :data ()
  "Method args: None
Retrieves the data."
  (send (send self :master) :data (send self :param-index)))

;;;
;;; Override the weights method for kde-proto for our use.
;;;

(send kde-proto :delete-method :weights)

(defmeth slave-proto :weights ()
  (send (send self :master) :weights))

(send kde-proto :delete-method :n)

(defmeth slave-proto :n ()
  (send (send self :master) :n))

(defmeth slave-proto :p ()
  (send (send self :master) :p))

(send kde-proto :delete-method :mean)
(send kde-proto :delete-method :stddev)

(require "slave-stats")

(defmeth slave-proto :isnew (master param-index
				    &key label
				    (superimpose nil)
				    (create-window t)
				    (show-window nil))
  "Method args: (master data param-index 
                        &key label (create-window t)
                         (show-window t))
Creates an instance of the Slave prototype."
  (setf (slot-value 'master) master)
  (setf (slot-value 'param-index) param-index)
  (setf (slot-value 'superimpose) superimpose)
  (setf (slot-value 'param-label) 
	(if label
	    label
	  (format nil "Beta(~d)" param-index)))

  (if create-window
      (send self :create-window))
  (unless show-window
	  (send self :hide-window))
  self)

(defmeth slave-proto :master ()
  "Method args: ()
Retrieves the master object that controls the slave."
    (slot-value 'master))

(send kde-proto :delete-method :redraw-content)

(defmeth slave-proto :redraw-content ()
  (when (not (send (send self :master) :slot-value 'lazy))
	(case (send self :superimpose)
	      (nil (send self :clear-lines :draw nil)
		   (send self :add-lines (send self :kde) :draw nil)
		   (send self :adjust-to-data :draw nil)
		   (if (slot-value 'stats-labels)
		       (send self :redraw-stats))
		   (call-next-method))
	      (t (send self :add-lines (send self :kde))
		 (call-next-method)))))

(defmeth slave-proto :superimpose ()
  (slot-value 'superimpose))

(defmeth slave-proto :choose-toggle-superimpose ()
  (setf (slot-value 'superimpose) 
	(not (slot-value 'superimpose)))
  (send self :clear :draw nil)
  (send self :redraw))

(defmeth slave-proto :stats-labels ()
  (slot-value 'stats-labels))

(defmeth slave-proto :stats-messages ()
  (slot-value 'stats-messages))

(defmeth slave-proto :stats-print-formats ()
  (slot-value 'stats-print-formats))

(defmeth slave-proto :redraw-stats ()
  (cond 
   ((= (send self :how-many-chains) 1)
    (let* ((ascent (send self :text-ascent))
	   (descent (send self :text-descent))
	   (skip (+ ascent descent))
	   (cw (send self :text-width "Current"))
	   (ow (send self :text-width "Original"))
	   (em (send self :text-width "m"))
	   (en (send self :text-width "n"))
	   (can-wid (send self :canvas-width))
	   (y ascent)
	   (snames (slot-value 'stats-labels))
	   (nw (max (mapcar #'(lambda(x) (send self :text-width x)) snames)))
	   (scvals (mapcar #'(lambda(x) (send self x)) (slot-value 'stats-messages)))
	   (sovals (slot-value 'stats-org-vals))
	   (val-wid (send self :text-width
			  (make-string (max (slot-value 'stats-print-formats)))))
	   (fw1 (max val-wid cw))
	   (x2 (- can-wid fw1 em))
	   (fw2 (max val-wid ow))
	   (x1 ( - x2 fw2 em en nw)))
      (let ((tot-wid (+ fw1 em fw2 em en nw)))
	(send self :draw-text "Statistics" (- can-wid (round (/ tot-wid 2))) y 1 0))
      (setf y (+ y skip))
      (send self :draw-text "Original" 	(- x2 em) y 2 0)
      (send self :draw-text "Current" (- can-wid em) y 2 0)
      (dotimes (i (length (slot-value 'stats-labels)))
	       (let* ((s (select snames i))
		      (sc (select scvals i))
		      (so (select sovals i))
		      (fl (select (slot-value 'stats-print-formats) i)))
		 (setf y (+ y skip))
		 (send self :draw-text s x1 y 0 0)
		 (send self :draw-text 
		       (format nil "~v,vf" (select fl 0) (select fl 1) so)
		       (- x2 em) y 2 0)
		 (send self :draw-text 
		       (format nil "~v,vf" (select fl 0) (select fl 1) sc)
		       (- can-wid em) y 2 0)))))
   (t 
    (let* ((ascent (send self :text-ascent))   ; ascent of text.
	   (descent (send self :text-descent)) ; descent of text
	   (skip (+ ascent descent))           ; line-skip amount.
	   (em (send self :text-width "m"))   
	   (en (send self :text-width "n"))
	   (can-wid (send self :canvas-width))
	   (snames (slot-value 'stats-labels))
	   (svals (mapcar #'(lambda(x) (send self x)) (slot-value 'stats-messages)))
	   (val-wid (send self :text-width 
			  (make-string (max (slot-value 'stats-print-formats))))) ; Max width of vals.
	   (nw (max (mapcar #'(lambda(x) (send self :text-width x)) snames))) ; Max width of names.
	   (y ascent)) ; y-coordinate.
      (let ((tot-wid (+ nw en val-wid em)))
	(send self :draw-text "Statistics" (- can-wid (round (/ tot-wid 2))) y 1 0))
      (dotimes (i (length (slot-value 'stats-labels)))
	(let* ((fl (select (slot-value 'stats-print-formats) i))
	       (sv-str (format nil "~v,vf" 
			       (select fl 0) (select fl 1) (select svals i))))
	  (setf y (+ y skip))
	  (send self :draw-text (select (slot-value 'stats-labels) i) 
		(- can-wid em en (send self :text-width sv-str))
		y 2 0)
	  (send self :draw-text sv-str (- can-wid em) y 2 0)))))))

(defmeth slave-proto :param-label ()
  (slot-value 'param-label))

(defmeth slave-proto :param-index ()
  (slot-value 'param-index))

(defmeth slave-proto :identifier ()
  (concatenate 'string 
	       (send (send self :master) :identifier)
	       "-"
	       (send self :param-label)))

(defmeth slave-proto :create-window ()
  (call-method kde-proto :isnew (send self :data) :go-away nil)
  (pause 60)
  (when (definedp  '*stats-to-show*)
	(setf (slot-value 'stats-labels) 
	      (mapcar #'(lambda(x) (string-upcase 
				    (concatenate 'string x ":")
				    :end 1)) 
		      *stats-to-show*))
	(setf (slot-value 'stats-messages)
	      (mapcar #'(lambda(x) (str-to-mes x)) *stats-to-show*))
	(setf (slot-value 'stats-print-formats) 
	      (if (definedp '*stats-print-formats*) 
		  (copy-list *stats-print-formats*)
		(mapcar #'(lambda(x) *default-stat-print-format*)
			*stats-to-show*)))
	(cond 
	 ((= (send self :how-many-chains) 1)
	  (let ((skip (+ (send self :text-ascent) (send self :text-descent))))
	    (send self :margin 0 (+ (* (length *stats-to-show*) skip)
				    skip skip) 0 0))
	  (let ((fl (slot-value 'stats-print-formats))
		(s (mapcar #'(lambda (x) (send self x t)) 
			   (slot-value 'stats-messages))))
	    (send self :add-slot 'stats-org-vals
		  (mapcar #'(lambda (x y) 
			      (format nil "~v,vf" (select x 0) (select x 1) y))
			  fl s))))
	 (t (let ((skip (+ (send self :text-ascent) 
			   (send self :text-descent))))
	      (send self :margin 0 (+ (* (length *stats-to-show*) skip)
				      skip) 0 0))))
	(let ((ovl (send self :opt-overlay)))
	  (send ovl :add-opt (list "Toggle-superimpose"))
	  (send ovl :add-opt (mapcar #'(lambda (x) 
					 (concatenate 'string x
						      "-print-format"))
				     *stats-to-show*)))
	(send self :title
	      (concatenate 'string (send self :identifier)
			   ": Marginal Posterior Density"))))









