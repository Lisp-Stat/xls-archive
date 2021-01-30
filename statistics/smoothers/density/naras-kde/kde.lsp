;;(debug)
;;
;;

(require "kde-opts")
(require "dyn-opts")
(require "kde-ovls")
(require "utils")
(provide "kde")

(defproto kde-proto '(kernel-type kernel-window-width kernel-points 
				  kernel-window-width-slider-interval
				  kernel-window-width-slider-stops
				  data) 
  () scatterplot-proto
  "The Kernel density estimate prototype. Kernel is a slot specifying
type of kernel, kernel-window-width is the bin-width used for the kde,
kernel-points specified the number of points at which the kde is
evaluated, kernel-window-width-slider-interval is an interval specifying
the range of the bin-width, kernel-window-width-slider-stops is the 
number of slider-stops in that range. Data is the data.")

(defmeth kde-proto :isnew (data &key (go-away t))
  "Method args: data &rest args
Creates a new instance of the kde-proto object."
  (setf (slot-value 'data) (float data))
  (let ((d (combine (send self :data))))    ; Made this change for the RM scheme.
    (setf (slot-value 'kernel-window-width)
	  (/ (- (max d) (min d))
	     (+ 1 (log (send self :n))))))
  (setf (slot-value 'kernel-type) *default-kernel*)
  (setf (slot-value 'kernel-points) *default-kernel-no-of-pts*)
  (setf (slot-value 'kernel-window-width-slider-interval) 
	*default-kernel-window-width-slider-interval*)
  (setf (slot-value 'kernel-window-width-slider-stops) 
	*default-kernel-window-width-slider-stops*)
  (call-next-method 2 :go-away go-away)
  (let* ((ascent (send self :text-ascent))
	 (descent (send self :text-descent))
         (marg-ht (round (* 1.5 (+ ascent descent))))
	 (x ascent)
	 (y (round (* 1.5 ascent)))
         (wid ascent)
	 (ht (round (* 1.5 wid)))
	 (opt-item (send kde-overlay-proto :new
			 "Options"
			 (list x y wid (+ x ht))
			 ':show-options)))
    (send self :margin 0 marg-ht 0 0)
    (send self :add-overlay opt-item)
    (send opt-item :add-slot 'opts '("Kernel Type" 
				     "Kernel Window Width" 
				     "Kernel points"))
    (defmeth opt-item :opts () (slot-value 'opts))
    (defmeth opt-item :add-opt (arg)
      (let* ((tmp (mapcar #'(lambda(x) (string-left-trim " " x)) arg))
	     (tmp (mapcar #'(lambda(x) (string-right-trim " " x)) tmp)))
	(setf (slot-value 'opts) (append (slot-value 'opts) tmp))))
    (defmeth opt-item :del-opt (olist)
      (let ((tmp (slot-value 'opts)))
	(dolist (x olist)
		(setf tmp (remove x tmp)))
	(setf (slot-value 'opts) tmp)))
    (defmeth opt-item :opt-messages ()
      (mapcar #'make-choose-mes (slot-value 'opts)))))

(defmeth kde-proto :data ()
  "Method args: none
Accessor method for slot 'data."
  (slot-value 'data))

(defmeth kde-proto :n ()
  "Method args: none
Returns the length of the data list."
  (length (send self :data)))

(defmeth kde-proto :weights ()
  (if (send self :has-slot 'weights)
      (slot-value 'weights)
    (let ((n (send self :n)))
      (repeat (/ 1 n) n))))

(defmeth kde-proto :kernel-window-width-slider-interval (&optional interval)
  "Method args: &optional interval
Accessor method for the slot 'kernel-window-width-slider-interval"
  (if interval
      (setf (slot-value 'kernel-window-width-slider-interval) interval)
    (slot-value 'kernel-window-width-slider-interval)))

(defmeth kde-proto :kernel-window-width-slider-stops (&optional num)
  "Method args: &optional num
Accessor method for the slot 'kernel-window-width-slider-stops."
  (if num
      (setf (slot-value 'kernel-window-width-slider-stops) num)
    (slot-value 'kernel-window-width-slider-stops)))

(defmeth kde-proto :kernel-window-width (&optional width)
  "Method args: &optional width
Accessor method for the slot 'kernel-window-width."
  (if width
      (progn 
	(setf (slot-value 'kernel-window-width) (float width))
	(send self :redraw-content))
    (slot-value 'kernel-window-width)))

(defmeth kde-proto :kernel-points (&optional num)
  "Method args: &optional num
Accessor method for the slot 'kernel-points."
  (if num
      (progn 
	(setf (slot-value 'kernel-points) num)
	(send self :redraw-content))
    (slot-value 'kernel-points)))

(defmeth kde-proto :kernel-type (&optional type)
  "Method args: &optional type
Accessor method for the slot 'kernel-type."
  (if type
      (progn 
	(setf (slot-value 'kernel-type) type)
	(send self :redraw-content))
    (slot-value 'kernel-type)))

(defmeth kde-proto :choose-kernel-window-width ()
  "Method args: none
Lets the user change Kernel bin width using a slider"
  (let* ((ti (send text-item-proto :new "Kernel Bin Width"))
	 (vi (send text-item-proto :new "" :text-length 5))
	 (slider (send interval-scroll-item-proto :new  
		       (send self :kernel-window-width-slider-interval)
		       :points (send self :kernel-window-width-slider-stops)
		       :action #'(lambda (x)
				   (send self :kernel-window-width x))
		       :text-item vi))
	 (ssize (send slider :slot-value 'size))
	 (ii (send button-item-proto :new "Change Interval"
		   :action #'(lambda ()
			       (let* ((d (send ii :dialog))
				      (int (send self :kernel-window-width-slider-interval))
				      (nint (get-value-dialog "New Interval?"
							      :initial 
							      `(list ,(select int 0) ,
								     (select int 1)))))
				 (when nint
				       (send d :close)
				       (send self :kernel-window-width-slider-interval
					     (select nint 0))
				       (send self :choose-kernel-window-width))))))
	 (sti (send button-item-proto :new "Change No. of Stops"
		    :action #'(lambda ()
				(let* ((d (send sti :dialog))
				       (s (send self :kernel-window-width-slider-stops))
				       (ns (get-value-dialog "Number of stops?"
							     :initial s)))
				  (when ns
					(send d :close)
					(send self :kernel-window-width-slider-stops (select ns 0))
					(send self :choose-kernel-window-width)))))))
    (send slider :slot-value 'size (list (+ 10 (select (send ii :slot-value 'size) 0)
					    (select (send sti :slot-value 'size) 0))
					 (select ssize 1)))
    (send slider :value (send self :kernel-window-width))
    (send dialog-proto :new (list (list ii sti) (list ti vi)
				  slider) 
	  :title (concatenate 'string (send self :title)
			      ": Kernel Bin Width"))))

(defmeth kde-proto :choose-kernel-type ()
  "Method args: ()
Create menu item to allow user to choose a Kernel type."
  (let* ((types '("Bisquare" "Gaussian" "Triangle" "Uniform" 
		  "Epanechnikov"))
	 (users-choice (choose-item-dialog 
			"Kernel Type ?"
			types
			:title (send self :title)
			:initial (position 
				  (slot-value 'kernel-type)
				  (list 'B 'G 'T 'U 'E)))))
    (if users-choice 
	(send self :kernel-type 
	      (select (list 'B 'G 'T 'U 'E) users-choice)))))

(defmeth kde-proto :choose-kernel-points ()
  "Method args: none.
Lets the user choose the number of points at which the kernel is
evaluated using a dialog."
  (let ((response (get-value-dialog "No of Kernel Points?"
				    :initial (send self :kernel-points)
				    :title (send self :title))))
    (if response
	(send self :kernel-points (select response 0)))))

(defmeth kde-proto :show-options ()
  "Method args: none.
Allow the user to change options in a dialog box."
  (let* ((ovl (send self :opt-overlay))
	 (change (send text-item-proto :new "Double Click to change"))
	 (opts (send ovl :opts))
	 (acts (send ovl :opt-messages))
	 (l-item (send list-item-proto :new opts
		       :action #'(lambda (x) 
				   (if x 
				       (let* ((d (send l-item :dialog))
					      (ind (send l-item 
							 :selection))
					      (mes (select acts ind)))
					 (if (send self :has-method mes)
					     (send self mes))
					 (send d :modal-dialog-return nil))))))
	 (cancel (send modal-button-proto :new "Cancel"))
	 (diag (send modal-dialog-proto :new
		     (list change l-item cancel) 
		     :title (send self :title))))
    (send diag :modal-dialog)
    (send ovl :toggle-show)))

(if *have-dyn-load*
    (require "kde-dyn")
  (require "kde-ndyn"))
		     
(defmeth kde-proto :redraw-content ()
  (send self :clear-lines :draw nil)
  (send self :add-lines (send self :kde) :draw nil)
  (send self :adjust-to-data :draw nil)
  (call-next-method))

(defmeth kde-proto :opt-overlay ()
  "Method args: none
Returns the options overlay object."
  (select (last (slot-value 'overlays)) 0))

(defmeth kde-proto :overlays ()
  "Method args: none
Returns a list of overlays."
  (slot-value 'overlays))



