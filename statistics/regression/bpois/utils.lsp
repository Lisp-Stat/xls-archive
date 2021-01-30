(provide "utils")

(defun new-xlispstat ()
  (boundp '*package*))

(defun get-id ()
  (get-string-dialog 
   (format nil "Please enter a short descriptive name~%~
                for this data set.~%~
                Ex: CancerData")))

(defun get-m (id)
  (let ((m (select 
	    (get-value-dialog 
	     (format nil 
		     "How many Markov chains did you run~%~
                      for ~a?" id)
	     :initial 1) 0)))
    m))

(defun get-p (id)
  (let ((p (select 
	    (get-value-dialog 
	     (format nil 
		     "Please enter the number of~%~
                      parameters in the model for ~a~%~
                      Ex: 10" id)) 0)))
    p))

(defun collect-values (list)
  (mapcar #'(lambda(x) (list (send (select x 1) :text)
			     (send (select x 2) :text)
			     (send (select x 3) :text))) list))

(defun get-all-fields (id m &optional list)
  (let* ((t1 (send text-item-proto :new "Markov Chain"))
	 (t2 (send text-item-proto :new "Output File Name"))
	 (t3 (send text-item-proto :new "Estimated Constant"))
	 (t4 (send text-item-proto :new "Hyperparameters file"))
	 (w1 (select (send t1 :slot-value 'size) 0))
	 (w2 (select (send t2 :slot-value 'size) 0))
	 (w3 (select (send t3 :slot-value 'size) 0))
	 (w4 (select (send t4 :slot-value 'size) 0))
	 (hrlist (list
		  (send text-item-proto :new 
			"File containing Ranges for Hyperparameters")
		  (send edit-text-item-proto :new 
			(if list 
			    (select (last list) 0)
			  "")
			:text-length 15)))
	 (ilist ())
	 (cancel (send modal-button-proto :new "Exit"
		       :action #'exit))
	 (ok (send modal-button-proto :new "OK" 
		   :action 
		   #'(lambda() (append (collect-values ilist)
				       (list (send (select hrlist 1) :text)))))))
    (dotimes (i m)
	     (let ((label (send text-item-proto :new 
				(format nil "~10d" (1+ i))))
		   (efield (send edit-text-item-proto :new 
				 (if list
				     (select (select list i) 0)
				   "")
				 :text-length 10))
		   (cfield (send edit-text-item-proto :new 
				 (if list
				     (select (select list i) 1)
				   "")
				 :text-length 10))
		   (hfield (send edit-text-item-proto :new
				 (if list
				     (select (select list i) 2)
				   "")
				 :text-length 10)))
	       (send label :slot-value 'size 
		     (list w1 (select (send label :slot-value 'size) 1)))
	       (send efield :slot-value 'size 
		     (list w2 (select (send efield :slot-value 'size) 1)))
	       (send cfield :slot-value 'size 
		     (list w3 (select (send cfield :slot-value 'size) 1)))
	       (send hfield :slot-value 'size 
		     (list w4 (select (send hfield :slot-value 'size) 1)))
	       (setf ilist (append ilist 
				   (list (list label efield cfield hfield))))))

    (send (send modal-dialog-proto :new  
		(append (list 
			 (send text-item-proto :new
			       (format nil "Please complete all fields ~
                                      for ~a and click the OK button.~%" id))
			 (list t1 t2 t3 t4))
			ilist 
			(list hrlist)
			(list (list ok cancel)))) :modal-dialog)))
  

(defun arrange (list &optional cols)
  "Method args: (list &optional cols)
Chops up a list into sublists of cols items each and
returns a list of lists. If cols is not given, it is 
floor of sqrt of length of list."
  (let ((n (length list)))
    (unless cols
	    (setf cols (floor (sqrt n))))
    (if (< n cols)
	list
      (let* ((q (floor (/ n cols)))
	     (tmp (mapcar #'(lambda(x) 
			      (select list (+ (* x cols) (iseq cols))))
			  (iseq q))))
	(if (= n (* q cols))
	    tmp
	  (append tmp (list (select list (iseq (* q cols) (- n 1))))))))))


(defun get-yes-no-list (prompt llist)
  (let* ((pitems (send text-item-proto :new prompt))
	 (items (mapcar #'(lambda(x) (send toggle-item-proto 
					   :new x :value t)) llist))
	 (ok (send modal-button-proto :new "OK"
		   :action
		   #'(lambda() (mapcar #'(lambda(x) (send x :value)) items))))
	 (d (send modal-dialog-proto :new 
		  (list (list pitems) (arrange items) (list ok)))))
    (send d :modal-dialog)))

;;;
;;; Things here very quick and very dirty hacks. Need to
;;; spend more time on this...
;;;
(defun files-exist (fnames)
  (let ((fs nil))
    (if (new-xlispstat)
	(setf fs (list (ignore-errors (mapcar #'open fnames))))
      (setf fs (unwind-protect (mapcar #'open fnames))))
    (if (some #'not fs)
	nil
      (progn
	(if (listp (select fs 0))
	    (setf fs (select fs 0)))
	(dolist (x fs)
		(close x))
	t))))

(defun numbers-ok (list)
  (if (some #'(lambda(x) (= (length x) 0)) list)
      nil
    (let ((numbers nil))
      (if (new-xlispstat)
	  (setf numbers 
		(ignore-errors 
		 (mapcar #'(lambda(x) (read-from-string x)) list)))
	(setf numbers 
	      (unwind-protect
		  (mapcar 
		   #'(lambda(x) (read (make-string-input-stream x))) list))))
      numbers)))

(defun check-fields (fields)
  (let* ((n (length fields))
	 (tmp (select fields (iseq (- n 1))))
	 (data-files (mapcar #'(lambda(x) (select x 0)) tmp))
	 (c (mapcar #'(lambda(y) (select y 1)) tmp))
	 (hfiles (mapcar #'(lambda(x) (select x 2)) tmp))
         (dfexist (files-exist data-files))
	 (cok (numbers-ok c))
	 (hfexist (files-exist hfiles))
	 (hrfexist (files-exist (last fields))))
    (if (not dfexist)
	(progn
	  (message-dialog 
	   "Error!\n\nSome Data files don't exist.\nPlease try again!")
	  nil)
      (if (not cok)
	  (progn
	    (message-dialog 
	     "Error!\n\nThe constants are not proper.\nPlease try again!")
	    nil)
	(if (not hfexist)
	    (progn 
	      (message-dialog 
	       "Error!\n\nSome Hyperparameter files don't exist.\nPlease try again!")
	      nil)
	  (if (not hrfexist)
	      (progn 
		(message-dialog 
		 "Error!\n\nHyperparameter ranges file doesn't exist.\nPlease try again!")
		nil)
	    t))))))

(defun make-same-struct (x y)
  (let ((w ())
	(c 0))
    (dotimes (i (length x))
	     (let* (( s (select x i))
		    (l (if (listp s)
			   (length s)
			 1)))
	       (cond 
		((listp s) (setf w (cons (select y (+ c (iseq l))) w)))
		(t (setf w (cons (select y c) w))))
	       (setf c (+ c l))))
    (reverse w)))

(defun even-seq (b)
  (1+ (* 2 (iseq (floor (/ b 2))))))

(defun definedp (symbol)
  "Method args: symbol
Returns t if symbol is defined and bound."
  (and (boundp symbol) symbol))

(defun ask-user (&rest args)
  "Method args: &rest args
Asks the user to enter data with a string which is all of args concatenated."
  (let ((tmp ""))
    (dolist (x args)
            (setf tmp (concatenate 'string tmp x)))
    (get-value-dialog tmp)))

(defun make-choose-mes (str)
  "Method args: str
Returns a message with the same name as the string with spaces
replaced by hyphens and the sequence of letters choose- prepended
to the string."
  (let ((tmp (coerce (mapcar #'(lambda(x) (if (equal x #\Space) #\- x))
			     (coerce str 'list)) 'string)))
    (read (make-string-input-stream 
	   (concatenate 'string ":choose-" tmp)))))

(defun str-to-mes (str)
  "Method args: str
Returns a message with the same name as the string with spaces
replaced by hyphens and : prepended to the string."
  (let ((tmp (coerce 
	      (mapcar #'(lambda(x) (if (equal x #\Space) #\- x))
		      (coerce str 'list)) 'string)))
    (read (make-string-input-stream 
	   (concatenate 'string ":" tmp)))))

(defmeth scatterplot-proto :add-modified-boxplot (y &key (x 1.0) 
						    (width 1.0) (draw t))
  (unless (= 2 (send self :num-variables)) 
          (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
         (half-foot (* 0.1 width))
         (fiv (fivnum y))
         (bot (select fiv 0))
         (top (select fiv 4))
         (q1 (select fiv 1))
         (med (select fiv 2))
         (q3 (select fiv 3))
         (iqr (- q3 q1))
         (low (- q1 (* 1.5 iqr)))
         (high (+ q3 (* 1.5 iqr)))
         (outliers nil))
    (dolist (val y)
            (if (or (> val high) (<  val low))
                (setf outliers (append outliers (list val)))))
    (when outliers 
          (send self :add-points (list x x) (list bot top))
          (send self :add-points (repeat x (length outliers)) outliers)
          (send self :point-symbol (iseq (+ 2 (length outliers))) 
                'diamond))
    (send self :plotline (- x half-foot) low  (+ x half-foot) low  nil)
    (send self :plotline (- x half-foot) high (+ x half-foot) high nil)
    (send self :plotline x low x q1   nil)
    (send self :plotline x q3  x high nil)
    (send self :plotline (- x half-box) q1  (+ x half-box) q1  nil)
    (send self :plotline (- x half-box) med (+ x half-box) med nil)
    (send self :plotline (- x half-box) q3  (+ x half-box) q3  nil)
    (send self :plotline (- x half-box) q1  (- x half-box) q3  nil)
    (send self :plotline (+ x half-box) q1  (+ x half-box) q3 nil))
  (send self :adjust-to-data))

(defun modified-boxplot (data &key (title "Modified Box Plot"))
  "Args: (data &key (title \"Modified Box Plot\"))
DATA is a sequence, a list of sequences or a matrix. Makes a 
boxplot of the sequence or a parallel box plot of the sequences
in the list or the columns of the matrix."
(let ((p (send scatterplot-proto :new 2 :title title :show nil 
		 :go-away nil)))
    (setq data
          (cond ((matrixp data) (column-list data))
                ((or (not (listp data)) (numberp (car data))) (list data))
                (t data)))
    (let ((range (get-nice-range (min data) (max data) 4)))
          (send p :range 1 (nth 0 range) (nth 1 range))
          (send p :y-axis t nil (nth 2 range)))
	(send p :range 0 0 (1+ (length data)))
    
    (dotimes (i (length data))
             (send p :add-modified-boxplot (nth i data) :x (1+ i)))
    (send p :show-window)
    p))

(defmeth text-item-proto :width (&optional width)
  (if width
      (let ((sz (slot-value 'size)))
	(setf (slot-value 'size) (list width (select sz 1))))
    (select (slot-value 'size) 0)))

(defmeth interval-scroll-item-proto :width (&optional width)
  (if width
      (let ((sz (slot-value 'size)))
	(setf (slot-value 'size) (list width (select sz 1))))
    (select (slot-value 'size) 0)))



