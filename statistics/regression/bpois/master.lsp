(provide "master")
(require "slave")
;;;;
;;;;                THE MASTER PROTOTYPE.
;;;;
;;;; Inherits from dialog proto. Has a number of configurable options.
;;;; The options are set in the file "master-opts.lsp".
;;;;
;;;; Slots:
;;;; ------
;;;; DATA is a filename or a list of filenames.  If it is a string,
;;;; or a list of one string, then it is assumed that only one 
;;;; Markov Chain has been run; otherwise, as many chains as there are
;;;; elements in the list is assumed to have been run. (User supplied)
;;;;
;;;; CONSTANTS is a list of estimates of the constants of proportionality.
;;;; It is not needed if the only one Chain is used. (User supplied)
;;;;
;;;; IDENTIFIER is simply a string that is used to identify instances
;;;; of the object.  (User supplied)
;;;;
;;;; N is the number of rows in the Matrix. It should be same for all
;;;; Chains. It is calculated automatically from DATA.
;;;;
;;;; P is the number of parameters in the regression model. It is safer
;;;; to specify this. If not specified, the layout of the data file is 
;;;; used to determine this. If p > 3, we recommend that you specify
;;;; p. 
;;;;
;;;; SLAVES holds p slaves, one for each parameter.
;;;;
;;;; ORG-HYPER-VALS is a list of the values of the hyper-parameters
;;;; at which the Markov Chain was run.  Its structure is determined by
;;;; how many Markov Chains are used.  If one Markov Chain is used,
;;;; it is a list of lists of the values of the hyper-parameters.
;;;; Otherwise it is a list of such lists, one for each Markov Chain.
;;;; (User supplied). 
;;;;
;;;; CUR-HYPER-VALS is a list of current values of the hyper-parameters.
;;;;
;;;; INITIAL-HYPER-VALS is a list of initial values for the hyper-paramaters.
;;;; It is used by the reset button. (User supplied)
;;;;
;;;; HYPER-RANGE-LIST is a list of intervals specifying the ranges within
;;;; which the user wishes to change the hyper-parameter values.
;;;; (User supplied). 
;;;;
;;;; HYPER-SHOW-LIST is a list of t's and nil's indicating which
;;;; which hyper-parameters should be shown in the sliders.  Thus,
;;;; one can selectively hide or not allow some hyper-parameters to 
;;;; be changed. (User supplied). 
;;;; 
;;;; SHOW-WINDOW-LIST is a list of t's or nil's indicating which 
;;;; slave windows
;;;; are to be displayed.  This allows the user to concentrate on a 
;;;; small set
;;;; of windows at a time. Windows can be "awoken" later if necessary.
;;;; (User supplied).  Default is all.
;;;;
;;;; CREATE-WINDOW-LIST is a list of t's or nil's indicating which 
;;;; slave windows are to be CREATED! This allows you to save
;;;; resources.
;;;; (User supplied).  Default is all.
;;;;
;;;; WEIGHT-WATCHER holds a weight-watcher plot if the user so desires.
;;;; Initially, it is nil.
;;;; 
;;;; WEIGHTS holds the weights used in reweighting. They contain weights
;;;; calculated using the Reweighting Mixtures (RM) scheme. 
;;;; (See Geyer and Thompson 1992, reply to discussion, and Geyer 1993).
;;;;
;;;; LAZY is a slot used for efficient synchronization.
;;;;
;;;;Creates a new instance of the master prototype. Leads the user along
;;;;with further questions if keyword arguments are not given. Data should be a
;;;;string or a list of strings. P is the number of parameters.
;;;;Param-labels is a list of labels used for the parameter. Hyper-vals is
;;;;a list of lists of hyper-parameters used in generating the data.
;;;;Hyper-range-list is a list of lists of the ranges between which each
;;;;hyper-parameter will be varied. Identifier is a string that will
;;;;uniquely identify the object. Hyper-param-names is a list of names for
;;;;hyper-parameters. Hyper-show-list is a list of t' or nil's to indicate
;;;;which hyper-parameters can be varied. Weight-watcher should be t or
;;;;nil. Window list should be a list of t's and nils to indicate which
;;;;windows should be shown. Initial-hyper-vals is a list of initial
;;;;values for the hyper-parameters; if not specified then the first value
;;;;of Hyper-vals is used.
;;;;
(require "master-opts")

(defproto master-proto '(data constants identifier n p m slaves
				   org-hyper-vals cur-hyper-vals
				   initial-hyper-vals h-mix
				   hyper-range-list hyper-show-list
				   hyper-param-names show-window-list
				   create-window-list
				   weight-watcher weights
				   lazy)
  () dialog-proto
  "The Master prototype. Creates and manipulates a harem of slaves.")

(defmeth master-proto :isnew (&key data constants p param-labels 
				   hyper-vals
				   initial-hyper-vals hyper-range-list 
				   identifier 
				   hyper-param-names hyper-show-list
				   weight-watcher show-window-list
				   create-window-list)
  "Method args: (&key data constants p param-labels hyper-vals
                 initial-hyper-vals hyper-range-list identifier
                 hyper-param-names hyper-show-list 
                 weight-watcher show-window-list create-window-list)
The parameters are discussed in the comments above."

  ;; Note: Although we are allowing keyword options, they are only
  ;; for convenience. This code assumes that many important keyword
  ;; arguments are supplied. Thus, those arguments are not really 
  ;; optional. Such a design makes it easy to build a user-friendly
  ;; graphical interface as a front-end. 

  ;;
  ;; Set up identifier.
  ;;
  (setf (slot-value 'identifier) identifier)
  ;;
  ;; Get the file name if not given.
  ;;
  (let ((fnames (if (stringp data)
		    (list data)
		  data)))
    ;;
    ;; Determine the number of markov chains.
    ;;
    (setf (slot-value 'm) (length fnames))
    
    (setf (slot-value 'p) p)
    
    (let ((d (read-data-columns (select fnames 0) (send self :p))))
      ;;
      ;; Calculate n, the number of observations.
      ;;
      (setf (slot-value 'n) (length (select d 0))))
    ;;
    ;; Put the whole data as one long array.
    ;;
    (setf (slot-value 'data)
	  (combine (mapcar #'read-data-file fnames))))
  ;;
  ;; Get values for the constants of proportionality for
  ;; the RM scheme.
  ;;
  
  (setf (slot-value 'constants) (/ constants))
					;Store reciprocal. 
  
  ;;
  ;; Get hyper-parameter values if not given
  ;;
  (setf (slot-value 'org-hyper-vals) 
	(if hyper-vals
	    hyper-vals
	  (get-hyper-vals (send self :how-many-chains) (send self :p))))
  
  ;;
  ;; Create names for Hyper-parameters if not given.
  ;;
  (setf (slot-value 'hyper-param-names)
	(if hyper-param-names
	    (combine hyper-param-names)
	  (if (definedp '*hyper-param-names*)
	      (combine *hyper-param-names*)
	    (mapcar #'(lambda(x) (format nil "Hyp(~d)" x)) 
		    (iseq (length (combine (select (send self :org-hyper-vals) 0))))))))

  ;;
  ;; What does the user want as an initial value for the hyper-params?
  ;; This has to be stored for resetting.
  ;; If it is not specified, use the middle value of the range.
  ;;
  (setf (slot-value 'initial-hyper-vals)
	(if initial-hyper-vals
	    (float (combine initial-hyper-vals))
	  (mapcar #'(lambda(x) (* (sum x) 0.5)) hyper-range-list)))
  ;;
  ;; Set up the current values of hyper-parameters.
  ;; The user will get to change this.
  ;;
  (setf (slot-value 'cur-hyper-vals) 
	(copy-list (slot-value 'initial-hyper-vals)))
  ;;
  ;; Which windows must be created?
  ;;
  (setf (slot-value 'create-window-list)
	(if create-window-list
	    create-window-list
	  (repeat t (send self :p))))
  ;;
  ;; Which windows must be shown?
  ;;
  (setf (slot-value 'show-window-list)
	(if show-window-list
	    show-window-list
	  (repeat t (send self :p))))
  ;;
  ;; You shouldn't be able to show windows that are not created!
  ;;
  (setf (slot-value 'show-window-list)
	(mapcar #'(lambda(x y) (and x y)) 
		(slot-value 'show-window-list) 
		(slot-value 'create-window-list)))
  
  (setf (slot-value 'lazy) t)   ; Be lazy for now.
  
  ;;
  ;; Which hyper-parameters are allowed to be changed?
  ;;
  (setf (slot-value 'hyper-show-list)
        (if hyper-show-list
            hyper-show-list
          (mapcar #'(lambda(x) (if (listp x)
                                   (make-list (length x) :initial-element t)
                                 t))
                  (select (send self :org-hyper-vals) 0))))
  ;;
  ;; Now we can trash the layout of or-hyper-vals since its purpose is over.
  ;;
  (setf (slot-value 'org-hyper-vals) 
	(float (combine (slot-value 'org-hyper-vals))))
  ;;
  ;; Now calculate the mixture distribution, h_mix, for fast
  ;; reweighting.  
  ;;

  (setf (slot-value 'h-mix) 
	(let ((n (send self :n))
	      (m (send self :how-many-chains)))
	  (select
	   (call-cfun "h_mix" n (send self :p) m (send self :data)
		      (send self :reciprocal-constants) 
		      (send self :org-hyper-vals)
		      (repeat 0.0 (* n m)))
	   6)))

  ;;
  ;; Calculate the weights.
  ;;
  (send self :calc-weights)
  ;;
  ;; Create Slaves, one for each parameter in the model.
  ;; First, the labels for the slaves..
  ;;
  (if (not param-labels)
      (setf param-labels 
	    (if (definedp '*param-labels*)
		*param-labels*
	      (mapcar #'(lambda(x) "Beta(~d)") (iseq (send self :p))))))
  ;;
  ;; Now, the real thing.
  ;;
  (setf (slot-value 'slaves)
	(mapcar #'(lambda(x) 
		    (send slave-proto :new self x 
			  :label (select param-labels x)
			  :create-window 
			  (select (slot-value 'create-window-list) x)))
		(iseq (send self :p))))
  ;;
  ;; Now, we need to set up the sliders for controlling the hyper-param
  ;; values. We need to know the range between which the hyper-params will be
  ;; changed.  To make the sliders line up nicely, we need to do some mundane things.
  ;; First, each slider will have two text-items above it, one for the label and
  ;; the other for the value. We need to know the sum of the widths of those two text
  ;; items and send the sliders the :size message to make them that width. Furthermore,
  ;; the layout of the sliders will be dictated by the org-hyper-vals slot.
  ;; Brace yourself for some ugly code....
  ;;
  (let ((rlist hyper-range-list)
	(tsize (max (mapcar #'(lambda(x) 
				(length (send self :hyper-identifier x)))
			    (iseq (length (send self :hyper-vals)))))) ; max text width.
	(maxsize 0)         ; the sum of the widths of the text and value slots, initially.
	(kount 0)           ; A counter.
	(slist ())          ; This will hold the list of sliders.
	(dlist ()))         ; The list of items to be sent to the dialog.
    (dolist (s (send self :hyper-show-list))
	    (cond
	     ((eql s nil) (setf kount (1+ kount)))   ; I haven't thought too much about this.
	     ((eql s t) (let* ((a kount)
			       (ti (send text-item-proto :new 
					 (send self :hyper-identifier a)
					 :text-size tsize))
			       (vi (send text-item-proto :new "" 
					 :text-length *default-val-width*))
			       (si (send interval-scroll-item-proto :new
					 (select rlist a)
					 :points *hyper-val-stops*
					 :text-item vi
					 :action #'(lambda(x)
						     (send self :hyper-vals a x)))))
			  (send si :value (send self :hyper-vals a))
			  (setf maxsize
				(max maxsize 
				     (+ (send ti :width) (send vi :width))))
			  (setf slist (append slist (list si)))
			  (setf dlist (append dlist (list (list ti vi) si)))
			  (setf kount (1+ kount))))
	     ((listp s) (let ((tvlist ())
			      (sslist ()))
			  (dolist (x s)
				  (when x 
					(let* ((a kount)
					       (ti (send text-item-proto :new 
							 (send self :hyper-identifier a)
							 :text-size tsize))
					       (vi (send text-item-proto :new "" 
							 :text-length *default-val-width*))
					       (si (send interval-scroll-item-proto :new
							 (select rlist a)
                                                         :points *hyper-val-stops*
							 :text-item vi
							 :action #'(lambda(x)
								     (send self :hyper-vals a x)))))
					  (send si :value (send self :hyper-vals a))
					  (setf maxsize
						(max maxsize 
						     (+ (send ti :width) (send vi :width))))
					  (setf slist (append slist (list si)))
					  (setf sslist (append sslist (list si)))
					  (setf tvlist (append tvlist (list ti vi)))))
				  (setf kount (1+ kount)))
			  (if tvlist
			      (setf dlist (append dlist 
						  (list tvlist sslist))))))
	     (t (error "Bad show hyper-parameters list"))))
    ;;  
    ;; Make all sliders the same size so as to line up decently.
    ;;
    ;; Need a better algorithm for this....
    ;;
    (dolist (obj slist)
	    (send obj :width (- maxsize 10)))
    ;;
    ;; Since we need the slider list for resetting,
    ;; create a method.
    ;;
    (defmeth self :slider-list ()
      slist)
    ;; 
    ;; Now add Reset Weight-watcher and Window-list buttons.
    ;;
    (let ((reset (send button-item-proto :new "Reset"
		       :action #'(lambda() (send self :reset))))
	  (watcher (send button-item-proto :new "Weight Watcher: Off"
			 :action #'(lambda() (send self :toggle-weight-watcher))))
	  (show (send button-item-proto :new "More..."
		      :action #'(lambda() (send self :show-options)))))
      (call-next-method (append (list (list reset watcher show)) dlist))))
					; End of the big let.
  ;;
  ;; Now we are done with setting up the sliders.
  ;; 

  ;;
  ;; Make a decent title.
  ;;
  (send self :title (concatenate 'string (send self :identifier) "-Master Object"))
  ;;
  ;; Ok, time to show those windows.
  ;;
  (setf (slot-value 'lazy) nil)
  (send self :synchronize)
  (send self :show-windows))
  
(defmeth master-proto :show-windows ()
  (let ((slaves (send self :slaves)))
    (dotimes (i (send self :p))
	     (if (select (slot-value 'show-window-list) i)
		 (send (select slaves i) :show-window)
	       (send (select slaves i) :hide-window)))))

(defmeth master-proto :how-many-chains ()
  (slot-value 'm))

(defmeth master-proto :toggle-weight-watcher ()
  (let ((w (slot-value 'weight-watcher))
	(button (select (select (send self :items) 0) 1))) 
					; I expect the button to be 2nd in row 1.
    (case w
      (nil (setf (slot-value 'weight-watcher)
		 (modified-boxplot (send self :weights)
				   :title (concatenate 'string 
						       (send self :identifier)
						       "-Weight Watcher")))
	   (send button :slot-value 'text "Weight Watcher: On ")
	   (send self :hide-window) (send self :show-window))
      (t (send w :remove) 
	 (setf (slot-value 'weight-watcher) nil)
	 (send button :slot-value 'text "Weight Watcher: Off")
	 (send self :hide-window) (send self :show-window)))))
  
(defmeth master-proto :hyper-identifier (i)
  "Method args: (i)
Returns a string identifying the hyper-parameter i"
  (select (slot-value 'hyper-param-names) i))

(defmeth master-proto :hyper-vals (&optional i x)
  "Method args: (&optional i x)
Returns a list of hyper-parameter values. If i is given, returns the 
i-th value. If x is specified, sets the i-th value to x."
  (if x
      (progn (setf (select (slot-value 'cur-hyper-vals) i) x)
             (send self :synchronize))

    (if i
	(select (slot-value 'cur-hyper-vals) i)
      (slot-value 'cur-hyper-vals))))
  
(defmeth master-proto :identifier ()
  "Method args: none
Retrieves a string that identifies this object."
  (slot-value 'identifier))

(defmeth master-proto :close ()
  (let ((slaves (send self :slaves)))
    (dotimes (i (send self :p))
	     (if (select (slot-value 'create-window-list) i)
		 (send (select slaves i) :remove))))
  (when (slot-value 'weight-watcher)
	(send (slot-value 'weight-watcher) :remove))
  (call-next-method))

(defmeth master-proto :slaves (&optional index)
  "Method args: (&optional index)
Retrieve the slave numbered index if index is specified, or return the
slot-value."
  (if index
      (select (slot-value 'slaves) index)
    (slot-value 'slaves)))

(defmeth master-proto :weights ()
  "Method args: ()
Retrieves the mixture weights."
  (slot-value 'weights))

(defmeth master-proto :data (&optional index)
  (if index
      (let* ((n (send self :n))
	     (p (send self :p))
	     (m (send self :how-many-chains))
	     (ilist (mapcar #'(lambda(x) (+ (* x n p) index (* p (iseq n)))) (iseq m))))
	(select (slot-value 'data) (combine ilist)))
    (slot-value 'data)))

(defmeth master-proto :synchronize ()
  (when (not (slot-value 'lazy))
	(send self :calc-weights)
	(when (slot-value 'weight-watcher)
	      (let ((w (slot-value 'weight-watcher)))
		(send w :clear :draw nil)
		(send w :add-modified-boxplot (send self :weights))
		(send w :adjust-to-data)))
	(dotimes (i (send self :p))
		 (when (select (slot-value 'show-window-list) i)
		       (let ((x (send self :slaves i)))
			 (send x :redraw)
			 (send x :adjust-to-data))))))

(defmeth master-proto :org-hyper-vals (&optional index)
  "Method args: (&optional index)
Returns a list of the original hyper-param values. If index is 
specified, returns the index-th value."
  (if index
      (select (slot-value 'org-hyper-vals) index)
    (slot-value 'org-hyper-vals)))

(defmeth master-proto :initial-hyper-vals ()
  (slot-value 'initial-hyper-vals))

(defmeth master-proto :n ()
  "Method args: ()
Returns the value of n."
  (slot-value 'n))

(defmeth master-proto :p ()
  (slot-value 'p))

(defmeth master-proto :reciprocal-constants ()
  (slot-value 'constants))

(defmeth master-proto :reset ()
  "Resets the state of all objects."
  (setf (slot-value 'lazy) t)
  (let ((sliders (send self :slider-list))
	(vals (send self :initial-hyper-vals)))
    (dotimes (i (length sliders))
      (send (select sliders i) :value (select vals i))))
  (setf (slot-value 'lazy) nil)
  (send self :synchronize))

(defmeth master-proto :mix-dist ()
  (slot-value 'h-mix))

(defmeth master-proto :hyper-show-list ()
  (slot-value 'hyper-show-list))

(defmeth master-proto :create-window-list ()
  (slot-value 'create-window-list))

(defmeth master-proto :show-window-list (&optional list)
  (if list
      (setf (slot-value 'show-window-list) list)
    (slot-value 'show-window-list)))

(defmeth master-proto :show-window-dialog ()
  (let* ((w (send self :show-window-list))
	 (s (send self :slaves))
	 (names (mapcar #'(lambda(x) (send x :slot-value 'param-label)) s))
	 (items (mapcar #'(lambda(x y) (send toggle-item-proto :new x :value y))
			names w))
	 (cancel (send modal-button-proto :new "Cancel"))
	 (ok (send modal-button-proto :new "OK"
		   :action
		   #'(lambda() 
		       (let ((vals (mapcar #'(lambda(x) 
					       (send x :value)) items))
			     (cw (send self :create-window-list)))
			 (send self :show-window-list vals)
			 (dotimes (i (send self :p))
				  (when (and (select vals i) (not (select cw i)))
					(send (select s i) :create-window)
					(setf (select cw i) t)))
			 (send self :show-windows))
		       (send (send ok :dialog) :modal-dialog-return nil))))
	 (d (send modal-dialog-proto :new 
		  (list (arrange items) (list ok cancel)))))
    (send d :modal-dialog)))

(defmeth master-proto :all-stats ()
  (format t "~%~%*** Statistics for ~a ***~%" (send self :identifier))
  (let* ((s (send self :slaves))
	 (mes (send (select s 0) :stats-messages))
	 (names (mapcar #'(lambda(y) (string-right-trim ":" y))
			(send (select s 0) :stats-labels)))
	 (fmt (send (select s 0) :stats-print-formats))
	 (mfl (max fmt (mapcar #'length names)))
	 (mnl (max (mapcar #'(lambda(y) (length (send y :param-label))) 
			   s)
		   (length "Parameter")))
	 (line (make-string (+ mnl 2 (* (length names) (1+ mfl)))
			    :initial-element #\-)))
    
    (format t "~a~%" line)
    (format t "~va" mnl "Parameter")
    (format t "  ")
    (dolist (x names)
	    (format t "~va" mfl x)
	    (format t " "))
    (format t "~%~a~%" line)
    (dotimes (i (send self :p))
	     (let* ((ss (select s i))
		    (res (mapcar #'(lambda(y) (send ss y)) mes)))
	       (format t "~va" mnl (send ss :param-label))
	       (format t " ")
	       (dotimes (j (length res))
			(let ((fl (select fmt j)))
			  (format t "~v,vf "
				  mfl (select fl 1)
				  (select res j))
			  (format t " "))))
	     (format t "~%"))
    (format t "~%*** End of Statistics ***~%~%")))

(defmeth master-proto :show-options ()
  "Method args: none.
Allow the user to change options in a dialog box."
  (let* ((change (send text-item-proto :new "Double Click to change"))
	 (opts '("Window List" "Statistics"))
	 (messages '(:show-window-dialog :all-stats))
	 (l-item (send list-item-proto :new opts
		       :action #'(lambda (x) 
				   (if x 
				       (let* ((d (send l-item :dialog))
					      (ind (send l-item 
							 :selection))
					      (mes (select messages ind)))
					 (send self mes)
					 (send d :modal-dialog-return nil))))))
	 (cancel (send modal-button-proto :new "Cancel"))
	 (diag (send modal-dialog-proto :new
		     (list change l-item cancel) 
		     :title (concatenate 'string
					 (send self :title)
					 ":Options"))))
    (send diag :modal-dialog)))


(require "master-dyn")











