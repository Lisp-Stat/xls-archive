;;file transfor.lsp, formerly transform.lsp, damn 8 char limit!
;;transform-proto is an object for controling transformation between
;;two intervals
;;; udina, unc, feb 1994
;;;        upf, jan 1995, march 1995

(defmacro WHILE (cond &rest exprs)
  `(loop
   (unless ,cond (return))
    (progn ,@exprs)))
(unless (fboundp 'system-has-windows)
	(defun system-has-windows ()
	  (or (member 'windows *features*)
	      (member :windows *features*))))

(defvar *kde-wsize*
  (if (featurep :macintosh) '(400 280)  '(650 450))
"the default size of windows to be created")

(defproto transf-proto
  '(domain ;an interval '(a b)
    image ;id.
    num-knots ; the number of knots
    knots ; a list of x-coords and y-coords of the knots 
    old-knots ; for being able to undo the last change
    use-vert-scale ; a flag t/nil used for having log scale in vertical axis.
    interpolation ; can be 'step 'linear'spline or 'mid-point-splines
    notice-to-owner ;a list (obj message) for notifying changes to owner
    window ; the graphical interface
    title
    )
  ()  *object*)

;;;
;;;  this is the main entry call, use it directly
;;;
(defun make-transformation  (&key (domain '(0 1)) (image nil) 
			    (num-knots nil) (knots nil)
			    (interpolation 'spline)
			    (use-vert-scale nil)
			    (notice-to-owner nil)
			    (title "Transformation")
			    (vert-label nil)
			    (want-window t)
			    (show t)
			    (constrain-knots nil)
			    (initial-transformation nil)
			    (initial-constant nil))
"Args: &key (domain '(0 1)) (image nil)  (num-knots 3) (knots nil) (constrain-knots nil) (interpolation 'spline) (title \"Transformation\") (want-window t) (initial-transformation nil) (initial-constant nil)
 Builds a transf object controlling a continuous transformation between the DOMAIN interval and the IMAGE interval. INTERPOLATION can be 'spline or 'linear. If WANT-WINDOW is t, a window for displaying and mofifying the transformation will be created. Send :help to the transformation for more info. If CONSTRAIN-KNOTS is not nil, moving the knots will be restricted to vertical.
INITIAL-TRANSFORMATION, if given, must be a function to apply to the initial knots, INITIAL-CONSTANT can be a number to be taken as the initial value for all the knots.
Examples of use:
(make-transformation)
(make-transformation :domain '(-2 2) :num-knots 5)"

(let ((tr (send transf-proto :new 
		:domain domain 
		:image image 
		:num-knots num-knots
		:constrain-knots constrain-knots
		:interpolation interpolation 
		:use-vert-scale use-vert-scale
		:notice-to-owner notice-to-owner
		:title title
		:vert-label vert-label
		:want-window want-window
		:show nil)))
  (when initial-constant
	(setf initial-transformation
	      #'(lambda (x) initial-constant))
	(unless (send tr :slot-value 'image)
		(send tr :image (* initial-constant '(0.33 3)))))
  (when initial-transformation
	(let* ((n (send tr :num-knots))
	       (kn (send tr :knots))
	       (xkn (first kn))
	       (ykn (mapcar initial-transformation xkn)))
	  (send tr :knots (list xkn ykn) t)
	  (unless (send tr :slot-value 'image)
		  (defmeth tr :default-image ()
		    (list (min (second (send self :knots)))
			  (max (second (send self :knots))))))
	  ))
  (when show (send tr :to-window :show-window)
	(send tr :to-window :adjust-to-data))
  tr))

;; isnew 

(defmeth transf-proto :isnew (&key (domain '(0 1)) ;an interval '(a b)
				   (image nil)  ;id. if it is nil, a default will
				                   ;be computed
				   (num-knots 3)   ; the number of knots
				   (knots nil)   ; when nil default is equally spaced
				   (constrain-knots nil)
				   (interpolation 'linear); can be 'linear 'spline
				   (use-vert-scale nil)
				   (notice-to-owner nil)
				   (title "Transformation")
				   (vert-label nil)
				   (want-window t)
				   (show nil)) ; the graphical interface
"This is the initialization method of transf-proto object.
 If you dont't know about objects, use make-transformation instead."
  (call-next-method)
;fill the slots
  (send self :domain domain)
  (send self :use-vert-scale use-vert-scale)
  (send self :num-knots     num-knots)
  (send self :knots knots)
  (send self :slot-value 'old-knots nil)
  (send self :interpolation interpolation) 
  (send self :title title)
  (send self :image image) 

  (send self :notice-to-owner notice-to-owner)
  (if (and want-window
	   (system-has-windows))
      (progn
	(send self :show-window :show show)
	(send self :to-window :vert-label vert-label)
	(when constrain-knots
	      (send self :to-window
		    :slot-value 'constrain-knots constrain-knots)))
    (setf (slot-value 'window) nil))
  self)

;;;accessor methods -----------------------------------------------------

(defmeth transf-proto :domain (&optional (domain nil set))
"Args: &optional (domain nil)
 Accessor method to the DOMAIN slot. Returns the value of the slot.  If given some arguments, it is set as new value for the slot. DOMAIN slot defaults to the [0 1] interval."
    (when set
  	  (setf (slot-value 'domain) domain))
    (unless (slot-value 'domain)
	    (setf (slot-value 'domain) '(0 1)))
    (slot-value 'domain))                                    

(defmeth transf-proto :notice-to-owner (&optional (notice-to-owner nil set))
"Args: &optional (notice-to-owner nil)
 Accessor method to the NOTICE-TO-OWNER slot. To be used internally."
    (when set
  	  (setf (slot-value 'notice-to-owner) notice-to-owner))
    (slot-value 'notice-to-owner))        

(defmeth transf-proto :title (&optional (title nil set))
"Args: &optional (title nil)
 Accessor method to the TITLE slot. Returns the value of the slot.  If given some arguments, it is set as new value for the slot."
    (when set
  	  (setf (slot-value 'title) title))
    (unless (slot-value 'title)
	    (setf (slot-value 'title) "No title"))
    (slot-value 'title))                         


(defmeth transf-proto :image (&optional (image nil set))
"Args: &optional (image nil)
 Accessor method to the IMAGE slot. Returns the value of the slot.  If given some arguments, it is set as new value for the slot.
This slot is only used as a guide for chosing the graphic range, if it is nil, the apropriate interval will be used."
  (when set
	(setf (slot-value 'image) image))
  (if (slot-value 'image)
      (slot-value 'image)
    (send self :default-image)))

(defmeth transf-proto :use-vert-scale (&optional (use nil set))
"standard accessor to use-vert-scale slot"
(when set
      (if (member use '(log nil))
	  (setf (slot-value 'use-vert-scale) use)
	(message-dialog "This kind of vertical scale is not defined")))
(slot-value 'use-vert-scale))

(defmeth transf-proto :default-image () 
"computes the automatic image range. Will be used if the IMAGE slot is nil."
(let* ((ykn nil);;;(second (send self :knots))
       (min (min ykn))
       (max (max ykn)))
;;  (list min max)
(send self :domain)))


(defmeth transf-proto :num-knots (&optional (num-knots nil set))
"Args: &optional (num-knots nil)
 Accessor method to the NUM-KNOTS slot. Returns the value of the slot.  If given some arguments, it is set as new value for the slot.
This slot is the number of knots to put in the domain interval, it includes the ends of the interval. It defaults to 7."
  (when set
	(setf (slot-value 'num-knots) num-knots))
  (unless (slot-value 'num-knots)
	  (setf (slot-value 'num-knots) 7))
  (slot-value 'num-knots))

(defmeth transf-proto :knots (&optional (knots nil set) (send-to-window nil))
"Args: &optional (knots nil) (send-to-window nil)
 Accessor method to the KNOTS slot. Returns the value of the slot.  If given some argument, it is set as the new slot value.
If send-to-window is t, the window will be updated.
This slot contains the current knots of the transformation. It is a list of two lists: the x and y coordinates of the knots. The length of these lists must match the slot :num-knots."
  (when set
	(when (slot-value 'knots)
	      (setf (slot-value 'old-knots) (slot-value 'knots)))
	(setf (slot-value 'knots) knots)
	(when (and send-to-window
		   (slot-value 'window))
	      (send self :redraw-window :force t)))
  (if (slot-value 'knots)
      (slot-value 'knots)
    (send self :knots (send self :default-knots))))

(defmeth transf-proto :undo-knots-change ()
  (when (slot-value 'old-knots)
	(send self :knots (slot-value 'old-knots))
	(send self :redraw-window :force t)))

(defmeth transf-proto :default-knots ()
"computes a list of equally spaced default knots. Used when slot KNOTS is found to be empty."
(let* ((xkn
	(apply #'rseq (append (send self :domain) (list (send self :num-knots)))))
       (img (if (send self :image)
		(send self :image)
	      (send self :domain)))
       (ykn
	(apply #'rseq (append img (list (send self :num-knots)))))
       )
  (list xkn ykn)))

(defmeth transf-proto :interpolation (&optional (interpolation nil set) (draw nil))
"Args: &optional (interpolation 'spline)
 Accessor method to the INTERPOLATION slot. Returns the value of the slot. If given some argument, it is set as the new value for the slot."
  (when set
	(if (member interpolation '(step linear spline mid-point-splines))
	    (setf (slot-value 'interpolation) interpolation)
	  (message-dialog "Not implemented yet"))
	(send self :redraw-window :force t))
  (unless (slot-value 'interpolation)
	  (setf (slot-value 'interpolation) 'spline))
  (slot-value 'interpolation))

;this is the real core of the object

;;

(defmeth transf-proto :target-points (xx &optional (yy nil yset))
  "points to draw, args: (list x y) or simply x y, where x y are lists"
(let* ((y (if yset yy (second xx)))
       (x (if yset xx (first xx))))
  (send self :to-window :slot-value 'target-points (list x y))
  (send self :to-window :redraw-content)))

;;;better to have a default for x, so a single call is enough for redrawing
;; Sure, I have written other code more efficient than the 'step and linear parts
;; of this method. Someone want to improve it?
(defmeth transf-proto :transform (x &key (interpolation
					  (send self :interpolation))
				    (want-pairs t))
"Args: x (&key (interpolation (send self :interpolation)) (want-pairs nil))
Actually computes the transformation for the list of values X. If WANT-PAIRS is true, a list of two list will be returned, the first beeing X itself. If not, only the list of transformed values is returned."
;(format t "transform: ~a, ~a, ~a~%" (length x) interpolation want-pairs)
(case interpolation
      ('step 
       (if (listp x)
	     (mapcar #'(lambda (num) (send self :transform num 
					   :interpolation interpolation
					   :want-pairs want-pairs))
		     x)
	 (let* ((kn (transpose (send self :knots))))
	   (while (and (> (length kn) 1)
		       (> x (/ (+ (first (first kn)) (first (second kn))) 2)))
	     (setq kn (cdr kn)))
	   (cadar kn))))
      ('linear 
       (if (listp x)
	     (mapcar #'(lambda (num) (send self :transform num 
					   :interpolation interpolation
					   :want-pairs want-pairs))
		     x)
	 (let ((kn (transpose (send self :knots))))
	   (while (and (> (length kn) 1)
		       (> x (first (second kn))))
	     (setq kn (cdr kn)))
	   (setq kn (select kn '(0 1)))
	   (apply #'linear-interpolation x kn))))
      ('spline
       (let* ((xx (if (< (length x) 2)
		      (repeat x 3)
		    x))
	      (res (if want-pairs 
		       (spline (first (send self :knots))
			       (second (send self :knots))
			       :xvals xx)
		     (second (spline (first (send self :knots))
				     (second (send self :knots))
				     :xvals xx)))))
	 (if (< (length x) 2)
	     (if want-pairs
		 (list (second (transpose res)))
	       (list (second res)))
	   res)))
      ('mid-point-splines
       (let* ((xx (if (< (length x) 2)
		      (repeat x 3)
		    x))
	      (fk (enclose-midpoints (first  (send self :knots))))
	      (sk (enclose-midpoints (second (send self :knots))))
	      (res (if want-pairs 
		       (spline fk sk :xvals xx)
		     (second (spline fk sk :xvals xx)))))
;;	 (mapcar #'(lambda (pair) 
;;		     (apply #'send 
;;			    self :to-window :draw-symbol 'diamond nil 
;;			    (apply #'send self :to-window :real-to-canvas pair)))
;;		  (transpose (list fk sk)))
	 (if (< (length x) 2)
	     (if want-pairs
		 (list (second (transpose res)))
	       (list (second res)))
	   res)))))


;;; the transf object is usually linked to a graphic window,
;; following, some methods for comunicating with it.
(defmeth transf-proto :to-window (&rest args)
"Sends all the arguments to the window, if it exists."
(if (null args)
    (slot-value 'window)
  (if (slot-value 'window)
      (apply #'send (slot-value 'window) args)
    (print (format nil "error?: a transf object with no window receive: ~a" 
		   args)))))

(defmeth transf-proto :show-window (&key (show t) (vert-label nil))
"Args: &key (show t vert-label nil)
 Will create and initialize the window object for diplaying and modifying the transformation. If SHOW is t, the window will be shown."

(if (and (slot-value 'window)
	 show)
    (send self :to-window :show-window)
  (when (system-has-windows)
	(setf (slot-value 'window)
	      (send transf-window-proto :new
		    :title (slot-value 'title) :vert-label vert-label
		    :core self :show show))
;;(message-dialog "This is in development stage!")
	(send self :arrange-window-menu)
	(apply #'send self :to-window :size *kde-wsize*)
	(unless (featurep :macintosh)
		(send self :to-window :back-color 'black)
		(send self :to-window :draw-color 'white))
	(send self :redraw-window :force t)
	(when show (send  self :to-window :show-window))
	(send self :to-window :adjust-to-data :draw nil)
	(send self :redraw-window :force t))))

(defmeth transf-proto :hide-window ()
  (send self :to-window :hide-window))

(defmeth transf-proto :close-windows ()
  "Closes all child windows"
(when (slot-value 'window)
      (send (slot-value 'window) :close)
      (setf (slot-value 'window) nil))
)

(defmeth transf-proto :arrange-window-menu ()
"will arrange the menu, not impkemented"
)

(defmeth transf-proto :redraw-window (&key (force nil))
"Args: &key (force nil)
recomputes graph, if force, forces recomp of all"
(let ((win (send self :slot-value 'window)))
  (when win
	(when force
	      (send win :draw-transformation-lines)
	      (send win :draw-knots)
;;	      (send win :adjust-to-data)
)
	(send win :redraw-content))
  win
  ))

(defmeth transf-proto :notify-owner ()
"Send a mesage to owner meaning something has changed in transformation.
 To be used internally."
(when (slot-value 'notice-to-owner)
      (apply #'send (slot-value 'notice-to-owner))))

(defmeth transf-proto :update-knots-from-window (&key (draw t))
"Args: &key (draw t)
Gets the points from the window and install them as the new knots. IF DRAW is t, all will be recomputed and the window will be redrawn."
(let* ((win (send self :slot-value 'window))
       (num-points (send win :num-points))
       (xs (send win :point-coordinate 0 (iseq num-points)))
       (ys (send win :point-coordinate 1 (iseq num-points)))
       (ord (order xs))
       (xs (select xs ord))
       (ys (select ys ord))
       ndp)
  (send self :num-knots num-points)
  (send self :knots (list xs ys))
  (when draw
	(send self :to-window :click-message "Please wait ..."))
  (send self :notify-owner)
  (when draw
	(send self :redraw-window :force t))))


(defmeth transf-proto :read-new-function (&key (draw t))
  (let ((st (get-string-dialog "Give an expr for (lambda (x) expr)"))
	fun kn fkn)
    (when st
	  (setf kn (first (send self :knots)))
	  (setf st (concatenate 'string "(lambda (x) " st ")"))
	  (setf fun (eval (read (make-string-input-stream st))))
	  (setf fkn (mapcar fun kn))
	  (send self :knots (list kn fkn) draw))
    st))

;::::::::::::::::::::::::::
;;; transf-window-proto :::
;::::::::::::::::::::::::::

(defproto transf-window-proto
  '(core ; the transf object that controls me
    constrain-knots vert-label y-labels target-points
    )
  nil
  graph-proto)

(defmeth transf-window-proto :isnew ( &key (title nil) core (vert-label nil)
					   (show nil) (debug nil))
;arrange menus
  (send self :menu-template 
	'(undo-knots-change dash
	  mouse dash 
          rescale 
	  #-small-machine options
	  #-macintosh     save-image
	  make-gnuplot-files
	  dash
	  transf-options choose-interpolation 
	  shift-transf
	  ))
  (send self :menu-title "Transform")
  (send self :vert-label vert-label)
  (apply #'call-next-method 2 :show nil :title title ())
  (setf (slot-value 'core) core)
  (setf (slot-value 'constrain-knots) nil)
;arrange mouse-modes
  #+macintosh (send self :delete-mouse-mode 'point-moving)
  #+macintosh (send self :delete-mouse-mode 'show-coordinates)
  (send self :add-mouse-mode 'show-coordinates
	:title "Show coords."
	:cursor 'cross
	:click :do-show-coordinates
)
  (send self :add-mouse-mode 'point-moving
	:title "Knot select/move"
	:cursor 'finger
	:click :do-click-select-move
)
  (send self :delete-mouse-mode 'selecting)
  (send self :delete-mouse-mode 'brushing)
  (send self :add-mouse-mode 'selecting
	:title "Knot select"
	:cursor 'finger
	:click :do-select-click)
  (send self :add-mouse-mode 'add-point
	:title "Add new knots"
	:cursor 'cross
	:click :do-add-point)
  (send self :add-mouse-mode 'trash-point
	:title "Trash knots"
	:cursor 'trash-can
	:click :do-trash-point)
  (send self :mouse-mode 'point-moving)
;adding overlay
  (let ((ov (send transf-win-overlay-proto :new)))
    (send self :margin 0 0 0 (+ 10 (send self :text-ascent)))
    (send self :add-overlay ov)
    (send ov :resize))
  (send self :x-axis t t 5)
  (send self :y-axis t t 5)
  (send self :to-core :redraw-window :force t)
  (when show 
	(send self :show-window)
	(send self :resize)
	(send self :redraw-content)
	(send self :adjust-to-data :draw nil)
	(send self :update nil))
  self
)

(defmeth transf-window-proto :vert-label (&optional label)
"accessor to slot vert-label"
(when label (send self :slot-value 'vert-label label))
(send self :slot-value 'vert-label))

(defmeth transf-window-proto :make-menu-item (item-template)
  (let ((item (case item-template
		    (choose-interpolation
		     (send graph-item-proto :new "Interpolation..."
			   self :choose-interpolation))
		    (transf-options
		     (send graph-item-proto :new "Knot options"
			   self :give-options))
		    (shift-transf
		     (send graph-item-proto :new "Transformation shift..."
			   self :shift-knots-dialog))
		    (make-gnuplot-files
		     (send graph-item-proto :new "Make gnuplot files"
			   self :to-gnuplot))
		    (undo-knots-change
		     (send graph-item-proto :new "Undo knot change"
			   self
			   :undo-knots-change))
		    )))
    (if item
	item
      (call-next-method item-template))))

(defmeth transf-window-proto :to-gnuplot ()
  (require "gnuplot")
  (call-next-method))

(defmeth transf-window-proto :undo-knots-change ()
  (send self :to-core :undo-knots-change)
  (send self :to-core :update-knots-from-window))


(defmeth transf-window-proto :give-options ()
"Let the user choose:
- knots are vertically constrained
- a handle to shift the whole transf is shown
- other..."
(let* ((curr-constr (slot-value 'constrain-knots))
       (toggle-vert-constr 
	(send toggle-item-proto :new "Knots can't move horiz."
	      :value (slot-value 'constrain-knots)))
       (toggle-eq-spaced 
	(send toggle-item-proto :new "Move now knots to vert. equally spaced"
	      :value nil))
       (toggle-reset-constant 
	(send toggle-item-proto :new "Reset now knots to a constant horz. value"
			       :value nil))
       (dlg (send ok-or-cancel-dialog-proto :new
		  (list toggle-vert-constr toggle-eq-spaced toggle-reset-constant)
		  :title "Knots options"))
       need-redraw)
  (send dlg :modal-dialog)
  (unless (eq curr-constr (send toggle-vert-constr :value))
	  (setf (slot-value 'constrain-knots) (send toggle-vert-constr :value))
	  (setf need-redraw t))
  (when (send toggle-eq-spaced :value)
	(let* ((dom (send self :to-core :domain))
	       (xs (apply #'rseq (append dom (list (send self :num-points))))))
	  (send self :point-coordinate 0 (iseq (send self :num-points)) xs))
	(setf need-redraw t))
  (when (send toggle-reset-constant :value)
	(let ((ave (mean (send self :point-coordinate 1 (iseq (send self :num-points))))))
	  (send self :point-coordinate 1 (iseq (send self :num-points)) ave))
	(setf need-redraw t))
  (when need-redraw
	(send self :to-core :update-knots-from-window :draw t))))

(defmeth transf-window-proto :choose-interpolation ()
"displays a dialog for choosing the norm to use, installs it  if OK is pressed."
  (let* ((loc (send self :location))
	 (xpos (+ (nth 0 loc) 
                  (nth 0 (send self :size))
		  15))
         (ypos (nth 1 loc))
	 (d (apply #'send choose-item-dialog-proto :new 
		   "Interpolation method:"
		   (list "Step" "Linear" "Cubic splines" "Mid-point splines")
                   :show nil 
		   :initial (position (send self :to-core :interpolation)
				      '(step linear spline mid-point-splines))
		   nil))
	 (pos (apply #'send d :location loc))
	 (reply (send d :modal-dialog)))
    (if reply
	(progn
	  (setf reply (select '(step linear spline mid-point-splines) reply))
	  (send self :to-core :interpolation reply t))
      nil)))

(defmeth transf-window-proto :shift-knots-dialog ()
"displays a dialog for shifting the transformation 
 by adding or multiplying it by some number."
  (let* ((loc (send self :location))
;	 (xpos (+ (nth 0 loc) 
;                  (nth 0 (send self :size))
;		  25))
;         (ypos (nth 1 loc))
	 (prompt-item (send text-item-proto :new "Enter a number to"))
         (string-item (send choice-item-proto :new '("Add" "Multiply") 
			    :value 1))
	 (text-item (send text-item-proto :new "the whole transformation"))
	 (value-item (send edit-text-item-proto :new "" 
			   :editable t 
			   :size 
			   #+macintosh '(50 15)
			   #+X11 '(100 30)
			   ))
	 (d (apply #'send ok-or-cancel-dialog-proto :new 
		   (list prompt-item string-item text-item value-item)
		   :title "Shift Transformation"
                   :show nil 
		   :ok-action #'(lambda () (list (send string-item :value)
					 (send value-item :text)))
		   nil))
	 (pos (apply #'send d :location (+ loc '(20 20))))
	 (reply (send d :modal-dialog)))
    (if reply
	(let* ((fun (select (list #'+ #'*) (first reply)))
	       (num (eval (read (make-string-input-stream (second reply)))))
	       (tr (slot-value 'core))
	       (knots (send tr :knots)))
	  (send tr :knots (list (first knots)
				(funcall fun num (second knots))))
	  (send tr :redraw-window :force t)
	  (send tr :notify-owner))
      nil)))

(defmeth transf-window-proto :to-core (&rest args) 
  (if (null args)
      (slot-value 'core)
      (apply #'send (slot-value 'core) args)))

(defmeth transf-window-proto :do-show-coordinates (x y m1 m2)
;modified from graphics.lsp, xlispstat for mac
;this version allows moving the mouse while seeing the coordinates
;of the click point
  (let* ((xy (cond (m1 (list x y))
                   (m2 (send self :canvas-to-scaled x y))
                   (t (send self :canvas-to-real x y))
               ))
         (s (strcat (string-3-digits (first xy)) " " (string-3-digits (second xy))))
         (str-size (send self :text-width s))
         (left (> (+ x str-size) (send self :canvas-width)))
         (horz (if left 2 0))
         (vert 0))
    (send self :draw-string-while-button s x y horz vert)))

(defmeth transf-window-proto :draw-string-while-button (s x y h v)
  (let* ((oldx x)
         (oldy y)
         (origin (first (transpose (send self :scaled-range '(0 1)))))
         (origin (apply #'send self :scaled-to-canvas origin))
         (mode (send self :draw-mode)))
    (send self :draw-mode 'xor)
    (send self :draw-line x y (first origin) y)
    (send self :draw-line x y x (second origin))
    (send self :draw-text s x y h v)
    (send self :while-button-down
          #'(lambda (nx ny)
              (send self :draw-text s oldx oldy h v)
              (send self :draw-text s nx ny h v)
              (setq oldx nx oldy ny)))
    ;redraw things for erasing
    (send self :draw-text s oldx oldy h v)
    (send self :draw-line x y (first origin) y)
    (send self :draw-line x y x (second origin))
    (send self :draw-mode mode)))

(defmeth transf-window-proto :add-point-and-check (x y)
"adds the point if there is no another point in the same vertical
 and the x value is in the domain
 returns t if added, nil otherwise."
(let* ((dom (send self :to-core :domain))
       (xs (send self :point-coordinate 0 (iseq (send self :num-points))))
       (?? (member x xs)))
  (if (or ??
	  (< x (first dom))
	  (> x (second dom)))
      nil
    (progn
      (send self :add-points (list (list x) (list y)))
      t))))

(defmeth transf-window-proto :draw-knots ()
  (send self :clear-points :draw nil)
  (send self :add-points (send self :to-core :knots) :draw nil)
  (when (slot-value 'constrain-knots)
	(let ((xv (first (send self :to-core :knots)))
	      (y0 (first (send self :scaled-range 1)))
	      (y1 (second (send self :scaled-range 1))))
	  (mapcar #'(lambda (x)
		      (send self :add-lines (list (list x x) (list y0 y1))
			    :draw nil
			    :type 'dashed))
		  xv))))

(defmeth transf-window-proto :draw-transformation-lines ()
  (send self :clear-lines :draw nil)
  (case (send self :to-core :interpolation)
	('linear (send self :add-lines
		       (send self :to-core :knots)
		       :draw nil))
	('step (mapcar #'(lambda (lin) (send self :add-lines lin :draw nil))
		       (let* ((kn (send self :to-core :knots))
			      (xs (first kn))
			      (ys (second kn)))
			 (transpose (list (stepy xs)
					  (transpose (list ys ys)))))))
	('spline
	 (let* ((rg (send self :scaled-range 0))
		(nxs (round (/ (send self :canvas-width) 3)))
		(xs (apply #'rseq (append rg (list nxs)))))
	   (send self :add-lines
		 (send self :to-core :transform xs)
		 :draw nil)))
	('mid-point-splines
	 (let* ((rg (send self :scaled-range 0))
		(nxs (round (/ (send self :canvas-width) 3)))
		(xs (apply #'rseq (append rg (list nxs)))))
	   (send self :add-lines
		 (send self :to-core :transform xs)
		 :draw nil)))
))

(defmeth transf-window-proto :click-message (string)
"Draws a message, intended for while mouse down"
(let ((width (send self :canvas-width))
      (hei (round (* 1.5 (send self :text-ascent)))))
  (send self :erase-rect 0 0 width hei)
  (send self :draw-text string (round (/ width 2)) hei 1 0)))

              
; :DRAG-POINT - if there is a point close to the mouse drag it
; and return its index. Otherwise returns NIL.
; For transformed data result only makes sense if transform is
; orthogonal.
; from luke tierney's graphics.lsp
; mine is much simpler, so better rewrite it
; and allow for constrained movement

(defmeth transf-window-proto :do-point-moving (x y a b)
  (when (slot-value 'constrain-knots)
	(send self :click-message "Movement is vertically constrained"))
  (let ((dragie (send self :drag-point x y))
	(dom (send self :to-core :domain))
	p nx ny)
    (when dragie
;;(print dragie)
	  (setq p (first dragie))
	  (setq nx (first (second dragie)))
	  (when (or (< nx (first dom))
		    (= 0 p))
		(setq nx (first dom)))
	  (when (or (> nx (second dom))
		    (= p (1- (send self :num-points))))
		(setq nx (second dom)))
	  (setq ny (second (second dragie)))
	  (if (member nx ;is it nx currently used by another knot?
		      (send self :point-coordinate 0 
			    (delete-if #'(lambda (x) (= x p))
				       (iseq (send self :num-points))))
		      :test #'=)
	      (message-dialog "Can't move knot here")
	    (progn
	      (send self :point-coordinate 1 p ny)
	      (unless (slot-value 'constrain-knots)
		      (send self :point-coordinate 0 p nx))
	      (send self :to-core :update-knots-from-window))))
;;(print (send self :point-coordinate '(0 1) p))
    (if (and (not dragie)
	     (slot-value 'constrain-knots))
	(send self :redraw-content))))

(defmeth transf-window-proto :do-click-select-move (x y a b)
  "handles mouse click"
  (if (send self :any-points-selected-p)
      (progn ;move all the selected points
	(send self :click-message "Move the selected knots vertically")
	(send self :cursor 'hand)
	(let ((drg (send self :drag-point x y)))
	  (if drg 
	      (send self :move-selection drg)
	    (progn
	      (send self :selection nil)
	      (send self :redraw-content)))
	  (send self :set-mode-cursor)))
    (progn ;no selection, move 1 point
      (send self :click-message (if (slot-value 'constrain-knots)
				    "Move this point vertically"
				  "Move this point"))
      (send self :cursor 'hand)
      (let ((drg0 (send self :drag-point x y)))
	(if drg0
	    (send self :move-point drg0)
	  (send self :redraw-content)))
;	  (progn 
;	    (send self :click-message "Select points by drawing a rectangle")
;	    (send self :do-select-click x y a b));this does'nt work!!! why??
      (send self :set-mode-cursor))))

(defmeth transf-window-proto :move-selection (dragie)
(let* ((p (first dragie))
       (ny (second (second dragie)))
       (oy (send self :point-coordinate 1 p))
       (lp (send self :selection)))
  (if (member p lp)
      (progn
	(mapcar #'(lambda (ap) (send self :point-coordinate 1 ap
				     (+ ny (- oy)
					(send self :point-coordinate 1 ap))))
		lp)
	(send self :to-core :update-knots-from-window)
	(send self :selection lp))
    (send self :redraw))))

(defmeth transf-window-proto :move-point (dragie)
  (let ((dom (send self :to-core :domain))
	p nx ny)
    ;;(print dragie)
    (setq p (first dragie))
    (setq nx (first (second dragie)))
    (when (or (< nx (first dom))
	      (= 0 p))
	  (setq nx (first dom)))
    (when (or (> nx (second dom))
	      (= p (1- (send self :num-points))))
	  (setq nx (second dom)))
    (setq ny (second (second dragie)))
    (if (member nx ;is it nx currently used by another knot?
		(send self :point-coordinate 0 
		      (delete-if #'(lambda (x) (= x p))
				 (iseq (send self :num-points))))
		:test #'=)
	(message-dialog "Can't move knot here")
      (progn
	(send self :point-coordinate 1 p ny)
	(unless (slot-value 'constrain-knots)
		(send self :point-coordinate 0 p nx))
	(send self :to-core :update-knots-from-window)))))
;;(print (send self :point-coordinate '(0 1) p))


(defmeth transf-window-proto :drag-point (x y)
  (let* ((tol 5)
         (width (* 2 tol))
         (points (send self :points-in-rect
                       (- x tol) (- y tol) width width))
         (p (if points (car points))))
    (if p
	(if (eql 'linear (send self :to-core :interpolation))
	    (let* ((coords (send self :drag-grey-rect  x y));p
		   (sx (+ (nth 0 coords) tol))
		   (sy (+ (nth 1 coords) tol))
		   (rcoords (send self :canvas-to-real sx sy)))
	      (list p rcoords))
	  (let* ((coords (send self :drag-grey-rect x y tol tol))
		   (sx (+ (nth 0 coords) tol))
		   (sy (+ (nth 1 coords) tol))
		   (rcoords (send self :canvas-to-real sx sy)))
	      (list p rcoords)))
	nil)))

(defmeth transf-window-proto :drag-grey-poly (p x y)
  (format t "~% ~a ~a ~a ~%" p x y))

(defmeth transf-window-proto :do-add-point (x y m1 m2)
    (let* ((tol 2)
	   (coords (send self :drag-grey-rect x  y tol tol))
	   (sx (+ (nth 0 coords) tol))
	   (sy (+ (nth 1 coords) tol))
	   (xy (send self :canvas-to-real sx sy))
           (num (send self :num-points)))
      (if (send self :add-point-and-check (nth 0 xy) (nth 1 xy))
	  (progn
	    (send self :point-symbol num 'X)
	    (send self :to-core :update-knots-from-window))
	(message-dialog "Knot cannot be in that place"))))

(defmeth transf-window-proto :do-trash-point (x y m1 m2)
"we don't care about selection, because transf has never points selected"
    (let* ((tol 2)
	   (coords (send self :drag-grey-rect x  y tol tol))
	   (nx (+ (nth 0 coords) tol))
	   (ny (+ (nth 1 coords) tol))
           (num (send self :num-points))
	   (tol 5)
	   (width (* 2 tol))
	   (points (send self :points-in-rect
			 (- nx tol) (- ny tol) width width))
	   (p (if points (car points)))
	   xrp yrp)
;there's no way to delete one point, so...
      (when p
	    (if (or (= p 0)
		    (= p (1- num)))
		(message-dialog "Don't trash the end knots")
	      (progn
		(setf xrp (send self :point-coordinate 0 (iseq num)))
		(setf yrp (send self :point-coordinate 1 (iseq num)))
		(setq xrp (select xrp
				  (delete-if #'(lambda (x) (= p x)) (iseq num))))
		(setq yrp (select yrp
				  (delete-if #'(lambda (x) (= p x)) (iseq num))))
		(send self :clear-points :draw nil)
		(send self :add-points (list xrp yrp) :draw nil)
		(send self :to-core :update-knots-from-window))))))


(defmeth transf-window-proto :close ()
"Before closing its windows, closes other child windows"
  (call-next-method)
  (send self :to-core :slot-value 'window nil)
  (send self :to-core :close-windows))

(defmeth transf-window-proto :set-ranges (xmin xmax ymin ymax &key (recalc nil))
  (send self :scaled-range 0 xmin xmax :draw nil)
  (send self :scaled-range 1 ymin ymax :draw nil)
  (when recalc (error "set-ranges does not recalculate" nil)))

(defmeth transf-window-proto :adjust-to-data (&key (draw t))
  (let* ((kn (send self :to-core :knots))
	 (img (send self :to-core :image))
	 (ys (append  img (* 0.8 (second kn)) (* 1.2 (second kn))))
	 (miny (min ys))
	 (maxy (max ys)))
(when (slot-value 'vert-label)
      (send self :y-axis t t 0))
    (setf (slot-value 'y-labels)
	  (string-3-digits (rseq miny maxy 5)))
    (apply #'send self :set-ranges 
	   (append (send self :to-core :domain) 
		   (list miny maxy))))
  (when draw
	(send self :redraw)))

(defmeth transf-window-proto :5ypos ()
  (let* ((5vals (apply #'rseq (append (send self :scaled-range 1) (list 5))))
	 (5points (mapcar #'(lambda (y)
			      (send self :real-to-canvas 0 y))
			  5vals)))
    (second (transpose 5points))))

(defmeth transf-window-proto :redraw-background ()
(call-next-method)
(when (slot-value 'vert-label);we draw the y-axis labels
      (mapcar #'(lambda (text ypos)
		  (send self :draw-text text 40 ypos 2 0)
		  (send self :draw-line 42 ypos 45 ypos))
	      (slot-value 'y-labels)
	      (send self :5ypos))))

(defmeth transf-window-proto :redraw-content () ; redefinim
(call-next-method)
(when (slot-value 'vert-label)
      (let* ((ypos (round (* 1.5 (send self :text-ascent))))
	     (xpos (round (/ (send self :canvas-width) 2))))
	(apply #'send self :draw-text (slot-value 'vert-label)
	       xpos ypos '(1 0)))))


#|
aixo per dibuixar el titol

(APPLY (FUNCTION SEND) WTR :DRAW-TEXT "hola" (append (SEND WTR :REAL-TO-CANVAS 30 -1) '(0 0)))
|#


;(def tr (send transf-proto :new ))

(princ "type \"(transf-test)\" for an example")


(defmeth transf-window-proto :redraw-content () ; redefinim
(call-next-method)
(when (slot-value 'vert-label)
      (let* ((ypos (round (* 1.5 (send self :text-ascent))))
	     (xpos (round (/ (send self :canvas-width) 2))))
	(apply #'send self :draw-text (slot-value 'vert-label)
	       xpos ypos '(1 0))))
(when (slot-value 'target-points)
      (mapcar #'(lambda (x y)
		  (apply #'send self :draw-symbol 'cross nil
			 (send self :real-to-canvas x y)))
	      (first (slot-value 'target-points))
	      (second (slot-value 'target-points)))))

;;;;;;;;;;;;;;;;;;;;
;;; transf-win-overlay-proto
;;; borrowed from spin-control-overlay-proto
;;; must be finished...

(defproto transf-win-overlay-proto 
          '(top lefts gap side ascent box-top text-base)
          ()
          graph-overlay-proto)

(defmeth transf-win-overlay-proto :isnew ()
  (setf (slot-value 'top) 4)
  (setf (slot-value 'box-top) 4)
  (setf (slot-value 'gap) 4)
  (setf (slot-value 'side) 9)
  (setf (slot-value 'ascent) (send graph-proto :text-ascent))
  (let ((w1 (send graph-proto :text-width "Show coords."))
        (w2 (send graph-proto :text-width "Select"))
        (w3 (send graph-proto :text-width "Move knots"))
        (w4 (send graph-proto :text-width "Add knots"))
        (w5 (send graph-proto :text-width "Trash knots"))
        (gap (slot-value 'gap))
        (side (slot-value 'side)))
    (setf (slot-value 'lefts)
          (list gap
                (+ (* 2 gap) side)
                (+ (* 5 gap) (* 2 side) w1)
                (+ (* 6 gap) (* 3 side) w1)
                (+ (* 9 gap) (* 4 side) w1 w2)
                (+ (* 10 gap) (* 5 side) w1 w2)
                (+ (* 13 gap) (* 6 side) w1 w2 w3)
                (+ (* 16 gap) (* 7 side) w1 w2 w3 w4)
                ))
    (setf (slot-value 'lefts)
          (list (+ (* 2 gap) )
                (+ (* 4 gap)  side w1)
                (+ (* 6 gap) (* 2 side) w1 w2)
                (+ (* 8 gap) (* 3 side) w1 w2 w3)
                (+ (* 10 gap) (* 4 side) w1 w2 w3 w4)
                (+ (* 10 gap) (* 5 side) w1 w2 w3 w4 w5)		
                )
)))

(defmeth transf-win-overlay-proto :resize ()
  (let* ((graph (send self :graph))
         (height (send graph :canvas-height))
         (bottom-margin (fourth (send graph :margin)))
         (top (+ (- height bottom-margin) 1))
         (gap (slot-value 'gap))
         (side (slot-value 'side))
         (ascent (send graph :text-ascent))
         (text-base (+ top gap (max side ascent)))
         (box-top (- text-base side)))
    (setf (slot-value 'top) top)
    (setf (slot-value 'text-base) text-base)
    (setf (slot-value 'box-top) box-top)))

(defmeth transf-win-overlay-proto :redraw ()
  (let* ((graph (slot-value 'graph))
        (top (slot-value 'top))
        (lefts (slot-value 'lefts))
        (gap (slot-value 'gap))
	(2gap (* 2 gap))
        (side (slot-value 'side))
        (text-base (slot-value 'text-base))
        (box-top (slot-value 'box-top)))
    (send graph :draw-line 0 top (send graph :canvas-width) top)
    (mapcar #'(lambda (x) (send graph :erase-rect x box-top side side))
             (butlast lefts))
    (mapcar #'(lambda (x) (send graph :frame-rect x box-top side side))
             (butlast lefts))
    (case (send graph :mouse-mode)
	  ('show-coordinates 
	   (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
		    (nth 0 lefts)))
	  ('selecting
	   (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
		    (nth 1 lefts)))
	  ('point-moving
	   (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
		    (nth 2 lefts)))
	  ('add-point
	   (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
		    (nth 3 lefts)))
	  ('trash-point
	   (funcall #'(lambda (x) (send graph :paint-rect x box-top side side))
		    (nth 4 lefts))))
    (mapcar #'(lambda (s x y) (send graph :draw-string s x y))
	    '("Show coords." "Select" "Move knots" "Add knots" "Trash knots")
            (+ lefts side gap) 
            (repeat text-base 5))))

(defmeth transf-win-overlay-proto :do-click (x y m1 m2)
  (let ((graph (slot-value 'graph))
        (top (slot-value 'top))
        (lefts (slot-value 'lefts))
        (gap (slot-value 'gap))
        (side (slot-value 'side))
        (text-base (slot-value 'text-base))
        (box-top (slot-value 'box-top))
        )
    (when (and (< top y) (< x (nth 5 lefts)))
	  (if (< box-top y text-base)
                    (let ((i (car (which (< lefts x (+ lefts side))))))
                      (when i
                            (send graph :mouse-mode 
                                  (select '(show-coordinates 
                                            selecting point-moving add-point trash-point)
					  i))
                            (send self :redraw))
		      t)))))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;some utility functions
(defun av (x y) (/ (+ x y) 2))

(defun stepy (dots)
  (let* ((curr dots)
         (prev dots)
         (next (cdr dots))
         (res nil))
    (setq res (list (list (car dots) (av (car dots) (car next)))))
    (setq curr (cdr curr))
    (setq next (cdr next))
    (loop
;     (print (list 'prev prev))
;     (print (list 'curr curr))
;     (print (list 'next next))
;     (print "---")
     (push (list (av (car prev) (car curr))
                 (av (car curr) (car next)))
           res)
     (unless (cdr next) (return))
     (setq prev (cdr prev))
     (setq curr (cdr curr))
     (setq next (cdr next))
     )
    (push (list (av (car curr) (car next))
                (car next))
          res)
    (reverse res)))

(defun string-3-digits (num &optional (number-of-digits 3))
"returns NUM with at most NUMBER-OF-DIGITS significant digits. Vectorized."
(if (consp num)
    (mapcar #'(lambda (x) (string-3-digits x number-of-digits))
	    num)
  (if (= 0 num)
      "0"
    (let* ((sign (if (> num 0)
		     1
		   (progn (setq num (- num))
			  -1)))
	   (order (floor (/ (log num) (log 10))))
	   (inf (- order number-of-digits -1))
	   (tr (round (/ num (expt 10 inf))))
	   (res (* sign tr (expt 10 inf))))
      (format nil "~a" res)))))


(defun linear-interpolation (x p0 p1)
"give two points p0 p1, and an abscissa value x, gives the linear interpolation for y"
(let ((rat (apply #'/ (reverse (- p1 p0)))))
  (+ (second p0) (* rat (- x (first p0))))))
	 

(defun enclose-midpoints (alist)
  (labels ((enc-midd-pnt (alist); aux recursive function
			 (let* ((f (first alist))
				(sec (second alist))
				(dif (- sec f))
				(this (+ f (* '(0.49 0.51) dif)))
				(rest (cdr alist)))
			   (if (cdr rest)
			       (append this (enc-midd-pnt rest))
			     this))))
	  (append (list (first alist))
		  (enc-midd-pnt alist)
		  (last alist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;some test functions
(defun transf-test ()
(format t "This is the result of running
    \"(make-transformation :num-knots 5)\"
The resulting object has been stored in tr~%")
(def tr (make-transformation :num-knots 5)))

(defun test () (setq tr (make-transformation :num-knots 3 :vert-label "Transf(x)")))

;;(send tr :to-window :mouse-mode 'trash-point))

;;(test)
 
;;(MAKE-TRANSFORMATION :INITIAL-transformation #'sqrt :constrain-knots t)

(defun testlog ()
  (setq tr (make-transformation :image '(1 10) :use-vert-scale 'log))
  (send tr :to-window :adjust-to-data))





;;;::::::::::::::::::::::provide "transfor"
(provide "transfor")

