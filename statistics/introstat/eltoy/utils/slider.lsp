;;; Copyright 1991 Russell G. Almond
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This redefines the slider-item prototype.  The new range slider
;;; item is a more completely documented and supported version of the
;;; slider-item proto built on the almost completely undocumented
;;; scroll-item-proto.

;;; This should probably be a standard part of xlispstat library.

(require :el-superset (strcat ElToY-directory "utils" *d-sep*  "superset.lsp"))
(require :el-checks (strcat ElToY-directory "utils" *d-sep*  "checks.lsp"))

;;; range-slider-item-proto
;;; Slots:
;;;	scroll-item (type Scroll-Item)	--- primitive display item.
;;;	text-item (type Text-Item)	--- display for value
;;;	uval	(type Number)    	--- value in user space
;;;     ival	(type Fixnum)    	--- intergized (displayed) value 
;;;	range   (type (list Number Number)) --- range of values
;;;	integer? (type (member T nil))	--- is value constrained to integers?
;;;	granularity (type Number)	--- increment of scroll
;;;	num-points (type Fixnum)	--- number of points in scroll
;;;	action  (type Function (val))	--- action function
;;;
;;; Parents: (scroll-item-proto)
;;; Defined Messages:
;;;  	:value (&optional new-value) Returns/sets value
;;;    	:display-value () Redisplayes text item
;;;	:do-action  () Call contents of :action slot Only-if value has
;;;		       changed.
;;;	:scroll-action  () Call contents of :action slot Only-if value has
;;;		       changed.
;;;	:user-action  () Call contents of :action slot Only-if value has
;;;		       changed.
;;;	:range (&optional new-range) Returns/sets range
;;;	:min (&optional new-min) Returns/sets min (first range)
;;;	:max (&optiona new-max) Returns/sets max (second range)
;;;	:granularity (&optional new-gran) Returns/sets granularity
;;;	:integer? (&optional new-flag) Returns/sets integer constraint flag
;;;	:num-points (&optional reset?) Returns/recalculates num-points
;;;	:scroll-item (&rest args) Returns scroll-item or sends args to
;;;				  scroll item.
;;;	:text-item (&rest args) Returns text-item or sends args to
;;;				  text item.
;;;	:set-text-item (&optional text-item) Returns/sets text-item
;;;	:uval-to-ival (val) Calculates integer position on scroll from value
;;;	:ival-to-uval (ival) Caclulates value from integer position on scroll


(defproto range-slider-proto
  '(scroll-item				;subsideary scroll item
    text-item				;subsideary text item
    uval				;current value
    ival				;current value in scroll integers
    range				;range of values (list min max)
    granularity				;size of increment
    integer?				;(flag) is value restricted to integer?
    num-points				;number of points in scroll
    action				;action function
   ) () ()
 "Clean slider-item interface meant to have well defined protocols
with documentation and easily modified behavior.

Note install :scroll-item slot in dialog, but keep record of range-slider.
")



;;; value
(defmeth range-slider-proto :value (&optional set-val)
  (when set-val
	(setf (slot-value 'uval) set-val)
	(setf (slot-value 'ival) (send self :uval-to-ival set-val))
	(send self :scroll-item :value (slot-value 'ival))
	(send self :display-value))
  (slot-value 'uval))


(send range-slider-proto :documentation :value
      "Returns/sets value in user co-ordinates

Note: because of modifications to :do-action, does *not* call :action 
function.  Modifications to :do-action could cause the behavior to
change. 
")


;;; display-value
(defmeth range-slider-proto :display-value ()
  (send self :text-item :text
	(format nil "~S" (slot-value 'uval)))
  (slot-value 'uval))


(send range-slider-proto :documentation :display-value
      "Displays the value in the text-item (if present)
")


;;;----------- Action Functions -----------
;;; :do-action
(defmeth range-slider-proto :do-action ()
  (send self :user-action))


(send range-slider-proto :documentation :do-action
      "Calls :user-action method.  

By default, calls method in :action slot *only-if* the value has
changed.  This insures against spurious calls to the :action function
when the value is being set externally.  This code can be specialized
by changing :user-action
")


(defmeth range-slider-proto :scroll-action ()
  (send self :user-action))

(send range-slider-proto :documentation :scroll-action
      "Calls :user-action method.  

By default, calls method in :action slot *only-if* the value has
changed.  This insures against spurious calls to the :action function
when the value is being set externally.  This code can be specialized
by changing :user-action.

This traps the slider item :scroll-action method.  As I'm not sure of
the subtle distinctions between :do-action and :scroll-action, you
will probably need to play with this definition to get the desired
behavior. 
")


(defmeth range-slider-proto :user-action ()
  (let ((new-ival (send self :scroll-item :value)))
    (unless (eql new-ival (slot-value 'ival))
					;trap for resetting value
	    (setf (slot-value 'ival) new-ival)
	    (setf (slot-value 'uval)
		  (send self :ival-to-uval new-ival))
	    (send self :display-value)
	    (if (and (slot-value 'action)
		     (functionp (slot-value 'action)))
		;; There seems to be a bug in functionp, this should
		;; get around it.
		(funcall (slot-value 'action) (slot-value 'uval))))))

(send range-slider-proto :documentation :user-action
      "Calls :action function if value has changed and if it exists.  

Note:  it only calls the :action function if the value has changed.
This allows the body of the action function to send us the :value
message with no harm.  This is particularly true, if certain values
are illegal and we would like to change the value back.

This is contrary to the default behavior of the xlispstat objects
where calling :values also calls :do-action.  The :unsafe-user-action
function imitates this behavior.
")


(defmeth range-slider-proto :unsafe-user-action ()
  (let ((new-ival (send self :scroll-item :value)))
    (setf (slot-value 'ival) new-ival)
    (setf (slot-value 'uval)
	  (send self :ival-to-uval new-ival))
    (send self :display-value)
    (if (and (slot-value 'action)
	     (functionp (slot-value 'action)))
	(funcall (slot-value 'action) (slot-value 'uval)))))

(send range-slider-proto :documentation :unsafe-user-action
      "Calls :action function if it exists.

Note:  Calls to :value with a setting arg will call the :do-action
method as side effect.  This can prove disturbing if the :action
function contains calls to :value to reset the value.

This is the default behavior of the xlispstat objects and can cause
infinite loops. The :user-action function traps for calls to
:do-action where there has been no change and does not call the
:action function.
")


;;; :action

(defmeth range-slider-proto :action (&optional new-act-fun)
  (when new-act-fun
	(unless (functionp new-act-fun)
		(if (and (listp new-action-fun)
			 (eql lambda (car new-act-fun)))
		    (setq new-act-fun (eval `(function ,new-act-fun)))
		  (error "Argument to :action must be a function.")))
	(setf (slot-value 'action) new-act-fun))
  (slot-value 'action))
		       

;;; ---------- Range /granularity setting methods ----------

;;; range 
(defmeth range-slider-proto :range (&optional set)
  (when set
	(unless (test-pair set (slot-value 'integer?))
		(error "Bad slider range:  ~S~%" set))
	(setf (slot-value 'range) set)
	(send self :num-points t) ;rest scroll
	(let ((value (force-between (slot-value 'uval) set)))
	  (unless (eql value (slot-value 'uval))
		  (setf (slot-value 'uval) value)
		  (setf (slot-value 'ival)
			(send self :uval-to-ival value))
		  (send self :scroll-item :value (slot-value 'ival))
		  (send self :display-value))))
  (slot-value 'range))


(send range-slider-proto :documentation :range
      "Returns/sets range of user co-ordinates.

On set also rescales slider.
")

;;; :max
(defmeth range-slider-proto :max (&optional set)
  (when set
	(let ((new-range (list (first (slot-value 'range)) set)))
	  (unless (test-pair new-range (slot-value 'integer?))
		  (error "Bad slider range:  ~S~%" new-range))
	  (send self :range new-range)))
  (second (slot-value 'range)))


(send range-slider-proto :documentation :max
      "Returns/sets max range of user co-ordinates.

On set also rescales slider.
")

;;; :min
(defmeth range-slider-proto :min (&optional set)
  (when set
	(let ((new-range (list set (second (slot-value 'range)))))
	  (unless (test-pair new-range (slot-value 'integer?))
		  (error "Bad slider range:  ~S~%" new-range))
	  (send self :range new-range)))
  (first (slot-value 'range)))


(send range-slider-proto :documentation :min
      "Returns/sets max range of user co-ordinates.

On set also rescales slider.
")



;;; :granularity
(defmeth range-slider-proto :granularity (&optional set)
  (when set
	(unless (checknump set (slot-value 'integer?))
		  (error "Bad slider granularity:  ~S~%" set))
	(setf (slot-value 'granularity) set)
	(send self :num-points t) ;rest scroll
	(setf (slot-value 'ival)
	      (send self :uval-to-ival))
	(send self :scroll-item :value (slot-value 'ival))
	)
  (slot-value 'granularity))


(send range-slider-proto :documentation :min
      "Returns/sets max range of user co-ordinates.

On set also rescales slider.
")


; note internally goes from 0 to num-points
(defmeth range-slider-proto :num-points (&optional set)
  (when set
	(let ((range (slot-value 'range)))
	  (setf (slot-value 'num-points)
		(ceiling (/ (- (second range) (first range))
			    (slot-value 'granularity))))
	  (send self :scroll-item :max-value (slot-value 'num-points))
	  (send self :scroll-item :value (slot-value 'ival))))
  (slot-value 'num-points))

(send range-slider-proto :documentation :num-points
      "Returns number of points in slider, 
if arg is present and non-nil recalculate and reset num-points.
")




;;; ---------- scroll-item ----------
(defmeth range-slider-proto :scroll-item (&rest args)
  (if args
      (apply #'send (slot-value 'scroll-item) args)
    (slot-value 'scroll-item)))
  
(send range-slider-proto :documentation :scroll-item
      " If there are not args, returns  scroll-item object
If there are args, they are send as message to the scroll-item object.
")

(defmeth range-slider-proto :set-scroll-item
  (&optional (new-scroll-item nil set))
  (if set
      (setf (slot-value 'scroll-item) new-scroll-item)
    (slot-value 'scroll-item)))



(defmeth range-slider-proto :text-item (&rest args)
  (if (and args (slot-value 'text-item))
      (apply #'send (slot-value 'text-item) args)
    (slot-value 'text-item)))

(send range-slider-proto :documentation :text-item
      " If there are not args, returns  text-item object
If there are args, they are send as message to the text-item object.
")

(defmeth range-slider-proto :set-text-item
  (&optional (new-text-item nil set))
  (if set
      (setf (slot-value 'text-item) new-text-item)
    (slot-value 'text-item)))

(send range-slider-proto :documentation :set-text-item
      " If there are not args, returns  text-item object
If there are args, set the text-item slot to the first arg.
As a speical case an arg of nil remove the reference to the text-item.
")

;;; ---------- Uval to Ival conversions ----------
(defmeth range-slider-proto :uval-to-ival (&optional (uval (slot-value 'uval)))
  (let ((range (slot-value 'range)))
    (force-between
     (round (/ (- uval (first range)) (slot-value 'granularity)))
     (list 0 (slot-value 'num-points)))))

(send range-slider-proto :documentation :uval-to-ival
      "Translates value in user range to integer value for scroll.
")

(defmeth range-slider-proto :ival-to-uval (&optional (ival (slot-value 'ival)))
  (let* ((range (slot-value 'range))
	 (uval (force-between
		(+ (first range) (* ival (slot-value 'granularity)))
		range)))
    (if (slot-value 'integer?) (round uval)
      uval)))

(send range-slider-proto :documentation :ival-to-uval
      "Translates value integer value from scroll to value in user
range. 
")


;;; ---------- Integer? ----------

;;; Integer constraint flag methods.
(defmeth range-slider-proto :integer? (&optional (int? nil set))
  (when set
	(setf (slot-value 'integer?) int?)
	(when int?
	      (unless (and (integerp (slot-value 'granularity))
			   (every #'intergerp (slot-value 'range)))
		      (setf (slot-value 'granularity)
			    (ceiling (slot-value 'granularity)))
		      (setf (slot-value 'range)
			    (round (slot-value 'range)))
		      (send self :num-points t))
	      (unless (integerp (slot-value 'uval))
		      (send self :value (round (slot-value 'uval))))))
  (slot-value 'integer?))


(send range-slider-proto :documentation :integer?
      "Returns/sets integer constraint flag.

If set to non-nil this forces all values, range limits and granularity
to be integers.
")


;;; ---------- isnew ----------

(defvar *default-granularity* .1
  "Default granularity for sliders.")


(defmeth range-slider-proto :isnew
  (&key (value nil)
	(text-item nil)
	(range nil)
	(max nil)
	(min nil)
	(integer? nil)
	(granularity *default-granularity*)
	(action nil)
   &allow-other-keys)
  
  ;; Simple set methods
  (setf (slot-value 'action) action)
  (setf (slot-value 'integer?) integer?)
  (setf (slot-value 'text-item) text-item)

  ;; Simple except for forcing to integers.
  (setq granularity (if integer? (ceiling granularity) granularity))
  (setf (slot-value 'granularity) granularity)
  
  
  ;; Should have either range or min and max
  (unless range
	  (if (and min max) (setq range (list min max))
	    (error "range-slider must have either :range or :min and
:max args")))
  (setq range (if integer? (round range) range))
  (setf (slot-value 'range) range)  
  
  ;; Value defaults to minimum
  (unless value
	  (setq value (first range)))
  (setq value (force-between (if integer? (round value) value) range))
  (setf (slot-value 'uval) value)
  

  ;; initial calculation of :num-points
  (setf (slot-value 'num-points)
	(ceiling (/ (- (second range) (first range)) granularity)))
  (setf (slot-value 'ival)
	(send self :uval-to-ival value))


  ;; launch range-scroll-item
  (setf (slot-value 'scroll-item)
	(send range-scroll-item-proto :new
	      :min-value 0
	      :max-value (slot-value 'num-points)
	      :page-increment 1
	      :value (slot-value 'ival)
	      :range-slider self))

  (send self :display-value)
  
  self
  )



(defmeth range-slider-proto :dispose ()
  (setf (slot-value 'scroll-item) nil)
  (setf (slot-value 'text-item) nil))



;
;;; ======================================================================
;;; range-scroll-item-proto ---- specializations to scroll-item 
;;; ======================================================================

(defproto range-scroll-item-proto 
  '(range-slider)			;parent range-slider
  () (list scroll-item-proto)
  "Specializations to scroll-item-proto needed for range-slider."
)

(defmeth range-scroll-item-proto :range-slider (&rest args)
  (if args (apply #'send (slot-value 'range-slider) args)
    (slot-value 'range-slider)))

(defmeth range-scroll-item-proto :set-range-slider (&optional set)
  (if set
      (setf (slot-value 'range-slider) set))
  (slot-value 'range-slider))


(defmeth range-scroll-item-proto :isnew (&rest args
					 &key range-slider      
					 &allow-other-keys)
  (setf (slot-value 'range-slider) range-slider)
  (apply #'call-next-method args))

(defmeth range-scroll-item-proto :do-action ()
  (send self :range-slider :do-action))

(defmeth range-scroll-item-proto :scroll-action ()
  (send self :range-slider :scroll-action))



;;;======================================================================
;;; range-slider-dialog
;;;======================================================================

;;; dummy copy of xlisp slider dialogs just for fun.

(defproto range-slider-dialog-proto '(range-slider) () dialog-proto)

(defmeth range-slider-dialog-proto :isnew 
  (&key (text "Value") (title "Slider") action
	(range '(0 1) range?) integer? (granularity *default-granularity*)
	value min max)
  (let* ((value-item (send text-item-proto :new "              "
                           :location '(100 5)))
         (name-item (send text-item-proto :new text))
	 (range (if range? range
		  (if (and min max) (list min max)
		    range)))
	 (range-slider (send range-slider-proto :new
			     :range range
			     :integer? integer?
			     :action action
			     :granularity granularity
			     :text-item value-item
			     :value value)))
    (setf (slot-value 'range-slider) range-slider)
    (call-next-method (list name-item value-item
			    (send range-slider :scroll-item))
		      :title title)
    (send range-slider :display-value)))


(defmeth range-slider-dialog-proto :value (&rest args)
  (apply #'send (slot-value 'range-slider) :value args))

(defmeth range-slider-dialog-proto :range-slider (&rest args)
  (if args
      (apply #'send (slot-value 'range-slider) args)
  (slot-value 'range-slider)))


(defun range-slider-dialog (&rest args)
"Args: (&key (text \"Value\") (title \"Slider\") action
              (range '(0 1)) (granularity *default-granularity*)
              min max value)
Opens modeless dialog with title TITLE, prompt TEXT, a text display and a
scrollbar. The scrollbar scrolls through the interval RANGE, a list of the form
(MIN MAX) and displays the value. When a scroll event occurs
ACTION is called with the current value in the interval as argument.
If GRANULARITY determines the size of one increment."
  (apply #'send range-slider-dialog-proto :new args))


(new-provide :slider)
