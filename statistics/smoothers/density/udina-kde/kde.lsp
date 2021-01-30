;;;file kde.lsp, implementation of kde objects
;; f. udina, upf, 93-94.
;; this file contain all the kde-proto object implementation
;; it rely on other files containing some other related objects
;; and computation and utility functions
;; see FILES file and other documentation.
;; 26 apr 94

;; for running kde objects, you usually will load the file runkde
;; and call make-kde or make-kde-from-file

;;;global variables and some other configuration settings are kept in:

(load "kde_conf.lsp")

(require "calckde")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kde-proto definition and :isnew method
; kde objects are for kernel density estimation,
; they contain data and methods to do its task
; they contain a window slot that stores a wkde object:
					;a specialized graph object.

(defproto kde-proto '(data fivnums data-is-sorted
			   x-values x-values-auto
			   title
			   kernel-type bandwidth
			   use-canonical calc-method variable-bandwidth
			   bin-counts fft-counts
			   estimates-y previous-estimates-y bw-ends
			   width-slider-dialog bw-transf
			   info-strings
			   bootstrap-lines bootstrap-quant-lines
			   distr-dens distr-rand distr-lines
			   show-distr-lines show-a-kernel show-data-points
			   show-previous-estimate show-kernel-estimates
			   histogram-data histogram-num-bins histogram-slider
			   histogram-bin-method histogram-shift
			   silverman-graph
			   window)
  () *object*
)

(load "kdehisto");must be (re)loaded after redefining kde-proto

;
; to add slots to kde-proto, add them to this declaration
; and deal with them in :isnew method

(defmeth kde-proto :isnew (num &key title
                               (sorted nil) data x-values
			       (bin-counts nil)
                               kernel-type bandwidth
			       (calc-method 'fft)
			       (use-canonical t)
			       (variable-bandwidth nil)
                               bw-ends
                               distr-dens
                               distr-rand
			       (show-previous-estimate nil)
                               (show t)
			       (debug nil))
"Initializes a kde-proto descendent.
 The normal way to create a kde-object is the `make-kde' function."
  (when debug
	(print "kde-proto :isnew got arguments:")
	(print (list 	       :show show
		       :sorted sorted
		       :data data :title title
		       :x-values x-values
		       :kernel-type kernel-type :bandwidth bandwidth
                       :use-canonical use-canonical
		       :show-previous-estimate show-previous-estimate
		       :variable-bandwidth variable-bandwidth
                       :calc-method calc-method
		       :debug debug)))

  (call-next-method)
  (when (not data) (error "kde-proto :isnew: you must provide :data key" nil))
  (when (system-has-windows)
      (progn (require "wkde")))
  (send self :calc-method calc-method nil)
  (send self :use-canonical use-canonical nil)
  (if bandwidth
      (send self :bandwidth bandwidth nil nil)
    (send self :bandwidth 1 nil nil))
  (if kernel-type
      (send self :kernel-type kernel-type nil) ; don't draw
    (send self :kernel-type 'b nil))
  (when x-values
    (send self :x-values x-values))
  (send self :bin-counts bin-counts)
  (send self :data data sorted)
  (if bandwidth
      (send self :bandwidth bandwidth nil nil))
  (when debug (print (list "data installed" "sorted:"
			   (send self :slot-value 'data-is-sorted))))
  (send self :x-values) ;will calculate auto values if there are not
  (when debug (print "x-values calc'ed"))
  (if title (send self :title title)
     (send self :title "KDE instance"))
  (send self :distr-dens distr-dens)
  (send self :distr-rand distr-rand)
  (send self :variable-bandwidth variable-bandwidth nil)
  (send self :toggle-show-previous-estimate show-previous-estimate)
  (send self :estimates-y nil)
  (setf (slot-value 'show-distr-lines)
                    (if (slot-value 'distr-dens) t nil))
  (setf (slot-value 'show-a-kernel) nil)
  (setf (slot-value 'show-data-points) nil)
  (setf (slot-value 'show-kernel-estimates) t)
  (setf (slot-value 'histogram-data) nil)
  (setf (slot-value 'histogram-num-bins) nil)
  (setf (slot-value 'histogram-shift) 0)
  (setf (slot-value 'histogram-bin-method) *kde*binning*histograms*)
  (unless bandwidth
	  (send self :rule-of-thumb :showing t))
  (setf (slot-value 'window) nil)
  (if (and show (system-has-windows))
      (progn (require "wkde")
	     (send self :show-window :show show)))
  self)

(defmeth kde-proto :print (&optional (stream t))
   (format stream "#<kde: ~S [~S]>"
	   (send self :title)
	   (call-next-method nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some methods to deal with the graphics window, it's a wkde object

(defmeth kde-proto :to-window (&rest args)
  (if (slot-value 'window)
      (if  args
	  (apply #'send (slot-value 'window) args)
	(slot-value 'window))
    (if *kde-debugging*
        (print (format nil "a kde object with no window receive: ~a"
		       (select args 0))))))

(defmeth kde-proto :show-window (&key (show t))
  (if (slot-value 'window)
      (send self :to-window :show-window)
    (when (system-has-windows)
	  (setf (slot-value 'window)
		(send wkde-proto :new 2
		      :title (slot-value 'title)
		      :kde-core self :show show))
	  (send self :build-kde-menu)
	  (apply #'send self  :to-window :size *kde-wsize*)
	  (unless (featurep :macintosh)
		  (send self :to-window :back-color 'black)
		  (send self :to-window :draw-color 'white))
	  (when show (send  self :to-window :show-window))
	  (send self :to-window :clear-lines :draw t)
	  (send self :redraw-window :force t)
	  (send self :to-window :adjust-to-data :draw nil)
	  (send self :to-window :redraw-content)
	  (send self :to-window :activate t);;release 3???
	  )))


(defmeth kde-proto :hide-window ()
  (send self :to-window :hide-window))

(defmeth kde-proto :close-windows ()
  "Closes all child windows"
  (let ((slider (send self :slot-value 'width-slider-dialog))
        (histo (send self :slot-value 'histogram-slider))
	(silver (send self :slot-value 'silverman-graph))
	(transfi (slot-value 'bw-transf)))
    (when slider (send slider :close))
    (when histo (send histo :close))
    (send self :slot-value 'width-slider-dialog nil)
    (send self :slot-value 'histogram-slider nil)
    (when silver (send silver :close))
    (send self :slot-value 'silverman-graph nil)
    (when transfi
	  (send transfi :close-windows)
	  (setf (slot-value 'bw-transf) nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;accessor methods for data, x-values, kernel-type, bandwidth
; distr-dens, bw-ends, histogram-data and histogram-num-bins
; included also some methods for calculate default values of
; some slots

(defmeth kde-proto :title (&optional (title nil tset))
    (when tset
      (setf (slot-value 'title) title)
      (send self :to-window :title title))
    (slot-value 'title))

(defmeth kde-proto :data (&optional (data-list nil set) (sorted nil))
"sets or retrieves :data slot. When setting, sets data-list as data for
the object, sorting it if needed, clears all data-dependent slots
  and sets also :x-values-auto if there is no :x-values set."
;;WARNING sort-data accesses the slot directly.
(when set
      (setf (slot-value 'data-is-sorted) nil)
      (setf (slot-value 'data) data-list)
      (if sorted
	    (send self :slot-value 'data-is-sorted t)
	(when (eql (send self :calc-method) 'direct)
	      (send self :sort-data)))
      (setf (slot-value 'x-values-auto) nil)
      (setf (slot-value 'x-values) nil);;;***
      (setf (slot-value 'fivnums) nil)
      (setf (slot-value 'bin-counts ) nil)
      (setf (slot-value 'fft-counts) nil)
      (setf (slot-value 'distr-lines) nil)
      (setf (slot-value 'bw-ends) nil)
      (setf (slot-value 'estimates-y) nil)
      (setf (slot-value 'bootstrap-lines) nil)
      (setf (slot-value 'histogram-data) nil)
      (setf (slot-value 'histogram-num-bins) 0)
      (setf (slot-value 'bootstrap-quant-lines) nil)
      (setf (slot-value 'bandwidth)
	    (first (send self :rule-of-thumb :data data)));;;*************
      (send self :bw-ends);will calc auto ends
      (send self :redraw-window :force t)
      )
(slot-value 'data))

(defmeth kde-proto :fivnums (&optional (fn nil set))
"computes or access the slot fivnums, that contains the five numbers of data"
 (if set
     (setf (slot-value 'fivnums) fn)
     (unless (slot-value 'fivnums)
       (let* ((dt (send self :data))
              (n (length dt)))
          (when *kde*computing*inspect*
            (princ (format nil "computing the five numbers, length ~a"
                        n)))
	  (unless dt
               (error ":fivnums called, no :data!" nil))
          (setf (slot-value 'fivnums)
                (append (fivnum dt) (list 'N= n)))
          (when *kde*computing*inspect*
            (princ (format nil "->: ~a~%"
                        (slot-value 'fivnums))))

)))
 (slot-value 'fivnums))

(defmeth kde-proto :calc-x-values-auto ()
  (let* ((dt (send self :data))
	 (err (unless dt
		      (error "data not found in calc-x-values-auto" nil)))
	 (fn (send self :fivnums))
	 (mind (- (* 2 (first fn)) (second fn)))
	 (ff (cdddr fn))
	 (maxd (- (* 2 (second ff)) (first ff))))
    (setf (slot-value 'x-values-auto)
	  (rseq mind maxd *kde*default*xvalues*)))
)


(defmeth kde-proto :x-values (&optional (xvals nil set))
"sets x-values slot. If no values are provided (in a list-of-values form)
 returns the current value, unless it is nil,
 in which case returns x-values-auto,
 as computed by :calc-x-values.
 If xvals has two numbers only, they are taken as range limits,
          if has three numbers, third is number of values"
(if set
    (progn
      (when (< 1 (length xvals) 4)
	    (if (= 2 (length xvals))
		(setf xvals (apply #'rseq
				   (append xvals
					   (list *kde*default*xvalues*))))
	      (setf xvals (apply #'rseq xvals))))
      (setf (slot-value 'x-values) xvals)
      (if xvals
	  (setf (slot-value 'x-values-auto) nil)
	(send self :calc-x-values-auto))
      (setf (slot-value 'bootstrap-lines) nil)
      (setf (slot-value 'bootstrap-quant-lines) nil)
      (setf (slot-value 'distr-lines) nil)
      (setf (slot-value 'estimates-y) nil)
      (when (slot-value 'bin-counts)
	    (send self :bin-counts nil))
      (setf (slot-value 'histogram-data) nil)
      (when (slot-value 'histogram-shift)
	    (send self :histogram-shift :offset 0)))
;if not set:
  (if (slot-value 'x-values)
      (slot-value 'x-values)
    (if (slot-value 'x-values-auto)
	(slot-value 'x-values-auto)
      (send self :calc-x-values-auto)))))

(defmeth kde-proto :bin-counts (&optional (bc nil set))
  "Accessor to slot 'bin-counts"
;(format *standard-output* "entering bin-counts, bc: ~a, set: ~a, slot: ~a~%"
;	bc set (not (null (slot-value 'bin-counts))))
  (if set
      (progn
	(send self :fft-counts nil)
        (setf (slot-value 'bin-counts) bc))
     (unless (slot-value 'bin-counts)
	     (setf (slot-value 'bin-counts)
		   (bin-data (send self :data)
			     :xrange (list (first (send self :x-values))
					   (car (last (send self :x-values))))
			     :method 'linear
			     :numbins (length (send self :x-values))
			     :outsiders 'delete))))
;(format *standard-output* "exiting bin-counts, bc: ~a, set: ~a, slot: ~a~%"
;	bc set (not (null (slot-value 'bin-counts))))
 (slot-value 'bin-counts))


(defmeth kde-proto :fft-counts (&optional (bc nil set))
"Accessor to slot fft-counts containing a pre-computed array"
(if set
    (setf (slot-value 'fft-counts) bc)
  (unless (slot-value 'fft-counts)
	  (setf (slot-value 'fft-counts)
		(build-fft-vector (send self :bin-counts)))))
(slot-value 'fft-counts))


(defmeth kde-proto :bw-ends (&optional list-min-max)
"sets (if arg list-min-max is given) or retrieves :bw-ends slot.
 These ends are used in various methods as the minimum and maximum
 bandwidths that make sense. This slot is reset when data
 or kernel-type change."
(unless (send self :data) (error "no hi ha dades en bw-ends" nil))
(when list-min-max
  (setf (slot-value 'bw-ends) list-min-max))
(when (not (slot-value 'bw-ends)) ; auto-setting bw-ends
  (if (eq 'r (send self :kernel-type))
      (setf (slot-value 'bw-ends) '(0.05 5))
    (setf (slot-value 'bw-ends)
	(* '(0.11 4) (first (send self :rule-of-thumb)))))
  (when (slot-value 'width-slider-dialog);we close it because his interval
					;is no longer valid
    (send (slot-value 'width-slider-dialog) :close)))
(slot-value 'bw-ends))


(defmeth kde-proto :estimates-y (&optional (lines nil set))
"sets or retrives the slot. When retrieving, if is nil,
 forces calculation."
(when set
      (when (and (slot-value 'estimates-y)
		 (slot-value 'show-previous-estimate))
	    (send self :push-estimate-as-previous))
      (setf (slot-value 'estimates-y) lines))
(unless (slot-value 'estimates-y)
	(send self :calc-and-set-estimates))
(slot-value 'estimates-y))

(defmeth kde-proto :kernel-type (&optional (type nil) (draw t))
"Sets or retrieves the kernel-type. For setting, you must give a
 character as argument. See *kernel-names* for valid ones,
but validity depends also on the computing method in use"
  (when type
    (if (assoc type *kernel-names*)
	(progn
	  (setf (slot-value 'kernel-type) type)
	  (send self :slot-value 'bw-ends nil)
	  (when draw (send self :redraw-window :force t)))
      (error "kernel-type not exists: " type)))
  (slot-value 'kernel-type))

(defmeth kde-proto :kernel-weights
  (&optional (type (send self :kernel-type))
	     (delta (- (second (send self :x-values))
		       (first (send self :x-values))))
	     (width (send self :bandwidth))
	     (canonical-rescaling (send self :use-canonical)))
"computes kernel-weights for warping computations.
Given a binwidth delta, returns a list of weigths."
 (let* ((kernel-data (assoc type *kernel-data*))
        (width (if canonical-rescaling
		   (* width
		       (nth 5 kernel-data))
		 width))
	(m (floor (/ (* width (nth 7 kernel-data))
		     delta)))
	(vals (* delta (iseq (1+ m))))
	(weights (funcall-kernel type vals width)))
   (append (reverse weights) (cdr weights)))
)

(defmeth kde-proto :bandwidth (&optional (width nil wset) (from-slider nil) (draw t))
"Sets (if given a number) or retrieves the current window width."
  (when width
        (setf (slot-value 'bandwidth) width)
        (when draw (send self :redraw-window :force t)))
  (when (and wset (null width) (slot-value 'data))
        (setf (slot-value 'bandwidth) (first (send self :rule-of-thumb))))
  (unless from-slider
	(when (slot-value 'width-slider-dialog)
	  (when width
	   (send (slot-value 'width-slider-dialog) :value width))))
(unless (slot-value 'bandwidth);;;*******************
(break))
  (slot-value 'bandwidth))

(defmeth kde-proto :variable-bandwidth (&optional (use nil set) (draw t))
"Sets (when given) or retreives the flag for using variable bandwidth technology
 Can be set to on-x-values or on-data-values"
;(print (list "var bw got args " use set draw))
(when set
;      (print (list "setting var bw to" use))
      (if (member use '(nil on-x-values on-data-values))
	  (setf (slot-value 'variable-bandwidth) use)
	(error "variable-bandwidth must be one of nil on-x-values on-data-values")))
(when use
      (unless (slot-value 'bw-transf)
	      (send self :create-bw-transformation)))
(when (and set draw) (send self :redraw-window :force t))
(slot-value 'variable-bandwidth))

(defmeth kde-proto :use-canonical (&optional (use nil set) (draw t))
"Sets (if given) or retrieves the current use canonically rescaled
kernel flag. Using canonically rescaled kernel means to have always the same
amount of smoothing for a given bandwidth, no matter the kernel in use."
(when set
      (setf (slot-value 'use-canonical) use))
(when (and (send self :kernel-type)
	   (slot-value 'use-canonical)
	   (eql (quote falta)
		(nth 5 (assoc (send self :kernel-type) *kernel-data*))))
      (let ((str (format t "~a~%~a"
			 "The selected kernel cannot be rescaled by now."
			 " Canonical rescaling will be turned off")))
	(message-dialog str)
	(send self :use-canonical nil t)))
(when (and set draw) (send self :redraw-window :force t))
(slot-value 'use-canonical))

(defmeth kde-proto :calc-method (&optional (meth nil set) (draw t))
"Sets (if given) or retrieves the current calculation method.
 1st arg can be 'direct, 'warp, 'fft or 'update."
(when set
      (unless (member meth '(direct warp fft update))
	      (error "method not known: ~a" meth))
      (setf (slot-value 'calc-method) meth)
      (when (and (eql meth 'direct)
		 (not (slot-value 'data-is-sorted))
		 (send self :data))
	    (send self :sort-data))
)
(when (and set draw) (send self :redraw-window :force t))
(slot-value 'calc-method))

(defmeth kde-proto :check-validity (&key (kernel (send self :kernel-type))
					 (calc-method (send self :calc-method))
					 (use-canonical (send self :use-canonical))
					 (variable-bandwidth
					  (send self :variable-bandwidth)))
"Checks if given kernel, calc-method and use canonical rescaling are
compatible. If they are, returns t, if not, returns a string message."
#| ei, problems here, must complete for variable-bandwidth |#
(let ((string "")
      (kdata (assoc kernel *kernel-data*)))
  (cond
   ((and (eql calc-method 'update)
	 (not (member kernel '(u a b w))))
    (setq string
	  (format nil
		  "~aKernel ~a ~a,~%~a.~%"
		  string
		  (nth 4 kdata)
		  "is not from the beta family"
		  "cannot be used in 'update computing mode."
		  )))
   ((and variable-bandwidth
	 (eql calc-method 'update))
    (setq string
;	  (format nil "~a~%~a"
;		  "Variable bandwidth not yet implemented"
;		  "with update computation method")
""))
   ((and variable-bandwidth
	 (not (member calc-method '(update direct))))
    (setq string
	  (format nil "~a~%~a"
		  "Variable bandwidth cannot be computed"
		  "with this computation method, use Update instead")))
   ((and (eq variable-bandwidth 'on-data-values)
	 (not (eq calc-method 'update)))
    (setq string
	  (format nil "~a~%~a"
		  "Bandwidth varying on data values"
		  "must use update computation method")))
   ((eql calc-method 'direct)
    (unless (assoc kernel *built-in-kernels*)
	    (setq string
		  (format nil
			  "~aKernel ~a ~a,~%~a.~%"
			  string
			  (nth 4 kdata)
			  "is not built-in in XLISPSTAT"
			  "cannot be used in 'direct computing mode"
			  ))))
  (t (setq string "")))
  (if (string= string "")
      t
    string)
))

(defmeth kde-proto :distr-dens (&optional (fun nil given))
"sets or retrieves 'distr-dens slot. Optional arg FUN must be a lambda function
 closure giving the density of theoretical distribution, accepting
 a x arg."
  (when given
        (setf (slot-value 'distr-dens) fun)
	(send self :distr-lines nil)
        (when fun
	      (setf (slot-value 'show-distr-lines) t))
        (if (slot-value 'window)
            (send self :redraw-window)))
  (slot-value 'distr-dens))

(defmeth kde-proto :distr-rand (&optional (fun nil given))
"sets or retrieves :distr-rand slot. It must be a lambda function
 closure for generating samples, accepting a count arg"
  (when given
        (setf (slot-value 'distr-rand) fun))
  (slot-value 'distr-rand))

(defmeth kde-proto :distr-lines (&optional (lines nil given))
"sets or retrieves :distr-lines slot. If a call for
 retrieving when slot is nil, will attempt to recalculate"
(if given
    (setf (slot-value 'distr-lines) lines)
  (when (not (slot-value 'distr-lines))
    (setf (slot-value 'distr-lines) (send self :calc-distr-lines))
    ))
  (slot-value 'distr-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some methods for file access

(when (featurep :unix)
      (defun set-file-dialog (prompt &optional (default ""))
	(prog ((filename (get-string-dialog prompt :initial default))
	       (file-exists nil)
	       (overwrite t))
	      (unless filename (return nil))
	      (setq file-exists
		    (eql 0 (system (strcat "test -f "
					   filename))))
	      (when file-exists
		    (setq overwrite
			  (ok-or-cancel-dialog (strcat "Overwrite "
						       filename "?"))))
	      (unless overwrite (return nil))
	      (return filename)))
      (defun open-file-dialog ();;;and override the standard one
	"code from Bernhard Walter, walter@pollux.edv.agrar.tu-muenchen.de"
	(require "getfile")
	(file-selector-dialog "*" ".")))


(defmeth kde-proto :read-data-file (&optional (filename (open-file-dialog))
					      (num-columns 1))
  (when filename
    (let ((data (read-data-columns filename num-columns)))
      (send self :data (first data)))
    
    (send self :redraw-window :force t)))

(defmeth kde-proto :save-confirmed (filename astream)
  (princ ";;this file has been created by a kde object, to reproduce itself"
      astream)
  (princ newline astream)
  (princ "(require \"kde\")" astream)
  (princ newline astream)
  (princ newline astream)
  (princ "(let ((myself (send kde-proto :new 2 " astream)
  (princ newline astream)
  (princ ":title " astream)
     (print (send self :title) astream)
  (princ newline astream)
  (princ ":sorted t" astream)
  (princ newline astream)
  (princ ":data '" astream)
     (princ (send self :data) astream)
  (princ newline astream)
  (princ ":x-values '" astream)
     (princ (send self :x-values) astream)
  (princ newline astream)
  (princ ":bin-counts '" astream)
     (princ (send self :bin-counts) astream)
     (princ newline astream)
  (princ ":kernel-type '" astream)
     (princ (send self :kernel-type) astream)
     (princ newline astream)
  (princ ":bandwidth " astream)
     (princ (send self :bandwidth) astream)
     (princ newline astream)
  (princ ":print t" astream)
     (princ newline astream)
  (princ ":show t)))" astream)
  (princ newline astream)
  (princ newline astream)

  (princ "(send myself :calc-method '" astream)
  (princ (send self :calc-method) astream)
  (princ " nil)" astream)
  (princ newline astream)
  (princ "(send myself :use-canonical '" astream)
  (princ (send self :use-canonical) astream)
  (princ " nil)" astream)
  (princ newline astream)
  (princ "(send myself :variable-bandwidth '" astream)
  (princ (send self :variable-bandwidth) astream)
  (princ " nil)" astream)
  (princ newline astream)
  (princ "(send myself :toggle-show-previous-estimate '" astream)
  (princ (send self :toggle-show-previous-estimate) astream)
  (princ " )" astream)
  (princ newline astream)

  (princ "(send myself :to-window :mouse-mode '" astream)
     (princ (send self :to-window :mouse-mode) astream)
  (princ ")" astream)
  (princ newline astream)

  (princ "(send myself :slot-value 'show-a-kernel " astream)
     (princ (send self :slot-value 'show-a-kernel) astream)
  (princ ")" astream)
  (princ newline astream)

  (princ "(send myself :slot-value 'show-data-points " astream)
     (princ (send self :slot-value 'show-data-points) astream)
  (princ ")" astream)
  (princ newline astream)

  (princ "(send myself :slot-value 'show-distr-lines " astream)
     (princ (send self :slot-value 'show-distr-lines) astream)
  (princ ")" astream)
  (princ newline astream)

  (princ "(send myself :bw-ends '" astream)
     (princ (send self :bw-ends) astream)
  (princ ")" astream)
  (princ newline astream)

  (princ "(send myself :show-window) (send myself :show-info-in-window)" astream)
  (princ newline astream)

  (princ "(print \"loaded kde object. Has been stored in **\")" astream)
  (princ newline astream)
  (princ "(setq * myself)" astream)

  (princ ")" astream)
  (princ newline astream)

  (princ newline astream)
)


(defmeth kde-proto :save-self (&key (file
				     (set-file-dialog "Save object in:"
						      (send self :title)))
				    (print t))
  (when file
    (let ((astream (open file :direction :output)))
      (if (not astream) nil
	(progn (send self :save-confirmed file astream)
	       (close astream)
	       (when print
		 (format t ;; t is a short for *standard-output* in format!
			 "; Object saved in file ~d~%" file))
	       t)))))

(defmeth kde-proto :to-gnuplot ()
  "Creates gnuplot files for drawing the window"
  (require "gnuplot")
  (send self :to-window :to-gnuplot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; computation methods


(defmeth kde-proto :sort-data (&key (force t))
"will sort the data if it is not already sorted, or if :force t"
(when (send self :data)
      (when (or force (not (send self :data-is-sorted)))
	    (when *kde*computing*inspect*
		  (princ (format nil "sorting data, length ~a ~%"
				 (length (send self :data)))))
	    (setf (slot-value 'data)
		  (sort-data (slot-value 'data)))
	    (setf (slot-value 'data-is-sorted) t))))

(defmeth kde-proto :describe-data (&key (print nil) (bins nil))
"args key (print nil)
 Returns (and print if given :print t) a list of
 0.- a list of length, mean and stdev of current data
 1.- a list of the five quartiles. If :bin key is a number
 returns also a list of counted data by bins.
 If bins is zero, reports a count of repeteated data."
   (let* ((data (send self :data))
          (quarts (butlast (send self :fivnums) 2))
          (len (car (last (send self :fivnums))))
          (mean (mean data))
          (sd (standard-deviation data))
	  (reps nil))
     (when print
       (terpri)
       (princ "N, mean, stdev: ")
       (princ (list len mean sd))
       (terpri)
       (princ "quartiles: ")
       (princ quarts)
       (terpri))
     (list (list len mean sd) quarts)))

(defmeth kde-proto :integrate ()
"for eay checking, computes the integral of the estimated density"
(integrate (transpose
	    (mapcar #'(lambda (l) (coerce l 'list))
		    (send self :calc-estimates :want-pairs t)))))


(defmeth kde-proto :resample-data (&optional (num nil set))
"Args: &optional num
 If there is random generator installed, new random data
 are generated from it. If num is given, will be the data length
 to use, if not given, the actual data size is used"
  (let ((num (if set
                 num
	       (length (send self :data)))))
    (unless (slot-value 'distr-rand)
      (error "there is no random generator." nil))
    (send self :data
	  (funcall (slot-value 'distr-rand) num))
    (send self :redraw-window :clear t)
    (slot-value 'data)))

(defmeth kde-proto :add-bootstrap-line ()
  (let* ((width (slot-value 'bandwidth))
        (type (slot-value 'kernel-type))
	(dat (slot-value 'data))
	(leng (length dat))
	(lines (send self :calc-estimates :data (sample dat leng t)
		       :want-pairs t)))
    (send self :to-window :add-lines lines )
    lines))

(defmeth kde-proto :calc-and-set-estimates (&key (force nil))
"if estimates-y is nil (or :force is t)
 calculates it and sets the slot. Returns the lines"
(when (and (slot-value 'show-previous-estimate)
	   force
	   (slot-value 'estimates-y))
      (send self :push-estimate-as-previous))
(when (or force (not (slot-value 'estimates-y)))
      (setf (slot-value 'estimates-y)
	    (send self :calc-estimates)))
(when (and (slot-value 'distr-dens)
	   (or force (not (slot-value 'distr-lines))))
      (send self :calc-distr-lines))
(slot-value 'estimates-y))

(defmeth kde-proto :calc-estimates (&key (data (slot-value 'data))
					 (width (slot-value 'bandwidth))
					 (type (slot-value 'kernel-type))
					 (xvals (send self :x-values))
                                         (method (send self :calc-method))
					 (variable-bandwith
					  (send self :variable-bandwidth))
					 (want-pairs nil))
"Returns the kernel density estimation for data, width, type keys given
 (default are slots values), If key want-pairs is t, returns a list
 of xvalues and yvalues. If not (default) returns only yvalues."
(cond
 ((eql method 'warp);;WARP METHOD <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  (calc-kernel-density-warping data width type xvals method want-pairs self))
 ((eql method 'direct);;DIRECT METHOD <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  (case variable-bandwith
      (on-x-values (calc-kd-direct-var-bw data nil type xvals
				method want-pairs self))
      (on-data-values (error "var bw with direct method not implemented"))
      (nil (calc-kernel-density-direct data width type xvals
				method want-pairs self))))
 ((eql method 'update);;<<<<<<<<<<<<UPDATE METHOD
  (case variable-bandwith
      (on-x-values (calc-kd-update-var-bw-x data
					    (exp (send (slot-value 'bw-transf)
						       :transform xvals
						       :want-pairs nil))
					    type xvals method want-pairs self))
      (on-data-values (calc-kd-update-var-bw-data data
						  (exp (send (slot-value 'bw-transf)
							     :transform xvals
							     :want-pairs nil))
						  type xvals method want-pairs self))
      (nil (calc-kernel-density-update data width type xvals
				       method want-pairs self))))
 ((eql method 'fft);;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<FFT METHOD 
  (calc-kernel-density-fast-fourier-transform nil width type xvals
			      want-pairs self nil))
 (t (error "computation method ~a not known in :calc-method" method ))))

(defmeth kde-proto :push-estimate-as-previous ()
"saves currents estimates in the previous-estimate slot"
(when (and (slot-value 'estimates-y)
	   (slot-value 'show-previous-estimate))
      (let ((prest (slot-value 'previous-estimates-y))
	    nw)
	
	(if (> (length prest) 2)
	    (setf (slot-value 'previous-estimates-y)
		  (list (slot-value 'estimates-y)
			(first prest)
			(second prest)))
	  (progn
	    (unless prest
		(setf (slot-value 'previous-estimates-y) (list nil)))
	    (push (slot-value 'estimates-y)
		  (slot-value 'previous-estimates-y)))))))

(defmeth kde-proto :calc-and-set-silverman ()
  (send (slot-value 'silverman-graph) :clear-lines :draw nil)
  (send (slot-value 'silverman-graph)
	:add-lines (transpose (second-derivative
			       (transpose (list
					   (send self :x-values)
					   (send self :estimates-y)))))
	:draw t))

(defmeth kde-proto :silverman-test-graph (&optional (band
						     (send self :bandwidth)))
  (if (slot-value 'silverman-graph)
      (send (slot-value 'silverman-graph) :show-window)
    (when (send self :force-gaussian-kernel)
	  (require "my-plot-lines" "plotline")
	  (let ((pl (my-plot-lines '(0 0) '(1 1)
				:title "Silverman test graph"))
		(me self))
	    (send self :slot-value 'silverman-graph pl)
	    (defmeth pl :close ()
	      (send me :slot-value 'silverman-graph nil)
	      (call-next-method))
	    (send self :redraw-window :force t)
	    (send pl :adjust-to-data)))))

(defmeth kde-proto :kernel-data ()
"Gives a list of integral of squared kernel, and int of t^2 k(t)
 it looks at use-canonical... Uses global var *kernel-data*"
   (let* ((type (send self :kernel-type))
          (kd (assoc type *kernel-data*))
          (isk (second kd))
          (mu2 (third kd))
          (delta (nth 5 kd)))
     (if (send self :use-canonical)
         (progn (setf isk (/ isk delta))
                (setf mu2 (* delta delta mu2))
                (list isk mu2))
         (list isk mu2))))

(defmeth kde-proto :rule-of-thumb
         (&key (showing nil) (print nil) (data (send self :data)))
"args key (showing nil) (print nil)
 Computes the assymptotical ideal bandwidth based in a reference to estimated
 normal distribution. If kernel is gaussian, gives also a robust version."
  (let* ((type (send self :kernel-type))
;;         (kd (assoc type *kernel-data*))
;;         (rk (second kd))
;;         (k2 (third kd))
         (kd (send self :kernel-data))
         (k2 (second kd))
         (rk (first kd))
         (n5 (expt (length data) -0.2))
         (sig (standard-deviation data))
;based on stdev
;         (bw1 (* (expt (/ rk (* k2 k2 0.2116)) .2)
;                 sig n5))
;based on interquartile range
	 (rg (send self :fivnums))
	 (irg (- (fourth rg) (second rg)))
         (bw1 (* (expt (/ rk (* k2 k2)) .2)
                 1.0113 irg n5))
;robust version, silverman 86 p. 48.
         (bw2 (if (eql type 'h)
		  (* .9 (min sig (/ irg 1.349)) n5)
		nil)))
      (when showing
         (send self :bandwidth bw1)
         (send self :show-info-in-window
                    (format
		     nil
		     "Optimal bandwith, normal reference (rule-of-thumb)")
                    (format nil "h = ~a"
			    (format-digits bw1 *kde*significant*digits*))
                    (if bw2 (format nil "robust: ~a"
				    (format-digits bw2
						   *kde*significant*digits*))
                            "")
                    ))
      (when print
	    (format t "Opt. bw. normal-ref: ~,4g; robust: ~,4g~%" bw1 bw2))
      (list bw1 bw2)))

(defmeth kde-proto :hsjm-selector (&key (showing nil))
  (when showing
	(send self :show-info-in-window
	      nil
	      "Computing the Hall-Sheater-Jones-Marron band selector"
	      ""))
  (let ((band (hsjm (send self :data)
		    (apply #'- (select (send self :fivnums) '(3 1))))))
    (when showing
	  (send self :bandwidth band)
	  (send self :show-info-in-window
		nil
		"bandwidth selected by hsjm method"
		"")
	  )
    band))

(defmeth kde-proto :iterate-plug-in (&key (precision 0.1)
					     (print nil)
					     (max-iterates 30)
					     (fun #'pm-plug-in)
					     (name "Park and Marron"))
  "iterates pm-plug-in up to :precision (default 0.001)
or :max-iterates (default 30)"
  (when (send self :force-gaussian-kernel)
    (let ((dt (send self :data))
	  (fun (if (functionp fun)
		   fun
		 (eval fun))))
      (do* ((prev-bw (send self :bandwidth))
	    (new-bw  prev-bw);update in body
	    (curr-prec 99999999.
		       (abs (/ (- prev-bw new-bw) new-bw)))
	    (curr-iter 0
		       (+ 1 curr-iter))
	    (maxreached nil nil)
	    (done nil
		  (or (< curr-prec precision) (> curr-iter max-iterates))))
	  (done ; test and results
	   (when (> curr-iter max-iterates)
	     (print "max-iterates reached in :iterate-plug-in"))
	   (list new-bw maxreached)
	   (send self :show-info-in-window
		 nil
		 (format nil "bandwidth selected by ~a method" name)
		 (format nil "iterations: ~a" curr-iter)))
	;body
	(send self :show-info-in-window
	      nil
	      (format nil "iterating ~a plug-in method..." name)
	      "")
	(setq prev-bw new-bw)
	(setq new-bw (send self :bandwidth
			   (funcall fun dt prev-bw )))
	(when print
	  (princ (list "iter" curr-iter "\n" )))))))

(defmeth kde-proto :binned-sjpi-iterated
  (&key (bandwidth (send self :bandwidth))
	(print t) (show nil)
	(maxit 15)
	(tol 0.01))
  (when (send self :force-gaussian-kernel)
	(let* ((xvals (send self :x-values))
	       (xmin (first xvals))
	       (delta (- (second xvals) xmin))
	       (numx (length xvals))
	       (bincounts (coerce (send self :bin-counts) 'vector))
	       (fn (send self :fivnums))
	       (iqr (- (fourth fn) (second fn)))
	       (datalen (car (last fn)))
	       (itnum 0)
	       (again t)
	       new)
	  (when show
		(send self :show-window)
		(send self :show-info-in-window
		      "Iterating for Sheather-Jones bandwidth..."
		      (format nil "from bw= ~a" bandwidth) ""))
	  (while again
	    (when print
		  (princ (format nil "iteration n. ~a from ~a "
				 (1+ itnum) bandwidth)))
	    (time (setq new
		  (+ bandwidth (binned-sj-plug-in xmin delta numx
						  bincounts iqr
						  bandwidth datalen))))
	    (when print
		  (princ (format nil "to ~a ~%" new)))
	    (when (and show (complexp new))
		  (message-dialog
		   "error in iteration. Try starting from a high value."))
	    (when (complexp new)
		  (error "error in iteration. Try starting from a high value."))
	    (when (or (< (abs (/ (- new bandwidth) new)) tol)
		      (= itnum maxit))
		  (setq again nil)
		  (when (and show (= itnum maxit))
			(message-dialog
			 "Max. num. of iterations reached, don't trust results.")))
	    (increment itnum)
	    (when (and again show)
		  (send self :show-info-in-window
			"Iterating for Sheather-Jones bandwidth..."
			(format nil "iteration n. ~a" itnum)
			(format nil "from bw= ~a to ~a" bandwidth new)))
	    (setq bandwidth new))
	  (when print
		(print (format nil "final value: ~a" new)))
	  (when (send self :use-canonical)
		(print "correction for canonical rescaling")
		(setq new (* new
			     (nth 5 (assoc 'h*kernel-data* )))))
	  (when show
		(send self :bandwidth new)
		(send self :show-info-in-window
		      nil
		      "this bandwidth approx. selected by Sheather Jones plug-in"
		      ""))
	  (when print
		(print (format nil "final value: ~a" new)))
	  new)))

(defmeth kde-proto :sheather-jones-bandwidth (&key (show nil) (print nil))
"Args: (&key (show nil) (print nil))
 Computes the sheather and jones bandwidth selector and, if SHOW, installs it.
 When PRINT, prints verbose information."
(when (send self :force-gaussian-kernel)
      (let* ((xvals (send self :x-values))
	     (xmin (first xvals))
	     (delta (- (second xvals) xmin))
	     (numx (length xvals))
	     (bincounts (coerce (send self :bin-counts) 'vector))
	     (fn (send self :fivnums))
	     (iqr (- (fourth fn) (second fn)))
	     (datalen (car (last fn)))
	     (itnum 0)
	     (again t)
	     new)
	(when show
	      (send self :show-window)
	      (send self :show-info-in-window
		    "Computing Sheather-Jones bandwidth..."
		    "by Newton-Raphson method, wait..."
		    ""))
	(when print
	      (format t "~a~a~%"
		      "Computing Sheather-Jones bandwidth..."
		      "by Newton-Raphson method, wait..."))
	(setq new (sheater-jones-bandwidth (send self :data) iqr))
	(when show
	      (send self :bandwidth new)
	      (send self :show-info-in-window
		    nil
		    "this bandwidth selected by Sheather Jones"
		    "solve the equation method."))
	(when print
	      (format t "final value: ~a~%" new))
	new)))

;;Devroye's double kernel method
"Computes the bandwidth selected by the data following Luc Devroye's double kernel method."
(defmeth kde-proto :double-kernel (&key (bw-ends (send self :bw-ends) bwset)
					(num-values 70)
					(norm (progn (require "function-norms"
							      "funnorms.lsp")
						     #'l1-norm))
					(plot t)
					(showing t)
					(debug nil)
					(print nil))
  (let* ((data (send self :data))
	 (xvals (send self :x-values))
	 from to bands
	 lines-a lines-r dists
	 minima
	 minmin
	 (user
	  (if (and (eql 'a (send self :kernel-type))
		   (not (send self :use-canonical)))
	      t
	    (ok-or-cancel-dialog
	     (format nil "~a~%~a~%~a"
		     "This method works with"
		     "Bartlett-Epan. kernel, no canon-resc." "Install it?")))))
    (when user
	  (when (send self :use-canonical)
		(send self :use-canonical nil))
	  (unless (eql 'a (send self :kernel-type))
		  (send self :kernel-type 'a)
		  (unless bwset
			  (setq bw-ends (send self :bw-ends))))
	  (when debug
		(print (format nil
			       "using ~d bands in ~a\n" num-values bw-ends)))
	  (when showing
		(send self :show-info-in-window
		      "Searching minima for Devroye's double kernel"
		      (format nil "~a values in ~a" num-values bw-ends)
		      ""))
	  (setq from (first bw-ends))
	  (setq to (second bw-ends))
	  (setq bands (logseq from to num-values))
	  (when *kde*computing*inspect*
		(princ
		 (format nil
			 "Computing 2nd order kernel estimation for ~a ~a~%"
			 num-values
			 " bandwidth values...")))
	  (setq lines-a
		(mapcar #'(lambda (h)
			    (send self :calc-estimates :width h :xvals xvals))
			bands))
	  (when *kde*computing*inspect*
		(princ
		 (format nil
			 "Computing 4th order kernel estimation for ~a ~a~%"
			 num-values
			 " bandwidth values...")))
	  (setq lines-r
		(mapcar #'(lambda (h)
			    (send self :calc-estimates :width h :xvals xvals
				  :type 'r))
			bands))
	  (setq dists (- lines-a lines-r))
	  (setq dists
		(mapcar #'(lambda (lines)
			    (funcall norm (transpose (list xvals lines))))
			dists))
	  (when plot
		(require "my-plot-lines" "plotline")
		(send self :to-window :add-subordinate 
		      (my-plot-lines bands dists :title "Score vs. bandwidth")))
	  (setq minima (find-local-minima (transpose (list bands dists))))
	  (setq minmin (find-minimum minima))
	  (when print
		(print (format nil "using ~d bands in ~a" num-values bw-ends))
		(print (format nil "found minima: ~a" minima))
	(print (format nil "minimum: ~a" minmin)))
	  (when showing
		(if (first minmin)
		    (progn
		      (send self :bandwidth (first minmin))
		      (send self :show-info-in-window
			    nil
			    "minimizes Devroye's double kernel criterion"
			    (format nil "l1-distance between estimations: ~d"
				    (second minmin))))
		  (send self :show-info-in-window
			"No local minimum found by Devroye's double kernel method"
			"" "")))
	  (first minmin))))


(defmeth kde-proto :minimize-lscv(&key (num-values 50) (print nil)
					(plot nil) (showing nil))
  (when (send self :force-gaussian-kernel)
    (let* ((data (send self :data))
	   (from (first (send self :bw-ends)))
	   (to (second (send self :bw-ends)))
	   (bands (logseq from to num-values))
	   (cvs (mapcar #'(lambda (x) (lscv data x)) bands))
	   (points (transpose (list bands cvs)))
	   (minima (find-local-minima points))
	   (maxmin (when minima (caar (last minima)))))
      (when print
	(print (transpose (list bands cvs)))
	(print "Local minima:")
	(print minima))
      (when plot
	(require "my-plot-lines" "plotline")
	(send self :to-window :add-subordinate (my-plot-lines bands cvs)))
      (when showing
	(if (null maxmin)
	    (send self :show-info-in-window
		  "No local minima found in the current interval" "" "")
	  (progn (send self :bandwidth maxmin)
		 (send self :show-info-in-window
		       nil
		       "minimizes least squares cross-val."
		       ""))))
      maxmin)))


(defmeth kde-proto :calc-probability-between (xmin xmax)
"Calculates probability for xmin < x < xmax based on the current density
 estimation. Value is approximate, of course."
  (let* ((list (transpose (list (send self :x-values)
                                (send self :estimates-y))))
         (list (REMOVE-IF #'(lambda (x)
                              (or (< (first x) xmin) (> (first x) xmax)))
                          list))
         (prob 0))
    (when (< xmin xmax)
          (setq prob (mapcar #'(lambda (pair1 pair2)
                                 (* (- (first pair2) (first pair1))
                                    (/ (+ (second pair1) (second pair2))
                                       2)))
                             (butlast list)
                             (rest list)))
          (setq prob (apply #'+ prob)))
    prob))


(defmeth kde-proto :calc-distr-lines ()
"calculates the lines for graphing the theoretical density
 if distr-dens and x-values both exists"
(let ((xv (send self :x-values))
      (dd (slot-value 'distr-dens)))
  (when (and xv dd)
    (mapcar dd xv))))

(defmeth kde-proto :do-bootstrap (&optional (num 1000))
"fills the slots :bootstrap-lines and :bootstrap-quant-lines"
  (let ((lines nil)
	(quants nil))
    (setf (slot-value 'bootstrap-lines) nil)
    (setf (slot-value 'bootstrap-quant-lines) nil)
    (dotimes (i num)
      (setq lines (append lines (last (send self :add-bootstrap-line)))))
    (setf (slot-value 'bootstrap-lines) lines)
    (setq lines (transpose lines))
    (setq quants (mapcar #'quartiles lines))
    (setf (slot-value 'bootstrap-quant-lines) (transpose quants))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kde-proto methods for computing and showing
;;; distances to theoretical distribution

(defmeth kde-proto :calc-distances (&key (norm (send self :choose-distance))
				    (plot t)
				    (showing nil)
				    (bw-ends (send self :bw-ends))
				    (bw-num 100))
"keys (norm (send self :choose-distance))  (plot t)
  (showing nil) (bw-ends (send self :bw-ends))  (bw-num 100).
 computes the distance between theor distr and kde estimation."
  (let* ((error (unless (slot-value 'distr-dens)
		  (error "no theoretical distribution!" nil)))
	 (xv (send self :x-values))
	 (bw-values (apply #'rseq (append bw-ends (list bw-num))))
	 (res  bw-values)
	 (dists (mapcar #'(lambda (x)
			    (send self :calc-one-distance :norm norm
				  :showing showing :band x :x-values xv))
			bw-values)))
    (when norm
      (when plot
	(require "my-plot-lines" "plotline")
	(send self :to-window :add-subordinate (my-plot-lines  bw-values dists)))
      dists)))

(defmeth kde-proto :calc-one-distance (&key (norm  (progn
						     (require "function-norms"
							      "funnorms.lsp")
						     #'l2-norm))
					    (showing nil)
					    (band (slot-value 'bandwidth))
					    (x-values (send self :x-values)))
  (let* ((d-lines (send self :distr-lines))
	 (lines (if showing
		    (progn (send self :bandwidth band)
			   (send self :estimates-y))
		  (send self :calc-estimates :width band :xvals x-values)))
	 (dist (funcall norm (transpose (list x-values (- lines d-lines))))))
    (when showing
             (send self :bandwidth band)
             (send self :show-info-in-window
			(format nil "h:     ~a" band)
			(format nil "dist:  ~a" dist)
                        (format nil "Norm: ~a" norm)))
    dist))

(defmeth kde-proto :minimize-distance-to-theor-distr
  (&key (norm (send self :choose-distance))
	(showing t)
	(ends (send self :bw-ends))
	(k-type (send self :kernel-type))
	(verbose nil)
	(tolerance 0.01))
  (when norm
	(when showing (send self :to-window :redraw))
	(let* ((data (send self :data))
	       (d-lines (send self :distr-lines))
	       (err (when (not d-lines)
			  (error "There is no theoretical distrib." nil)))
	       (xv (send self :x-values))
	       (stringa "golden-searching...")
	       (sol
		(flet ((fun (band)
			    (when showing
				  (send self :bandwidth band)
				  (send self :show-info-in-window nil
					stringa ""))
			    (funcall norm
				     (transpose
				      (list xv
					    (abs (- (send self :calc-estimates
							  :width band
							  :type k-type)
						    d-lines)))))))
		      (golden-search #'fun
				     (first ends)
				     (second ends)
				     :tolerance tolerance
				     :verbose verbose)))
	       (res (first sol)))
	  (send self :bandwidth res)
	  (send self :show-info-in-window
		nil
		"minimizes distance to theor. distribution"
		(format nil "distance: ~a (~a)"
			(second sol) norm))
	  sol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; drawing methods

(defmeth kde-proto :redraw-window (&key (force nil) (clear t) (only-info nil))
  (unless only-info
    (if (send self :slot-value 'bootstrap-quant-lines)
        (send self :draw-bootstrap-quants)
      (send self :draw-estimates :force force :clear clear)))
  (send self :to-window :redraw-content)
  (when force
	;; (send self :to-window :adjust-to-data)
	(send self :show-info-in-window))
  (send self :draw-info-in-window))

(defmeth kde-proto :draw-info-in-window ()
  (if (slot-value 'window)
      (send self :to-window :draw-info-strings)
    ;;(print (slot-value 'info-strings))
))

(defmeth kde-proto :draw-data ()
"draw a cross for each data point in top of window"
(let* ((w (slot-value 'window))
       (ym (second (send w :scaled-range 1)))
       (dt (send self :data)))
  (mapcar #'(lambda (x)
	      (apply #'send w :draw-symbol 'cross nil
		     (send w :real-to-canvas x ym)))
	  dt)))

(defmeth kde-proto :draw-estimates (&key (clear t) (force nil))
  (when (slot-value 'window)
	(when clear
	      (send self :to-window :clear-lines :draw nil))
	(when (slot-value 'distr-dens)
	      (send self :draw-distr-lines))
	(when (slot-value 'show-a-kernel)
	      (send self :draw-kernel-sample))
	(when (slot-value 'histogram-slider)
	      (send self :draw-histogram)
	      )
	(when force
	      (send self :estimates-y nil))
	(when (and (slot-value 'show-previous-estimate)
		   (car (slot-value 'previous-estimates-y))
		   (= (length (send self :x-values))
		      (length (car (slot-value 'previous-estimates-y)))))
	      #+color	(send self :to-window :add-lines
			      (list (send self :x-values)
				    (car (slot-value 'previous-estimates-y)))
			      :draw nil :color 'blue)
	      #-color (send self :to-window :add-lines
			    (list (send self :x-values)
				  (car (slot-value 'previous-estimates-y)))
			    :draw nil :type 'dashed)
	      )
	(when (slot-value 'show-kernel-estimates)
	      (send self :to-window :add-lines
		    (list (send self :x-values) (send self :estimates-y))
		    :draw nil))
	(when (slot-value 'silverman-graph)
	      (send self :calc-and-set-silverman))
	(slot-value 'estimates-y)))

(defmeth kde-proto :toggle-show-previous-estimate (&optional (show nil set)
							     (draw t))
"accessor to the show-previous-estimate slot, it is a flag that decides
 if a line for the previous estimate mustbe shown"
(if  set (setf (slot-value 'show-previous-estimate) show)
  (setf (slot-value 'show-previous-estimate)
	(not (slot-value 'show-previous-estimate))))
(when draw
      (send self :redraw-window :force t))
(slot-value 'show-previous-estimate))

(defmeth kde-proto :toggle-show-distr-lines (&optional (value nil set))
    (if set (setf (slot-value 'show-distr-lines) value)
            (setf (slot-value 'show-distr-lines)
                  (not (slot-value 'show-distr-lines))))
    (send self :redraw-window)
    (slot-value 'show-distr-lines))

(defmeth kde-proto :toggle-show-a-kernel (&key (force nil set))
    (if set (setf (slot-value 'show-a-kernel) force)
            (setf (slot-value 'show-a-kernel)
                  (not (slot-value 'show-a-kernel))))
    (send self :redraw-window)
    (slot-value 'show-a-kernel))

(defmeth kde-proto :toggle-show-data-points (&key (force nil set))
    (if set (setf (slot-value 'show-data-points) force)
            (setf (slot-value 'show-data-points)
                  (not (slot-value 'show-data-points))))
    (send self :redraw-window)
    (slot-value 'show-data-points))

(defmeth kde-proto :draw-kernel-sample
  (&optional (xval (mean (send self :to-window :scaled-range 0)) xvalset)
             (scale-factor 0.1)
	     (width (if (send self :variable-bandwidth)
			(first (exp (send (slot-value 'bw-transf)
				     :transform (list xval) :want-pairs nil)))
		      (send self :bandwidth))))
  (let* ((xv (send self :x-values))
	 (delta (- (second xv) (first xv)))
	 (kd (assoc (send self :kernel-type) *kernel-data*))
	 (supp (nth 7 kd))
	 (lamb (eval (nth 6 kd)))
	 (width (if (send self :use-canonical)
		    (* width (nth 5 kd))
		  width))
;;	 (num (1+ (floor (/ (* width supp) delta))))
	 (num (floor (/ (* width supp) delta)))
	 (xlist (* delta (iseq (- num) num)))
	 (lines (mapcar #'(lambda (x)
                              (list x (/ (funcall lamb (/ x width)) width)))
			xlist)))
    (when lines
	  (setf lines (transpose lines))
	  (setf (first lines) (+ xval (first lines)))
	  (setf (second lines) (* (second lines) scale-factor))
	  #-color (send self :to-window
			:add-lines lines :type 'dashed :draw nil)
	  #+color (send self :to-window
			:add-lines lines :color 'green :draw nil)))
  ;this trick is for drawing 2 more kernel graphs when variable bandwidth
  (unless xvalset
	  (when (send self :variable-bandwidth)
		(send self :draw-kernel-sample
		      (sum (* '(0.25 0.75)
			      (send self :to-window :scaled-range 0))))
		(send self :draw-kernel-sample
		      (sum (* '(0.75 0.25)
			      (send self :to-window :scaled-range 0)))))))


(defmeth kde-proto :draw-distr-lines ()
"If slot :distr-dens is defined to a density function corresponding
to the theoretical density originating the data, this method draws
the graph of the function"
  (if (slot-value 'distr-dens)
    (let ((lines (send self :distr-lines)))
      (when (and lines (slot-value 'show-distr-lines))
#-color       (send self :to-window :add-lines (list (send self :x-values)
					  lines) :type 'dashed :draw nil)
#+color       (send self :to-window :add-lines (list (send self :x-values)
					  lines) :color 'magenta :draw nil)
))
    (message-dialog "There is no theoretical\ndensity function")))


(defmeth kde-proto :draw-bootstrap-quants ()
  (let ((lines (slot-value 'bootstrap-quant-lines))
	(xv (send self :x-values))
	(curr-lines nil)
	(win (slot-value 'window))
#+color (kind :color)
#-color	(kind :type)
#+color (line-types '(cyan blue green))
#-color (line-types '(dashed solid dashed))
)
    (unless lines
      (error "draw-bootstrap-quants: call do-bootstrap before!" nil))
    (setq curr-lines (cons xv (list (nth 0 lines))))
    (send win :add-lines curr-lines kind (first line-types))
    (setq curr-lines (cons xv (list (nth 1 lines))))
    (send win :add-lines curr-lines kind (second line-types))
    (setq curr-lines (cons xv (list (nth 2 lines))))
    (send win :add-lines curr-lines kind (third line-types))
    (setq curr-lines (cons xv (list (nth 3 lines))))
    (send win :add-lines curr-lines kind (second line-types))
    (setq curr-lines (cons xv (list (nth 4 lines))))
    (send win :add-lines curr-lines kind (first line-types)))
  )


(defmeth kde-proto :calc-and-draw-bootstrap-quants ()
(message-dialog "This doesn't works rigth now"))
; must reprogram to work with binned methods
; and do not-naive bootstrap
;  (if (send self :slot-value 'bootstrap-quant-lines)
;      (progn
;	(send self :slot-value 'bootstrap-quant-lines nil)
;	(send self :redraw-window :clear t))
;    (let ((num (get-value-dialog "number of samples:" :initial 100)))
;      (when num
;	(send self :do-bootstrap (first num))
;	(send self :to-window :clear-lines :draw nil)
;	(send self :draw-bootstrap-quants)
;	; get off with it, for the moment I do not want it for anything else
;	(setf (slot-value 'bootstrap-lines) t))))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface methods and objects for kde-proto

(defmeth kde-proto :choose-kernel ()
"Gives user chance to choice kernel type via dialog"
  (let* ((kerns '((u . "Uniform")
		(t . "Triangular")
;;		(e . "Epanechnikov")
		(a . "Bartlett-Epanechnikov")
                (b . "Biweight")
		(w . "Triweight")
		(h . "Gaussian")		     		
		(r . "Order 4 kernel")))

	 (types (mapcar #'car kerns))
	 (names (mapcar #'cdr kerns))
	 (using-canon (send self :use-canonical))
	 (current (position (send self :kernel-type) types))
         (dlg (send choose-item-w-toggle-dialog-proto :new
                  "Kernel Type" names "Use canonical rescaling"
		  :initial current
		  :toggle-value using-canon
		  :show nil))
	 (reply t)
         (valid nil))
    (send self :to-window :locate-dialog dlg)
    (loop
	(when (or (null reply) (eql t valid))
           (return))
        (setq reply (send dlg :modal-dialog))
        (when reply
          (setq valid (send self :check-validity
                         :kernel (select types (first reply))
                         :use-canonical (second reply)))
          (when (stringp valid)
	    (message-dialog valid))))
    (when (eql t valid)
	  (send self :kernel-type (select types (first reply)) nil)
	  (send self :use-canonical (second reply) t)
	  (send self :show-info-in-window
		nil "" ""))))

(defmeth kde-proto :choose-bandwidth (&optional (w (send self :bandwidth)))
"Gives user a dialog to type in a bandwidth, then sets it to :bandwidth
 slot. If given an argument, it will be the default for dialog
If the user types two values, they will be used
to set a new bandwidth range for use
by the bandwidth slider and some automatic selector methods."
;;;this must be changed to include choosing variable/fixed bandwidth
(if (send self :variable-bandwidth)
    (message-dialog
     (format nil "~a~%~a"
	     "Bandwidth is set to be variable, see computing method."
	     "You can control it with Local bandwidth controller"))
  (let* ((ends (send self :bw-ends))
	 (string (strcat "Type a new bandwidth:"
			 newline
			 "(or a new bandwidth range)"))
	 (dlg (send get-string-dialog-proto :new
		    string :initial w :show nil)))
    (send self :show-info-in-window nil
	  (format nil "current bandw. range: ~a ~a"
		  (first ends) (second ends))
	  "")
    (send self :to-window :locate-dialog dlg)
    (setq w (send dlg :modal-dialog))
    (when w
	  (setq w (eval (read (make-string-input-stream
			       (strcat< "(list " w ")")) nil)))
	  (cond ((cdr w)
		 (send self :bw-ends w))
		(t
		 (setq w (first w))
		 (send self :bandwidth w)
		 (cond ((< w (first ends))
			(setf (first ends) w))
		       ((> w (second ends))
			(setf (second ends) w)))))
	  (send self :show-info-in-window
		nil "" "")))))

(defmeth kde-proto :choose-distance ()
"displays a dialog for choosing the norm to use, returns it or nil if cancel."
(require "function-norms" "funnorms.lsp")
(let* ((loc (send self :to-window :location))
       (xpos (+ (nth 0 loc)
		(nth 0 (send self :to-window :size))
		15))
       (ypos (nth 1 loc))
       (d (apply #'send choose-item-dialog-proto :new
		 "Distance to apply:"
		 (list "l0-norm" "l1-norm" "l2-norm" "dl1-dist" "dl2-dist")
		 :show nil
		 :initial 2 nil))
       (pos (apply #'send d :location loc))
       (reply (send d :modal-dialog)))
  (if reply
      (select (list #'l0-norm #'l1-norm #'l2-norm )
	      ;;#'dl1-norm #'dl2-norm
	      reply)
    nil)))

(defmeth kde-proto :choose-calc-method (&key (showing nil))
"displays a dialog for choosing the computation method to apply
when computing kernel density estimates."
  (let* ((mets '(direct warp update fft))
	 (metsstr '("Direct" "Binning data (Warp)"
		    "Updating" "Binning and FFT"))
	 (bwopts '(nil on-x-values on-data-values))
	 (bwoptstr '("Fixed bandwidth" "Variable, depending on x value"
		     "Variable, depending on data points"))
	 (prompt-item1 (send text-item-proto :new "Computation method"))
	 (prompt-item2 (send text-item-proto :new "Choose an option for bandwidth"))
	 (choose-calc (send choice-item-proto :new metsstr :initial
			    (position (send self :calc-method) mets)))
	 (choose-bw (send choice-item-proto :new bwoptstr :initial
			  (position (send self :variable-bandwidth) bwopts)))
         (d (send ok-or-cancel-dialog-proto :new
		  (list prompt-item1 choose-calc prompt-item2 choose-bw)
		  :show nil))
	 (reply t)
         (valid nil)
	 (tit (send d :title "Computation method:"))
	 (pos (send self :to-window :locate-dialog d))
	 met opt)
    (send d :show-window)
    (send choose-calc :value (position (send self :calc-method) mets))
    (send choose-bw :value (position (send self :variable-bandwidth) bwopts))
    (send (send d :slot-value 'xlisp::ok-button)
	  :action
	  #'(lambda ()
	      (setf met (send choose-calc :value))
	      (setf opt (send choose-bw :value))))
    (loop
     (when (or (null reply) (eql t valid))
           (return))
     (setq reply (send d :modal-dialog))
     (when reply
	   (setq valid (send self :check-validity
			     :calc-method
			     (select mets (send choose-calc :value))
			     :variable-bandwidth
			     (select bwopts (send choose-bw :value))))
	   (when (stringp valid)
		 (message-dialog valid))))
    (if reply
	(progn
;	  (print (format nil "got ok, mets: ~a, bwopts ~a, (~a ~a)~%"
;			 (send choose-calc :value)
;			 (send choose-bw :value) met (select bwopts opt)))
	  (send self :calc-method (select mets met) nil)
	  (send self :variable-bandwidth (select bwopts opt) t)
	  (send self :show-info-in-window)
	  t)
      nil)))


(defmeth kde-proto :new-x-range
  (&optional (from (first (send self :x-values)) fromset)
             (to (car (last (send self :x-values))) toset)
             (num (length (send self :x-values)) numset))
"When called from the menu, gives chance to set new x-range and number
of values for x. Number of values is optional, default is to keep old
numer of values.
 When called from the listener, you can give xfrom xto xnum as arguments."
(let* (
;;(str (concatenate 'string (num-to-string from) " "
;;(num-to-string to) " " (num-to-string num))
       (str (format nil "~,4g ~,4g ~a " from to num))
       (res nil))
  (if fromset
      (setq res (list from to num))
    (progn
      (setq res
	    (get-string-dialog "Enter from to [num]" :initial str))
      (when res
	    (setq res
		  (eval (read (make-string-input-stream
			       (strcat "(list " res ")"))))))))
  (when res
	(when (= (length res) 2)
	      (setf res (append res (list num))))
	(setq res (apply #'rseq res))
	(send self :x-values res)
	(send self :calc-and-set-estimates :force t)
	(send self :redraw-window)
	(send self :to-window :adjust-to-data)
	)))


(defun make-kde-slider (object &key (log nil) title text interval initial show)
"Creates a slider dialog with a reference to OBJECT and a couple of methods
 for comunicating with OBJECT."
  (let* ((sl (if log
              (log-interval-slider-dialog
	        interval
		:title title
		:text text
                :show show
		:action #'(lambda (s)
			    (send object :bandwidth s t)))
              (interval-slider-dialog
	        interval
		:title title
		:text text
                :show show
		:action #'(lambda (s)
			    (send object :bandwidth s t)))))
	 (sc (nth 2 (send sl :slot-value 'items))));l'item de scroll
    ; define a window slot in sl
    (send sl :add-slot 'parent-window object)
    (when initial (send sc :value initial))
    ; define a method for sl for sending s to the window
    (defmeth sl :send-to-parent (s)
      (send (slot-value 'parent-window) :bandwidth s t))
		;this t tells parent that message is arriving from this dialog
		;otherwise parent will update my :value if I exist
    (defmeth sl :close ()
      (send (slot-value 'parent-window)
	    :slot-value 'width-slider-dialog nil)
      (send (slot-value 'parent-window)
	    :show-info-in-window)
      (call-next-method))
    sl))

(defmeth kde-proto :pop-bandwidth-slider ()
  (send self :pop-bw-slider))

(defmeth kde-proto :pop-bw-slider ()
  (if (send self :variable-bandwidth)
      (let* ((tr (slot-value 'bw-transf))
	     (mypos (send self :to-window :location))
	     (mysize (send self :to-window :size))
	     (screen (screen-size))
	     (pos (list (first mypos)
			(round (/ (second screen) 2))))
	     size)
	(if (send tr :slot-value 'window)
	    (progn
	      (send tr :show-window :vert-label "Log(bandwith)")
	      (send tr :to-window :adjust-to-data))
	  (progn
	    (when
		 (ok-or-cancel-dialog "Reset the controller to constant?")
		 (let* ((kn (send tr :knots))
			(ave (mean (second kn))))
		   (send tr :knots (list (first kn)
					 (repeat ave (length (first kn)))))))
	    (send tr :show-window :vert-label "Log(bandwith)")
	    (send tr :to-window :adjust-to-data)))
	(setq size (list (first mysize)
			 (min (round (/ (second screen) 2))
			      (second (send tr :to-window :size)))))
	(apply #'send tr :to-window :location pos)
	(apply #'send tr :to-window :size size)
	)
    ;else, fixed bandwidth
    (let ((sl (slot-value 'width-slider-dialog))
	  (bwr (send self :bw-ends)))
      (when sl (send sl :close))
      (send self :show-info-in-window
	    (format nil "Bandwidth range: [~,4g, ~,4g]" 
		    (first bwr) (second bwr))
		    "" "")
      (setf (slot-value 'width-slider-dialog)
	    (make-kde-slider self
			     :show nil
			     :log t ; logaritmic scale
			     :text "bandwidth:"
			     :title "Kernel bandwidth"
			     :interval bwr))
      (send self :to-window :locate-dialog (slot-value 'width-slider-dialog))
      (send (slot-value 'width-slider-dialog) :show-window)
      (send (slot-value 'width-slider-dialog)
	    :value (slot-value 'bandwidth)))))


(defmeth kde-proto :pop-histogram-slider (&optional ignore)
"Creates and installs the histogram-slider
 or destroys it if exists"
(if (slot-value 'histogram-slider)
  (send (slot-value 'histogram-slider) :close)
  (let* ((nb (send self :histogram-num-bins))
         (nint (round (* '(0.3 2) nb)))
         (nsteps (min 30 (- (apply #'- nint))))
	 (sl (send slider-choice-dialog-proto :new
		 (format nil "Select between ~a and ~a"
			 (first nint) (second nint))
		 nint '("direct" "linear")
		 :target self
		 :points nsteps
		 :message :redraw-histogram
		 :title "KDE histogram"
		 :slider-text "Number of bins"
		 :choose-text "Binning method"
		 :initial-value nb
		 :initial-choice (position
				  (first (slot-value 'histogram-bin-method))
				  '(direct linear))))
	 (nb (if nb nb
	       (send self :histogram-num-bins 0))))
    (setf (slot-value 'histogram-slider) sl)
    (defmeth sl :close ()
      (send (slot-value 'target)
	    :slot-value 'histogram-slider nil)
      (send (slot-value 'target) :redraw-window)
      (call-next-method))
    (send self :redraw-window))))


(defmeth kde-proto :create-bw-transformation ()
"Creates a transformation object to control varaible bandwidth"
(require "transfor")
(let ((transf
       (make-transformation :initial-constant (log (send self :bandwidth))
			    :domain (list (first (send self :x-values))
					  (car (last (send self :x-values))))
			    :image (+ (log '(0.33333 3))
				      (log (send self :bandwidth)))
			    :notice-to-owner (list self :redraw-window :force t)
			    :title "Local bandwidth to use"
			    :vert-label "Log(bandwith)"
			    :show nil)))
  (setf (slot-value 'bw-transf) transf)))

(defmeth kde-proto :show-info-in-window (&optional text1 text2 text3)
"Arg: (&optional text1 text2 text3)
 prints on bottom of window some info. text can be up to three strings,
 one for each line. If they are nil, default info is shown."
(let* ((data-size (length (send self :data)))
       (quarts (format-digits (quartiles (send self :data))
			      *kde*significant*digits*))
       (vary (send self :variable-bandwidth)))
  (unless text1
	  (setq text1 (format nil "bandwidth: ~,4g,   kernel: ~d"
			      (case vary
				    (on-x-values "var. (on x)")
				    (on-data-values "var. (on data)")
				    (nil (format-digits (slot-value 'bandwidth)
							*kde*significant*digits*)))
			      (cdr (assoc (send self :kernel-type)
					  *kernel-names*))))
	  (when (send self :use-canonical)
		(setq text1 (strcat text1 " (can-resc.)")))
	  (setq text1 (strcat text1
			      "  Comp: "
			      (symbol-name (send self :calc-method)))))
  (unless text2
    (setq text2 (format nil "Data size: ~a  Data range: ~,4g to ~,4g"
                        data-size (first quarts) (first (last quarts)))))
  (unless text3
    (setq text3 (format nil "quartiles: ~,4g, ~,4g, ~,4g"
                               (second quarts) (third quarts)
                               (fourth quarts))))
  (setf (slot-value 'info-strings) (list text1 text2 text3))
  (send self :redraw-window :only-info t)))


(defmeth kde-proto :force-gaussian-kernel ()
  (if (and (not (slot-value 'use-canonical))
	   (eql 'h (send self :kernel-type)))
      t
    (let ((str
	   (format nil "This method implemented only~%for gaussian kernel, no canonical rescaling.~%Install it?")))
      (if (ok-or-cancel-dialog str)
	  (progn (send self :kernel-type 'h)
		 (send self :use-canonical nil)
		 t)
	nil))))

(defmeth kde-proto :automatic-bandwidth-selectors (&key (help nil))
"open a dialog offering a choice of six methods for bw selection"
  (let* (
	;;(dlg0 (message-dialog "Some problems, not all finished"))
	 (item-strings (list "Normal based rule-of-thumb"
			     "Least squares cross-valid."
			     "Park-Marron plug-in"
			     "Sheater-Jones solve-the-equation"
			     "Devroye's double kernel"
			     "Hall-Sheater-Jones-Marron selector"))
	 (help-text (list
		     (format nil "~a~%~a~%~a" "Normal based rule-of-thumb"
			     "This is the simpler bw selector."
			     "Assumes normality in data")
		     (format nil "~a~%~a~%~a" "Least squares cross-valid."
			     "LS Cross-validation was popular in the past."
			     "It's not currently recommended, is too noisy.")
		     (format nil "~a~%~a~%~a" "Park-Marron plug-in"
			     "This is a good bw selector."
			     "our implementation is quite slow")
		     (format nil "~a~%~a~%~a" "Sheater-Jones solve-the-equation"
			     "This is the best bw selector."
			     "according some experts in the field")
		     (format nil "~a~%~a~%~a" "Devroye's double kernel"
			     "This is a nice bw selector."
			     "The only one based in L1 distance")
		     (format nil "~a~%~a~%~a" "Hall-Sheater-Jones-Marron selector"
			     "A good bw selector,"
			     "fast to compute.")
		     ))
	 (list-of-actions '((:rule-of-thumb :showing t)
			    (:minimize-lscv :plot t :showing t)
			    (:iterate-plug-in)
			    (:sheather-jones-bandwidth :show t)
			    (:double-kernel)
			    (:hsjm-selector :showing t)
			    (:automatic-bandwidth-selectors :help t)
			    ))
	 (dlg (send choose-item-dialog-proto :new 
		    (if help
			"HELP on automatic method:"
		      "Choose automatic method:")
		    item-strings
		    :initial 0 :show nil))
	 (want-help nil)
	 resp
	 (help-button (send modal-button-proto :new "Help"
			    :action #'(lambda () (setf want-help t)
					(send (second (send dlg :slot-value 'items))
					      :value)))))
    (send dlg :slot-value 'items (list (first (send dlg :slot-value 'items))
				       (second (send dlg :slot-value 'items))
				       (append (third (send dlg :slot-value 'items))
					       (list help-button))))
    (loop
     (setf want-help nil)
     (setq resp (send dlg :modal-dialog))
     (if want-help
	 (message-dialog (select help-text resp))
       (if resp
	   (progn
	    (apply #'send self (select list-of-actions resp))
	    (return))
	 (return))))))

(defmeth kde-proto :install-theoretical-distribution (&key (distr-object
							    nil dset))
"Args: key distr-obj, a distribution object.
 If not given, a dialog is shown"
  (if dset
      (if distr-object
	  (progn
	    (send self :distr-dens
		  (send distr-object :slot-value 'density-function))
	    (send self :distr-rand
		  (send distr-object :slot-value 'random-generator)))
	(progn
	  (send self :distr-dens nil)
	  (send self :distr-rand nil)))
    ;if not dset, ask by dialog
    (let* ((strings '("Normal Distribution Adjusted to Data"
		      "Normal Mixture..."
		      "No distribution"))
	   (dlg (apply #'send choose-item-dialog-proto :new
		       "Choose a distribution:"
		       strings
		       :show nil
		       :initial 0 nil))
	   (loc (send self :to-window :location))
	   (pos (apply #'send dlg :location loc))
	   (reply (send dlg :modal-dialog)))
      (when reply
	    (require "distrobj")
	    (case reply
		  (0 (send self :install-theoretical-distribution
			   :distr-object (make-normal-distribution
					  (mean (send self :data))
					  (standard-deviation
					   (send self :data)))))
;		  (1 (let ((do (get-normal-mixture-dialog)))
;		       (when do
;			     (send self :install-theoretical-distribution
;				   :distr-object do))))
		  (1 (send self :adjust-a-normal-mixture))
		  (2 (send self :install-theoretical-distribution
			    :distr-object nil)))))))



;;starting code for this method was made by the sliders generator 
;;from B. Narasimhan.

(defmeth kde-proto :adjust-a-normal-mixture ()
  "gives a dialog for adjusting a normal mixture"
  (require "distrobj")
  (let* ((ms (list (mean (send self :data)) (standard-deviation (send self :data))))
	 (params (append '(1) ms '(0) ms '(0) ms)) 
	 (xlo (first (send self :x-values)))
	 (xhi (first (last (send self :x-values))))
	 (slo (* 0.2 (select params 2)))
	 (shi (* 2.5 (select params 2)))
	 (user t)
	 (tl (if (featurep :macintosh) 5 10))
	 sli-list)
    (labels
     ((iszero (x) (< (abs x) 1.d-3))
      (install-distr ()
		     (send self :install-theoretical-distribution
			   :distr-object (make-normal-mixture
					  (remove-if 
					   #'(lambda (trio)
					       (iszero (first trio)))
					   (split-list params 3)))))
      (update-it (n x)
		 (when user
		       (setf user nil)
		       ;; (format t "~a--" params)
		       (if (member n '(0 3 6))
			   (repart n x)
			 (setf (select params n) x)
			 )
		       ;; (format t ">>~a" params)
		       (install-distr)
		       (setf user t)))
      (repart (n x);proportions must add to one
	      (let* ((n1 (mod (+ n 3) 9))
		     (n2 (mod (+ n 6) 9))
		     (ix (- x (nth n params)))
		     rt inc)
		(unless (iszero ix)
			(setf (nth n params) x)
			(cond ((and (iszero (nth n1 params))
				    (iszero (nth n2 params)))
			       (setf ix (/ ix 2))
			       (setf (nth n2 params) (- (nth n2 params) ix))
			       (setf (nth n1 params) (- (nth n1 params) ix)))
			      ((iszero (nth n1 params))
			       (setf (nth n2 params) (- (nth n2 params) ix)))
			      ((iszero (nth n2 params))
			       (setf (nth n1 params) (- (nth n1 params) ix)))
			      (t (setf rt (/ (nth n1 params) (nth n2 params)))
				 (setf inc (/ ix (1+ rt)))
				 (setf (nth n2 params)
				       (- (nth n2 params) inc))
				 (setf (nth n1 params)
				       (- (nth n1 params) (* inc rt)))))
			(mapcar #'(lambda (nn so)
				    (send so :value (nth nn params)))
				'(0 3 6) sli-list)))));end labels
     (let* ((ttt (send text-item-proto :new
		       "Adjust Proportions, means and stdevs"))
	    (to0 (send text-item-proto :new
		       "Prop. 1"))
	    (vo0 (send text-item-proto :new
		       "" :text-length tl))
	    (so0 (send interval-scroll-item-proto :new
		       '(0 1)
		       :text-item vo0
		       :action
		       #'(lambda(x) (update-it 0 x))))
	    (tx3 (send text-item-proto :new
		       "Prop. 2"))
	    (vx3 (send text-item-proto :new
		       "" :text-length tl))
	    (sx3 (send interval-scroll-item-proto :new
		       '(0 1)
		       :text-item vx3
		       :action
		       #'(lambda(x) (update-it 3 x))))
	    (tx6 (send text-item-proto :new
		       "Prop. 3"))
	    (vx6 (send text-item-proto :new
		       "" :text-length tl))
	    (sx6 (send interval-scroll-item-proto :new
		       '(0 1)
		       :text-item vx6
		       :action
		       #'(lambda(x) (update-it 6 x))))
	    (tx1 (send text-item-proto :new
		       "Mean 1"))
	    (vx1 (send text-item-proto :new
		       "" :text-length tl))
	    (sx1 (send interval-scroll-item-proto :new
		       (list XLO XHI)
		       :text-item vx1
		       :action
		       #'(lambda(x) (update-it 1 x))))
	    (to4 (send text-item-proto :new
		       "Mean 2"))
	    (vo4 (send text-item-proto :new
		       "" :text-length tl))
	    (so4 (send interval-scroll-item-proto :new
		       (list XLO XHI)
		       :text-item vo4
		       :action
		       #'(lambda(x) (update-it 4 x))))
	    (tx7 (send text-item-proto :new
		       "Mean 3"))
	    (vx7 (send text-item-proto :new
		       "" :text-length tl))
	    (sx7 (send interval-scroll-item-proto :new
		       (list XLO XHI)
		       :text-item vx7
		       :action
		       #'(lambda(x) (update-it 7 x))))
	    (tx2 (send text-item-proto :new
		       "StDev 1"))
	    (vx2 (send text-item-proto :new
		       "" :text-length tl))
	    (sx2 (send interval-scroll-item-proto :new
		       (list SLO SHI)
		       :text-item vx2
		       :action
		       #'(lambda(x) (update-it 2 x))))
	    (tx5 (send text-item-proto :new
		       "StDev 2"))
	    (vx5 (send text-item-proto :new
		       "" :text-length tl))
	    (sx5 (send interval-scroll-item-proto :new
		       (list SLO SHI)
		       :text-item vx5
		       :action
		       #'(lambda(x) (update-it 5 x))))
	    (to8 (send text-item-proto :new
		       "StDev 3"))
	    (vo8 (send text-item-proto :new
		       "" :text-length tl))
	    (so8 (send interval-scroll-item-proto :new
		       (list SLO SHI)
		       :text-item vo8
		       :action
		       #'(lambda(x) (update-it 8 x))))
	    (dlg (send dialog-proto :new
		       (list
			ttt
			(list (list (list to0 vo0) so0)
			      (list (list tx3 vx3) sx3)
			      (list (list tx6 vx6) sx6))
			(list (list (list tx1 vx1) sx1)
			      (list (list to4 vo4) so4)
			      (list (list tx7 vx7) sx7))
			(list (list (list tx2 vx2) sx2)
			      (list (list tx5 vx5) sx5)
			      (list (list to8 vo8) so8)))
		       :title "Make a normal mixture")))
       (setf sli-list (list so0 sx1 sx2 sx3 so4 sx5 sx6 sx7 so8))
       (setf user nil)
       (mapcar #'(lambda (n so)
		   (send so :value (nth n params)))
	       (iseq 9) sli-list)
       (setq user t)
       (setq sli-list (list so0 sx3 sx6))
       (install-distr)
       (send self :to-window :add-subordinate dlg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; building a menu for kde-proto
(defmeth kde-proto :kde-menu-item (title &rest message)
  (let ((item (send menu-item-proto :new title)))
    (send item :add-slot 'dest-object self)
    (defmeth item :do-action ()
      (apply #'send (slot-value 'dest-object) message))
    item))


(defmeth kde-proto :make-menu-item (item-template)
  (let (item)
    (case
     item-template
     (to-gnuplot
      (setf item (send self :kde-menu-item "Graph to gnuplot file"
		       :to-gnuplot)))
     (draw-distr-lines
      (setf item (send self :kde-menu-item "Draw theoretical density"
		       :toggle-show-distr-lines))
      (defmeth item :update ()
	(send self :enabled
	      (send (slot-value 'dest-object) :distr-dens))
	(send self :mark
	      (send (slot-value 'dest-object) :slot-value 'show-distr-lines))
	(call-next-method)))
     (draw-a-kernel
      (setf item (send self :kde-menu-item "Draw kernel sample"
		       :toggle-show-a-kernel))
      (defmeth item :update ()
	(send self :mark
	      (send (slot-value 'dest-object) :slot-value 'show-a-kernel))
	(call-next-method)))
     (draw-data-points
      (setf item (send self :kde-menu-item "Draw data points"
	    :toggle-show-data-points))
      (defmeth item :update ()
	(send self :mark
	      (send (slot-value 'dest-object) :slot-value 'show-data-points))
	(call-next-method)))
     (draw-previous
      (setf item (send self :kde-menu-item "Show previous estimate"
		       :toggle-show-previous-estimate))
      (defmeth item :update ()
	(send self :mark
	      (send (slot-value 'dest-object)
		    :slot-value 'show-previous-estimate))
	(call-next-method)))
     (draw-histogram
      (setf item (send self :kde-menu-item "Draw histogram"
		       :pop-histogram-slider))
      (defmeth item :update ()
	(send self :mark
	      (if (send (slot-value 'dest-object)
			:slot-value 'histogram-slider) t))
	(call-next-method)))
     (minimize-dist
      (setf item (send self :kde-menu-item "Minimize distance"
		       :minimize-distance-to-theor-distr))
      (defmeth item :update ()
	(send self :enabled
	      (send (slot-value 'dest-object) :distr-dens))
	(call-next-method)))
     (do-bootstrap
      (setf item (send self :kde-menu-item "Bootstrap variability"
		       :calc-and-draw-bootstrap-quants))
      (defmeth item :update ()
	(send self :mark
	      (when (send (slot-value 'dest-object)
			  :slot-value 'bootstrap-quant-lines) t))
	(call-next-method)))
     (set-distribution
      (setf item (send self :kde-menu-item "Install theor. distr..."
		       :install-theoretical-distribution)))
     (silverman
      (setf item (send self :kde-menu-item "Silverman's test graph"
		       :silverman-test-graph))
      (defmeth item :update ()
	(send self :enabled
	      (not (send (slot-value 'dest-object) :variable-bandwidth)))
	(call-next-method)))
     (auto-selectors
      (setf item (send self :kde-menu-item "Automatic bandwith selectors..."
		       :automatic-bandwidth-selectors))
      (defmeth item :update ()
	(send self :enabled
	      (not (send (slot-value 'dest-object) :variable-bandwidth)))
	(call-next-method)))
     (pop-bw-slider
      (setf item (send self :kde-menu-item  "Bandwidth slider" :pop-bw-slider))
      (defmeth item :update ()
	(send item :title
	      (if (send (slot-value 'dest-object) :variable-bandwidth)
		  "Local bandwidth controller"
		"Bandwidth slider"))
	(call-next-method)))
     (x-range
      (setf item (send self :kde-menu-item "X range and count..." :new-x-range)))
     (choose-bw
      (setf item (send self :kde-menu-item "Bandwidth..." :choose-bandwidth))
      (defmeth item :update ()
	(send self :enabled
	      (not (send (slot-value 'dest-object) :variable-bandwidth)))
	(call-next-method)))
     (choose-calc
      (setf item (send self :kde-menu-item "Computing method..."
		       :choose-calc-method :showing t)))
     (info
      (Setf item (send self :kde-menu-item "Update info in window"
		       :show-info-in-window)))
     (redraw
      (setf item (send self :kde-menu-item "Redraw window"
		       :redraw-window :force t)))
     (resample
      (setf item (send self :kde-menu-item "Resample data" :resample-data))
      (defmeth item :update ()
	(send self :enabled
	      (send (slot-value 'dest-object) :distr-rand))
	(call-next-method)))
     (kernel-type
      (setf item (send self :kde-menu-item "Kernel..." :choose-kernel)))
     (save-self
      (setf item (send self :kde-menu-item "Save myself in a lisp file"
		       :save-self)))
     (read-data
      (setf item (send self :kde-menu-item "Read data file"
		       :read-data-file))))
  item))

(defmeth kde-proto :build-kde-menu ()
  (let ((win (slot-value 'window))
	(template '(rescale options mouse)))
    (setf template
	  (append template
		  '(dash
		    read-data
 		    #+unix save-image
		    save-self to-gnuplot
		    dash
		    info draw-a-kernel draw-data-points
		    draw-histogram draw-previous draw-distr-lines
		    dash
		    x-range kernel-type choose-bw
		    pop-bw-slider choose-calc
		    dash
		    set-distribution resample minimize-dist do-bootstrap
		    dash
		    silverman auto-selectors)))
    (send win :menu-template template)
    (send win :new-menu "KDE menu")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interface  objects used by kde-proto and its instances
;;; they are slight modifications of standard ones
;;; in order to keep the mother window in a slot

(defproto log-interval-slider-dialog-proto () () interval-slider-dialog-proto)

(defmeth log-interval-slider-dialog-proto :isnew
    (data &key (text "Value") (title "Log slider")
     (action #'(lambda (x) ())) (points 30) (nice nil))
  (call-next-method (log data) :text text :title title :nice nice
	:points points :action #'(lambda (x) (funcall action (exp x))))
  (defmeth (nth 2 (slot-value 'items)) :display-value () ;mantain compat release 2
    (if (send self :has-slot 'value-text-item)
	(if (slot-value 'value-text-item)
	    (send (slot-value 'value-text-item)
		  :text (format nil "~,4g" (exp (send self :value)))))
;;(num-to-string (format-digits (exp (send self :value)) *kde*significant*digits*))
      (if (slot-value 'xlisp::value-text-item)
	  (send (slot-value 'xlisp::value-text-item)
		:text (format nil "~,4g" (exp (send self :value)))))))
;; (format-digits (exp (send self :value)) *kde*significant*digits*)
    (send (nth 2 (slot-value 'items)) :display-value))

(defmeth log-interval-slider-dialog-proto :value (&optional (val nil set))
  (when set
    (call-next-method (log val)))
  (exp (call-next-method)))

(defun log-interval-slider-dialog (&rest args)
"Args: (data &key (text \"Value\") (title \"Log slider\") action (points 30) (nice t))
Opens modeless dialog with title TITLE, prompt TEXT, a text display and a
scrollbar. The scrollbar scrolls through the interval DATA, a list of the form
(LOW HIGH), sequence and displays the value. When a scroll event occurs
ACTION is called with the current value in the interval as argument. If NICE
is not NIL DATA and POINTS are revised to produce a nice set of values.
The scroll bar is tuned for exponential steps."
  (apply #'send log-interval-slider-dialog-proto :new args))

;;; Choose string/value dialog prototype
;;
;; this is a slight modification of choose-item-dialog-proto
;; for inclusion of a toggle item

(defproto choose-item-w-toggle-dialog-proto () () ok-or-cancel-dialog-proto)

(defmeth choose-item-w-toggle-dialog-proto :isnew (s strings
						     text-toggle
						     &rest args
						     &key (initial 0)
						     (toggle-value nil))
  (let* ((prompt-item (send text-item-proto :new s))
         (string-item (send choice-item-proto :new strings :value initial))
	 (toggle-item (send toggle-item-proto :new text-toggle
			    :value toggle-value)))
    (apply #'call-next-method (list prompt-item string-item toggle-item)
           :ok-action #'(lambda () (list (send string-item :value)
					 (send toggle-item :value)))
           args)))

(defun choose-item-w-toggle-dialog (&rest args)
"Args: (s strings toggle-text &key initial toggle-value)
Opens modal dialog with prompt S, a choice item for list of strings STRINGS
a toggle item with text TOGGLE-TEXT
and OK, Cancel buttons. Returns NIL on cancel
and on OK, a list of chosen item and T/nil for toggle value."
  (let ((d (apply #'send choose-item-w-toggle-dialog-proto :new args)))
    (send d :modal-dialog)))


(defproto slider-choice-dialog-proto '(target message)  ()
  dialog-proto)

(defmeth slider-choice-dialog-proto :isnew (prompt interval choices 
						   &key 
						   (slider-text "Value")
						   (choose-text "Choose one")
						   (title "Slider w. choice")
						   (points 30)
						   (nice t)
						   (initial-choice 0)
						   initial-value
						   target message)
"action is to send target message along with the slider value and the item choosen"
(when nice
      (let* ((range (get-nice-range (nth 0 interval) (nth 1 interval) points)))
	(setq interval (list (nth 0 range) (nth 1 range)))
	(setq points (nth 2 range))))
(let* ((prompt-item (send text-item-proto :new prompt))
       (choose-text (send text-item-proto :new choose-text))
       (choices-item (send choice-item-proto :new choices :value initial-choice))
       (value-item (send text-item-proto :new "   "
			 ;;:location '(100 5)
			 ))
       (sl-tx-item (send text-item-proto :new slider-text))
       (scroll-item (send interval-scroll-item-proto :new interval 
			  :text-item value-item
			  
			  :value (if initial-value initial-value
				   (first interval))
			  :points points)))
  (send scroll-item :slot-value 'action 
	#'(lambda (x)
	    (send target message (send scroll-item :value)
		  (send choices-item :value))))
  (setf (slot-value 'target) target)
  (setf (slot-value 'message) message)
  (send choices-item :slot-value 'action
	#'(lambda ()
	    (send target message (send scroll-item :value)
		  (send choices-item :value))))
  (call-next-method (list prompt-item
			  (list (list sl-tx-item scroll-item) value-item) 
			  (list choose-text choices-item))
		    :title title)
  (send scroll-item :value initial-value)
  (send scroll-item :display-value)))




;;;useful while developing
(defun ml ()
  (load "kde")
  (setq kde (make-kde-normal 200 :x-values (rseq -5 5 400) :calc-method 'update)))
(defun testkde (&optional (size 50))
  (setq testkde (make-kde :data (uniform-rand size))))

(provide "kde")


;;;end-of-file kde.lsp
