;;;file wfde.lsp
;;; wfde objects, are the window objects for kde objects
;;; extracted and specialized from kde on june 95
;;; obviously, some work is still to be done...


(defun make-histogram (data &rest key-value-pairs)
"Constructs a rich histogram for DATA,
 bin width is computed from normal-ref rule.
 Other args must be key/value pairs ot be passed to
 method :isnew for fde-proto:
 bin-width    defaults to normal-ref rule: bw = 3.5 x sigma x n^(-1/3)
 anchor-shift defaults to 0.5, the first bin edge will be xmin - 0.5 bw
 x-range      defaults to xmin..xmax expanded by oversmoothed bin-width
 show         wether to show it in a window, defaults to true
 what-to-show What estimates to show, must be a list containing 1+ of
              :histo-lines :hohistolines :fpoly-lines :mfpoly-lines :linpoly-lines
              defaults to '(:histo-lines)
 Example calls:
 (make-histogram my-data :x-range '(0 5) :show nil)
 (make-histogram (read-data-columns \"myfile.dat\" 1))
 etc."
(apply #'send fde-proto :new :data data key-value-pairs))

;;;
;;; Object fde-proto holds statistical data and methods
;;; to compute histograms, frequency polygons in its
;;; various versions.
;;;


(defproto fde-proto '(data data-summary scale-estimate
			   x-range bin-edges
			   title
			   bin-width bw-ends
			   bin-counts long-bin-counts stability-index
			   boxplot-lines data-plot-info
			   hohisto-lines histo-lines
			   fpoly-lines mfpoly-lines linpoly-lines
			   anchor-shift
			   info-strings
			   what-to-show
			   window
			   window-up-to-date)
  '(dependence-chain) *object*
)

;;;dependence-chain
(send fde-proto :slot-value 'dependence-chain
      '((init-all data anchor-shift what-to-show)
	(data bw-ends data-summary scale-estimate bin-width data-plot-info)
	(bw-ends x-range)
	(data-summary x-range boxplot-lines)
	(bin-width x-range)
	(x-range bin-edges)
	(anchor-shift bin-edges)
	(bin-edges bin-counts long-bin-counts)
	(bin-counts hohisto-lines histo-lines fpoly-lines mfpoly-lines)
	(long-bin-counts  stability-index linpoly-lines)
	(hohisto-lines  window-up-to-date)
	(histo-lines  window-up-to-date)
	(fpoly-lines  window-up-to-date)
	(mfpoly-lines window-up-to-date)
	(data-plot-info window-up-to-date)
	(linpoly-lines window-up-to-date)
	(what-to-show window-up-to-date)))

;;(load "kdehisto");must be (re)loaded after redefining kde-proto

;
; to add slots to kde-proto, add them to this declaration
; and deal with them in :isnew method

(defmeth fde-proto :isnew ( &key (title "Histogram analyzer")
                               data
			       bin-width
			       anchor-shift
			       x-range
                               (show t)
			       what-to-show
			       (debug nil))
"Initializes a fde-proto descendent.
 The normal way to create a fde-object is the `make-histogram' function."
  (when debug
	(print "fde-proto :isnew got arguments:")
	(print (list   :show show
		       :data data :title title
		       :bin-width    bin-width    
		       :anchor-shift anchor-shift 
		       :x-range	     x-range      
		       :what-to-show what-to-show 
		       :debug debug)))

  (send self :mark-changed 'init-all)
  (when title (send self :title title))
  (when data (send self :data data))
  (when bin-width (send self :bin-width bin-width))
  (when anchor-shift (send self :anchor-shift anchor-shift))
  (when x-range (send self :x-range x-range))
  (when what-to-show (send self :what-to-show what-to-show))
  (if (and show (system-has-windows))
      (send self :have-window :show show))
  self)

(defmeth fde-proto :print (&optional (stream t))
   (format stream "#<fde: ~S [~S]>"
	   (send self :title)
	   (call-next-method nil)))


;;;
;;; Accessor methods
;;; all follow the same pattern
;;;
;;;

;;(defmeth fde-proto :slot-name (&optional (value nil vset))
;;  (if vset
;;      (progn
;;	;;set all dependants to nil
;;	(send self :mark-changed 'slot-name)
;;	(slot-value 'slot-name value))
;;    ;;not vset, compute if needed
;;    (when (eq 'must-compute (slot-value 'slot-name))
;;	  (slot-value 'slot-name
;;		      (compute-slot-value-default))))
;;  (slot-value 'slot-name))

(defmeth fde-proto :mark-changed (symb)
  (let ((seq (find symb (slot-value 'dependence-chain)
		   :key #'first)))
    (when seq
	  (setf seq (cdr seq))
	  (mapcar #'(lambda (slo)
		      (slot-value slo 'must-compute)
		      (send self :mark-changed slo))
		  seq))))

(defmeth fde-proto :title (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'title)
	(slot-value 'title value))
    ;;not vset, compute if needed
    (when (or (null (slot-value 'title)) (eq 'must-compute (slot-value 'title)))
	  (slot-value 'title
		      "Default title never occurs")))
  (slot-value 'title))

(defmeth fde-proto :data (&optional (value nil vset) &key (draw nil))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'data)
	(slot-value 'data (coerce value 'vector))
	(when draw (send self :redraw-window)))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'data))
	  (error (progn (message-dialog "There are no data for the histogram")
			"no data"))))
  (slot-value 'data))

(defmeth fde-proto :bin-width (&optional (value nil vset) &key (draw nil))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'bin-width)
	(slot-value 'bin-width value)
	(when draw (send self :redraw-window)))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'bin-width))
	  (slot-value 'bin-width
		      (* 3.49 (send self :scale-estimate)
			 (^ (caadr (send self :data-summary)) -0.333333333)))))
    (slot-value 'bin-width))

(defmeth fde-proto :data-summary (&optional (value nil vset))
"data-summary is a list of: a list with the five numbers
 and a list with length, mean and stdev"
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'data-summary)
	(slot-value 'data-summary value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'data-summary))
	      (slot-value 'data-summary
			  (let ((dt (send self :data)))
			    (list (fivnum dt)
				  (list (length dt)
					(mean dt)
					(standard-deviation dt)))))))
    (slot-value 'data-summary))

(defmeth fde-proto :scale-estimate (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'scale-estimate)
	(slot-value 'scale-estimate value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'scale-estimate))
(require "superscale")
	  (slot-value 'scale-estimate
		      (if (find "superscale" *modules* :test #'string=)
			  (superscale (send self :data))
			(car (cddadr (send self :data-summary)))))))
  (slot-value 'scale-estimate))

(defmeth fde-proto :x-range (&optional (value nil vset) &key (draw nil))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'x-range)
	(slot-value 'x-range value)
	(when draw (send self :redraw-window)))
    ;;not vset, compute it if needed
    (when (eq 'must-compute (slot-value 'x-range))
	  (slot-value 
	   'x-range
	   (let* ((ds (send self :data-summary))
		  (mbw (second (send self :bw-ends))))
	     (list (- (caar ds) mbw)
		   (+ (nth 4 (car ds)) mbw))))))
  (slot-value 'x-range))

;;we take as maximum bin width the maximum of the two oversmoothing values 
;;in Scott's book (1992, Wiley), page 74, and increase it by 20%
(defmeth fde-proto :bw-ends (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'bw-ends)
	(slot-value 'bw-ends value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'bw-ends))
	  (slot-value
	   'bw-ends
	   (let* ((ds (send self :data-summary))
		  (iqr (- (fourth (first ds)) (second (first ds))))
		  (sig (send self :scale-estimate))
		  (n3 (^ (first (second ds)) -0.3333333))
		  (os (max (* 2.603 iqr n3) (* 3.729 sig n3))))
	     (* os '(0.05 1.2))))))
  (slot-value 'bw-ends))

(defmeth fde-proto :anchor-shift (&optional (value nil vset) &key (draw nil))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'anchor-shift)
	(loop
	 (when (< value 1) (return))
	 (setf value (1- value)))
	(loop
	 (when (>= value 0) (return))
	 (setf value (1+ value)))
	(slot-value 'anchor-shift value)
	(when draw (send self :redraw-window)))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'anchor-shift))
	      (slot-value 'anchor-shift
			  0.5)))
  (slot-value 'anchor-shift))

(defmeth fde-proto :bin-edges (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'bin-edges)
	(slot-value 'bin-edges value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'bin-edges))
	  (slot-value
	   'bin-edges
	   (let* ((bw (send self :bin-width))
		  (xr (send self :x-range))
		  (xmin (nth 0 (first (send self :data-summary))))
		  (xmax (nth 4 (first (send self :data-summary))))
		  (b1 (- xmin (* bw (send self :anchor-shift))))
		  (nlow (ceiling (/ (abs (- (min (first xr) xmin)
					    b1))
				    bw)))
		  (blow (- b1 (* bw nlow)))
		  (nhi (ceiling (/ (abs (- (max xmax (second xr))
					   b1))
				   bw)))
		  (bhi (+ b1 (* bw nhi))))
	     (rseq blow bhi (+ nlow nhi 1))))))
  (slot-value 'bin-edges))

(defmeth fde-proto :bin-counts (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'bin-counts)
	(slot-value 'bin-counts value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'bin-counts))
	      (slot-value
	       'bin-counts
	       (let* ((bw (send self :bin-width))
		      (xl (first (send self :bin-edges)))
		      (bc (make-array (1- (length (send self :bin-edges)))
				      :initial-element 0))
		      (lbc (make-array (* 100 (1- (length (send self :bin-edges))))
				       :initial-element 0))
		      (dt (send self :data))
		      (ll (length dt))
		      ind indl)
		 (dotimes (i ll)
			  (setq ind (floor (/ (- (aref dt i) xl) bw)))
			  (setf (aref bc ind) (1+ (aref bc ind)))
			  (setq indl (floor (/ (- (aref dt i) xl) (/ bw 100))))
			  (setf (aref lbc indl) (1+ (aref lbc indl))))
		 (slot-value 'long-bin-counts lbc)
		 bc))))
  (slot-value 'bin-counts))

(defmeth fde-proto :stability-index ()
    ;;compute if needed
    (when (eq 'must-compute (slot-value 'stability-index))
	  (slot-value
	   'stability-index
	   (let* ((nb (length (send self :bin-counts)))
		  (k (1- nb))
		  (lbc (slot-value 'long-bin-counts))
		  (ap 100) ; number of anchor positions
		  (ll (* ap nb)) ; length of long-bin-counts
		  (mj (make-array (* k ap) :initial-element 0))
		  (dj (make-array (* (1- k) ap) :initial-element 0))
		  (st (make-array ap :initial-element 0));histo
		  (smft (make-array ap :initial-element 0));mean-freq poly
		  tmp tt i)
	     (dotimes
	      (i ll)
	      (cond ((< i ap); 0 <= i < ap
		     (setf (aref mj 0)
			   (+ (aref mj 0)
			      (aref lbc i))))
		    ((< i (+ ap ap)); ap <= i < 2 ap
		     (setf j (- i ap))
		     (setf (aref mj (1+ j))
			   (+ (aref mj j)
			      (aref lbc i)
			      (- (aref lbc j))))
		     (setf (aref smft (rem j ap))
			   (+ (aref smft (rem j ap))
			      (* (aref mj j) (aref mj j)))))
		    ((< i (- ll ap)); 2 ap <= i < ll - ap
		     (setf j (- i ap))
		     (setf jj (- i ap ap))
		     (setf (aref mj (1+ j))
			   (+ (aref mj j)
			      (aref lbc i)
			      (- (aref lbc j))))
		     (setf (aref smft (rem j ap))
			   (+ (aref smft (rem j ap))
			      (* (aref mj j) (aref mj j))))
		     (setf (aref dj jj)
			   (- (aref mj j) (aref mj jj)))
		     (setf (aref st (rem jj ap))
			   (+ (aref st (rem jj ap))
			      (* (aref dj jj) (aref dj jj)))))
		    (t ; ll > i > ll-ap
		     (setf j (- i ap))
		     (setf jj (- i ap ap))
		     (setf tt (- ap (- ll i)))
		     (when (< i (1- ll))
			      (setf (aref mj (1+ j))
				    (+ (aref mj j)
				       (- (aref lbc j)))))
		     (setf (aref smft (rem j ap))
			   (+ (aref smft (rem j ap))
			      (* (aref mj j) (aref mj j))))
		     (setf (aref dj jj)
			   (- (aref mj j) (aref mj jj)))
		     (setf (aref st (rem jj ap))
			   (+ (aref st (rem jj ap))
			      (* (aref dj jj) (aref dj jj))))
		     (setf (aref st tt)
			   (+ (aref st tt)
			      (* (aref mj tt) (aref mj tt))
			      (* (aref mj j) (aref mj j)))))))
;;	     (break)
	     (list
	      (gini-index (coerce st 'list))
	      (gini-index (coerce smft 'list)))
)))
    (slot-value 'stability-index))

(defmeth fde-proto :boxplot-lines (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'boxplot-lines)
	(slot-value 'boxplot-lines value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'boxplot-lines))
	  (slot-value 'boxplot-lines
		      (compute-slot-value-default))))
  (slot-value 'boxplot-lines))

(defmeth fde-proto :hohisto-lines (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'hohisto-lines)
	(slot-value 'hohisto-lines value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'hohisto-lines))
	  (slot-value
	   'hohisto-lines
	   (let* ((be (send self :bin-edges))
		  (bc (/ (send self :bin-counts)
			 (send self :bin-width)
			 (caadr (send self :data-summary))))
		  xs ys)
	     (labels ((add-point (x y)
				 (setf xs (cons x xs))
				 (setf ys (cons y ys))))
		     (add-point (select be 0) 0)
		     (dotimes (i (length bc))
			      (add-point (select be i) (select bc i))
			      (add-point (select be (1+ i)) (select bc i)))
		     (add-point (select be (1- (length be))) 0))
	     (list (reverse xs)  (reverse ys))))))
  (slot-value 'hohisto-lines))

(defmeth fde-proto :histo-lines (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'histo-lines)
	(slot-value 'histo-lines value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'histo-lines))
	  (slot-value
	   'histo-lines
	   (let* ((be (send self :bin-edges))
		  (bc (/ (send self :bin-counts)
			 (send self :bin-width)
			 (caadr (send self :data-summary))))
		  xs ys)
	     (labels ((add-point (x y)
				 (setf xs (cons x xs))
				 (setf ys (cons y ys))))
		     (dotimes (i (1- (length bc)))
			      (add-point (select be i) 0)
			      (add-point (select be i) (select bc i))
			      (add-point (select be (1+ i)) (select bc i))
			      (add-point (select be (1+ i)) 0)))
	     (list (reverse xs)  (reverse ys)    )))))
  (slot-value 'histo-lines))

(defmeth fde-proto :fpoly-lines (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'fpoly-lines)
	(slot-value 'fpoly-lines value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'fpoly-lines))
	  (slot-value
	   'fpoly-lines
	   (list (- (cdr (send self :bin-edges))
		    (/ (send self :bin-width) 2))
		 (/ (send self :bin-counts)
		    (send self :bin-width)
		    (caadr (send self :data-summary)))))))
  (slot-value 'fpoly-lines))

(defmeth fde-proto :mfpoly-lines (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'mfpoly-lines)
	(slot-value 'mfpoly-lines value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'mfpoly-lines))
	  (slot-value
	   'mfpoly-lines
	   (let ((bc (coerce (/ (send self :bin-counts)
				(send self :bin-width)
				(caadr (send self :data-summary)))
			     'list)))
	     (list (send self :bin-edges)
		   (/ (+ (cons 0 bc) (append bc '(0))) 2))))))
  (slot-value 'mfpoly-lines))

(defmeth fde-proto :linpoly-lines (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'linpoly-lines)
	(slot-value 'linpoly-lines value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'linpoly-lines))
	  (slot-value 'linpoly-lines
		      'not-implemented)))
  (slot-value 'linpoly-lines))

(defmeth fde-proto :data-plot-info (&optional (value nil vset))
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'data-plot-info)
	(slot-value 'data-plot-info value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'data-plot-info))
	  (slot-value 'data-plot-info
		      (compute-slot-value-default))))
  (slot-value 'data-plot-info))

(defmeth fde-proto :what-to-show (&optional (value nil vset) &key (draw nil))
"sets from a user demand) the slot what-to-show
 or retrieves it to be shown in the window"
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'what-to-show)
	(slot-value 'what-to-show value)
	(when draw (send self :redraw-window)))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'what-to-show))
	  (slot-value 'what-to-show
		      '(:histo-lines))))
  (slot-value 'what-to-show))

(defmeth fde-proto :window-up-to-date (&optional (value nil vset))
"called with 'must-recompute to mark that window is not up-to-date
 called w/o args to enforce computations to be up-to-date"
  (if vset
      (progn
	;;set all dependants to nil
	(send self :mark-changed 'window-up-to-date)
	(slot-value 'window-up-to-date value))
    ;;not vset, compute if needed
    (when (eq 'must-compute (slot-value 'window-up-to-date))
	  (mapcar #'(lambda (m)
		      (send self m))
		  (send self :what-to-show))
	  (slot-value 'window-up-to-date t)))
  (slot-value 'window-up-to-date))

;;;
;;; some method to connect fde and its window
;;;
;;;
;;;

(defmeth fde-proto :have-window (&key (show t))
  (slot-value 'window
	      (send wfde-proto
		    :new :show show :title (slot-value 'title)
		    :fde-core self)))

(defmeth fde-proto :show-info-in-window (&rest args)
)

(defmeth fde-proto :to-window (&rest args)
  (if (slot-value 'window)
      (if  args
	  (apply #'send (slot-value 'window) args)
	(slot-value 'window))))

(defmeth fde-proto :redraw-window ()
  (send self :to-window :redraw))

;;;
;;; Some computation functions and methods needed
;;; and other utilities,
;;; configuration vars, etc
;;;

(defmeth fde-proto :stability-index-values (&optional 
					    (from (first (send self :bw-ends)))
					    (to (second (send self :bw-ends)))
					    (num 50))
"returns a list of 1) bin-width values, 2) index for the histograms
 and 3) index for the mean-freq polygons."
  (let* ((bw0 (send self :bin-width))
	 (vals (rseq from to num))
	 (idxs (mapcar #'(lambda (bw)
			   (send self :bin-width bw :draw nil)
			   (send self :stability-index))
		       vals))
	 (tr (transpose idxs)))
    (send self :bin-width bw0)
    (list vals (first tr) (second tr))))


(defmeth fde-proto :num-bins ()
  (let* ((bc (send self :bin-counts))
	 (nb (length bc))
	 (zf 0) (zl 0))
    (loop (unless (and (< zf nb) (= 0 (aref bc zf))) (return))
	  (setf zf (1+ zf)))
    (if (>= zf nb)
	0
      (loop (unless (= 0 (aref bc (- nb zl 1))) (return (- nb zf zl)))
	    (setf zl (1+ zl))))))

(defun gini-index (alist); this is the faster to compute
"Given ALIST of positive-or-zero number, gives a Gini-like index measuring
 the diversity of the numbers. Ranges in [0 1]."
  (let* ((nli (sort-data alist))
	 (nn (length nli))
	 (sim (sum (* (iseq nn 1) nli)))
	 (sm (sum nli)))
    (/ (1- (* 2 (/ sim sm))) nn)))

(defun featurep (sym)
  (member sym *features*))
(defun system-has-color ()
  (or (featurep 'color)
      (featurep :color)))

(unless (fboundp 'system-has-windows)
	(defun system-has-windows ()
	  (or (member 'windows *features*)
	      (member :windows *features*))))


(defvar *kde-wsize*
  (if (featurep :macintosh) '(400 280)  '(650 450))
"the default size of windows to be created")

(defvar *line-colors* '(yellow green red cyan magenta blue white black))

;;;
;;; wfde is the object to display
;;; histogram and polygon frequency estimates
;;; is similar to wkde-proto
;;;


(defproto wfde-proto '(fde-core) () graph-proto)

(defmeth wfde-proto :to-core (&rest args)
  (if (null args)
      (slot-value 'fde-core)
      (apply #'send (slot-value 'fde-core) args)))

(defmeth wfde-proto :isnew (&key (title nil) fde-core 
				(show nil) (debug nil))
  (call-next-method 2 :show nil :title title 
	 :menu-template '(mouse dash rescale options
				dash info stability-index show-index-plot
				dash 
				bin-control what-to-show
				animate)
	 :menu-title "Fde")
  (setf (slot-value 'fde-core) fde-core)

  (send self :add-mouse-mode 'show-coordinates 
	:title "Show Coordinates" :cursor 'finger 
	:click :do-show-coordinates)
  (send self :add-mouse-mode 'zoom-in 
	:title "Zoom in" :cursor 'cross 
	:click :do-zoom-in)
  (send self :delete-mouse-mode 'selecting)
  (send self :delete-mouse-mode 'brushing)
  (send self :delete-mouse-mode 'point-moving)
  (send self :mouse-mode 'show-coordinates)

;;  (send self :margin 0 0 0 (* 3 (send self :text-ascent)))
  (send self :x-axis t t 5)
  (send self :y-axis t t 4)
  (send self :adjust-to-data :draw nil)
  (send self :update nil)
  
  (apply #'send self :size *kde-wsize*)
  (unless (featurep :macintosh)
	  (send self :back-color 'black)
	  (send self :draw-color 'white))
  (when show (send self :show-window)
	(send self :redraw-content)
	(send self :to-core :show-info-in-window)
	(send self :activate t);;in release 3 this is needed???
	)
  self
)

(defmeth wfde-proto :redraw-content ()
  (let* ((core (send self :to-core))
	 (wts (send core :what-to-show))
	 (clip (send self :clip-rect)))
    (send core :window-up-to-date)
    (send self :clear-lines :draw nil)
    (loop
     (unless wts (return))
     (case (first wts)
	   (:histo-lines
	    (send self :add-lines (send core :histo-lines) 
		  :draw nil :color (select *line-colors* 0)))
	   (:hohisto-lines
	    (send self :add-lines (send core :hohisto-lines) 
		  :draw nil :color (select *line-colors* 0)))
	   (:fpoly-lines
	    (send self :add-lines (send core :fpoly-lines) 
		  :draw nil :color (select *line-colors* 1)))
	   (:mfpoly-lines
	    (send self :add-lines (send core :mfpoly-lines) 
		  :draw nil :color (select *line-colors* 2)))
	   (:linpoly-lines
	    (print "linearly binned poly not implemented yet")))
     (setf wts (cdr wts)))
    (apply #'send self :clip-rect (send self :content-rect))
    (call-next-method)
    (apply #'send self :clip-rect clip)
;    (apply #'send self :paint-rect (send self :content-rect) )
    ;;  (send self :draw-info-strings)
))

(defmeth wfde-proto :adjust-to-data (&key (draw t))
  (let ((core (send self :to-core))
	rng)
    (call-next-method :draw nil)
    (setf rng (send self :range 1))
    (if (< (abs (first rng)) 0.0001)
	(setf (first rng) 0))
    (send self :range 1 (first rng) (second rng) :draw nil)
    (setf rng (send core :x-range))
    (send self :range 0 (first rng) (second rng) :draw nil)
    (send self :redraw)))


(defmeth wfde-proto :close ()
"Before closing its windows, closes other child windows"
(call-next-method)
(send self :to-core :slot-value 'window nil)
;(send self :to-core :close-windows)
)

(defmeth wfde-proto :draw-info-strings ()
  (let* ((bot (nth 3 (send self :margin)))
	 (l 2)                                ;l top w h
	 (top (- (send self :canvas-height) bot 10))
	 (w (- (send self :canvas-width) 4))
	 (h (+ bot 8))
	 (incpos (+ (list 0 (send self :text-ascent))))
	 (pos (+ '(5 2) (list l top)))
	 (is (send self :to-core :slot-value 'info-strings))
	 (text1 (first is))
	 (text2 (second is))
	 (text3 (third is)))
    (when is
	  (send self :erase-rect l top w h)
	  (send self :frame-rect l top w h)
	  (setf pos (+ pos incpos))
	  (when text1
		(send self :draw-text text1 (nth 0 pos) (nth 1 pos) 0 0))
	  (setf pos (+ pos incpos))
	  (when text2  
		(send self :draw-text text2 (nth 0 pos) (nth 1 pos) 0 0))
	  (setf pos (+ pos incpos))
	  (when text3
		(send self :draw-text text3 (nth 0 pos) (nth 1 pos) 0 0)))))


(defmeth wfde-proto :do-show-coordinates (x y m1 m2)
;modified from graphics.lsp, xlispstat for mac
;this version allows moving the mouse while seeing the coordinates
;of the click point
  (let* ((xy (cond (m1 (list x y))
                   (m2 (send self :canvas-to-scaled x y))
                   (t (send self :canvas-to-real x y))
               ))
         (s (format nil "(~,3g, ~,3g)" (first xy) (second xy)))
         (str-size (send self :text-width s))
         (left (> (+ x str-size) (send self :canvas-width)))
         (horz (if left 2 0))
         (vert 0))
    (send self :draw-string-while-button s x y horz vert)))

(defmeth wfde-proto :draw-string-while-button (s x y h v)
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

(defmeth wfde-proto :do-zoom-in (x y m1 m2)
(if (or m1 m2)
  (send self :adjust-to-data)
  (let* ((oldx x)
         (oldy y)
         (old-draw-mode (send self :draw-mode))
         (rect (progn (send self 
                            :while-button-down
                            #'(lambda (nx ny)
                                (send self :draw-mode 'xor)
                                (send self :frame-rect x y
                                      (- oldx x) (- oldy y))
                                (send self :frame-rect x y
                                      (- nx x) (- ny y))
                                (setf oldx nx)
                                (setf oldy ny)))
                      (list x y (- oldx x) (- oldy y))))
         (l (nth 0 rect))
         (to (nth 1 rect))
         (w (nth 2 rect))
         (h (nth 3 rect))
         (lb (send self :canvas-to-scaled l (+ to h)))
         (tr (send self :canvas-to-scaled (+ l w) to))
         (xmin (nth 0 lb))
         (xmax (nth 0 tr))
         (ymin (nth 1 lb))
         (ymax (nth 1 tr)))
    (send self :frame-rect l to w h)
    (send self :draw-mode old-draw-mode)
    (send self :set-ranges (min xmin xmax) (max xmin xmax)
          (min ymin ymax) (max ymin ymax)))))


(defmeth wfde-proto :set-ranges (xmin xmax ymin ymax &key (recalc nil))
  (send self :scaled-range 0 xmin xmax :draw nil)
  (send self :scaled-range 1 ymin ymax :draw nil)
  (when recalc (error "set-ranges does not recalculate" nil)) ;this is to be done...
#-macintosh
  (send self :redraw)
  (send self :update nil))

(defmeth wfde-proto :locate-dialog (dialog)
#-msdos"will return a good position for a dialog to appear"
#+msdos"this method does nothing in MS-Windows"
;it tries to locate first in rigth
;then in down
;and if all fails, over left-up corner of myself
#+msdos()
#-msdos(let* ((mypos (send self :location))
       (dlgsize (if (send dialog :size)
		    (send dialog :size)
		  '(150 150)))
       (screen (screen-size))
       (mysize (send self :size))
       
       (all (- screen (+ mypos mysize dlgsize))))
  (cond ((> (first all) 0)
	 (apply #'send dialog :location (list (+ (first mypos)
						 (first mysize))
					      (second mypos))))
	((> (second all) 0)
	 (apply #'send dialog :location (list (first mypos)
					      (+ (second mypos)
						 (second mysize)))))
	(t (apply #'send dialog :location (+ '(10 20) mypos))))))


(defmeth wfde-proto :make-menu-item (item-template)
  (let ((it (call-next-method item-template)))
    (if it it
      (case item-template
	    (animate (send graph-item-proto :new "Animate anchor moving" self 
               :toggle-animation :idle-on :toggle t))
	    (show-index-plot (send graph-item-proto :new "Stability index plot..." 
				   self :show-index-plot))
	    (info (send graph-item-proto :new "Show info..." 
				   self :info-dialog))
	    (stability-index (send graph-item-proto :new "Stability index..." self 
               :show-stability-index))
	    (bin-control (send graph-item-proto :new "Bin control..." self 
               :pop-bins-dialog))
	    (what-to-show (send graph-item-proto :new "What to show..." self 
               :what-to-show-dialog)))))) 

;;;
;;; interface methods
;;;


(defmeth wfde-proto :do-idle ()
  (send self :to-core :anchor-shift
	(+ 0.04 (send self :to-core :anchor-shift)) :draw t))

(defmeth wfde-proto :toggle-animation (&optional (bool nil set))
  (if set (send self :idle-on bool)
    (send self :idle-on (not (send self :idle-on)))))

(defmeth wfde-proto :show-stability-index ()
  (let ((core (Send self :to-core))
	dlg str1 str2 gind)
    (unless (listp (send core :slot-value 'stability-index))
	    (setf str1 (format nil
			       "Please, wait for the index~%to be computed"))
	    (setf dlg
		  (send dialog-proto :new
			(list (send text-item-proto :new str1))
			:show t
			:title "Computing the index"))
	    (send dlg :show-window))
    (setf gind (send self :to-core :stability-index))
    (when dlg (send dlg :dispose))
    (message-dialog
     (print
      (format nil
	      "For bin width: ~,3g~%stability index for the histogram~%is G = ~,3g~%For the mean-frequencies polygon~%is G = ~,3g~%"
	      (send self :to-core :bin-width)
	      (first gind) (second gind))))))

(defmeth wfde-proto :show-index-plot ()
  (let* ((core (Send self :to-core))
	 (bwe (send core :bw-ends))
	 (str (format nil "~,4g ~,4g ~a" 
		      (first bwe) (second bwe) 50))
	 (resp (get-string-dialog
		(format nil "This will compute and display stability index values~%for a series of bin width values in a range.~%It is a some minute(s) computation.~%Enter bin width range:~%    from  to  num-values")
		:initial str))
	 pl lins win)
    (when resp
	  (setq win
		(send dialog-proto :new
		      (list (send text-item-proto
				  :new "This will take some time..."))
		      :title "This will take some time..."))
	  (send win :show-window)
	  (labels ((evalstring (aString)
		    "Returns a list of the expressions obtained 
on evaluation of the characters in aString"  
		    (let ((st (make-string-input-stream aString))   
			  (result nil)   
			  (expr nil))  
		      (loop (setq expr (read st nil 'eof))  
			    (when (eql expr 'eof)    
				  (return (reverse result)))  
			    (setf result (cons expr result))))))
		  (setq lins (apply #'send core :stability-index-values
				    (evalstring resp)))
(require "my-plot-lines" "plotline")
                  (send win :dispose)
		  (if (find "my-plot-lines" *modules* :test #'string=)
			  (setq pl
				(my-plot-lines (first lins)
					       (cdr lins)))
		    (progn
		      (setq pl (plot-lines (first lins) (second lins)))
		      (send pl :add-lines (list (first lins) (third lins)))))))
     lins))

(defmeth wfde-proto :info-dialog ()
  (let* ((cor (send self :to-core))
	 (ds (send cor :data-summary))
	 (str (format
	       nil
	       "~a~,3g~%~a~a~%~a~,3g ~,3g~%~a~,3g ~,3g ~,3g~%~a~a~%~a~,3g~%~a~,3g"
	       "Current bin-width: " (send cor :bin-width)
	       "Number of bins: " (send cor :num-bins)
	       "Data range: " (first (first ds)) (fifth (first ds))
	       "Quartiles: " (second (first ds)) (third (first ds)) (fourth
								     (first ds))
	       "N         = " (first (second ds))
	       "Mean      = " (second (second ds))
	       "Std. dev. = " (third (second ds)))))
    (message-dialog str :title "Current data set")
    (list (send cor :bin-width) (send cor :num-bins)
	  (first (first ds)) (fifth (first ds))
	  (second (first ds)) (third (first ds))(fourth (first ds))
	  (first (second ds))(second (second ds))(third (second ds)))))

(defmeth wfde-proto :pop-bins-dialog ()
  (let* ((core (send self :to-core))
	 (bwpr (send text-item-proto :new
		     (format nil "Bin-width:~% ")))
	 (bwdp (send text-item-proto :new "" :text-length 6))
	 (bwsl (send interval-scroll-item-proto :new
		       (send core :bw-ends)
		       :text-item bwdp
		       :action #'(lambda(x) (send core :bin-width x :draw t))))
	 (aspr (send text-item-proto :new
		     (format nil "Anchor shift~%(% of the bin)")))
	 (asdp (send text-item-proto :new "" :text-length 6))
	 (assl (send interval-scroll-item-proto :new
		       '(0 100)
		       :text-item asdp
		       :action #'(lambda(x) (send core :anchor-shift (- 1 (/ x 100))
						  :draw t))
		       :points 21))
	 (items (list (list (list bwsl (list bwpr bwdp) )
			    (list assl (list aspr asdp) ))))
	 (dlg (send dialog-proto :new items :title "Bin control" :show nil)))
    (send dlg :location
	  (first (send self :location))
	  (+ 10 (second (send self :location))
	     (second (send self :size))))
    (send dlg :show-window)
    (send bwsl :value (send core :bin-width))
    (send assl :value (send core :anchor-shift))
    (send self :add-subordinate dlg)))


(defmeth wfde-proto :what-to-show-dialog ()
  (let* ((core (send self :to-core))
	 (sh '(:histo-lines :hohisto-lines :fpoly-lines :mfpoly-lines :linpoly-lines))
	 (str '("Histogram" "Hollow histogram" "Frequency polygon"
		"Mean-frequency polygon" "Linearly binned freq. poly"))
	 (in (which (mapcar #'(lambda (x)
				(find x (send core :what-to-show)))
			    sh)))
	 (resp (choose-subset-dialog "Mark what must be shown:"
				     str :initial in)))
    (when (and resp (caar resp))
	  (send core :what-to-show (select sh (car resp)) :draw t))))

(defmeth wfde-proto :do-key (char shift option)
"some keyboard interface"
(case char
      (#\+
       (send self :to-core
	     :bin-width (* 1.1 (send self :to-core :bin-width)) :draw t)
       (send self :to-core :show-info-in-window nil "" ""))
      (#\-
       (send self :to-core
	     :bin-width (/ (send self :to-core :bin-width) 1.1) :draw t)
       (send self :to-core :show-info-in-window nil "" ""))
      (#\a 
       (send self :adjust-to-data))
      ((#\r) (send self :to-core :anchor-shift
		   (- (send self :to-core :anchor-shift) 0.1) :draw t))
      ((#\l) (send self :to-core :anchor-shift
		   (+ (send self :to-core :anchor-shift) 0.1) :draw t))
      ))

;;;
;;; Some data sets analyzed in Simonoff-Udina(1995)
;;;

;; Old faithful geyser eruptions duration in August 1978 and August 1979.
(setf oldf-dur '(4.4 3.9 4 4 3.5 4.1 2.3 4.7 1.7 4.9 1.7 4.6 3.4 4.3 1.7 3.9 3.7 3.1 4 1.8 4.1 1.8 3.2 1.9 4.6 2 4.5 3.9 4.3 2.3 3.8 1.9 4.6 1.8 4.7 1.8 4.6 1.9 3.5 4 3.7 3.7 4.3 3.6 3.8 3.8 3.8 2.5 4.5 4.1 3.7 3.8 3.4 4 2.3 4.4 4.1 4.3 3.3 2 4.3 2.9 4.6 1.9 3.6 3.7 3.7 1.8 4.6 3.5 4 3.7 1.7 4.6 1.7 4 1.8 4.4 1.9 4.6 2.9 3.5 2 4.3 1.8 4.1 1.8 4.7 4.2 3.9 4.3 1.8 4.5 2 4.2 4.4 4.1 4.1 4 4.1 2.7 4.6 1.9 4.5 2 4.8 4.1 4.1 4.2 4.5 1.9 4.7 2 4.7 2.5 4.3 4.4 4.4 4.3 4.6 2.1 4.8 4.1 4 4 4.4 4.1 4.3 4 3.9 3.2 4.5 2.2 4.7 4.6 2.2 4.8 4.3 3.8 4 4.1 1.8 4.4 4 2.2 5.1 1.9 5 4.4 4.5 3.8 4.3 4.4 2.2 4.8 1.9 4.7 1.8 4.8 2 4.4 2.5 4.3 4.4 1.9 4.7 4.3 2.2 4.7 2.3 4.6 3.3 4.2 2.9 4.6 3.3 4.2 2.6 4.6 3.7 1.8 4.7 4.5 4.5 4.8 2 4.8 1.9 4.7 2 5.1 4.3 4.8 3 2.1 4.6 4 2.2 5.1 2.9 4.3 2.1 4.7 4.5 1.7 4.2 4.3 1.7 4.4 4.2 2.2 4.7 4 1.8 4.7 1.8 4.5 2.1 4.2 2.1 5.2 2))

(setq pcb-concentrations '(77.55 29.23 403.1 736 192.15 220.6 8.62 174.31 529.28 130.67 39.74 0.0 8.43 0.0 120.04 0.0 11.93 0.0 0.0 30.14 0.0 0.0 0.0 531.67 9.3 5.74 46.47 176.9 13.69 4.89 6.6 6.73 4.28 20.5 20.5 5.8 5.08 ))

(setq buffalo-snowfall '(126.4 82.4 78.1 51.1 90.9 76.2 104.5 87.4 110.5 25 69.3 53.5 39.8 63.6 46.7 72.9 79.6 83.6 80.7 60.3 79 74.4 49.6 54.7 71.8 49.1 103.9 51.6 82.4 83.6 77.8 79.3 89.6 85.5 58 120.7 110.5 65.4 39.9 40.1 88.7 71.4 83 55.9 89.9 84.8 105.2 113.7 124.7 114.5 115.6 102.4 101.4 89.8 71.5 70.9 98.3 55.5 66.1 78.4 120.5 97 110))

(setq visa-data '( 1.612800 6.021000E-01 1.707600 2.250400 1.770900 1.079200 2.420000 1.792400 2.721800 1.740400 1.699000 1.041400 2.086400 6.990000E-01 2.510500 1.716000 2.387400 1.602100 1.397900 2.651300 1.591100 1.919100 1.230400 2.025300 1.041400 1.204100 9.542000E-01 1 2.248000 2.858500 2.620100 1.977700 1 3.406900 3.259400 1.740400 2.103800 7.782000E-01 1.230400 ))


(setq oldf-pauses '(78 74 68 76 80 84 50 93 55 76 58 74 75 80 56 80 69 57 90 42 91 51 79 53 82 51 76 82 84 53 86 51 85 45 88 51 80 49 82 75 73 67 68 86 72 75 75 66 84 70 79 60 86 71 67 81 76 83 76 55 73 56 83 57 71 72 77 55 75 73 70 83 50 95 51 82 54 83 51 80 78 81 53 89 44 78 61 73 75 73 76 55 86 48 77 73 70 88 75 83 61 78 61 81 51 80 79 82 80 76 56 82 47 76 61 75 72 74 69 78 52 91 66 71 75 81 77 74 70 83 53 82 62 73 84 58 82 77 75 77 77 53 75 78 51 81 52 76 73 84 72 89 75 57 81 49 87 43 94 45 81 59 82 80 54 75 73 57 80 51 77 66 77 60 86 62 75 67 69 84 58 90 82 71 80 51 80 62 84 51 81 83 84 72 54 75 74 51 91 60 80 54 80 70 60 86 78 51 83 76 51 90 71 49 88 52 79 61 81 48 84 63 ))

(setq nba-ages '(28 30 26 30 28 31 30 27 29 24 27 29 24 30 28 32 25 29 34 23 32 28 28 23 32 27 34 26 30 30 23 31 28 27 25 32 29 34 28 23 26 30 32 27 27 25 24 27 25 27 31 30 25 26 33 24 26 31 24 27 28 22 30 31 23 25 31 33 28 37 28 24 34 24 28 33 23 26 28 26 25 25 26 25 27 35 31 25 30 24 23 23 27 27 25 24 24 23 23 26 24 23 32 24 27))


(defun fde-demo ()
  (setf fd (send fde-proto :new :data oldf-dur :x-range '(0 6))))


(provide "wfde")

(defun stability-index (data bin-width &optional max-bin-width number-of-values)
  (let ((fde (send fde-proto :new :data data :bin-width bin-width :show nil))
	bws res)
    (if max-bin-width
	(progn
	  (setq bws (rseq bin-width max-bin-width number-of-values))
	  (list bws 
		(mapcar #'(lambda (b)
			    (send fde :bin-width b)
			    (send fde :stability-index))
			bws)))
      (send fde :stability-index))))
