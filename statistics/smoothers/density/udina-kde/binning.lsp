;;;file binning.lsp
;;; functions for binning data
#|
 methods to implement:
   direct
   linear
   free-kernel will accept any binning-kernel (See Hall-Wand 93), not implemented

 there are three options regarding outsiders (data outside the grid range)
  1) ignore it: 'delete
  2) count them as in the ending bin, lower or upper: 'shift
  3) be sure there are no outsider, error if one is found: 'none
(see Fan-Marron 94)
|#


(defvar *kde*computing*inspect* t);this is usually def'd in kde.lsp



(defun bin-data (data &key (method 'direct)
                      (kern-function nil)
                      (xrange nil)
                      (binwidth nil)
                      (numbins nil)
                      (outsiders 'delete))
"Given data, returns a list of bin counts.
Keys :method 'direct 'linear 'custom, if custom, kern-function must be given
     :xrange, a list of xmin xmax, the first and last bin centers
     :binwidth or :numbins, one of the two must be given
     :outsiders 'delete or 'shift or 'none, how to process outsiders"
(when *kde*computing*inspect*
      (format t "~%xrange: (~,4g, ~,4g), binwidth: ~,4g, numbins: ~a~%" 
	      (first xrange) (second xrange) binwidth numbins))
(when kern-function
      (error "bin-data: kern-function not implemented"))
(unless (or binwidth numbins)
	(error "binwidth or numbins must be given"))
(if binwidth
    (setf numbins (1+ (round (/ (- (apply #'- xrange)) binwidth))))
  (setf binwidth (/ (- (apply #'- xrange)) (1- numbins))))
(when  *kde*computing*inspect* 
       (princ (format nil
		      "Binning data, ~a bins, delta: ~,4g, method ~a(~a)~%"
		      numbins binwidth method outsiders)))
(cond
 ((and (eql outsiders 'none) (eql method 'direct))
  (bin-data-direct data (first xrange) (second xrange) binwidth numbins))
 ((and (eql outsiders 'delete) (eql method 'direct))
  (bin-data-direct-delete data (first xrange) 
			  (second xrange) binwidth numbins))
 ((and (eql outsiders 'shift) (eql method 'direct))
  (bin-data-direct-shift data (first xrange) 
			 (second xrange) binwidth numbins))
 ((and (eql outsiders 'none) (eql method 'linear))
  (bin-data-linear data (first xrange) 
		   (second xrange) binwidth numbins))
 ((and (eql outsiders 'delete) (eql method 'linear))
  (bin-data-linear-delete data (first xrange) 
			  (second xrange) binwidth numbins))
 ((and (eql outsiders 'shift) (eql method 'linear))
  (bin-data-linear-shift data (first xrange) 
			 (second xrange) binwidth numbins))
 (t (error "not implemented method of binning"))))

(defun bin-data-direct (data xmin xmax binwidth numbins)
"xmin xmax, the first and last bin centers"
  (let ((counts (make-array numbins :initial-element 0))
        bin)
    (dolist (item data)
            (setf bin (floor (+ 0.5 (/ (- item xmin) binwidth))))
            (setf (aref counts bin) (1+ (aref counts bin))))
    counts))

(defun bin-data-direct-delete (data xmin xmax binwidth numbins)
"xmin xmax, the first and last bin centers"
  (let ((counts (make-array numbins :initial-element 0))
	(halfbin (/ binwidth 2))
        bin)
    (dolist (item data)
            (if (and (< item (+ xmax halfbin))
		       (>= item (- xmin halfbin)))
		(progn
		  (setf bin (floor (+ 0.5 (/ (- item xmin) binwidth))))
		  (setf (aref counts bin) (1+ (aref counts bin))))
	      (when *kde*computing*inspect*
		    (princ (format nil "bin-delete rejects data: ~a~%" item)))))
    counts))

(defun bin-data-direct-shift (data xmin xmax binwidth numbins)
"xmin xmax, the first and last bin centers"
  (let ((counts (make-array numbins :initial-element 0))
        (nb-1 (1- numbins))
        bin)
    (dolist (item data)
            (cond
              ((> item xmax) (setf (aref counts nb-1)
				   (1+ (aref counts nb-1)))) 
              ((< item xmin) (setf (aref counts 0)
				   (1+ (aref counts 0))))
              (t
               (progn
		  (setf bin (floor (+ 0.5 (/ (- item xmin) binwidth))))
		  (setf (aref counts bin) (1+ (aref counts bin)))))))
    counts))

(defun bin-data-linear (data xmin xmax binwidth numbins)
"xmin xmax, the first and last bin centers
 will flag error if an outsider is found"
  (let ((counts (make-array numbins :initial-element 0))
        quoc flquoc bin part tmp)
    (dolist
     (item data)
     (setf quoc (/ (- item xmin) binwidth))
     (setf flquoc (floor quoc))
     (setf part (* flquoc binwidth))
     (setf (aref counts (1+ flquoc)) 
	   (+ (aref counts (1+ flquoc))
	      (setq tmp (- quoc flquoc))))
     (setf (aref counts flquoc)
	   (+ (aref counts flquoc)
	      (- 1 tmp))))
    counts))

(defun bin-data-linear-delete (data xmin xmax binwidth numbins)
"xmin xmax, the first and last bin centers"
  (let ((counts (make-array numbins :initial-element 0))
        quoc flquoc bin part tmp)
    (dolist
     (item data)
     (if (and (< item xmax) ;
	      (> item xmin));otherwise weights go outside limits
	 (progn
	   (setf quoc (/ (- item xmin) binwidth))
	   (setf flquoc (floor quoc))
	   (setf part (* flquoc binwidth))
	   (unless (= quoc flquoc)
		   (setf (aref counts (1+ flquoc)) 
			 (+ (aref counts (1+ flquoc))
			    (setq tmp (- quoc flquoc)))))
	   (setf (aref counts flquoc)
		 (+ (aref counts flquoc)
		    (- 1 tmp))))
       (when *kde*computing*inspect*
	     (princ (format nil "bin-delete rejects data: ~a~%" item)))))
    counts))

(defun bin-data-linear-shift (data xmin xmax binwidth numbins)
"xmin xmax, the first and last bin centers"
  (let ((counts (make-array numbins :initial-element 0))
        quoc flquoc bin part tmp)
    (dolist (item data)
	    (cond
	     ((and (< item xmax) (> item xmin))
	      (setf quoc (/ (- item xmin) binwidth))
	      (setf flquoc (floor quoc))
	      (setf part (* flquoc binwidth))
	      (setf (aref counts (1+ flquoc)) 
		    (+ (aref counts (1+ flquoc))
		       (setq tmp (- quoc flquoc))))
	      (setf (aref counts flquoc)
		    (+ (aref counts flquoc)
		       (- 1 tmp))))
	     ((< item xmin)
	      (setf (aref counts 0) (1+ (aref counts 0))))
	     ((> item xmax)
	      (setf (aref counts (1- numbins)) 
		    (1+ (aref counts (1- numbins)))))))
    counts))

;;a previous version, still valid for sorted data
(defun bin-count-sorted-data (lowends sorted-data)
"returns a list of the number of elements of sorted-data less than or equal
to each element of lowend and greater then the previous one.
The total of the returned list can be less than
the length of sorted-data, when there ara data grater than the last lowend.
Useful for counting histograms"
(let ((next-one sorted-data)
      (current-count 0)
      (previous-count (length sorted-data))
      (result ()))
  (dolist (le lowends result)
          (setf next-one (member le next-one :test #'<))
          (setf current-count (length next-one))
          (setf result (append result (list (- previous-count current-count))))
          (setf previous-count current-count))))

;;;fast fourier transform
(defun build-fft-vector (data &optional (num-bins nil) )
  (let* ((data (coerce data 'vector))
         (nc (length data))
         (p (if num-bins
                num-bins
                (progn
                   (setf p 1)
                   (loop (if (> p (* 1.8 nc))
                             (return p)
                             (setq p (* 2 p))))))) 
	  (arc (make-array p :initial-element 0)))
     (when *kde*computing*inspect*
       (princ (format nil "computing a fft vector, ~a to ~a~%"
                      nc p)))
     (dotimes (i nc)
              (setf (aref arc i)
                    (aref data i)))
     (fft arc)))

;;;testing while developing
(setq data (normal-rand 1000))
(setq data2 (normal-rand 10000))

(defun testbinning ()
  (time (setq bc (bin-data data2 :xrange '(-6 6) :numbins 10 :outsiders 'none)))
  (time (setq bc3 (bin-data data2 :xrange '(-6 6)
			    :method 'linear :numbins 10 :outsiders 'none)))
  (time (setq bc4 (bin-data data2 :xrange '(-6 6)
			    :method 'linear :numbins 100 :outsiders 'none)))
  (setq bc2 (bin-data '(-4.5 2.5 6) :xrange '(-6 6) :numbins 10)))

(defun setbc5 ()
  (setq bc5 (bin-data  (normal-rand 5000)
		       :xrange '(-6 6) 
		       :method 'linear :numbins 400
		       :outsiders 'none)))

(defun bc () (load "binning"))
         
(provide "binning");;++++++++++++++++++++++++++
