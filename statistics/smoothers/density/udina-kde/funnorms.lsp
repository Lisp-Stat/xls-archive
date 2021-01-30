;file funnorms.lsp (former function-norms.lsp, damn 8 char limit of DOS!)
; f.udina jun 93
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; l1-norm and related functions
; for univariate real functions 
; does not currently supports care of precision
; except when called for a function

(provide "function-norms")

(defun l1-norm ( &optional (data nil)
                           &key function range x-count (precision 0.01))
"Returns an estimate of l1-norm of a function having yv values over xv
 Currrently, only for unidimensional functions.
 Can be called in any of
      (l1-norm :function closure :range range :x-count num :precision num)
where x-count and precision are optional, precision defaults to 10e-2,
      (l1-norm data)
where data is a list of points ((x1 y1) (x2 y2) ...)
      or as (l1-norm data :range range)
 where data is a list of yvalues and range is a list (xmin xmax)"
  (if (not data) ;there are no data, there is a fun and range
      (l1-norm-for-function function range x-count precision)
      (if range
          (l1-norm-for-data-range data range)
          (l1-norm-for-points data))))

; several functions called by l1norm depending on the arguments
; it gets.
(defun l1-norm-for-data-range (data range)
"args data range
 data is a list of yvalues, range is (xmin xmax)"
  (let* ((count (length data))
         (xvals (rseq (nth 0 range) (nth 1 range) count))
         (xyvals (transpose (list xvals data))))
    (l1-norm-for-points xyvals)))

(defun l1-norm-for-points (data)
"data must be a list of points ((x1 y1) (x2 y2) ...)"
;  (print data)
  (let* ((trdata (transpose data))
         (xvals (car trdata))
         (yvals (abs (first (cdr trdata))))
         (heights (/ (+ (butlast yvals) (rest yvals)) 2))
         (bases (- (rest xvals) (butlast xvals))))
    (apply #'+ (* heights bases))))

;this will be the maximum number of interval subdivisions before giving up
; in searching required precision
(setq *max-level-l1-norm* 10)

(defun l1-norm-for-function (function range x-count precision)
"args (function range x-count precision)
Calc the l1-norm, integral of absolute value"
  (let* ((x-count (if x-count x-count 13))
         (ends (iseq 1 (- x-count 1)))
         (segments (transpose (list (cons 0 ends) 
                                    (append ends (list x-count)))))
         (segments (+ (first range) 
                      (* (/ (apply #'- (reverse range)) x-count)
                         segments)))
         (values (mapcar #'(lambda (pair)
                         (auto-l1-norm function (first pair) (second pair)
                                       precision *max-level-l1-norm*))
                         segments))
         (values (first (transpose values))))
    (apply #'+ values)))


(defun auto-l1-norm (fun p1 p2 prec level)
"takes two points and a precision
 returns a value for the integral of fun between points to prec
 and flag t if possible. Returns the value and flag nil 
 if level reaches zero."
  (let* ((y1 (abs (funcall fun p1)))
         (y2 (abs (funcall fun p2)))
         (currprec (/ (* (abs (- y1 y2)) (- p2 p1)) 2)))
    (cond 
      ((< currprec prec)
       (list (* (- p2 p1) 
                (/ (+ y1 y2) 2))
                t))
      ((= level 0)
       (print (list "l1-norm prec fails between " p1 p2))
       (list (* (- p2 p1) 
                (/ (+ y1 y2) 2))
                nil))
      (t 
       (let* ((phalf (/ (+ p1 p2) 2))
              (firsthalf (auto-l1-norm fun p1 phalf (/ prec 2) 
                                       (- level 1)))
              (sechalf (auto-l1-norm fun phalf p2 (/ prec 2)
                                     (- level 1)))
              (flag (and (second firsthalf)(second sechalf))))
         (list (+ (first firsthalf) (first sechalf)) flag))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; l2-norm, first approach
; only works for a list of points

(defun expt2second (pair)
  (setf (second pair) (expt (second pair) 2))
  pair)

(defun l2-norm (data)
  (l1-norm-for-points (mapcar #'expt2second (copy-list data))))

(defun l0-norm (data)
  (max (abs (nth 1 (transpose data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compute second derivative of a table function or a function

; dirty version. must take care of precision!
(defun second-derivative (pair-list 
			  &key (function nil) (precission 0.01))
"Given a list of pairs (x f(x)), returns an estimate
of  f''(x) as a list of pairs (you loose the first and last x values).
 When given a :function, first arg
must be a list of x-values to use and :precision the one to reach. 
Not fully implemented. xvals are assumed to be equally spaced"

(let* ((xv (if function pair-list (first (transpose pair-list))))
       (yv (if function (mapcar function xv) (second (transpose pair-list))))
       (nv (length xv))
       (xinc (- (second xv) (first xv)))
       (yres (make-array (- nv 2)))
       (xres (cdr (butlast xv)))
       (yv (coerce yv 'vector)))
  (dotimes (i (- nv 2))
    (setf (select yres i)
	  (/ (+ (select yv i) (select yv (+ i 2)) (* -2 (select yv (+ i 1))))
	     xinc xinc)))
  (transpose (list xres (coerce yres 'list)))))


(defun integrate (points-list)
"data must be a list of points ((x1 y1) (x2 y2) ...)
 returns an approx to the integral of the line as graph of a function"
;  (print data)
  (let* ((trdata (transpose points-list))
         (xvals (car trdata))
         (yvals (first (cdr trdata)))
         (heights (/ (+ (butlast yvals) (rest yvals)) 2))
         (bases (- (rest xvals) (butlast xvals))))
    (apply #'+ (* heights bases))))

(defun integrate-sq-sec-deriv (&rest args)
"args: a list of points or a list of x-values and a :function
 returns (approx) to integrated squared second derivative"
(apply #'(lambda (x &key function)
	   (integrate (mapcar #'expt2second 
			      (second-derivative x :function function))))
       args))

