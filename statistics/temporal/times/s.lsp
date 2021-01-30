;		s.lsp
;
;	xlispstat functions used in the analysis of UdFilter program
;	outputs.  These functions assume that the current directory
;	contains the UdFilter output files stateplt.dat, statesig.dat
;	and the individual channel residual files.
;
;	W. Hatch Sun Jul  7 07:55:18 EDT 1991
;	Coleman Research
;	Project Jasper Pink
;
(def wyse "wyse") ;
(def tek  "tek")  ;
(def pc   "pc")   ;
;(def term wyse)   ;
(def term "pc")
(def huge 1.e36)
(def minus-huge (- huge))
(def EPSILON 1.e-15)
(defun safediv (num denom) ;
	"----------------------------------------------------------------
	 safediv (num denom) returns scalar division result
	 ----------------------------------------------------------------"
	( cond 
		((/= denom 0.0)
			(/ num denom)
		)
		(t 1.e-14)
	)
	
)
(defun delta (x) ;
	"----------------------------------------------------------------
	 delta (x) returns scalar forward first difference, zero at end of list
	 ----------------------------------------------------------------"
	( cond 
		((not (endp (cddr x)))
			(- (second x) (first x)))
		(t 0.0 )
	)
)
(defun dydx (x y) ;
	"----------------------------------------------------------------
	 dydx (x y) returns scalar numerical first derivative
	 ----------------------------------------------------------------"
	(safediv (delta y) (delta x))
)
(defun vec-dydx (x y) ;
	"----------------------------------------------------------------
	 vec-dydx (x y) return list of dydx, chop off last 2 list elements
	 ----------------------------------------------------------------"
	(chop-last (chop-last (maplist #'dydx  x  (smooth-kernel x y))))
)
(defun load-seq (file) ;
	"----------------------------------------------------------------
	 load-seq (file) load col of data to y, make x real sequence (0,1)
	 ----------------------------------------------------------------"
	(def data (read-data-columns file))
	(def y (select data 0))
	(def x (rseq 0.0 1.0 (length y)))
)
(defun smooth-lowess (x y) ;
	"----------------------------------------------------------------
	 smooth-lowess (x y) return smoothed y sequence at given x values
	 ----------------------------------------------------------------"
	(second (lowess x y))
)
(defun smooth-spline (x y) ;
	"----------------------------------------------------------------
	 smooth-spline (x y) return smoothed y sequence at given x values
	 ----------------------------------------------------------------"
	(second (spline x y :xvals x))
)
(defun smooth-kernel (x y) ;
	"----------------------------------------------------------------
	 smooth-kernel (x y) return smoothed y sequence at given x values
	 ----------------------------------------------------------------"
	(second (kernel-smooth x y :xvals x))
)
(defun plot-dydx (x y &key (min-x minus-huge) (max-x huge))
	"----------------------------------------------------------------
	plot-dydx (x y &key (min-x minus-huge) (max-x huge))
	plot dy/dx chopping off last 2 end points in vec-dydx
	 ----------------------------------------------------------------"
	(tekpoints (chop-last (chop-last x)) (vec-dydx x y) 
		:min-x min-x :max-x max-x)
)
(defun print-list (file x &rest args ) ;
	"----------------------------------------------------------------
	 print-list (file x &rest args ) print multiple lists in parallel 
	 column format assumes lists are of equal length; NIL printed for 
	 some entries if this is not true
	 ----------------------------------------------------------------"
	;------------------------------------------------------------
	; open file and define local loop index variables
	;------------------------------------------------------------
	(setf pf (open file :direction :output))
	(let  ((i 0) (j 0))
	;------------------------------------------------------------
	; loop over element count of first list
	;------------------------------------------------------------
	(loop 
		(if (not (nth i x)) (close pf) )
		(if (not (nth i x)) (return t) )
		; ----------------------------------------------------
		; print element from first list
		; ----------------------------------------------------
		(princ (nth i x) pf)
		(format pf " ")
		(def yargs  args)
		; ----------------------------------------------------
		; print remainder of list elements
		; ----------------------------------------------------
		(dotimes (j (length args)  )
			(def ycar (car yargs))
			(def yargs (cdr yargs))	
			(princ (nth i ycar) pf)
			(format pf " ")
		)
		; ----------------------------------------------------
		; newline and increment list element count
		; ----------------------------------------------------
		(terpri pf)
		(setf i (+ i 1))
	))
	(close pf)
)
(defun vec+ (x y) ;
	"-------------------------------------------------------------
 	 vec+ (x y) addition of simple or compound objects, 
	 Tierney page 98,99
	 -------------------------------------------------------------"
	( if (or (compound-data-p x) (compound-data-p y))
		( map-elements #'vec+ x y)
		( + x y))
)
(defun vec- (x y) ;
	"-------------------------------------------------------------
 	 vec- (x y) subtraction of simple or compound objects, 
	 Tierney page 98,99
	 -------------------------------------------------------------"
	( if (or (compound-data-p x) (compound-data-p y))
		( map-elements #'vec- x y)
		( - x y))
)
(defun vec* (x y) ;
	"-------------------------------------------------------------
 	 vec* (x y) multiplication of simple or compound objects, 
	 Tierney page 98,99
	 -------------------------------------------------------------"
	( if (or (compound-data-p x) (compound-data-p y))
		( map-elements #'vec* x y)
		( * x y))
)
(defun vec/ (x y) ;
	"-------------------------------------------------------------
 	 vec/ (x y)  division of simple or compound objects, Tierney page 98,99
	-------------------------------------------------------------"
	( if (or (compound-data-p x) (compound-data-p y))
		( map-elements #'vec/ x y)
		( / x y))
)
(defun rms3 (x y z) ;
	"-------------------------------------------------------------
	 rms3 (x y z) returns list of rms of given 3 lists elements
	 -------------------------------------------------------------"
	(sqrt (vec+ (vec* x x) (vec+ (vec* y y) (vec* z z))))
)
(defun tekmode() ;
	"-------------------------------------------------------------
	 tekmode () if Wyse terminal, switch to tek graphics mode
		otherwise do nothing
	 -------------------------------------------------------------"
	(if (string= term wyse) (system "tekmode")
	)
)
(defun ansimode() ;
	"-------------------------------------------------------------
	 ansimode () if Wyse terminal switch to ansi character mode
		otherwise do nothing
	 -------------------------------------------------------------"
	(if (string= term wyse) (system "ansimode")
	)
)
(defun plot-density(x) ;
	"-------------------------------------------------------------
	 plot-density (x) plot probability density function, then plot
		normal sample density for same number of points
	 -------------------------------------------------------------"
	(tekmode)
	(plot-points(kernel-dens x))
	(read-line)
	(read-line)
	(plot-lines (kernel-dens (normal-rand (length x) ) :type "g"))
	(read-line)
	(ansimode)
)
(defun tekpoints(x y &key (min-x minus-huge) (max-x  huge))
	"-------------------------------------------------------------
	 tekpoints(x y &key (min-x minus-huge) (max-x  huge))
	 select range for x and plot-points
	 -------------------------------------------------------------"
	(let
		(
			(data 0)
			(plotx 0)
			(ploty 0)
		)
		(tekmode)
		(setf data (list x y))
		(if (> min-x (- huge))
			(setf data (gt-select data min-x))
		)
		(if (< max-x huge)
			(setf data (lt-select data max-x))
		)
		(setf plotx (select data 0))
		(setf ploty (select data 1))
		(plot-points plotx ploty)
		(read-line)
		(ansimode)
	) ; END let
)

(defun norm-plot (x) ;
	"-------------------------------------------------------------
	 norm-plot (x) normal probability plot of given list of values
	 -------------------------------------------------------------"
	(def __y (sort-data x ))
	(def __a (/ 1 (* 2 (length x))))
	(def __b (- 1 __a))
	(def __p (rseq __a __b (length x)))
	(tekpoints (normal-quant __p) __y)
	(undef (list '__y '__a '__b '__p))
)
(defun ksh () ;
	"-------------------------------------------------------------
	 ksh () escape to korn shell
	 -------------------------------------------------------------"
	(system "ksh")
)
(defun raw-psd ( x )
	"-------------------------------------------------------------
	 raw-psd(x) returns squared magnitude of fft(x)
	 raw psd is scaled such that sum of components is unity
	--------------------------------------------------------------"
	(let ((stdx 0) (abs-fftx 0) (power 0))
		(setf stdx (center-and-scale x))
		(setf abs-fftx (abs (/ (fft stdx) (length stdx))))
		(half-list (* 2 (* abs-fftx abs-fftx)))
	)
)
(defun plot-raw-psd (x)
	"-------------------------------------------------------------
	 plot-raw-psd(x) plots squared magnitude of fft(x)
	 vs normalized frequencies 0 to .5
	--------------------------------------------------------------"
	(let ((power 0) (stdx 0) (freq 0))
		(setf power (raw-psd x))
		(setf freq (rseq 0 .5 (length power)))
		(tekpoints freq power)
	)
)
	
	
(defun plot-smooth-psd (x)
	"--------------------------------------------------------------
	 plot-smooth-psd (x)  plots the smooth-psd of x at normalized
	 frequencies 0 to .5
	---------------------------------------------------------------"
	(let ((power 0) (stdx 0) (freq 0))
		(setf power (smooth-psd x))
		(setf freq (rseq 0 .5 (length power)))
		(tekpoints freq power)
	)
)
(defun smooth-psd (x)
	"--------------------------------------------------------------
	 smooth-psd (x) returns kernel-smooth of raw psd
	---------------------------------------------------------------"
	(let ((freq 0) (power 0))
		(setf power (raw-psd x))
		(setf freq (rseq 0 .5 (length power)))
		(second (kernel-smooth freq power :width .1 :xvals freq))
	)
)
(defun half-list (x)
	"-------------------------------------------------------------
	 half-list(x) returns list containing first half of the elements
		in x 
	--------------------------------------------------------------"
	(let ((j 0) (len 0))
		(def len (/ (length x) 2))
		(loop
			(def x (chop-last x))
			(def j (+ j 1))
			(if (or (eql j len) (zerop len))
				(return x)
			)
		)
	)
)
(defun center-and-scale (x)
	"-------------------------------------------------------------
	center-and-scale (x) return x with mean subtracted and divided
		by standard deviation
	--------------------------------------------------------------"
	(let ((xbar 0) (xsig 0))
		(setf xbar (mean x))
		(setf xsig (standard-deviation x))
		(vec/ (- x xbar) xsig)
	)
)
(defun read-xyz (file)
	"-------------------------------------------------------------
	read-xyz (file)  def x y, and z as first 3 data column read from file
	--------------------------------------------------------------"
	(let ((col 0))
		(setf col (read-data-columns file))
		(def x (select col 0))
		(if (>= (length col) 2)
			(def y (select col 1))
		)
		(if (>= (length col) 3)
			(def z (select col 2))
		)
	)
)
(defun greater-than (x y)
	"-------------------------------------------------------------
	greater-than (x y) returns true if x greater than y, else nil
	--------------------------------------------------------------"
	(plusp (- x y)) 
)
(defun less-than (x y)
	"-------------------------------------------------------------
	less-than (x y) returns true if x is less than y, else nil
	--------------------------------------------------------------"
	(minusp (- x y))
)
(defun equal-to (x y)
	"-------------------------------------------------------------
	equal-to (x y) returns true if x is equal to y, else nil
	--------------------------------------------------------------"
	(equalp x y)
)
(defun log10 (x)
	"-------------------------------------------------------------
	log10 (x) returns log to the base 10 of x, vectorized
	--------------------------------------------------------------"
	(/ (log x) (log 10.))
)
(defun pow (x y)
	"-------------------------------------------------------------
	pow (x y)  returns x raised to the yth power
	--------------------------------------------------------------"
	(exp (* (log x) y))
)
(defun limit (x &key (lower .05) (upper .95))
	"-------------------------------------------------------------
	limit (x &key (lower .05) (upper .95)) evaluates the upper and
	lower quantiles of x and returns x with values above the upper
	quantile set to the upper quantile and values less than the lower
	quantile set to the lower quantile.
	--------------------------------------------------------------"
	(let ((idx 0) (x-up 0) (x-low 0) (x-value 0) (x-yield 0))
		(setf x-up (quantile x upper))
		(setf x-low (quantile x lower))
		(dotimes (i (length x) x-yield)
			(setf idx i)
			(setf x-value (nth idx x))
			(if (> x-value x-up)
				(setf x-value x-up)
			)
			(if (< x-value x-low)
				(setf x-value x-low)
			)
			(if (= i 0)
				; i = 0
				(setf x-yield (list x-value))
				; i /= 0
				(setf x-yield (combine x-yield x-value))
			)
		) ; END dotimes
	) ; END let
)
(defun atan (y)
	"---------------------------------------------------------------
	atan (y)  returns the arc tangent of y in radians
	---------------------------------------------------------------"
	(let ((fx 0) (x0 0) (x1 0) (endc 0) (ya 0) (yb 0) (signy 0) 
		(cx 0) (dx 0)
	     )
	(block at
		(setf endc (* 1.e4 EPSILON))
		;-----------------------------------------------------------
		; reductions to work with principal angle between 0 and pi/4
		;-----------------------------------------------------------
		(if (< 0 y) 
			(setf signy 1)
			(setf signy (- 1))
		)
		(if (= 0. y)
			(setf signy 0.)
		)
		(setf ya (abs y))
		;-----------------------------------------------------------
		; if angle is greater than pi/4 then work with its complement
		;-----------------------------------------------------------
		(if (> ya 1)
			(setf yb (/ 1 ya))
			(setf yb ya)
		)
		(setf x1 yb)
		;-----------------------------------------------------------
		; Newton-Rapson iterations
		;-----------------------------------------------------------
		(loop 
			(if (< (abs (- x0 x1)) endc)
				(return-from at) 
			)
			(setf x0 x1)
			(setf fx (- yb (tan x0)))
			(setf cx (cos x0))
			(setf dx (* fx cx cx))
			(setf x1 (+ x0 dx))
			;(print-object "x1 : " x1)
		) ; END loop
	) ; END block at
	;------------------------------------------------------------------
	; take complement if initial principal angle > pi/4
	;------------------------------------------------------------------
	;(print-object "ya : " ya)
	(if (> ya 1)
		(setf x1 (- (/ pi 2) x1))
	)
	;------------------------------------------------------------------
	; put returned angle in 2d quadrant if given argument was negative
	;------------------------------------------------------------------
	;(print-object "signy : " signy)
	;(print-object "x1 : " x1)
	(if (> 0 signy)
		(setf x1 (- x1))
	)
	;(print-object "x1 : " x1)
	(setf xb x1)
	) ; END let
)
