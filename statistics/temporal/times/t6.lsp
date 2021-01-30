; t6.lsp
; fft convolution and correlation
; Sat Nov 30 18:45:22 EST 1991
;
;Bill Hatch
;Coleman Research Corporation
;14502 Greenview Drive Suite 206
;Laurel, Maryland 20708
;Phone (301)470-3839
;FAX (301)776-5461
;HOME (301)441-1675
;uunet!bts!bill
;
(defun convolve (x h)
	"-------------------------------------------------------------------
	convolve (x h)  discrete concolution of 2 finite duration sequences
	h and x .  h and x may be different lengths
	Reference: Brigham, The Fast Fourier Transform And Its Applications
	Prentice-Hall 1988, page 208
	-------------------------------------------------------------------"
	(let ( (y 0) (N 0) (P 0) (Q 0) (myx 0) (myh 0)
	     )
	(setf P (length x))
	(setf Q (length h))
	(setf N (+ P Q))
	(setf myx (combine x (vec-const 0 (- N P))))
	;(print-object "myx: " myx)
	(setf myh (combine h (vec-const 0 (- N Q))))
	;(print-object "myh: " myh)
	(setf y (/ (* (fft myx) (fft myh)) N))
	;(print-object "fft(y): " y)
	(setf y (fft y t))
	(realpart y)
	) ; END let
)
(defun correlate (x h)
	"-------------------------------------------------------------------
	correlate (x h)  discrete correlation of 2 finite duration sequences
	h and x .  h and x may be different lengths. The output is scaled
	so that the largest positive peak is 1.0.
	Reference: Brigham, The Fast Fourier Transform And Its Applications
	Prentice-Hall 1988, page 228
	-------------------------------------------------------------------"
	(let ( (y 0) (N 0) (P 0) (Q 0) (myx 0) (myh 0)
	     )
	(setf P (length x))
	(setf Q (length h))
	(setf N (+ P Q))
	(setf myx (combine (vec-const 0 (- N P)) x))
	;(print-object "myx: " myx)
	(setf myh (combine h (vec-const 0 (- N Q) )))
	;(print-object "myh: " myh)
	(setf y (/ (* (fft myx) (conjugate (fft myh))) N))
	;(print-object "fft(y): " y)
	(realpart (fft y t))
	) ; END let
)
(defun test-convolve ()
	"----------------------------------------------------------------
	test-convolve ()
	-----------------------------------------------------------------"
	(let ((N 0) (Q 10) (P 10) (N 0)
	     )
		(setf x (list 1 2 3))
		(setf h x)
		(setf y (convolve x h))
		(print-object "convolve x h : " y)
		(message "(should be 1 4 10 12 9 0)")
		(setf y (correlate x x))
		(setf y (/ y (max y)))
		(message "correlation output")
		(print-object "y: " y)
		(print-object "y should be : " (list
			0
			.214286
			.571429
			1
			.571429
			.214286)
		)
	) ; END let
)
(defun crshift (x)
	"-------------------------------------------------------------
	crshift (x) circular right shift of elements of list x
	CAUTION: this function has NOT been tested as of 12/4/91
	-------------------------------------------------------------"
	(block sh
		(if (not (listp x))
			(return-from sh (message "crshift: x not a list"))
		)
		(let ((y 0))
			(setf y (cdr x))
			(setf y (combine y (car x)))
			(return-from sh y)
		) ; END let
	) ; END block sh
)
(defun clshift (x)
	"-------------------------------------------------------------
	clshift (x) circular left shift of elements of list x
	CAUTION: this function has NOT been tested as of 12/4/91
	-------------------------------------------------------------"
	(block lsh
		(if (not (listp x))
			(return-from lsh (message "clshift: x not a list"))
		)
		(let ((y 0)(myx 0))
			(setf myx (reverse x))
			(setf y (cdr myx))
			(setf y (combine y (car myx)))
			(return-from lsh (reverse y))
		) ; END let
	) ; END block lsh
)
