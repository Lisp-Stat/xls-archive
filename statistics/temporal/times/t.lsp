;		t.lsp - xlispstat time series analysis functions
;
;		requires functions defined in s.lsp
;
;		by W. Hatch
;		   Coleman Research
;		   14502 Greenview Drive Suite 206
;		   Laurel, MD 20708
;
(defun get-filter (n cutoff rolloff &key (nyquest .5) ( units 10) 
	( type "lowpass") )
	"-------------------------------------------------------------
	get-filter (n cutoff rolloff &key (nyquest .5) (units 10) 
		(type lowpass))
	returns list of n real, frequency domain filter coefficients
	for given cutoff and rolloff. If units = 10, the rolloff is in
	dB per decade, if units = 2, the rolloff is in db per octave.
	The cutoff is the half power frequency in Hz. The type is
	lowpass or highpass.  The fft of the time series to be 
	filtered will be  multiplied by the real filter coeffients term by
	term.
	--------------------------------------------------------------"
	(let (
			(half-power (sqrt 0.5) )
			(gain 0)
			(sign (- 1)) 
			(freq-interval 0.)
			(active-freq 0.)
			(octaves 0.)
			(filter-factor 1.)
			(exp-var 0.)
			(iplus 0)
			(filter 0)
			(rev-filter 0)
			(small-gain .00001)
			(m 0)
			(oddn nil)
			(mless 0)
			(gain0 0.)
		)
		;------------------------------------------------------
		; set up for odd or even number of coefficients
		;------------------------------------------------------
		(when (evenp n)
			(setf m (/ n 2))
			(setf oddn nil)
		)
		(when (oddp n)
			(setf m (/ (- n 1) 2))
			(setf oddn t)
		)
		;------------------------------------------------------
		; set up for lowpass or highpass
		;------------------------------------------------------
		(when (equal type "lowpass")
		 	(setf gain0  1.) 
			(setf sign (- 1.))
			(print "get-filter lowpass")
		)
		(when (equal type "highpass")
			(setf gain0 0.)
			(setf sign 1)
			(print "get-filter highpass")
		)
		;------------------------------------------------------
		; set filter constants
		;------------------------------------------------------
		(setf filter-factor (log10 units))
		(setf num-filter-coef m)
		(setf freq-interval (/ nyquest m))
		;------------------------------------------------------
		; compute first half of filter coefficients
		;------------------------------------------------------
		(block end-of	; label block to allow return-from
		(dotimes  (i m) 
			;---------------------------------------------------
			; compute filter gain coeffivient
			;---------------------------------------------------
			(setf iplus (+ i 1))
			(setf active-freq (* freq-interval iplus))
			(setf octaves 
			   (safediv (log10 
				(safediv active-freq cutoff)) filter-factor))
			(setf exp-var (safediv (* rolloff octaves sign) 20.0))
			(setf exp-var (pow 10. exp-var))
			(setf gain (* exp-var half-power))
			;---------------------------------------------------
			; constrain gain to be <= 1.0
			;---------------------------------------------------
			(if (> gain 1)
				(setf gain 1)
			)
			;---------------------------------------------------
			; append gain to list of filter coefficients
			;---------------------------------------------------
			(if (eq i 0)
				(setf filter (list gain))
				(setf filter (combine filter gain))
			)
			;---------------------------------------------------
			; check if remainder of gains are constant
			;---------------------------------------------------
			(if (equal type "lowpass")
				;--------------------------------------------
				; lowpass - set remainder of gains to 0.
				;--------------------------------------------
				(when (<= gain small-gain)
					(setf mless (- (- m i) 1))
					(dotimes (j mless)
					    (setf filter (combine filter 0.))
					) ;			---------------
				    (return-from end-of ) ;jump out of block
				) ;				---------------
				;--------------------------------------------
				; highpass - set remainder of gains to 1.0
				;--------------------------------------------
				(when (>= gain 1.0)
					(setf mless (- (- m i) 1))
					   (dotimes (j mless)
					     (setf filter (combine filter 1.0))
					   );                  ----------------
					(return-from end-of) ;jump out of block
				) ;			       ----------------

			) ; ENDIF
		)  ; END dotimes
		)  ; END block end-of
		;------------------------------------------------------
		; append mirror image of filter to filter, if odd number
		; of terms then delete the extra term
		;------------------------------------------------------
		(setf rev-filter (reverse filter))
		(if (not oddn)
			(setf filter (chop-last filter))
		)
		(setf filter (combine filter rev-filter))
		(setf filter (combine gain0 filter))
		;------------------------------------------------------
		; return the list of filter coefficients
		;------------------------------------------------------
		;(first (list filter))
	) ; END let
) 
(defun real-filter (x cutoff rolloff &key (type "lowpass") (nyquest .5))
	"-------------------------------------------------------------
	real-filter (x cutoff rolloff &key (type lowpass) (nyquest .5))
	returns real x lowpass or highpass filtered
	--------------------------------------------------------------"
	(let  	(
			(forward-transform 0)
			(inverse-transform 0)
			(filter-coefficients 0)
			(imag-forward-transform 0)
			(real-forward-transform 0)
			(n 0)
		) ; END local declarations
		(setf n (length x))
		(setf forward-transform (fft x))
		(setf filter-coefficients
			(if (equal type "lowpass")
				(get-filter n cutoff rolloff 
					:type "lowpass" :nyquest nyquest)
				(get-filter n cutoff rolloff 
					:type "highpass" :nyquest nyquest)
			)
		)
		;(print "filter-coefficients")
		;(print filter-coefficients)
		;(print "given forward-transform")
		;(print forward-transform)
		(setf real-forward-transform 
			(* filter-coefficients (realpart forward-transform)))
		(setf imag-forward-transform 
			(* filter-coefficients (imagpart forward-transform)))
		(setf forward-transform 
			(complex real-forward-transform 
				imag-forward-transform))
		;(print "filtered forward-transform")
		;(print forward-transform)
		(setf forward-transform (/ forward-transform n))
		(setf inverse-transform (fft forward-transform t))
		(realpart inverse-transform)
	) ; END let
)
(defun real-band-filter (x low-freq high-freq rolloff &key (nyquest .5)
	(type "bandpass"))
	"-------------------------------------------------------------
	real-band-filter (x low-freq high-freq rolloff &key (nyquest .5)
		(type bandpass))
	returns real x after bandpass or bandstop filtering	
	--------------------------------------------------------------"
	(let ( (xlow 0))
		(if (equal type "bandpass")
			(setf xlow 
				(complex-filter (complex-filter x high-freq 
					rolloff :type "lowpass") low-freq 
					rolloff :type "highpass")
			)
			(setf xlow
				(+ (complex-filter x low-freq 
					rolloff :type "lowpass") 
					(complex-filter x high-freq 
					rolloff :type "highpass")
				)
			)
		) ; END if
		;(print "yielded complex time series")
		;(print xlow)
		(realpart xlow)
	) ; END let
)
(defun complex-filter (x cutoff rolloff &key (type "lowpass") (nyquest .5))
	"-------------------------------------------------------------
	complex-filter (x cutoff rolloff &key (type lowpass) (nyquest .5))
	returns complex lowpass or highpass filtered sequence
	--------------------------------------------------------------"
	(let  	(
			(forward-transform 0)
			(inverse-transform 0)
			(filter-coefficients 0)
			(imag-forward-transform 0)
			(real-forward-transform 0)
			(n 0)
			(n 0)
		) ; END local declarations
		(setf n (length x))
		(setf forward-transform (fft x))
		(setf filter-coefficients
			(if (equal type "lowpass")
				(get-filter n cutoff rolloff 
					:type "lowpass" :nyquest nyquest)
				(get-filter n cutoff rolloff 
					:type "highpass" :nyquest nyquest)
			)
		)
		(setf real-forward-transform 
			(* filter-coefficients (realpart forward-transform)))
		(setf imag-forward-transform 
			(* filter-coefficients (imagpart forward-transform)))
		(setf forward-transform 
			(complex real-forward-transform 
				imag-forward-transform))
		(setf forward-transform (/ forward-transform n))
		(setf inverse-transform (fft forward-transform t))
	) ; END let
)
(defun complex-demodulate (x mod-freq &key (cutoff .1) (rolloff 200))
	"-------------------------------------------------------------
	complex-demodulate (x mod-freq &key (cutoff .1) (rolloff 200)
	returns series x demodulated by mod-freq and low pass filtered
	mod-freq must be between 0. and .5 (normalized frequency)
	-------------------------------------------------------------"
	(let 	(	(n 0)
			(alpha 0)
			(real-alpha 0)
			(imag-alpha 0)
			(modx 0)
			(outx 0)
		)
		(setf mod-freq (- 0 mod-freq))
		(setf n (length x))
		(dotimes (i n)
			(setf real-alpha (cos (* 2 pi i mod-freq)))
			(setf imag-alpha (sin (* 2 pi i mod-freq)))
			(if (eq i 0)
				(setf modx (complex real-alpha imag-alpha))
				(setf modx (combine
					modx (complex real-alpha imag-alpha)))
			)
		)
		(setf outx (complex-filter modx cutoff rolloff))
		;(real-part outx)
	) ; END let
)
(defun gen-data (n amp freq )
	"-------------------------------------------------------------
	gen-data (n amp freq )
	returns n data points, (+ normal-rand (* amp (sin (* 2 PI i freq)))
	-------------------------------------------------------------"
	(let 	(	(data 0)
		)
		
		(dotimes (i n)
			(if (eq i 0)
				(setf data (* amp (sin (* 2 pi i freq))))
				(setf data (combine data 
					(* amp (sin (* 2 pi i freq)))))
			) ; END if
		) ; END dotimes
		(+ data (normal-rand n))
	) ; END let
)
(defun decimate ( x &optional (n 5))
	"--------------------------------------------------------------
	decimate ( x &optional (n 5))  returns list containing
	every nth element of the given list x, n defaults to 5
	--------------------------------------------------------------"
	(let ((new-x nil) (test 0) (i1 0))
		(dotimes (i (length x) new-x)
			(setf i1 (+ i 1))
			(setf test (truncate (/ (float i1) (float n))))
			(if (= 0 (- i1 (* n test)))
				(if (= i1 n)
					(setf new-x (list (nth i x)))
					(setf new-x (combine new-x (nth i x)))
				)

			)
		)
	) ; END let
)
