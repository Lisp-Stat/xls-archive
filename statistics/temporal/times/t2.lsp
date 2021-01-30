;	W. Hatch  uunet!bts!bill
;	Sun Nov  3 16:17:09 EST 1991
;
;	infinite impulse response (iir) digital filter design
;	and filtering of timeseries data
;-----------------------------------------------------------------------
(defun low-pass-iir (x fp gp fs gs)
	"---------------------------------------------------------------
	low-pass-iir (x fp gp fs gs) given pass freq fp, and gain  gp,
	stop frequency fs and gain gs; design iir low pass filter and
	filter the time series x.  returns the filtered time series.
	SEE iir-filter, low-pass-design
	---------------------------------------------------------------"
	(block lpiir
	(let ((y 0) (coef 0))
		(setf coef (low-pass-design fp gp fs gs))
		(return-from lpiir (iir-filter x coef))
	) ; END let
	) ; END block lpiir
)
(defun high-pass-iir (x fs gs fp gp)
	"---------------------------------------------------------------
	high-pass-iir (x fs gs fp gp) given stop frequency fs and gain gs; 
	pass freq fp, and gain  gp, design iir high pass filter and
	filter the time series x.  returns the filtered time series.
	SEE iir-filter, high-pass-design
	---------------------------------------------------------------"
	(block hpiir
	(let ((y 0) (coef 0))
		(setf coef (high-pass-design fs gs fp gp))
		(return-from hpiir (iir-filter x coef))
	) ; END let
	) ; END block hpiir
)
(defun band-pass-iir (x fs1 gs1 fp1 gp1 fp2 gp2 fs2 gs2)
	"---------------------------------------------------------------
	band-pass-iir (x fs1 gs1 fp1 gp1 fp2 gp2 fs2 gs2)
	given stop frequency fs1 and gain gs1 pass frequency fp1 and 
	gain  gp1, pass frequency fp2 and gain fp2, stop frequency fs2
	and gain gs2; design iir band pass filter and
	filter the time series x.  returns the filtered time series.
	SEE iir-filter, band-pass-design
	---------------------------------------------------------------"
	(block bpiir
	(let ((y 0) (coef 0))
		(setf coef (band-pass-design fs1 gs1 fp1 gp1 fp2 gp2 fs2 gs2))
		;(print-object "bp coef: " coef)
		(return-from bpiir (iir-filter x coef))
	) ; END let
	) ; END block bpiir
)
(defun band-stop-iir (x fp1 gp1 fs1 gs1 fs2 gs2 fp2 gp2)
	"---------------------------------------------------------------
	band-stop-iir (x fp1 gp1 fs1 gs1 fs2 gs2 fp2 gp2)
	given pass frequency fp1 and gain gp1, stop frequency fs1 and 
	gain  gs1, stop frequency fs2 and gain fs2, pass frequency fp2
	and gain gp2; design iir band stop filter and
	filter the time series x.  returns the filtered time series.
	SEE iir-filter, band-stop-design
	---------------------------------------------------------------"
	(block bsiir
	(let ((y 0) (coef 0))
		(setf coef (band-stop-design fp1 gp1 fs1 gs1 fs2 gs2 fp2 gp2))
		;(print-object "bs coef: " coef)
		;(setf y (digital-transfer coef))
		;(message "transfer magnitude of band stop")
		;(tekpoints (rseq 0 .5 (length y)) y)
		(return-from bsiir (iir-filter x coef))
	) ; END let
	) ; END block bsiir
)
(defun print-object (msg obj)
	"--------------------------------------------------------------
	print-object (msg obj)  display message, print object followed
	by newline
	--------------------------------------------------------------"
	(princ msg)
	(princ " ")
	(princ obj)
	(princ "\n")
)
(defun message (msg)
	"--------------------------------------------------------------
	message (msg)  print message followed by newline
	--------------------------------------------------------------"
	(princ msg)
	(princ "\n")
)
(defun iir-error (pass-gain pass-freq stop-gain stop-freq)
	(princ "pass-gain: ")
	(princ pass-gain)
	(princ " db  pass-freq: ")
	(princ pass-freq)
	(princ " Hz\n")
	(princ "stop-gain: ")
	(princ stop-gain)
	(princ " db stop-freq: ")
	(princ stop-freq)
	(princ " Hz\n")
	
)
(defun iir-filter (x coef)
	"----------------------------------------------------------------
	iir-filter (x coef) infinite impulse response (iir) filter
	x is the input time series and coef 
	is either 1.) a single 2d or 4th order section or 2.) a list
	of 2d and/or 4th order serial sections. Each 2d order filter 
	section is a list containing the coefficients (Kd a0 a1 b0 b1) 
	for a digital filter section of the form

			    z**2 + a1 * z + a0
		H(z) = Kd * -------------------
			    z**2 + b1 * z + b0

	The corresponding difference equation is

	    y(i) = Kd * {x(i) + a1*x(i-1) + a0*x(i-2) - b1*y(i-1) + b0*y(i-2)}

	Each 4th order section contains (Kd a0 a1 a2 a3 b0 b1 b2 b3) for
	a 4th order form of the above.

	The time series x is filtered forward and backward to cancal out
	any phase shift or delays.
	----------------------------------------------------------------"
	(block biir
	(let ((nsect 0) (y 0))
		;--------------------------------------------------------
		; coef is either the coefficients for a single section or
		; a list of sections
		;--------------------------------------------------------
		(if (listp (nth 0 coef))
			(setf nsect (length coef))
			(setf nsect 1)
		)
		;--------------------------------------------------------
		; forward pass
		;--------------------------------------------------------
		(setf y x)
		(if (listp (nth 0 coef))
			;-----------------------------------------------
			; multiple sections
			;-----------------------------------------------
			(dotimes (k nsect)
				(setf y (iir-section y (nth k coef)))
			)
			;-----------------------------------------------
			; single section
			;-----------------------------------------------
			(setf y (iir-section y coef))
		)
		;--------------------------------------------------------
		; reverse pass
		;--------------------------------------------------------
		;(print-object "reverse y : " y)
		(setf y (reverse y))
		;(print-object "reverse y : " y)
		(if (listp (nth 0 coef))
			;-----------------------------------------------
			; multiple sections
			;-----------------------------------------------
			(dotimes (k nsect)
				(setf y (iir-section y (nth k coef)))
			)
			;-----------------------------------------------
			; single section
			;-----------------------------------------------
			(setf y (iir-section y coef))
		)
		(setf y (reverse y))
	) ; END let
	) ; END block biir
)
(defun iir-section (x coef)
	"----------------------------------------------------------------
	iir-section (x coef)  2d or 4th order iir section 
	returns filtered time series. x is time series to be filtered, 
	For a 2d order section coef contains Kd, a0, a1, b0, b1 for the 
	digital transfer function

			    z**2 + a1 * z + a0
		H(z) = Kd * -------------------
			    z**2 + b1 * z + b0

	the corresponding difference equation is

	    y(i) = Kd * {x(i) + a1*x(i-1) + a0*x(i-2) - b1*y(i-1) + b0*y(i-2)}

	For a 4th order section coef contains (Kd a0 a1 a2 a3 b0 b1 b2 b3)
	for the 4th order version of the above.
	----------------------------------------------------------------"
	;(print-object "iir-section given coef : " coef)
	;(print-object "iir-section given x : " x)
	(let ((n 0) (y 0) (ynow 0) (gain 0) (a0 0) (a1 0) (b0 0) (b1 0)
		(newx 0))
	(block iir
		(if (> (length coef) 5)
			(return-from iir (iir4-section x coef))
		)
		(setf y (combine 0 0 ))
		(setf newx (combine  0 0 x))
		(setf n (length newx))
		(setf gain (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf b0 (nth 3 coef))
		(setf b1 (nth 4 coef))
		(dotimes (i n)
			(when (> i 1)
				(setf ynow (+  (nth i newx)
						(* a1 (nth (- i 1) newx))
						(* a0 (nth (- i 2) newx))
					   )
				)
				(setf ynow (* ynow gain))
				(setf ynow (- ynow (* b1 (nth (- i 1)  y) )))
				(setf ynow (- ynow (* b0 (nth  (- i 2) y) )))
				(setf y (combine y  ynow))
			) ; END when
		) ; END dotimes
		(setf y (chop-first y 2))
		(return-from iir  y)
	) ; END iir
	) ; END let
)
(defun iir4-section (x coef)
	"----------------------------------------------------------------
	iir4-section (x coef)  4th order iir section 
	returns filtered time series. x is time series to be filtered, 
	SEE iir-section
	----------------------------------------------------------------"
	;(print-object "iir4-section given coef : " coef)
	;(print-object "iir4-section given x : " x)
	(let ((n 0) (y 0) (ynow 0) (Kd 0) (a0 0) (a1 0) (a2 0) (a3 0)
		(b0 0) (b1 0) (b2 0) (b3 0)
		(newx 0))
	(block iir4
		(setf y (combine 0 0 0 0))
		(setf newx (combine  0 0 0 0 x))
		(setf n (length newx))
		(setf Kd (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf a3 (nth 4 coef))
		(setf b0 (nth 5 coef))
		(setf b1 (nth 6 coef))
		(setf b2 (nth 7 coef))
		(setf b3 (nth 8 coef))
		(dotimes (i n)
			(when (> i 3)
				(setf ynow (+  (nth i newx)
						(* a3 (nth (- i 1) newx))
						(* a2 (nth (- i 2) newx))
						(* a1 (nth (- i 3) newx))
						(* a0 (nth (- i 4) newx))
					   )
				)
				(setf ynow (* Kd ynow))
				(setf ynow (- ynow 
						(* b3 (nth (- i 1) y))
						(* b2 (nth (- i 2) y))
						(* b1 (nth (- i 3) y))
						(* b0 (nth (- i 4) y))
					)
				)
				(setf y (combine y  ynow))
			) ; END when
		) ; END dotimes
		(setf y (chop-first y 4))
		(return-from iir4  y)
	) ; END iir4
	) ; END let
)
(defun butterworth-size (fp gp fs gs &key (base 1))
	"---------------------------------------------------------------
	butterworth-size (fp gp fs gs &key (base 1)) given fp passband 
	frequency, gp the gain in db at frequency fp, fs stopband frequency, and
	gs the gain in db at frequency fs, returns list (n fc) where
	n is the order and fc is the cutoff frequency.   n is a multiple
	of base.  if n is to be even, base is 2; if n is to be a multiple
	of 4, then base is 4.

	note that gp = 20 log10 (gain at fp),  gs = 20 log10 (gain at fs) with
	both gp and gs assumed to be negative with gp > gs (low pass filter) 
	the frequencies fp < fs
	are assumed to be normalized such that 0 < fp  and 0 < fp 

	reference: Digital Filters, R. W. Hamming, Prentice-Hall,1977,
	pages 189-195 
	---------------------------------------------------------------"
	(let ((a 0) (e 0) (n 0) (fc 0))
	(block bs
		;------------------------------------------------------
		; assure that frequencies are between 0 and .5
		;------------------------------------------------------
		(when (> 0 fs)
			(princ "illegal negative frequency, fs:  ") (princ fs)
			(princ "\n")
			(return-from bs nil)
		)
		(when (> 0 fp)
			(princ "illegal negative frequency, fp:  ") (princ fp)
			(princ "\n")
			(return-from bs nil)
		)
		(when (< fs  fp)
			(princ "illegal frequencies fs:  ") (princ fs)
			(princ "greater than fp: ") (princ fp) (princ "\n")
			(return-from bs nil)
		)
		;------------------------------------------------------
		; assure that gains are negative db and gp > gs
		;------------------------------------------------------
		(if (< 0 gs)
			(setf gs (- gs))
		)
		(if (< 0 gp)
			(setf gp (- gp))
		)
		(when (< gp gs)
			(princ "illegal gains db, gp: ") (princ gp)
			(princ "less than gs: ") (princ gs) (princ "\n") 
			(return-from bs nil)
		)
		(setf gp (expt 10. (/ gp 20)))
		(setf gs (expt 10. (/ gs 20)))
		(setf e (sqrt (- (/ 1 gp) 1)))
		(setf a (sqrt (/ 1 gs)))
		;------------------------------------------------------
		; evaluate the integer order of the filter
		;------------------------------------------------------
		(setf n (log (/ e (sqrt (- (* a a) 1)))))
		(setf n (/ n (log (/ fp fs))))
		(if ( < (truncate n) n)
			(setf n (+ (truncate n) 1))
			(setf n (truncate n))
		)
		;------------------------------------------------------
		; assure that n is a multiple of 4, this simplifies
		; the logic of coefficient evaluation
		;------------------------------------------------------
		(if ( < (truncate (/ n base)) (/ n base))
			(setf n (* (+ (truncate (/ n base)) 1) base))
		)
		;------------------------------------------------------
		; evaluate filter cutoff frequency (3db down point)
		;------------------------------------------------------
		(setf e (exp (/ (log e) n)))
		(setf fc (/ fp e))
		(return-from bs (combine n fc))
	) ; END block bs
	) ; END let
)
(defun iir-test ()
	"------------------------------------------------------------------
	iir-test () test the following infinite impulse (iir) filter functions
		low-pass-iir
		high-pass-iir
		band-pass-iir
		band-stop-iir
	testing consists of generating a white noise time series, filtering
	the time series and plotting the smoothed power spectrum of the
	filter output
	------------------------------------------------------------------"
	(let ((y 0) (x 0) (ypsd 0) (xpsd 0) (xfer 0) (freq 0)(samp-size 200)
		(xfreq 0))
		(setf x (normal-rand samp-size))
		(setf freq (rseq 0 .5 (/ samp-size 2)))
		(setf xfreq (rseq 0 .5 50))
		(message "smooth psd of white noise input series")
		(setf xpsd (smooth-psd x))
		(tekpoints freq xpsd)
		;---------------------------------------------------------
		; low pass
		;---------------------------------------------------------
		(setf y (low-pass-iir x .2 (- 3) .3 (- 20) ))
		(message "smooth psd of low pass iir output")
		(message ".2Hz -3db .3Hz -20db")
		(setf ypsd (smooth-psd y))
		(tekpoints freq ypsd)
		(message "analytical transfer magnitude of low-pass")
		(message ".2Hz -3db .3Hz -20db")
		(setf xfer (digital-transfer 
			(low-pass-design .2 (- 3) .3 (- 20)) :steps 50))
		(tekpoints xfreq xfer)
		;---------------------------------------------------------
		; high pass
		;---------------------------------------------------------
		(setf y (high-pass-iir x .3 (- 20) .4 (- 3.)))
		(message "smooth psd of high pass iir output")
		(message ".3Hz -20db .4Hz -3db")
		(setf ypsd (smooth-psd y))
		(tekpoints freq ypsd)
		(message "analytical transfer magnitude of high pass")
		(message ".2Hz -3db .3Hz -20db")
		(setf xfer (digital-transfer
			(high-pass-design .3 (- 20) .4 (- 3)) :steps 50))
		(tekpoints xfreq xfer)
		;---------------------------------------------------------
		; band pass
		;---------------------------------------------------------
		(setf y (band-pass-iir x .3 (- 20) .35 (- 3.)
			.4 (- 3.) .45 (- 20) ))
		(message "smooth psd of band pass iir output") 
		(message "(.3Hz -20db .35Hz -3db .4Hz -3db .45Hz -20db)")
		(setf ypsd (smooth-psd y))
		(tekpoints freq ypsd)
		(message "analytical transfer magnitude of band pass iir")
		(message ".3Hz -20db .35Hz -3db .4Hz -3db .45Hz -20db")
		(setf xfer (digital-transfer (
			band-pass-design .3 (- 20) .35 (- 3) .4 (- 3)
				.45 (- 20)) :steps 50))
		(tekpoints xfreq xfer)
		;---------------------------------------------------------
		; band stop
		;---------------------------------------------------------
		(setf y (band-stop-iir x .2 (- 3) .25 (- 20.)
			.3 (- 20.) .35 (- 3) ))
		(message "smooth psd of band stop iir output")
		(message ".2Hz -3db .25Hz -20db .3Hz -20db .35Hz -3db")
		(setf ypsd (smooth-psd y))
		(tekpoints freq ypsd)
		(message "analytical transfer magnitude band stop iir")
		(message ".2Hz -3db .25Hz -20db .3Hz -20db .35Hz -3db")
		(setf xfer (digital-transfer 
			(band-stop-design .2 (- 3) .25 (- 20) .3 (- 20)
				.35 (- 3)) :steps 50))
		(tekpoints xfreq xfer)
	) ; END let
)
