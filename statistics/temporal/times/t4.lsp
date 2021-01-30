;	timeseries functions
;	W. Hatch  uunet!bts!bill
;	Wed Nov 20 13:52:59 EST 1991
(defun tb2 ()
	(test-bilinear2)
)
(defun test-bilinear24 ()
	"----------------------------------------------------------------
	test-bilinear24 ()  test d2a-freq, bilinear2, bilinear4,
	and digital-transfer
	----------------------------------------------------------------"
	(let (	(lp-proto 0) (lp 0) (lp-digital 0) (wd 0) (wa 0) (fa 0)
		)
		; --------------------------------------------------------
		; define Butterworth low-pass prototype
		;---------------------------------------------------------
		(setf lp-proto (combine 1 1 0 0 1 (* 2 (cos (/  pi 4)))))
		;------------------------------------------------------------
		; pre-warp the .2 Hz digital cutoff frequency
		;------------------------------------------------------------
		(setf wd (* 2 pi .2))
		(setf wa (tan (/ wd 2)))
		(setf fa (/ wa (* 2 pi)))
		(print-object "fa : " fa)
		;------------------------------------------------------------
		; compute analog filter and take bilinear transform
		;------------------------------------------------------------
		(setf lp (analog-low2low lp-proto fa))
		(setf lp-digital (bilinear2 lp))
		(print-object "lp-digital .2 Hz cutoff : " lp-digital)
		;------------------------------------------------------------
		; compute digital filter transfer function magnitude vs freq
		;------------------------------------------------------------
		(def lpd-transfer (digital-transfer lp-digital :steps 10))
		(print-object "lpd-transfer : " lpd-transfer)
		(def flow (d2a-freq .2))
		(def fhigh (d2a-freq .4))
		(print-object "digital flow : " (list flow " fhigh : " fhigh))
		(def band (analog-low2pass lp-proto flow fhigh))
		(print-object "band pass coef : " band)
		(def band-digital (bilinear4 band))
		(print-object "band-digital .2, .4 Hz cutoff : " band-digital)
		(def band-transfer (digital-transfer band-digital :steps 10))
		(print-object "band-transfer : " band-transfer)
	) ; END let
)
(defun d2a-freq (fd)
	"-------------------------------------------------------------------
	d2a-freq (fd) given digital frequency 0 <= fd <= .5 Hz,
	returns pre-warped analog frequency fa for use of bilinear transform.
	--------------------------------------------------------------------"
	(let ((fa 0) (wa 0))
	(block adf
		(when (or ( < fd 0 ) (> fd .5))
			(print-object "fd : " 
				(list fd " must be between 0 and .5 Hz")
			)
			(return-from adf)
		)
		(setf wa (tan (/ (* 2 pi fd) 2)))
		(setf fa (/ wa (* 2 pi)))
	) ; END block adf
	) ; END let
)
(defun a2d-freq (fa)
	"-------------------------------------------------------------------
	a2d-freq (fa) given digital frequency 0 <= fa <= + infinity
	returns un-warped digital frequency fd = 2*atan(2*pi*fd) / (2*pi)
	--------------------------------------------------------------------"
	(let ((fd 0) (wd 0))
	(block adf
		(when ( < fa 0 )
			(print-object "fa : " 
				(list fa " must be between 0 and + infinity")
			)
			(return-from adf)
		)
		(setf wd (* 2 (atan (* 2 pi fa))))
		(setf fd (/ wd (* 2 pi)))
	) ; END block adf
	) ; END let
)
(defun bilinear2 (coef)
	"---------------------------------------------------------------
	bilinear2 (coef) return bilinear transform of 2d order analog
	filter section. The analog section is assumed to have the form

		H(s) = Ka * (a2 * s**2 + a1 * s + a0) / (s**2 + b1 * s + bo)

	The resultant digital filter is assumed to have the form

		H(z) = Kd*(z**2 + alpha1*z * alpha0) / (z**2 + beta1*Z + beta0)

	The given coef contains (Ka a0 a1 a2 b0 b1) and the returned list
	contains (Kd alpha0 alpha1 beta0 beta1). Note that in the digital
	filter, H(z), the 2d order terms in both numerator are assumed
	to have a coefficient of 1.0 .  In the analog filter, H(s), only
	the denominator coefficient of s**2 is assumed to be 1.0 .
	---------------------------------------------------------------"
	(let ((Kd 0) (Ka 0) (a0 0) (a1 0) (a2 0) (b0 0) (b1 0) (b2 1) (yield 0)
		(alpha0 0) (alpha1 0) (beta0 0) (beta1 0) (Sn 0) (Sd 0)
	     ) ; END local initialization
	(block bl2
		(when (< (length coef) 6)
			(print-object "(length coef) : " (list
				(length coef) 
				"less than 6 \ncoef contents : "
				coef)
			)
			(return-from bl2)
		)
		; extract given coef contents 
		(setf Ka (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf b0 (nth 4 coef))
		(setf b1 (nth 5 coef))
		(setf Sn (+ a0 a1 a2))
		(setf Sd (+ b0 b1 b2))
		(setf Kd (/ (* Ka Sn) Sd))
		(setf alpha0 (/ (- (+ a2 a0) a1) Sn))
		(setf alpha1 (/ (* 2 (- a0 a2)) Sn))
		(setf beta0 (/ (- (+ b2 b0) b1) Sd))
		(setf beta1 (/ (* 2 (- b0 b2)) Sd))
		(setf yield (list Kd alpha0 alpha1 beta0 beta1))
		(return-from bl2 yield)
	) ; END block bl2
	) ; END let
)
(defun bilinear4 (coef)
	"---------------------------------------------------------------
	bilinear4 (coef) return bilinear transform of 4th order analog
	filter section. The analog section is assumed to have the form

			Ka * (a4 * s**4 + a3 * s**3 + a2 * s**2 + a1 * s + a0) 
		H(s) =  -----------------------------------------------------
			 (s**4 + b3 * s**3 + b2 * s**2 + b1 * s + bo)

	The resultant digital filter is assumed to have the form

        	Kd * (z**4 + alpha3 * z**3 + alpha2 * z**2 + alpha1*z * alpha0) 
	H(z) = ----------------------------------------------------------------
		(z**4 + beta3 * z**3 + beta2 * z**2 + beta1*Z + beta0)

	The given coef contains 
		(Ka a0 a1 a2 a3 a4 b0 b1 b2 b3 )

	The returned list contains 
		(Kd alpha0 alpha1 alpha2 alpha3 beta0 beta1 beta2 beta3)

	Note that in the digital filter, H(z), the 4th order terms in 
	both numerator are assumed to have a coefficient of 1.0 .  
	In the analog filter, H(s), only the denominator coefficient of 
	s**4 is assumed to be 1.0 .
	---------------------------------------------------------------"
	(let ((Kd 0) (Ka 0) (a0 0) (a1 0) (a2 0) (a3 0) (a4 0) 
		(b0 0) (b1 0) (b2 1) (b3 0) (b4 1) (yield 0)
		(alpha0 0) (alpha1 0) (alpha2 0) (alpha3 0)
		(beta0 0) (beta1 0) (beta2 0) (beta3 0) (Sn 0) (Sd 0)
	     ) ; END local initialization
	(block bl4
		(when (< (length coef) 10)
			(print-object "(length coef) : " (list
				(length coef) 
				"less than 10 \ncoef contents : "
				coef)
			)
			(return-from bl4)
		)
		; extract given coef contents 
		(setf Ka (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf a3 (nth 4 coef))
		(setf a4 (nth 5 coef))
		(setf b0 (nth 6 coef))
		(setf b1 (nth 7 coef))
		(setf b2 (nth 8 coef))
		(setf b3 (nth 9 coef))	
		(setf Sn (+ a0 a1 a2 a3 a4))
		(setf Sd (+ b0 b1 b2 b3 b4))
		(setf Kd (/ (* Ka Sn) Sd))
		(setf alpha0 (- (+ a4 a2 a0) (+ a3 a1)) )
		(setf alpha0 (/ alpha0 Sn))
		(setf alpha1 (+ (* 2 (- a3 a1))  (* 4 (- a0 a4))))
		(setf alpha1 (/ alpha1 Sn))
		(setf alpha2 (-  (* 6 (+ a4 a0))  (* 2 a2) )  )
		(setf alpha2 (/ alpha2 Sn))
		(setf alpha3 (+ (* 4 (- a0 a4)) (* 2 (- a1 a3))))
		(setf alpha3 (/ alpha3 Sn))
		(setf beta0 (- (+ b4 b2 b0) (+ b3 b1) ))
		(setf beta0 (/ beta0 Sd))
		(setf beta1 (+ (* 2 (- b3 b1))  (* 4 (- b0 b4))))
		(setf beta1 (/ beta1 Sd))
		(setf beta2 (-  (* 6 (+ b4 b0))  (* 2 b2) )  )
		(setf beta2 (/ beta2 Sd))
		(setf beta3 (+ (* 4 (- b0 b4)) (* 2 (- b1 b3))))
		(setf beta3 (/ beta3 Sd))
		(setf yield (list Kd alpha0 alpha1 alpha2 alpha3
			beta0 beta1 beta2 beta3))
		(return-from bl4 yield)
	) ; END block bl4
	) ; END let
)
(defun digital-transfer (coef &key (hfreq .5) (steps 50) (magnitude t))
	"------------------------------------------------------------------
	digital-transfer (coef &key (hfreq .5) (steps 50) magnitude t))
	returns the magnitude of the H(z) for z = exp(j*2*pi*f(i))
	where f(i) ranges from 0 Hz to hfreq in steps and j=sqrt(-1)

			    z**n + a(n-1)*z**(n-1) ... + a(1)*z + a(0)
		H(z)= Kd*  -------------------------------------------
			    z**n + b(n-1)*z**(n-1) ... + b(1)*z + b(0)

	coef contains (Kd a(0) a(1) ... a(n-1) b(0) b(1) ... b(n-1)

	if the given magnitude is false, then the complex digital transfer
	function is returned
	------------------------------------------------------------------"
	(let 	((num 0) (denom 0) (freq 0) (K 0) (n 0) (z 0) (dfreq 0)
		 (zexp 1) (y 0) (yield 0)
		)
	(block dt
		;--------------------------------------------------------
		; treat multiple sections (lists)
		;--------------------------------------------------------
		(if (listp (first coef))
			(return-from dt (multi-sect-digital-transfer 
				coef :hfreq hfreq :steps steps
				:magnitude magnitude))
		)
		(setf dfreq (/ hfreq steps))		; frequency step size
		(setf n (/ (- (length coef) 1) 2))	; order of H(z)
		(setf K (nth 0 coef))			; gain
		;------------------------------------------------------------
		; loop over frequencies
		;------------------------------------------------------------
		(dotimes (i steps)
			(setf z (exp (complex 0 (* i 2 pi dfreq))))
			(setf zexp 1)
			(setf num 0)
			(setf denom 0)
			;---------------------------------------------------
			; loop over powers of z = exp (complex 0 wt)
			;---------------------------------------------------
			(dotimes (j  n)
				(setf num 
					(+ num (* zexp (nth (+ j 1) coef)))
				)
				(setf denom
					(+ denom (* zexp (nth (+ j n 1) coef)))
				)
				;---------------------------------------------
				; compute (j+1)th power of z
				;---------------------------------------------
				(setf zexp (* zexp z))
			) ; END dotimes j
			;------------------------------------------------------
			; nth power of z has coefficient 1.0 in both num, denom
			;------------------------------------------------------
			(setf num (+ num zexp))
			(setf denom (+ denom  zexp))
			(if  magnitude
				(setf y (abs (* K (/ num denom))))
				(setf y (* K (/ num denom)))
			)
			(if (= i 0)
				(setf yield (list y))
				(setf yield (combine yield y))
			) ; END if
		) ; END dotimes i
		(return-from dt yield)
	) ; END block dt
	) ; END let
)
(defun multi-sect-digital-transfer (coef &key (hfreq .5) (steps 50)
	(magnitude t))
	"-------------------------------------------------------------
	multi-sect-digital-transfer (coef &key (hfreq .5) (steps 50) 
		(magnitude t))
	returns the magnitude of the H(z) for z = exp(j*2*pi*f(i))
	where f(i) ranges from 0 Hz to hfreq in steps and j=sqrt(-1)
	The transfer function for each section is of the form

			    z**n + a(n-1)*z**(n-1) ... + a(1)*z + a(0)
		HS(z)= Kd*  -------------------------------------------
			    z**n + b(n-1)*z**(n-1) ... + b(1)*z + b(0)
	
	and the overall transfer function is

		H(z) = H0(z) * H1(z) * ...

	coef contains (coef0 coef1 .... ) and for each section coef(i)
	contains

	(Kd a(0) a(1) ... a(n-1) b(0) b(1) ... b(n-1)

	if given magnitude is false, then the complex frequency domain
	transfer function is returned
	-------------------------------------------------------------"
	(block msdt
	(let ((coefi 0) (nsect 0) (Hi 0) (H 0))
		(setf nsect (length coef))
		(dotimes (i nsect)
			(setf coefi (nth i coef))
			(setf Hi (digital-transfer coefi 
				:hfreq hfreq :steps steps
				:magnitude magnitude))
			(if (= i 0)
				(setf H Hi)
				(setf H (* H Hi))
			) 
		) ; END dotimes i
		(return-from msdt H)
	) ; END let	
	) ; END block msdt
)
