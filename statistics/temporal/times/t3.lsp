;	timeseries functions
;	W. Hatch  uunet!bts!bill
;	Mon Nov 18 19:05:48 EST 1991
(defun at ()
	(analog-test)
)
(defun analog-test ( )
	"----------------------------------------------------------------
	analog-test () test the following analog low pass prototype filter
	transforms in this xlispstat file
		- analog-low2low
		- analog-low2high
		- analog-low2pass
		- analog-low2stop
		- analog24-transfer
	-----------------------------------------------------------------"
	(def fproto (/ 1 (* 2 pi)))
	(def freq-tags (rseq 0 .5 50))
	;----------------------------------------------------------------
	; low pass prototype  -  2d order Butterworth
	;----------------------------------------------------------------
	(def lp-proto (combine 1 1 0 0 1 (* 2 (cos (/  pi 4)))))
	(def mag-lp-proto (analog24-transfer lp-proto))
	(message "lp-proto")
	(tekpoints freq-tags mag-lp-proto)
	;----------------------------------------------------------------
	; low2low
	;----------------------------------------------------------------
	(def lp (analog-low2low lp-proto .2))
	(def mag-lp (analog24-transfer lp))
	(message "low pass , cutoff .2 Hz ")
	(tekpoints freq-tags mag-lp)
	;----------------------------------------------------------------
	; low2high
	;----------------------------------------------------------------
	(def lp-proto (combine 1 1 0 0 1 (* 2 (cos (/ pi 4)))))
	(def hp (analog-low2high lp-proto .3 ))
	(def mag-hp (analog24-transfer hp ))
	(message "high pass , cutoff .3 Hz ")
	(print-object "high pass coefficients : " hp)
	(tekpoints freq-tags mag-hp)
	;----------------------------------------------------------------
	; low2pass
	;----------------------------------------------------------------
	(def bp (analog-low2pass lp-proto .2 .3 ))
	(def mag-bp (analog24-transfer bp))
	(message "band pass , cutoff .2 Hz , .3 Hz")
	(tekpoints freq-tags mag-bp)
	;----------------------------------------------------------------
	; low2stop
	;----------------------------------------------------------------
	(def bs (analog-low2stop lp-proto .2 .3 ))
	(def mag-bs (analog24-transfer bs))
	(message "band stop , cutoff .2 Hz , .3 Hz")
	(tekpoints freq-tags mag-bs)
)
(defun analog-low2low (coef freq)
	"-----------------------------------------------------------------
	analog-low2low (coef freq) given in coef the coefficients of an analog,
	low pass, second order filter with cutoff radian frequency of 1.0,
	return the coefficients of a low pass filter with cutoff 
	frequency of f Hz .  The assumed form of a second order section is

	   H(s) = K * (a2 * s**2 + a1 * s + a0)/(s**2 + b1 * s + b0)

	coef is a list (K a0 a1 a2 b0 b1), the returned list is
	(Ka alpha0 alpha1 alpha2 beta0 beta1 beta2)
	------------------------------------------------------------------"
	(let ((K 0) (a0 0) (a1 0) (a2 0) (b0 0) (b1 0) (Ka 0) (alpha0 0)
		(alpha1 0) (alpha2 0) (beta0 0) (beta1 0) (w 0) (yield 0))
		(setf K (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf b0 (nth 4 coef))
		(setf b1 (nth 5 coef))
		(setf w (* 2 pi freq))
		(setf alpha2 a2)
		(setf alpha1 (* a1 w))
		(setf alpha0 (* a0 w w))
		(setf beta1  (* b1 w))
		(setf beta0  (* b0 w w))
		(setf Ka K)
		(setf yield (combine Ka alpha0 alpha1 alpha2 beta0 beta1))
	) ; END let
)
(defun analog-low2high (coef freq)
	"-----------------------------------------------------------------
	analog-low2high (coef freq) given in coef the coefficients of an analog,
	low pass, second order filter with cutoff radian frequency of 1.0,
	return the coefficients of a high pass filter with cutoff 
	frequency of freq Hz .  The assumed form of a second order section is

	   H(s) = K * (a2 * s**2 + a1 * s + a0)/(s**2 + b1 * s + b0)

	coef is a list (K a0 a1 a2 b0 b1), the returned list is
	(Ka alpha0 alpha1 alpha2 beta0 beta1 beta2)
	------------------------------------------------------------------"
	(let ((K 0) (a0 0) (a1 0) (a2 0) (b0 0) (b1 0) (Ka 0) (alpha0 0)
			(alpha1 0) (alpha2 0) (beta0 0) (beta1 0) (w 0) 
			(yield 0)
			(w2 0)
		)
	(block l2h
		;(print-object "given coef : " coef)
		(setf K (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf b0 (nth 4 coef))
		(setf b1 (nth 5 coef))
		(setf w (* 2 pi freq))
		(setf w2 (* w w))
		(setf alpha2 a0)
		(setf alpha1 (* a1 w))
		(setf alpha0 (* a2 w2))
		(setf beta0  (/ w2 b0 ))
		(setf beta1  (/ (* b1 w) b0))
		(setf Ka (/ K b0))
		(setf yield (combine Ka alpha0 alpha1 alpha2 beta0 beta1))
		;(print-object "yielded coef: " yield)
		(return-from l2h yield)
	) ; END block l2h
	) ; END let
)
(defun analog-low2pass (coef lowfreq upfreq)
	"-----------------------------------------------------------------
	analog-low2pass (coef lowfreq upfreq) given in coef the coefficients 
	of an analog, low pass, second order filter with cutoff radian 
	frequency of 1.0, return the coefficients of a band pass filter with 
	lower cutoff frequency lowfreq and upper cutoff upfreq Hz.
	The assumed form of the given second order section is

		H(s) = K * (a2 * s**2 + a1 * s + a0)/(s**2 + b1 * s + b0)

	coef is a list (K a0 a1 a2 b0 b1), the returned list contains
	the coefficients for the 4th order band pass section as
	(Ka alpha0 alpha1 alpha2 alpha3 alpha4 beta0 beta1 beta2 beta3)
	------------------------------------------------------------------"
	(let (	(K 0) (a0 0) (a1 0) (a2 0) (b0 0) (b1 0) (Ka 0) (alpha0 0)
		(alpha1 0) (alpha2 0) (alpha3 0) (alpha4 0) (w2 0)
		(beta1 0) (beta1 0) (beta2 0) (beta3 0) (Band 0) (w 0) (yield 0)
	     )
	(block ab
		(when (< upfreq lowfreq)
			(print-object "analog-low2pass upfreq : ", 
				(combine upfreq " < lowfreq : " lowfreq))
			(return-from ab)
		)
		(setf K (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf b0 (nth 4 coef))
		(setf b1 (nth 5 coef))
		(setf w (sqrt (* 2 pi lowfreq 2 pi upfreq))) ;*** geo. mean freq
		(setf w2 (* w w))
		(setf Band (* 2 pi (- upfreq lowfreq))) ;*** Bandandwidth
		(setf Band2 (* Band Band))
		(setf alpha4 a2)
		(setf alpha3 (* a1 Band))
		(setf alpha2 (+ (* 2 a2 w2) (* a0 Band2)))
		(setf alpha1 (* a1 Band w2))
		(setf alpha0 (* a2 w2 w2))
		(setf beta0  (* w2 w2))
		(setf beta1  (* b1 Band w2))
		(setf beta2  (+ (* 2 w2) (* b0 Band2)))
		(setf beta3  (* b1 Band))
		(setf Ka  K )
		(setf yield (combine Ka alpha0 alpha1 alpha2 alpha3 alpha4
			beta0 beta1 beta2 beta3))
	) ; END block ab
	) ; END let
)
(defun analog-low2stop (coef lowfreq upfreq)
	"-----------------------------------------------------------------
	analog-low2stop (coef lowfreq upfreq) given in coef the coefficients 
	of an analog, low pass, second order filter with cutoff radian 
	frequency of 1.0, return the coefficients of a band stop filter with 
	lower cutoff frequency lowfreq and upper cutoff upfreq Hz.
	The assumed form of the given second order section is

		H(s) = K * (a2 * s**2 + a1 * s + a0)/(s**2 + b1 * s + b0)

	coef is a list (K a0 a1 a2 b0 b1), the returned list contains
	the coefficients for the 4th order band stop section as
	(Ka alpha0 alpha1 alpha2 alpha3 alpha4 beta0 beta1 beta2 beta3)
	------------------------------------------------------------------"
	(let (	(K 0) (a0 0) (a1 0) (a2 0) (b0 0) (b1 0) (Ka 0) (alpha0 0)
		(alpha1 0) (alpha2 0) (alpha3 0) (alpha4 0) (w2 0)
		(beta1 0) (beta1 0) (beta2 0) (beta3 0) (Band 0) (w 0) (yield 0)
	     )
	(block ap
		(when (< upfreq lowfreq)
			(print-object "analog-low2stop upfreq : ", 
				(combine upfreq " < lowfreq : " lowfreq))
			(return-from ap)
		)
		(setf K (nth 0 coef))
		(setf a0 (nth 1 coef))
		(setf a1 (nth 2 coef))
		(setf a2 (nth 3 coef))
		(setf b0 (nth 4 coef))
		(setf b1 (nth 5 coef))
		(setf w (sqrt (* 2 pi lowfreq 2 pi upfreq))) ;*** geo. mean freq
		(setf w2 (* w w))
		(setf Band (* 2 pi (- upfreq lowfreq))) ;*** Bandandwidth
		(setf Band2 (* Band Band))
		(setf alpha4 a0)
		(setf alpha3 (* a1 Band))
		(setf alpha2 (+ (* 2 a0 w2) (* a2 Band2)))
		(setf alpha1 (* a1 Band w2))
		(setf alpha0 (* a0 w2 w2))
		(setf beta0  (* w2 w2))
		(setf beta1  (/ (* b1 Band w2) b0))
		(setf beta2  (/ (+ (* 2 w2 b0) Band2) b0))
		(setf beta3  (/ (* b1 Band) b0))
		(setf Ka  (/ K b0))
		(setf yield (combine Ka alpha0 alpha1 alpha2 alpha3 alpha4
			beta0 beta1 beta2 beta3))
	) ; END block ap
	) ; END let
)
(defun analog24-transfer (coef &key (hfreq .5) (steps 50))
	"------------------------------------------------------------
	analog24-transfer (coef &key (hfreq .5) (steps 50))
	returns magnitude of analog transfer function for a 2d or 4th
	order filter section with gain and coefficients in coef.
	these are for 2d order section: (K a0 a1 a2 b0 b1)   
	with b2 assumed to be 1.0 and the transfer function given by

		H(S) = K * (a2 * s**2 + a1 * s + a0) / (s**2 + b1 * s + b0)

	For 4th order section the coefficients are 
	(K a0 a1 a2 a3 a4 b0 b1 b2 b3) with b4 assumed to be 1.0
	-------------------------------------------------------------"
	(let (	(K (complex 0 0)) (a0 (complex 0 0)) (a1 (complex 0 0)) 
		(a2 (complex 0 0)) 
		(b0 (complex 0 0)) (b1 (complex 0 0)) (Ka (complex 0 0))
		(a3 (complex 0 0)) (a4 (complex 0 0)) (b2 (complex 0 0)) 
		(b3 (complex 0 0)) 
		(yield 0) (deltaf 0)
		(y 0) (num 0) (denom 0) (w 0) (s1 (complex 0 0)) 
		(s2 (complex 0 0)) (s3 (complex 0 0))
		(s4 (complex 0 0)) (zero 0)
	     )
	(block ta
		(setf K (complex (nth 0 coef) 0))
		(setf a0 (complex (nth 1 coef) 0))
		(setf a1 (complex (nth 2 coef) 0))
		(setf a2 (complex (nth 3 coef) 0))
		(setf b0 (complex (nth 4 coef) 0))
		(setf b1 (complex (nth 5 coef) 0))
		(when (> (length coef)  6)
			(setf a3 (complex (nth 4 coef) 0))
			(setf a4 (complex (nth 5 coef) 0))
			(setf b0 (complex (nth 6 coef) 0))
			(setf b1 (complex (nth 7 coef) 0))
			(setf b2 (complex (nth 8 coef) 0))
			(setf b3 (complex (nth 9 coef) 0))
			(setf b4 (complex 1 zero))
		)
		(when (< (length coef) 7)
			(setf a3 (complex zero zero))
			(setf a4 (complex zero zero))
			(setf b2 (complex 1    zero))
			(setf b3 (complex zero zero))
			(setf b4 (complex zero zero))
		)
		(setf deltaf (/ hfreq steps))
		(dotimes (i steps)
			(setf num (complex 0 0))
			(setf denom (complex 0 0))
			(setf w (complex 0 (* 2 pi i deltaf)))
			(setf s1 w)
			(setf s2 (* s1 w))
			(setf s3 (* s2 w))
			(setf s4 (* s3 w))
			(setf num (+
					(* a4 s4)
					(* a3 s3)
					(* a2 s2)
					(* a1 s1)
					a0
				)
			)
			(setf denom (+
					(* b4 s4)
					(* b3 s3)
					(* b2 s2)
					(* b1 s1)
					b0
				)
			)
			(setf y (/  num denom))
			(setf y (* K y))
			(if (< i  1)
				(setf yield (list (abs y)))
				(setf yield (combine yield (abs y)))
			)
		)
		(return-from ta yield)
	) ; END block ta
	) ; END let
)
