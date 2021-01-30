; t5.lsp
; top level digital filter design functions
; Sun Nov 24 19:36:43 EST 1991
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
(def butterworth "butterworth")
(defun dftt ()
	(digital-filter-design-test)
)

(defun digital-filter-design-test ()
	"----------------------------------------------------------------
	digital-filter-design--test () test the following functions
		- low-pass-design
		- high-pass-design
		- band-pass-design
		- band-stop-design
		- digital-transfer
		- butterworth-analog-lp-proto
		- d2a-freq
		- bilinear2
		- bilinear4
	-----------------------------------------------------------------"
	(let ((coef 0) (H 0) (freq 0))
		(setf freq (rseq 0 .5 50))
		(message "low pass digital filter design")
		(setf coef (low-pass-design .2 (- 3.) .3 (- 20.)))
		(setf H (digital-transfer coef))
		(tekpoints freq H)
		(message "high pass digital filter design")
		(setf coef (high-pass-design .2 (- 20.) .3 (- 3.)))
		(setf H (digital-transfer coef))
		(tekpoints freq H)
		(message "band pass digital filter design")
		(setf coef (band-pass-design .1 (- 20.) .2 (- 3.) .3 (- 3.)
			.4 (- 20.)))
		(setf H (digital-transfer coef))
		(tekpoints freq H)
		(message "band stop digital filter design")
		(setf coef (band-stop-design .1 (- 3.) .2 (- 20.) .3 (- 20.)
			.4 (- 3.)))
		(setf H (digital-transfer coef))
		(tekpoints freq H)
	) ; END let
)
(defun low-pass-design (fp gp fs gs &key (type "butterworth"))
	"-------------------------------------------------------------------
	low-pass-design (fp gp fs gs &key (type butterworth))
	given pass frequency fp and gp, the gain in db at fp, the stop
	frequency fs and gs, the gain in db at fs.  gp and gs must be
	negative with gp > gs.  both fp and fs must be between 0 and .5
	with fp < fs.  the function returns a list of lists.
	each of the component lists contains the digital gain Kd and the
	coefficients for one serial section of the filter.
	a typical 2d order section would contain (Kd a0 a1 b0 b1) and
	the corresponding digital transfer function for the section is

		H(z) = Kd * (z**2 + a1 * z + a0) / (z**2 + b1 * z + b0)

	note that the coefficients of z**2 is 1.0 in both numerator and
	denominator.  

	at present butterworth is the only type that is implemented.
	-------------------------------------------------------------------"
	(block lpd
		(when (or (> 0. fp) (> 0. fs) (> fp fs) (> fp .5) (> fs .5))
			(print-object 
				"low-pass-design:: fp : " (list fp " fs : " fs))
			(message 
			  "requires: 0 < fp < .5 ; 0 < fs < .5 ; and fp < fs")
			(return-from lpd)
		)
		(when (or (> gs 0) (> gp 0) (> gs gp))
			(print-object 
				"low-pass-design:: gp : " (list gp " gs : " gs))
			(message "requires: 0 > gp, 0 > gs and gp > gs")
			(return-from lpd)
		)
	;(message "low-pass-design: validity checks ok")
	(let 	((nsect 0) (fc 0) (y 0) (yield 0) (n2 0) (order-fc 0) 
		(pass-freq 0)
		(stop-freq 0) (pass-gain 0) (stop-gain 0) (order 0)
		)
		;-------------------------------------------------------------
		; pre-warp the frequencies to map digital frequencies to
		; analog prototype 
		;-------------------------------------------------------------
		(setf pass-freq (d2a-freq fp))
		(setf stop-freq (d2a-freq fs))
		;-------------------------------------------------------------
		; determine number of sections and cutoff frequency
		; restrict to nsect even meaning all 2d order sections
		;-------------------------------------------------------------
		(setf order-fc (butterworth-size pass-freq gp stop-freq gs 
			:base 2))
		;(print-object "order-fc : " order-fc)
		(setf order (nth 0 order-fc))
		(setf fc (nth 1 order-fc))
		(setf nsect (/ order 2))
		(dotimes (k nsect)
			(setf y (butterworth-analog-lp-proto order k))
			;(print-object "butterworth-analog-proto: " y)
			(setf y (analog-low2low y fc))
			;(print-object "analog-low2low: " y)
			(setf y (bilinear2 y))
			;(print-object "bilinear: " y)
			(if (= k 0)
				(setf yield (list y))
				(setf yield (append yield (list y)))
			)
		) ; END dotimes
		(return-from lpd yield)
	) ; END let
	) ; END block lpd
)
(defun butterworth-analog-lp-proto (order k)
	"-------------------------------------------------------------------
	butterworth-analog-lp-proto (order k)
	-------------------------------------------------------------------"
	(let ((yield 0) (b1 0))
		(setf b1 (* (+ (* 2 k) 1) pi))
		(setf b1 (* 2 (cos (/ b1 (* 2 order)))))
		(setf yield (list 1 1 0 0 1 b1 1))
	) ; END let
)
(defun high-pass-design (fs gs fp gp &key (type "butterworth"))
	"-------------------------------------------------------------------
	high-pass-design (fs gs fp gp &key (type butterworth))
	given stop frequency fs and gs, the gain in db at fs, the pass
	frequency fp and gp, the gain in db at fp.  gs and gp must be
	negative with gp > gs.  both fp and fs must be between 0 and .5
	with fp > fs.  the function returns a list of lists.
	each of the component lists contains the digital gain Kd and the
	coefficients for one serial section of the filter.
	a typical 2d order section would contain (Kd a0 a1 b0 b1) and
	the corresponding digital transfer function for the section is

		H(z) = Kd * (z**2 + a1 * z + a0) / (z**2 + b1 * z + b0)

	note that the coefficients of z**2 is 1.0 in both numerator and
	denominator.  

	at present butterworth is the only type that is implemented.
	-------------------------------------------------------------------"
	(block hpd
		(when (or (> 0. fp) (> 0. fs) (< fp fs) (> fp .5) (> fs .5))
			(print-object 
			    "high-pass-design:: fp : " (list fp " fs : " fs))
			(message 
			  "requires: 0 < fp < .5 ; 0 < fs < .5 ; and fp > fs")
			(return-from hpd)
		)
		(when (or (> gs 0) (> gp 0) (> gs gp))
			(print-object 
			    "high-pass-design:: gp : " (list gp " gs : " gs))
			(message "requires: 0 > gp, 0 > gs and gp > gs")
			(return-from hpd)
		)
	;(message "high-pass-design: validity checks ok")
	(let 	((nsect 0) (fc 0) (y 0) (yield 0) (n2 0) (order-fc 0) 
		(pass-freq 0)
		(stop-freq 0) (pass-gain 0) (stop-gain 0) (order 0)
		)
		;-------------------------------------------------------------
		; pre-warp the frequencies to map digital frequencies to
		; analog prototype 
		;-------------------------------------------------------------
		(setf pass-freq (d2a-freq fp))
		(setf stop-freq (d2a-freq fs))
		;-------------------------------------------------------------
		; convert frequencies to mirror image for low pass prototype
		;-------------------------------------------------------------
		(setf pass-freq (- .5 pass-freq))
		(setf stop-freq (- .5 stop-freq))
		;-------------------------------------------------------------
		; determine number of sections and cutoff frequency
		; restrict to nsect even meaning all 2d order sections
		;-------------------------------------------------------------
		(setf order-fc (butterworth-size pass-freq gp stop-freq gs 
			:base 2))
		;(print-object "order-fc : " order-fc)
		;-------------------------------------------------------------
		; extract order and fc, take mirror image of fc to get
		; high pass cutoff frequency
		;-------------------------------------------------------------
		(setf order (nth 0 order-fc))
		(setf fc (- .5 (nth 1 order-fc)))
		(setf nsect (/ order 2))
		(dotimes (k nsect)
			(setf y (butterworth-analog-lp-proto order k))
			;(print-object "butterworth-analog-proto: " y)
			(setf y (analog-low2high y fc))
			;(print-object "analog-low2high: " y)
			(setf y (bilinear2 y))
			;(print-object "bilinear: " y)
			(if (= k 0)
				(setf yield (list y))
				(setf yield (append yield (list y)))
			)
		) ; END dotimes
		(return-from hpd yield)
	) ; END let
	) ; END block hpd
)
(defun band-pass-design (fs1 gs1 fp1 gp1 fp2 gp2 fs2 gs2 
	&key (type "butterworth"))
	"-------------------------------------------------------------------
	band-pass-design (fs1 gs1 fp1 gp1 fs2 gp2 fs2 gs2 
		&key (type butterworth))
	given frequencies fs1, fp1, fp2, fs2 and the corresponding gains
	gp1, fp2, gp2, and gs2, where fs1 and fs2 are the stop frequencies, 
	gs1 and gs2 are the stop gains, fp1 and fp2 are the pass frequencies, 
	gp1 and gp2 are the pass gains.
	The frequencies are in Hz with 0 < fs1 < fp1 < fp2 < fs2 < .5 .
	The gains are negative db with gs1 and gs2 < gp1 and gp2 .  
	
	the function returns a list of lists.  each of the component 
	lists contains the digital gain Kd and the coefficients for 
	one serial section of the filter.  a typical 4th order section 
	would contain (Kd a0 a1 a2 a3 b0 b1 b2 b3) and the 
	corresponding digital transfer function for the section is

			    (z**4 + a3 * z**3 + a2 * z**2 + a1 * z + a0)
		H(z) = Kd * --------------------------------------------
			    (z**4 + b3 * z**3 + b2 * z**2 + b1 * z + b0)

	note that the coefficients of z**4 is 1.0 in both numerator and
	denominator.  

	at present butterworth is the only type that is implemented.
	-------------------------------------------------------------------"
	(block bpd
		(when (or 
			(> 0. fp1) (> 0. fp2)
			(> 0. fs1) (> 0. fs2)
			(not (< fs1 fp1 fp2 fs2))
			(> fp1 .5) (> fp2 .5)
			(> fs2 .5) (> fs2 .5)
		      )
			(print-object 
				"band-pass-design:: fs1 : " 
				(list fs1 
					" fp1 : " 
					fp1  
					" fp2 : " 
					fp2 " fs2 : " 
					fs2)
				)
			(message 
	  "requires: 0 < fp. < .5 ; 0 < fs. < .5 ; and fs1 < fp1 < fp2 < fs2")
			(return-from bpd)
		) ; END when

		(when (or (> gs1 0) (> gs2 0) (> gp1 0) (> gp2 0)
			(> gs1 gp1) (> gs2 gp2 ))
			(print-object 
			"band-pass-design:: gs1 : " 
				(list gs1 
					" gp1 : " 
					gp1
					" gp2 : " 
					gp2  
					" gs2 : " 
					gs2 )
				)
			(message 
		"requires: 0 > gp[12] , 0 > gs[12]  and gp[12] > gs[12]")
			(return-from bpd)
		) ; END when

	;(message "band-pass-design: validity checks ok")
	(let 	((nsect 0) (fc1 0) (fc2 0) (y 0) (yield 0) (n2 0) 
		(order-fc1 0) (ordef-fc2 0) 
		(pass-freq1 0) (pass-freq2 0)
		(stop-freq1 0) (pass-freq2 0)
		(pass-gain 0) (stop-gain 0) (order 0)
		)
		;-------------------------------------------------------------
		; pre-warp the frequencies to map digital frequencies to
		; analog prototype 
		;-------------------------------------------------------------
		(setf pass-freq1 (d2a-freq (- .5 fp1)))
		(setf pass-freq2 (d2a-freq  fp2))
		(setf stop-freq1 (d2a-freq (- .5 fs1)))
		(setf stop-freq2 (d2a-freq fs2))
		;-------------------------------------------------------------
		; determine number of sections and cutoff frequency
		; restrict to nsect even meaning all 2d order sections
		;-------------------------------------------------------------
		(setf order-fc2 (butterworth-size pass-freq2 gp2 stop-freq2 gs2 
			:base 2))
		(setf order-fc1 (butterworth-size pass-freq1 gp1 
			stop-freq1 gs1 :base 2))
		;(print-object "from size [analog] : "
		;	(list "fc1-lp: " (second order-fc1)
		;	" fc2: " (second order-fc2))) 
		(setf order (max (first order-fc2) (first order-fc1)))
		(setf fc1 (- .5 (a2d-freq (second order-fc1))))
		;(print-object "fc1-bp [digital]: " fc1)
		(setf fc1 (d2a-freq fc1))
		;(print-object "fc1-bp [analog]: " fc1)
		(setf fc2 (second order-fc2))
		(setf nsect (/ order 2))
		(dotimes (k nsect)
			(setf y (butterworth-analog-lp-proto order k))
			;(print-object "butterworth-analog-proto: " y)
			(setf y (analog-low2pass y fc1 fc2))
			;(print-object "analog-low2band: " y)
			(setf y (bilinear4 y))
			;(print-object "bilinear: " y)
			(if (= k 0)
				(setf yield (list y))
				(setf yield (append yield (list y)))
			)
		) ; END dotimes
		(return-from bpd yield)
	) ; END let
	) ; END block bpd
)
(defun band-stop-design (fp1 gp1 fs1 gs1 fs2 gs2 fp2 gp2 
	&key (type "butterworth"))
	"-------------------------------------------------------------------
	band-stop-design (fp1 gp1 fs1 gs1 fs2 gs2 fp2 gp2 
		&key (type butterworth))
	given frequencies fp1, fs1, fs2, fp2 and the corresponding gains
	gs1, gp1, gs2, and gp2, where fs1 and fs2 are the stop frequencies, 
	gs1 and gs2 are the stop gains, fp1 and fp2 are the stop frequencies, 
	gp1 and gp2 are the stop gains.
	The frequencies are in Hz with 0 < fp1 < fs1 < fs2 < fp2 < .5 .
	The gains are negative db with gs1 and gs2 < gp1 and gp2 .  
	
	the function returns a list of lists.  each of the component 
	lists contains the digital gain Kd and the coefficients for 
	one serial section of the filter.  a typical 4th order section 
	would contain (Kd a0 a1 a2 a3 b0 b1 b2 b3) and the 
	corresponding digital transfer function for the section is

			    (z**4 + a3 * z**3 + a2 * z**2 + a1 * z + a0)
		H(z) = Kd * --------------------------------------------
			    (z**4 + b3 * z**3 + b2 * z**2 + b1 * z + b0)

	note that the coefficients of z**4 is 1.0 in both numerator and
	denominator.  

	at present butterworth is the only type that is implemented.
	-------------------------------------------------------------------"
	(block bsd
		(when (or 
			(> 0. fp1) (> 0. fp2)
			(> 0. fs1) (> 0. fs2)
			(not (< fp1 fs1 fs2 fp2))
			(> fp1 .5) (> fp2 .5)
			(> fs2 .5) (> fs2 .5)
		      )
			(print-object 
				"band-stop-design:: fs1 : " 
				(list fs1 
					" fp1 : " 
					fp1  
					" fp2 : " 
					fp2 " fs2 : " 
					fs2)
				)
			(message 
	  "requires: 0 < fp. < .5 ; 0 < fs. < .5 ; and fp1 < fs1 < fs2 < fp2")
			(return-from bsd)
		) ; END when

		(when (or (> gs1 0) (> gs2 0) (> gp1 0) (> gp2 0)
			(> gs1 gp1) (> gs2 gp2 ))
			(print-object 
			"band-stop-design:: gs1 : " 
				(list gs1 
					" gp1 : " 
					gp1
					" gp2 : " 
					gp2  
					" gs2 : " 
					gs2 )
				)
			(message 
		"requires: 0 > gp[12] , 0 > gs[12]  and gp[12] > gs[12]")
			(return-from bsd)
		) ; END when

	;(message "band-stop-design: validity checks ok")
	(let 	((nsect 0) (fc1 0) (fc2 0) (y 0) (yield 0) (n2 0) 
		(order-fc1 0) (ordef-fc2 0) 
		(pass-freq1 0) (pass-freq2 0)
		(stop-freq1 0) (pass-freq2 0)
		(pass-gain 0) (stop-gain 0) (order 0)
		)
		;-------------------------------------------------------------
		; pre-warp the frequencies to map digital frequencies to
		; analog prototype 
		;-------------------------------------------------------------
		(setf pass-freq1 (d2a-freq fp1))
		(setf pass-freq2 (d2a-freq (- .5 fp2)))
		(setf stop-freq1 (d2a-freq fs1))
		(setf stop-freq2 (d2a-freq (- .5 fs2)))
		;-------------------------------------------------------------
		; determine number of sections and cutoff frequency
		; restrict to nsect even meaning all 2d order sections
		;-------------------------------------------------------------
		(setf order-fc1 (butterworth-size pass-freq1 gp1 stop-freq1 gs1 
			:base 2))
		(setf order-fc2 (butterworth-size pass-freq2 gp2 
			stop-freq2 gs2 :base 2))
		;(print-object "from size [analog] : "
		;	(list "fc1-bs: " (second order-fc1)
		;	" fc2: " (second order-fc2))) 
		(setf order (max (first order-fc2) (first order-fc1)))
		(setf fc2 (- .5 (a2d-freq (second order-fc2))))
		;(print-object "fc2-bs [digital]: " fc2)
		(setf fc2 (d2a-freq fc2))
		;(print-object "fc2-bs [analog]: " fc2)
		(setf fc1 (second order-fc1))
		;(print-object "fc1-bs [analog]: " fc1)
		(setf nsect (/ order 2))
		(dotimes (k nsect)
			(setf y (butterworth-analog-lp-proto order k))
			(setf y (analog-low2stop y fc1 fc2))
			(setf y (bilinear4 y))
			(if (= k 0)
				(setf yield (list y))
				(setf yield (append yield (list y)))
			)
		) ; END dotimes
		;(message "END dotimes")
		(return-from bsd yield)
	) ; END let
	) ; END block bsd
)
