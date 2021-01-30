;;;  double precision function ppnd16 (p, ifault)
;;;       algorithm as241  appl. statist. (1988) vol. 37, no. 3
;;;
;;;       produces the normal deviate z corresponding to a given lower
;;;       tail area of p; z is accurate to about 1 part in 10**16.
;;;
;;;       the hash sums below are the sums of the mantissas of the
;;;       coefficients.   they are included for use in checking
;;;       transcription.
;;;  translated from Fortran by F. Udina, not by an automatic translator
(defun ppnd16 (p ifault)
  (let* ((zero  0.d0) (one  1.d0) (half  0.5d0)
	 (split1  0.425d0) (split2  5.d0)
         (const1  0.180625d0) (const2 1.6d0)
	 a0 a1 a2 a3 a4 a5 a6 a7 b1 b2 b3 b4 b5 b6 b7
	 c0 c1 c2 c3 c4 c5 c6 c7 d1 d2 d3 d4 d5
	 d6 d7 e0 e1 e2 e3 e4 e5 e6 e7 f1 f2 f3
	 f4 f5 f6 f7 q r
	 resp)
;;;
;;;       coefficients for p close to 0.5
;;;
    (setq
     a0  3.3871328727963666080d0
     a1  1.3314166789178437745d+2
     a2  1.9715909503065514427d+3
     a3  1.3731693765509461125d+4
     a4  4.5921953931549871457d+4
     a5  6.7265770927008700853d+4
     a6  3.3430575583588128105d+4
     a7  2.5090809287301226727d+3
     b1  4.2313330701600911252d+1
     b2  6.8718700749205790830d+2
     b3  5.3941960214247511077d+3
     b4  2.1213794301586595867d+4
     b5  3.9307895800092710610d+4
     b6  2.8729085735721942674d+4
     b7  5.2264952788528545610d+3)
;;;       hash sum ab    55.88319 28806 14901 4439
;;;
;;;       coefficients for p not close to 0 0.5 or 1.
;;;
    (setq 
     c0  1.42343711074968357734d0
     c1  4.63033784615654529590d0
     c2  5.76949722146069140550d0
     c3  3.64784832476320460504d0
     c4  1.27045825245236838258d0
     c5  2.41780725177450611770d-1
     c6  2.27238449892691845833d-2
     c7  7.74545014278341407640d-4
     d1  2.05319162663775882187d0
     d2  1.67638483018380384940d0
     d3  6.89767334985100004550d-1
     d4  1.48103976427480074590d-1
     d5  1.51986665636164571966d-2
     d6  5.47593808499534494600d-4
     d7  1.05075007164441684324d-9)
;;;       hash sum cd    49.33206 50330 16102 89036
;;;
;;;       coefficients for p near 0 or 1.
;;;
    (setq
     e0  6.65790464350110377720d0
     e1  5.46378491116411436990d0
     e2  1.78482653991729133580d0
     e3  2.96560571828504891230d-1
     e4  2.65321895265761230930d-2
     e5  1.24266094738807843860d-3
     e6  2.71155556874348757815d-5
     e7  2.01033439929228813265d-7
     f1  5.99832206555887937690d-1
     f2  1.36929880922735805310d-1
     f3  1.48753612908506148525d-2
     f4  7.86869131145613259100d-4
     f5  1.84631831751005468180d-5
     f6  1.42151175831644588870d-7
     f7  2.04426310338993978564d-15)
;;;       hash sum ef    47.52583 31754 92896 71629
;;;
    (setq ifault 0)
    (setq q  (- p half))
    (if (<= (abs q)  split1)
	(progn
	  (setq r (- const1 (* q  q)))
	  (setq resp
		(* q (/  (+ a0 (* r (+ a1 (* r (+ a2 (* r (+ a3 (* r (+ a4 (* r (+ a5 (* r (+ a6 (* r a7))))))))))))))
			 (+ one (* r (+ b1 (* r (+ b2 (* r (+ b3 (* r (+ b4 (* r (+ b5 (* r (+ b6 (* r b7))))))))))))))))))
;;;else
      (progn
	(if (< q  zero)
	    (setq r p)
	  (setq r (- one p)))
	(if (< r zero)
	    (progn
	      (setq ifault 1)
	      (setq resp zero))
	  (progn
	    (setq r (sqrt (- (log r))))
	    (if (<= r split2)
		(progn
		  (setq r (- r const2))
		  (setq resp (/  (+ c0 (* r (+ c1 (* r (+ c2 (* r (+ c3 (* r (+ c4 (* r (+ c5 (* r (+ c6 (* r c7))))))))))))))
				   (+ one (* r (+ d1 (* r (+ d2 (* r (+ d3 (* r (+ d4 (* r (+ d5 (* r (+ d6 (* r d7)))))))))))))))))
	      (progn
		(setq r (- r split2))
		(setq resp (/  (+ e0 (* r (+ e1 (* r (+ e2 (* r (+ e3 (* r (+ e4 (* r (+ e5 (* r (+ e6 (* r e7))))))))))))))
				 (+ one (* r (+ f1 (* r (+ f2 (* r (+ f3 (* r (+ f4 (* r (+ f5 (* r (+ f6 (* r f7))))))))))))))))))
	    (when (< q zero)
		  (setq resp (- resp)))))))
    resp))



