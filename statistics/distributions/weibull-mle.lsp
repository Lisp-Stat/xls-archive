;;;; Course project:  STA450/2102S, University of Toronto, Spring 1993
;;;; Instructor: N. Reid
;;;; Student: M. Ennis
;;;; Project Title:  Maximum likelihood estimation of Weibull parameters
;;;;			(Chapter 2.8 of Lispstat, by L. Tierney)



;The functions below can all be typed in in XLISPSTAT, or you could copy 
;them into your account as separate files, eg weibull-ll.lsp, spin-weibull.lsp,
;etc. Then , from inside XLISPSTAT, say (load "weibull-ll"). XLISPSTAT will
;reply "T" if the load is successfull. Then simply use the function when needed.

;1. To find the values of lamda and beta that maximizes the Weibull
   ;log-likelihood using the default numerical differentiation in 'newtonmax':
   ;a) (load "weibull-ll:)
   ;b) (newtonmax #'weibull-ll (list 0.012 1))
     ; 
        ;(0.012 1) are starting values, the function weibull-ll just calculates
        ;the loglikelihood at whatever values one enters.
;
;2. To find the values of lamda and beta that maximizes the Weibull
   ;log-likelihood using the actual derivatives in 'newtonmax':
   ;a) (load "weibull-lld1")
   ;b) (newtonmax #'weibull-lld1 (list 0.012 1) )
;
       ;weibull-lld1 calculates the loglikelihood and the derivatives wrt
       ;lamda and beta at whatever values one enters.
;
;3. To make a spin-plot of the 2-dimesional loglikelihood (ie as a function
;of lamda and beta):
   ;a) (load "weibull-ll")   
   ;b) (load "spin-weibull")
   ;c) (spin-function #'spin-weibull 0.001 0.02  0.5 1.6)
;
     ;(0.001 to 0.02 is the range for lamda and (0.5 to 1.6) is the range for 
     ;beta that the 'spin-function' uses to make the plot. 'spin-weibull' is 
     ;needed because 'spin-function' wants two explicit parameters iso
     ;the list that 'newtonmax' wanted - otherwise the 2 routines are the same.
     ;I couldn't figure out how to label the axes, but I'm sure its possible.
;
;4. Plotting the likelihood as a function of lamda for beta fixed at 1.2933:
   ;a) (load "wlam")
   ;b) (def lamda (rseq 0.001 0.03 50))
   ;c) (plot-lines lamda (wlam lamda) )
;
;5. Plotting the deriv. of the loglik. wrt lamda as a function of lamda, for
   ;beta fixed at 1.2933:
   ;a) (load "wlamd1")
   ;b) (plot-lines lamda (wlamd1 lamda) )          - assuming lamda as in 4.
;
;6. Making the plot of the gamma and weibull densities (my way is very crude)
   ;a) (load "my-gamma-dens")
   ;b) (load "weibull-dens")
   ;c) (def xx (rseq 0 350 1000) )
   ;d) (def gx (my-gamma-dens 83.5173 1.67099 xx) )
   ;e) (def wx (weibull-dens 0.011 1.2933 xx) )
   ;f) (def myplot (plot-lines xx gx))
   ;g) (send myplot :add-lines xx wx)
;
        ;xx is the range of values for which I want to calculate the densities.
        ;gx and wx are the calculated values of the densities using the
        ;maximum-likelihood values for the parameters.
;
;
;FUNCTION DEFINITIONS:

(defun weibull-ll (init)
  (let* ( (lamda (select init 0))
          (beta (select init 1))
          (n (length x))
        )
     (+  (* n beta (log lamda))
         (* n (log beta))
         (* (- beta 1) (sum (log x)) )
         (* (- 1) (^ lamda beta) (sum (^ x beta)))
     )
  )
)

(defun weibull-lld1 (init)
  (let* ( (lamda (select init 0))
          (beta (select init 1))
          (n (length x))
        )
     (list (+  (* n beta (log lamda))
               (* n (log beta))
               (* (- beta 1) (sum (log x)) )
               (* (- 1) (^ lamda beta) (sum (^ x beta)))
           )
        (list   (+  (/ (* n beta) lamda)
                    (* -1 beta (^ lamda (- beta 1)) (sum (^ x beta)))
                )
                (+  (* n (log lamda))
                    (/ n beta)
                    (sum (log x))
                   (* -1 (^ lamda beta) (sum (* (^ x beta) (log (* lamda x)))))
                )
        )
      )
  )
)

(defun spin-weibull (lamda beta)
  (weibull-ll (list lamda beta)) )

(defun wlam (lamda)
  (let* ( (beta 1.2933)
          (n (length x))
        )
     (+  (* n beta (log lamda))
         (* n (log beta))
         (* (- beta 1) (sum (log x)) )
         (* (- 1) (^ lamda beta) (sum (^ x beta)))
     )
  )
)

(defun wlamd1 (lamda)
  (let* ( (beta 1.2933)
          (n (length x))
        )
   
     (+  (/ (* n beta) lamda)
         (* -1 beta (^ lamda (- beta 1)) (sum (^ x beta)))
      )
  )
)

(DEFUN MY-GAMMA-DENS (MU BETA X)
  (* (^ (/ BETA MU) BETA)
     (^ X (- BETA 1))
     (EXP (- (/ (* BETA X) MU)))
     (/ 1 (EXP (LOG-GAMMA BETA)))
  )
)

(DEFUN WEIBULL-DENS (LAMDA BETA X)
   (* (^ LAMDA BETA)
      BETA
      (^ X (- BETA 1))
      (EXP (- (^ (* LAMDA X) BETA)))
   )
)


