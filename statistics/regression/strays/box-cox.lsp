;; Dynamic Box-Cox-transformation in regression
;;
;; by
;;
;;    Juha Puranen
;;    Department of statistics
;;    University of Helsinki
;;    Aleksanterinkatu 7
;;    00100 Helsinki
;;
;;    jpuranen@noppa.helsinki.fi
::
;; Data taken from
;;
;;    Ezekiel M. & Fox K. A.
;;    Methods of Correlation and Regressio analysis
;;    John Wiley & Sons, New York, 1965.
;;
;;    Average Distance Required for Stopping after Signal,
;;    for Different Speeds.
;;
;;
;;       x = speed             (Miles per hour)
;;       y = distance to stop  (Feet)
;;

  (def x (list 4 5 5 5 5 7 7 8 8 8 8 9 9 9 10 10 10 12 12 12 13 13 13 14 14
  15 16 16 16 17 17 18 18 18 19 20 21 21 21 24 25 25 25 25 26 26 27 27
  28 28 29 29 30 30 30 31 35 35 36 39 40 40))

  (def y (list 4 2 4 8 8 7 7 8 9 11 13 5 5 13 8 14 17 11 19 21 15 18 27 14 16
  16 14 19 34 22 29 29 34 47 30 48 39 42 55 56 33 48 56 59 39 41 57 78
  64 84 54 68 60 67 101 77 85 107 79 138 110 134))

;; define a function to compute the Box-Cox transformation

(defun bc (x c p)
  (let* ((x (- x c))
	 (bcx (if (< (abs p) .0001)
                  (log x)
		  (/ (^ x p) p)))
         (min (min bcx))
         (max (max bcx)))
    (/ (- bcx min) (- max min))))

;; compute the normal quantiles of the expected uniform order statistics

(def z (normal-quant (/ (iseq 1 62) 63)))

;; Box-Cox-transformation for variable y

(def zy (bc y 0 1))

;;  scatterplot

(def myplot1 (plot-points x zy))

(send myplot1 :title "Scatterplot")
(send myplot1 :location 30 50 )    ; for SVGA  1024x768
(send myplot1  :size 350 450)      ; for SVGA
;(send myplot1 :location 30 50 )   ; for SUN
;(send myplot1 :size 400 650)      ; for SUN
;;  regression model
(def suora (regression-model x zy :print nil))
(def res (send suora :residuals))
(def coefs (send suora :coef-estimates))
(send myplot1 :clear-lines :draw nil)
(send myplot1 :abline (select coefs 0) (select coefs 1))

;; sorted residuals

;; construct an initial plot without transformation

(def myplot2 (plot-points x res))
(send myplot2 :title "Residual Plot")
(send myplot2 :range 1 -0.35 0.45)
(send myplot2 :location 390 380)     ; for SVGA
(send myplot2 :size 580 270)         ; for SVGA
;(send myplot2 :location  500 550 )  ; for SUN
;(send myplot2 :size 650 350)        ; for SUN

(def myplot3 (histogram res))
(send myplot3 :title "Histogramm")
(send myplot3 :location 390 50)      ; for SVGA
(send myplot3 :size 290 200)         ; for SVGA
;(send myplot3 :location 440 50 )    ; for SUN
;(send myplot3 :size 300 300)        ; for SUN

(send myplot3 :range 0 -0.35 0.45)
;; (send myplot3 :x-axis t t 4)
(def sres (sort res `<))
(def myplot4 (plot-points z sres))
(send myplot4 :title "Propability Plot")
(send myplot4 :location 680 50)     ; for SVGA
(send myplot4 :size 300 300)        ; for SVGA
;(send myplot4 :location 750 50 )     ; for SUN
;(send myplot4 :size 380 380)         ; for SUN

(send myplot4 :range 1 -.35 .45)

;;
;; construct a dialog for scrolling through powers and recomputing the
;; plot
;;

(setf liukuri
(interval-slider-dialog (list -1 2)
                        :title "Box-Cox-transformation"
                        :text "Power"
                        :points 60
                        :action #'(lambda (p)

                         (def zy (bc y 0 p ))
			 (def suora (regression-model x zy :print nil))

			 (def coefs (send suora :coef-estimates))
                         (def res (send suora  :residuals))

                         (send myplot1 :clear nil)
                         (send myplot1 :add-points x zy)
                         (send myplot1 :clear-lines :draw nil)
                         (send myplot1 :abline (select coefs 0) (select coefs 1))

                         (send myplot2 :clear nil)
			 (send myplot2 :add-points x res)
                         (send myplot2 :add-lines (kernel-smooth x res :type `g :width 10))

                         (send myplot3 :clear nil)
                         (send myplot3 :add-points res)

                         (def sres (sort res `<))
                         (send myplot4 :clear nil)
			 (send myplot4 :add-points z sres)
)))
(send liukuri :value 1)
; (send liukuri :location 470 420)  ;for SUN
(send liukuri :location 400 350) ; for SVGA
