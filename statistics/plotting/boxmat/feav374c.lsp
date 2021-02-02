
(defun boxmat-B (data &key (title "FEAV 375") df min max)
	 (boxmat data
		 :df df
		 :title title
		 :variable-labels '("Material" "Lab")
		 :min min
		 :max max
	 )
)

 (def b:Bekk	     (boxmat-B Bekk	      :title "b:Bekk"       :df 159                   ))
 (def b:mat.lab      (boxmat-B e:mat.lab      :title "b:mat.lab"    :df  19 :min -100 :max 150))
 (def b:residual     (boxmat-B e:residual     :title "b:residual"   :df 140 :min -100 :max 150))

 (def b:l-mat.lab    (boxmat-B e:l-mat.lab    :title "b:l-mat.lab"  :df  19))
 (def b:l-residual   (boxmat-B e:l-residual   :title "b:l-residual" :df 140))

 (def b:l-material   (boxmat-B e:l-material   :title "b:l-material" :df   4))
 (def b:l-lab	     (boxmat-B e:l-lab	      :title "b:l-lab"      :df   3))
 (def b:l-mat*lab    (boxmat-B e:l-mat*lab    :title "b:l-mat*lab"  :df  12))


(defun mat-std-dev(y)
   (map-elements #'(lambda (x) (cond ((> (length x) 1) (standard-deviation x))
				     ((= (length x) 1) 0)
				     (t  nil)
			       )
		   )
		 y
   )
)



(def x (log10 (combine (mat-mean Bekk))))
(def y (log10 (combine (mat-std-dev Bekk))))
(def p.sl (plot-points x y :title "spread vs level"))
(send p.sl :variable-label '(0 1) '("log cell mean" "log cell stddev"))
(send p.sl :x-axis  T t 3)
(send p.sl :y-axis  T t 4)
(send p.sl :margin -5 -35 -25 -5)
(def ab-etc (regress-ab x y))
(apply #'send p.sl :abline (select ab-etc 0))
;(select ab-etc 0)
;(-1.23268 1.1318)
(def power (- 1 (select (select ab-etc 0) 1)))
;-0.131797


(def xl (log10 (combine (mat-mean l-Bekk))))
(def yl (log10 (combine (mat-std-dev l-Bekk))))
(def p.lsl (plot-points xl yl :title "spread-l vs level-l"))
(send p.lsl :variable-label '(0 1) '("log cell mean-l" "log cell stddev-l"))
(send p.lsl :x-axis  T t 2)
(send p.lsl :y-axis  T t 2)
(send p.lsl :margin -5 -20 -25 -5)
(def abl-etc (regress-ab xl yl))
(apply #'send p.lsl :abline (select abl-etc 0))
;(select abl-etc 0)
;(-1.25906 0.445012)
(def powerl (- 1 (select (select abl-etc 0) 1)))
;0.554988

(def x5 (remove 5 (iseq 20)))
(def ablx-etc (regress-ab (select xl x5) (select yl x5)))
(select ablx-etc 0)
;(-1.13523 0.25258)
(- 1 (select ablx-etc 0))
;(2.13523 0.74742)
(apply #'send p.lsl :abline (select ablx-etc 0))

