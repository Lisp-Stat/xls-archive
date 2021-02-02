(send *data-set-menu* :new-title "Bekk Smoothness, FEAV 375")

(def Bekk-in (make-array '(4 8 5) :initial-contents
'(
((6.5 12.9 38.7 166.3 125.9)
 (6.5 12.5 47.7 151.8 113.5)
 (6.6 12.8 44.5 141.0 123.4)
 (8.1 11.0 45.5 149.4 174.0)
 (6.5  9.3 41.5 151.7 130.1)
 (6.5 13.7 46.0 166.1 158.2)
 (6.8 13.1 43.8 148.6 144.5)
 (6.5 14.0 58.0 158.3 180.0))

((6.0 13.2 42.9 178.5 196.7)
 (6.0 14.2 39.3 183.3 230.8)
 (6.1 12.2 39.7 150.8 146.0)
 (5.8 13.1 45.8 145.0 247.3)
 (5.7  9.9 43.3 162.9 183.7)
 (5.7  9.6 41.4 184.1 237.2)
 (5.7 12.6 41.8 160.5 229.3)
 (6.0  9.0 39.2 170.8 185.9))

((6.0 11.0 39.0 150.0 150.0)
 (5.0 10.0 44.0 150.0 165.0)
 (6.0 12.0 43.0 150.0 172.0)
 (6.0 13.0 43.0 160.0 162.0)
 (6.0 11.0 40.0 150.0 170.0)
 (6.0 10.0 35.0 160.0 150.0)
 (6.0 11.0 34.0 182.0 158.0)
 (6.0 12.0 45.0 140.0 198.0))

((5.0 11.4 35.4 121.8 123.8)
 (5.0 10.0 37.2 127.4 162.0)
 (4.9  8.8 34.8 145.0 128.4)
 (4.8  8.2 41.2 162.4 153.0)
 (4.6 10.0 42.6 122.2 164.4)
 (4.5  8.4 37.8 124.0 140.0)
 (4.8 10.0 34.8 110.2 130.2)
 (4.3 12.6 34.0 141.2 198.8))
)
))

(def Bekk (array-size-list (make-array '(4 5) :initial-element 8)
			   (permute-array Bekk-in '(0 2 1)) ))

(def material (col-indices Bekk))
(def lab (row-indices Bekk))
(def mat*lab (interaction-indices material lab))


(def e:Bekk-mean  (- Bekk (mean Bekk)))
(def e:material   (wilk-sweep e:Bekk-mean material))
(def e:w-material (- e:Bekk-mean e:material))
(def e:lab	  (wilk-sweep e:w-material lab))
(def e:w-mat*lab  (- e:w-material e:lab))
(def e:mat*lab	  (wilk-sweep e:w-mat*lab mat*lab))
(def e:residual   (- e:w-mat*lab e:mat*lab))

(def e:mat.lab	  (+ e:material e:lab e:mat*lab))
(def e:mat.lab/mean  (/ (* e:lab e:material) (mean Bekk))) ;comparison value


(defun log10 (x) (/ (log x) (log 10)))

(defun mat-mean (y)
   (map-elements #'(lambda (x) (if (> (length x) 0) (mean x) nil))
		 y
   )
)



(def   l-Bekk	    (log10 Bekk))
(def e:l-Bekk-mean  (- l-Bekk (mean l-Bekk)))
(def e:l-material   (wilk-sweep e:l-Bekk-mean material))
(def e:l-w-material (- e:l-Bekk-mean e:l-material))
(def e:l-lab	    (wilk-sweep e:l-w-material lab))
(def e:l-w-mat*lab  (- e:l-w-material e:l-lab))
(def e:l-mat*lab    (wilk-sweep e:l-w-mat*lab mat*lab))
(def e:l-residual   (- e:l-w-mat*lab e:l-mat*lab))

(def e:l-mat.lab    (+ e:l-material e:l-lab e:l-mat*lab))
(def e:l-mat.lab/mean  (/ (* e:l-lab e:l-material) (mean l-Bekk)))


