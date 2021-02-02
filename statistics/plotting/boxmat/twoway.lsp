(send *data-set-menu* :new-title "twoway")
(def tw (matrix (list 2 3)
		(list (list 1 2 3)
		      (list 4)
		      (list 5 6 7 8 9 10 11 12 25)
		      (list 13 14 15 16 17 18 19)
		      ()
		      (list 5 20 21 22 23 24 25 26 27))))

(def btw
    (boxmat tw :df 26
	       :x-axis-label '("a" "b" "c")
;	       :y-axis-label '("One" "Two")
	       :row-names '("low" "hi")
	       :variable-labels '("Letters" "Size")
	       :title "btw"
	       :margin '(5 -2 -35 -9)
    )
)
