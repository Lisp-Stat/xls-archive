(send *data-set-menu* :new-title "Freshman Cohort")

(def fc
 #2a(
   (   0  232  355  394  427  449  435)
   (1000  291	20   12    7	4    3)
   (   0  473  280   46   15   10    8)
   (   0    4  330  210   49   16   14)
   (   0    0	 8  315  166   30   19)
   (   0    0	 0    4   60   24    8)
   (   0    0	 6   19  276  466  513)
 )
)


(defun bp-surv (sp &key (title "probabilities")
			(class '("W" "F" "So" "Jr" "Sr" "HS" "G"))
			(semester '(0 1 2 3 4 5 6))
			(lines '())
	       )
  (let* ((bp
	  (boxmat (map-elements #'list 0 sp)
	   :title title
	   :print-ss nil
	   :margin '(0 -40 -35 -9)
	   :variable-labels '("semester" "class")
	   :row-names class
	   :x-axis-label semester
	   :max 1
	   :min 0
	  )
	 )
	 (range (send bp :range 1))
	)
	(dolist (x lines)
		(send bp :add-lines (list x x) range :type 'dashed)
	)
	bp
  )
)

(def fcsurv (bp-surv (/ fc 1000) :title "Freshman Cohort" ))

