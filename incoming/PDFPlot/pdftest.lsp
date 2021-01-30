;;; TEST CODE

#-unix	(progn
				(require "parscolr")
				(when (not (boundp '*colortable*))
						(load-colortable)))


( require "pdfplot" )

( require "backgrid" ) ;; actually optional

( when (send scatterplot-proto :has-method :grid-color)
	(send scatterplot-proto :grid-color 'cyan))


(defun bplot (x y &rest args)
  (let ((bp (send basic-plot-proto :new 2)))
    (send bp :add-points x y)
    (apply #'send bp :add-lines x y args)
    (send bp :adjust-to-data)
  bp))

( defun test ( nn )
	( when ( or (not (boundp 'x)) (null x) (not (boundp 'y)) (null y))
		( setf x ( iseq nn ))
		( setf y ( + x ( normal-rand nn  ) 
					   ( * ( sqrt x ) ( uniform-rand nn ) (sin (/ x pi)) 2))))
   ( let (( b (bplot x y :color 'yellow :width 2 )))
   	( send b :redraw )
   	b ))


( setf tst (test 30))
( write-pdf tst "test5.pdf" )
