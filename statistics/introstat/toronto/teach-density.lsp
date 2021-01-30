;;;;
;;;;  This file contains programs to run demonstrations of plots of density
;;;;  and probability functions.  Where there are enough similarities
;;;;  between plots, these  have been consolidated into and object or function
;;;;

;;;
;;;  This defines a new prototype for a probability or density function
;;;  Slots are defined to hold the xrange of the data, the y-values for
;;;  those x-values, a title, a function  and parameters to the function
;;;
(defproto density-proto '(xrange yvals title function parm1 parm2)
			() scatterplot-proto)

(defmeth density-proto :isnew (&key title function xrange parm1 parm2)
  (setf (slot-value 'title) title)
  (setf (slot-value 'xrange) xrange)
  (setf (slot-value 'function) function)
  (setf (slot-value 'parm1) parm1)
  (setf (slot-value 'parm2) parm2)
  (call-next-method 2 :show nil))
;;
;; These are 4 accessor methods, which are as as usual, except that a new
;; parameter argument sends the graph the replot message
;;
(defmeth density-proto :parm1 (&optional (parm1 nil set))
  (if set (setf (slot-value 'parm1) parm1))
  (if set (send self :replot))
  (slot-value 'parm1))

(defmeth density-proto :parm2 (&optional (parm2 nil set))
  (if set (setf (slot-value 'parm2) parm2))
  (if set (send self :replot))
  (slot-value 'parm2))

(defmeth density-proto :xrange (&optional (xrange nil set))
  (if set (setf (slot-value 'xrange) xrange))
  (if set (send self :replot))
  (slot-value 'xrange))

(defmeth density-proto :function (&optional (function nil set))
  (if set (setf (slot-value 'function) function))
  (if set (send self :replot))
  (slot-value 'function))
;;
;;  This method causes the graph to replot itself with the current
;;  parameter argument
;;  The uniform distribution and the discrete probability functions are
;;  different from the others: as a result, the replot method is slightly
;;  different for these	.
;;  The uniform-density  and pmf-profile functions are called explicitly	


(defmeth density-proto :replot ()
    (let* ((p1 (send self :parm1))
           (p2 (send self :parm2))
	   (x (send self :xrange))
  	   (function (send self :function))

; define xy as the co-ordinate list for lines to be drawn
; it is slightly different for the uniform and probability functions

	   (xy (case function
		(unif-dens (unif-dens p1 p2 x))
		(pois-pmf (discrete-pmf function p1 p2 (max x)))
		(bin-pmf  (discrete-pmf function p1 p2 (max x)))
		(t (list x (funcall function x p1 p2))))))

; The redraw method is slightly different for t-distribution,
; as it has a reference line which slows things down if it too is redrawn
; This procedure redraws only the t-distribution, not the reference.

	(if (eql function 'vec-t)
		(let ((x (select xy 0))
		      (y (select xy 1)))
; the first time through (only 200 pts), add the lines for the t-distribution
			(if ( > 201 (send self :num-lines))
      				(send self :add-lines xy))
; once the lines are in place, adjust their linestarts to the current x and y values
			(send self :linestart-coordinate 0 (iseq 200 399) x )
      			(send self :linestart-coordinate 1 (iseq 200 399) y )
      			(send self :redraw))

; For other distributions, simply clear and redraw the lines each time
		(let ((x (select xy 0))
		      (y (select xy 1)))
      			(send self :clear-lines)
      			(send self :add-lines xy)))))
;;;
;;;  This follows the code in Tierney for adding a mouse mode that shows
;;;  co-ordinates on a graph when the mouse is clicked there.
;;;
(send density-proto :add-mouse-mode 'show-coords
        :title "Show Co-ordinates"
        :click :do-show-coordinates
        :cursor 'finger)

(defmeth density-proto :do-show-coordinates (x y m1 m2)
  (let* ((xy (send self :canvas-to-real x y))
    (s (format nil "~s" xy))
    (mode (send self :draw-mode)))
  (send self :draw-mode 'xor)
  (send self :draw-string s x y)
  (send self :while-button-down #'(lambda (x y ) nil))
  (send self :draw-string s x y)
  (send self :draw-mode mode)))

;;;;
;;;;	This function takes all the parameters necessary for a
;;;;	density function and creates an object representing a graph
;;;; 	of the density as well as a dialog box controlling parameters
;;;;	affecting the graph
;;;;
;;;;  When this function is called, it puts a graph and dialog box on the screen.
;;;;
;;;;	title: "String"
;;;;	function 'function
;;;;	xrange (rseq)
;;;;	parmi-init number
;;;;	parmi-range (rseq)
;;;;	parmi-label "String"
;;;;
;;;;	By including all references locally within the function,
;;;;	the program allows multiple graphs to be shown and used at
;;;;	the same time.

(defun density-demo 	(&key title function xrange
			number-parms
			parm1-init parm1-range	
			parm2-init parm2-range
			parm1-string parm2-string
			dialog-title)
			
;;
;; This section sets up the initial plot and x-range,changes the size,
;; adjusts to the initial graph state and shows the window.
;;
(let* ( (w (send density-proto :new
                 :title title
		 :function (case function (1 'vec-gamma)
					  (2 'vec-chisq)
					  (3 'vec-beta)
					  (4 'vec-norm)
					  (5 'vec-exp)
					  (6 'vec-f)
					  (7 'unif-dens)
					  (8 'vec-t)
					  (9 'pois-pmf)
					  (10 'bin-pmf)
							)
                 :xrange xrange
                 :parm1 parm1-init
                 :parm2 parm2-init
                 :show nil)))
;;
;;  For the t-dist'b'n, these lines add a reference graph of the 
;; standard normal to the plot in a thicker line type.
;; The lines representing the reference have linestarts 0 to 199
;;

		(if (eql 'vec-t (send w :function))
			(let ((x (send w :xrange)))
			  (send w :add-lines x (normal-dens x))
			  (send w :linestart-width (iseq 200) (repeat 2 200))))

		(send w :replot)
		(send w :adjust-to-data)
		(send w :mouse-mode 'show-coords)
		;(send w :size 600 240)
		(send w :show-window)

		(let* ((sp1 (with-output-to-string (s) (prin1 parm1-init s)))
			(sp2 (with-output-to-string (s) (prin1 parm2-init s)))
			(parm1-label (send text-item-proto :new parm1-string))
			(parm2-label (if (= number-parms 2)
				(send text-item-proto :new parm2-string )))
			(parm1-value (send text-item-proto :new sp1 :text-length 10))
			(parm2-value (if (= number-parms 2)
				(send text-item-proto :new sp2 :text-length 10)))
			(parm1-scroll
			   (if (eql (send w :function) 'unif-dens)
; if this is a uniform then set value of parm1-scroll to this section
; Use sequence-slider as this allows values outside the initial range
				(send interval-scroll-item-proto :new
                        		(list (min parm1-range) (max parm1-range))
                        		:text-item parm1-value
					:points (length parm1-range)
                        		:action
; action section resets minimum value for other scroll bar, since a and b
; are related by 0 < a < b
                        		#'(lambda (x)
						(send parm2-scroll
						:min-value (round (+ (* 10 (- x 5.1 ) ) 1)))
                            			(send w :parm1  x)))
; else set parm1-scroll to the value of this section and
;  use sequence slider only
				(send sequence-scroll-item-proto :new
       		                 	parm1-range
               		         	:text-item parm1-value
                       		 	:action
                        		#'(lambda (x)
                            			(send w :parm1  x)))))
; create the second scroll bar only if there are 2 parameters
			(parm2-scroll (if (= number-parms 2)
; again, if this is the uniform, things are slightly different
			   (if (eql (send w :function) 'unif-dens)
; set parm2-scroll to the value of this section if it is a uniform
				(send interval-scroll-item-proto :new
                        		(list (min parm2-range) (max parm2-range))
                            		:text-item parm2-value
					:points (length parm2-range)
                            		:action
                            		#'(lambda (x)
						(send parm1-scroll
; reset the maximum value of the other scroll bar
						:max-value (round (- (* x 10 ) 1)))
                                		(send w :parm2  x)))
; else if not uniform, set parm2-scroll to the value of this section
				(send sequence-scroll-item-proto :new
                            		parm2-range
                            		:text-item parm2-value
                            		:action
                            		#'(lambda (x)
                                		(send w :parm2  x))) )))
			(close-all (send button-item-proto :new "Close Plot"
                      		:action
                      		#'(lambda ()
                          		(Send d :remove)
                          		(send w :close)
			  		(gc))))
			(rescale (Send button-item-proto :new "Rescale Plot"
                    		:action
                    		#'(lambda () (send w :adjust-to-data))))
;  The form of the dialog box depends on the number of parameters
			(d (if (= 2 number-parms)
				(send dialog-proto :new
					(list
					(list parm1-label parm1-value rescale)
					parm1-scroll
					(list parm2-label parm2-value close-all)
					parm2-scroll)
              				:title dialog-title)
            			(send dialog-proto :new
					(list
					(list parm1-label parm1-value rescale)
                                        (list parm1-scroll close-all))
              				:title dialog-title ))))
		;(send d :location 200 325)
					   )))
#|
TEMPLATE FOR USE OF DENSITY-DEMO FUNCTION
(density-demo		:title
			:number-parms
			:function
			:xrange
			:parm1-init
			:parm1-range
			:parm2-init
			:parm2-range
			:parm1-string
			:parm2-string
			:dialog-title)

|#
;;;;
;;;;		FUNCTION DEFINITIONS
;;;;
;;;;
;; This sets up vectorized density functions that take a vector of x-values
;; as their first argument and two further parameter arguments.  This allows 
;; each to be used in exactly the same way. 

;;;; GAMMA FUNCTION
;;;
;;;  This function defines a vectorized gamma with both scale and exponent
;;;  The built-in function has only the exponent
;;;

(defun vec-gamma (x exponent scale)
   (mapcar #'(lambda (z)
	(/ (* (^ scale exponent) (^ z (- exponent 1)) (exp (* -1 scale z)))
	(exp (log-gamma exponent)))) x))


;;;; CHI-SQUARED FUNCTION
;;;

(defun vec-chisq (x df dummy)
 (mapcar #'(lambda (z) (chisq-dens z df)) x))

;;;; BETA FUNCTION

(defun vec-beta (x alpha beta)
  (mapcar #'(lambda (z) (beta-dens z alpha beta))x))


;;;; NORMAL FUNCTION
;;
;; This defines a vectorized normal density function which takes as
;; arguments a vector of x-values, a mean and a variance,
;; and which return a vector of densities corresponding to the x-values
;; Oddly, this function appears not to be part of the language.

(defun vec-norm (x mean variance)
 (mapcar #'(lambda (z)
   (/ 1 (* (sqrt (* 2 pi variance) )
           (exp (/ ( ^ (- z mean) 2) (* 2 variance)))))) x))

;;;; EXPONENTIAL FUNCTION

(defun vec-exp (x mean dummy)
     (mapcar #'(lambda(z) (/ 1 (* (exp (/ z mean)) mean))) x))

;;;; F FUNCTION

(defun vec-f (x ndf ddf)
	(mapcar #'(lambda (z) (f-dens z ndf ddf)) x))

;;;; UNIFORM FUNCTION
;;
;;	The unif-dens function returns the x and y co-ordinates necessary
;;	to draw the rectangle representing the uniform density 
;;	on [a,b] over the range xrange
;;
(defun unif-dens (a b xrange)
	(let (	(lowx  (min xrange))
		(highx (max xrange)))
	(list 	(list lowx a a b b highx)
		(list 0 0 (/ 1 (- b a)) (/ 1 (- b a)) 0 0))))

;;;; STUDENT'S T FUNCTION

(defun vec-t (x df dummy)
	(mapcar #'(lambda(z) (t-dens z df)) x ))

;;;; POISSON AND BINOMIAL DISTRIBUTION FUNCTIONS
;;
;; These functions (yvals-top-***) calculate the y-values for the tops of the bars
;; of the probability function (f) and return 2 values for each x value
;; One takes 2 arguments and the other 3.

(defun yvals-top-bin (p n max-x)
  (map 'list #'(lambda (x) (repeat (binomial-pmf x n p) 2))
       (iseq (+ max-x 1))))

(defun yvals-top-pois (mean max-x)
	(map 'list #'(lambda (x) (repeat (poisson-pmf x mean) 2))
	(iseq (+ max-x 1))))
;;
;; yvals-all creates the correct pattern of y-values and zeroes
;; for the tops and bottoms of the bars.
;; The pattern is: (0,p(0),p(0),0,p(1),p(1),0,p(2),p(2),0,...p((max_x)).p((max_x)),0)
;;e.g
;;	p0------p0
;;	|	|
;;	|	|
;;	|	p1------p1
;;	|	|	p2------p2
;;	|	|	|	|
;;	0	0	0	0

(defun yvals-all (function p1 p2 max-x)
	(combine 0 (map 'list #'(lambda (x) (combine x 0))
		(if (eql function 'pois-pmf)
       			(yvals-top-pois  p1 max-x)
       			(yvals-top-bin   p1 p2 max-x)  ))))

;;
;; The function xvals-all creates the correct pattern of x-values for the
;; lines needed to draw the probability function.
;; The pattern is: (0,0,1,1,1,2,2,2,3,3,...,(max_x),(max_x)+1,(max_x)+1)
;;
(defun xvals-all (max-x)
    (combine (list 0 0
             (repeat (iseq 1 max-x) (repeat 3 max-x) )
             (+ max-x 1) (+ max-x 1))))

;;
;; This function returns the vectors representing the plot of the
;; discrete probability function.
;;
(defun discrete-pmf (function p1 p2 max-x)
           (list (xvals-all max-x) (yvals-all function p1 p2 max-x)))

;;;
;;;	This section defines menu items and their actions
;;;	Each menu item has an action which calls the "density-demo"
;;;	function with parameters appropriate for a reasonable
;;;	demonstration of the density or probability function
;;;
(setf normal-density-item
	(send menu-item-proto :new "Normal"
		:action
		#'(lambda()
			(density-demo		:title 		"Normal Distribution"
			:number-parms	2
			:function 	4
			:xrange 	(rseq -10 15 200)
			:parm1-init 	0
			:parm1-range	(rseq 0 5 26)
			:parm2-init 	0.5
			:parm2-range 	(rseq 0.5 10 20)
			:parm1-string 	"mean    "
			:parm2-string 	"variance"
			:dialog-title "Parameters for Normal"))))

(setf gamma-density-item
	(send menu-item-proto :new "Gamma"
		:action
		#'(lambda()
			(density-demo		:title 	"Gamma Distribution"
			:function 	1
			:number-parms	2
			:xrange 	(rseq 0.01 80 200)
			:parm1-init 	0.5	
			:parm1-range	(rseq 0.5 20 40)
			:parm2-init 	0.5
			:parm2-range 	(rseq 0.5 2.5 9)
			:parm1-string 	"exponent"
			:parm2-string 	"scale   "
			:dialog-title	"Parameters for Gamma"))))
(setf chi-squared-density-item
	(send menu-item-proto :new "Chi-squared"
		:action
		#'(lambda()
			(density-demo          	:title "Chi-squared Distribution"
			:number-parms	1
			:function	2
                 	:xrange 	(rseq 0.01 50 200)
			:parm1-init 	1
			:parm1-range 	(iseq 1 25)
			:parm2-init	nil
			:parm2-range	nil
			:parm1-string "Degrees of Freedom"
			:parm2-string	nil
			:dialog-title "Parameter for Chi-squared"))))
(setf beta-density-item
	(send menu-item-proto :new "Beta"
		:action
		#'(lambda()
			(density-demo		:title "Beta Distribution"
			:number-parms	2
		 	:function 	3
                 	:xrange 	(rseq 0.001 0.999 200)
                 	:parm1-init	0.5
                 	:parm1-range	(append (/ (iseq 1 20) 10) (iseq 2 20))
                 	:parm2-init	0.5
                 	:parm2-range	(append (/ (iseq 1 20) 10) (iseq 2 20))
			:parm1-string 	"alpha"
			:parm2-string 	"beta"
			:dialog-title 	"Parameters for Beta"))))
(setf Exponential-density-item
	(send menu-item-proto :new "Exponential"
		:action
		#'(lambda()
			(density-demo		:title 		"Exponential Distribution"
			:number-parms	1
			:function 	5
			:xrange		(rseq 0.01 40 200)
			:parm1-init 	1
			:parm1-range	(rseq 0.5 10 20)
			:parm2-init 	nil
			:parm2-range 	nil
			:parm1-string 	"mean"
			:parm2-string 	nil
			:dialog-title	"Parameter for Exponential"))))

(setf f-density-item
	(send menu-item-proto :new "F"
		:action
		#'(lambda()
			(density-demo		:title 		"F Distribution"
			:number-parms	2
			:function 	6
			:xrange 	(rseq 0.001 5 200)
			:parm1-init 	1
			:parm1-range	(iseq 1 10)
			:parm2-init 	10
			:parm2-range 	(* (iseq 2 12) 5)
			:parm1-string 	"numerator d.f.  "
			:parm2-string 	"denominator d.f."
			:dialog-title	"Parameters for F"))))

(setf unif-density-item
	(send menu-item-proto :new "Uniform"
		:action
		#'(lambda()
			(density-demo		:title 		"Uniform Distribution"
			:number-parms	2
			:function 	7
			:xrange 	(iseq 0 10)
			:parm1-init 	0
			:parm1-range	(rseq 0 4.9 50)
			:parm2-init	5
			:parm2-range 	(rseq 5.1 10 50)
			:parm1-string 	"a"
			:parm2-string 	"b"
			:dialog-title	"Parameters for Uniform [0<a<b<10]"))))


(setf t-density-item
	(send menu-item-proto :new "Student's t"
		:action
		#'(lambda()
			(density-demo		:title 		"Student's t Distribution (with Normal reference)"
			:number-parms	1
			:function 	8
			:xrange 	(rseq -5 5 200)
			:parm1-init 	1
			:parm1-range	(combine (list (iseq 1 20) (list 25 30 35 40 45 50 55 60)))
			:parm2-init	nil
			:parm2-range 	nil
			:parm1-string 	"Degrees of freedom"
			:parm2-string 	nil
			:dialog-title	"Parameters for Student's t"))))

(setf poisson-density-item
	(send menu-item-proto :new "Poisson"
		:action
		#'(lambda()
			(density-demo		:title 		"Poisson Distribution"
			:number-parms	1
			:function 	9
			:xrange 	(iseq 0 35)
			:parm1-init 	1
			:parm1-range	(/ (iseq 0 200) 10)
			:parm2-init	nil
			:parm2-range 	nil
			:parm1-string 	"mean       "
			:parm2-string 	nil
			:dialog-title	"Parameters for Poisson"))))
(setf binomial-density-item
	(send menu-item-proto :new "Binomial"
		:action
		#'(lambda()
			(density-demo		:title 		"Binomial Distribution"
			:number-parms	2
			:function 	10
			:xrange 	(iseq 0 35)
			:parm1-init 	0.00
			:parm1-range	(rseq 0.00 1.00 101)
			:parm2-init	1
			:parm2-range 	(iseq 1 35)
			:parm1-string 	"p"
			:parm2-string 	"n"
			:dialog-title	"Parameters for Binomial"))))

(setf demo-menu (send menu-proto :new "&Densities"))
(setf close-item
	(send menu-item-proto :new "Close Menu"
		:action
		#'(lambda() (send demo-menu :dispose) (gc))))
(send demo-menu :append-items
		normal-density-item
		t-density-item
		chi-squared-density-item
		f-density-item
		exponential-density-item
		beta-density-item
		gamma-density-item
		unif-density-item	
		(send dash-item-proto :new)
		poisson-density-item
		binomial-density-item
		(send dash-item-proto :new)
		close-item)
(send demo-menu :install)
 
