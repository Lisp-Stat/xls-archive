
;;; file denspol.lsp Frederic Udina 1999
;;; per l'assignatura de probabilitats a ciencia politica
;;; a partir del densuoc.lsp fet per la UOC, material d'Estadistica I dipl. Informatica
;;; fet a partir de densdemo.lsp de Jan DeLeeuw

;;; type (start-density) to start
;;;   or (start-teach) for a restricted version

#|

cal carregar el densuoc, millor compilat
previament amb (compile-file "densuoc")


cal afegir 'start-density a la llista *startup-functions*

fer (save-workspace "xlisp.wks")

i barco. 

|#


;;(setq *TRACENABLE* nil)
;;(setq *startup-functions* (append *startup-functions* '(dens-demo::start-teach)))

;;(setq *BREAKENABLE* t);;;<<<<<<< canviar-ho
(setq *gc-flag* nil)

(unless (find-package "DENS-DEMO")
	(make-package "DENS-DEMO" :use '(xlisp)))

(use-package "DENS-DEMO")

(in-package "DENS-DEMO")
(export '(start-density start-teach))
(import '(user::rseq user::combine ))

;;;;
;;;;  This file contains programs to run demonstrations of plots of density
;;;;  and probability functions.  Where there are enough similarities
;;;;  between plots, these  have been consolidated into and object or function
;;;;

(defvar *current-model* nil "a hack to access help strings")
(defvar *current-name*  nil "a hack to de removed")


;;; macros to build methods to access slots

(defmacro normal-assessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(defmacro replot-assessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set 
     (setf (slot-value ',slot) content)
     (setf (slot-value 'first-shot) nil)
     (send self :replot))
   (slot-value ',slot)))

;;; prototypes definitions
;;;

(defproto density-proto '(dens-fun dist-fun quant-fun xrange yvals title
                          cont parm-names parm-values parm-ranges 
			  xcompact region mean-stdev distr-object 
			  show-normal show-mnstdv first-shot)
			() scatterplot-proto )

(defproto distribution-proto '(dens-object) () density-proto)

(defproto new-slider-proto '(old-x old-value) () sequence-scroll-item-proto)

;;; new-slider proto methods

(defmeth new-slider-proto :old-x (&optional (val nil set))
 (if set (setf (slot-value 'old-x) val)
 (slot-value 'old-x)))

(defmeth new-slider-proto :old-value (&optional (val nil set))
 (if set (setf (slot-value 'old-value) val)
 (slot-value 'old-value)))


;;; density and distribution proto methods

(defmeth density-proto :isnew (&key title dens-fun dist-fun quant-fun cont
                                    parm-names parm-values parm-ranges
                                    xcompact location)
  (setf (slot-value 'title) title)
  (setf (slot-value 'dens-fun) dens-fun)
  (setf (slot-value 'dist-fun) dist-fun)
  (setf (slot-value 'quant-fun) quant-fun)
  (setf (slot-value 'cont) cont)
  (setf (slot-value 'parm-names) parm-names)
  (setf (slot-value 'parm-values) parm-values)
  (setf (slot-value 'parm-ranges) parm-ranges)
  (setf (slot-value 'xcompact) xcompact)
  (setf (slot-value 'location) location)
  (call-next-method 2 :show nil))

(defmeth density-proto :check-range ()
 (let* (
        (parm-values (send self :parm-values))
        (quant (send self :quant-fun))
        (dens-fun (send self :dens-fun))
        (p .0001)
;;quick hack by udina to skip the sum-of-uniforms bug
        (top (if quant
		 (apply #'funcall quant (- 1 p) parm-values)
	       (car parm-values)))
        (bottom (if quant
		    (apply #'funcall quant p parm-values)
		  0))
       )
   (list bottom top)))

              
(defmeth density-proto :xrange (&optional (val nil set))
  (if set (setf (slot-value 'xrange) val))
  (let* (
	 (range (send self :check-range))
	 (bottom (first range)) 
	 (top (second range))
	 (xcompact (combine (mapcar #'eval (send self :xcompact))))
	 (xbot (first xcompact))
	 (xtop (second xcompact))
	 (cont (send self :cont))
	 (xlist (list (if xbot xbot bottom) (if xtop xtop top)))
	 )
    (if cont (setf (slot-value 'xrange) 
		   (apply #'rseq (append (+ xlist (list -1 1)) (list 200))))
      (setf (slot-value 'xrange) (apply #'iseq (+ (list -1 1) xlist))))
    (unless set (slot-value 'xrange))
    )
  )

(defmeth density-proto :region (&optional (val nil set))
  (when set
    (slot-value 'region val))
  (slot-value 'region))

(defmeth distribution-proto :region (&optional (val nil set))
  (when set (send (slot-value 'dens-object) :region val))
  (send (slot-value 'dens-object) :region))

(normal-assessor :parm-names parm-names density-proto)
(replot-assessor :parm-values parm-values density-proto)
(replot-assessor :parm-ranges parm-ranges density-proto)
(normal-assessor :dens-fun dens-fun density-proto)
(normal-assessor :dist-fun dist-fun density-proto)
(normal-assessor :quant-fun quant-fun density-proto)
(normal-assessor :cont cont density-proto)
(normal-assessor :xcompact xcompact density-proto)

;;
;;  This method causes the graph to replot itself with the current
;;  parameter argument
;;  The uniform distribution and the discrete probability functions are
;;  different from the others: as a result, the replot method is slightly
;;  different for these	.
;;  The uniform-density  and pmf-profile functions are called explicitly	

(defmeth distribution-proto :replot ()
  (let* (
	 (parm-names (send self :parm-names))
	 (parm-values (send self :parm-values))
	 (parm-ranges (send self :parm-ranges))
	 (dist-fun (send self :dist-fun))
	 (x (send self :xrange))
	 (xy (case dist-fun
		   (binomial-cdf (discrete-cdf dist-fun parm-values x))
		   (geometric-cdf (discrete-cdf dist-fun parm-values x))
		   (poisson-cdf (discrete-cdf dist-fun parm-values x))
		   (t (list x (apply #'funcall dist-fun x parm-values)))))
	 )
    (if (eql dist-fun 't-cdf)
	(let ((x (select xy 0))
	      (y (select xy 1)))
	  (if ( > 201 (send self :num-lines))
	      (send self :add-lines xy :draw nil))
	  (send self :linestart-coordinate 0 (iseq 200 399) x )
	  (send self :linestart-coordinate 1 (iseq 200 399) y )
	  (when ( < 400 (send self :num-lines))
	    (send self :linestart-coordinate 0 (iseq 400 401) '(0 0) )
	    (send self :linestart-coordinate 1 (iseq 400 401) '(0 0) ))
	  ;;(send self :redraw)
	  )
      (progn
	(send self :clear-lines :draw nil)
	(send self :add-lines xy :draw nil)))
    (send self :redraw-content)
    )
  )

(defmeth density-proto :draw-help ()
  "temp draw a help string"
  (map-elements #'send self :draw-string 
		(list
		 (if (slot-value 'cont)
		     "Pots fer clic per calcular densitats"
		   "Pots fer clic per calcular probabilitats")
		 (if (slot-value 'cont)
		     "o clic-i-arrossega per calcular probabilitats"
		   ""))
		100 '(100 120)))

  "temp draw a help string"
(defmeth distribution-proto :draw-help ()
  (map-elements #'send self :draw-string 
		'("Pots fer clic per sota la diagonal per calcular"
		  "probabilitats acumulades"
		  "o per sobre la diagonal per calcular quantils")
		100 '(100 120 140))
  (send self :add-lines (list (send self :scaled-range 0)
					(send self :scaled-range 1))
	:draw t :type 'dashed)
)


(defmeth density-proto :replot ()
"params can have changed, update things and redraw window"
    (let* (
           (parm-names (send self :parm-names))
           (parm-values (send self :parm-values))
	   (parm1 (first parm-values))
	   (parm2 (second parm-values))
           (parm-ranges (send self :parm-ranges))
           (dens-fun (send self :dens-fun))
           (x (send self :xrange))
; define xy as the co-ordinate list for lines to be drawn
; it is slightly different for the uniform and probability functions
	   (xy (case dens-fun
		(uniform-dens (apply #'uniform-dens x parm-values))
		(poisson-pmf (discrete-pmf dens-fun parm-values x))
		(geometric-pmf (discrete-pmf dens-fun parm-values x))
		(binomial-pmf (discrete-pmf dens-fun parm-values x))
                (t (list x (apply #'funcall dens-fun x parm-values))))))
	;;for some cases, store the mean and stdev in mean-stdev slot
	(slot-value 'mean-stdev 
		    (case dens-fun
			  (gaussian-dens parm-values)
			  (poisson-pmf (list parm1
					     (sqrt parm1)))
			  (geometric-pmf (list (/ parm1) (/ (sqrt (- 1 parm1)) parm1)))
			  (binomial-pmf (list (* parm1 parm2) 
					      (sqrt (* parm1 parm2 (- 1 parm2)))))
			  (exponential-dens (repeat (/ parm1) 2))
			  (sum-of-uniforms-dens
			   (list (* 0.5 parm1) (sqrt (/ parm1 12))))
			  (pareto-dens
			   (if (> parm1 2)
			       (list (* parm1 parm2 (/ (1- parm1)))
				     (sqrt (/ (* parm1 parm2 parm2) 
					      (* (- parm1 2) (1- parm1) (1- parm1)))))
			     nil))
			  (t nil)))
	;; The redraw method is slightly different for t-distribution,
	;; as it has a reference line which slows things down if it too is redrawn
	;; This procedure redraws only the t-distribution, not the reference.
	(if (eql dens-fun 't-dens)
	    (let ((x (select xy 0))
		  (y (select xy 1)))
	      ;; the first time through (only 200 pts), 
	      ;;add the lines for the t-distribution
	      (if ( > 201 (send self :num-lines))
		  (send self :add-lines xy :draw nil))
	      ;; once the lines are in place, 
	      ;;adjust their linestarts to the current x and y values
	      (send self :linestart-coordinate 0 (iseq 200 399) x )
	      (send self :linestart-coordinate 1 (iseq 200 399) y )
	      (send self :redraw))
	  ;; For other distributions, simply clear and redraw the lines each time
	  (let ((x (select xy 0))
		(y (select xy 1)))
	    (send self :clear-lines :draw nil)
	    (when (and (slot-value 'show-normal)
		       (slot-value 'mean-stdev))
		  (send self :draw-normal-dens))
	    (send self :add-lines xy :draw nil)))
	;;now call redraw-content, it's modified to paint region in normal density win
	;;and mean/stdev segment in some cases
	(send self :redraw-content)
))

(defmeth density-proto :draw-mean-stdev ()
"if available, draw a line for 1 and 2 stdev centered in the mean
 on top of the graph"
  (when (slot-value 'mean-stdev)
	;(print (slot-value 'mean-stdev))
	(let* ((mn (first (slot-value 'mean-stdev)))
	       (stdev (second (slot-value 'mean-stdev)))
	       (hg (* 0.98 (second (send self :range 1))))
	       (col (send self :draw-color))
	       (cr (send self :clip-rect))
	       (1stdev (+ mn stdev))
	       (2stdev (+ mn stdev stdev))
	       (-1stdev (- mn stdev))
	       (-2stdev (- mn stdev stdev)))
	  (setf mn (send self :real-to-canvas mn hg))
	  (setf stdev (send self :real-to-canvas stdev hg))
	  (setf 1stdev (send self :real-to-canvas 1stdev hg))
	  (setf 2stdev (send self :real-to-canvas 2stdev hg))
	  (setf -1stdev (send self :real-to-canvas -1stdev hg))
	  (setf -2stdev (send self :real-to-canvas -2stdev hg))
	  (setf hg (second mn))
	  (send self :draw-color 'blue)
	  (apply #'send self :clip-rect (send self :content-rect))
	  (send self :draw-line (first -2stdev) hg (first 2stdev)  hg)
	  (apply #'send self :draw-symbol 'disk t mn)
	  (apply #'send self :draw-symbol 'x nil -1stdev)
	  (apply #'send self :draw-symbol 'x nil 1stdev)
	  (apply #'send self :draw-symbol 'cross nil -2stdev)
	  (apply #'send self :draw-symbol 'cross nil 2stdev)
	  ;(print mn)
	  (send self :draw-color col)
	  (apply #'send self :clip-rect cr))))



;;defmeth distribution-proto :draw-mean-stdev ()
;;"don't do anything"
;;nil  

(defmeth density-proto :redraw-content ()
  "modif for dens functions to paint the prob region"
  (call-next-method)
  (when (and 
	 (send self :cont)
	     (send self :region))
	(send self :pinta-regio)
	)
  (when (send self :slot-value 'show-mnstdv)
	(send self :draw-mean-stdev))
  (when (slot-value 'first-shot)
    (Send self :draw-help))
  )

(defmeth density-proto :pinta-regio ()
"nomes per continues"
  (let* ((rg (send self :region))
	 (lt (max (first rg)) (first (send self :range 0)))
	 (rt (min (second rg) (second (send self :range 0))))
	 (mn (first (send self :parm-values)))
	 (std (second (send self :parm-values)))
	 (cdf (slot-value 'distr-object))
	 (s (if cdf (format nil "Prob. ~5,3f < X < ~5,3f: ~1,4f" lt rt
			    (- (first (second (send cdf :function-value rt)))
			       (first (second (send cdf :function-value lt)))))
	      nil))
	 (xx (rseq lt rt 100))
	 (yy (second (send self :function-value xx)))
	 (col (send self :draw-color))
	 (rect (send self :clip-rect)))
    (setf yy (combine (repeat 0 (length xx)) (reverse yy) 0)) 
    (setf xx (combine xx                     (reverse xx) (first xx)))
    (send self :draw-color 'red)
    (apply #'send self :clip-rect (send self :content-rect))
    (send self :paint-poly
	  (mapcar #'(lambda (xy)
		      (apply #'send self :real-to-canvas xy))
		  (transpose (list xx yy)))
	  t);from-origin
    (apply #'send self :clip-rect rect)
    (when s (send self :draw-string s 20 20))
    (send self :draw-color col))
  
)

(defmeth density-proto :draw-many-lines (xy)
"xy is a list of points
 draws lines between each consecutive points"
(when (> (length xy) 1)
      (apply #'send self :draw-line (combine (first xy) (second xy)))
      (send self :draw-many-lines (cdr xy))))


(defmeth distribution-proto :pinta-regio ()
  (let* ((rg (send self :region))
	 (xmin (first (send self :range 0)))
	 (ymin (first (send self :range 1)))
	 (xw (+ xmin
		(* -0.05 (apply #'- (send self :range 0)))))
	 (lt (max (first rg)) xmin)
	 (rt (min (second rg) (second (send self :range 0))))
	 (cdf (slot-value 'distr-object))
	 (s (format nil "Prob. ~5,3f < X < ~5,3f = ~1,4f - ~1,4f" lt rt
			    (first (second (send self :function-value rt)))
			    (first (second (send self :function-value lt)))))
	 (xx (list lt rt))
	 (yy (second (send self :function-value xx)))
	 (col (send self :draw-color))
	 (rect (send self :clip-rect))
	 xl yl)
    ;;(break)
    (setf xl (list lt lt 
		   xmin xmin 
		   rt rt))
    (setf yl (list ymin (first yy) 
		   (first yy) (second yy) 
		   (second yy) ymin))
    (send self :draw-color 'red)
    (apply #'send self :clip-rect (send self :content-rect))
    (send self :draw-many-lines
	  (mapcar #'(lambda (xy)
		      (apply #'send self :real-to-canvas xy))
		  (transpose (list xl yl))))
    (setf xl (list xmin xmin xw xw xmin))
    (setf yl (combine yy (reverse yy) (first yy)))
    (send self :paint-poly
	  (mapcar #'(lambda (xy)
		      (apply #'send self :real-to-canvas xy))
		  (transpose (list xl yl)))
	  t);from-origin
    (apply #'send self :clip-rect rect)
    (when s (send self :draw-string s 20 20))
    (send self :draw-color col))
  )

;;  This follows the code in Tierney for adding a mouse mode that shows
;;  co-ordinates on a graph when the mouse is clicked there.

(send density-proto :add-mouse-mode 'show-coords
        :title "Show Co-ordinates"
        :click :do-show-coordinates
        :cursor 'finger)

(defmeth density-proto :do-show-coordinates (x y m1 m2)
  (let* ((xy (send self :canvas-to-real x y))
	 ;;    (s (format nil "(~5,3f, ~5,3f)" (first xy) (second xy)))
	 (xx (first xy))
	 (fv (send self :function-value xx))
	 (toline (send self :real-to-canvas (first (first fv))
		       (first (second fv))))
	 (baseline (send self :real-to-canvas
			 (min (send self :range 0))
			 (min (send self :range 1))))
	 (line1 (list (first toline) (second baseline) (first toline) (second toline)))
	 (line2 (list (first toline) (second toline) (first baseline) (second toline)))
	 (s (format nil "x=~5,3f, f(x)=~5,3f" (first (first fv))
		    (first (second fv))))
	 (mode (send self :draw-mode))
	 (oldx x)
	 (oldy y)
	 (firstmov t)
	 rs oldrs)
    (when (send self :region)
	  (send self :region nil)
	  (send self :redraw)
	  (when (slot-value 'distr-object)
		(send (slot-value 'distr-object) :redraw-content))
	  
	  )
    (send self :draw-mode 'xor)
    (send self :draw-string s 20 20)
    (apply #'send self :draw-line line1)
    (apply #'send self :draw-line line2)
    (if (not (Send self :cont))
	(progn
	  (send self :while-button-down #'(lambda (nx ny)
					    nil))
	  (send self :draw-string s 20 20)
	  (apply #'send self :draw-line line1)
	  (apply #'send self :draw-line line2))
      (progn; it's a cont distribution, get region
	(Setf oldrs s)
	(send self :while-button-down 
	      #'(lambda (nx ny )
		  (cond ((and firstmov (/= x nx) (/= y ny));first mouse movement
			 (setf firstmov nil)
			 (send self :draw-string oldrs 20 20)
			 (apply #'send self :draw-line line1)
			 (apply #'send self :draw-line line2)
			 (setf rs (format nil "des de ~5,3f fins a ~5,3f"
					  (first (send self :canvas-to-scaled x x))
					  (first (send self :canvas-to-scaled nx nx))))
			 (send self :draw-string rs 20 20)
			 (send self :draw-line x y nx y)
			 (setf oldx nx)
			 (setf oldy ny)
			 (setf oldrs rs))
			(t
			 (send self :draw-line x y oldx y)
			 (send self :draw-string oldrs 20 20)
			 (setf rs (format nil "des de ~5,3f fins a ~5,3f"
					  (first (send self :canvas-to-scaled x x))
					  (first (send self :canvas-to-scaled nx nx))))
			 (send self :draw-string rs 20 20)
			 (send self :draw-line x y nx y)			 

			 (setf oldx nx)
			 (setf oldy ny)
			 (Setf oldrs rs)))))
	(cond ((= x oldx)
	       (send self :region nil))
	      (t
	       (setf x (first (send self :canvas-to-scaled x x)))
	       (setf oldx (first (send self :canvas-to-scaled oldx oldx)))
	       (send self :region (sort-data (list x oldx)))))
	(send self :draw-mode mode)
	(send self :redraw)
	(when (slot-value 'distr-object)
	      (send (slot-value 'distr-object) :redraw))
))
    (send self :draw-mode mode)))

;;only a slight difference...
;;;override the previous one
(defmeth distribution-proto :do-show-coordinates (x y m1 m2)
  (let* ((xy (send self :canvas-to-real x y))
;;	 (s (format t "(~5,3f, ~5,3f)" (first xy) (second xy)))
;;	 (xx (first xy))
;;	 (fv (send self :function-value xx))
;;aixo es per dibuixar el quantile si el ratoli esta per sobre
;;la diagonal, i la prob acumulada is no
	 (xrg (send self :range 0))
	 (yrg (send self :range 1))
	 (underdiag (or (null (send self :quant-fun))
			(> (/ (- (first xy) (first xrg)) (apply #'- (reverse xrg)))
			   (/ (- (second xy) (first yrg)) (apply #'- (reverse yrg))))))
	 (xx (if underdiag
		 (first xy)
	       (first (second (send self :quant-value (second xy))))))
	 ;;problems here, :quant-value returns nil if prob is out of bounds
	 (fv (if underdiag
		 (send self :function-value xx)
	       (list (list xx) (list (second xy)))))
	 (toline (send self :real-to-canvas xx
		       (first (second fv))))
	 (baseline (send self :real-to-canvas
			 (min (send self :range 0))
			 (min (send self :range 1))))
	 (s (format nil "x=~5,3f, F(x)=~5,3f" xx
		    (first (second fv))))
	 (mode (send self :draw-mode))
	 (dens (slot-value 'dens-object)))
    (when (send dens :region)
	  (send dens :region nil)
	  (send dens :redraw)
	  (send self :redraw)
	  )
    (send self :draw-mode 'xor)
    ;;(send self :draw-color 'red) don't affect draw-line?
    (send self :draw-string s 20 20)
    (send self :draw-line (first toline)
	  (second baseline)
	  (first toline)
	  (second toline))
    (send self :draw-line (first toline)
	  (second toline)
	  (first baseline)
	  (second toline))
    (send self :while-button-down #'(lambda (x y ) nil))
    (send self :draw-string s 20 20)
    (send self :draw-line (first toline)
	  (second baseline)
	  (first toline)
	  (second toline))
    (send self :draw-line (first toline)
	  (second toline)
	  (first baseline)
	  (second toline))
    (send self :draw-mode mode)))


  
;; some code for the previous methods
;; is for computing f(x) or F(x) given x

(defmeth density-proto :function-value (x)
"returns a list (x f(x) ) where x will be a list of values"
    (let* ((x (if (listp x) x (list x)))
           (parm-names (send self :parm-names))
           (parm-values (send self :parm-values))
           (parm-ranges (send self :parm-ranges))
           (dens-fun (send self :dens-fun))
           (rx (case dens-fun
           	(binomial-pmf (round x))
           	(geometric-pmf (round x))
           	(poisson-pmf (round x))
           	(t x)))
; define xy as the co-ordinate list for lines to be drawn
; it is slightly different for the uniform and probability functions
	   (xy (case dens-fun
		(uniform-dens (apply #'uniform-dens x parm-values))
                (t (list rx (apply #'funcall dens-fun rx parm-values))))))
          xy))


(defmeth distribution-proto :function-value (x)
    (let* ((x (if (listp x) x (list x)))
           (parm-names (send self :parm-names))
           (parm-values (send self :parm-values))
           (parm-ranges (send self :parm-ranges))
           (dist-fun (send self :dist-fun))
           (tx (case dist-fun
                 (binomial-cdf (truncate x))
                 (geometric-cdf (truncate x))
                 (poisson-cdf (truncate x))
                 (t x)))
           (xy
                 (list tx (apply #'funcall dist-fun tx parm-values)))
          )
          xy))

(defmeth distribution-proto :quant-value (x)
"returns the p quantile"
    (let* ((x (if (listp x) x (list x)))
           (parm-values (send self :parm-values))
           (quant-fun (send self :quant-fun))
           (xy
	    (if (and (>= (min (combine x)) 0)
		     (<= (max (combine x)) 1))
		(list x (apply #'funcall quant-fun x parm-values))
	      nil))
          )
          xy))


           



(defmeth density-proto :draw-normal-dens ()
"used to compare with the normal"
(cond ((eql (send self :dens-fun) 't-dens)
       (let ((x (send self :xrange)))
	 (send self :add-lines x (normal-dens x) :type 'dash :color 'green)
	 (send self :linestart-width (iseq 200) (repeat 1 200))))
      (t
       (let* ((x (send self :xrange))
	      y)
	 ;;in discrete cases, more points are needed
	 (when (> 100 (length x))
	       (setf x (rseq (first x) (max x) 100)))
	 (setf y (gaussian-dens x (first (slot-value 'mean-stdev))
				(second (slot-value 'mean-stdev))))
	 (send self :add-lines x y :type 'dash :color 'green)))))

(defmeth density-proto :draw-normal-cdf ()
"used with t-dens to compare with the normal"
  (let ((x (send self :xrange)))
    (send self :add-lines x (normal-cdf x):type 'dash :color 'green)
    (send self :linestart-width (iseq 200) (repeat 1 200))))





  
;;
;; These functions (yvals-top-***) calculate the y-values for the tops of the bars
;; of the probability function (f) and return 2 values for each x value
;; One takes 2 arguments and the other 3.

;;; functions to draw the densities and cdf's for discrete models

(defun yvals-top-pmf (function parms x)
 (map 'list #'(lambda (z) (repeat (apply #'funcall function z parms) 2))
       (iseq (min x) (max x))))

(defun yvals-top-cdf (function parms x)
  (map 'list #'(lambda (z) (repeat (apply #'funcall function z parms) 2))
       (iseq (min x) (max x))))


;;
;;
;; Yvals-all creates the correct pattern of y-values and zeroes
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



(defun yvals-all-pmf (function parms x)
   (combine 0 (map 'list #'(lambda (z) (combine z 0))
          (yvals-top-pmf function parms x))))


(defun yvals-all-cdf (function parms x)
 (let ((top (yvals-top-cdf function parms x)))
   (combine 0 top 1)))
   

;;
;; The function xvals-all creates the correct pattern of x-values for the
;; lines needed to draw the probability function.
;; The pattern is: (0,0,1,1,1,2,2,2,3,3,...,(max_x),(max_x)+1,(max_x)+1)
;;
(defun xvals-all (max-x)
(combine (list 0 0
             (repeat (iseq 1 max-x) (repeat 3 max-x))
             (+ max-x 1) (+ max-x 1))))

;;
;; This function returns the vectors representing the plot of the
;; discrete probability function.
;; modi by kic to draw separated bars

(defun discrete-pmf (function parms x)
"generates the lines to draw a discrete pmf"
 (let* (
        (maxx (max x))
        (minx (min x))
        (xvals (combine minx minx (repeat (iseq (1+ minx) maxx) 
                             (repeat 3 (- maxx minx))) (1+ maxx) (1+ maxx)))
        (yvals (yvals-all-pmf function parms x))
	;;(xx (combine (mapcar #'(lambda (p) (list p p (+ p 0.4) (+ p 0.4)))
			     ;;x)))
	(xx (combine (mapcar #'(lambda (p) (list (- p 0.2) (- p 0.2)
						(+ p 0.2) (+ p 0.2)))
			     x)))
	(yy (combine (mapcar #'(lambda (pair) (append '(0) pair '(0)))
			    (yvals-top-pmf function parms x)))) 
       )
;;(print x)
;;(print (yvals-top-pmf function parms x))
;;(print xx)
;;(print yy)
;;(terpri)
;;  (list xvals yvals)
(list xx yy)
))


(defun discrete-cdf (function parms x)
"generates the lines to draw a discrete cdf"
 (let* (
        (maxx (max x))
        (xvals (combine (repeat x (repeat 2 (length x))) maxx maxx))
        (yvals (yvals-all-cdf function parms x))
       )
  (list xvals yvals)))

;;;
;;;	This section starts the game and contains the main interaction
;;;	the main entry function is 'start-teach
;;;	it calls start-density with the appropriate restrictions
;;;     start-density can be called without args to get a full version


;;; main starting call
(defun start-density (&optional (dists *distributions*) (strings *distr-names*))
  (let* (
         (ask-dist (send text-item-proto :new "Tria una distribuci€:"))
         (dist-list (send list-item-proto :new strings :columns 2))
         (hlp (send button-item-proto :new "Ajuda"
                :action #'help-teach))
         (clo (Send button-item-proto :new "Tancar i Sortir"
         	:action #'(lambda ()
         		(message-dialog
"Aquest programa, versi€ desembre 1998,
  »s fet per Frederic Udina (udina@upf.es)
  com a material per a Probabilitat a Ciencia Politica (UPF)
  a partir d'un de Jan deLeeuw (deleeuw@stat.ucla.edu)
  i tot usant Xlisp-Stat, de Luke Tierney (luke@stat.umn.edu)
  Michael Greenacre (michael@upf.es) va col.laborar en una versio previa
")
  			(exit))))
         (ok (send button-item-proto :new "Veure model"
		   :action 
		   #'(lambda ()
		       (let* (
			      (choice (elt dists (send dist-list :selection)))
			      (choice-name (elt strings (send dist-list :selection)))
			      (dist-fun (find-symbol (string-upcase
						      (concatenate 'string choice "-cdf")) 
						     "DENS-DEMO"))
			      (dens-fun (find-symbol (string-upcase
						      (concatenate 'string choice "-dens"))
						     "DENS-DEMO"))
			      (pmf-fun (find-symbol (string-upcase
						     (concatenate 'string choice "-pmf")) 
						    "DENS-DEMO"))
			      (quant-fun (find-symbol (string-upcase
						       (concatenate 'string choice 
								    "-quant")) 
						      "DENS-DEMO"))
			      (cont (if dens-fun t nil))
			      )
			 (setq *current-model* (if (eq dists *dist-teach*)
						   (send dist-list :selection)
						 nil))
			 (setq *current-name*  choice-name)
			 ;;(format t "looking for ~a~%" choice)
			 (if (fboundp (find-symbol (string-upcase choice) "DENS-DEMO"))
			     (funcall (find-symbol (string-upcase choice) "DENS-DEMO")
				      (if dens-fun
					  (concatenate 'string choice-name 
						       ": Funci€ de densitat")
					(concatenate 'string choice-name
						     ": Funci€ de probabilitat"))
				      (concatenate 'string choice-name
						   ": Funci€ de distribuci€ acum.")
				      (if dens-fun dens-fun pmf-fun)
				      dist-fun quant-fun cont)
			   (message-dialog "Not fully implemented")))))))
    (send dialog-proto :new (list ask-dist dist-list (list ok hlp  clo)))
    (send dist-list :Selection 0)
))


(defvar *distributions* (list "GAUSSIAN" "RECTANGULAR" "EXPONENTIAL" "F" "BETA" "CHISQ"
"RAYLEIGH" "PARETO" "LOGISTIC" "LAPLACE" "WEIBULL" "TRIANGULAR"
"INVERSE-GAUSSIAN" "SUM-OF-UNIFORMS" "POWER" "PERKS" "T"
"HYPERBOLIC-SECANT" "EXPONENTIAL-POWER" "EXTREME-VALUE"
"BINOMIAL" "POISSON" "Geometric" "MULTINOMIAL" "MULTINORMAL")
"The names for the available distributions"
)

(defvar *distr-names* (list "Gaussian" "Rectangular" "Exponential" "F" "Beta" "Chisq"
"Rayleigh" "Pareto" "Logistic" "Laplace" "Weibull" "Triangular"
"Inverse-Gaussian" "Sum of Uniforms" "Power" "Perks" "T"
"Hyperbolic Secant" "Exponential Power" "Extreme Value"
"Binomial" "Poisson" "Geometric" "Multinomial" "Multinormal")
"The names to show for the available distributions"
)

;;; TEACH is a restricted version for teaching purposes.

(defun help-teach ()
 (message-dialog
"Tria un model de probabilitat i demana •Veure model•
Obtindr˝s la funci€ de densitat o de probabilitat
  segons el cas, i la funci€ de distribuci€: 
  fixa't en el tÃtol de cada finestra.
Despr»s de canviar els par˝metres del model,
  pots demanar 'Ajustar gr˝fics' per recentrar les finestres.
Si fas clic dins les finestres,
  veur˝s els valors de les funcions corresponents.
Per distribucions continues, pots tambe fer clic-i-arrossegar
  i veuras les probabilitats entre dos valors.
Pots 'copiar' els gr˝fics i dur-los a d'altres programes.
 ")
 (info-dialog))

(defun info-dialog ()
  (message-dialog
"Aquest programa, versi€ juliol 1998,
  »s fet per Frederic Udina (udina@upf.es)
  com a material per a Probabilitat a Ciencia Politica (UPF)
  a partir d'un de Jan deLeeuw (deleeuw@stat.ucla.edu)
  i tot usant Xlisp-Stat, de Luke Tierney (luke@stat.umn.edu)
  Michael Greenacre (michael@upf.es) va col.laborar en una versio previa
"))


(defmeth density-proto :help-model ()
  (let ((key (send self :dens-fun)))
 (message-dialog
    (case key
	  ('binomial-pmf
"En la distr binomial, n »s el nombre de proves de Bernouilli,
p »s la probabilitat d'Àxit en cada una d'elles,
i els resultats x s€n el nombre d'Àxits obtinguts.
 El segment blau de dalt indica la mitjana 
 i menys/mes una i dues desviacions tipiques.
")
    ('poisson-pmf
"La distribuci€ de Poisson t» un ônic par˝metre que coincideix
 amb el valor esperat o mitjana de la distribuci€
 (i tamb» amb la vari˝ncia).
 El segment blau de dalt indica la mitjana 
 i menys/mes una i dues desviacions tipiques.
"
)
    ('geometric-pmf
"Distribuci€ geomÀtrica: nombre d'experiments
 de Bernouilli fins el primer Àxit,
 el parametre p »s la probabilitat d'Àxit.
 El segment blau de dalt indica la mitjana 
 i menys/mes una i dues desviacions tipiques.
")
    ('Sum-of-uniforms-dens
"Aquesta distribuci€ »s la suma de n variables
aleatories uniformes en [0, 1]. Si n »s gran,
la distribuci€ s'assembla m»s i m»s a una normal.
")
    ('gaussian-dens
"La distribuci€ Gaussiana o normal
t» par˝metres mu (el valor esperat)
i sigma (la desviaci€ tÃpica).
En la finestra de la densitat gaussiana
es pot arrossegar el ratoli amb el bot€
premut per marcar una regi€ i veure'n 
la probabilitat.
 El segment blau de dalt indica la mitjana 
 i menys/m»s una i dues desviacions tÃpiques.
")
    ('Exponential-dens
"La distribuci€ exponencial t» un ônic par˝metre lambda
 que coincideix amb el punt d'arrencada de la funci€ de densitat
 i »s l'invers del valor esperat.
 El segment blau de dalt indica la mitjana 
 i menys/mes una i dues desviacions tipiques.
")
    ('ChiSq-dens
"El par˝metre df s'anomena 'graus de llibertat'.
")
    (t "Ajuda no disponible")))))


(defvar  *dist-teach* (list "Binomial" "Poisson" "Geometric" "Sum-of-uniforms"
	 "Gaussian" "Exponential")
"The available distributions in the restricted demo"
)

(defvar *strings-teach* (list "Binomial" "Poisson" "GeomÀtrica" "Suma d'uniformes"
	 "Normal" "Exponencial")
"Names to be displayed for the available distributions in the restricted demo"
)


(defun start-teach ()
	(start-density *dist-teach* *strings-teach*))

;;this is the main working function, big!
;;it creates the objects for a distribution linked to the
;;dialog to control it.
(defun distribution-demo (&key pdf-title cdf-title 
                               dens-fun dist-fun quant-fun cont xcompact
                               parm-names parm-values parm-ranges
                               dialog-title)
  (let* ((w (if dist-fun (send density-proto :new
			       :title pdf-title
			       :dens-fun dens-fun
			       :dist-fun dist-fun
			       :quant-fun quant-fun
			       :cont cont
			       :xcompact xcompact
			       :parm-names parm-names
			       :parm-values parm-values
			       :parm-ranges parm-ranges
			       :location (list 5 5)
			       :size (round (* 0.45 (screen-size))))))
	 (u (if dens-fun (send distribution-proto :new 
			       :title cdf-title
			       :dens-fun dens-fun
			       :dist-fun dist-fun
			       :quant-fun quant-fun
			       :cont cont
			       :xcompact xcompact
			       :parm-names parm-names
			       :parm-values parm-values
			       :parm-ranges parm-ranges
			       :location (list (/ (first (screen-size)) 2) 5)
			       :size (round (* 0.45 (screen-size)))
			       ;;#+unix (list 450 0)
			       ;;#+msdos (list 300 0)
			       ;;#+macintosh (list 300 0)
			       )))
	 (plots (if (and dist-fun dens-fun) (list u w)
		  (if dist-fun w u))))
    ;;
    ;;  For the t-dist'b'n, these lines add a reference graph of the 
    ;; standard normal to the plot in a thicker line type.
    ;; The lines representing the reference have linestarts 0 to 199
    ;;
    (send w :slot-value 'distr-object u)
    (send u :slot-value 'dens-object w)
    (send u :use-color t)
    (send w :use-color t)
    ;;;(send u :draw-color 'red)
    (apply #'send u :size (round (* 0.45 (screen-size))))
    (apply #'send w :size (round (* 0.45 (screen-size))))
    (when (eql 't-dens (send w :dens-fun))
	  (send w :draw-normal-dens)
	  (send u :draw-normal-cdf))
    (map-elements #'send plots :replot)
    (map-elements #'send plots :adjust-to-data)
    (map-elements #'send plots :mouse-mode 'show-coords)
    (map-elements #'send plots :show-window)
    (let* (dlg
	   (parm-labels (mapcar #'(lambda (x) 
				    (send text-item-proto :new x)) parm-names))
	   (parm-display (mapcar #'(lambda (x) 
				     (send text-item-proto :new (format nil "~a" x)
					   :text-length 5))
				 parm-values))
	   (parm-scrolls
	    (mapcar #'(lambda (label range display 
				     values)
			(send new-slider-proto :new  range
			      :action #'(lambda (x)
					  (let* ((ind (position label parm-labels))
						 (parm-values (send u :parm-values))
						 (seq (send self :slot-value 'sequence))
						 (dseq (combine (slot-value
								 'xlisp::display-sequence)))
						 (x (if (< x (first dseq))
							(first dseq) x))
						 (x (if (> x (car (last dseq)))
							(car (last dseq)) x))
						 (diff (- x (send self :old-x)))
						 (value (send self :value))
						 (old-value (send self :old-value))
						 (valuediff (- value old-value))
						 )
					    (send display :text (format nil "~5,3f" x))
					    (setf (elt parm-values ind) x)
					    (map-elements #'send plots :parm-values 
							  (repeat (list parm-values)
								  (length plots)))
					    ;;(send self :slot-value 'sequence
					    ;;	     (+ seq diff))
					    ;;(send self :value (- value valuediff))
					    (send self :old-x x)))));;;kic
		    parm-labels parm-ranges
		    parm-display parm-values))
	   (close-all (send button-item-proto :new "Tancar model"
			    :action
			    #'(lambda ()
				(send dlg :close)
				(map-elements #'send plots :remove)
				(gc))))
	   (hlp-model (send button-item-proto :new "Ajuda"
			    ;;:action #'hlp-model
			    :action #'(lambda ()
					(send w :slot-value 'first-shot nil)
					(send u :slot-value 'first-shot nil)
					(send w :help-model))
			    ))
	   (rescale (send button-item-proto :new "Ajustar gr˝fics"
			  :action
			  #'(lambda ()
			      (send w :slot-value 'first-shot nil)
			      (send u :slot-value 'first-shot nil)
			      (send u :replot)
			      (send u :adjust-to-data)
			      (send w :adjust-to-data))))
	   (show-normal 
	    (if (and (send w :slot-value 'mean-stdev)
		     (not (eql (send w :dens-fun) 'gaussian-dens)))
		(send toggle-item-proto :new "Veure Normal"
		      :action #'(lambda ()
				  (send w :slot-value 'first-shot nil)
				  (send u :slot-value 'first-shot nil)
				  (send w :slot-value 'show-normal 
					(send show-normal :value))
				  (send w :replot)))
	      (send text-item-proto :new "")))
	   (show-mnstdv 
	    (if (send w :slot-value 'mean-stdev)
		(send toggle-item-proto :new "Veure mitja+/-desv"
		      :action #'(lambda ()
				  (send w :slot-value 'first-shot nil)
				  (send u :slot-value 'first-shot nil)
				  (send w :slot-value 'show-mnstdv 
					(send show-mnstdv :value))
				  (send w :replot)))
	      (send text-item-proto :new ""))))
      (setf dlg (send dialog-proto :new
		      (append (list (list (transpose (list parm-labels
					       parm-scrolls 
					       parm-display))
					  (list show-mnstdv show-normal)))
			      (list (list close-all hlp-model rescale)))
		      :title *current-name* ;;dialog-title
		      :location
		      ;;#+unix (list 250 500)
		      #+unix (list 200 300)
		      #+msdos (list 200 300)
		      #+macintosh (list 200 300)
		      :go-away nil))
      (defmeth u :close ()
	(send dlg :close)
	(send w :remove)
	(send self :remove))
      (defmeth w :close ()
	(send dlg :close)
	(send u :remove)
	(send self :remove))
      (mapcar #'(lambda (x y) 
		  (send x :old-x
			(elt
			 (send x :slot-value 'sequence) 3))
		  (send x :old-value 3)
		  ;;(send x :value 3)
		  (send x :value
			(round (/ (1- (length
				       (send x :slot-value
					     'xlisp::display-sequence)))
				  2)))
		  (send x :do-action)
		  )
	      parm-scrolls parm-values)
      (send rescale :do-action)
      )
    (map-elements #'send plots :slot-value 'first-shot t)
))


(defun exit-dialog ()
  (ok-or-cancel-dialog "Exit xlisp-stat?"))
  


;;; Here all distributions are created using the macro make-dist
;;  that builds a call to distribution-demo for each implemented distribution
;;
;;  Format for entering a distribution:
;;
;;  make-dist name pdf-title cdf-title dens-fun dist-fun quant-fun cont
;;             XRANGE PARAMETER-NAMES PARAMETER-INITIAL-VALUES 
;;             INITIAL-RANGE-FOR-SCROLL-BAR
;;
;;  where XRANGE is either: 
;;  a) nil            if the domain of x is not bounded.
;;  b) (list LOW nil) where LOW is the lower bound for the domain of x
;;                    and there is no upper bound.
;;  c) (list nil UP)  where UP is the upper bound for the domain of x
;;                    and there is no lower bound.
;;  d) (list LOW UP)  where LOW is the lower bound for the domain of x
;;                    and UP is the upper bound for the domain of x.
;;
;;  PARAMETER-NAMES  is a list of strings for the names of the parameters.
;;
;;  PARAMETER-INITIAL-VALUES  is a list of read number for the initial
;;                            values of the parameters. 
;;
;;  INITIAL-RANGE-FOR-SCROLL-BAR  is a preliminary range for the 
;;                                scrolling of the parameters.  This range
;;                                can be extended by just scrolling on 
;;                                the scroll bar.
;;
;;
;;

(defmacro make-dist (fun pdf-title cdf-title dens-fun dist-fun quant-fun cont
                     xcompact parm-names parm-values parm-ranges)
`(defun ,fun  (,pdf-title ,cdf-title ,dens-fun ,dist-fun ,quant-fun ,cont)
   (distribution-demo 
      :pdf-title ,pdf-title
      :cdf-title ,cdf-title
      :dens-fun ,dens-fun
      :dist-fun ,dist-fun
      :quant-fun ,quant-fun
      :cont ,cont
      :xcompact ,xcompact
      :parm-names ,parm-names
      :parm-values ,parm-values
      :parm-ranges ,parm-ranges)))

(make-dist gaussian pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "Mu" "Sigma") 
           (list 0 2.5)
           (list (rseq -10 10 21) (rseq 0.01 10 35)))

(make-dist exponential pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 0 nil)
           (list "Lambda")
           (list 1)
           (list (rseq 0.1 3 59)))

(make-dist triangular pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list '(select (send self :parm-values) (list 0 2)))
           (list "Left" "Right" "Middle")
           (list 0 5 2.5);kic changes 10->2.5
           (list (iseq -3 3) (iseq 2 8) (iseq 7 13)))

(make-dist binomial pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 0 '(elt (send self :parm-values) 0))
           (list "n" "p")
           (list 10 0.5)
           (list (iseq 1 30) (rseq .01 .99 99)))

(make-dist geometric pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 0 '(+ 1 (round (/ (log 0.01)
                                   (log (- 1 (send self :parm-values)))))))
           (list "p")
           (list 0.5)
           (list (rseq .002 0.99 99)))

(make-dist poisson pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 0 nil) 
           (list "Mean")
           (list 1)
           (list (rseq 0.5 30.0001 60)))

(make-dist rectangular pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list '(send self :parm-values))
           (list "a" "b")
           (list 0 10)
           (list (rseq -3 3 7) (rseq 7 13 7)))

(make-dist sum-of-uniforms pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "n")
           (list 10)
           (list (iseq 1 16)))

(make-dist rayleigh pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "Sigma")
           (list 1)
           (list (rseq 0 2 7)))

(make-dist F pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 1 nil)
           (list "Numerator" "Denominator")
           (list 5 5)
           (list (iseq 2 8) (iseq 2 8)))

(make-dist chisq pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 0 nil)
           (list "Df")
           (list 3)
           (list (iseq 0 6)))

(make-dist power pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list -1 2)
           (list "a")
           (list 3)
           (list (iseq 0 6)))

(make-dist t pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list -4 4)
           (list "Df")
           (list 3)
           (list (iseq 1 30)))

(make-dist beta pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list .5 .5)
           (list "alpha" "beta")
           (list  1 1)
           (list (rseq .5 4.5 9) (rseq .5 4.5 9)))

(make-dist rectangular pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list '(send self :parm-values))
           (list "a" "b")
           (list 0 10)
           (list (rseq -3 3 7) (rseq 7 13 7)))


(make-dist pareto pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list '(second (send self :parm-values))
		 '(exp (/ (log (/ 0.01 (* (first (send self :parm-values))
					 (expt (second (send self :parm-values)) 
					       (first (send self :parm-values))))))
			  (- (1+ (first (send self :parm-values)))))))
           (list "a" "b")
           (list 1 1)
           (list (rseq .01 5 11) (rseq .01 5 11)))



(make-dist logistic pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "Mu" "Sigma")
           (list 0 5)
           (list (rseq -3 3 7) (rseq 2 8 7)))


(make-dist laplace pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "Mu" "Sigma")
           (list 0 5)
           (list (rseq -3 3 7) (rseq 4.5 5.5 7)))


(make-dist weibull pdf-title cdf-title dens-fun dist-fun quant-fun cont
           (list 1 nil)
           (list "a")
           (list 5)
           (list (rseq 4.5 5.5 7)))

(make-dist inverse-gaussian pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "Mu" "Lambda")
           (list 5 5)
           (list (rseq 4.5 5.5 7) (rseq 4.5 5.5 7)))


(make-dist perks pdf-title cdf-title dens-fun dist-fun quant-fun cont
           nil
           (list "Mu" "Sigma" "a")
           (list 0 5 3)
           (list (rseq -3 3 7) (rseq 4.5 5.5 7) (iseq 0 6)))

(make-dist hyperbolic-secant pdf-title cdf-title dens-fun dist-fun quant-fun 
           cont 
           nil
           (list "Mu" "Sigma")
           (list 0 1)
           (list (rseq -1 1 7) (rseq 1 10 50)))

(make-dist exponential-power pdf-title cdf-title dens-fun dist-fun quant-fun
           cont
           nil
           (list "Mu" "Sigma" "a")
           (list 0 1 3)
           (list (rseq -.5 .5 7) (rseq 4.5 5.5 7) (iseq 0 6)))

(make-dist multinormal pdf-title cdf-title dens-fun dist-fun quant-fun
           cont
           nil
           (list "Mu" "Sigma")
           (list 0 1)
           (list (rseq -1 1 7) (rseq .5 1.5 7)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; density and distribution functions for the supported dists
;; 
;; Stockpile of probability densities, cumulative distribution functions,
;; and their inverses (quantile functions). I did not do the various
;; generating functions, since their use is mainly theoretical and not
;; computational. 
;;
;; I did put in random generators, using the inversion method, 
;; wherever possible, even if it is relatively inefficient.
;;
;; Xlisp-Stat has support for beta, gamma, normal (no parameter), 
;; Cauchy (no parameter), gamma (one parameter), chi-square (one parameter), 
;; t (one parameter), Poisson (one parameter), beta (two parameters),
;; F (two parameters), binomial (two parameters), and bivnorm (one
;; parameter).
;;
;; I generalize to more parameters, if that seems desirable, changing
;; the names to new ones, different from the supported versions. And I 
;; added a slew of new ones, taken from Devroye, Johnson and Kotz, 
;; Abramowitz and Segun, and wherever I could find them.
;;
;; As a rule, I use recursive vectorization, and I check for the most
;; obvious errors.
;;
;; Version 1.0   -- 06-18-95 -- Jan de Leeuw
;;                              just collected what I had and organized it
;;                              and added some more.
;; Version 1.1   -- 06-19-95 -- added some error checks
;;                              added inverse gaussian stuff
;;                              added sum-of-uniforms
;;                              added power distribution
;; Version 1.1.a -- 06-20-95 -- added multinomial distribution
;;                              added Perks density and cdf
;;                              added hyperbolic secant
;;                              some bugs corrected
;; Version 1.1.b -- 06-21-95 -- more bugs corrected
;;                              added reading/debugging flags
;;                              added exponential power distribution
;; Version 1.1.c -- 06-23-95 -- bug fix in multinomial (Udina)
;;                              added pieces of extreme value, multinormal,
;;                              multivariate rectangular, negative
;;                              binomial
;;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; UNIFORM FUNCTION
;;
;;	The unif-dens function returns the x and y co-ordinates necessary
;;	to draw the rectangle representing the uniform density 
;;	on [a,b] over the range xrange
;;
(defun uniform-dens (xrange &optional (a 0) (b 10))

	(let (	(lowx  (min xrange))
		(highx (max xrange)))
	(list 	(list lowx a a b b highx)
		(list 0 0 (/ 1 (- b a)) (/ 1 (- b a)) 0 0))))

(defun uniform-cdf (xrange &optional (a 0) (b 10))
 (let (
       (l1 (/ (- xrange a) (- b a)))
      )
  (setf l1 (map-elements #'max l1 0))
  (setf l1 (map-elements #'min l1 1))
  l1))




;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; geometric one parameter udina april 97
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun geometric-pmf (x p)
 (if (listp p) (setf p (car p)))
 (if (listp x)
 	(map-elements #'geometric-pmf x (repeat p (length x)))
     (if (<= x 0) 0
              (* p (expt (- 1 p) (- x 1))))))

(defun geometric-cdf (x p)
 (if (listp p) (setf p (car p)))
 (if (listp x)
 	(map-elements #'geometric-cdf x (repeat p (length x)))
  (if (<=  x 0) 0 (- 1 (expt (- 1 p) x)))))

(defun geometric-quant (pii p)
  (if (compound-data-p pii)
      (map-elements #'geometric-quant pii p)
    (cond ((< pii 0) 0)
	  ((< pii 1) (ceiling (/ (log (- 1 pii)) (log (- 1 p)))))
	  (t 1))))
	   



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gaussian, two parameters 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gaussian-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'gaussian-dens x mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Gaussian"))
      (/ (normal-dens (/ (- x mu) sigma)) sigma))
    )
  )

(defun gaussian-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'gaussian-cdf x mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Gaussian"))
      (normal-cdf (/ (- x mu) sigma)))
    )
  )

(defun gaussian-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'gaussian-quant p mu sigma)
    (progn
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Gaussian"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
     (+ mu (* sigma (normal-quant p))))
    )
  )

(defun gaussian-rand (n &optional (mu 0) (sigma 1))
  (map-elements #'gaussian-quant (uniform-rand n) mu sigma)
  )

;(format t "Gaussian loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangular on [a,b]
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rectangular-dens (x &optional (a 0) (b 1))
  (if (compound-data-p x)
      (map-elements #'rectangular-dens x a b)
    (progn
      (if (<= b a)
          (error "Nonsense interval for rectangular"))
      (cond ((< x a) 0)
            ((> x b) 0)
            (t (/ (- b a)))
            ))
    )
  )

(defun rectangular-cdf (x &optional (a 0) (b 1))
  (if (compound-data-p x)
      (map-elements #'rectangular-cdf x a b)
    (progn
      (if (<= b a)
          (error "Nonsense interval for rectangular"))
      (cond ((< x a) 0)
            ((> x b) 1)
            (t (/ (- x a) (- b a)))
            ))
    )
  )

(defun rectangular-quant (p &optional (a 0) (b 1))
  (if (compound-data-p p)
      (map-elements #'rectangular-quant p a b)
    (progn
      (if (<= b a)
          (error "Nonsense interval for rectangular"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (+ a (* (- b a) p)))
    )
  )


(defun rectangular-rand (n &optional (a 0) (b 1))
  (map-elements #'rectangular-quant (uniform-rand n) a b)
  )

;(format t "Rectangular loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponential 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exponential-dens (x &optional (lambda 1))
  (if (compound-data-p x)
      (map-elements #'exponential-dens x lambda)
    (progn
      (if (<= lambda 0)
          (error "Non-positive exponential parameter"))
      (cond ((< x 0) 0)
            (t (* lambda (exp (- (* lambda x)))))
            ))
    )
  )

(defun exponential-cdf (x &optional (lambda 1))
  (if (compound-data-p x)
      (map-elements #'exponential-cdf x lambda)
    (progn
      (if (<= lambda 0)
          (error "Non-positive exponential parameter"))
      (cond ((< x 0) 0)
            (t (- 1 (exp (- (* lambda x)))))
            ))
    )
  )

(defun exponential-quant (p &optional (lambda 1))
  (if (compound-data-p p)
      (map-elements #'exponential-quant p lambda)
    (progn
      (if (<= lambda 0)
          (error "Non-positive exponential parameter"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (- (/ (log (- 1 p)) lambda)))
    )
)

(defun exponential-rand (n &optional (lambda 1))
  (map-elements #'exponential-quant (uniform-rand n) lambda)
  )

;(format t "Exponential loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rayleigh 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rayleigh-dens (x &optional (sigma 1))
  (if (compound-data-p x)
      (map-elements #'rayleigh-dens x sigma)
    (progn
      (if (<= sigma 0)
          (error "Non-positive Rayleigh parameter"))
      (let ((y (/ x sigma)))
        (cond ((< x 0) 0)
              (t (* y (exp ( - (/ (^ y 2) 2)))))
              )))
    )
  )

(defun rayleigh-cdf (x &optional (sigma 1))
  (if (compound-data-p x)
      (map-elements #'rayleigh-cdf x sigma)
    (progn
      (if (<= sigma 0)
          (error "Non-positive Rayleigh parameter"))
      (cond ((< x 0) 0)
            (t (- 1 (exp (- (/ (^ (/ x sigma) 2) 2)))))
            ))
    )
  )

(defun rayleigh-quant (p &optional (sigma 1))
  (if (compound-data-p p)
      (map-elements #'rayleigh-quant p sigma)
    (progn
      (if (<= sigma 0)
          (error "Non-positive Rayleigh parameter"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (* sigma (sqrt (- (log (- 1 p))))))
    )
  )

(defun rayleigh-rand (n &optional (sigma 1))
  (map-elements #'rayleigh-quant (uniform-rand n) sigma)
  )

;(format t "Rayleigh loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Triangular on [a,b] with mode at c 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun triangular-dens (x &optional (a 0) (b 1) (c .5)) 
  (if (compound-data-p x)
      (map-elements #'triangular-dens x a b c)
    (progn
      (if (not (< a c b))
          (error "Parameters for triangular not in correct order"))
      (cond ((< x a) 0)       
            ((> x b) 0)
            (t (if (<= x c) (/ (* 2 (- x a)) (* (- b a) (- c a)))
                 (/ (* 2 (- x b)) (* (- b a) (- c b)))))))
    )
  )

(defun triangular-cdf (x &optional (a 0) (b 1) (c .5))
  (if (compound-data-p x)
      (map-elements #'triangular-cdf x a b c)
    (progn
      (if (not (< a c b))
          (error "Parameters for triangular not in correct order"))
      (cond ((< x a) 0)
            ((> x b) 1)
            (t (if (<= x c) (/ (^ (- x a) 2) (* (- b a) (- c a)))
                 (/ (+ (* (- x a) (- b c)) (* (- x c) (- b x)))
                    (* (- b a) (- b c)))))))
    )
  )

;(format t "Triangular loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pareto 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pareto-dens (x &optional (a 1) (b 1))
  (if (compound-data-p x)
      (map-elements #'pareto-dens x a b)
    (progn 
      (cond ((< x b) 0)
	    (t (/ (* a (expt b a)) (expt x (1+ a))))
	    ))
    )
  )

(defun pareto-cdf (x &optional (a 1) (b 1))
  (if (compound-data-p x)
      (map-elements #'pareto-cdf x a b)
    (progn 
      (cond ((< x b) 0)
            (t (- 1 (expt (/ b x) a)))
            ))
    )
  )

(defun pareto-quant (p &optional (a 1) (b 1))
  (if (compound-data-p p)
      (map-elements #'pareto-quant p a b)
    (progn 
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (/ b (expt (- 1 p) (/ a))))
    )
  )

(defun pareto-rand (n &optional (a 1) (b 1))
  (map-elements #'pareto-quant (uniform-rand n) a b)
  )

;(format t "Pareto loaded\n")
;; Logistic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logistic-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'logistic-dens x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for logistic"))
      (let* ((z (/ (- x mu) sigma))
             (y (exp (- z))))
        (/ y (^ (1+ y) 2) sigma)))
    )
  )

(defun logistic-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'logistic-cdf x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for logistic"))
      (/ (1+ (exp (- (/ (- x mu) sigma))))))
    )
  )

(defun logistic-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'logistic-quant p mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for logistic"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (+ mu (* sigma (log (/ p (- 1 p))))))
    )
  )

(defun logistic-rand (n &optional (mu 0) (sigma 1))
  (map-elements #'logistic-quant (uniform-rand n) mu sigma)
  )

;(format t "Logistic loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Laplace 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun laplace-dens (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'laplace-dens x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (/ (exp (- (abs (/ (- x mu) sigma)))) (* 2 sigma)))
    )
  )

(defun laplace-cdf (x &optional (mu 0) (sigma 1))
  (if (compound-data-p x)
      (map-elements #'laplace-cdf x mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (let ((y (/ (exp (- (abs (/ (- x mu) sigma)))) 2)))
        (if (<= x mu) y (- 1 y))))
    )
  )

(defun laplace-quant (p &optional (mu 0) (sigma 1))
  (if (compound-data-p p)
      (map-elements #'laplace-quant p mu sigma)
    (progn 
      (if (<= sigma 0)
          (error "Incorrect standard deviation for Laplace"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (if (<= p .5)
          (+ mu (* sigma (log (* 2 p))))
        (- mu (* sigma (log (* 2 (- 1 p)))))))
    )
  )

(defun laplace-rand (n &optional (mu 0) (sigma 1))
   (map-elements #'laplace-quant (uniform-rand n) mu sigma)
   )

;(format t "Laplace loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Weibull (one parameter)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun weibull-dens (x &optional (a 1))
  (if (compound-data-p x)
      (map-elements #'weibull-dens x a)
    (progn 
      (* a (expt x (1- a)) (exp (- (expt x a)))))
    )
  )

(defun weibull-cdf (x &optional (a 1))
  (if (compound-data-p x)
      (map-elements #'weibull-cdf x a)
    (progn 
      (- 1 (exp (- (expt x a)))))
    )
  )

(defun weibull-quant (p &optional (a 1))
  (if (compound-data-p p)
      (map-elements #'weibull-quant p a)
    (progn 
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (expt (- (log (- 1 p))) (/ a)))
    )
  )

(defun weibull-rand (n &optional (a 1))
  (map-elements #'weibull-quant (uniform-rand n) a)
  )

;(format t "Weibull loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inverse Gaussian ; compare Devroye, pages 148-150
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inverse-gaussian-dens (x &optional (mu 1) (lambda 1))
  (if (compound-data-p x)
      (map-elements #'inverse-gaussian-dens x mu lambda)
    (progn
      (if (or (< mu 0) (< lambda 0))
          (error "Wrong parameters for inverse Gaussian"))
      (if (< x 0) 0
        (* (sqrt (/ lambda (* 2 pi (^ x 3))))
           (exp (- (/ (* lambda (^ (- x mu) 2))
                      (* x (^ mu 2)))))))
      )
    )
  )

(defun inverse-gaussian-cdf (x &optional (mu 1) (lambda 1))
  (if (compound-data-p x)
      (map-elements #'inverse-gaussian-cdf x mu lambda)
      (progn
        (if (or (< mu 0) (< lambda 0))
            (error "Wrong parameters for inverse Gaussian"))
        (if (<= x 0) 0
          (+ (normal-cdf (* (sqrt (/ lambda x)) (1- (/ x mu))))
             (* (exp (/ (* 2 lambda) mu))
              (normal-cdf (- (* (sqrt (/ lambda x)) (1+ (/ x mu))))))))
        )
      )
    )

(defun inverse-gaussian-quant (p &optional (mu 1) (lambda 1))
  (if (compound-data-p p)
      (map-elements #'inverse-gaussian-quant p mu lambda)
    (rough-invert-cdf #'inverse-gaussian-cdf p (list mu lambda))))

(defun one-inverse-gaussian-rand (&optional (mu 1) (lambda 1))
  (let* ((y (^ (first (normal-rand 1)) 2))
         (h (- (+ mu (/ (* y (^ mu 2)) (* 2 lambda)))
               (* (/ mu (* 2 lambda))
                  (sqrt (+ (* 4 mu lambda y) (^ (* mu y) 2))))))
         (u (one-uniform-rand)))
    (if (<= u (/ mu (+ mu h))) h (/ (^ mu 2) h))
    )
  )
        
(defun inverse-gaussian-rand (n &optional (mu 1) (lambda 1))
 (mapcar #'(lambda (x)
             (one-inverse-gaussian-rand mu lambda))
         (make-list n))
 )

;(format t "Inverse Gaussian loaded\n") 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum of n Uniforms ; compare Devroye, pages 21-22
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun positive-part (x)
  (if (compound-data-p x)
      (map-elements #'positive-part x)
    (if (>= x 0) x 0))
  )

(defun sum-of-uniforms-dens (x &optional (n 2))
    (if (= n 1)
    	(mapcar #'(lambda (xx) (if (<= 0 xx 1) 1 0))
		x)
     (let ((ff 1)
           (bf 1)
           (nn (1- n))
           (ss (* n (^ (positive-part x) (1- n)))))
       (dolist (i (1+ (iseq n)))
        (setf ff (* ff i))
        (setf bf (* bf (/ (1+ (- n i)) i)))
        (if (evenp i)
           (incf ss (* n bf (^ (positive-part (- x i)) nn)))
          (decf ss (* n bf (^ (positive-part (- x i)) nn))))
        )
       (/ ss ff)
    )
  ))
  
(defun sum-of-uniforms-cdf (x &optional (n 2))
  (let ((ff 1)
        (bf 1)
        (ss (^ (positive-part x) n)))
    (dolist (i (1+ (iseq n)))
      (setf ff (* ff i))
      (setf bf (* bf (/ (1+ (- n i)) i)))
      (if (evenp i)
          (incf ss (* bf (^ (positive-part (- x i)) n)))
        (decf ss (* bf (^ (positive-part (- x i)) n))))
      )
    (/ ss ff)
    )
  )

(defun sum-of-uniforms-quant (p &optional (n 2))
  (if (compound-data-p p)
      (map-elements #'sum-of-uniforms-quant p n)
  (rough-invert-cdf #'sum-of-uniforms-cdf p (list n))))


;(format t "Sum of Uniforms loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Power distribution ; compare Devroye, pages 24
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun power-dens (x &optional (a 0))
  (if (compound-data-p x)
      (map-elements #'power-dens x a)
    (progn
      (if (<= a -1)
          (error "Incorrect parameter power distribution"))
      (cond ((<= x 0) 0)
            ((> x 1) 0)
            (t (* (1+ a) (expt x a)))
            )
      )
    )
  )

(defun power-cdf (x &optional (a 0))
  (if (compound-data-p x)
      (map-elements #'power-cdf x a)
    (progn
      (if (<= a -1)
          (error "Incorrect parameter power distribution"))
      (cond ((<= x 0) 0)
            ((> x 1) 1)
            (t (expt x (1+ a)))
            )
      )
    )
  )

(defun power-quant (p &optional (a 0))
  (if (compound-data-p p)
      (map-elements #'power-quant p a)
    (progn
      (if (<= a -1)
          (error "Incorrect parameter power distribution"))
      (if (or (<= p 0) (>= p 1))
          (error "Probability not strictly between 0 and 1")) 
      (expt p (/ (1+ a)))
      )
    )
  )

(defun power-rand (n &optional (a 0))
  (map-elements #'power-quant (uniform-rand n) a)
  )

;(format t "Power loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multinomial distribution 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinomial-pmf (x &optional (p (/ (length n))))
  (/ (prod (poisson-pmf x p)) (poisson-pmf (sum x) 1))
  )

(defun one-multinomial-rand 
  (&optional (nn 1) (d 2) (p (make-list d :initial-element (/ d))))
  (let ((s 1)
        (m nn)
        (x (make-list d)))
    (dotimes (k d x)
      (let ((pp (elt p k)))
        (cond ((= m 0) (setf (elt x k) 0))
	      ((>= pp s);just for the case that fp arit gives that
	       (decf m (setf (elt x k) m))
	       (decf s pp))
              (t (decf m (setf (elt x k) 
                               (first (binomial-rand  1 m (/ pp s)))))
                 (decf s pp)))) 
      )
    )
  )

(defun multinomial-rand 
  (n &optional (nn 1) (d 2) (p (make-list d :initial-element (/ d))))
 (mapcar #'(lambda (x)
             (one-multinomial-rand nn d p))
         (make-list n))
 )   

;(format t "Multinomial loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perks distribution (Devroye, p. 287, p. 472)
;; For teaching purposes, the a parameter could be put on a slider 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun perks-dens (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'perks-dens x mu sigma a)
    (progn
      (if (<= a -2)
          (error "Incorrect shape parameter Perks distribution"))
      (if (<= sigma 0)
          (error "Incorrect sigma parameter Perks distribution"))
      (let* ((z (/ (- x mu) sigma))
             (b (/ a 2))
             (k (cond ((= a 2) 0)
                      ((< a 2) (sqrt (- 1 (^ b 2))))
                      (t (sqrt (- (^ b 2) 1)))))
             (c (cond ((= a 2) 1)
                      ((< a 2) (/ (- (/ pi 2) (atan (/ b k))) k)) 
                      (t (/ (log (/ (abs (+ k b)) (abs (- k b)))) 2 k)))))
        (/ (* c sigma (+ (exp z) (exp (- z)) a))))
      )
    )
  )

(defun perks-cdf (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'perks-cdf x mu sigma a)
    (progn
      (if (<= a -2)
          (error "Incorrect shape parameter Perks distribution"))
      (if (<= sigma 0)
          (error "Incorrect sigma parameter Perks distribution"))
      (let* ((z (/ (- x mu) sigma))
             (b (/ a 2))
             (k (cond ((= a 2) 0)
                      ((< a 2) (sqrt (- 1 (^ b 2))))
                      (t (sqrt (- (^ b 2) 1)))))
             (c (cond ((= a 2) 1)
                      ((< a 2) (/ (- (/ pi 2) (atan (/ b k))) k)) 
                      (t (/ (log (/ (abs (+ k b)) (abs (- k b)))) 2 k)))))
         (cond ((= a 2) (logistic-cdf x mu sigma)) 
               ((< a 2) (/ (- (atan (/ (+ b (exp x)) k))
                              (atan (/ b k))) (* c k)))
               (t (/ (- (log (/ (abs (+ k b)) 
                                (abs (- k b))))
                        (log (/ (abs (+ k (+ b (exp x))))
                                (abs (- k (+ b (exp x))))))) (* 2 c k)))
               )
         )                          
      )
    )
  )

;(format t "Perks loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hyperbolic secant distribution (Devroye, p. 471-472) 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hyperbolic-secant-dens (x &optional (mu 0) (sigma 1))
   (if (compound-data-p x)
      (map-elements #'hyperbolic-secant-dens x mu sigma)
     (progn
       (if (<= sigma 0)
           (error "Incorrect sigma parameter hyperbolic secant distribution"))
       (let ((z (/ (- x mu) sigma)))
         (/ 2 (* sigma pi (+ (exp z) (exp (- z))))))
      )
     )
   ) 

(defun hyperbolic-secant-cdf (x &optional (mu 0) (sigma 1))
   (if (compound-data-p x)
      (map-elements #'hyperbolic-secant-cdf x mu sigma)
     (progn
       (if (<= sigma 0)
           (error "Incorrect sigma parameter hyperbolic secant distribution"))
       (let ((z (/ (- x mu) sigma)))
         (/ (* 2 (atan (exp z))) pi))
       )
     )
   ) 

(defun hyperbolic-secant-quant (p &optional (mu 0) (sigma 1))
   (if (compound-data-p p)
      (map-elements #'hyperbolic-secant-quant p mu sigma)
     (progn
       (if (<= sigma 0)
           (error "Incorrect sigma parameter hyperbolic secant distribution"))
       (let ((z (/ (- p mu) sigma)))
         (+ mu (* sigma (log (tan (* (/ pi 2) p))))))
       )
     )
   )

(defun hyperbolic-secant-rand (n &optional (mu 0) (sigma 1))
  (map-elements #'hyperbolic-secant-quant (uniform-rand n) mu sigma)
  )

;(format t "Hyperbolic Secant loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponential Power distribution (Devroye, p. 287) 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gamma (x)
  (exp (log-gamma x)))

;; aquestes dues no es fan servir...
(defun vec-gamma (x exponent scale)
  (/ (* (^ scale exponent) (^ x (- exponent 1)) (exp (* -1 scale x)))
	(exp (log-gamma exponent))))


(defun vec-gamma-cdf (x exponent scale)
  (let (
        (l1 (cumsum (mapcar #'(lambda (u v) (* (- v u) 
                                    (vec-gamma (list (mean (list u v)))
                                                exponent scale)))
                        (butlast x) (rest x))))
       )
    (combine (append l1 (last l1)))
  )
)



(defun exponential-power-dens (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'exponential-power-dens x mu sigma a)
    (progn 
      (if (< sigma 0)
           (error "Incorrect sigma parameter exponential power distribution"))
      (if (< a 1)
           (error "Incorrect shape parameter exponential power distribution"))
      (let ((z (/ (- x mu) sigma)))
        (/ (exp (- (expt (abs z) a))) (* 2 sigma (gamma (1+ (/ a))))))
      )
    )
  )

(defun exponential-power-cdf (x &optional (mu 0) (sigma 1) (a 2))
  (if (compound-data-p x)
      (map-elements #'exponential-power-cdf x mu sigma a)
    (progn 
      (if (< sigma 0)
           (error "Incorrect sigma parameter exponential power distribution"))
      (if (< a 1)
           (error "Incorrect shape parameter exponential power distribution"))
      (let ((z (/ (- x mu) sigma)))
        (if (>= z 0)
            (/ (+ 1 (gamma-cdf z (/ a))) 2)
          (/ (- 1 (gamma-cdf (- z) (/ a))) 2)))
      )
    )
  )

(defun one-exponential-power-rand (&optional (mu 0) (sigma 1) (a 2))
  (+ mu (* sigma (expt (first (gamma-rand (1+ (/ a)))) (/ a))
           (- (* 2 (random 1.0)) 1)))
  )

;(format t "Exponential Power loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extreme value distribution (Devroye, p. 287) 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extreme-value-dens (x &optional (mu 0) (sigma 1) (k 1))
  (if (compound-data-p x)
      (map-elements #'extreme-value-dens x mu k)
    (progn 
      (if (< sigma 0)
           (error "Incorrect sigma parameter extreme-value distribution"))
      (if (or (not (integerp k)) (< k 1))
           (error "Incorrect shape parameter extreme-value distribution"))
      (* (exp (- (* k (log k)) (log-gamma k)))
         (exp (- (* k (+ x (exp (- x)))))))
      )
    )
  )

;(format t "Extreme Value loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized Inverse Gaussian  distribution (Devroye, p. 287) 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;(format t "Generalized Inverse Gaussian loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Negative binomial distribution  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negative-binomial-pmf (x &optional (n 1) (p .5))
  (if (compound-data-p x)
      (map-elements #'negative-binomial-pmf x n p)
    (progn 
      (if (or (< p 0) (> p 1))
          (error "Incorrect p parameter negative binomial distribution"))
      (if (or (not (integerp n)) (< n 0))
          (error "Incorrect n parameter negative binomial distribution"))
      (* (/ n (+ n x)) (binomial-pdf n (+ n x) p))
      )
    )
  )

;(format t "Negative Binomial loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multivariate normal distribution  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multinormal-dens 
  (x &optional (mu (repeat 0 length x))
     (sigma (identity-matrix (length x))))
  (let ((ndm (length x))
        (inv (inverse sigma))
        (det (determinant sigma)))
    (* (expt (* 2 pi det) (/ ndm 2))
       (exp (* -.5 (matmult (- x mu) (matmult inv (- x mu))))))
    )
  )

(defun independent-normal-dens (x mu sigma)
  (x &optional (mu (repeat 0 length x))
                (sigma (repeat 1ength x)))
  (let* ((zz (/ (- x mu) (sqrt sigma)))
         (pp (/ (normal-dens zz) (sqrt sigma))))
    (prod pp)
    )
  )

(defun multinormal-rand 
  (&optional (ndim 1)(mu (repeat 0 ndim)) (sigma (identity-matrix ndim)))
  (let ((s (chol-decomp sigma)))
    (+ mu (matmult s (normal-rand ndim))))
  )

(defun independent-normal-rand (mu sigma)
  (&optional (ndim 1)(mu (repeat 0 ndim)) (sigma (repeat 1 ndim)))
  (+ mu (* (sqrt sigma) (normal-rand ndim)))
  )

;(format t "Multivariate Normal loaded\n")
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multivariate Rectangular distribution  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multiuniform-dens (d)
  (&optional (ndim 1) (a (repeat 0 ndim)) (b (repeat 1 ndim)))
  (/ (prod (- b a)))
)

(defun multiuniform-rand 
  (&optional (ndim 1) (a (repeat 0 ndim)) (b (repeat 1 ndim)))
  (+ a (* (- b a) (mapcar #'random (repeat 1.0 ndim))))
)

;(format t "Multivariate Rectangular loaded\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aux functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rough-invert-cdf (cdf-fun p extra-args &key (tolerance 0.01) (verbose nil))
"a simple numerical inversion for CDF-FUN at P with EXTRA-ARGS
Sample call: (rough-invert-cdf #'inverse-gaussian-cdf 0.5 '(1 1))"
(let* ((m -1) (mm 1) mp fm fmm fmp)
  (labels ((iterate (a c)
		    (setf mp (/ (+ a c) 2))
		    (setf fm (apply cdf-fun a extra-args))
		    (setf fmp (apply cdf-fun mp extra-args))
		    (setf fmm (apply cdf-fun c extra-args))
		    (when verbose
		      (format t "looking between: ~a, ~a: ~a, ~a, ~a~%"
			      a c fm fmp fmm))
		    (cond ((< (- c a) tolerance)
			   ());exit
			  ((< fmp p)
			   (iterate mp c))
			  ((> fmp p)
			   (iterate a mp))
			  (t; found exact! never occur
			   (format t "bingo!~%" )))))
	  (when verbose
	    (format t "looking for inverse at ~a, fun:~a~%" p cdf-fun))
	  ;;search the range
	  (loop (setf fmm (apply cdf-fun mm extra-args))
		(when (> fmm p)
		  (return))
		(setf mm (* mm 2)))
	  (loop (setf fm (apply cdf-fun m extra-args))
		(when (< fm p)
		  (return))
		(setf m (/ mm 2)))
	  (when verbose
	    (format t "range for search: ~a, ~a~%" m mm))
	  (iterate m mm)
	  mp)))

;;(start-density)

#+dontload(defun find-plots ()
	(def wi (xlisp::active-windows))
	(def dpl (third wi))
	(def cpl (second wi)))
	

