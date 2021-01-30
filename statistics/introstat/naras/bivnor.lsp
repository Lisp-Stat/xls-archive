;;;;
;;;; bivnor.lsp                    B. Narasimhan
;;;;
;;;;  This is a little tool I use in my multivariate classes to
;;;;  demonstrate the Bivariate and Multivariate Normal distribution
;;;;  densities to my students.
;;;;  January 26, 1993.
;;;;
;;;;  GNU copyleft applies.
;;;;
;;;;  1)  Add some checks so that stupid values for the covariance
;;;;      matrix are avoided.

;;;; Bug Note:  There is an overlay in the Bivariate Normal
;;;;            plot that doesn't show up.  But if 
;;;;            you click in the top-left corner of the plot
;;;;            you will get a contour plot and the click will
;;;;            be disabled. It will be enabled again when
;;;;            you close the contour plot.

(provide "bivnor")

(require "statistics")
(require "graphics")
(require "help")


;;;
;;; These Lisp-Stat routines enable one to dynamically change the
;;; parameters for a bivariate normal density.
;;;

(defproto bivnor-proto '(title mu-1 mu-2
			       sigma-11 sigma-22 rho-12 
			       min-1 max-1 min-2 max-2
			       contour-obj contour-overlay update)
  nil spin-proto
  "The Bivariate Normal prototype.")

(defmeth bivnor-proto :isnew (&key title mu-1 mu-2 
				   sigma-11 sigma-22 rho-12
				   min-1 min-2 max-1 max-2)
  "Method args: (&key title mu-1 mu-2 sigma-11 sigma-22 rho-12
                      min-1 min-2 max-1 max-2)
Creates an instance of the bivnor prototype."
  (setf (slot-value 'title)
	(if title 
	    title
	  "BivNormal"))
  (setf (slot-value 'mu-1) 
	(if mu-1
	    mu-1
	  (select (get-value-dialog "Enter Mu-1 for the density.") 0)))
  (setf (slot-value 'mu-2) 
	(if mu-2
	    mu-2
	  (select (get-value-dialog "Enter Mu-2 for the density.") 0)))
  (setf (slot-value 'sigma-11) 
	(if sigma-11
	    sigma-11
	  (select (get-value-dialog "Enter Sigma 11 for the density.") 0)))
  (setf (slot-value 'sigma-22)
	(if sigma-22
	    sigma-22
	  (select (get-value-dialog "Enter Sigma 22 for the density.") 0)))
  (setf (slot-value 'rho-12)
	(if rho-12
	    rho-12
	  (select (get-value-dialog "Enter Rho 12 for the density.") 0)))
  (setf (slot-value 'min-1)
	(if min-1
	    min-1
	  -3))
  (setf (slot-value 'min-2)
	(if min-2
	    min-2
	  -3))
  (setf (slot-value 'max-1)
	(if max-1
	    max-1
	  3))
  (setf (slot-value 'max-2)
	(if max-2
	    max-2
	  3))
  (call-next-method 3 :show nil)


  (let* (
	 (s11 (slot-value 'sigma-11))
	 (s22 (slot-value 'sigma-22))
	 (r12 (slot-value 'rho-12))
	 (m1 (slot-value 'mu-1))
	 (m2 (slot-value 'mu-2))
	 (temp (- 1 (^ r12 2)))
	 (c (/ 1 (* 2 pi (sqrt (* s11 s22 temp))))))
    (send self :add-function 
	  #'(lambda(x y) 
	      (* c 
		 (exp (* (/ -0.5  temp)
			 (+ (/ (^ (- x m1) 2) s11)
			    (/ (^ (- y m2) 2) s22)
			    (/ (* 2 r12 (- m1 x) (- y m2))
			       (sqrt (* s11 s22))))))))
	  (slot-value 'min-1) (slot-value 'max-1)
	  (slot-value 'min-2) (slot-value 'max-2)
	  :num-points 10 :draw nil))


  (send self :adjust-to-data :draw nil)
  (send self :new-menu)
  (send self :showing-axes nil)
  (send self :rotate-2 0 1 (/ pi 3) :draw nil)
  (send self :rotate-2 1 2 (- (/ pi 3)) :draw nil)
  (send self :show-window)

  (let* (
         (mu1-label (send text-item-proto :new "Mean-1:"))
         (mu1-val (send text-item-proto :new "" :text-length 5))
         (mu1-scroll (send interval-scroll-item-proto :new
                           '(0 5)
                           :text-item mu1-val
                           :action
                           #'(lambda(x) (send self :change :mu-1 x))))

         (mu2-label (send text-item-proto :new "     Mean-2:"))
         (mu2-val (send text-item-proto :new "" :text-length 5))
         (mu2-scroll (send interval-scroll-item-proto :new
                           '(0 5)
                           :text-item mu2-val
                           :action
                           #'(lambda(x) (send self :change :mu-2 x))))

         (s11-label (send text-item-proto :new "Sigma-11:"))
         (s11-val (send text-item-proto :new "" :text-length 5))
         (s11-scroll (send interval-scroll-item-proto :new
                           '(0 5)
                           :text-item s11-val
                           :action
                           #'(lambda(x) (send self :change :sigma-11 x))))

         (s22-label (send text-item-proto :new "   Sigma-22:"))
         (s22-val (send text-item-proto :new "" :text-length 5))
         (s22-scroll (send interval-scroll-item-proto :new
                           '(0 5)
                           :text-item s22-val
                           :action
                           #'(lambda(x) (send self :change :sigma-22 x))))

         (r12-label (send text-item-proto :new "Rho-12:"))
         (r12-val (send text-item-proto :new "" :text-length 5))
         (r12-scroll (send interval-scroll-item-proto :new
                           '(-1 1)
                           :text-item r12-val
                           :action
                           #'(lambda(x) (send self :change :rho-12 x)))))
    (send mu1-scroll :value (slot-value 'mu-1))
    (send mu2-scroll :value (slot-value 'mu-2))
    (send s11-scroll :value (slot-value 'sigma-11))
    (send s22-scroll :value (slot-value 'sigma-22))
    (send r12-scroll :value (slot-value 'rho-12))
    (let ((dwin 
	   (send dialog-proto :new 
		 (list (list mu1-label mu1-val mu2-label mu2-val)
		       (list mu1-scroll mu2-scroll)
		       (list s11-label s11-val s22-label s22-val)
		       (list s11-scroll s22-scroll)
		       (list r12-label r12-val)
		       r12-scroll))))
      (send dwin :add-subordinate self)))
  (setf (slot-value 'update) t)
  (send self :add-contour-overlay))

(defmeth bivnor-proto :title ()
  "Method args: ()
Retrieve the title."
  (slot-value 'title))

(defmeth bivnor-proto :covariance ()
  "Method args: ()
Returns the Covariance Matrix."
  (let* ((s11 (slot-value 'sigma-11))
         (s22 (slot-value 'sigma-22))
         (s12 (* (slot-value 'rho-12) (sqrt s11) (sqrt s22))))
    (#2a((s11 s12) (s12 s22)))))

(defmeth bivnor-proto :mean ()
  "Method args: ()
Returns the Mean Vector."
  (list (slot-value 'mu-1) (slot-value 'mu-2)))

(defmeth bivnor-proto :change (&key mu-1 mu-2 
				    sigma-11 sigma-22 rho-12
				    min-1 min-2 max-1 max-2)
  "Method args: (&key mu-1 mu-2 sigma-11 sigma-22 rho-12
                      min-1 min-2 max-1 max-2)
Changes the distribution parameters."
  (if mu-1
      (setf (slot-value 'mu-1) mu-1))
  (if mu-2
      (setf (slot-value 'mu-2) mu-2))
  (if sigma-11
      (setf (slot-value 'sigma-11) sigma-11))
  (if sigma-22
      (setf (slot-value 'sigma-22) sigma-22))
  (if rho-12
      (setf (slot-value 'rho-12) rho-12))
  (if min-1
      (setf (slot-value 'min-1) min-1))
  (if min-2
      (setf (slot-value 'min-2) min-2))
  (if max-1
      (setf (slot-value 'max-1) max-1))
  (if max-2
      (setf (slot-value 'max-2) max-2))
  (if (slot-value 'update)
      (let* (
             (s11 (slot-value 'sigma-11))
             (s22 (slot-value 'sigma-22))
             (r12 (slot-value 'rho-12))
             (m1 (slot-value 'mu-1))
             (m2 (slot-value 'mu-2))
             (tmp (- 1 (^ r12 2)))
             (c (/ 1 (* 2 pi (sqrt (* s11 s22 tmp))))))

        (send self :clear :draw nil)
	(send self :add-function 
	      #'(lambda(x y) 
		  (* c 
		     (exp (* (/ -0.5  tmp)
			     (+ (/ (^ (- x m1) 2) s11)
				(/ (^ (- y m2) 2) s22)
				(/ (* 2 r12 (- m1 x) (- y m2))
				   (sqrt (* s11 s22))))))))
	      (slot-value 'min-1) (slot-value 'max-1)
	      (slot-value 'min-2) (slot-value 'max-2)
	      :num-points 10 :draw t))))

(defmeth bivnor-proto :redraw ()
  (call-next-method))
;;  (if (slot-value 'contour-overlay)
;;      (send (slot-value 'contour-overlay) :redraw)))

(defmeth bivnor-proto :add-contour-plot ()
  "Message args: ()
Adds a contour-plot."
  (setf (slot-value 'contour-obj) 
	(send contour-proto :new 
	      self
	      (slot-value 'mu-1)
	      (slot-value 'mu-2)
	      (slot-value 'sigma-11)
	      (slot-value 'sigma-22)
	      (slot-value 'rho-12)
	      (slot-value 'min-1)
	      (slot-value 'min-2)
	      (slot-value 'max-1)
	      (slot-value 'max-2))))

(defmeth bivnor-proto :add-contour-overlay ()
  (let* ((ascent (send self :text-ascent))
	 (x ascent)
	 (y (round (* 1.5 ascent)))
	 (box ascent))
    (setf (slot-value 'contour-overlay)
	  (send contour-overlay-proto :new
		(list x y box (round (+ x (* 1.5 box)))))))
  (send self :add-overlay (slot-value 'contour-overlay))
  (send self :redraw))

(defmeth bivnor-proto :drop-contour-overlay ()
  (send self :delete-overlay (slot-value 'contour-overlay))
  (setf (slot-value 'contour-overlay) nil)
  (send self :redraw))
  

(defproto contour-overlay-proto '(location) nil graph-overlay-proto)

(defmeth contour-overlay-proto :isnew (location)
  (setf (slot-value 'location) location)
  (call-next-method))

(defmeth contour-overlay-proto :location ()
  (slot-value 'location))

(defmeth contour-overlay-proto :redraw ()
  (let* ((loc (send self :location))
	 (x (first loc))
	 (y (second loc))
	 (box (third loc))
	 (string-x (fourth loc))
	 (graph (send self :graph)))
    (send graph :frame-rect x (- y box) box box)
    (send graph :draw-string "Contours" string-x y)))

(defmeth contour-overlay-proto :do-click (x y m1 m2)
  (let* ((loc (send self :location))
	 (box (third loc))
	 (left (first loc))
	 (top (- (second loc) box))
	 (right (+ left box))
	 (bottom (+ top box))
	 (graph (send self :graph)))
    (when (and (< left x right) (< top y bottom))
	  (send graph :add-contour-plot)
	  t)))

(defproto contour-proto '(the-boss) nil scatterplot-proto
  "A simple prototype for handling contour plot.")

(defmeth contour-proto :isnew (the-boss m1 m2 
					s11 s22 r12
					min-1 min-2 max-1 max-2)
  "Method args: (the-boss mu-1 mu-2 
                          sigma-11 sigma-22 rho-12
                          min-1 min-2 max-1 max-2)
Creates an instance of the contour prototype."
  (setf (slot-value 'the-boss) the-boss)
  (call-next-method 2 :show nil)
  (let* (
         (tmp (- 1 (^ r12 2)))
         (c (/ 1 (* 2 pi (sqrt (* s11 s22 tmp))))))
    (send self :add-function-contours
          #'(lambda(x y) 
	      (* c 
		 (exp (* (/ -0.5 tmp)
			 (+ (/ (^ (- x m1) 2) s11)
			    (/ (^ (- y m2) 2) s22)
			    (/ (* 2 r12 (- m1 x) (- y m2))
			       (sqrt (* s11 s22))))))))
	  min-1 max-1 min-2 max-2))
  (send self :adjust-to-data :draw nil)
  (send self :title (send the-boss :title))
  (send self :show-window)
  (send the-boss :drop-contour-overlay)
  (send the-boss :add-subordinate self))

(defmeth contour-proto :close ()
  (send (slot-value 'the-boss) :add-contour-overlay)
  (call-next-method))


(setf z (send bivnor-proto :new 
              :mu-1 0
              :mu-2 0
              :sigma-11 1
              :sigma-22 1
              :rho-12 0))











