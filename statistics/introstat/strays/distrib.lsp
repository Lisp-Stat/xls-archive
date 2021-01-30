;; Distributions -  moving point in parameterspace
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
;;
;;
;;
;;
;;
;;
;; Note: You might be able to combine the VGA, SVGA cases using
;; screen-size and :frame-position and :frame-location. (I'm not sure
;; I have these working quite right in Windows -- I know they are not
;; quite right in X11 and some window managers.

;;
;; Some changes/corrections by Kjetil Halvorsen, La Paz, 1998:
;;    Most cleaning up and clarifying, and a few bugs corrected.
;;

;; starting values

;(setq s2p (sqrt (* 2 pi)))

(setq par1 0)
(setq par2 1)

(def x (rseq -10 10 300))

(def u '(10 10))
(def v '(0 .7))

(def u1 '(0 0))
(def v1 '(1 0))


;;;
;;; DENSITY FUNCTIONS:
;;;

#|
(defun lndf (x m s)
  (/ (exp (* (- .5) 
             (/ (- (log x) 
                   m) 
                s)
             (/ (- (log x) 
                   m) 
                s)))
     (* s  x s2p)))
|#

(defun lndf (x m s)
(let ((ex (exp x)))
  (* (/ ex s)
     (normal-dens (/ (- ex m)
                     s)))))
                                                             
;;; redefined by Kjetil Halvorsen, 1998 (bug in original def)
(defun ndf (x m s)  ;; arguments are mean and standard deviation.
  (/ (normal-dens (/ (- x m)
                     s))
     s))

#|
(defun gammadf (x a b)
  (/ (* (exp (* a (log  b)))  
        (exp (* (- a 1) (log x))) 
        (exp (- (* x b))))
     (/ 1 (* (gamma-dens 1 a) 
             (exp 1)))))
|#

(defun gammadf (x a b)  ;; a: form   b:  inverse scale
(* b (gamma-dens (* x b) a)))



;;;   (defun gamma (a)
;;;       (/ 1 (* (gamma-dens 1 (+ a 1)) (exp 1))))

(setf (symbol-function 'density) #'ndf)

(def y (ndf x par1 par2))

;; construct the plots

(def dist-plot (plot-lines x y))
(send dist-plot :y-axis t nil 2)
(send dist-plot :x-axis t nil 2)

(def par-plot  (plot-points (list par1) (list par2) :menu-title "Parameter Space"))
;; (send par-plot :draw nil)
(send par-plot :title "Parameter space")
(send par-plot :y-axis t t 2)
(send par-plot :x-axis t t 2)
(send par-plot :use-color t)
(send par-plot :draw-color 'red)

;; add a new "mouse mode", with menu title,
;; cursor and mouse method name

(send par-plot :add-mouse-mode 'point-moving
      :title "Point Moving"
      :cursor 'finger
      :click :do-point-moving)

;; add the new mouse method

(defmeth par-plot :do-point-moving (par1 par2 a b)
  (let ((p (send self :drag-point par1 par2)))
    (if p (send self :update-picture))))

;; plot densityfunction

(defmeth par-plot :update-picture ()
  (setq par1 (send self :point-coordinate 0 0 ))
  (setq par2 (send self :point-coordinate 1 0 ))
  (send dist-plot :set-parameters par1 par2)
;;  (send self :add-lines u1 v1 :type 'dashed :draw-color 'red)
  )

(defmeth dist-plot :set-parameters (par1 par2)
  (setq y (density x par1 par2))
  (send self :clear :draw nil)
  (send self :add-lines x y)
  (send self :add-lines u v))

;; modified :drag-point method to constrain the point to the current
;; plot range

(defmeth par-plot :drag-point (x y &key (draw t))
  (let ((p (call-next-method x y :draw nil)))
    (if p
	(flet ((mm (x r) (min (max x (first r)) (second r))))
	  (let ((rx (send self :range 0))
		(ry (send self :range 1))
		(x (send self :point-coordinate 0 p))
		(y (send self :point-coordinate 1 p)))
	    (send self :point-coordinate 0 p (mm x rx))
	    (send self :point-coordinate 1 p (mm y ry))
	    (if draw (send self :redraw))
	    p)))))


(defun normal-density-demo ()
  (setq par1 0)
  (setq par2 1)
  (setq u '(-10 -10))
  (setq v '(0 .7 ))
  (setq u1 '(5 5 -5 -5 5 5 -5 -5 -4 -4 -3 -3 -2 -2 -1 -1 0 0 1 1 2 2 3 3 4 4 ))
  (setq v1 '(0 3  3  2 2 1  1  0  0  3  3  0  0  3  3  0 0 3 3 0 0 3 3 0 0 3))
  (setf (symbol-function 'density) #'ndf)
  (setq x (rseq (- 10) 10 300))
  (send dist-plot :title "Normal distribution")
  (send dist-plot :set-parameters par1 par2)
  (send dist-plot :location 15 20 )  ; for vga
  (send dist-plot :size 600 160 )    ; for vga
  (send dist-plot :range 0 -10.0 10.0)
  (send dist-plot :range 1  0.0 .7)
  (send par-plot :variable-label '(0 1) (list "Mu" "Sigma"))
  (send par-plot :clear-lines :draw nil)
  (send par-plot :location 148 205)
  (send par-plot :size 340 150)
  (send par-plot :range 0 -5.0 5.0)
  (send par-plot :range 1  0.1 3.0)
  (send par-plot :point-coordinate 0 0  par1)
  (send par-plot :point-coordinate 1 0  par2)
  (send par-plot :add-lines u1 v1 :type 'dashed :draw nil)
  (send par-plot :redraw))


(defun log-normal-density-demo ()
  (setq par1 1)
  (setq par2 1)
  (setq u1 '(.5 0))
  (setq v1 '(1 0))
  (setq u '(0 0))
  (setq v '(0 .7 ))
  (setq u1 '(2.5 2.5 .1 .1  2.5 2.5 .1 .1 2.5 2.5 .5 .5  1  1 1.5 1.5 2 2 ))
  (setq v1 '(  0   2  2 1.5 1.5  1  1  .5 .5  .1  .1  2  2 .1 .1  2   2 .1 ))
  (setf (symbol-function 'density) #'lndf)
  (setq x (rseq 0.0005 25 300))
  (send dist-plot :title "Log-Normal distribution" )
  (send dist-plot :set-parameters par1 par2)
  (send dist-plot :location 15 20 )  ; for vga
  (send dist-plot :size 430 300 )    ; for vga
  (send dist-plot :range 0  0.0 25.0)
  (send dist-plot :range 1  0.0 .7)
  (send par-plot  :variable-label '(0 1) (list "Mu" "Sigma"))
  (send par-plot :clear-lines :draw nil)
  (send par-plot :location 450 20 )
  (send par-plot :size 180 300)
  (send par-plot :range 0  0.1 2.5)
  (send par-plot :range 1  0.1 2.0)
  (send par-plot :point-coordinate 0 0  par1)
  (send par-plot :point-coordinate 1 0  par2)
  (send par-plot :add-lines u1 v1 :type 'dashed :draw nil)
  (send par-plot :redraw))

(defun beta-density-demo ()
  (setq par1 1)
  (setq par2 1)
  (setq u '(1 1))
  (setq v '(0 10))
  (setq u1 '(5 5 .1 .1 .1 5 5 .1 .1 5 5 .1 .1  1 1 2  2  3 3 4  4))
  (setq v1 '(0 5  5  5  4 4 3  3  2 2 1  1 .1 .1 5 5 .1 .1 5 5 .1))
  (setq u2 '(0 5))
  (setq v2 '(0 5))
  (setf (symbol-function 'density) #'beta-dens)
  (setq x (rseq .0001 .9999 200))
  (send dist-plot :title "Beta distribution" )
  (send dist-plot :set-parameters par1 par2)
  (send dist-plot :location 10 20 )  ; for vga
  (send dist-plot :size 300 350 )    ; for vga
  (send dist-plot :range 0 0.0001 .9999)
  (send dist-plot :range 1  .0 10)
  (send par-plot  :variable-label '(0 1) (list "a" "b"))
  (send par-plot :clear-lines :draw nil)
  (send par-plot :location 330 20 )
  (send par-plot :size 280 350)
  (send par-plot :range 0 .1 5.0)
  (send par-plot :range 1 .1 5.0)
  (send par-plot :point-coordinate 0 0  par1)
  (send par-plot :point-coordinate 1 0  par2)
  (send par-plot :add-lines u1 v1 :type 'dashed :draw nil)
  (send par-plot :add-lines u2 v2 :type 'solid :draw nil)
  (send par-plot :redraw))

(defun gamma-density-demo ()
	(setq par1 1)
	(setq par2 1)
	(setq u '(0 0))
	(setq v '(0 1.5))
	(setq u1 '( 4 4 .5 .5 4 4 .5 .5 4 4 .5 .5 4 4 .5 .5 4 4   3 3 2  2  1 1))
	(setq v1 '(.5 8  8  7 7 6  6  5 5 4  4  3 3 2  2  1 1 .5 .5 8 8 .5 .5 8))
	(setf (symbol-function 'density) #'gammadf)
	(setq x (rseq 0.0005 7 300))
	(send dist-plot :title "Gamma-distribution")
        (send par-plot :variable-label (list 0 1) (list "Form" "1/Scale"))
	(send dist-plot :set-parameters par1 par2)
	(send dist-plot :location 15 20 )  ; for vga
	(send dist-plot :size 430 300 )    ; for vga
	(send dist-plot :range 0  0.0 7)
	(send dist-plot :range 1  0.0 1.5)
	(send par-plot :clear-lines)
	(send par-plot :location 450 20 )
	(send par-plot :size 180 300)
	(send par-plot :range 0  0.5 4.0)
	(send par-plot :range 1  0.5 8.0)
	(send par-plot :point-coordinate 0 0  par1)
	(send par-plot :point-coordinate 1 0  par2)
	(send par-plot :add-lines u1 v1 :type 'dashed)
	(send par-plot :redraw))

(setq distrib-menu (send menu-proto :new "Distribution"))

(send distrib-menu :append-items
      (send menu-item-proto :new "Normal" :action
	#'(lambda () (normal-density-demo)))
      (send menu-item-proto :new "Log-Normal" :action
	#'(lambda () (log-normal-density-demo)))
      (send menu-item-proto :new "Beta" :action
	#'(lambda () (beta-density-demo)))
      (send menu-item-proto :new "Gamma" :action
	#'(lambda () (gamma-density-demo)))
      (send menu-item-proto :new "Remove" :action
         #'(lambda ()          
             (send distrib-menu :remove)
             (send dist-plot :remove)
             (undef '( A B BOX C D DIST-PLOT E F G H I N PAR-PLOT U U1 V V1 X Y))))
      (send menu-item-proto :new "Adjust-to-data" :action
          #'(lambda () (send dist-plot :adjust-to-data))))

(send distrib-menu :install)

(normal-density-demo)

;; put the plot in "point moving" mode
(send par-plot :mouse-mode 'point-moving)

(send dist-plot :add-subordinate par-plot)

;   EOF    --------------------------------------------------------



