; Hi all,
; you find enclosed a modified version of the 
;
; (defmeth scatterplot-proto :add-boxplot ...,
; 
; taken from the file graph2.lsp of the WXLS version 2.1R3 Alpha 6,
; for MS-Windows.
;
; The modification allows to detect outliers in univariate sample.
; The bounds low and high are now evaluated according to the rule in 
; U.R.E.D.A. (Hoaglin, Mosteller & Tukey, 1983 - Wiley) page 60. 
; The outlying points are drawn.
;
; Stefano M. Pagnotta
; Dept.of Math.Stat.
; University of Naples Federico II - Italy.


;;;;
;;;;
;;;; Boxplot  Functions
;;;; sept 28, 1994.
;;;;

(defmeth scatterplot-proto :add-boxplot (y &key (x 1.0) (width 1.0) (draw t))
  (unless (= 2 (send self :num-variables)) (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
         (half-foot (* 0.1 width))
         (fiv (fivnum y))
         (q1 (select fiv 1))
         (med (select fiv 2))
         (q3 (select fiv 3))
         (dq (* 1.5 (- q3 q1)))                    
         (low (- q1 dq))                           
         (low (min (select y (which (< low y)))))  
         (high (+ q3 dq))                          
         (high (max (select y (which (> high y)))))
         (below (which (> low y)))                 
         (lsuspect (if below (select y below)))    
         (above (which (< high y)))
         (hsuspect (if above (select y above))))
    (send self :plotline (- x half-foot) low  (+ x half-foot) low  nil)
    (send self :plotline (- x half-foot) high (+ x half-foot) high nil)
    (send self :plotline x low x q1   nil)
    (send self :plotline x q3  x high nil)
    (send self :plotline (- x half-box) q1  (+ x half-box) q1  nil)
    (send self :plotline (- x half-box) med (+ x half-box) med nil)
    (send self :plotline (- x half-box) q3  (+ x half-box) q3  nil)
    (send self :plotline (- x half-box) q1  (- x half-box) q3  nil)
    (send self :plotline (+ x half-box) q1  (+ x half-box) q3  nil)
    (if hsuspect (send self :add-points (repeat x (length hsuspect)) hsuspect))
    (if lsuspect (send self :add-points (repeat x (length lsuspect)) lsuspect))))

