;; 
;; add methods for drawing a colored grid and/or x,y = zero axis to
;; scatterplot proto. 
;; If GRID-COLOR is set, then :DRAW-BACKGROUND will draw the grid. 
;; If you want the zero's to be hilited with a different color, then
;; also set :AXIS-COLOR. 
;;
;; Future project: Change the default axis drawing routines to draw
;; disconnected axis for non zero based ranges. ( as in E. Tufte ) 
;;

;; See mods of scatterplot-proto's  :BACK-COLOR in xlinkable.lsp & 
;;  :ISNEW in realgraph.lsp. They redefine the behaviour to make setting
;; defaults easier, but I'm not sure that they should be a 'REQUIRE' 


;; realgraf and some others may also use this same redefinition
;; need to make clear-content NULL so that it doesn't erase grid.

( defmeth scatterplot-proto :clear-content ())
( send scatterplot-proto :add-slot 'grid-color nil )
( send scatterplot-proto :add-slot 'axis-color nil )


( defmeth scatterplot-proto :draw-grid ( &key color )
  ( let ((x-range (send self :range 0))
         (y-range (send self :range 1))
         (draw-c  (send self :draw-color))
         (lwidth  (send self :line-width))
         (ltype   (send self :line-type)))
    (send self :line-width 1)
    (send self :line-type 'DASHED)
    (send self :draw-color (or color color draw-c))
    ( dolist ( x ( apply #'rseq 
                         (append x-range 
                                 ( last ( send self :x-axis )))))
             ( apply #'send self :draw-line 
                     ( append 
                       ( send self :real-to-canvas  x ( car y-range ))
                       ( send self :real-to-canvas  x ( cadr y-range )))))

       ( dolist ( y ( apply #'rseq 
                         (append y-range 
                                 ( last ( send self :y-axis )))))
             ( apply #'send self :draw-line 
                     ( append 
                       ( send self :real-to-canvas ( car x-range ) y )
                       ( send self :real-to-canvas ( cadr x-range) y ))))
    (send self :draw-color draw-c)
    (send self :line-width lwidth)
    (send self :line-type ltype)))


( defmeth scatterplot-proto :draw-zero-axis ( &key color )
  ( let ((x-range (send self :range 0))
         (y-range (send self :range 1))
         (draw-c  (send self :draw-color))
         (lwidth  (send self :line-width))
         (ltype   (send self :line-type)))
    (send self :line-type 'DASHED)
    (send self :line-width 2)
    (send self :draw-color ( or color draw-c ))
  	(apply #'send self :draw-line 
          ( append ( send self :real-to-canvas 0 ( car y-range ))
                   ( send self :real-to-canvas 0 ( cadr y-range ))))
  	(apply #'send self :draw-line
          ( append ( send self :real-to-canvas (car x-range) 0 )
                   ( send self :real-to-canvas (cadr x-range) 0 )))
  	(send self :draw-color draw-c)
  	(send self :line-width lwidth)
  	(send self :line-type ltype)))
 

( defmeth scatterplot-proto :grid-color ( &optional (color nil set))
  (when set
	(if (not (send self :has-slot 'grid-color :own T))
       ( send self :add-slot 'grid-color color ))
    (send self :slot-value 'grid-color color))
  (slot-value 'grid-color))
       

( defmeth scatterplot-proto :axis-color ( &optional (color nil set))
  (when set
	(if (not (send self :has-slot 'axis-color :own T))
       ( send self :add-slot 'axis-color color ))
    (send self :slot-value 'axis-color color))
  (slot-value 'axis-color))


 ( defmeth scatterplot-proto :redraw-background ()
   (call-next-method)
   (let ((c (send self :grid-color)))
   	   (when c 
   	   	(send self :draw-grid :color c )
   	   	(send self :draw-zero-axis :color c))))
   	
   	   


   
                     