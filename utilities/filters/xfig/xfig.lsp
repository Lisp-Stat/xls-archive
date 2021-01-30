;; This file contains code to convert plots derived from graph-proto
;; to XFIG 3.1
;;
;;     !!   WORKS ONLY UNDER UNIX    (since XFIG is an X11 tool)         !!
;;
;; It is written for XLISPSTAT 2.1 Release 3.44. Maybe it won't run under
;; older versions, Maybe there can be difficulties in newer versions :-)
;;
;; With this file you should have get the manual xfig.ps. 
;; Read this for further information and usage
;;
;; Author:         Bernhard Walter 
;; First version:  31.5.1995
;; modified:       
;;
;;
;; for any ideas, bugs, etc, please contact
;;
;; walter@pollux.edv.agrar.tu-muenchen.de
;;

(provide "xfig")

(defmeth graph-proto :to-xfig(&key (scale 12)            
  				   (bw nil)
				   (font 16)
				   (font-size 12)
				   (filename "plot.fig")
				   (box nil)
				   (x-zero-axis nil)
				   (y-zero-axis nil)
				   (grid nil)
				   (fig2dev nil)
				   (fig2dev-par " -Lps -c -p xxx "))
"Args: (&key (scale 12) (bw nil) 
            (font 16) (font-size 12) 
            (filename \"plot.fig\")
	    (box nil) (grid nil) (x-zero-axis nil) (y-zero-axis nil)
            (fig2dev nil) (fig2dev-switches \" -Lps -c -p xxx \"))
Parameters: 
scale:       It is  assumed that the screen has a resolution of 100 ppi. 
             XFIG 3.1 works with an internal resolution of 1200 ppi. 
             Parameter scale is used to convert from screen to XFIG 3.1 
bw:          For non-color printers set the black/white parameter bw to T.
font:        The number of font, see manual (xfig.ps) 
font-size:   It is measured as usual in pt (= 1 / 72.27 inch).
filename:    Shoud be clear :-) 
box:         Draw a box around the plot instead of the usual axes
x-zero-axis: Draw a horizontal axis at y=0.
y-zero-axis: Draw a vertical axis at x=0.
grid:        Draw a dotted grid according to the ticks of the axes are.
fig2dev:     When T fig2dev is called with scalefactor m=1.000
             When given a number s, fig2dev is called with scalefactor m=s.
fig2dev-par: The default produces portrait EPS files. See manual fig2dev.1"

  (let* ((margin   (send self :margin))                 ; get margins from frame
	 (x-margin (+ (first margin) (third margin)))   ; sum of horiz. margins
	 (y-margin (+ (second margin) (fourth margin))) ; sum of vert. margins
	 (w-size   (- (send self :size)                 ; size of window content
		      (list x-margin y-margin)))        ; raw content size
	 (x-axis   (send self :x-axis))                 ; setting x-axis
	 (y-axis   (send self :y-axis))                 ; setting y-axis
	 (x-lab    (send self :variable-label 0))       ; label of x-axis
	 (y-lab    (send self :variable-label 1))       ; label of y-axis
	 (x-range  (send self :range 0))
	 (y-range  (send self :range 1))
	 (title    (send self :title))

	 ;; factors for conversion from plot scale to XFIG scale 
	 ;; Assuming screen "shows 100 dpi", this converts to 1200 dpi of 
	 ;; XFIG 3.1, if scale is set to 12
	 (x-fact   (* scale (/ (first  w-size) (- (apply #'- x-range)))))  
      	 (y-fact   (* scale (/ (second w-size) (- (apply #'- y-range))))) 

	 (x-offset (- (* scale 100)                 ; left offset - min x value
		      (* x-fact (first x-range))))  ; = XFIG coord of x=0 
	 (y-offset (+ (* scale 100)                 ; top offset + max y value
		      (* y-fact (second y-range)))) ; = XFIG coord of y=0 

	 (vis-p   (send self :points-showing))     ; visible points
	 (points  (mapcar 
		    #'(lambda(i)           ; list of coords,color,symbol
		        (list              ; in XFIG units
			 (xfig-make-xcoord 
			   x-offset x-fact
		 	   (send self :point-coordinate 0 i)) ; x-coordinate
			 (xfig-make-ycoord
			   y-offset y-fact
		 	   (send self :point-coordinate 1 i)) ; y-coordinate
		 	 (send self :point-color i)           ; color of point
		 	 (send self :point-symbol i)          ; symbol of point
			 (send self :point-selected i)        ; point highlighted ?
		        ))
		    vis-p))

	 (l-next  (if (=  (send self :num-lines) 0)
		      nil
		      (send self 
			    :linestart-next 
			    (iseq (send self :num-lines))))) ; every NIL entry is 
				                             ; start of a new line
	 (l-ends   (when l-next (which (mapcar #'not l-next))))
	 (l-starts (when l-next
			 (select (append '(0) (1+ l-ends))  ; 0 is the first linestart
				 (iseq (length l-ends)))))    ; remove last element
				 
	 (polygons (when l-next
			 (mapcar #'(lambda(l-s l-e) ; convert connected lines
						    ; to polygons 
			       (list
				(send self :linestart-type  l-s)
				(send self :linestart-width l-s)
				(send self :linestart-color l-s)
				(mapcar #'(lambda(i)
					    (list
					     (xfig-make-xcoord 
					       x-offset x-fact
					       (send self :linestart-coordinate 0 i))
					     (xfig-make-ycoord
					       y-offset y-fact
					       (send self :linestart-coordinate 1 i))
					    ))
				        (iseq l-s l-e))
			       ))
		       l-starts l-ends)))
	 )

    (with-open-file (fig filename :direction :OUTPUT)

      ;; The XFIG 3.1 initialization lines
      (format fig "#FIG 3.1~%")
      (format fig "Landscape~%")
      (format fig "Center~%")
      (format fig "Inches~%")
      (format fig "1200 2~%")

      ;; Set all points with symbols, colors and selection
      (mapcar #'(lambda(p)
		  (let* ((color  (xfig-get-color (third p) bw))
			)
		    (case (fourth p)
			  ('DISK     (xfig-make-circle  fig (first p) (second p) 
					               32 color (fifth p)))
			  ('DOT      (xfig-make-circle  fig (first p) (second p)
						       (if (fifth p) 16 8)
					               color (fifth p)))
			  ('DIAMOND  (xfig-make-diamond fig (first p) (second p) 
					               color (fifth p)))
			  ('CROSS    (xfig-make-cross   fig (first p) (second p) 
					               color (fifth p)))
			  ('SQUARE   (xfig-make-square  fig (first p) (second p) 
					               color (fifth p)))
			  ('WEDGE1   (xfig-make-wedge1  fig (first p) (second p) 
					               color (fifth p)))
			  ('WEDGE2   (xfig-make-wedge2  fig (first p) (second p) 
					               color (fifth p)))
			  ('X        (xfig-make-x       fig (first p) (second p) 
					               color (fifth p)))
	            )))
	      points)

      ;; Draw all lines 
      (when polygons
	    (mapcar #'(lambda(p)
			(let* ((color  (xfig-get-color (third p) bw))
			       (style  (if (eq (first p) 'SOLID) 0 1))
			       )
			  (xfig-make-polyline fig (fourth p) 
					      color style
					      (second p))
			  ))
		    polygons))
      
      ;; Have to draw a box ??
      (when box 
	    (xfig-make-polyline fig
		       (list (list (xfig-make-xcoord x-offset x-fact
						     (first  x-range))
				   (xfig-make-ycoord y-offset y-fact
						     (first  y-range)))
			     (list (xfig-make-xcoord x-offset x-fact
						     (second x-range))
				   (xfig-make-ycoord y-offset y-fact
						     (first y-range)))
			     (list (xfig-make-xcoord x-offset x-fact
						     (second x-range))
				   (xfig-make-ycoord y-offset y-fact
						     (second y-range)))
			     (list (xfig-make-xcoord x-offset x-fact
						     (first  x-range))
				   (xfig-make-ycoord y-offset y-fact
						     (second y-range)))
                             (list (xfig-make-xcoord x-offset x-fact
						     (first  x-range))
				   (xfig-make-ycoord y-offset y-fact
						     (first  y-range))))
		       0 0 1 t))

      ;; if zero is in the y-range, draw an x-zero-axis, if necessary
      (when (and x-zero-axis (< (first y-range) 0) (> (second y-range) 0) )
	    (xfig-make-polyline fig 
		       (list (list (xfig-make-xcoord x-offset x-fact
						     (first  x-range))
				   (round y-offset)) 
			     (list (xfig-make-xcoord x-offset x-fact
						     (second x-range))
				   (round y-offset)))
		       0 0 1))

      ;; if zero is in the x-range, draw an y-zero-axis, if necessary
      (when (and y-zero-axis (< (first x-range) 0) (> (second x-range) 0) )
	    (xfig-make-polyline fig 
		       (list (list (round x-offset)
			           (xfig-make-ycoord y-offset y-fact
						     (first   y-range)))
			     (list (round x-offset)
			           (xfig-make-ycoord y-offset y-fact
						     (second  y-range))))
		       0 0 1))

	   
      ;; Label the x-axis, if necessary
      (when (first x-axis)
	    (when (and (not box) (not x-zero-axis))  ; when box or x-zero-axis
					             ;   don't draw x-axis
		  (xfig-make-polyline fig 
			     (list (list (xfig-make-xcoord x-offset x-fact
							   (first  x-range))
					 (xfig-make-ycoord y-offset y-fact
							   (first y-range)))
				   (list (xfig-make-xcoord x-offset x-fact
							   (second x-range))
					 (xfig-make-ycoord y-offset y-fact
							   (first y-range))))
			     0 0 1))
	    (when (not (= (third x-axis) 0))
		  (mapcar #'(lambda(xi)
			      (let* ((xcoord  (xfig-make-xcoord x-offset x-fact xi))
				     (ycoord1 (xfig-make-ycoord y-offset y-fact 
							   (first y-range)))
				     (ycoord2 (xfig-make-ycoord y-offset y-fact 
							   (second y-range))))
			      (xfig-make-polyline fig 
					     (list (list xcoord ycoord1)
						   (list xcoord (+ ycoord1 80)))
					     0 0 1)
			      (when box
				    (xfig-make-polyline fig 
					       (list (list xcoord ycoord2)
						     (list xcoord (- ycoord2 80)))
					       0 0 1))
			      (when grid
				    (xfig-make-polyline fig 
					       (list (list xcoord ycoord1)
						     (list xcoord ycoord2))
					       0 2 1))
			      (xfig-make-text fig xcoord 
					 (+ ycoord1 120
					    (round 
					      (* 1200 (/ font-size 72.27))))
    					           ; pt to XFIG units
					 1 0.0 
					 (format nil "~6,3f" xi)
					 font font-size))
			    )
			  (rseq (first x-range) (second x-range) (third
								  x-axis))))
	    (when (and (second x-axis) (/= (length x-lab) 0))
		  (xfig-make-text fig (xfig-make-xcoord x-offset x-fact
					      (/ (apply #'+ x-range) 2))
			         (+ (xfig-make-ycoord y-offset y-fact 
						      (first y-range))
				    160
				    (round (* 2 1200 (/ font-size 72.27))))
				 1 0.0
				 (format nil "~a" x-lab)
				 font font-size))
      )

      ;; Label the y-axis, if necessary
      (when (first y-axis)
	    (when (and (not box) (not y-zero-axis))  ; when box or y-zero-axis
					             ;   don't draw y-axis
		  (xfig-make-polyline fig 
			     (list (list (xfig-make-xcoord x-offset x-fact
							   (first  x-range))
					 (xfig-make-ycoord y-offset y-fact
							   (first  y-range)))
				   (list (xfig-make-xcoord x-offset x-fact
							   (first  x-range)) 
				       (xfig-make-ycoord y-offset y-fact
							 (second y-range))))
			     0 0 1))
	    (when (not (= (third y-axis) 0))
		  (mapcar #'(lambda(yi)
			      (let* ((xcoord1 (xfig-make-xcoord x-offset x-fact
				                          (first x-range)))
			             (xcoord2 (xfig-make-xcoord x-offset x-fact
				                          (second x-range)))
				     (ycoord (xfig-make-ycoord y-offset y-fact yi)))
							  
			      (xfig-make-polyline fig 
					     (list (list xcoord1 ycoord)
						   (list (- xcoord1 80) ycoord))
					     0 0 1)
			      (when box
				    (xfig-make-polyline fig 
					       (list (list xcoord2 ycoord)
						     (list (+ xcoord2 80) ycoord 80))
					       0 0 1))
			      (when grid
				    (xfig-make-polyline fig 
					       (list (list xcoord1 ycoord)
						     (list xcoord2 ycoord))
					       0 2 1))
			      (xfig-make-text fig (- xcoord1 80
						(round 
						 (* 1200 (/ font-size 72.27))))
					         ; pt to XFIG units 
					     ycoord 1 (/ pi 2) 
					     (format nil "~6,3f" yi)
					     font font-size))
			    )
			  (rseq (first y-range) (second y-range) (third y-axis))))
	    (when (and (second y-axis) (/= (length y-lab) 0))
		  (xfig-make-text fig (- (xfig-make-xcoord x-offset x-fact 
							   (first x-range))
				    160
				    (round (* 2 1200 (/ font-size 72.27))))
  			         (xfig-make-ycoord y-offset y-fact
					      (/ (apply #'+ y-range) 2))
				 1 (/ pi 2)
				 (format nil "~a" y-lab)
				 font font-size))
       )
      (when (send self :showing-labels)
	    (let* ((showing (send self :selection)))
	      (mapcar 
	        #'(lambda(p)
		     (xfig-make-text fig 
				(+ 40 (xfig-make-xcoord 
				        x-offset x-fact 
					(send self :point-coordinate 0 p)))
				(- (xfig-make-ycoord 
				     y-offset y-fact
				     (send self :point-coordinate 1 p))
				   40)
				0 0.0 (format nil "~a" (send self :point-label p)) 
				font 10))
		showing)))

      ;; Put title on plot
      (when (/= (length title) 0)
	    (xfig-make-text fig (xfig-make-xcoord x-offset x-fact
					(/ (apply #'+ x-range) 2))
		       (- (xfig-make-ycoord y-offset y-fact (second y-range)) 400)
		       1 0.0
		       (format nil "~a" title)
		       font (* 2 font-size)))
      )

      ;; Call fig2dev if necessary
      (when fig2dev
	    (if (numberp fig2dev) 
		(system (format nil "fig2dev ~a -m ~f ~a ~a.eps" 
			            fig2dev-par fig2dev filename filename))
		(system (format nil "fig2dev ~a -m 1.0000 ~a ~a.eps" 
			            fig2dev-par filename filename))))
    nil
    ))
		 
(defun xfig-make-xcoord (x-offset x-fact x)
"Not for standalone use"
  (round (+ x-offset (* x-fact x))))

(defun xfig-make-ycoord (y-offset y-fact y)
"Not for standalone use"
  (round (- y-offset (* y-fact y))))

(defun xfig-get-color (c &optional (bw nil))
"Not for standalone use"
  (if bw 
      0         ; BLACK
      (case c
	    (NIL -1)
	    ('WHITE 7)
	    ('BLACK 0)
	    ('RED 4)
	    ('GREEN 2)
	    ('BLUE 1)
	    ('CYAN 3)
	    ('MAGENTA 5)
	    ('YELLOW 6)
	    )))

(defun xfig-make-polyline (file points color style width 
			   &optional (polygon nil)
			             (fill    nil))
"Not for standalone use"
  (if polygon
      (format file "2 3 ")                  ; polygon
      (format file "2 1 "))                 ; polyline
  (format file "~2d " style)                ; line style
  (format file "~2d " width)                ; thickness
  (format file "~2d " color)                ; pen-color
  (format file "~2d " color)                ; fill-color
  (format file "1 ")                        ; depth
  (format file "0 ")                        ; pen-style
  (format file "~2d " (if fill 20 -1))      ; area fill, not filled
  (format file "~9,1f " 6.0)                ; style val
  (format file "0 ")                        ; join style
  (format file "0 ")                        ; cap_style
  (format file "0 ")                        ; radius
  (format file "0 ")                        ; forward arrow
  (format file "0 ")                        ; backward arrow
  (format file "~4d~% " (length points))    ; number of points
  (format file "         ")
  (mapcar #'(lambda (p)
	      (format file "~6d" (first  p))
	      (format file "~6d" (second p)))
	  points)
  (format file "~%")
)

(defun xfig-make-circle (file x y rad color sel)
"Not for standalone use"
  (format file "1 4 ")                     ; circle defined by diameter (1 4)
  (format file "~2d " 0)                   ; solid  
  (format file "~2d " 1)                   ; thickness 
  (format file "~2d " color)               ; pen-color
  (format file "~2d " color)               ;  fill-color, not filled
  (format file "0 ")                       ; depth
  (format file "0 ")                       ; pen-style
  (format file "~2d " (if sel 20 -1))      ; area fill, not filled
  (format file "~9,1f " 0.0)               ; style val
  (format file "1 ")                       ; direction
  (format file "~9,1f " 0.0)               ; angle
  (format file "~6d " x)                   ; center_x
  (format file "~6d " y)                   ; center_y
  (format file "~6d ~6d " rad rad)         ; radius_x, radius_y
  (format file "~6d "  (- x rad))          ; start_x
  (format file "~6d "  y)                  ; starty_y
  (format file "~6d "  (+ x rad))          ; end_x
  (format file "~6d~%" y)                  ; end_y
)

(defun xfig-make-text (file x y type angle text &optional 
			    (font 16) (font-size 12))
"Not for standalone use"
  (format file "4 ")                       ; text
  (format file "~2d " type)                ; 0=left, 1=center, 2=right
  (format file "0 ")                       ; color = BLACK
  (format file "0 ")                       ; depth
  (format file "0 ")                       ; penstyle
  (format file "~2d " font)                ; default font: PS Helvetica
  (format file "~2d " font-size)           ; default size: 12 pt
  (format file "~6,2f " angle)             ; angle
  (format file "4 ")                       ; use Postscript fonts
  (format file "0 0 ")                     ; ??? don't know what for ???
  (format file "~6d ~6d " x y)             ; coordinates of text
  (format file "~a\\001~%" text)            ; text to draw
)


(defun xfig-make-diamond (file x y color sel)
"Not for standalone use"
  (let* ((size 32))
    (xfig-make-polyline file 
		   (list (list (- x size) y       ) 
			 (list x          (- y size)) 
			 (list (+ x size) y       ) 
			 (list x          (+ y size))
			 (list (- x size) y       ) 
			 )
		   color 0 1 t (if sel t nil))
  ))

(defun xfig-make-cross (file x y color sel)
"Not for standalone use"
  (let* ((size 32))
    (xfig-make-polyline file 
		   (list (list (- x size) y) 
			 (list (+ x size) y) 
			 )
		   color 0 (if sel 2 1))
    (xfig-make-polyline file 
		   (list (list x (+ y size)) 
			 (list x (- y size)) 
			 )
		   color 0 (if sel 2 1))
    ))

(defun xfig-make-square (file x y color sel)
"Not for standalone use"
  (let* ((size 20))
    (xfig-make-polyline file 
		   (list (list (- x size) (- y size)) 
			 (list (- x size) (+ y size)) 
			 (list (+ x size) (+ y size)) 
			 (list (+ x size) (- y size)) 
			 (list (- x size) (- y size)) 
			 )
		   color 0 1 t (if sel t nil))
    ))

(defun xfig-make-wedge2 (file x y color sel)
"Not for standalone use"
  (let* ((up    32)
	 (down  24)
	 (long  32)
	 (short 20))
    (xfig-make-polyline file 
		   (list (list    x        (- y up)  )
			 (list (- x long)  y         )
			 (list (- x short) (+ y down))
			 (list (+ x short) (+ y down))
			 (list (+ x long ) y         )
			 (list    x        (- y up)  )
			 )
		   color 0 1 t (if sel t nil))
    ))


(defun xfig-make-wedge1 (file x y color sel)
"Not for standalone use"
  (let* ((up    32)
	 (down  24)
	 (long  32)
	 (short 20))
    (xfig-make-polyline file 
		   (list (list    x        (+ y up)  )
			 (list (- x long)  y         )
			 (list (- x short) (- y down))
			 (list (+ x short) (- y down))
			 (list (+ x long ) y         )
			 (list    x        (+ y up)  )
			 )
		   color 0 1 t (if sel t nil))
    ))

(defun xfig-make-x (file x y color sel)
"Not for standalone use"
  (let* ((size 32))
    (xfig-make-polyline file 
		   (list (list (- x size) (+ y size)) 
			 (list (+ x size) (- y size)) 
			 )
		   color 0 (if sel 2 1))
    (xfig-make-polyline file 
		   (list (list (- x size) (- y size)) 
			 (list (+ x size) (+ y size)) 
			 )
		   color 0 (if sel 2 1))
    ))



