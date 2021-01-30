#|

blin-mpp.lsp

(C) Darren J Wilkinson

This file may be distributed in unmodified form, and may be modified
for personal use. It may not be distributed in modified form.
Suggested modifications should be sent to me for inclusion in the
next release.

d.j.wilkinson@newcastle.ac.uk
http://www.ncl.ac.uk/~ndjw1/

|#

(provide "blin-mpp")

;; define the moral graph plotting prototype and methods

;; define prototype
(defproto moral-plot-proto
  '(nodes real-size radius diagnostics node-labels outlines)
  nil
  graph-window-proto)

;; now some methods

;; resize method (message sent by windowing system)
(defmeth moral-plot-proto :resize nil
"Method args: nil
Resets real-size and radius slot-values."
  (slot-value 'real-size
	      (min (send self :size)))
  (slot-value 'radius
	      (round (/ (slot-value 'real-size) (* 4 (sqrt (length (send self :slot-value 'nodes)))))))
)

;; scaling transformation methods
(defmeth moral-plot-proto :s-to-r (coords)
"Method args: (coords)
Takes zero-one coords to integer coords for plotting."
(round (* coords (slot-value 'real-size)))
)

(defmeth moral-plot-proto :r-to-s (coords)
"Method args: (coords)
Takes integer plotting coords to zero-one coords."
(/ coords (slot-value 'real-size))
)

;; redraw method
(defmeth moral-plot-proto :redraw nil
"Method args: nil
Redraws the moral graph plot."

(defvar *eps-stream*)
(with-open-file (*eps-stream* "mpw.eps" :direction :output)

(send self :erase-window)
(dolist (node (slot-value 'nodes))
  (send self :plot-arcs node)
  )
(dolist (node (slot-value 'nodes))
  (send self :plot-node node)
  )

(format *eps-stream* "
$BlinEnd
rs
")

)

)

;; arc-plotting method
(defmeth moral-plot-proto :plot-arcs (node)
"Method args: (node)
Draws the arcs for the given node."
(send self :draw-color 'arc)
(send self :line-width 2)
(let* ((s1 (send (symbol-value node) :location))
       (r1 (send self :s-to-r s1)))
  (dolist (node2 (send (symbol-value node) :slot-value 'neighbours))
    (let* ((s2 (send (symbol-value node2) :location))
	   (r2 (send self :s-to-r s2)))
      (send self :draw-line (first r1) (second r1) (first r2) (second r2))
      )
    )
  )
)

;; a diagnostics accessor method
(defmeth moral-plot-proto :diagnostics (&optional (d -1))
"Method args: (&optional (d nil))
Sets or returns node diagnostics status."
  (if (eq d -1)
      (slot-value 'diagnostics)
    (let ()
      (slot-value 'diagnostics d)
      (send self :redraw)
    ))
)

;; a node-labels accessor method
(defmeth moral-plot-proto :node-labels (&optional (n -1))
"Method args: (&optional (n nil))
Sets or returns node label status."
  (if (eq n -1)
      (slot-value 'node-labels)
    (let ()
      (slot-value 'node-labels n)
      (send self :redraw)
    ))
)

;; an outlines accessor method
(defmeth moral-plot-proto :outlines (&optional (n -1))
"Method args: (&optional (n nil))
Sets or returns node outline status."
  (if (eq n -1)
      (slot-value 'outlines)
    (let ()
      (slot-value 'outlines n)
      (send self :redraw)
    ))
)

;; node-plotting method
(defmeth moral-plot-proto :plot-node (node)
"Method args: (node)
Draws the given node."
(let* ((sloc (send (symbol-value node) :location))
       (rloc (send self :s-to-r sloc))
       (x (first rloc))
       (y (second rloc))
       (rad (send self :slot-value 'radius))
       (creslist (sqrt 
		  (map-elements #'max
		  (- 1 (cumsum (reverse 
		     (send (symbol-value node) :slot-value 'resolutions))))
		  0)))
       (srlist (reverse (send (symbol-value node) :slot-value 'size-ratios)))
       )
  ;; first the basic node outline
  (send self :draw-color (intern (format nil "~a" 0)))
  (send self :paint-oval (- x rad) (- y rad) (* 2 rad) (* 2 rad))
  ;; fill in outer diagnostic shadings
  (if (send self :diagnostics)
      (let* ((thisdiag (send self :sr-map (first srlist)))
	 (thisangle (round (* 180 thisdiag)))
	 )
          (when (> thisangle 0)
	  ;; red shading
	   (send self :draw-color 'red)
	   (send self :paint-arc (- x rad) (- y rad)
		 (* 2 rad) (* 2 rad) 90 thisangle)
	   )
	;; blue shading
	(when (<= thisangle 0)
	 (send self :draw-color 'blue)
	 (send self :paint-arc (- x rad) (- y rad)
	       (* 2 rad) (* 2 rad) 90 thisangle)
	 )
    ))
  ;; now outline
  (if (send self :outlines)
      (list
  (send self :line-width 1)
  (send self :draw-color 'outline)
  (send self :frame-oval (- x rad) (- y rad) (* 2 rad) (* 2 rad))
  ))
  ;; loop through the resolutions
  (dotimes (i (- (length creslist) 1))
;    (print (select creslist i))
    (let* ((thisrad (round (* rad (select creslist i))))
	   (thisdiag (send self :sr-map (select srlist (+ i 1))))
	   (thisangle (round (* 180 thisdiag)))
	   )
      ;; first shade the region
      (send self :draw-color (intern (format nil "~a" (+ i 1))))
      (send self :paint-oval (- x thisrad) (- y thisrad)
	    (* 2 thisrad) (* 2 thisrad))
      ;; next the diagnostics
      (if (send self :diagnostics)
	  (let ()
	  (when (> thisangle 0)
	  ;; red shading
	   (send self :draw-color 'red)
	   (send self :paint-arc (- x thisrad) (- y thisrad)
		 (* 2 thisrad) (* 2 thisrad) 90 thisangle)
	   )
	;; blue shading
	(when (<= thisangle 0)
	 (send self :draw-color 'blue)
	 (send self :paint-arc (- x thisrad) (- y thisrad)
	       (* 2 thisrad) (* 2 thisrad) 90 thisangle)
	 )))
      ;; now the outlines
      (if (send self :outlines)
	  (list
      (send self :draw-color 'outline)
      (send self :frame-oval (- x thisrad) (- y thisrad)
	    (* 2 thisrad) (* 2 thisrad))
      ))
    )
    )
  ;; fill in unresolved
  (let* ((thisrad (round (* rad (first (reverse creslist))))))
      (send self :draw-color 'node)
      (send self :paint-oval (- x thisrad) (- y thisrad)
	    (* 2 thisrad) (* 2 thisrad))
;      (send self :draw-color 'outline)
;      (send self :frame-oval (- x thisrad) (- y thisrad)
;	    (* 2 thisrad) (* 2 thisrad))
    )
  (send self :draw-color 'outline)
  (if (send self :node-labels)
      (send self :draw-text
	    (send (symbol-value node) :print-name)
	    x (+ y rad) 1 1)
  ))
)

;; size-ratio conversion method
(defmeth moral-plot-proto :sr-map (sr)
"Method args: (sr)
Method which maps a size-ratio (0,Infinity) onto (-1,1) for use
with diagnostic shadings. It is monotonic and maps 1 to 0.
You should feel free to re-define this method as long as it
retains those properties."
;	(/
;	 (- sr 1)
;	 (+ sr 1)
;	 )
(- (* 2 (chisq-cdf sr 1)) 1)
)

;; info grabbing method
(defmeth moral-plot-proto :record ()
"Method args: ()
Send this message to the plot after an :OBSERVE and before an
:ABSORB in order to have resolution and size-ratio information
stored in the moral graph nodes to be used by the plot.
Returns nil."
(dolist (node (send self :slot-value 'nodes))
;; first grab resolution
  (send (symbol-value node) :slot-value 'resolutions
     (cons (send (symbol-value node) :resolution)
	   (send (symbol-value node) :slot-value 'resolutions)))
;; now grab size-ratio
  (send (symbol-value node) :slot-value 'size-ratios
	(cons (send (symbol-value node) :size-ratio)
	      (send (symbol-value node) :slot-value 'size-ratios)))
;; now grab global size-ratio
  (send (symbol-value node) :slot-value 'global-size-ratios
	(cons (send (symbol-value node) :global-size-ratio)
	      (send (symbol-value node) :slot-value 'global-size-ratios)))
  )
;; now redraw the plot
(send self :resize)
(send self :redraw)
nil
)

;; ***********************************************************

;; global plotting stuff


;; define prototype
(defproto global-moral-plot-proto
  nil
  nil
  moral-plot-proto)


;; redraw method
(defmeth global-moral-plot-proto :redraw nil
"Method args: nil
Redraws the moral graph plot."

(defvar *eps-stream*)
(with-open-file (*eps-stream* "gmpw.eps" :direction :output)

(send self :erase-window)
(dolist (node (slot-value 'nodes))
  (send self :plot-arcs node)
  )
(dolist (node (slot-value 'nodes))
  (send self :plot-node node)
  )

(format *eps-stream* "
$BlinEnd
rs
")

)



)


;; node-plotting method
(defmeth global-moral-plot-proto :plot-node (node)
"Method args: (node)
Draws the given node."
(let* ((sloc (send (symbol-value node) :location))
       (rloc (send self :s-to-r sloc))
       (x (first rloc))
       (y (second rloc))
       (rad (send self :slot-value 'radius))
       (creslist (sqrt 
		  (map-elements #'max
		  (- 1 (cumsum (reverse 
		     (send (symbol-value node) :slot-value 'resolutions))))
		  0)))
       )
  ;; first the basic node outline
  (send self :draw-color 'node-back)
  (send self :paint-oval (- x rad) (- y rad) (* 2 rad) (* 2 rad))
  ;; fill in outer diagnostic shadings
  (if (send self :diagnostics)
      (let* ((thisdiag (send self :sr-map 
	      (first (send (symbol-value node) :slot-value 'global-size-ratios))
 ))
	 (thisangle (round (* 180 thisdiag)))
	 )
          (when (> thisangle 0)
	  ;; red shading
	   (send self :draw-color 'red)
	   (send self :paint-arc (- x rad) (- y rad)
		 (* 2 rad) (* 2 rad) 90 thisangle)
	   )
	;; blue shading
	(when (<= thisangle 0)
	 (send self :draw-color 'blue)
	 (send self :paint-arc (- x rad) (- y rad)
	       (* 2 rad) (* 2 rad) 90 thisangle)
	 )
    ))
  ;; fill in unresolved
  (let* ((thisrad (round (* rad (first (reverse creslist))))))
      (send self :draw-color 'node)
      (send self :paint-oval (- x thisrad) (- y thisrad)
	    (* 2 thisrad) (* 2 thisrad))
    )
  (send self :draw-color 'outline)
  (if (send self :node-labels)
      (send self :draw-text
	    (send (symbol-value node) :print-name)
	    x (+ y rad) 1 1)
  ))
)










;; **********************************************************


;; test hacks for a postscript driver!

(defmeth moral-plot-proto :erase-window ()
  (format *eps-stream* "~&%!PS-Adobe-3.0 EPSF-3.0")
  (format *eps-stream* "~&%%BoundingBox: 0 0 ~a ~a"
	  (send self :slot-value 'real-size)
	  (send self :slot-value 'real-size)
)
  (format *eps-stream* "~&%%Creator: BAYES-LIN
%%Orientation: Portrait
%%Pages: 0
%%EndComments

/$BlinDict 200 dict def
$BlinDict begin
$BlinDict /mtrx matrix put

end
save")

(format *eps-stream* "~&0 ~a translate" (send self :slot-value 'real-size))

(format *eps-stream* "
1 -1 scale

/cp {closepath} bind def
/ef {eofill} bind def
/gr {grestore} bind def
/gs {gsave} bind def
/sa {save} bind def
/rs {restore} bind def
/l {lineto} bind def
/m {moveto} bind def
/rm {rmoveto} bind def
/n {newpath} bind def
/s {stroke} bind def
/sh {show} bind def
/slc {setlinecap} bind def
/slj {setlinejoin} bind def
/slw {setlinewidth} bind def
/srgb {setrgbcolor} bind def
/rot {rotate} bind def
/sc {scale} bind def
/sd {setdash} bind def
/ff {findfont} bind def
/sf {setfont} bind def
/scf {scalefont} bind def
/sw {stringwidth} bind def
/tr {translate} bind def
/tnt {dup dup currentrgbcolor
  4 -2 roll dup 1 exch sub 3 -1 roll mul add
  4 -2 roll dup 1 exch sub 3 -1 roll mul add
  4 -2 roll dup 1 exch sub 3 -1 roll mul add srgb}
  bind def
/shd {dup dup currentrgbcolor 4 -2 roll mul 4 -2 roll mul
  4 -2 roll mul srgb} bind def
 /DrawEllipse {
        /endangle exch def
        /startangle exch def
        /yrad exch def
        /xrad exch def
        /y exch def
        /x exch def
        /savematrix mtrx currentmatrix def
        x y tr xrad yrad sc 0 0 1 startangle endangle arc
        closepath
        savematrix setmatrix
        } def

/$BlinBegin {$BlinDict begin /$BlinEnteredState save def} def
/$BlinEnd {$BlinEnteredState restore end} def
%%EndProlog

$BlinBegin

/dl {n m l s} bind def
/sg {setgray} bind def
/f {fill} bind def

% /ArialMT findfont 14 scalefont setfont
/Helvetica-Bold findfont 16 scalefont setfont

")

  (call-next-method)
)

(defmeth moral-plot-proto :draw-line (x1 y1 x2 y2)
  (format *eps-stream* "~&% :draw-line ~a ~a ~a ~a" x1 y1 x2 y2)
  (format *eps-stream* "~&~a ~a ~a ~a dl"
	  x1 y1
	  x2 y2)
  (call-next-method x1 y1 x2 y2)
)

(defmeth moral-plot-proto :paint-oval (x1 y1 x2 y2)
  (format *eps-stream* "~&% :paint-oval ~a ~a ~a ~a" x1 y1 x2 y2)
  (format *eps-stream* "~&n ~a ~a ~a ~a ~a ~a DrawEllipse f"
	  (+ x1 (round (/ x2 2))) (+ y1 (round (/ y2 2)))
	  (round (/ x2 2)) (round (/ y2 2)) 0 360)
  (call-next-method x1 y1 x2 y2)
)

(defmeth moral-plot-proto :paint-arc (x1 y1 x2 y2 a1 a2)
  (format *eps-stream* "~&% :paint-arc ~a ~a ~a ~a ~a ~a" x1 y1 x2 y2 a1 a2)
  (format *eps-stream* "~&n ~a ~a m"
	  (+ x1 (round (/ x2 2))) (+ y1 (round (/ y2 2))))
  (if (> a2 0)
      (format *eps-stream* "~&~a ~a ~a ~a ~a ~a DrawEllipse f"
	      (+ x1 (round (/ x2 2))) (+ y1 (round (/ y2 2)))
	      (round (/ x2 2)) (round (/ y2 2)) (+ 180 (- a1 a2)) (+ 180 a1))
    (format *eps-stream* "~&~a ~a ~a ~a ~a ~a DrawEllipse f"
	    (+ x1 (round (/ x2 2))) (+ y1 (round (/ y2 2)))
	    (round (/ x2 2)) (round (/ y2 2)) (+ 180 a1) (+ 180 (- a1 a2)))
    )
  (call-next-method x1 y1 x2 y2 a1 a2)
)

(defmeth moral-plot-proto :frame-oval (x1 y1 x2 y2)
  (format *eps-stream* "~&% :frame-oval ~a ~a ~a ~a" x1 y1 x2 y2)
  (format *eps-stream* "~&n ~a ~a ~a ~a ~a ~a DrawEllipse s"
	  (+ x1 (round (/ x2 2))) (+ y1 (round (/ y2 2)))
	  (round (/ x2 2)) (round (/ y2 2)) 0 360)
  (call-next-method x1 y1 x2 y2)
)

(defmeth moral-plot-proto :draw-string (str x y)
  (format *eps-stream* "~&% :draw-string ~a ~a ~a" str x y)
  (format *eps-stream* "~&n ~a ~a m (~a) show" x y str)
  (call-next-method str x y)
)

(defmeth moral-plot-proto :draw-text (str x y jus bel)
  (format *eps-stream* "~&% :draw-string ~a ~a ~a" str x y)
  (let ((xx x) (yy y))
    (case jus
      (1 (setf xx (- x (round (/ (send self :text-width str) 2)))))
      (2 (setf xx (- x (send self :text-width str))))
      )
    (if (= bel 1)
	(setf yy (+ y (send self :text-ascent)))
	)
    (format *eps-stream* "~&n ~a ~a m gs 1 -1 sc (~a) sh gr" xx yy str)
    )
  (call-next-method str x y jus bel)
)

(defmeth moral-plot-proto :line-width (&optional (w nil))
  (if w
      (list
       (format *eps-stream* "~&% :line-width ~a" w)
       (format *eps-stream* "~&~a slw" w)
       )
    )
  (if w
      (call-next-method w)
    (call-next-method)
    )
)

(defmeth moral-plot-proto :gray-draw-color (&optional (c nil))
  (if c
      (list
       (format *eps-stream* "~&% :draw-color ~a" c)
       (case c
	 (arc (format *eps-stream* "~&0.8 sg"))
	 (node (format *eps-stream* "~&0.1 sg"))
	 (node-back (format *eps-stream* "~&0.9 sg"))
	 (outline (format *eps-stream* "~&0.0 sg"))
	 (red (format *eps-stream* "~&0.2 sg"))
	 (blue (format *eps-stream* "~&0.5 sg"))
	 (\0 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 16 256))))
	 (\1 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 32 256))))
	 (\2 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 8  256))))
	 (\3 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 48 256))))
	 (\4 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 24 256))))
	 (\5 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 56 256))))
	 (\6 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 40 256))))
	 (\7 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 0  256))))
	 (\8 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 36 256))))
	 (\9 (format *eps-stream* "~&~a sg" (+ 0.75 (/ 12 256))))
	 )
       )
    )
  (if c
      (call-next-method c)
    (call-next-method)
    )
)

(defmeth moral-plot-proto :draw-color (&optional (c nil))
  (if c
      (list
       (format *eps-stream* "~&% :draw-color ~a" c)
       (case c
	 (arc (format *eps-stream* "~&0.8 sg"))
	 (node (format *eps-stream* "~&0.1 sg"))
	 (node-back (format *eps-stream* "~&0.9 0.9 0.9 srgb"))
	 (outline (format *eps-stream* "~&0 0 0 srgb"))
	 (red (format *eps-stream* "~&1 0 0 srgb"))
	 (blue (format *eps-stream* "~&0 0 1 srgb"))
	 (\0 (format *eps-stream* "~&0.8 0.8 1 srgb"))
	 (\1 (format *eps-stream* "~&1 0.8 0.8 srgb"))
	 (\2 (format *eps-stream* "~&0.8 1 0.8 srgb"))
	 (\3 (format *eps-stream* "~&1 1 0.7 srgb"))
	 (\4 (format *eps-stream* "~&1 0.7 1 srgb"))
	 (\5 (format *eps-stream* "~&0.7 1 1 srgb"))
	 (\6 (format *eps-stream* "~&1 0.8 0.8 srgb"))
	 (\7 (format *eps-stream* "~&1 0.8 0.8 srgb"))
	 (\8 (format *eps-stream* "~&1 0.8 0.8 srgb"))
	 (\9 (format *eps-stream* "~&1 0.8 0.8 srgb"))
	 (t  (format *eps-stream* "~&0.6 0.6 0.6 srgb"))	 
	 )
       )
    )
  (if c
      (call-next-method c)
    (call-next-method)
    )
)




;; end

