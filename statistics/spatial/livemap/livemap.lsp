(provide "Map Protos")
;;;
;;;  To kick off with,  we need a few spatial data types,  and some functions
;;; 
(defstruct zones
           coords
           values) 

(defstruct lines
           coords
           values)

(defstruct points
           coords
           values)

;;;
;;;   These functions read in the above data types from Arc/Info
;;;   UNGENERATED files
;;;

(defun read-points (file-name)
"Args: (file-name) 
Returns the point structure for FILE-NAME, an Arc/Info ungenerated point coverage." 
  (let
    ((file (open file-name :direction :input))
     (coord nil)
     (value nil)
     (inline nil))
    (loop
     (setf inline (read-line file))
     (if (equal 'END (read-from-string inline)) (return))
     (setf inline (read-from-string (format nil "(~a)" inline)))
     (setf coord (cons (select inline '(1 2)) coord))
     (setf value (cons (nth 0 inline) value)))
    (close file)
    (make-points :coords coord :values value)))

(defun read-zones (file-name)
"Args: (file-name) 
Returns the zone structure for FILE-NAME, an Arc/Info ungenerated polygon coverage." 
  (let
    ((file (open file-name :direction :input))
     (coord nil)
     (value nil)
     (inline nil)
     (curline nil))
    (loop
     (setf inline (read-line file))
     (if (equal 'END (read-from-string inline)) (return))
     (setf value (cons (read-from-string inline) value))
     (setf curline nil)
     (loop
      (setf inline (read-line file))
      (if (equal 'END (read-from-string inline)) (return))
      (setf inline (read-from-string (format nil "(~a)" inline)))
      (setf curline (cons inline curline)))
     (setf coord (cons curline coord)))
    (close file)
    (make-zones :coords (mapcar #'transpose coord) :values value)))

(defun read-lines (file-name)
"Args: (file-name) 
Returns the line structure for FILE-NAME, an Arc/Info ungenerated arc coverage." 
  (let
    ((file (open file-name :direction :input))
     (coord nil)
     (value nil)
     (inline nil)
     (curline nil))
    (loop
     (setf inline (read-line file))
     (if (equal 'END (read-from-string inline)) (return))
     (setf value (cons (read-from-string inline) value))
     (setf curline nil)
     (loop
      (setf inline (read-line file))
      (if (equal 'END (read-from-string inline)) (return))
      (setf inline (read-from-string (format nil "(~a)" inline)))
      (setf curline (cons inline curline)))
     (setf coord (cons curline coord)))
    (close file)
    (make-lines :coords (mapcar #'transpose coord) :values value)))

;;;
;;;  These ones do things with them...
;;;

(defun median-2d (x)
  (mapcar #'median x))

(defun mean-2d (x)
	(mapcar #'mean x))

(defun centres (x)
"Args: (x)
Returns a point structure of zonal centres for zone structure X"
  (when (zones-p x)
        (make-points
         :coords (mapcar #'median-2d (zones-coords x))
         :values (zones-values x))))
         
(defun centres2 (x)
"Args: (x)
Returns a point structure of zonal centres for zone structure X"
  (when (zones-p x)
        (make-points
         :coords (mapcar #'mean-2d (zones-coords x))
         :values (zones-values x))))         

(defun outline (x)
"Args: (x)
Returns the outlines of zones as a line structure"
	(when (zones-p x)
		(make-lines
			:coords (zones-coords x)
			:values (zones-values x))))

(defun bounds (x)
"Args: (x)
Returns a point structure of a pair of bounds for geographical structure X"
  (let ((result nil))
    (when (zones-p x)
          (let ((c (zones-coords x)))
            (def result
                 (make-points
                  :coords (list
                           (list
                            (min (first  (transpose c)))
                            (min (second (transpose c))))
                           (list 
                            (max (first  (transpose c)))
                            (max (second (transpose c)))))))))
    (when (points-p x)
          (let ((c (points-coords x)))
            (def result
                 (make-points
                  :coords (list
                           (list
                            (min (first  (transpose c)))
                            (min (second (transpose c))))
                           (list 
                            (max (first  (transpose c)))
                            (max (second (transpose c)))))))))
    (when (lines-p x)
          (let ((c (lines-coords x)))
            (def result
                 (make-points
                  :coords (list
                           (list
                            (min (first  (transpose c)))
                            (min (second (transpose c))))
                           (list 
                            (max (first  (transpose c)))
                            (max (second (transpose c)))))))))
    result))


(defun nodes (x)
  (when (zones-p x)
        (make-points :coords (mapcar #'first (mapcar #'transpose (zones-coords x)))
                     :values (zones-values x))))

(defun select-if (x condition)
"Args: (x condition)
Returns a geographical structure containing elements of X meeting CONDITION"
  (cond
    ((zones-p x)
     (make-zones 
      :coords (select (zones-coords x) (which condition))
      :values (select (zones-values x) (which condition))))
    ((lines-p x)
     (make-lines 
      :coords (select (lines-coords x) (which condition))
      :values (select (lines-values x) (which condition))))
    ((points-p x)
     (make-points 
      :coords (select (points-coords x) (which condition))
      :values (select (points-values x) (which condition))))))

(defun map-order (geog id x)
"Args: (geog id x)
Re-orders the elements of X so they match the element order of GEOG.
ID is a list of the element values of X in the order matching X"
  (cond
    ((zones-p geog)
     (select x (select (order id) (rank (zones-values geog)))))
    ((lines-p geog)
     (select x (select (order id) (rank (lines-values geog)))))
    ((points-p geog)
     (select x (select (order id) (rank (points-values geog)))))))
;;;
;;; Convert to and from the complex number representations of geometry
;;;

(defun complex-form (x)
"Args: (x)
Returns the complex number representation of geographical structure X."
  (cond 
    ((points-p x)
     (apply #'complex (transpose (points-coords x))))
    ((zones-p x)
     (mapcar #'(lambda (z) (apply #'complex z)) (zones-coords x)))
    ((lines-p x)
        (mapcar #'(lambda (z) (apply #'complex z)) (lines-coords x))))) 

(defun complex-to-points (c &optional points)
"Args: (C POINTS)
Puts the complex form C into a new point coverage with attribute values from POINTS"
  (make-points :coords (transpose (list (realpart c) (imagpart c)))
               :values 
               (if points (points-values points) (iseq (length c)))))

(defun complex-to-lines (c &optional lines)
"Args: (C LINES)
Puts the complex form C into a new line coverage with attribute values from LINES"
  (make-lines :coords
              (mapcar #'(lambda (q) 
                         (list (realpart q) (imagpart q)))
                      c)
              :values 
              (if lines (lines-values lines) (iseq (length c)))))

(defun complex-to-zones (c &optional zones)
"Args: (C ZONES)
Puts the complex form C into a new zone coverage with attribute values from ZONES"
  (make-zones :coords
              (mapcar #'(lambda (q) 
                          (list (realpart q) (imagpart q)))
                      c)
              :values 
              (if zones (zones-values zones) (iseq (length c)))))

(defun complex-to-geog (c geog)
  (cond 
    ((zones-p geog) (complex-to-zones c geog))
    ((lines-p geog) (complex-to-lines c geog))
    ((points-p geog) (complex-to-points c geog))
    (t nil)))

(defun geog-type (x)
"Args: (Geog)
What type of geographical variable is X? Can return POINTS, LINES, ZONES or NIL"
    (cond
      ((zones-p x) 'zones)
      ((points-p x) 'points)
      ((lines-p x) 'lines)
      t nil))

;;;
;;; 
;;;


;;;
;;;  This is just a mongrel at the minute,  but is used in other stuff...
;;;
    
(defun nicebounds (x)
  (let
    ((b (bounds x)))
    (when b
          (let
            ((c (points-coords b)))
            (mapcar #'get-nice-range 
                    (first c) (second c) '(10 10))))))
       

;;;
;;; Then we need some prototypes to draw them as map layers.  These are 
;;; polygons
;;;


(defproto poly-layer-proto '(polys polycols) )

(defmeth poly-layer-proto :exhibit (owner)
  (let 
    ((polys (slot-value 'polys))
     (polycols (slot-value 'polycols)))
    (dotimes (i (length polys))
             (send owner :draw-poly (nth i polys) (nth i polycols)))))
     

(defmeth poly-layer-proto :polys (x) 
  (setf (slot-value 'polys) x))

(defmeth poly-layer-proto :polycols (x)
  (setf (slot-value 'polycols) x))


;;;
;;; These are spots
;;;

(defproto spot-layer-proto '(spots spotcols spotsizes))
  
(defmeth spot-layer-proto :exhibit (owner)
  (let
    ((spots (slot-value 'spots))
     (spotcols (slot-value 'spotcols))
     (spotsizes (slot-value 'spotsizes)))
    (dotimes (i (length spots))
             (send owner :draw-spot (nth i spots) (nth i spotsizes) (nth i spotcols)))))
 
(defmeth spot-layer-proto :spots (x)
  (setf (slot-value 'spots) x))

(defmeth spot-layer-proto :spotcols (x)
  (setf (slot-value 'spotcols) x))

(defmeth spot-layer-proto :spotsizes (x)
  (setf (slot-value 'spotsizes) x))
 
;;;
;;; This is powder
;;;

(defproto powder-layer-proto '(specks speckcols))
  
(defmeth powder-layer-proto :exhibit (owner)
  (let
    ((specks (slot-value 'specks))
     (speckcols (slot-value 'speckcols)))
    (dotimes (i (length specks))
             (send owner :draw-speck (nth i specks) (nth i speckcols)))))
 
(defmeth powder-layer-proto :specks (x)
  (setf (slot-value 'specks) x))

(defmeth powder-layer-proto :speckcols (x)
  (setf (slot-value 'speckcols) x))


 
;;;
;;; These are lines
;;;

(defproto line-layer-proto '(lines linecols linesizes) )

(defmeth line-layer-proto :exhibit (owner)
  (let 
    ((lines (slot-value 'lines))
     (linecols (slot-value 'linecols))
     (linesizes (slot-value 'linesizes)))
    (dotimes (i (length lines))
             (send owner :draw-line (nth i lines) (nth i linecols)(nth i linesizes)))))

(defmeth line-layer-proto :lines (x) 
  (setf (slot-value 'lines) x))

(defmeth line-layer-proto :linecols (x)
  (setf (slot-value 'linecols) x))

(defmeth line-layer-proto :linesizes (x)
  (setf (slot-value 'linesizes) x))


;;;
;;;  And this is text
;;;

(defproto text-layer-proto '(points text) )

(defmeth text-layer-proto :exhibit (owner)
  (let 
    ((points (slot-value 'points))
     (text (slot-value 'text)))
    (dotimes (i (length text))
             (let 
               ((pix (send owner :real-to-canvas 
                           (first (nth i points))
                           (second (nth i points)))))
               (send owner :draw-string 
                     (nth i text) 
                     (first pix)
                     (second pix))))))
                          
(defmeth text-layer-proto :text (x) 
  (setf (slot-value 'text) x))

(defmeth text-layer-proto :points (x)
  (setf (slot-value 'points) x))

;;;
;;;  Not a direct "Map data object" but this is a key 
;;;

(defproto key-proto '(intervals cols locx locy keytitle))

(defmeth key-proto :intervals (x) 
  (setf (slot-value 'intervals) x))

(defmeth key-proto :cols (x) 
  (setf (slot-value 'cols) x))

(defmeth key-proto :keytitle (x)
  (setf (slot-value 'keytitle) x))

(defmeth key-proto :loc (x y) 
  (setf (slot-value 'locx) x)
  (setf (slot-value 'locy) y))

(defmeth key-proto :exhibit (owner)
  (let 
    ((intervals (slot-value 'intervals))
     (cols (slot-value 'cols))
     (x (slot-value 'locx))
  	 (y (- (second (send owner :size))(slot-value 'locy))))
    (dotimes (i (length intervals))
             (send owner :draw-string (format nil ">~,3f" (nth i intervals)) (+ x 15) (- y (* i 20)))
             (send owner :draw-color (nth i cols))
             (send owner :paint-rect x (- y 12 (* i 20)) 12 12)
             (send owner :draw-color 'black)
             (send owner :frame-rect x (- y 12 (* i 20)) 12 12))
    (send owner :draw-string (slot-value 'keytitle) x (- y (* (length cols) 20)))))

(defmeth key-proto :eps-exhibit (owner)
  (let 
    ((intervals (slot-value 'intervals))
     (cols (slot-value 'cols))
     (x (slot-value 'locx))
  	 (y (- (second (send owner :size))(slot-value 'locy))))
    (dotimes (i (length intervals))
             (send owner :eps-string  (format nil ">~,3f" (nth i intervals)) (+ x 15) (- y (* i 20)))
             (send owner :eps-box x (- y 12 (* i 20)) 12 12 (nth i cols)))
    (send owner :eps-string (slot-value 'keytitle) x (- y (* (length cols) 20)))))

;;;
;;; In a similar vane,  here is a "scale" proto
;;;

(defproto scale-proto '(locx locy len unit-name))

(defmeth scale-proto :exhibit (owner)
  (let*
  	((unit-name (slot-value 'unit-name))
  	 (x (slot-value 'locx))
  	 (y (- (second (send owner :size))(slot-value 'locy)))
  	 (len (slot-value 'len))
  	 (locn (send owner :canvas-to-real x y))
  	 (plen (abs (first (- 
  	 			(send owner :real-to-canvas (first locn) (second locn))
  	 	      	(send owner :real-to-canvas (+ (first locn) len) (second locn)))))))	 	      	
    (send owner :frame-poly (transpose (list (list x x (+ x plen) (+ x plen))
    					               (list (- y 3) y y (- y 3)))))
  	(send owner :draw-string unit-name x (+ y 15))))
  	 
(defmeth scale-proto :eps-exhibit (owner)
  (let*
  	((unit-name (slot-value 'unit-name))
  	 (ymax (second (send owner :size)))
  	 (eps-stream (send owner :slot-value 'eps-stream))
  	 (x (slot-value 'locx))
  	 (y (- (second (send owner :size))(slot-value 'locy)))
  	 (len (slot-value 'len))
  	 (locn (send owner :canvas-to-real x y))
  	 (plen (abs (first (- 
  	 			(send owner :eps-real-to-canvas (first locn) (second locn))
  	 	      	(send owner :eps-real-to-canvas (+ (first locn) len) (second locn)))))))	
  	(format eps-stream "~f ~f m~%" x (- ymax (- y 3)))
  	(format eps-stream "~f ~f l~%" x (- ymax y))
  	(format eps-stream "~f ~f l~%" (+ x plen) (- ymax y))
  	(format eps-stream "~f ~f l stroke~%" (+ x plen) (- ymax (- y 3)))
  	(send owner :eps-string unit-name x (+ y 15))))
;;;
;;;  Here are 'live' versions
;;;

(def *live-color* 'yellow)

(defproto live-poly-layer-proto '(selected hotspots) nil poly-layer-proto)

(defmeth live-poly-layer-proto :exhibit (owner)
  (let 
    ((polys (slot-value 'polys))
     (selected (slot-value 'selected))
     (polycols (slot-value 'polycols)))
    (dotimes (i (length polys))
             (send owner :draw-poly (nth i polys) 
                   (if (nth i selected) *live-color* (nth i polycols))))))

(defmeth live-poly-layer-proto :eps-exhibit (owner)
  (let 
    ((polys (slot-value 'polys))
     (selected (slot-value 'selected))
     (polycols (slot-value 'polycols)))
    (dotimes (i (length polys))
             (send owner :eps-poly (nth i polys) 
                   (if (nth i selected) *live-color* (nth i polycols))))))

(defmeth live-poly-layer-proto :choose (x)
  (let 
    ((c (apply #'complex x))
     (polys (slot-value 'polys))
     (hotspots (slot-value 'hotspots)))
    (if (null hotspots) 
        (setf hotspots (mapcar #'mean (apply #'complex (transpose polys)))))   
    (first (which (= (min (abs (- c hotspots))) (abs (- c hotspots)))))))
   
    

(defmeth live-poly-layer-proto :adjust (i owner)
  (let
    ((polys (slot-value 'polys))
     (selected (slot-value 'selected))
     (polycols (slot-value 'polycols)))
    (setf (nth i selected) (null (nth i selected)))
    (send owner :draw-poly (nth i polys) 
                   (if (nth i selected) *live-color* (nth i polycols)))))


(defproto live-spot-layer-proto '(selected hotspots) nil spot-layer-proto)
  
(defmeth live-spot-layer-proto :exhibit (owner)
  (let*
    ((spots (slot-value 'spots))
     (spotcols (slot-value 'spotcols))
     (spotsizes (slot-value 'spotsizes))
     (seq (reverse (order spotsizes)))
     (selected (slot-value 'selected)))
    (dotimes (i (length spots))
             (send owner :draw-spot (nth i (select spots seq)) 
                   (nth i (select spotsizes seq)) 
                   (if (nth i (select selected seq))
                       *live-color*
                        (nth i (select spotcols seq)))))))
 
(defmeth live-spot-layer-proto :eps-exhibit (owner)
  (let
    ((spots (slot-value 'spots))
     (spotcols (slot-value 'spotcols))
     (spotsizes (slot-value 'spotsizes))
     (seq (reverse (order spotsizes)))
     (selected (slot-value 'selected)))
    (dotimes (i (length spots))
             (send owner :eps-spot (nth i (select spots seq)) 
                   (nth i (select spotsizes seq)) 
                   (if (nth i (select selected seq))
                       *live-color*
                        (nth i (select spotcols seq)))))))
 
 
(defmeth live-spot-layer-proto :choose (x)
  (let 
    ((c (apply #'complex x))
     (spots (slot-value 'spots))
     (hotspots (slot-value 'hotspots)))
    (if (null hotspots) 
        (setf hotspots (apply #'complex (transpose spots))))   
    (first (which (= (min (abs (- c hotspots))) (abs (- c hotspots))))))) 
 
(defmeth live-spot-layer-proto :adjust (i owner)
  (let
    ((spots (slot-value 'spots))
     (selected (slot-value 'selected))
     (spotsizes (slot-value 'spotsizes))
     (polycols (slot-value 'spotcols)))
    (setf (nth i selected) (null (nth i selected)))
    (send owner :draw-spot 
          (nth i spots)
          (nth i spotsizes)        
          (if (nth i selected) *live-color* (nth i polycols)))))

(defmeth live-spot-layer-proto :spots (x)
  (setf (slot-value 'spots) x))

(defmeth live-spot-layer-proto :spotcols (x)
  (setf (slot-value 'spotcols) x))

(defmeth live-spot-layer-proto :spotsizes (x)
  (setf (slot-value 'spotsizes) x))

(defproto live-powder-layer-proto '(selected hotspots) nil powder-layer-proto)
  
(defmeth live-powder-layer-proto :exhibit (owner)
  (let
    ((specks (slot-value 'specks))
     (selected (slot-value 'selected))
     (speckcols (slot-value 'speckcols)))
    (dotimes (i (length specks))
             (send owner :draw-speck (nth i specks) 
                   (if (nth i selected) *live-color* (nth i speckcols))))))

(defmeth live-powder-layer-proto :eps-exhibit (owner)
  (let
    ((specks (slot-value 'specks))
     (selected (slot-value 'selected))
     (speckcols (slot-value 'speckcols)))
    (dotimes (i (length specks))
             (send owner :eps-speck (nth i specks) 
                   (if (nth i selected) *live-color* (nth i speckcols))))))

(defmeth live-powder-layer-proto :choose (x)
  (let 
    ((c (apply #'complex x))
     (specks (slot-value 'specks))
     (hotspots (slot-value 'hotspots)))
    (if (null hotspots) 
        (setf hotspots (apply #'complex (transpose specks))))   
    (first (which (= (min (abs (- c hotspots))) (abs (- c hotspots))))))) 

(defmeth live-powder-layer-proto :adjust (i owner)
  (let
    ((specks (slot-value 'specks))
     (selected (slot-value 'selected))
     (speckcols (slot-value 'speckcols)))
    (setf (nth i selected) (null (nth i selected)))
    (send owner :draw-speck 
          (nth i specks)       
          (if (nth i selected) *live-color* (nth i speckcols)))))

 


(defproto live-line-layer-proto '(selected hotspots) nil line-layer-proto)

(defmeth live-line-layer-proto :exhibit (owner)
  (let 
    ((lines (slot-value 'lines))
     (linecols (slot-value 'linecols))
     (linesizes (slot-value 'linesizes))
     (selected (slot-value 'selected)))
    (dotimes (i (length lines))
             (send owner :draw-line 
                   (nth i lines) 
                   (if (nth i selected) *live-color* (nth i linecols))
                   (nth i linesizes)))))

(defmeth live-line-layer-proto :eps-exhibit (owner)
  (let 
    ((lines (slot-value 'lines))
     (selected (slot-value 'selected))
     (linecols (slot-value 'linecols)))
    (dotimes (i (length lines))
             (send owner :eps-line (nth i lines) 
                   (if (nth i selected) *live-color* (nth i linecols))))))

(defmeth live-line-layer-proto :choose (x)
  (let 
    ((c (apply #'complex x))
     (lines (slot-value 'lines))
     (hotspots (slot-value 'hotspots)))
    (if (null hotspots) 
        (setf hotspots (mapcar #'mean (apply #'complex (transpose lines)))))   
    (first (which (= (min (abs (- c hotspots))) (abs (- c hotspots)))))))


(defmeth live-line-layer-proto :adjust (i owner)
  (let
    ((lines (slot-value 'lines))
     (selected (slot-value 'selected))
     (linesizes (slot-value 'linesizes))
     (linecols (slot-value 'linecols)))
    (setf (nth i selected) (null (nth i selected)))
    (send owner :draw-line
          (nth i lines)     
          (if (nth i selected) *live-color* (nth i linecols))
          (nth i linesizes))))


(defmeth live-line-layer-proto :lines (x) 
  (setf (slot-value 'lines) x))

(defmeth live-line-layer-proto :linecols (x)
  (setf (slot-value 'linecols) x))

(defmeth live-line-layer-proto :linesizes (x)
  (setf (slot-value 'linesizes) x))

;;;
;;; Then we need map windows to draw them in...
;;; This starts of as a graph window with polys,  then gets better
  

;;; (defproto map-window-proto '(items) nil graph-proto)
(defproto map-window-proto '(items eps-stream ymax) nil graph-proto)

(defmeth map-window-proto :isnew (&rest args)
  (apply #'call-next-method 2 args))


(defmeth map-window-proto :scale-adjust ()
    (let*
      ((r0 (send self :range 0))
       (r1 (send self :range 1))
       (l0 (abs (apply #'- r0)))
       (l1 (abs (apply #'- r1)))
       (d01 (abs (- l0 l1))))
      (if (> l1 l0)
          (send self :range 0 (- (min r0) (/ d01 2)) (+ (max r0) (/ d01 2)))
          (send self :range 1 (- (min r1) (/ d01 2)) (+ (max r1) (/ d01 2))))))


(defmeth map-window-proto :eps-open ()
  (let 
    ((eps-stream (slot-value 'eps-stream))
     (xmax (first (send self :size)))
     (ymax (second (send self :size))))
    (setf eps-stream
        (open 
         (set-file-dialog "Encapsulated Postscript File:")
         :direction :output))
    (setf (slot-value 'ymax) ymax)
    (format eps-stream "%!PS-Adobe-3.0 EPSF-3.0~%")
    (format eps-stream "%%Creator: XLisp-Stat~%")
    (format eps-stream "%%CreationDate: Tue May  7 20:33:17 1996~%")
    (format eps-stream "%%BoundingBox: 0 0 ~f ~f~%" xmax ymax)                                              
    (format eps-stream "%%LanguageLevel: 1~%")
    (format eps-stream "%%EndComments~%")
    (format eps-stream "/bd{bind def}bind def~%") 
    (format eps-stream "/m{moveto}bd /l{lineto}bd /sf{setrgbcolor fill}bd /sff{setrgbcolor gsave fill~%")
    (format eps-stream "grestore 0.0 setgray stroke}bd~%")
    (format eps-stream "/ss{setrgbcolor stroke}bd~%")
    (format eps-stream "/dt{5 0 360 arc 0 0 0 sf}bd~%")
    (format eps-stream "/ci{0 360 arc}bd~%")
    (format eps-stream "/fin{showpage}bd~%")
    (format eps-stream "/ArialMT findfont 10 scalefont setfont~%")
    (format eps-stream "0 0 m 0 ~f l ~f ~f l ~f 0 l 0 0 l 1.0 1.0 1.0 sf~%" ymax xmax ymax xmax)
    (setf (slot-value 'eps-stream) eps-stream)))

(defmeth map-window-proto :eps-close ()
  (let
    ((eps-stream (slot-value 'eps-stream)))
    (format eps-stream "fin~%")
    (format eps-stream "%%EOF~%")
    (close eps-stream)))


(defmeth map-window-proto :eps-output ()
 (send self :eps-open)
 (send self :eps-copy)
 (send self :eps-close))
  
(defmeth map-window-proto :draw-poly (poly colour)
  (let*
    (
     (canvas-zone 
      (map-elements 
       #'send self :real-to-canvas (first poly) (second poly)))) 
    (send self :draw-color colour)
    (send self :paint-poly canvas-zone)
    (send self :draw-color 'black)
;   (send self :frame-poly canvas-zone)
    ))

(defmeth map-window-proto :eps-real-to-canvas (x y)
  (let*	
	((screen-frame (send self :content-rect))
     (win-xmin (nth 0 screen-frame))
	 (win-ymin (nth 1 screen-frame))
	 (win-xmax (nth 2 screen-frame))
	 (win-ymax (nth 3 screen-frame))
	 (map-frame-x (send self :range 0))
	 (map-frame-y (send self :range 1))
	 (map-xmin (nth 0 map-frame-x))
	 (map-xmax (nth 1 map-frame-x))
	 (map-ymin (nth 0 map-frame-y))
	 (map-ymax (nth 1 map-frame-y)))
	(list
     (+ win-xmin (* (- win-xmax win-xmin) (/ (- x map-xmin) (- map-xmax map-xmin))))
     (- win-ymax (+ win-ymin (* (- win-ymax win-ymin)
                 (/ (- y map-ymin) (- map-ymax map-ymin))))))))

(defmeth map-window-proto :eps-poly (poly colour)
  (let*
    ((eps-stream (slot-value 'eps-stream))
     (cvec (color-rgb colour))
     (ymax (slot-value 'ymax))
     (canvas-zone 
      (map-elements 
       #'send self :eps-real-to-canvas (first poly) (second poly))))
    (format eps-stream "~f ~f m~%" (first (first canvas-zone)) 
            (- ymax (second (first canvas-zone))))
    (mapcar #'(lambda (x) (format eps-stream "~f ~f l~%" (first x) (- ymax (second x))))
            (rest canvas-zone))
    (format eps-stream "~f ~f l~%" (first (first canvas-zone)) 
            (- ymax (second (first canvas-zone))))
    (format eps-stream "~f ~f ~f sf~%"
            (first cvec) (second cvec) (third cvec))))

(defmeth map-window-proto :eps-string (string x y)
	(let*
    	((eps-stream (slot-value 'eps-stream))
    	 (ymax (slot-value 'ymax)))
		(format eps-stream "~f ~f m~%" x (- ymax y))
		(format eps-stream "(~a) show~%" string)))
		
(defmeth map-window-proto :eps-black (string x y)
	(let*
    	((eps-stream (slot-value 'eps-stream))
    	 (ymax (slot-value 'ymax)))
		(format eps-stream "0.0 0.0 0.0 setrgbcolor ~%")))		
		
		
(defmeth map-window-proto :eps-box (x y xl yl colour)
	(let*
    	((eps-stream (slot-value 'eps-stream))
    	 (cvec (color-rgb colour))
    	 (ymax (slot-value 'ymax)))
    	(format eps-stream "~f ~f m~%" x (- ymax y))
    	(format eps-stream "~f ~f l~%" (+ x xl) (- ymax y))
    	(format eps-stream "~f ~f l~%" (+ x xl) (- ymax y yl))
    	(format eps-stream "~f ~f l~%" x (- ymax y yl))
    	(format eps-stream "~f ~f l~%" x (- ymax y))
    	(format eps-stream "~f ~f ~f sff~%" (first cvec) (second cvec) (third cvec))))
    	 
    	 
    	 
    	 
(defmeth map-window-proto :draw-line (line colour width)
  (let*
    (
     (canvas-zone 
      (map-elements 
       #'send self :real-to-canvas (first line) (second line)))) 
    (send self :draw-color colour)
    (send self :line-width width)
    (send self :frame-poly canvas-zone)
    (send self :draw-color 'black)
    (send self :line-width 1)))


(defmeth map-window-proto :eps-line (line colour)
  (let*
    ((eps-stream (slot-value 'eps-stream))
     (cvec (color-rgb colour))
     (ymax (slot-value 'ymax))
     (canvas-zone 
      (map-elements 
       #'send self :eps-real-to-canvas (first line) (second line))))
    (format eps-stream "~f ~f m~%" (first (first canvas-zone)) 
            (- ymax (second (first canvas-zone))))
    (mapcar #'(lambda (x) (format eps-stream "~f ~f l~%" (first x) (- ymax (second x))))
            (rest canvas-zone))
;   (format eps-stream "~f ~f l~%" (first (first canvas-zone)) 
;           (- ymax (second (first canvas-zone))))
    (format eps-stream "~f ~f ~f ss~%"
            (first cvec) (second cvec) (third cvec))))

(defmeth map-window-proto :draw-speck (speck colour)
  (let*
    (
     (canvas-speck 
      (send self :real-to-canvas (first speck) (second speck)))) 
    (send self :draw-color colour)
    (send self :draw-point (first canvas-speck) (second canvas-speck))
    (send self :draw-color 'black)))

(defmeth map-window-proto :eps-speck (speck colour)
  (let*	
    ((eps-stream (slot-value 'eps-stream))
     (cvec (color-rgb colour))
     (ymax (slot-value 'ymax))
     (canvas-speck
      (map-elements 
       #'send self :eps-real-to-canvas (first speck) (second speck))))
    (format eps-stream "~f ~f 1 ci " (first canvas-speck) (- ymax (second canvas-speck)))
    (format eps-stream "~f ~f ~f sf~%" (first cvec) (second cvec) (third cvec))))


(defmeth map-window-proto :draw-spot (spot size colour)
 (let*
   (
    (x1 (- (first spot) size ))
    (y1 (- (second spot) size ))
    (x2 (+ (first spot) size ))
    (y2 (+ (second spot) size ))
    (p1 (send self :real-to-canvas x1 y1))
    (p2 (send self :real-to-canvas x2 y2))
    (d1 (* '(1 -1) (- p2 p1))))
   (send self :draw-color colour)
   (send self :paint-oval (first p1) (second p2) (first d1) (second d1))
   (send self :draw-color 'black)
   (send self :frame-oval (first p1) (second p2) (first d1) (second d1))))

(defmeth map-window-proto :eps-spot (spot size colour)
  (let*	
    ((eps-stream (slot-value 'eps-stream))
     (cvec (color-rgb colour))
     (ymax (slot-value 'ymax))
     (cs1 (send self :eps-real-to-canvas 0 0))
     (cs2 (send self :eps-real-to-canvas 0 1))
     (cs3 (- cs2 cs1))
     (cs4 (sqrt (sum (* cs3 cs3))))
     (canvas-spot
      (map-elements 
       #'send self :eps-real-to-canvas (first spot) (second spot))))
    (format eps-stream "~f ~f ~f ci " (first canvas-spot) (- ymax (second canvas-spot)) (* cs4 size))
    (format eps-stream "~f ~f ~f sff~%" (first cvec) (second cvec) (third cvec))))


(defmeth map-window-proto :draw-polys ()
  (let 
    ((polys (slot-value 'polys))
     (polycols (slot-value 'polycols)))
    (dotimes (i (length polys))
             (send self :draw-poly (nth i polys) (nth i polycols)))))

(defmeth map-window-proto :eps-polys ()
  (let 
    ((polys (slot-value 'polys))
     (polycols (slot-value 'polycols)))
    (dotimes (i (length polys))
             (send self :eps-poly (nth i polys) (nth i polycols)))))

(defmeth map-window-proto :add-poly (poly color)  
  (setf (slot-value 'polys) (cons poly (slot-value 'polys)))
  (setf (slot-value 'polycols) (cons color (slot-value 'polycols)))
  (send self :redraw))


(defmeth map-window-proto :redraw ()
 (let 
   ((items (slot-value 'items)))
   (send self :clear)
   (send self :use-color t)
   (dolist (item (reverse items))
           (send item :exhibit self))))

(defmeth map-window-proto :eps-copy ()
 (let 
   ((items (slot-value 'items)))
   (send self :clear)
   (dolist (item (reverse items))
           (send item :eps-exhibit self))))

(defmeth map-window-proto :adjust-screen-point (i)
  (let 
    ((top (first (slot-value 'items))))
    (if (member ':adjust (send top :method-selectors)) 
        (send top :adjust i self))))
        

(defmeth map-window-proto :add-item (item)
  (setf (slot-value 'items) (cons item (slot-value 'items)))
  (send self :redraw))

(defmeth map-window-proto :drop-item ()
  (setf (slot-value 'items) (rest (slot-value 'items)))
  (send self :redraw))

(defmeth map-window-proto :demote-item ()
  (setf (slot-value 'items) (append (rest (slot-value 'items)) 
                                    (list (first (slot-value 'items)))))
  (send self :redraw))

(defmeth map-window-proto :set-window (x1 y1 x2 y2)
  (send self :range 0 x1 x2)
  (send self :range 1 y1 y2))

(defmeth map-window-proto :focus-on (layer)
  (let* 
    ((n (nicebounds layer))
     (f (first n))
     (s (second n)))
    (send self :set-window (first f) (first s) (second f) (second s))
    (send self :redraw)))

;;;
;;;  Get rid of the 'bog-standard' mouse modes and 
;;;  put in two new ones
;;;
   


(send map-window-proto :add-mouse-mode 'show-coordinates
      :title "Show Coordinates"
      :click :do-show-coordinates
      :cursor 'finger)

(defmeth map-window-proto :do-show-coordinates (x y m1 m2)
  (let*
    (
     (c (send self :canvas-to-real x y))
     (s (format nil "(~8,0f,~8,0f)" (first c) (second c)))
     (mode (send self :draw-mode)))
    (send self :draw-mode 'xor)
    (send self :draw-string s x y)
    (send self :while-button-down #'(lambda (x y) nil))
    (send self :draw-string s x y)
    (send self :draw-mode mode)))

(send map-window-proto :add-mouse-mode 'alter-selection
      :title "Alter Selection"
      :click :do-alter-selection
      :cursor 'cross)

(defmeth map-window-proto :do-alter-selection (x y m1 m2)
  (let*
    ((top (first (slot-value 'items)))
     (sel nil)
     (wsel nil)
     (item (send top :choose (send self :canvas-to-real x y))))
    (unless (member self (linked-plots))
    	    (send self :adjust-screen-point item))
    (when (member self (linked-plots))
          (def sel (send top :slot-value 'selected))
          (setf (nth item sel) (null (nth item sel)))
          (setf wsel (which sel))
          (dolist (plot (linked-plots))
          	      (unless (eq plot self)
          	              (send self :linked nil)
          	              (send plot :selection wsel)
                          (send top :exhibit self)
          	              (send self :linked t))))))

  
						  
;;;
;;; Some color deciding utilities
;;;

(def miss-num -999)
(def *eps-colors* nil)
(def *eps-rgb* nil)

(defun def-color (name r g b)
  (when (not (member name *colors*))
        (make-color name r g b))
  (def *eps-colors* (cons name *eps-colors*))
  (def *eps-rgb* (cons (list r g b) *eps-rgb*)))

(defun color-rgb (k) 
 (nth  (first (which 
               (map-elements #'eq k *eps-colors*))) 
       *eps-rgb*))

(def-color 'white 1.0 1.0 1.0)
(def-color 'red 1.0 0.0 0.0)
(def-color 'green 0.0 1.0 0.0)
(def-color 'blue 0.0 0.0 1.0)
(def-color 'yellow 1.0 1.0 0.0)
(def-color 'magenta 1.0 0.0 1.0)
(def-color 'cyan 0.0 1.0 1.0)
(def-color 'black 0.0 0.0 0.0)
(def-color 'miss-color 0.8 0.7 0.6)
(def-color 'red5 1.0 0.0 0.0)
(def-color 'red4 1.0 0.3 0.3)
(def-color 'red3 1.0 0.5 0.5)
(def-color 'red2 1.0 0.7 0.7)
(def-color 'red1 1.0 0.8 0.8)
(def-color 'mbr0 0.9 0.8 0.9)
(def-color 'blu5 0.0 0.0 1.0)
(def-color 'blu4 0.3 0.3 1.0)
(def-color 'blu3 0.5 0.5 1.0)
(def-color 'blu2 0.7 0.7 1.0)
(def-color 'blu1 0.8 0.8 1.0)
(def-color 'mgb0 0.8 0.9 0.9)
(def-color 'grn5 0.0 1.0 0.0)
(def-color 'grn4 0.3 1.0 0.3)
(def-color 'grn3 0.5 1.0 0.5)
(def-color 'grn2 0.7 1.0 0.7)
(def-color 'grn1 0.8 1.0 0.8)
(def-color 'mrg0 0.9 0.9 0.8)
(def-color 'grey5 0.1 0.1 0.1)
(def-color 'grey4 0.3 0.3 0.3)
(def-color 'grey3 0.5 0.5 0.5)
(def-color 'grey2 0.7 0.7 0.7)
(def-color 'grey1 0.8 0.8 0.8)


(defun shade-steps (x)
  (let*
    (
     (mids (quantile x '(0.05 0.25 0.5 0.75 0.95))))
    (reverse (/ (+ (butlast mids 1) (rest mids)) 2))))

(defun shade-class (x midpoints colours)
  (let*
    (
     (edges (reverse (/ (+ (butlast midpoints 1) (rest midpoints)) 2)))
     (cols  (reverse colours))
     (shade (nth 0 cols)))
    (dotimes (i (1- (length midpoints)))
             (def shade (if-else (< x (nth i edges))
                  (nth (1+ i) cols) shade)))
  (if-else (= miss-num x) 'miss-color shade)))

(defun auto-shade-class (x)
  (let* 
    (
     (mids (quantile x '(0.05 0.25 0.5 0.75 0.95)))
     (palette '(red1 red2 red3 red4 red5)))
    (shade-class x mids palette)))

(defun auto-key (data x y title)
  (key (quantile data '(0.05 0.25 0.5 0.75 0.95)) 
       '(red1 red2 red3 red4 red5) x y title))

(defun fancy-shade-class (x &optional (midvals nil))
  (let* 
    (
     (mids (if midvals midvals (quantile x '(0.05 0.25 0.5 0.75 0.95))))
     (palette '(yellow cyan red blue black)))
    (shade-class x mids palette)))

(defun key (intervals cols x y title)
  (send key-proto :new
        :intervals intervals
        :cols cols
        :keytitle title
        :locx x
        :locy y))

(defun scale (x y l title)
  (send scale-proto :new
        :locx x
        :locy y
        :len l
        :unit-name title))



(defun choro-layer (geog x)
  (send poly-layer-proto 
        :new 
        :polys (zones-coords geog) 
        :polycols (auto-shade-class x)))

(defun live-choro-layer (geog &optional (x nil))
  (send live-poly-layer-proto 
        :new 
        :polys (zones-coords geog) 
        :selected (repeat nil (length (zones-coords geog)))
        :polycols (if x (auto-shade-class x) 
                      (repeat 'green (length (zones-coords geog))))))

(defun fancy-choro-layer (geog x &optional (mids nil))
  (send poly-layer-proto
        :new
        :polys (zones-coords geog)
        :polycols (fancy-shade-class x mids)))

(defun spot-layer (geog x &optional (maxsize 750))
  (send spot-layer-proto 
        :new
        :spots (points-coords geog)
        :spotcols (repeat 'yellow (length (points-coords geog)))
        :spotsizes (* maxsize (/ x (max x)))))

(defun live-spot-layer (geog x &optional (maxsize 750))
  (send live-spot-layer-proto 
        :new
        :spots (points-coords geog)
        :spotcols (repeat 'mgb0 (length (points-coords geog)))
        :selected (repeat nil (length (points-coords geog)))
        :spotsizes (* maxsize (/ x (max x)))))

(defun powder-layer (geog)
  (send powder-layer-proto 
        :new 
        :specks (points-coords geog)
        :speckcols (repeat 'black (length (points-coords geog)))))

(defun live-powder-layer (geog)
  (send live-powder-layer-proto
        :new
        :specks (points-coords geog)
        :speckcols (repeat 'black (length (points-coords geog)))
        :selected (repeat nil (length (points-coords geog)))))
        
(defun line-layer (geog)
  (send line-layer-proto 
        :new
        :lines (lines-coords geog)
        :linecols (repeat 'green (length (lines-coords geog)))
        :linesizes (repeat 1 (length (lines-coords geog)))))

(defun live-line-layer (geog)
  (send live-line-layer-proto 
        :new
        :lines (lines-coords geog)
        :selected (repeat nil (length (lines-coords geog)))
        :linecols (repeat 'grey5 (length (lines-coords geog)))
        :linesizes (repeat 1 (length (lines-coords geog)))))

(defun text-layer (geog text)
  (send text-layer-proto 
        :new 
        :points (points-coords geog)
        :text text))

(defun map (geog &optional (x nil) (max 750))
  (let*
    ((mapwin (send map-window-proto :new)))
    (send mapwin :fixed-aspect t)	
    (when (zones-p geog)
          (send mapwin :focus-on geog)
          (send mapwin :add-item (live-choro-layer geog x)))
    (when (points-p geog)
          (send mapwin :focus-on geog)
          (send mapwin :add-item 
                (if (null x)
                    (live-powder-layer geog) 
                    (live-spot-layer geog x max))))
    (when (lines-p geog)
          (send mapwin :focus-on geog)
          (send mapwin :add-item (live-line-layer geog)))
    (send mapwin :scale-adjust)  
    (send mapwin :title "Map Window")  
    (send mapwin :delete-mouse-mode 'brushing)
    (send mapwin :delete-mouse-mode 'selecting)    
    mapwin))

(defun map-layer (geog &optional (x nil) (max 750))
  (cond
    ((zones-p geog) (live-choro-layer geog x))
    ((lines-p geog) (live-line-layer geog))
    ((points-p geog) 
     (if (null x) (live-powder-layer geog) (live-spot-layer geog x max)))))

;;;
;;; More data manipulation
;;;
   
(defun de-run (x)
  (select x (which (cons t (/= (butlast x) (rest x))))))

(defun *gen* (x map)
  (let 
    ((s1
      (de-run 
       (apply #'complex 
              (transpose (mapcar #'(lambda (z) 
                                     (send map :real-to-canvas 
                                           (realpart z) (imagpart z))) x))))))
    (transpose (map-elements #'send map :canvas-to-real (realpart s1) (imagpart s1)))))
    

(defun generalise (geog map)
"Args: (geog map)
Generalises the lines (or boundaries) in GEOG to pixels in window MAP"
  (let
    ((cf (complex-form geog)))
    (cond
      ((lines-p geog)
       (make-lines :coords (mapcar #'(lambda (z) (*gen* z map)) cf)
                   :values (lines-values geog)))
      ((zones-p geog)
       (make-zones :coords (mapcar #'(lambda (z) (*gen* z map)) cf)
                   :values (zones-values geog))))))

(defun within (x z)
"Args: (X Z)
Is the complex point X within the complex closed zone Z?"  
  (let
    ((cz (- z x)))
    (cond
      ((minusp (max (realpart cz))) nil)
      ((plusp (min (realpart cz))) nil)
      ((minusp (max (imagpart cz))) nil)
      ((plusp (min (imagpart cz))) nil)
      (t  
       (oddp (round (/
                     (sum (phase (/ cz (append (last cz) (butlast cz)))))
                     (* 2 pi))))))))

(defun id (x z)
"Args: (X Z) 
Which complex closed zone in the list Z contains X?"
  (let
    ((i 0)
     (l (length z)))
    (loop 
     (if (within x (nth i z)) (return i))
     (setf i (1+ i))
     (if (= i l) (return)))))

(defun identity (points zones)
"Args: (POINTS ZONES)
Gives a list of ZONE-based indices that each POINT lies in"
  (let 
    ((p (complex-form points))
     (z (complex-form zones)))
    (mapcar #'(lambda (x) (id x z)) p)))

;;; these need mending!

(defun zonal-count (points zones)
"Args: (POINTS ZONES)
Counts the number of POINTS in each ZONE" 
  (let*
    ((id (identity points zones))
     (counts (repeat 0 (length (zones-values zones)))))
    (dolist (i id)
            (setf (nth i counts) (1+ (nth i counts))))
    counts))



(defun zonal-sum (points points-vars zones)
"Args: (POINTS POINTS-VARS ZONES)
Sums POINTS-VARS according to containment of POINTS in ZONES"
  (let*
    ((id (identity points zones))
     (sums (repeat 0 (length (zones-values zones)))))
    (dotimes (i (length id))
             (setf (nth (nth i id) sums) 
                   (+ (nth (nth i id) sums) 
                      (nth i points-vars))))
    sums))

(defun zonal-mean (points points-vars zones)
"Args: (POINTS POINTS-VARS ZONES)
Sums POINTS-VARS according to containment of POINTS in ZONES"
  (let*
    ((id (identity points zones))
     (counts (repeat 0 (length (zones-values zones))))
     (sums (repeat 0 (length (zones-values zones)))))
    (dotimes (i (length id))
    	     (setf (nth (nth i id) counts) 
    	           (1+ (nth (nth i id) counts)))
             (setf (nth (nth i id) sums) 
                   (+ (nth (nth i id) sums) 
                      (nth i points-vars))))
    (dotimes (i (length sums))
    	(if (= (nth i counts) 0)
    		(setf (nth i sums) -999)
    		(setf (nth i sums) (/ (nth i sums) (nth i counts)))))
    sums))
    	


(defun zonal-list (points zones)
"Args: (POINTS ZONES)
Sorts POINTS indices into lists according to containment in ZONES"
  (let*
    ((id (identity points zones))
     (lists (repeat nil (length (zones-values zones)))))
    (dotimes (i (length id))
             (setf (nth (nth i id) lists) 
                   (cons i (nth (nth i id) lists))))
    lists))

;;;
;;;
;;;
 
(defun fancy-map (geog &optional (x nil) (mids nil))
  (let
    ((mapwin (send map-window-proto :new)))
    (when (zones-p geog)
          (send mapwin :focus-on geog)
          (send mapwin :add-item (fancy-choro-layer geog x mids)))
    (when (points-p geog)
          (send mapwin :focus-on geog)
          (send mapwin :add-item 
                (if (null x)
                    (live-powder-layer geog) 
                    (live-spot-layer geog x max))))
    (when (lines-p geog)
          (send mapwin :focus-on geog)
          (send mapwin :add-item (live-line-layer geog))) 
    mapwin))
 
 

(defun complex-area (z)
    (let*
      ((zz (append (last z) (butlast z)))
       (a (/ (imagpart (+ z zz)) 2))
       (b (realpart (- z zz))))
      (abs (sum (* a b)))))
      
(defun areas  (zones)
	(mapcar #'complex-area (complex-form zones)))
 
 (defun contig (zone-obj)
"Args: (X) 
Gives an index-based contiguity list for zone structure X." 
 (let
  ((q (complex-form zone-obj)))
  (mapcar #'(lambda (y)
              (which (mapcar #'(lambda (z) (intersection z y)) q)))
          q)))


(defun mean-polish (x contig)
"Args: (x contig)
Gives a mean polish to variable X using contiguity list CONTIG"
 (mapcar #'mean (mapcar #'(lambda (y) (select x y)) contig)))

(defun mean-surround (x contig)
"Args: (x contig)
Gives a mean surround to variable X using contiguity list CONTIG"
  (let
    ((contig2 (mapcar #'set-difference contig 
                      (mapcar #'list (iseq (length contig))))))
    (mapcar #'mean (mapcar #'(lambda (y) (select x y)) contig2))))


(defun median-polish (x contig)
"Args: (x contig)
Gives a median polish to variable X using contiguity list CONTIG"
 (mapcar #'median (mapcar #'(lambda (y) (select x y)) contig)))

(defun median-surround (x contig)
"Args: (x contig)
Gives a median surround to variable X using contiguity list CONTIG"
  (let
    ((contig2 (mapcar #'set-difference contig 
                      (mapcar #'list (iseq (length contig))))))
    (mapcar #'median (mapcar #'(lambda (y) (select x y)) contig2))))



(defun polish (x contig method)
"Args: (x contig function)
Gives a polish to variable X using contiguity list CONTIG and FUNCTION"
  (mapcar method (mapcar #'(lambda (y) (select x y)) contig)))

(defun surround (x contig method)
"Args: (x contig function)
Gives a surround to variable X using contiguity list CONTIG and method FUNCTION"
  (let
    ((contig2 (mapcar #'set-difference contig 
                      (mapcar #'list (iseq (length contig))))))
    (mapcar #'median (mapcar #'(lambda (y) (select x y)) contig2))))

(defun neighbour-lists (x nbr)
  (mapcar #'(lambda (y) (select x y)) nbr))