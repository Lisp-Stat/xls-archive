;;;
;;; Methods for outputting content of graphs as latex code.
;;; Uses latex picture environment, needs the latex packages epic and eepic.
;;; Primitive, written for simple graphs.
;;;

;; Kjetil Halvorsen, La Paz, 1997/1998
;; Copyright: GPL

;; Problems:
;       Doesnt clip if data is outside window, must do :adjust-to-data before
;            sending :2latex
;       Uses only scatterplot-proto's slots for points and lines, so any
;          graphical element represented with the graph-window-proto messages
;          is not included. Dont know any way to fix this at the lisp level
;       Corrolarry of above: doesnt work with histogram.
;
;       y-axis label and numbers isnt written horizontally, needs a dvi driver with rotate to
;             do that.
;       Taking acount of size of numbers is primitive

;;   works with:  plots made with:  plot-points
;                                   plot-function
;                                   box-plot
;                                   plot-lines
;                                   quantile-plot
;                                   probability-plot
;                                   my-histogram ...?

;    Doesnt work well with Majewskis jitterplot function, shows only points, as
;    lines/boxes are represented by graph-window-proto messages (assume!), so is
;    missed.

;                                  
;; where tuning may be done, marked ;;TUNING

(defmeth scatterplot-proto :2latex (file size origen 
                              &key (labelpos '(14 14))
                                   (border '(43 43))
                                   (tick-dec '(1 1)))
"Args: (file size origen &key labelpos border tick-dec) size of latex picture,
 and position of origin relative to lower left corner, in terms of latex \unitlength.
 Default values of arguments assumes that  \unitlength is points.
 size and origen are pairs, lists of two integers. 
 Labelpos gives position of point labels, relative to point.
 border is right and up border size. Tick-dec is number of decimals
 for tick marks.
 Writes latex picture environment to file; needs epic (and eepic)."
  (let* ((n (send self :num-lines))
         (np (send self :num-points))
         (have-points (> np 0))
         (have-lines (> n 0))
         (title (send self :title))
         (current (send self :current-variables))
         (x-var (first current))
         (y-var (second current))
         (x (if have-lines
                (send self :linestart-coordinate x-var (iseq n))))
         (y (if have-lines
                (send self :linestart-coordinate y-var (iseq n))))
         (ranx (send self :range x-var))
         (rany (send self :range y-var))
         (rx (- (first size) (first origen)))
         (ry (- (second size) (second origen)))
         (sx (/ (- (second ranx)(first ranx)) rx))
         (sy (/ (- (second rany)(first rany)) ry))
         (x (if have-lines 
                (/ (- x (first ranx)) sx)))
         (y (if have-lines 
                (/ (- y (first rany)) sy)))
         (pt (if have-lines 
                 (multiple-value-list
                     (travl x y (send self :linestart-next (iseq n))
                         (send self :linestart-masked (iseq n))
                         (send self :linestart-type (iseq n))))))
         (paths (if have-lines
                (first pt)) )
         (types (if have-lines
                (second pt)))
         (x-axis (send self :x-axis))
         (y-axis (send self :y-axis))
         (x-present (first x-axis))
         (y-present (first y-axis))
         (xlab-present (second x-axis))
         (ylab-present (second y-axis))
         (xtics        (third x-axis))
         (ytics        (third y-axis))
         (xlab (send self :variable-label x-var))
         (ylab (send self :variable-label y-var)))
    (flet ((drawpath (path type file)
              (let ((count 0))
                 (format file 
                      (if (eq type 'solid) "\\drawline" "\\drawline[-60]"))   ;;TUNING
                 (dolist (point path)
                    (incf count)
                    (format file "(~,3f,~,3f)" (first point) (second point))
                    (if (= 0 (rem count 5))
                        (format file "~%   ")))))
           (points2latex (x y file)
              (let* ((symbols (plot-symbol-symbols))
                     (symbols-latex (list "triangleright" "diamond" "Box"
                                         "circ" "+" "circ" "cdot" "circ"
                                         "times" "cdot" "circ" "triangleright"))
                     (labels (if (send self :showing-labels)
                                (send self :point-label (iseq np))
                                nil))
                     (selected (if labels
                                  (mapcar #'(lambda (a1 a2) (or a1 a2))
                                      (send self :point-hilited (iseq np))
                                      (send self :point-selected (iseq np)))
                                  nil))   
                     (showing (mapcar #'not (mapcar #'eql (repeat 'invisible np)
                                       (send self :point-state (iseq np)))))
                     (plotsyms (send self :point-symbol (iseq np)))
                     (xl2lasym (transpose (list symbols symbols-latex))))

                (mapc #'(lambda (x y showing plotsym)
                           (when showing
                              (let ((lasym (second (find plotsym xl2lasym :key #'first))))
                                (format file "  \\put(~,3f,~,3f){$\\~a$}~%" x y lasym))))
                           x y showing plotsyms)
                (when labels
                     (mapc #'(lambda (x y selected label)
                               (when selected
                                   (format file "  \\put(~,3f,~,3f){~a}~%" (+ x (first labelpos)) 
                                                                         (+ y (second labelpos))
                                                                         label)))
                          x y selected labels)) 
                ))
           (write-title (x y title file)
                  (format file "  \\put(~,3f,~,3f){~a}~%"  x y title))

           (write-xlabel (x y label file)
                  (let* ((len (length label))
                         (xx (- x (* 2 len))) )
                     (format file "  \\put(~,3f,~,3f){~a}~%" xx y label))) 

#| ylabel is more complicated, my dvi-driver doesnt support rotate, so try with
   shortstack:
|#
           (write-ylabel (x y label file)
                 (let* ((len (length label))
                        (yy (- y (* 3 len))))
                    (unless (= len 0)
                    (format file "  \\put(~,3f,~,3f){\\shortstack[l]{~a\\\\~%"
                                 x yy (elt label 0))
                    (dotimes (i (- len 2))
                          (let ((ii (+ 1 i)))
                             (format file "   ~a \\\\~%" (elt label ii))))
                    (when (> len 1)
                         (format file "   ~a }}~%" (elt label (- len 1)))))))

           (draw-tics (axis file)
                (let ((ntics (if (eq axis 'x) xtics ytics))
                      (dx (if (eq axis 'x) (/ (- (first size) (first origen))
                                              (- xtics 1))      0))
                      (dy (if (eq axis 'x) 0  (/ (- (second size) (second origen))
                                                 (- ytics 1)))) 
                      (xx (if (eq axis 'x) 0 -1))
                      (yy (if (eq axis 'x) -1 0))  )
                   (format file "  \\multiput(0,0)(~,3f,~,3f){~d}{\\line(~d,~d){3}}~%"
                           dx dy ntics xx yy)))   
           (write-ticknum (axis min max file)
               (let* ((ntics (if (eq axis 'x) xtics ytics))
                      (delta (/ (- max min) (- ntics 1))) 
                      (dx (if (eq axis 'x) (/ (- (first size) (first origen))
                                              (- xtics 1))      0))
                      (dy (if (eq axis 'x) 0  (/ (- (second size) (second origen))
                                                 (- ytics 1))))     )
                   (dotimes (i ntics)
                       (format file "  \\put(~,3f,~,3f){~,vf}~%"
                               (if (eq axis 'x) (- (* i dx) 5)     ;;TUNING
                                                (- 20))            ;;TUNING
                               (if (eq axis 'x) (- 15)             ;;TUNING
                                                (- (* i dy) 5) )   ;;TUNING
                               (if (eq axis 'x) (first tick-dec)
                                                (second tick-dec))
                               (+ min (* i delta))))))          )
                    
                    
    (with-open-file (bez file :direction :output)
       (format bez "\\begin{picture}(~d,~d)(~d,~d)~%"
               (+ (first size) (first border))
               (+ (second size) (second border))
               (- (first origen))(- (second origen)))   ; picture environment command

       (format bez "\\put(~d,~d){\\framebox(~d,~d)}~%" (- (first origen))(- (second origen))
               (+ (first size) (first border))
               (+ (second size) (second border)))               ; put a framebox 

       (when x-present
             (format bez "\\put(0,0){\\line(1,0){~d}}~%" 
                     (round (- (first size) (first origen)))))

       (when y-present
             (format bez "\\put(0,0){\\line(0,1){~d}}~%" 
                     (round  (- (second size) (second origen)))))           ; put axes

       (write-title  5                                               ;title    ;;TUNING
                     (- (+ (- (second size) (second origen)) 
                           (second border)) 
                        15)                                            ;;TUNING
                     title bez)
                                       ;; labels:
       (when xlab-present (write-xlabel (round (/ (- (first size) (first origen)) 2))
                                        (+ (- (second origen)) 5)         ;;TUNING
                                        xlab bez))

       (when x-present
             (draw-tics 'x bez)
             (write-ticknum 'x (first ranx) (second ranx) bez))

       (when ylab-present (write-ylabel (+ 5 (- (first origen)))             ;;TUNING
                                        (round (/ (- (second size) (second origen)) 2))
                                        ylab bez))

       (when y-present
             (draw-tics 'y bez)
             (write-ticknum 'y (first rany) (second rany) bez))


       (when have-lines
             (dolist2 (pa paths tps types)
                   (drawpath pa tps bez)
                   (format bez "~%")))
                
       (when have-points    
              (points2latex (/ (- (send self :point-coordinate x-var (iseq np)) (first ranx)) sx)
                     (/ (- (send self :point-coordinate y-var (iseq np)) (first rany)) sy)
                     bez))

        (format bez "\\end{picture}"))))
  'done)
         


;;;
;;; UTILITY FUNCTIONS:
;;;

;; constructs paths for drawing lines:
(defun travl (x y nexts masked linetypes)
  (let* ((n (length x))
         (visited masked)
         (lst () ) (start 0) (types nil))
    (flet ((makepath (start x y nexts visited)
                     (let ((path nil)
                           (vis (copy-list visited))
                           (i start)
                           (type (elt linetypes start)))
                       (loop
                        (if (null i) 
                            (return (values (nreverse path) vis type)))
                        (push (list (elt x i) (elt y i)) path)
                        (if (or (elt vis i) 
                                (not (eql type (elt linetypes i))))
                            (return (values (nreverse path) vis type)))
                        (setf (elt vis i) t)
                        (setf i (elt nexts i))))))
      (loop
       (multiple-value-bind (pa vis type) 
                            (makepath start x y nexts visited)
                            (setq visited vis)
                            (cond ((not (null-or-single pa))
                                   (push pa lst)
                                   (push type types))))
       (setq start (position nil visited))
       (if (not start)
           (return (values (nreverse lst) (nreverse types))))))))
 
;;;
;;; Macro for parallell traversing of two lists:
;;; This is slow, probably better to base it on #'mapcar?

(defmacro dolist2 ((term1 l1 term2 l2) &body body)
 "Args: ((term1 l1 term2 l2) &body body) As dolist, but
  traverses two lists parallelly. Should be of same length, if not
  stops when end of shortest, silently."
    (let ((i1 (gensym))
          (i2 (gensym)))
      `(do ((,i1 ,l1 (cdr ,i1))
            (,i2 ,l2 (cdr ,i2)))
           ((or (null ,i1) (null ,i2)))
           (let ((,term1 (car ,i1))
                 (,term2 (car ,i2)))
             ,@body))))

;;; A more general macro for parallell iteration over n lists:
;;; Used as dolist, but do not admit a special return clause in the
;;; iteration form, as by now. So returned values must be made in the
;;; body, by return. Example call:

#|
(dolistn (it1 (iseq 1 4) it2 '(-1 1 -1 1) it3 '(a b c d))
         (print (list it3 (^ it1 it2))))
|#

(defmacro dolistn ((&rest args) &body body)
  (let* ((n (/ (length args) 2))
         (syms (mapcar #'gensym (iseq n))))
    (labels ((make-initbind (itforms syms)
             (if (null itforms)
                 nil
                 
                 `((,(car syms) ,(second (car itforms)) (cdr ,(car syms)))
                 ,@(make-initbind (cdr itforms) (cdr syms)))))
             (make-letforms (itforms syms)
                 (if (null itforms)
                     nil
                     
                     `((,(first (car itforms)) (car ,(car syms)))
                      ,@(make-letforms (cdr itforms) (cdr syms)))))
             (make-testclause (syms)
                  (if (null syms)
                      nil
                      
                       `((null ,(car syms)) ,@(make-testclause (cdr syms))))))
                       
      `(do (,@(make-initbind (group args 2) syms) )
           ((or ,@(make-testclause syms)) )
           (let (,@(make-letforms (group args 2) syms) )
             ,@body)))))
  
;;; dolistn isnt used above, but could be used instead of two mapcar's.

;;; From Paul Graham, ``On Lisp'':

(defun single (lst)                      ;;; not used in program
  (and (consp lst) (not (cdr lst))))

(defun  null-or-single (lst)
  (and (listp lst) (not (cdr lst))))


;;; this should be compiled to work with large examples, as the
;;; compiler is properly tail recursive.
(defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                      (nreverse (cons source acc))))))
      (if source (rec source nil) nil)))


;;; examples:

#|

 (def x '(.1 .3 .5 .7 .9))

 (def y '(.9 .1 .35 .1 .9))

 (send w :add-points x y)

 (send w :add-lines x y)

 (send w :point-label 0 "A large w")

The command:

(send w :2latex "test.tex" '(200 200) '(20 20))

produces the .tex file:

\begin{picture}(200,200)(-20,-20)
\put(-20,-20){\framebox(200,200)}
\put(0,0){\vector(1,0){160}}
\put(0,0){\vector(0,1){160}}
\drawline(18.000,162.000)(54.000,18.000)(90.000,63.000)(126.000,18.000)(162.000,162.000)
   
\put(18.000,162.000){$\diamond$}
\put(54.000,18.000){$\diamond$}
\put(90.000,63.000){$\diamond$}
\put(126.000,18.000){$\diamond$}
\put(162.000,162.000){$\diamond$}
\end{picture}

|#

#|
  A suitable wrapper to test this is:

\documentclass[12pt, a4paper]{article}
                 % some of the constants in the program is tuned for 12pt, and will
                 % not (maybe) work well for different type sizes.
\usepackage[spanish]{babel}
\usepackage{epic,eepic}

\begin{document}

\input{test2}  % test2.tex is output given above

\end{document}
|#

#|
A larger example  of an tex file produced by this, by the commando:

(send qprobe :2latex "test3.tex" '(200 200) '(40 40) :tick-dec '(0 0))

tex file:

\begin{picture}(200,200)(-40,-40)
\put(-40,-40){\framebox(243,243)}
\put(0,0){\line(1,0){160}}
\put(0,0){\line(0,1){160}}
  \put(5.000,188.000){Number of clients waiting}
  \put(72.000,-35.000){Time}
  \multiput(0,0)(53.333,0.000){4}{\line(0,-1){3}}
  \put(-5.000,-15.000){0.}
  \put(48.333,-15.000){50.}
  \put(101.667,-15.000){100.}
  \put(155.000,-15.000){150.}
  \put(-35.000,62.000){\shortstack[l]{L\\
   e \\
   n \\
   g \\
   t \\
   h }}
  \multiput(0,0)(0.000,40.000){5}{\line(-1,0){3}}
  \put(-20.000,-5.000){0.}
  \put(-20.000,35.000){10.}
  \put(-20.000,75.000){20.}
  \put(-20.000,115.000){30.}
  \put(-20.000,155.000){40.}
\drawline(0.000,0.000)(1.046,4.000)(1.046,0.000)(1.706,0.000)(1.904,4.000)
   (1.904,0.000)(1.976,0.000)(3.879,4.000)(3.879,0.000)(4.042,0.000)
   (4.861,4.000)(4.861,0.000)(5.608,4.000)(7.117,0.000)(7.152,0.000)
   (8.184,4.000)(8.184,0.000)(10.267,4.000)(11.336,8.000)(12.398,12.000)
   (13.595,16.000)(14.522,20.000)(14.647,16.000)(14.723,12.000)(14.837,8.000)
   (15.283,12.000)(15.782,16.000)(16.850,20.000)(17.124,24.000)(18.553,28.000)
   (19.120,24.000)(19.583,20.000)(19.631,24.000)(19.694,20.000)(19.815,16.000)
   (19.932,12.000)(19.968,8.000)(19.970,4.000)(20.172,8.000)(20.260,4.000)
   (21.095,0.000)(21.097,0.000)(21.187,4.000)(21.187,0.000)(21.223,0.000)
   (21.805,4.000)(21.805,0.000)(22.335,4.000)(22.805,8.000)(23.302,12.000)
   (26.688,16.000)(27.416,12.000)(27.436,8.000)(27.441,4.000)(27.537,8.000)
   (28.121,4.000)(28.270,0.000)(28.293,0.000)(28.440,4.000)(28.440,0.000)
   (28.649,0.000)(29.355,4.000)(29.355,0.000)(29.506,0.000)(30.339,4.000)
   (30.339,0.000)(31.698,4.000)(32.813,8.000)(34.703,12.000)(35.015,16.000)
   (36.190,20.000)(36.558,24.000)(37.112,28.000)(38.042,32.000)(38.547,36.000)
   (38.683,40.000)(39.440,44.000)(39.984,48.000)(40.080,44.000)(40.081,40.000)
   (40.087,36.000)(40.087,32.000)(40.154,28.000)(40.213,24.000)(40.273,20.000)
   (40.374,16.000)(41.011,20.000)(41.593,24.000)(42.300,20.000)(42.302,16.000)
   (42.326,20.000)(43.718,16.000)(43.938,20.000)(45.573,16.000)(45.682,20.000)
   (47.370,24.000)(47.782,28.000)(48.557,32.000)(49.071,36.000)(49.557,40.000)
   (51.475,44.000)(52.416,48.000)(53.274,52.000)(53.785,48.000)(53.785,44.000)
   (53.785,40.000)(53.808,36.000)(53.812,32.000)(54.037,28.000)(55.620,32.000)
   (57.000,36.000)(57.231,40.000)(57.909,44.000)(59.083,48.000)(59.801,52.000)
   (61.572,56.000)(61.826,52.000)(61.827,48.000)(61.827,44.000)(62.139,48.000)
   (62.601,52.000)(63.079,56.000)(63.923,60.000)(64.637,64.000)(65.859,68.000)
   (66.331,64.000)(66.468,68.000)(67.166,72.000)(69.380,76.000)(69.891,80.000)
   (71.476,84.000)(72.287,88.000)(73.814,92.000)(74.043,96.000)(75.002,100.000)
   (76.178,104.000)(76.925,108.000)(77.761,112.000)(78.297,116.000)(78.676,120.000)
   (80.196,124.000)(81.298,128.000)(81.601,132.000)(82.147,136.000)(83.398,132.000)
   (83.416,128.000)(83.707,124.000)(84.275,128.000)(84.948,124.000)(85.078,120.000)
   (85.080,116.000)(85.291,120.000)(86.557,124.000)(86.569,120.000)(86.569,116.000)
   (86.824,120.000)(87.299,116.000)(87.599,112.000)(88.111,116.000)(88.709,112.000)
   (88.726,108.000)(88.986,112.000)(90.186,108.000)(90.196,104.000)(90.239,108.000)
   (91.662,104.000)(91.662,100.000)(91.936,104.000)(93.499,100.000)(93.570,104.000)
   (94.814,108.000)(95.263,112.000)(96.785,116.000)(98.073,120.000)(99.221,124.000)
   (100.139,128.000)(101.403,124.000)(101.404,120.000)(101.405,116.000)(101.799,112.000)
   (101.840,108.000)(102.247,112.000)(102.730,116.000)(104.103,112.000)(116.419,108.000)
   (116.828,104.000)(118.596,100.000)(118.614,96.000)(124.514,92.000)(124.551,88.000)
   (125.323,84.000)(125.325,80.000)(125.325,76.000)(125.332,72.000)(125.348,68.000)
   (127.913,64.000)(128.549,60.000)(129.124,56.000)(136.069,52.000)(136.871,48.000)
   (136.940,44.000)(137.048,40.000)(137.109,36.000)(137.109,32.000)(137.116,28.000)
   (137.163,24.000)(137.581,20.000)(137.712,16.000)(137.713,12.000)(137.850,8.000)
   (139.110,4.000)(139.857,0.000)(140.085,0.000)
\end{picture}

|#




