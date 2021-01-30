
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;
;;; programmed by Huh, Moon Yul
;;; Dept. of Statistics, Sungkyunkwan Univ., Seoul, Korea
;;; 
;;; parallel FEDF introduced in "Exploring Multidimensional 
;;; data with Flipped Empirical Distribution", 
;;; J. of Comp. & Graphical Stat.
;;; 
;;;
;;; example: notebook data
;;;
;;; (fedf-plot data :var-labels var-labels :color t
;;;           :labels labels :category category :title title)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def d-list (list ':o-data ':delete ':select ':link ':label ':screening))
(def dial 
     (mapcar #'(lambda (x) (string (format nil "~a"
         (string-left-trim ":" (string x))))) d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; prototype and methods for FEDF scatterplot
;;;
(defproto w '(data orders analysis-data labels my-order menu-sel corr-list
                   var-name p-list w-size  ncr x-pos r-data y-list 
                   copy-data copy-labels np category start-end num-loc
                   pts-so identify link associate pts start) () graph-proto)

(defmeth w :isnew (&rest args)
  (apply #'call-next-method args))
;;;
;;;data structure
;;;    ((x-list, y-list, #points), (x-list, y-list, #points),...)
;;;
(defmeth w :form (&optional (val nil set))
  (if set     
      (setf (slot-value 'form) val))
  (slot-value 'form))

(defmeth w :y-list (&optional (val nil set))
  (if set     
      (setf (slot-value 'y-list) val))
  (slot-value 'y-list))

(defmeth w :data (&optional (val nil set))
  (when set    
        (setf (slot-value 'data) val)
        (send self :analysis-data val))
  (slot-value 'data))

(defmeth w :copy-data (&optional (val nil set))
  (if set     
      (setf (slot-value 'copy-data) val))
  (slot-value 'copy-data))

(defmeth w :np (&optional (val nil set))
  (if set     
      (setf (slot-value 'np) val))
  (slot-value 'np))

(defmeth w :orders (&optional (data nil set))
  (if set     
      (setf (slot-value 'orders) 
            (mapcar #'order data)))
  (slot-value 'orders))

(defmeth w :analysis-data (&optional (val nil set))
  (when set
        (setf (slot-value 'analysis-data)
              (mapcar #'(lambda (data)
                          (let* ((x (sort-data data))
                                 (y (fedf x)))
                            (list x y))) val)))
  (slot-value 'analysis-data))

(defmeth w :var-name (&optional (val nil set))
  (if set     
      (setf (slot-value 'var-name) val))
  (slot-value 'var-name))

(defmeth w :labels (&optional (val nil set))
  (if set     
      (setf (slot-value 'labels) val))
  (slot-value 'labels))
          
(defmeth w :copy-labels (&optional (val nil set))
  (if set     
      (setf (slot-value 'copy-labels) val))
  (slot-value 'copy-labels))

(defmeth w :menu-sel (&optional (val nil set))
  (if set (setf (slot-value 'menu-sel) val))
  (slot-value 'menu-sel))
        
(defmeth w :start-end (&optional (val nil set))
  (if set (setf (slot-value 'start-end) val))
  (slot-value 'start-end))

(defmeth w :r-data (&optional (data nil set))
  (if set
      (setf (slot-value 'r-data)
            (mapcar #'rank data)))
  (slot-value 'r-data))

(defmeth w :num-loc (&optional (val nil set))
  (if set (setf (slot-value 'num-loc) val))
  (slot-value 'num-loc))


(defmeth w :ncr (&optional set)
  (if set 
      (let* ((n (length data))
             (np (send self :np))
             (nc (if (<= n np) n (send self :np)))
             (nr (ceiling (/ n nc))))
        (setf (slot-value 'ncr) (list nc nr))))
  (slot-value 'ncr))

(defmeth w :category (&optional (vali nil set))
  (if set (setf (slot-value 'category) val))
  (slot-value 'category))

(defmeth w :do-click (x y &rest args)
  (let ((hilite (send self :points-hilited))
        (menu (send self :menu-sel)))
    (apply #'call-next-method x y args)
    (if (> x (first (send self :margin)))
        (if (send self :selection)   
            (send self menu)
            (prog ()
                  (send self :unselect-all-points)
                  (send self :point-color 
                        (iseq (send self :num-points)) 'black)
                  (send self :redraw))))))

(defmeth w :redraw (&optional (click nil))
  (when (> (send self :num-points) 0)
        (send self :start-buffering)
        (call-next-method)
        (let ((menu (send self :menu-sel)))
          (if (and menu (null click))
              (cond
                ((member menu (list ':o-data ':delete ':select 
                                    ':link ':screening))
                 (send self :start-end nil))
                ((equal menu ':label)
                 (send self :label t))
                (t (send self menu t)))))
        (send self :line-pt)
        (send self :var-label)
        (send self :out-line)
        (send self :buffer-to-screen)))

(defmeth w :p-loc ()
"list of plot-locations (x, y, plot-width, plot-height)"
  (let* ((start (send self :make-start))
         (xy-off '(.1 .93))
         (start (mapcar #'(lambda (x)
                            (+ xy-off x)) start)))
    (mapcar #'(lambda (xy)
                (send self :real-to-canvas (first xy) (second xy)))
            start)))

(defmeth w :var-label ()
  (let* ((ta (1+ (send self :text-ascent)))
         (vn (send self :var-name))
         (p-loc (send self :p-loc)))
    (mapcar #'(lambda (x y)
                (send self :draw-string 
                      y (1+ (first x)) (+ (second x) ta))) p-loc vn)))

(defmeth w :out-line ()
  (let* ((sz (1- (send self :size)))
         (x (first sz))
         (y (second sz)))
    (send self :draw-line 0 0 x 0)
    (send self :draw-line x 0 x y)
    (send self :draw-line x y 0 y)
    (send self :draw-line 0 0 0 y)))

(defmeth w :point-labelling ()
  (let ((str (map-elements 
              #'(lambda (x)
                  (string (format nil "~a" x)))
              (combine (send self :orders))))
        (nm (send self :num-points)))
    (send self :point-label (iseq nm) str)))

(defmeth w :line-pt ()
  (let* 
    ((x-data (mapcar #'(lambda (x) (first x))
                     (send self :analysis-data)))
     (lx (length x-data))
     (l (length (first x-data)))
     (inp (iseq (send self :num-points)))
     (xp (send self :point-canvas-coordinate 0 inp))
     (yp (send self :point-canvas-coordinate 1 inp))
     (xp (split-list xp l))
     (yp (split-list yp l))
     (yp (- (send self :canvas-height) yp -5))
     (marg (first (send self :margin))))
    (dotimes (i lx)
             (let* 
               ((data (elt x-data i))
                (f-data (fivnum data))
                (cx (+ marg (elt xp i)))
                (cy (elt yp i))
                (f-cx (floor (fivnum cx)))
                (f-cx4 (- f-cx 4))
                (yp (max cy))
                (ypt (+ (send self :text-ascent) yp)))
               (send self :bottom-line i f-cx yp)
               (cond
                 ((member i (send self :category)) 
                  (let* ((cat (assoc i cat-list))
                         (cat (second cat))
                         (sel (remove-duplicates (select x-data i)))
                         (sel-cat (select cat sel))
                         (ls (length sel-cat))
                         (fc (fcx sel (first f-cx) (fifth f-cx))))
                    (dotimes (j (floor (/ ls 2)))
                             (send self :draw-str-i 
                                   sel-cat (* 2 j) (- fc 4) yp :category t)
                             (send self :draw-str-i 
                                   sel-cat (1+ (* 2 j)) (- fc 4) 
                                   (if (= 2 ls) yp ypt) :category t))
                    (if (oddp ls)
                        (send self :draw-str-i 
                              sel-cat (1- ls) (- fc 4) yp :category t))))
                 ((equal (fifth f-data) (first f-data)) 
                  (send self :draw-str-i f-data 2 f-cx4 yp))
                 ((> (fifth f-data) (first f-data))
                  (prog ()
                        (send self :draw-str-i f-data 0 (- f-cx 10) yp)
                        (send self :draw-str-i f-data 2 f-cx4 ypt)
                        (send self :draw-str-i f-data 4 f-cx4 yp))))))))

(defmeth w :bottom-line (i f-cx yp)
  (let* ((min (first f-cx))
         (max (fifth f-cx))
         (xs (-  min 5))
         (xe (+  max 5))
         (fc (if (member i (send self :category))
                 (fcx 
                  (remove-duplicates 
                   (first (select (send self :analysis-data) i)))
                  min max)
                 f-cx)))
    (send self :draw-line xs yp xe yp) 
    (dolist (x fc) 
            (send self :draw-symbol 'cross nil  x (1+ yp)))))


(defmeth w :draw-str-i (f-data i f-cx y &key (category nil))
  (let* ((xoff -10)
         (yoff 12)
         (x (floor (elt f-cx i)))
         (fi (elt f-data i))
         (form (if category "~a" "~5f"))
         (st (string (format nil form fi))))
    (send self :draw-string st (+ x xoff) (+ y yoff))))
                  
      
(defmeth w :adjust-screen-point (i)
  (let* ((state (send self :point-state i)))
    (send self :point-color i 'red)
    (call-next-method i)))

(defmeth w :pts-so (&optional set)
  "i: i-th points
so: index of the selected points of the main plot"
  (if set
      (let* ((data (send self :copy-data))
             (n (length (first data)))
             (pts (send self :points-selected))
             (fp (first pts))
             (i (floor (/ fp n)))
             (pts (- pts (* i n)))
             (o-indx (select (send self :orders) i))
             (so (select o-indx pts))
             (lst (list i pts so)))
        (setf (slot-value 'pts-so) lst)))
  (slot-value 'pts-so))

(defmeth w :o-data (&optional (redraw nil))
  (when (not redraw)
       (let ((n-data (send self :data))
             (n-labels (send self :labels)))
         (send self :copy-data n-data)
         (send self :analysis-data n-data)
         (send self :orders n-data)
         (send self :r-data n-data)
         (send self :copy-labels n-labels)))
  (send self :draw-plot))

(defmeth w :delete (&optional (redraw nil))
  (when (not redraw)
        (let* ((data (send self :copy-data))
               (n (length (first data)))
               (lst (send self :pts-so t))
               (so (third lst))
               (seq (delete-list (iseq n) so))
               (c-labels (send self :copy-labels))
               (n-labels (select c-labels seq))                              
               (n-data
                (mapcar 
                 #'(lambda (j)
                     (select (elt data j) seq))
                 (iseq (length data)))))
          (send self :copy-data n-data)
          (send self :analysis-data n-data)
          (send self :orders n-data)
          (send self :r-data n-data)
          (send self :copy-labels n-labels)))
  (send self :draw-plot))

(defmeth w :select (&optional (redraw nil))
  (when (not redraw)
        (let* ((data (send self :copy-data))
               (n (length (first data)))
               (lst (send self :pts-so t))
               (seq (third lst))
               (c-labels (send self :copy-labels))
               (n-labels (select c-labels seq))                              
               (n-data
                (mapcar 
                 #'(lambda (j) 
                     (select (elt data j) seq))
                 (iseq (length data)))))
          (send self :copy-data n-data)
          (send self :analysis-data n-data)
          (send self :orders n-data)
          (send self :r-data n-data)
          (send self :copy-labels n-labels)))
  (send self :draw-plot))


(defmeth w :link (&optional (redraw nil))
  "y-index: index of the selected points of the other plots"
  (if (send self :selection)
      (let* ((data (send self :copy-data))
             (n (length (first data)))
             (lst (send self :pts-so t))
             (i (first lst))
             (so (third lst))
             (j-list (delete i (iseq (length data))))
             (sel-list 
              (mapcar 
               #'(lambda (j)
                   (+ (select (rank (select data j)) so) (* j n)))
               j-list)))
        (send self :point-color (send self :selection) col)
        (send self :point-color sel-list  col)
        (send self :point-state sel-list 'hilited)
        (send self :point-state (send self :selection) 'selected))))
          
(defmeth w :label (&optional (redraw nil))
  (when (send self :selection)
        (send self :link redraw)
        (send self :labels-draw)))

(defmeth w :labels-draw ()
  (let* ((so (third (send self :pts-so)))
         (lab-sel (select (send self :copy-labels) so))
         (so-lab (transpose (list so lab-sel)))
         (box (send self :text-ascent))
         (gap (round (/ box 4)))
         (bottom (- (second (send self :size)) gap)))
    (mapcar #'(lambda (i x)
       (let* ((y (- bottom (* i (+ gap box))))
              (str (string (format nil "~3d:~6a" 
                              (first x) (second x)))))
         (send self :draw-string str 0 y))) 
            (iseq (length so-lab)) so-lab)))


(defmeth w :up-locate (&optional (redraw nil))         
  (let* ((num-loc (send self :num-loc))
         (st-end (send self :start-end))
         (len (send self :data-length))
         (start (if st-end (first st-end) 0))
         (end (if st-end (second st-end) 
                  (min (1- num-loc) (1- len)))))
    (if redraw (send self :start-end (list start end))
        (if (or (null st-end) (= end (1- len)))
            (send self :start-end (list start end))
            (send self :start-end
                  (list (1+ end) (min (+ end num-loc) (1- len))))))
    (send self :locate-labels-draw redraw)))

(defmeth w :data-length ()
 (length (first (first (send self :analysis-data)))))

(defmeth w :down-locate (&optional (redraw nil))
  (let ((st-end (send self :start-end)))
    (when st-end
          (if redraw ()           
           (let* ((num-loc (send self :num-loc))
                  (len (send self :data-length))                        
                  (start (max 0 (- (first st-end) num-loc)))
                  (end (min (+ start num-loc -1) (1- len))))
             (send self :start-end (list start end))))))
  (send self :locate-labels-draw redraw))

(defmeth w :locate-labels-draw (&optional (redraw nil))
  (if (not redraw) (send self :y-list nil))
  (let* ((st-end (send self :start-end))
         (start (first st-end))
         (end (second st-end))
         (len (- end start -1))
         (labels (send self :copy-labels))
         (sz-y (second (send self :size)))
         (box (send self :text-ascent))
         (gap (round (/ box 4)))
         (gb (+ gap box)))
    (dotimes (i len)
             (let* ((si (+ start i))
                    (label (elt labels si))
                    (str (string (format nil " ~2d:~a" si label)))
                    (y (- sz-y (* i gb))))
               (send self :draw-string str gap (- y gap))))
    (if (and (send self :y-list) redraw) (send self :locate-point))))

(defmeth w :locate-point ()
  (let* ((box (send self :text-ascent))
         (gap (round (/ box 4)))         
         (gb (+ gap box))
         (ys (second (send self :size)))
         (start (first (send self :start-end)))
         (np (send self :y-list))
         (yy (- ys (* (- np start) gb) (/ gb 2))))
    (dolist (y yy)
            (send self :draw-symbol 'dot4 t 7 y))
    (send self :point-state (send self :link-points np) 'selected)))
  
(defmeth w :link-points (pt)
  (let* ((data (send self :copy-data))
         (n (length (first data)))
         (r-data (send self :r-data)))
    (mapcar #'(lambda (j)
                (let* ((j-data (select r-data j))
                       (j-indx (select j-data pt)))
                  (+ j-indx (* j n))))
            (iseq (length data)))))

(defmeth w :y-points (y)
  (let* ((box (send self :text-ascent))
         (gap (round (/ box 4)))         
         (gb (+ gap box))
         (ys (second (send self :size)))
         (st-end(send self :start-end))
         (st (first st-end))
         (end (second st-end))
         (py (- ys y))
         (np (floor (/  py gb)))
         (np (+ np st)))
    (when (<= np end)
          (send self :y-list (cons np (send self :y-list)))
          (send self :locate-point))))

(defmeth w :win-lines ()
  (let* ((ncr (send self :ncr))
         (nc (first ncr))
         (nr (second ncr)))
    (dotimes (i nr)
             (send self :add-lines 
                   (list (list 0 nc) (list i i)) :draw nil))
    (dotimes (i nc)
             (send self :add-lines 
                   (list (list i i) (list 0 nr)) :draw nil))))

(defmeth w :win-points ()
;;;draw points in each windows
  (let* ((data (send self :analysis-data))
         (start (send self :make-start t)))
    (mapcar #'(lambda (d s)
                (let ((wd (win-standard d s)))
                  (send self :add-points wd :draw nil)))
            data start)))

     
(defmeth w :make-start (&optional set)
;;;starting position of each fedf plots
  (if set
      (setf (slot-value 'start)
            (let* ((n (length (send self :analysis-data)))
                   (ncr (send self :ncr))
                   (nc (first ncr))
                   (nr (second ncr)))
              (mapcar #'(lambda (i) 
                          (let* ((x (rem i nc))
                                 (y (floor (/ i nc))))
                            (list x y))) (iseq n)))))
  (slot-value 'start))

(defmeth w :draw-plot ()
  (send self :clear :draw nil)
  (send self :win-points)
  (send self :win-lines)
  (send self :point-labelling)
  (send self :adjust-to-data))


(defmeth w :screening (&optional (redraw nil))
  (when (send self :selection)
        (send self :link)
        (let* ((show (send self :points-showing))
               (sel1 (send self :selection))
               (sel2 (send self :points-hilited))
               (sel (append sel1 sel2))
               (inv (set-difference show sel)))
          (send self :point-state inv 'invisible)
          (send self :redraw))))



(defun win-standard (data &optional (start '(0 0)) (scale '(.5 .6)))
;;;standardize a plot to be inside of (start (+ 1 start))
  (let* ((n (length (first data)))
         (min (mapcar #'min data))
         (max (mapcar #'max data))
         (range (- max min))
         (st (+ start (* .5 (- 1 scale))))
         (inside 
          (mapcar 
           #'(lambda ( d sc m x)
               (if (zerop x) (repeat .25 n)
                   (* sc (/ (- d m) x))))
           data scale min range)))
    (+ st inside)))

(defun fcx (data min max)
  (let* ((range (- max min))
         (ncat (- (max data) (min data)))
         (gap (if (zerop ncat) 0 (/ range ncat))))
    (mapcar #'(lambda (j) 
                (floor (+ min (* j gap)))) (iseq (1+ ncat)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; overlays for FEDF scatterplot
;;;

(defproto o '(m-list loc  o-menu xy) () graph-overlay-proto)
;;;				
(defmeth o :redraw ()
  (let* ((graph (send self :graph))
        (menu (send graph :menu-sel)))
    (send self :draw-menu)
    (send self :mark)))
  
(defmeth o :do-click (x y m n)
  (let ((graph (send self :graph)))
    (if (< x (first (send graph :margin)))
        (let* ((loc (send self :menu-loc))
               (gap (first loc))
               (box (second loc))
               (tw (third loc))
               (gb (+ gap box))
               (ygb (/ y gb))
               (ni (floor ygb )))
          (send self :menu-click y ni)))))

(defmeth o :menu-click (y ni)
  (let ((graph (send self :graph)))
    (if (>= ni (length d-list))
        (send graph :y-points y)        
        (let ((menu (select d-list ni)))
          (send graph :redraw t)
          (send graph :menu-sel menu)
          (send self :mark)
          (if (member (send graph :menu-sel)
                      (list ':label ':up-locate ':down-locate))
              (send graph :showing-labels t)
              (send graph :showing-labels nil))
          (if (member menu (list ':o-data ':up-locate ':down-locate))
              (send graph menu))))))

(defmeth o :menu-loc (&optional (set nil))
;;i-th menu location:
;;; box: (gap, gap + (gap + box) * i)
;;; string: gap + box + gap, (box-y)
;;;
  (if set 
      (let* ((graph (send self :graph))
             (box (send graph-proto :text-ascent))
             (tw (send graph-proto :text-width "0123456789"))
             (gap (round (/ box 4))))
        (setf (slot-value 'loc) (list gap box tw))))
  (slot-value 'loc))

(defmeth o :isnew (&rest args)
  (apply #'call-next-method args)
  (send self :menu-loc t))

(defmeth o :draw-menu ()
  (let* ((graph (send self :graph))
         (loc (send self :menu-loc)))
    (dotimes (i (length dial))
     (let* ((x (first loc))
            (box (second loc))
            (tw (third loc))
            (y (+ x (* i (+ x box)))))
       (send graph :frame-rect x y box box )
       (send graph :draw-string 
	      (select dial i) (+ x box x) (+ box y))))))

(defmeth o :mark ()
"mark cross at the menu-selected"
  (let* ((graph (send self :graph))
         (menu (send graph :menu-sel))
         (o-menu (slot-value 'o-menu)))
    (send self :do-mark o-menu nil)
    (setf (slot-value 'o-menu) menu)
    (send self :do-mark menu t)))

(defmeth o :do-mark (menu &optional (mark nil))
  (when menu
        (let* ((graph (send self :graph))
               (loc (send self :get-position menu))
               (x (first loc))
               (y (second loc))  
               (map (if mark check blank)))
          (send graph :draw-bitmap map x y))))

(defmeth o :get-position (menu)
  (let* ((graph (send self :graph))
         (box (send graph :text-ascent))
         (gap (round (/ box 4)))
         (i (position menu d-list))
         (x (+ 2 gap))
         (y (+ 2 gap (* i (+ gap box)))))
    (list x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions for FEDF scatterplot
;;;
(defun delete-list (list d)
;;delete d from the list
  (let ((x (set-difference list d)))
    (reverse x)))

(defun labels (n &optional labels)
  (if labels labels
      (mapcar #'(lambda (x) (string (format nil "~s" x)))
              (iseq n))))

(defun var-labels (p &optional labels)
  (gensym "var-")
  (if labels labels
      (mapcar 
       #'(lambda (x) (string (gensym x))) (iseq 1 p))))
         
(defun transf (xy theta &key (tx 0) (ty 0) (sx 1) (sy 1))
  "transform (x,y) with the transformation matrix
((sx 0 0) (0 sy 0) (tx ty 1))
and
((cos sin 0) (-sin cos 0) (0 0 1))"
  
  (let* ((t1 (list (cos theta) (sin theta)))
         (t2 (list (- (sin theta)) (cos theta)))
         (m (bind-rows t1 t2))
         (fxy (first xy))
         (sxy (second xy))
         (sxy (+ .03 sxy))
         (txy (repeat 1 (length fxy)))
         (x (bind-columns fxy sxy txy))
         (sxy (bind-rows (list sx 0 ) (list 0 sy ) 
                         (list (* sx tx) (* sy ty ))))
         (tm (matmult sxy m))
         (xm (matmult x tm)))
    (transpose (mat-to-list xm))))

(defun correlation (x y)
  (flet ((sxy (x y)
              (let ((xy (inner-product x y)))
                (- xy (* (length x) (mean x) (mean y))))))
    (let ((ss (sqrt (* (sxy x x) (sxy y y)))))
      (if (zerop ss) 0 
          (/ (sxy x y) ss)))))

(defun correlation-matrix (data)
;;;correlation-matrix of a list
;;;gives only the lower left side
;;;
  (let ((p (length data)))
    (mapcar 
     #'(lambda (i)
         (mapcar 
          #'(lambda (j)
              (cond 
                ((> i j)
                 (correlation (select data i) (select data j)))
                ((= i j) 1)
                (t 0)))
          (iseq p)))
     (iseq p))))

(defun lower-right-corr (data)
;;upper left part of correlation matrix for data list
;;will be used as lower right part for the FEDF-scatterplot
;;
  (let* ((p (length data))
         (corr (transpose (correlation-matrix data)))
         (corr (make-array (List p p) :initial-contents corr))
         (st (first(make-start p))))
    (mapcar #'(lambda (x) (apply #'aref corr x)) st)))
  
(defun mat-to-list (x)
  (mapcar #'(lambda (d) (coerce d 'list))
          (column-list (transpose x))))

(defun make-tn (p &aux (val t))
"for p=3 case,  make (t nil t nil nil t)"
  (dolist (i (iseq 1 (1- p)) val)
          (let* ((rn (repeat nil i))
                 (cnt (combine rn t)))
            (setf val (combine val cnt)))))

(defun diag (p)
  (let* ((d (make-tn p))
         (t-n (mapcar #'(lambda (x) (if x 1 0)) d))
         (c-s (- (cumsum t-n) 1)))
    (mapcar #'(lambda (x y) (if x y nil)) d c-s)))

(defun make-start (p &aux val)
"returns starting position of p*p matrix
such that u:upper-left '((0 1) (0 2) (1 2) (0 3) (1 3) (2 3)...
          d: diagonal '((0 0) (1 1) (2 2) ,,,
          l: lower-right '((1 0) (2 0) (2 1) ..."    
   (flet ((is (k)
             (mapcar #'(lambda (j) (list j (1- k))) (iseq k))))
     (let* ((Val
             (dotimes (i p val)
                      (setf val (append val (is (1+ i))))))
            (dp (diag p))
            (d (which dp))
            (u (mapcar #'not dp))
            (u (which u))
            (d (Select val d))
            (u (select val u))
            (l (mapcar #'(lambda (x) (list (second x) (first x))) u)))
       (list u d l))))

(defun edf (data)
  (let* ((n (length data))
         (th (/ 3))
         (den (+ n th)))
    (/ (- (iseq 1 n) th) den)))

(defun fedf (data &key normal)
  (if normal (z-edf data)
      (let* ((n (length data))
             (ef (edf data)))
        (mapcar #'(lambda (y)
                    (if (> y .5) (- 1 y) y))
                ef))))


(defun make-y (data height)
  (let* ((sm (sort-data data))
         (n (length sm))
         (imed (ceiling (/ n 2)))
         (med (elt sm (1- imed)))
         (th 0.33333)
         (den (+ n th))
         (p 
          (mapcar #'(lambda (i)
                      (let ((f (/ (- i th) den))
                            (x (elt sm (1- i))))
                        (if (<= x med) f (- 1 f)))) (iseq 1 n))))
    (* p (/ height (max p)))))

(defun z-edf (data)
"normal-quantiles for data"
  (let* ((ef (edf data))
         (nf (normal-quant ef))
         (nf+ (mapcar #'(lambda (x)
                         (if (<= x 0) x (- x))) nf)))
    (+ .5 (* nf+ (/ .5 (max (abs nf+)))))))


(defun vedf (data &key (vedf t))
":vedf , if yes V-shaped if not FEDF
(data, (v-shaped fedf))"
  (let* ((ef (fedf data))        
        (y  (if vedf (- .5 ef) ef)))
    (list (sort-data data) y)))

(defun paired-vedf (data &key (vedf t))
  (let* ((u (first data))
         (l (second data))
         (vu (vedf u :vedf vedf))
         (vl (vedf l :vedf vedf))
         (data (append (first vu) (first vl)))
         (v (append (second vu) (- (+ (second vl) .02)))))
    (list data v)))
         
(defun plot-paired-vedf (data &key (vedf t))
  (let* ((p-data (paired-vedf data :vedf vedf))
         (fv (fivnum (first p-data)))
         (max (fifth fv))
         (min (first fv))
         (off (/ (- max min) (length (first data))))
         (min (- min off))
         (max (+ max off))
         (w (send graph-proto :new 2)))
    (send w :add-points p-data :draw nil)
    (send w :add-lines (list (list min max) '(0 0)))
    (send w :adjust-to-data :draw nil)
    (send w :y-axis t nil 5 :draw nil)
    (send w :range 1 -.5 .5)))

(defun my-rank (data)
;;; for tied observations
  (let* ((n-same  
          (mapcar #'(lambda (x) (count t (= x data))) data))
         (n-small 
          (mapcar #'(lambda (x) (count t (> x data))) data))
         (rank (+ n-small (/ (- n-same 1) 2))))
    rank))


(def check '#2a(       (  1 0 0 0 0 1   )
                       (  0 1 0 0 1 0   )
                       (  0 0 1 1 0 0   )
                       (  0 0 1 1 0 0   )
                       (  0 1 0 0 1 0   )
                       (  1 0 0 0 0 1   )))
(def blank '#2a(       (  0 0 0 0 0 0  )
                       (  0 0 0 0 0 0  )
                       (  0 0 0 0 0 0  )
                       (  0 0 0 0 0 0  )
                       (  0 0 0 0 0 0  )
                       (  0 0 0 0 0 0  )))


(defun parallel-fedf (data &key var-labels labels np category 
                       (color nil) (title "FEDFs") (symbol 'disk))

" data: the only required parameter.
  var-labels : labels for variables
  labels : labels for observations
  np: number of plots /horizon
  category: variable numer for categorical data
  data structure:
  ((x-list, y-list, #points), (x-list, y-list, #points),...)"

  (close-all-plots) 
  (let* ((m (length data))
         (n (length (first data)))
         (np 
          (if np np
              (cond 
               ((<= m 3) m)
               ((<= m 6) (ceiling (/ m 2)))
               ((<= m 9) (ceiling (/ m 3)))
               ((<= m 16) (ceiling (/ m 4)))
               (t (error "too many plots to draw")))))
         (xc (gensym 0))
         (var-labels 
          (if var-labels  var-labels
              (mapcar #'(lambda (x) 
                          (string (gensym"x-"))) (iseq m))))
         (xc (gensym 0))        
         (labels
          (if labels labels
              (mapcar #'(lambda (x)
                          (string (gensym "obs-"))) (iseq n))))
         (obj (send w :new 2 :data data :var-name var-labels 
                   :title title :labels labels :copy-data data 
                   :copy-labels labels
                   :num-loc 15 :np np :category category))
         (ov (send o :new :menu-loc t)))
    (setf col 'red)
    (send obj :orders data)
    (send obj :r-data data)
    (send obj :analysis-data (send obj :copy-data))
    (if color
        (send obj :use-color 't))
    (send obj :add-overlay ov)
    (send obj :margin 100 0 0 0)
    (send obj :ncr t)
    (send obj :draw-plot)
    (send obj :x-axis nil)
    (send obj :y-axis nil)
    (send obj :point-symbol (iseq (send obj :num-points)) symbol)))
