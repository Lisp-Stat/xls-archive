;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              ;;;
;;;

(defun star-plot (data &key var-labels labels  (height .27) 
                      (title "FEDF star-plot") (symbol 'disk))
  (gensym 0) 
  (let* ((p (length data))
         (var-labels (var-labels p var-labels))
         (n (length (first data)))
         (labels (labels n labels))
         (dial (list ':connect-median ':connect-quartile 
                     ':fivenum-points ':link-subset ':subset-line))
         (p (send fedf-star :new 2 :data data :titles var-labels 
                  :labels labels :title title)))
    (setf ov (send o :new))
    (send ov :dial dial)
    (send p :has-v-scroll nil)
    (send p :has-h-scroll nil)
    (send p :add-points-transf height symbol)
    (send p :add-lines-transf)
    (send p :add-overlay ov)
    (send p :my-label)
    (send p :adjust-to-data :draw nil)
    (send p :margin 120 50 10 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions for star-fedf plot
;;; some functions are not needed for the plot
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
  
(defun win-standard (start pt &optional (scale '(.7 .6)))
  (let* ((fedf? (if (= (first start) (second start)) t nil))
         (pt (if fedf? (fedf-pts (first pt)) pt))
         (min (mapcar #'min pt))
         (max (mapcar #'max pt))
         (range (- max min))
         (range (mapcar #'(lambda (x) (if (zerop x) .5 x)) 
                        range))
         (st (+ start (* .5 (- 1 scale)))))
    (+ st (* scale (/ (- pt min) range)))))

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
  (let* ((c-data (copy-list data))
         (sm (sort c-data '<))
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

(defun vedf (data &key vedf normal)
  (let ((ef (fedf data :normal normal))
        (off .01))
    (+ off (if vedf (- .5 ef) ef))))

(defun z-edf (data)
"normal-quantiles for data"
  (let* ((ef (edf data))
         (nf (normal-quant ef))
         (nf+ (mapcar #'(lambda (x)
                         (if (<= x 0) x (- x))) nf)))
    (+ .5 (* nf+ (/ .5 (max (abs nf+)))))))


(defun fedf-pts (data &optional height)
  (let* ((y (if height (make-y data height)
                (fedf data)))
         (c-data (copy-list data))
         (x (sort c-data '<)))
    (list x y)))

(defun my-rank (data)
;;; for tied observations
  (let* ((n-same  
          (mapcar #'(lambda (x) (count t (= x data))) data))
         (n-small 
          (mapcar #'(lambda (x) (count t (> x data))) data))
         (rank (+ n-small (/ (- n-same 1) 2))))
    rank))

(defun color-symbols () '(BLUE CYAN red BLACK YELLOW GREEN MAGENTA))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; prototype and methods for star-fedf plot
;;;
(defproto fedf-star '(data menu-sel titles labels subset-median sel-indx) 
  () graph-proto)

(defmeth fedf-star :isnew (&rest args)
  (apply #'call-next-method  args))

(defmeth fedf-star :data (&optional (data nil set))
  (if set 
      (setf (slot-value 'data) data))
  (slot-value 'data))


(defmeth fedf-star :labels (&optional (val nil set))
  (if set 
      (setf (slot-value 'labels) val))
  (slot-value 'labels))

(defmeth fedf-star :titles (&optional (titles nil set))
  (if set 
      (setf (slot-value 'titles) titles))
  (slot-value 'titles))

(defmeth fedf-star :menu-sel (&optional (data nil set))
  (if set 
      (setf (slot-value 'menu-sel) data))
  (slot-value 'menu-sel))

(defmeth fedf-star :subset-median (&optional (data nil set))
  (if set 
      (setf (slot-value 'subset-median) 
            (cons (median data) (slot-value 'subset-median))))
  (slot-value 'subset-median))

(defmeth fedf-star :indx-of-selection ()
  (let* ((data (send self :data))
         (sel (send self :selection))
         (n (length (first data)))
         (i (floor (/ (first sel) (+ n 5))))
         (x (- sel (* i (+ n 5))))
         (sel (select x (which (< x n))))
         (data-i (select data i))
         (o-i (order data-i)))
    (select o-i sel)))

(defmeth fedf-star :c-line (pt)  
  (let* ((data (send self :data))
         (f-data (first data))
         (l (length data))
         (fl (length f-data))
         (seq (- (* (iseq 1 l) (+ fl 5)) pt))
         (x (send self :point-coordinate 0 seq))
         (y (send self :point-coordinate 1 seq)))
    (send self :add-lines (list x y) :draw nil :type 'dashed)
    (send self :add-lines (list (list (elt x (1- l)) (first x))
                                (list (elt y (1- l)) (first y)))
          :draw nil :type 'dashed)))

(defmeth fedf-star :add-points-fedf (data theta height symbol)
"drawing range of x: (0, 1)
points lie: (.2, 1.2)
height: height of the median point"
  (let* ((d (send self :data))
         (l (length d))
         (tx (/ (* 2 pi) l))         
         (min (min data))
         (max (max data))
         (sx (- max min))
         (data (fedf-pts data height))
         (n-data (transf data (- theta)
                         :sx (/ sx)
                         :sy (sin tx)
                         :tx (+ (* 0.2 sx) (- min))))
         (np (send self :num-points)))
    (send self :add-points n-data :draw nil)
    (send self :point-symbol 
          (iseq np (1- (send self :num-points))) symbol)))


(defmeth fedf-star :add-lines-transf ()
  (let* ((data (send self :data))
         (l (length data))
         (theta (/ (* 2 pi) l))
         (thetas (* theta (iseq l)))
         (r 1.4)
         (x (* r (cos thetas)))
         (y (* r (sin thetas)))
         (val (/ 192 252)))
    (make-color 'grey val val val)
    (mapcar #'(lambda (x1 y1)
                (send self :add-lines 
                      (list (list 0 x1) (list 0 y1)) 
                      :width 2 :color 'grey
                      :draw nil)) x y)))

(defmeth fedf-star :add-five-points (data theta)
  (let* ((fn (fivnum data))
         (min (elt fn 0))
         (max (elt fn 4))
         (range (- max min))
         (x (+ 0.2 (mapcar #'(lambda (x) (/ (- x min) range)) fn)))
         (xy (list x (repeat 0 5)))
         (xy (transf xy (- theta)))
         (np (send self :num-points)))
    (send self :add-points xy :draw nil)
    (send self :point-symbol (iseq np (+ 4 np)) 'cross)
    (send self :point-state (iseq np (+ 4 np)) 'invisible)))

(defmeth fedf-star :add-points-transf (height symbol)
  (let* ((data (send self :data))
         (l (length data))
         (theta (/ (* 2 pi) l))
         (thetas (* theta (iseq l))))
    (mapcar 
     #'(lambda (d tx)
         (send self :add-points-fedf d tx height symbol)
         (send self :add-five-points d tx)) data thetas)))


(defmeth fedf-star :my-clear ()
  (setf (slot-value 'sel-indx) nil)
  (send self :point-color 
        (iseq (send self :num-points)) 'black)
  (send self :clear-lines :draw nil)
  (send self :add-lines-transf)
  (if (member ':connect-quartile (send self :menu-sel))
      (send self :connect-quartile t))
  (if (member ':connect-median (send self :menu-sel))
      (send self :connect-median t)))
             

(defmeth fedf-star :add-selected-indx (&optional indx)
  (if indx     
      (setf (slot-value 'sel-indx) 
	(append  (slot-value 'sel-indx) (list indx))))
  (slot-value 'sel-indx))

(defmeth fedf-star :first-select ()
  (let* ((n (length (first (send self :data))))
         (sel (send self :selection)))
    (select sel (which (>= n sel)))))

(defmeth fedf-star :my-point-label 
  (seq symb &key x-off y-off left-right above-below)
"draw seq of symbols at x-off y-off and 
left-or-right-justified, above-or-below the points"
  (let* ((co (send self :content-rect))
         (mar (send self :margin))
         (xp (send self :point-canvas-coordinate 0 seq))
         (yp (send self :point-canvas-coordinate 1 seq))
         (yp (- (fourth co) yp))
         (xp (+ xp (first mar)))
         (yp (+ yp (second mar)))
         (sx (first co))
         (sy (second co))
         (len (length seq))
         (x-off (if x-off x-off (repeat 0 len)))
         (y-off (if y-off y-off (repeat 0 len)))
         (left-right (if left-right left-right (repeat 0 len)))
         (above-below (if above-below above-below (repeat 0 len))))
    (mapcar #'(lambda (s x y x-o y-o l a)       
                (send self :draw-text s (+ x x-o) (+ y y-o) l a))
            symb xp yp x-off y-off left-right above-below)))
             

(defmeth fedf-star :my-label ()
  (let* ((data (send self :data))
         (len (+ 5 (length (first data))))
         (label (mapcar #'(lambda (x)
                            (list (order x) (iseq 5))) data))
         (label (mapcar #'(lambda (x) (format nil "~s" x)) 
                        (combine label))))
    (send self :point-label (iseq (send self :num-points)) label)))

(defmeth fedf-star :draw-titles ()
  (let* ((titles (send self :titles))
         (data (send self :data))
         (l (length data))
         (seq (- (* (+ 5 (length (first data))) (iseq 1 l)) 6))
         (x-off (combine (repeat 15 (floor (/ l 2)))
                         (repeat -10 (ceiling (/ l 2)))))
         (y-off (combine -10 (repeat 10 (floor (/ l 2)))
                         (repeat -10 (- l (ceiling (/ l 2))))))
         (theta (/ (* 2 pi) l))
         (thetas  (* theta (iseq l)))
         (l-r (repeat 1 l))
         (a-b (mapcar 
               #'(lambda (x)
                   (if (and (< 0 x) (<= x pi)) 1 0)) thetas)))
    (send self :my-point-label seq titles
          :x-off x-off
          :y-off y-off
          :left-right l-r
          :above-below a-b)))


(defmeth fedf-star :redraw (&rest args)
  (when (> (send self :num-points) 0)
        (send self :start-buffering)
        (apply #'call-next-method args)
        (send self :draw-titles)
        (if (and (send self :selection) 
                 (member ':labelling (send self :menu-sel)))
            (send self :labelling t))
        (send self :buffer-to-screen)))

(defmeth fedf-star :do-click (x y &rest args)
  (let ((pts (send self :points-hilited)))
    (apply #'call-next-method x y args)
    (if (> x (first (send self :margin)))
        (if (send self :selection)
            (prog ()
                  (mapcar #'(lambda (x) (send self x t))
                          (reverse (send self :menu-sel)))
                  (if (member ':link-subset (send self :menu-sel))
                      (send self :point-hilited
                            (append (send self :selection) pts) t :draw nil)))
            (send self :my-clear)))
    (send self :redraw)))

(defmeth fedf-star :fivenum-points (&optional visible)
  (let* ((data (send self :data))
         (l (length data))
         (n (length (first data)))
         (normal? (if visible 'normal 'invisible)))
    (dotimes (i l)
             (let* ((st (+ (* 5 i) (* n (1+ i))))
                    (end (+ st 4)))
               (send self :point-state (iseq st end) normal?)))))
  
(defmeth fedf-star :connect-median (&optional set)
  (if set
      (send self :c-line 3)
      (prog ()
            (send self :clear-lines :draw nil)
            (send self :add-lines-transf)
            (if (member ':connect-quartile (send self :menu-sel))
                (send self :connect-quartile t))
            (if (member ':subset-line (send self :menu-sel))
                (send self :subset-line t)))))

(defmeth fedf-star :connect-quartile (&optional set)
  (if set
      (prog ()
            (send self :c-line 2)
            (Send self :c-line 4))
      (prog ()
            (send self :clear-lines :draw nil)
            (send self :add-lines-transf)
            (if (member ':connect-median 
                        (send self :menu-sel))
                (send self :connect-median t))
            (if (member ':subset-line (send self :menu-sel))
                (send self :subset-line t)))))

(defmeth fedf-star :subset-line (&optional set)
  (if (and set (send self :selection))
      (let* ((d (send self :data))
             (l (length d))
             (theta (/ (* 2 pi) l))
             (thetas (* theta (iseq l)))
             (med (reverse (send self :subset-median)))
             (med-xy 
              (mapcar 
               #'(lambda (i m tx)
                   (let* ((data (select d i))
                          (min (min data))
                          (max (max data))
                          (sx (- max min))
                          (xy (list (list m) '(0))))
                     (transf xy (- tx)
                             :sx (/ sx)
                             :sy (sin tx)
                             :tx (+ (* 0.2 sx) (- min)))))
               (iseq l) med thetas))
             (med-x (combine (mapcar #'first med-xy)))
             (med-y (combine (mapcar #'second med-xy)))
             (med-xy (list (combine med-x (first med-x))
                           (combine med-y (first med-y))))
             (indx (send self :add-selected-indx))
             (col (elt (color-symbols) (length indx))))
        (send self :add-lines med-xy :color col :width 2))
      (send self :my-clear)))

(defmeth fedf-star :link-subset (&optional set)
  (setf (slot-value 'subset-median) nil)
  (if (and set (send self :selection))
      (let* ((data (send self :data))
             (n (length (first data)))
             (indx (send self :indx-of-selection))
             (len (length (send self :add-selected-indx indx)))
       	     (col (elt (color-symbols) len))
             (r-indx
              (mapcar 
               #'(lambda (j)
                   (let* ((data-j (select data j))
                          (x (select (rank (Modify-data data-j)) indx))
                          (jn5 (* j (+ n 5)))                
                          (r-indx-j (+ x jn5))
                          (sub-select (select data-j indx)))
                     (send self :subset-median sub-select)
                     r-indx-j)) (iseq (length data)))))
        (send self :point-color r-indx col :draw nil)
        (send self :point-selected r-indx t :draw nil))
      (send self :unselect-all-points)))  
         
(defun modify-data (data)
;;
;;add small amount to protect tie
;;
(let* ((eps (/ (- (max data)  (min data)) 1000))
       (len (length data)))
  (+ (rseq 0 eps len) data)))

(defmeth fedf-star :labelling (&optional set)
  (if (and set (send self :add-selected-indx))
      (let* ((data (send self :data))
             (box (send self :text-ascent))
             (gap (round (/ box 4)))
             (gb (+ gap box))
             (labels (send self :labels))        
             (ys (second (send self :size)))
             (sel (send self :add-selected-indx)))
        (dotimes (i (length sel))
                 (let* ((x (elt sel i))
                        (label (elt labels x))
                        (str (string (format nil "~2d:~a" x label)))
                        (y (- ys (* i gb))))
                   (send self :draw-string str gap (- y gap)))))))

(defmeth fedf-star :identify ()
  ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; overlays for star-fedf plot
;;;

(defproto o '(m-list loc dial menu-sel) 
  () graph-overlay-proto)
				

(defmeth o :menu-sel (&optional menu add)
  (if menu 
      (setf (slot-value 'menu-sel)
           (if add 
               (cons menu (slot-value 'menu-sel))
               (delete menu (slot-value 'menu-sel)))))
  (slot-value 'menu-sel)) 

(defmeth o :redraw ()
  (let* ((menu (send self :menu-sel)))
    (send self :draw-menu)
    (if menu
        (mapcar #'(lambda (m)
                    (send self :do-mark m (send self m))) menu))))

(defmeth o :dial (&optional (val nil set))
  (if set 
      (prog ()
            (setf (slot-value 'dial) val)            
            (mapcar #'(lambda (d)
                        (send self :add-slot d)) val)))
  (slot-value 'dial))
 
(defmeth o :isnew (&rest args)
  (apply #'call-next-method args)
  (send self :menu-loc t))
  
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
  (let* ((graph (send self :graph))
         (dial (send self :dial))
         (menu (elt dial ni)))
    (send self menu t)
    (send self :menu-sel menu (send self menu))
    (send self :do-mark menu (send self menu))
    (send graph :menu-sel (send self :menu-sel))
    (send graph menu (send self menu))
    (send graph :selection)))

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

(defmeth o :draw-menu ()
  (let* ((graph (send self :graph))
         (loc (send self :menu-loc))
         (dial (send self :dial)))
    (dotimes (i (length dial))
     (let* ((x (first loc))
            (box (second loc))
            (tw (third loc))
            (y (+ x (* i (+ x box)))))
       (send graph :frame-rect x y box box )
       (send graph :draw-string 
             (string 
              (format nil "~a" (string-left-trim ":" 
                (string (elt dial i)))))
             (+ x box x) (+ box y))))))

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
         (i (position menu (send self :dial)))
         (x (+ 2 gap))
         (y (+ 2 gap (* i (+ gap box)))))
    (list x y)))


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

;;;
;;;make overlay menus marking methods
;;;
    
(defmeth o :connect-median (&optional set)
  (if set (setf (slot-value ':connect-median)
                (not (slot-value ':connect-median))))
  (slot-value ':connect-median))

(defmeth o :connect-quartile (&optional set)
  (if set (setf (slot-value ':connect-quartile)
                (not (slot-value ':connect-quartile))))
  (slot-value ':connect-quartile))

(defmeth o :fivenum-points (&optional set)
  (if set (setf (slot-value ':fivenum-points)
                (not (slot-value ':fivenum-points))))
  (slot-value ':fivenum-points))

(defmeth o :link-subset (&optional set)
  (if set (setf (slot-value ':link-subset)
                (not (slot-value ':link-subset))))
  (slot-value ':link-subset))


(defmeth o :subset-line (&optional set)
  (if set (setf (slot-value ':subset-line )
                (not (slot-value ':subset-line ))))
  (slot-value ':subset-line ))

(defmeth o :labelling (&optional set)
  (if set (setf (slot-value ':labelling )
                (not (slot-value ':labelling ))))
  (slot-value ':labelling ))

(defmeth o :identify (&optional set)
  (if set (setf (slot-value ':identify )
                (not (slot-value ':identify ))))
  (slot-value ':identify ))