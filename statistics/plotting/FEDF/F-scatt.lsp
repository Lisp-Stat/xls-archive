
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; functions for fedf-scatterplot
;;;
(defun color-symbols () '(BLUE CYAN red BLACK YELLOW GREEN MAGENTA))

(defun fedf-scatter  
  (data &key (var-labels nil) (scale '(.7 .6)) (symbol 'dot3) 
        (title "FEDF scatterplot"))

" data: the only required parameter.
  var-labels : labels for variables
  scale : x-y scale for the plot 
  symbol : symbols for the data points
  title : title for the plot"

  (Close-all-plots)
  (setf col (first (color-symbols)))
  (setf unselect nil)
  (let* ((p (length data))
         (start (make-start p))
         (var-labels (var-labels p var-labels))
         (w (send fedf-scatterplot :new 2 :title title 
                  :data data :start start :var-labels var-labels
                  :scale scale)))
    (send w :analysis-data data)
    (send w :rank-data data)
    (send w :rank-corr data)
    (send w :draw-window)
    (send w :use-color t)
    (send w :has-h-scroll nil)
    (send w :has-v-scroll nil)   
    (send w :point-symbol (iseq (send w :num-points)) symbol)
    (send w :adjust-to-data)))
                    

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  prototype and methods for FEDF-scatterplot
;;;
(defproto fedf-scatterplot 
  '(menu-sel data no-pts pt-labels axis-labels axis-location 
             var-labels  pt-symbol lines start
             axis analysis-data st-data statistics x-label 
             y-label off scale row? cond-corr rank-data 
             rank-corr w-h-d all-cond-corr pts-hilited)
  () graph-proto) 

	
(defmeth fedf-scatterplot :lines (&optional (val nil set))
  (if set (setf (slot-value 'lines) val))
  (slot-value 'lines))    


(defmeth fedf-scatterplot :start (&optional (val nil set))
  (if set (setf (slot-value 'start) val))
  (slot-value 'start))    

(defmeth fedf-scatterplot :axis (&optional (val nil set))
  (if set (setf (slot-value 'axis) val))
  (slot-value 'axis))    

(defmeth fedf-scatterplot :menu-sel (&optional (val nil set))
  (if set (setf (slot-value 'menu-sel) val))
  (slot-value 'menu-sel))       

(defmeth fedf-scatterplot :isnew (&rest args)
  (apply #'call-next-method args))

(defmeth fedf-scatterplot :data (&optional (val nil set))
  (if set    
      (setf (slot-value 'data) val))
  (slot-value 'data))

(defmeth fedf-scatterplot :analysis-data (&optional val)
"make data (list standardized-data original-data)"
  (if val
      (let* ((data (send self :standardize-data val)))
        (setf (slot-value 'analysis-data) (list data val))))
  (slot-value 'analysis-data))

(defmeth fedf-scatterplot :standardize-data (&optional data)
  (if data
      (let* ((start (send self :start))
             (start (append (first start) (second start)))
             (scale (send self :scale))       
             (n-data
              (mapcar 
               #'(lambda (st) (win-standard st (select data st) scale))
               start)))
         (setf (slot-value 'st-data) n-data)))
  (slot-value 'st-data))

(defmeth fedf-scatterplot :pt-labels (&optional (val nil set))
  (if set (setf (slot-value 'pt-labels) val))
  (slot-value 'pt-labels))

(defmeth fedf-scatterplot :pt-symbol (&optional (val nil set))
  (if set (setf (slot-value 'pt-symbol) val))
  (slot-value 'pt-symbol))

(defmeth fedf-scatterplot :axis-labels (&optional (val nil set))
  (if set (setf (slot-value 'axis-labels) val))
  (slot-value 'axis-labels))

(defmeth fedf-scatterplot :var-labels  (&optional (val nil set))
  (if set (setf (slot-value 'var-labels ) val))
  (slot-value 'var-labels ))

(defmeth fedf-scatterplot :scale  (&optional (val nil set))
  (if set (setf (slot-value 'scale ) val))
  (slot-value 'scale ))

(defmeth fedf-scatterplot :no-pts (&optional (val nil set))
  (if set (setf (slot-value 'no-pts) val))
  (slot-value 'no-pts))

(defmeth fedf-scatterplot :statistics (&optional (data nil set))
  (if set (setf (slot-value 'statistics) (two-factor data)))
  (slot-value 'statistics))

(defmeth fedf-scatterplot :x-label (&optional (label nil set))
  (if set 
      (setf (slot-value 'x-label) label))            
  (slot-value 'x-label))

(defmeth fedf-scatterplot :nrc (&optional (stats nil set))
  (if set 
      (let* ((nr (length (second stats)))
             (nc (length (third stats)))
             (ni (* nr nc)))
        (setf (slot-value 'nrc) (list nr nc ni))))
  (slot-value 'nrc))

(defmeth fedf-scatterplot :all-cond-corr (&optional (val nil set))
  (if set 
      (setf (slot-value 'all-cond-corr)
             (append  (slot-value 'all-cond-corr) (list val))))
  (slot-value 'all-cond-corr))

(defmeth fedf-scatterplot :out-line ()
  (let* ((sz (1- (send self :size)))
         (x (first sz))
         (y (second sz)))
    (send self :draw-line 0 0 x 0)
    (send self :draw-line x 0 x y)
    (send self :draw-line x y 0 y)
    (send self :draw-line 0 0 0 y)))

(defmeth fedf-scatterplot :pts-hilited (&optional p-sel)
  (if p-sel
      (setf (slot-value 'pts-hilited)
            (remove nil (append (slot-value 'pts-hilited) 
                                (list p-sel)))))
  (slot-value 'pts-hilited))

(defmeth fedf-scatterplot :do-select-click (x y m1 m2)
  (setf unselct 'nil)
  (call-next-method x y m1 m2)
  (setf unselect 't)
  (if (send self :selection)
      (let ((p-sel (send self :link t)))
        (send self :pts-hilited p-sel)            
        (send self :linking p-sel))
      (send self :unselecting))
  (send self :redraw))

(defmeth fedf-scatterplot :unselecting ()
  (setf (slot-value 'pts-hilited) nil)
  (send self :unselect-all-points)
  (send self :point-color 
        (iseq (send self :num-points)) 'black)
  (setf (slot-value 'all-cond-corr) nil))


(defmeth fedf-scatterplot :linking (p-sel)
  (let* ((p (length (send self :data)))
         (st (/ (* p (1- p)) 2))
         (sel (iseq st (+ st p -1)))
         (sel (select p-sel sel))
         (l-corr (lower-right-corr sel))) 
    (send self :do-hilite)
    (send self :all-cond-corr l-corr)))

(defmeth fedf-scatterplot :do-hilite ()
  (let* ((pts (send self :pts-hilited))
         (colors (remove 'white (color-symbols))))
    (dotimes (i (length pts))
             (let ((pt (elt pts i))
                   (color (elt colors i)))
               (send self :point-color pt color :draw nil)
               (send self :point-state pt 'hilited :draw nil)))))

(defmeth fedf-scatterplot :redraw (&rest args)
(when (> (send self :num-points) 0)
      (send self :start-buffering)
      (apply #'call-next-method args)
      (send self :w-h-d :set t)
      (send self :draw-rank-corr)
      (send self :var-labelling)
      (send self :out-line)
      (send self :draw-all-cond-corr)
      (send self :buffer-to-screen)))

(defmeth fedf-scatterplot :my-draw-rect ()
  (let* ((p (length (send self :data))))
    (dotimes (i (1+ p))
             (let* ((x (list 0 p))
                    (y (list i i)))
               (send self :add-lines (list x y) :draw nil)
               (send self :add-lines (List y x) :draw nil)))))
         
(defmeth fedf-scatterplot :var-labelling ()
  (let* ((off '(0.1 .9))
         (p (length (send self :data)))
         (start (send self :start))
         (start (second start))
         (start (mapcar #'(lambda (x) (+ off x)) start))
         (var-labels (send self :var-labels)))
    (mapcar #'(lambda (st y)
                (let ((cxy (send self :real-to-canvas 
                                 (first st) (second st))))
                  (send self :draw-string y 
                        (first cxy) (second cxy))))
            start var-labels)))
                      
(defmeth fedf-scatterplot :which-win ()
;;;for the selected poits
;;;determine which window, selected points, and 
;;;cumulative lengths of the points for the windows
;;;
  (let* ((data (send self :analysis-data))
         (data (first data))
         (pp (length data))
         (n (length (first (first data))))
         (seq (cumsum (repeat n pp)))
         (s (send self :selection))
         (win (length (which (>= (first s) seq))))
         (seq (select (cons 0 seq) (iseq pp)))
         (sel (- s (elt seq win))))
    (list sel seq win)))

(defmeth fedf-scatterplot :link  (&optional set)
  (if set
      (let* ((data (send self :analysis-data))
             (o-data (second data))
             (p (length o-data)) 
             (win-sel (send self :which-win))
             (sel (first win-sel))
             (seq (second win-sel))
             (win (third win-sel))
             (o-d (append (repeat nil (/ (* p (1- p)) 2)) (iseq p)))
             (fedf? (elt o-d win))
             (sel (if fedf?
                      (let ((c-data (select o-data fedf?)))
                        (select (order c-data) sel))
                      sel))
             (p-sel 
              (mapcar 
               #'(lambda (i)
                   (let ((i-sum (elt seq i))
                         (o-i (elt o-d i)))
                     (if o-i 
                         (let* ((c-data (select o-data o-i))
                                (r-data (rank c-data)))
                           (+ i-sum (select r-data sel)))
                         (+ i-sum sel)))) 
               (iseq (length seq)))))
        p-sel)
      nil))

(defmeth fedf-scatterplot :my-point-symbol ()
  (let* ((symbol (send self :pt-symbol))
         (symbol (if symbol symbol 'disk))
         (data (send self :data))
         (p (length data))
         (n (length (first data))))
    (send self :point-symbol (iseq (send self :num-points)) symbol)
    (dotimes (i p)
             (let* ((ii (1+ i))
                    (is (/ (* ii (1+ ii)) 2))
                    (istart (+ (* i 4) (* n is)))
                    (iend (+ 3 istart)))
               (send self :point-state 
                     (iseq istart iend) 'invisible)))))

(defmeth fedf-scatterplot :axis-draw (data)
  (let* ((fd (first data))       
         (fv (fivnum fd))
         (3v (select fv '(0 2 4)))
         (miny (min (second data)))
         (eps 0.05)
         (miny (- miny eps))
         (ey (- miny eps))
         (x-list (list (list (elt 3v 0) (elt 3v 0))
                       (list (elt 3v 1) (elt 3v 1))
                       (list (elt 3v 2) (elt 3v 2)))))
    (send self :add-lines 
          (list (select 3v '(0 2)) (list miny miny))
          :draw nil)
    (dolist (xl x-list)
            (send self :add-lines 
                  (list xl (list miny ey)) :draw nil))))
          
         
(defmeth fedf-scatterplot :draw-window ()
  (let* ((data (first (send self :analysis-data)))
         (start (send self :start))
         (Start (append (first start) (second start))))
    (dotimes (i (Length start))
             (let* ((pt (elt data i))
                    (st (elt start i))
                    (fedf? (= (first st) (second st))))
               (send self :add-points pt :draw nil)
               (when fedf?
                     (send self :axis-draw pt))))
    (send self :my-draw-rect)
    (send self :adjust-to-data :draw nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  methods for correlations
;;;
(defmeth fedf-scatterplot :rank-data (&optional val)
  (if val      
      (let ((r-val (mapcar #'my-rank val)))
        (setf (slot-value 'rank-data) r-val)))
  (slot-value 'rank-data))


(defmeth fedf-scatterplot :rank-corr (&optional val)
  (if val
      (let* ((r-val (slot-value 'rank-data))
             (low-cor (lower-right-corr r-val)))
        (setf (slot-value 'rank-corr) low-cor)))
  (slot-value 'rank-corr))

(defmeth fedf-scatterplot :draw-all-cond-corr ()  
  (mapcar 
   #'(lambda (corr col)
       (send self :draw-cond-corr corr :color col))
   (send self :all-cond-corr) (remove 'white (color-symbols))))

(defmeth fedf-scatterplot :draw-cond-corr (corr &key color)
;;;color for the conditional correlation bar
;;;
  (let* ((data (send self :data))
         (p (length data))
         (w-h-d (send self :w-h-d :set t))
         (w (first w-h-d))         
         (h (second w-h-d))
         (D (third w-h-d))
         (w (floor (/ w 2)))
         (h (floor (/ h 3)))
         (start (third (send self :start))))
    (mapcar #'(lambda (st r)
                (send self :draw-corr-bar st r
                      w d h :cond t :Color color)) 
            start corr)))

(defmeth fedf-scatterplot :draw-rank-corr (&key (color 'blue))
  (let* ((data (second (send self :analysis-data)))
         (p (length data))
         (r-cor (send self :rank-corr))
         (w-h-d (send self :w-h-d ))
         (w (first w-h-d))
         (h (second w-h-d))
         (D (third w-h-d))
         (w (floor (/ w 2)))
         (ps -1))
    (mapcar #'(lambda (st)
                (setf ps (1+ ps))
                (send self :draw-corr-box st)
                (send self :draw-corr-bar st (elt r-cor ps)
                      w d h)) 
            (third (send self :start)))))

(defmeth fedf-scatterplot :draw-corr-box (start &key (cond nil))
  (let* ((xy0 (+ '(.5 .65) start))
         (x0 (first xy0))
         (y0 (second xy0))
         (xy0 (send self :real-to-canvas x0 y0))
         (x0 (first xy0))
         (y0 (second xy0))
         (w-h-d (send self :w-h-d))
         (w (first w-h-d))
         (h (second w-h-d))
         (h (if cond (floor (/ h 4)) h))
         (D (third w-h-d))
         (w2 (floor (/ w 2)))
         (w4 (floor (/ w 4)))
         (xu (- x0 w2))
         (yu (- y0 d h))
         (yl (+ y0 d))
         (d2 (floor (/ d 2)))
         (xlm (- x0 w4))
         (xrm (+ x0 w4))
         (yuh (+ yu h)))
    (if cond
        (prog ()    
              (send self :frame-rect xu yl w h)
              (send self :draw-line x0 yl x0 (+ yl h))
              (send self :draw-line x0 (- yl d2) x0 (+ yl d2))
              (send self :draw-line xlm (- yl d2) xlm (+ yl d2))
              (send self :draw-line xrm (- yl d2) xrm (+ yl d2)))
        (prog ()
              (send self :frame-rect xu yu w h)
              (send self :draw-line x0 yu x0 yuh)
              (send self :draw-line x0 (- yuh d2) x0 (+ yuh d2))
              (send self :draw-line xlm (- yuh d2) xlm (+ yuh d2)) 
              (send self :draw-line xrm (- yuh d2) xrm (+ yuh d2))
              (send self :draw-string "-1" (- x0 w2 4) (+ y0 3))
              (send self :draw-string "+1"  (+ x0 w2 -3) (+ y0 3))
              (send self :draw-string "0" (- x0 2) (+ y0 3))))))

       
(defmeth fedf-scatterplot :draw-corr-bar (start r w d h &key (cond nil) 
                                       (color 'black))
  (let* ((xy0 (+ '(.5 .65) start))
         (x0 (first xy0))
         (y0 (second xy0))
         (xy0 (send self :real-to-canvas x0 y0))
         (x0 (first xy0))
         (y0 (second xy0))
         (rw (floor (* r w)))
         (xr0 (if (plusp r) x0 (+ x0 rw)))
         (p (1+ (position color (remove 'white (color-symbols)))))
         (ph3 (* p (* 3 h)))
         (y0 (if cond (+ y0 ph3) (- y0 d h)))
         (old-col (send self :draw-color))
         (y0h (+ y0 h)))
    (send self :draw-color color)
    (when cond
          (send self :draw-line  (- x0 w) y0h (+ x0 w) y0h)
          (send self :draw-line  x0 y0h x0 y0))
    (send self :paint-rect xr0 y0 (abs rw) h)
    (send self :draw-color old-col)))


(defmeth fedf-scatterplot :w-h-d (&key (d .1) (h .15) (w .35) (set nil))
;;;obtain the width, height of the box for correlation
;;;and (* 2 d) is the difference between the upper and lower box
  (if set 
      (setf (slot-value 'w-h-d) 
            (let* ((x0 .5)
                   (y0 .5)
                   (mxw (- x0 w))
                   (pxw (+ x0 w))
                   (pyd (+ y0 d))
                   (pydh (+ y0 d h))
                   (xy0 (send self :real-to-canvas x0 y0))
                   (xylu (send self :real-to-canvas mxw pydh))
                   (xyrl (send self :real-to-canvas pxw pyd))
                   (diff (- xyrl xylu))
                   (w (first diff))
                   (h (second diff))
                   (d (second (- xy0 xyrl)))) 
              (list w h d))))
  (slot-value 'w-h-d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
