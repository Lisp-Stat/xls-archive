;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Create Dialog Box         
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dialog ()
  (let* (
         (data (read-data-columns (call-data-dialog)))
         (nodes (get-nodes data))
         (nxval (first nodes)) 
         (nyval (second nodes)) 
               (xlist (transpose (mapcar #'(lambda (x) 
                          (select (first data) (iseq (* nxval x)
                                  (+ (1- nxval) (* nxval x))))) 
                                  (iseq nyval))))
               (ylist (transpose (mapcar #'(lambda (x) 
                          (select (second data) (iseq (* nyval x) 
                                  (+ (1- nyval) (* nyval x))))) (iseq nxval))))
               (array1 (make-array (list nxval nyval 2) :initial-contents
                                (combine (transpose (list (combine xlist)
                                         (combine ylist))))))
         (arryask (send text-item-proto :new "View Data"))
         (labelask (send text-item-proto :new "Circle Plot"))
         (nlagask  (send text-item-proto :new "Number of Lags: "))
         (headask  (send text-item-proto :new "Head Variable: "))
         (tailask (send text-item-proto :new "Tail Variable: "))
         (xdirask (send text-item-proto :new "X Direction?: "))
         (ydirask (send text-item-proto :new "  Y Direction?: "))
         (varioask (send text-item-proto :new "Type of Procedure: "))
         (nlagget (send edit-text-item-proto :new "" :text-length 3))
         (headget (send choice-item-proto :new 
                  (mapcar #'(lambda (x) (format nil "Var ~a" x))
                      (iseq (length data)))))
         (tailget (send choice-item-proto :new  
                  (mapcar #'(lambda (x) (format nil "Var ~a" x))
                      (iseq (length data)))))
         (xdirget (send choice-item-proto :new
                      (list "No" "Yes")))
         (ydirget (send choice-item-proto :new
                      (list "No" "Yes")))
         (varioget (send choice-item-proto :new
                      (list "Semivariogram" "Covariogram"
                            "Correlogram" "General Relative Semivariogram")))
        (arrybutton (send button-item-proto :new "Push"
                       :action #'(lambda ()
                                   (print array1)
                                   (terpri))))
        (labelbutton (send button-item-proto :new "Push"
                       :action #'(lambda ()
                            (let (
                                  (labels (mapcar #'(lambda (i) 
                                           (mapcar #'(lambda (j)
                                             (aref array1 i j 0)) (iseq nyval)))
                                           (iseq nxval)))
                                 )
                              (oval-plot nxval nyval labels)))))
         (ok (send button-item-proto :new "Run"
              :action #'(lambda ()
         (let* (
               (nlagval (read (make-string-input-stream
                              (send nlagget :text)) nil))
               (headval (send headget :value))
               (tailval (send tailget :value))
#|
               (headval (read (make-string-input-stream
                         (send headget :text)) nil))
               (tailval (read (make-string-input-stream
                         (send tailget :text)) nil))
|#
               (xdirval (send xdirget :value))
               (ydirval (send ydirget :value))
               (varioval (send varioget :value))
               )
          (setf results
            (case varioval (0 (variogram array1 nxval nyval 
                                         (list xdirval ydirval)
                                         nlagval headval tailval))
                           (1 (covarg array1 nxval nyval (list xdirval ydirval)
                                      nlagval headval tailval))
                           (2 (correlogram array1 nxval nyval
                                           (list xdirval ydirval) nlagval
                                           headval tailval))
                           (3 (general-relative-variogram array1 nxval nyval
                                       (list xdirval ydirval) nlagval
                                       headval tailval))))
           (result-dialog results nlagval varioval array1 nxval nyval)
          ))))
         )
    (send dialog-proto :new (list (list arryask arrybutton) 
                                  (list labelask labelbutton)
                                  (list nlagask nlagget)
                                  (list headask headget tailask tailget) 

                                  (list xdirask xdirget ydirask ydirget) 

                                  (list varioask varioget) ok))
  )
)

 
(defun result-dialog (results nlagval varioval array1 nxval nyval)
 (let* (
        (plotlabel (case varioval (0 "Semivariogram") (1 "Covariogram")
                                  (2 "Correlogram") 
                                  (3 "General Relative Semivariogram")))
        (resultask (send text-item-proto :new "Results"))
        (headask (send text-item-proto :new "Head Means"))
        (tailask (send text-item-proto :new "Tail Means"))
        (plotask (send text-item-proto :new "Variogram"))
        (resultbutton (send button-item-proto :new "Push"
                       :action #'(lambda ()
                                   (print (first results))
                                   (terpri))))
        (headbutton (send button-item-proto :new "Push"
                       :action #'(lambda ()
                                   (print (second results))
                                   (terpri))))
        (tailbutton (send button-item-proto :new "Push"
                       :action #'(lambda ()
                                   (print (third results))
                                   (terpri))))
        (plotbutton (send button-item-proto :new "Push"
                       :action #'(lambda ()
                  (let* (
                         (lagseq (iseq 1 nlagval))
                         (varvals (first results))
                         (plot (plot-lines lagseq varvals :title plotlabel))
                         (minind (position (min varvals) varvals))
                         (maxind (position (max varvals) varvals))
                        )
                    (send plot :add-points 
                       (list (select lagseq (list minind maxind))
                             (select varvals (list minind maxind))))
                    (send plot :point-label (iseq 2)
                          (list (format nil "~5,2f" (elt varvals minind))
                                (format nil "~5,2f" (elt varvals maxind))))
                    (send plot :showing-labels t)
                    (send plot :point-selected (iseq 2) t)))))

       )
   (send dialog-proto :new (list (list resultask resultbutton)
                                 (list headask headbutton)
                                 (list tailask tailbutton)
                                 (list plotask plotbutton)))
 )
)
                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Manipulate the data into a matrix, array1(nx,ny,2)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(mapcar #'(lambda (x) (mapcar #'(lambda (y) (aref array1 x y 0))
        (iseq 50))) (iseq 50))

(def data (read-data-columns "krig.dat"))

(def list1 (mapcar #'(lambda (x) (select (first data) (iseq (* nx x) 
       (+ (1- nx) (* nx x))))) (iseq ny)))

(def first-list (transpose list1))

(def list2 (mapcar #'(lambda (x) (select (second data) (iseq (* ny x) 
       (+ (1- ny) (* ny x))))) (iseq nx)))

(def second-list (transpose list2))

(def array1 (make-array (list nx ny 2) :initial-contents
   (combine (transpose (list (combine first-list) (combine second-list))))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Get the name of the data file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-data-dialog ()
  (let* (
         (fileask (send text-item-proto :new "Enter File Name: "))
         (fileget (send edit-text-item-proto :new "" :text-length 15))
         (ok (send modal-button-proto :new "Get File" 
               :action #'(lambda () (send fileget :text))))
         (dialog (send modal-dialog-proto :new 
                       (list (list fileask) (list fileget) (list ok))))
        )
    (send dialog :modal-dialog)
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calculate the variogram
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variogram (data nx ny direction nlag head tail) 
 (let* (
       (np (repeat 0 nlag))
       (hm (repeat 0 nlag))
       (tm (repeat 0 nlag))
       (gamm (repeat 0 nlag))
       (vrh 0)
       (vrt 0)
       (temp 0)
       )
  (dotimes (i nx)
   (dotimes (j ny)
    (let* (
          (ix1 i)
          (iy1 j)
          (ixinc (elt direction 0))
          (iyinc (elt direction 1))
          (k -1)
          ) 
      (loop 
         (setf k (1+ k))
         (setf ix1 (+ ix1 ixinc))
         (setf iy1 (+ iy1 iyinc))
         (if (= ix1 nx) (return))
         (if (= iy1 ny) (return))
         (if (= k nlag) (return))
         (setf vrt (aref data i j head))
         (setf vrh (aref data ix1 iy1 tail))
         (setf temp (setf (elt gamm k) (+ (elt gamm k) 
                                             (^ (- vrh vrt) 2))))
         (setf (elt np k) (1+ (elt np k)))
         (setf (elt tm k) (+ vrt (elt tm k)))
         (setf (elt hm k) (+ vrh (elt hm k)))
      )            
     )
    )
   )
  (list (/ gamm (* 2 np)) (/ hm np) (/ tm np))
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calculate the covariance variogram
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun covarg (data nx ny direction nlag head tail) 
 (let* (
       (np (repeat 0 nlag))
       (hm (repeat 0 nlag))
       (tm (repeat 0 nlag))
       (gamm (repeat 0 nlag))
       (vrh 0)
       (vrt 0)
       (temp 0)
       )
  (dotimes (i nx)
   (dotimes (j ny)
    (let* (
          (ix1 i)
          (iy1 j)
          (ixinc (elt direction 0))
          (iyinc (elt direction 1))
          (k -1)
          ) 
      (loop 
         (setf k (1+ k))
         (setf ix1 (+ ix1 ixinc))
         (setf iy1 (+ iy1 iyinc))
         (if (= ix1 nx) (return))
         (if (= iy1 ny) (return))
         (if (= k nlag) (return))
         (setf vrt (aref data i j head))
         (setf vrh (aref data ix1 iy1 tail))
         (setf temp (setf (elt gamm k) (+ (elt gamm k) 
                                             (* vrh vrt))))
         (setf (elt np k) (1+ (elt np k)))
         (setf (elt tm k) (+ vrt (elt tm k)))
         (setf (elt hm k) (+ vrh (elt hm k)))
      )            
     )
    )
   )
  (list (- (/ gamm np) (* (/ tm np) (/ hm np))) (/ hm np) (/ tm np))
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calculate the correlogram
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun correlogram (data nx ny direction nlag head tail) 
 (let* (
       (np (repeat 0 nlag))
       (hm (repeat 0 nlag))
       (tm (repeat 0 nlag))
       (hv (repeat 0 nlag))
       (tv (repeat 0 nlag))
       (gamm (repeat 0 nlag))
       (vrh 0)
       (vrt 0)
       (temp 0)
       )
  (dotimes (i nx)
   (dotimes (j ny)
    (let* (
          (ix1 i)
          (iy1 j)
          (ixinc (elt direction 0))
          (iyinc (elt direction 1))
          (k -1)
          ) 
      (loop 
         (setf k (1+ k))
         (setf ix1 (+ ix1 ixinc))
         (setf iy1 (+ iy1 iyinc))
         (if (= ix1 nx) (return))
         (if (= iy1 ny) (return))
         (if (= k nlag) (return))
         (setf vrt (aref data i j head))
         (setf vrh (aref data ix1 iy1 tail))
         (setf temp (setf (elt gamm k) (+ (elt gamm k) 
                                             (* vrh vrt))))
         (setf (elt np k) (1+ (elt np k)))
         (setf (elt tm k) (+ vrt (elt tm k)))
         (setf (elt hm k) (+ vrh (elt hm k)))
         (setf (elt tv k) (+ (^ vrt 2) (elt tv k)))
         (setf (elt hv k) (+ (^ vrh 2) (elt hv k)))
      )            
     )
    )
   )
  (def correl-tm1 (/ tm np))
  (def correl-hm1 (/ hm np))
  (def newgamm (/ gamm np)) 
  (def tv1 (/ tv np))
  (def hv1 (/ hv np))
  (def hv2 (^ (- hv1 (* correl-hm1 correl-hm1)) .5))
  (def tv2 (^ (- tv1 (* correl-tm1 correl-tm1)) .5))
  (def hv3 (* hv2 hv2))
  (def tv3 (* tv2 tv2))
  (dotimes (f nlag) 
   (if (< (* (elt hv2 f) (elt tv2 f)) .00001) (setf (elt newgamm f) 0))
   (setf (elt newgamm f) (/ (- (elt newgamm f) (* (elt correl-tm1 f)
                                            (elt correl-hm1 f)))
                         (* (elt hv2 f) (elt tv2 f)))) 
  ) 
  (list newgamm correl-hm1 correl-tm1)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calculate the General Relative Semivariogram (Variogram) 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun general-relative-variogram (data nx ny direction nlag head tail) 
 (let* (
       (np (repeat 0 nlag))
       (hm (repeat 0 nlag))
       (tm (repeat 0 nlag))
       (gamm (repeat 0 nlag))
       (vrh 0)
       (vrt 0)
       (temp 0)
       )
  (dotimes (i nx)
   (dotimes (j ny)
    (let* (
          (ix1 i)
          (iy1 j)
          (ixinc (elt direction 0))
          (iyinc (elt direction 1))
          (k -1)
          ) 
      (loop 
         (setf k (1+ k))
         (setf ix1 (+ ix1 ixinc))
         (setf iy1 (+ iy1 iyinc))
         (if (= ix1 nx) (return))
         (if (= iy1 ny) (return))
         (if (= k nlag) (return))
         (setf vrt (aref data i j head))
         (setf vrh (aref data ix1 iy1 tail))
         (setf temp (setf (elt gamm k) (+ (elt gamm k) 
                                             (^ (- vrh vrt) 2))))
         (setf (elt np k) (1+ (elt np k)))
         (setf (elt tm k) (+ vrt (elt tm k)))
         (setf (elt hm k) (+ vrh (elt hm k)))
      )            
     )
    )
   )
  (def newgamm (/ gamm np))
  (def tm1 (/ tm np))
  (def hm1 (/ hm np))
  (def hmtm (^ (/ (+ hm1 tm1) 2) 2))
  (dotimes (g nlag)
   (if (< (elt hmtm g) .00001) (setf (elt newgamm g) 0))
   (setf (elt newgamm g) (/ (elt newgamm g) (elt hmtm g)))
  )
  (list newgamm hm1 tm1)
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create Oval Plot
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto oval-plot-proto () () graph-proto)


(defmeth oval-plot-proto :eps (&optional (val nil set))
 (if set (setf (slot-value 'eps) val)
   (slot-value 'eps)))

(defmeth oval-plot-proto :vals (&optional (val nil set))
 (if set (setf (slot-value 'vals) val)
   (slot-value 'vals)))

(defmeth oval-plot-proto :nx (&optional (val nil set))
 (if set (setf (slot-value 'nx) val)
   (slot-value 'nx)))

(defmeth oval-plot-proto :ny (&optional (val nil set))
 (if set (setf (slot-value 'ny) val)
   (slot-value 'ny)))

(defmeth oval-plot-proto :ord (&optional (val nil set))
 (if set (setf (slot-value 'ord) val)
   (slot-value 'ord)))



(defun oval-plot (nx ny vals &key (eps .05))
 (let* (
        (vals (combine (1+ (- vals (min 0 (min vals))))))
        (op (send oval-plot-proto :new 2))
        (button (send button-item-proto :new "Enter Ratio"
          :action #'(lambda ()
               (let ((val (first (get-value-dialog "Tolerance: "))))
                  (when (numberp val) (send op :eps val)
                                      (send op :redraw))))))
       )
  (send op :add-slot 'vals vals)
  (send op :add-slot 'ord (sort-data vals))
  (send op :add-slot 'eps eps)
  (send op :add-slot 'nx nx)
  (send op :add-slot 'ny ny)
  (send dialog-proto :new (list button))
  op
))

(defmeth oval-plot-proto :redraw ()
  (call-next-method)
  (let* (
         (colors (select *colors* (list 1 4 3 6 5 7 2 0)))
         (vals (send self :vals))
         (sort-vals (send self :ord))
         (eps (send self :eps))
         (minvals (first sort-vals))
         (i (1- (length vals)))
         (maxvals (elt sort-vals i))
         bigvals
        )
     (loop
        (cond ((< (/ minvals maxvals) eps)
               (setf bigvals (append bigvals (list (position maxvals vals))))
               (setf i (1- i))
               (setf maxvals (elt sort-vals i)))
              (t (setf vals (sqrt (/ vals maxvals)))
                 (setf vals (map-elements #'min vals 1))
                 (return))))
  (let* (
         (xsize (first (send self :size)))
         (ysize (second (send self :size)))
         (nx (send self :nx))
         (ny (send self :ny))
         (x-rect (floor (/ xsize nx)))
         (y-rect (floor (/ ysize ny)))
         (xoval (map-elements #'max 1 (floor (* x-rect vals))))
         (yoval (map-elements #'max 1 (floor (* y-rect vals))))
         (xc (floor (/ (- x-rect xoval) 2)))
         (yc (floor (/ (- y-rect yoval) 2)))
        )
     (mapcar #'(lambda (x y z w)
                 (send self :frame-oval x y z w)
                 (if (and (= z x-rect) (= w y-rect))
                     (send self :frame-oval 
                        (floor (+ x (/ x-rect 3.5))) 
                        (floor (+ y (/ y-rect 3.5)))
                                   (floor (/ x-rect 2))
                                   (floor (/ y-rect 2)))))
                 (+ xc (repeat (* x-rect (iseq nx)) (repeat ny nx)))
                 (+ yc (repeat (* y-rect (iseq (1- ny) 0)) nx))
                 xoval yoval)
 nil
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-nodes (data)
  (let* (
         (nxask (send text-item-proto :new "Number of Nodes in X: "))
         (nyask (send text-item-proto :new "Number of Nodes in Y: "))
         (nxget (send edit-text-item-proto :new "" :text-length 3))
         (nyget (send edit-text-item-proto :new "" :text-length 3))
         (ok (send modal-button-proto
                 :new "ok"
                 :action #'(lambda ( ) (list 
               (read (make-string-input-stream (send nxget :text)) nil)
               (read (make-string-input-stream (send nyget :text)) nil)
         ))))
        (dialog (send modal-dialog-proto :new (list (list nxask nxget) 
                                                    (list nyask nyget) ok))))
        (send dialog :modal-dialog)
       ))


(dialog)

