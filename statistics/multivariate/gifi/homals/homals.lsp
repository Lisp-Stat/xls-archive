;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prototype Definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto homals-proto '(data n m z y d-m k-j-list
                         p active-categories active-variables 
                         variable-labels object-labels category-labels
                         output-file dialog eps-0 eps-1))

(defproto homals-dialog-proto '(homals-parent dialog-items plot-dialog) 
                                nil dialog-proto)

(defproto homals-plot-dialog-proto '(showing) 
                                     nil homals-dialog-proto)

(defproto homals-plot-proto '(plot-parent zoom plot-name overlay-list)
                              nil graph-proto)

(defproto homals-2d-plot-proto nil nil 
                              (list scatterplot-proto homals-plot-proto))

(defproto homals-3d-plot-proto nil nil 
                               (list spin-proto homals-plot-proto))

(defproto homals-list-item-proto '(last-click) nil list-item-proto)

(defproto plot-route-proto '(homals-parent plot dims))

(defproto star-plot-proto nil nil plot-route-proto)
(defproto score-plot-proto nil nil plot-route-proto)
(defproto category-plot-proto nil nil plot-route-proto)
(defproto category-object-plot-proto nil nil plot-route-proto)
(defproto combined-category-plot-proto nil nil plot-route-proto)
(defproto discrimination-plot-proto nil nil plot-route-proto)



(defmeth homals-proto :compute (
               &key
               (data (send self :data))           ; data matrix
               (ndim (send self :p))              ; # computing dimensions
               (active-variables                  ; list of active variables
                 (send self :active-variables))
               (active-categories                 ; list of active categories
                 (send self :active-categories))  ; for the active variables
               (object-labels                     ; labels of all objects
                   (send self :object-labels))
               (variable-labels                   ; labels of all variables
                   (send self :variable-labels))
               (category-labels                   ; labels of all categories
                   (send self :category-labels))
               (eps-0 (send self :eps-0))         ; function change
               (eps-1 (send self :eps-1))         ; solution change
               (itmax 200)                        ; # iterations
               (qr t)                             ; Q-R or Gram ?
               (speed t))                         ; Speed or Memory

(let* ((n (send self :n))
       (e (select data (iseq n) active-variables))
       (m (length active-variables))
       (w (make-weights e active-categories))
       (x (make-random-orthonormal w n ndim m))
       (fit-0 0)
       (fit-1 0)
       (itel 0)
       h)
  (send self :d-m nil)
  (send self :k-j-list nil)
  (if speed
      (dotimes (j m)
               (let ((g (make-indicator (elt (column-list e) j)
                                             (elt active-categories j))))
                 (setf h (append h (list g))))))
  (loop
   (let ((z (make-array (list n ndim) :initial-element 0))
         (new-fit-0 (* n ndim))
         (new-fit-1 0)
         )
     (dotimes (j m)
       (let* ((g (if speed (elt h j)
                     (make-indicator (elt (column-list e) j)
                                     (elt active-categories j))))
              (y (make-category-quantifications x g))
              (disc (* (/ n m) (make-discrimination-measures y g))))
         (if (= itel 0) (send self :k-j-list (array-dimension g 1)))
         (setf new-fit-0 (- new-fit-0 (sum (diagonal disc))))
         (setf z (+ z (matmult g y)))))
     (if qr
         (setf z (* (sqrt n)
                    (q-r-decomp (center (apply #'bind-rows
                                            (* (geninv w) (row-list z))) w)
                                (/ w m))))
         (setf z (* (sqrt n)
                    (gram-schmidt (center (apply #'bind-rows
                                            (* (geninv w) (row-list z))) w)
                                 (/ w m)))))
     (setf new-fit-1 (/ (sum (combine (matmult (transpose z)
                                      (apply #'bind-rows (* w (row-list x))))))
                        (* m n ndim)))
     (if (or (and (> eps-0 (abs (- new-fit-0 fit-0)))
                  (> eps-1 (abs (- new-fit-1 fit-1))))
             (> itel itmax))
         (progn
               (let ((bigy (repeat (list (list 0)) ndim))
                     (c (make-array (list ndim ndim) :initial-element 0)))
                 (dotimes (j m)
                   (let* ((g (if speed (elt h j)
                             (make-indicator (elt (column-list e) j)
                                             (elt active-categories j))))
                          (y (make-category-quantifications z g))
                          (d (make-discrimination-measures y g)))
                     (setf bigy (mapcar #'combine bigy (column-list y)))
                     (if qr (send self :d-m d))
                     (setf c (+ c d))))
                 (if qr 
                  (progn 
                   (send self :z z)
                   (send self :y (apply #'bind-columns
                                          (mapcar #'rest bigy))))
                  (progn               
                    (let* ((f (elt (eigen c) 1))
                           (k (apply #'bind-columns f)))
                     (send self :z (matmult z k))
                     (send self :y (matmult (apply #'bind-columns
                                            (mapcar #'rest bigy)) k))
                    (dotimes (j m)
                      (let* ((g (if speed (elt h j)
                             (make-indicator (elt (column-list e) j)
                                             (elt active-categories j))))
                             (y (make-category-quantifications z g))
                             (d (make-discrimination-measures y g)))
                      (send self :d-m d))))))
                 (format t "~%Converged....\n\n")
                 (return)))
       (progn
         (format t "~%Iteration ~4d, Loss Measure ~,10f, Change measure ~,10f"
                 (1+ itel) new-fit-0 new-fit-1)
         (setf fit-0 new-fit-0)
         (setf fit-1 new-fit-1)
         (setf itel (1+ itel))
         (setf x z)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Output routines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-proto :homals-numerical-output (output-file)
  (let (
        (data (send self :data))
        (z (send self :z))
        (y (send self :y))
        (d-m (send self :d-m))
        (av (send self :active-variables))
        (ac (send self :active-categories))
        (ol (send self :object-labels))
        (vl (send self :variable-labels))
        (cl (send self :category-labels))
        (p (send self :p))
        (n (send self :n))
        (m (send self :m)))
    (with-open-file
     (output output-file :direction :output)
     (format output "~a~%" (make-string (+ 27 (* 13 p)) :initial-element #\=))
     (format output "Object Scores~%")
     (format output "~a~%" (make-string (+ 27 (* 13 p)) :initial-element #\=))
     (dotimes (i n)
         (format output "~20a *** A " (elt ol i))
       (dotimes (s p)
         (format output "~12,8f " (aref z i s)))
       (format output "~%"))
     (format output "~a~%~a~%" (make-string (+ 27 (* 13 p)) :initial-element #\=) #\C-L)
     (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\=))
     (format output "Category Quantifications~%")
     (format output "~a~%~%" (make-string (+ 36 (* 13 p)) :initial-element #\=))
     (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\-))
     (dotimes (j m)
      (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\-))
      (if (find j av)
           (progn
           (format output "Variable ~20a      (ACTIVE)~%" (elt vl j))
           (format output "~a~%" (make-string (+ 36 (* 13 p)) 
                           :initial-element #\-))
           (let* ((r (elt (column-list data) j))
                  (u (sort-data (remove-duplicates r :test 'equal)))
                  (g (make-indicator r u))
                  (y (make-category-quantifications z g))
                  (d (column-sums g))
                  (k (length d)))
             (dotimes (l k)
               (if (find (elt u l) (elt ac (position j av)) :test 'equal)
                   (format output "~20a *** ~4d *** A "
                           (elt (elt cl j) l) (elt d l))
                   (format output "~20a *** ~4d *** P "
                           (elt (elt cl j) l) (elt d l)))
               (dotimes (s p)
                 (format output "~12,8f " (aref y l s)))
               (format output "~%"))))

           (progn
            (format output "Variable ~20a     (PASSIVE)~%" (elt vl j))))
       (format output "~a~%" (make-string (+ 36 (* 13 p)) 
                      :initial-element #\-)))

     (format output "~a~%" #\C-L)
     (format output "~a~%" (make-string (max 43 (* 13 p)) 
                    :initial-element #\=))
     (format output "Discrimination Measures~%")
     (format output "~a~%~%" (make-string (max 43 (* 13 p)) 
                    :initial-element #\=))

     (let ((e (make-array (list p p) :initial-element 0)))
       (dotimes (j m)
         (format output "~a~%" (make-string (max 43 (* 13 p)) 
                    :initial-element #\-))

       (if (find j av)
           (progn
             (format output "Variable ~20a      (ACTIVE)~%~%" (elt vl j))
             (let ((d (elt d-m (position j av))))
               (setf e (+ e d))
               (dotimes (s p)
                        (dotimes (u p)
                                 (format output "~12,8f " (aref d s u)))
                        (format output "~%"))))

           (progn
               (format output "Variable ~20a     (PASSIVE)~%" (elt vl j))))
         (format output "~a~%" (make-string (max 43 (* 13 p)) 
                         :initial-element #\-)))

       (setf e (/ e (length av)))
       (format output "~a~%" (make-string (max 43 (* 13 p)) 
                   :initial-element #\-))
       (format output "Average Discrimination Measure~%")   
       (format output "~a~%" (make-string (max 43 (* 13 p)) 
                   :initial-element #\-))
       (dotimes (s p)
                (dotimes (u p)
                       (format output "~12,8f " (aref e s u)))
              (format output "~%"))
       (format output "~a~%" (make-string (max 43 (* 13 p)) 
                   :initial-element #\-))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Computing methods for an ALS iteration.
;;
;; It would be useful to have versions of this in C,
;; and load them dynamically.
;;
;; The same thing is true for the Gram-Schmidt and the Procrustus
;; modules below, because that is where most of the computing takes
;; place.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-category-quantifications (x g)
  (let* ((d (geninv (column-sums g))))
    (matmult (diagonal d) (matmult (transpose g) x))))

(defun make-discrimination-measures (y g)
  (let* ((d (column-sums g)))
    (/ (matmult (transpose y) (matmult (diagonal d) y)) 
       (array-dimension g 0))))

(defun make-weights (e s)
  (let* ((n (first (array-dimensions e)))
         (m (second (array-dimensions e)))
         (f (column-list e))
         (w (make-list n :initial-element 0)))
    (dotimes (j m)
      (incf w (row-sums (make-indicator (elt f j) (elt s j)))))    
    w
 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun column-sums (x)
  (mapcar #'sum (column-list x)))

(defun column-means (x)
  (mapcar #'mean (column-list x)))

(defun row-sums (x)
  (mapcar #'sum (row-list x)))


(defun make-indicator (x &optional (z nil set))
  "Args: sequence
Elements of SEQUENCE are either numbers or strings.
Returns a dummy with sorted category values."     
(let* (
      (y (if set z
           (sort-data (remove-duplicates x :test 'equal))))
      (m (length y))
      (n (length x))
      (a (make-array (list n m) :initial-element 1))
      (b (make-array (list n m) :initial-element 0))
      )
  (if-else (outer-product x y #'equal) a b)
))

(defun marginals (x)
  "Args: sequence
SEQUENCE is a sequence of numbers or strings. Different entries are
sorted and counted."
  (mapcar #'sum (column-list (make-indicator x)))
  )

(defun number-of-values (x)
  "Args: sequence
Elements of SEQUENCE are either numbers or strings.
Returns the number of different values."
  (length (remove-duplicates x :test 'equal))
  )

(defun make-random-orthonormal (w n ndim m)
  "Args: w n ndim m
Makes a matrix of order N x M with standard normals,
then centers and w-orthogonalizes it."
  (let ((z (make-array (list n ndim) :displaced-to
                       (coerce (normal-rand (* n ndim)) 'vector))))
    (* (sqrt n) (gram-schmidt (center z w) (/ w m)))))

(defun q-r-decomp (x w)
"Args: X W 
X is decomposed as QR, with Q w-orthonormal."
 (let* ((v (geninv (sqrt w)))
        (y (first (qr-decomp x))))
   (apply #'bind-rows (* v (row-list y)))))


(defun gram-schmidt (x w)
  "Args: X
X is decomposed as KS, with K w-orthonormal and S upper-triangular,
returns K."
  (let ((y (chol-decomp (matmult (transpose x) 
              (apply #'bind-rows (* w (row-list x)))))))
    (matmult x (inverse (transpose (first y))))
    ))

(defun center (x w)
  "Args: X W
X is a matrix and W is a list of weights.  Returned is a matrix Z such that
u'WZ=0"
  (let (
        (n (first (array-dimensions x)))
        (mu (/ (matmult w x) (sum w)))
        )
    (- x (outer-product (repeat 0 n) mu #'+))
    ))

(defun geninv (x &optional (ozo 1))
  (if (compound-data-p x)
      (map-elements #'geninv x)
    (if (= 0 x) ozo (/ x))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Lines Overlay Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto lines-overlay-proto nil nil graph-overlay-proto)

(defmeth lines-overlay-proto :redraw ()
 (let* (
       (graph (send self :graph))
       (lines (send graph :lines))
       (ht (send graph :text-ascent))
      )
   (send self :bitmap lines)
   (send graph :draw-string "Draw" 25 (+ 10 (* 7 ht)))
   (send graph :draw-string "Lines" 25 (+ 10 (* 8 ht)))
 )
)

(defmeth lines-overlay-proto :bitmap (lines)
 (let* ((check-bitmap (check))
        (blank-bitmap (empty))
        (graph (send self :graph))
        (ht (send graph :text-ascent)))
    (send graph :draw-bitmap (if lines check-bitmap blank-bitmap)
                          10 (+ 10 (* 7 ht)))))


(defmeth lines-overlay-proto :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (plot-parent (send graph :plot-parent))
        (lines (send graph :lines))
        (ht (send graph :text-ascent))
        (zoom (send graph :zoom))
       )
  (when (and (< 10 x 20) (< (+ 10 (* 7 ht)) y (+ 10 (* 8 ht))))
        (if lines
            (progn
              (send graph :clear-lines)
              (send self :bitmap nil))
            (progn
              (send graph :clear-lines)
              (send plot-parent :make-lines (send graph :points-showing))
              (send graph :redraw)
              (send self :bitmap t)))
        (unless zoom (send plot-parent :selected-points))
        (send graph :lines (not lines)))))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Zoom Overlay Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto zoom-overlay-proto nil nil graph-overlay-proto)

(defmeth zoom-overlay-proto :redraw ()
 (let* (
       (graph (send self :graph))
       (zoom (send graph :zoom))
       (ht (send graph :text-ascent))
      )
   (send self :bitmap zoom)
   (send graph :draw-string "Zoom" 25 (+ 10 ht))
   (send graph :draw-string "Out" 25 (+ 10 (* 2 ht)))
   (send graph :draw-string "Zoom" 25 (+ 10 (* 4 ht)))
   (send graph :draw-string "In" 25 (+ 10 (* 5 ht)))
 )
)


(defmeth zoom-overlay-proto :do-click (x y m1 m2)
 (let* (
        (graph (send self :graph))
        (plot-parent (send graph :plot-parent))
        (zoom (send graph :zoom))
        (ht (send graph :text-ascent))
        (lines (send graph :lines))
       )
  (cond ((and (< 10 x 20) (< (+ 10 ht) y (+ 10 (* 2 ht))) zoom)
          (send graph :zoom nil)
          (send graph :mouse-mode 'newselecting)
          (send plot-parent :cleanup)
          (send graph :show-all-points)
          (if lines (send plot-parent :make-lines))
          (send plot-parent :selected-points)
          (send self :bitmap nil))
        ((and (< 10 x 20) (< (+ 10 (* 4 ht)) y (+ 10 (* 5 ht))) (not zoom))
          (send plot-parent :selected-points)
          (send graph :zoom t)
          (send self :bitmap t)
          (send plot-parent :cleanup)
          (send graph :mouse-mode 'zoom)))))
 

(defmeth zoom-overlay-proto :bitmap (zoom)
 (let* ((check-bitmap (bullet))
        (blank-bitmap (empty))
        (graph (send self :graph))
        (ht (send graph :text-ascent)))
    (send graph :draw-bitmap (if zoom blank-bitmap check-bitmap) 10 (+ 10 ht))
    (send graph :draw-bitmap (if zoom check-bitmap blank-bitmap) 
                          10 (+ 10 (* 4 ht)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Zooming and Selecting Mouse Mode Action Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-plot-proto :get-zoom-points (x y m1 m2)
 (let* ((plot-parent (send self :plot-parent))
        (coords (send self :draw-box x y))
        (pts (send self :points-in-rect 
                        (if (< x (first coords)) x (first coords))
                        (if (< y (second coords)) y (second coords))
                        (abs (- x (first coords)))
                        (abs (- y (second coords)))))
        (pts-showing (send self :points-showing))
        (lines (send self :lines)))
    (when (and pts (not (= (length pts) (length pts-showing))))
      (send self :point-showing 
         (sort-data (set-difference pts-showing pts))
         (repeat nil (- (length pts-showing) (length pts))))
      (if lines 
        (progn
          (send self :clear-lines)
          (send self :adjust-to-data)
          (send plot-parent :make-lines (send self :points-showing))
          (send self :redraw))
        (send self :adjust-to-data)))))

        
(defmeth homals-plot-proto :newselect (x y m1 m2)
  (let ((plot-parent (send self :plot-parent))
        (margin (send self :margin)))
   (when (and (< (elt margin 0) x)
              (< (elt margin 1) y))
     (send self :unselect-all-points)
     (send plot-parent :cleanup)
     (let ((cl (send self :click-range)))
       (send self :adjust-points-in-rect
              (floor (- x (/ (first cl) 2))) (floor (- y (/ (second cl) 2)))
              (first cl) (second cl) 'selected))
     (let ((coords (send self :draw-box x y)))
       (when (and (not (= x (first coords))) (not (= y (second coords))))
         (send self :adjust-points-in-rect
                        (if (< x (first coords)) x (first coords))
                        (if (< y (second coords)) y (second coords))
                        (abs (- x (first coords)))
                        (abs (- y (second coords))) 'selected))))))

(defmeth homals-plot-proto :draw-box (x y)
 (let ((newx x)
       (newy y))
 (send self :draw-mode 'xor)
 (send self :while-button-down #'(lambda (x1 y1)
                      (send self :draw-box-lines x y x1 y1)
                      (send self :draw-box-lines x y newx newy)
                      (setf newx x1)
                      (setf newy y1)))
 (send self :draw-box-lines x y newx newy)
 (send self :draw-mode 'normal)
 (list newx newy)))

(defmeth homals-plot-proto :draw-box-lines (x y x1 y1)
  (send self :draw-line x y x y1)
  (send self :draw-line x y x1 y)
  (send self :draw-line x y1 x1 y1)
  (send self :draw-line x1 y1 x1 y)
)



;;  Redraw Methods to Make Windows Plots Happy

(defmeth homals-2d-plot-proto :redraw ()
 (send self :redraw-background)
 (send self :redraw-content)
 (send self :redraw-overlays)
)

(defmeth homals-3d-plot-proto :redraw ()
 (send self :redraw-background)
 (send self :redraw-content)
 (send self :redraw-overlays)
)


;; Setup Methods.  Put dimension specific setup calls in the corresponding
;; dimension method.  Put common setup calls in the homals-plot-proto setup.

(defmeth homals-2d-plot-proto :setup (dims plot-parent)
 (let ((wid (send self :text-width "Zoom")))
   (send self :back-color 'white)
   (send self :draw-color 'black)
   (send self :margin (* 2 wid) 0 0 0)
   (call-next-method dims plot-parent)
   self))

(defmeth homals-3d-plot-proto :setup (dims plot-parent)
 (let ((wid (send self :text-width "Zoom")))
   (send self :back-color 'black)
   (send self :draw-color 'white)
   (send self :margin (* 2 wid) 0 0 25)
   (call-next-method dims plot-parent)
   self))


(defmeth homals-plot-proto :setup (dims plot-parent)
 (let ((wid (send self :text-width "Zoom"))
       (zoom-overlay (send zoom-overlay-proto :new))
       (lines-overlay (send lines-overlay-proto :new)))
  (send self :add-slot 'lines t)
  (send self :plot-parent plot-parent)
  (send self :title (send self :plot-name))
  (send self :showing-labels t)
  (send self :variable-label (iseq (length dims)) (make-dim-labels dims))
  (send self :add-mouse-mode 'newselecting
                :title "New Selecting" :cursor 'arrow :click :newselect)
  (send self :add-mouse-mode 'zoom
                :title "Zooming" :cursor 'finger :click :get-zoom-points)
  (send self :add-overlay zoom-overlay)
  (if (kind-of-p plot-parent score-plot-proto)
       (send self :overlay-list (list zoom-overlay))
       (progn
        (send self :add-overlay lines-overlay)
        (send self :overlay-list (list zoom-overlay lines-overlay))))
  (send self :mouse-mode 'newselecting)
  (send self :start-buffering)
  (send plot-parent :make-points)
  (send plot-parent :make-point-labels)
  (send plot-parent :selected-points)
  (send plot-parent :make-lines)
  (send self :adjust-to-data)
  (send self :buffer-to-screen)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :isnew methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-proto :isnew ()
  (let ((homals-dialog (send homals-dialog-proto :new self)))
   (send self :dialog homals-dialog)))

(defmeth homals-dialog-proto :isnew (homals-parent)
  (send self :homals-parent homals-parent)
  (send self :add-read-data)
  (send self :add-names-items)
  (send self :add-output-file)
  (send self :add-compute)
  (send self :add-converge)
  (send self :add-start-plot)
  (call-next-method (slot-value 'dialog-items)
                    :title "Homals Dialog"))

(defmeth homals-plot-dialog-proto :isnew (homals-parent)
  (send self :homals-parent homals-parent)
  (send self :add-plot-items)
  (call-method dialog-proto :isnew (slot-value 'dialog-items)
                            :title "Plot Dialog"
                            :go-away nil))

(defmeth homals-proto :initialize-slots ()
 (send self :z nil)
 (send self :m nil)
 (send self :n nil)
 (send self :eps-0 nil)
 (send self :eps-1 nil)
 (send self :active-variables nil)
 (send self :active-categories nil)
 (send self :category-labels nil)
 (send self :variable-labels nil)
 (send self :object-labels nil))


(defmeth homals-dialog-proto :close ()
 (let ((plot-dialog (send self :plot-dialog)))
   (send plot-dialog :dispose)
   (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Dialog item creator methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-dialog-proto :add-dialog-item (items &optional (stick nil))
  (let ((dialog-items (send self :dialog-items)))
    (send self :dialog-items 
         (if stick (append (butlast dialog-items)
                           (list (combine (last dialog-items) items)))
                   (append dialog-items items)))))

(defmeth homals-dialog-proto :add-converge ()
 (let* ((homals-parent (send self :homals-parent))
        (converge-button (send button-item-proto :new "Convergence"
          :action #'(lambda () 
   (let* (
          (eps0-ask (send text-item-proto :new 
                             "Number of Decimal Places for Fit:"))
          (eps0-get (send edit-text-item-proto :new (format nil "~a"
               (round (abs (/ (log (send homals-parent :eps-0)) (log 10)))))
                :text-length 4))
          (eps1-ask (send text-item-proto :new
                             "Number of Decimal Places for Function Change:"))
          (eps1-get (send edit-text-item-proto :new (format nil "~a" 
               (round (abs (/ (log (send homals-parent :eps-1)) (log 10)))))
                :text-length 4))
          (ok (send modal-button-proto :new "Ok" :action #'(lambda ()
                        (send homals-parent :eps-0
                             (read-from-string (send eps0-get :text)))
                        (send homals-parent :eps-1
                             (read-from-string (send eps1-get :text))))))
          (dialog (send modal-dialog-proto :new
                             (list (list eps0-ask eps0-get)
                                   (list eps1-ask eps1-get) ok))))
   (send dialog :modal-dialog))))))
   (send self :add-dialog-item (list converge-button) 't)))
  

(defmeth homals-dialog-proto :add-compute ()
 (let* (
        (homals-parent (send self :homals-parent))
        (ask-dimension (send text-item-proto :new "Solution Dimension:"))
        (plot-dimension (send edit-text-item-proto :new "" :text-length 4))
        (ask-status-name (send text-item-proto :new
                      (pad-string "[Optional] Variable Status Filename:" 37)))
        (show-status-file (send edit-text-item-proto :new "" :text-length 20))
        (compute-button (send button-item-proto :new "Compute"
                :action #'(lambda ()
                  (let ((dim (send plot-dimension :text)))
                    (if (natural-numberp dim)
                        (progn
                         (send homals-parent :p (floor (read-from-string dim)))
                         (send homals-parent :active-variables
                            (send show-status-file :text))
                         (send homals-parent :compute))
                        (format t "~%Invalid Solution Dimension!~%"))))))
       )
  (send self :add-dialog-item (list (list ask-dimension plot-dimension)
                                    (list ask-status-name show-status-file)
                                    compute-button))))

(defmeth homals-dialog-proto :add-read-data ()
 (let* (
        (homals-parent (send self :homals-parent))
        (tell-data-file (send text-item-proto :new "Data Filename: "))
        (show-data-file (send edit-text-item-proto :new "" :text-length 20))
        (seperator (send text-item-proto :new
          (make-string 52 :initial-element #\_)))
        (get-data-file (send button-item-proto :new "Load Data File"
                   :action #'(lambda ()
                    (let ((file (send show-data-file :text)))
                      (when (> (length file) 0)
                            (send homals-parent :data (apply #'bind-columns
                                       (read-data-columns file)))
                            (send homals-parent :initialize-slots))))))
       )
    (send self :add-dialog-item 
           (list (list tell-data-file show-data-file)
                 (list get-data-file seperator)))))

(defmeth homals-dialog-proto :add-output-file ()
 (let* (
        (homals-parent (send self :homals-parent))
        (seperator (send text-item-proto :new
           (make-string 49 :initial-element #\_)))
        (ask-output-name (send text-item-proto :new
                       (pad-string "[Optional] Output Filename:" 37)))
        (show-output-file (send edit-text-item-proto :new "" :text-length 20))
        (get-output-file (send button-item-proto :new "Write Output File"
                   :action #'(lambda ()
                       (let ((output-file (send show-output-file :text)))
                         (if (and (send homals-parent :z)
                                  (> (length output-file) 0))
                             (progn
                               (send homals-parent :homals-numerical-output
                                   output-file)
                               (format t
                                "~%Done Writing Output File: ~a~%" output-file))
                           (if z
                            (format t  "Error: No Output Filename!~%")
                            (format t  "Error: No Solution Computed!~%")))))))
      )
 (send self :add-dialog-item
     (list (list ask-output-name show-output-file)
           (list get-output-file seperator)))))



(defmeth homals-dialog-proto :add-names-items ()
 (let* (
        (homals-parent (send self :homals-parent))
        (ask-obj-name (send text-item-proto :new
                      (pad-string "[Optional] Object Names Filename:" 37)))
        (show-obj-file (send edit-text-item-proto :new "" :text-length 20))
        (ask-var-name (send text-item-proto :new
                      (pad-string "[Optional] Variable Names Filename:" 37)))
        (show-var-file (send edit-text-item-proto :new "" :text-length 20))
        (seperator (send text-item-proto :new
             (make-string 50 :initial-element #\_)))
        (get-names-files (send button-item-proto :new "Get/Remove Files"
                       :action #'(lambda ()
            (send homals-parent :object-labels (send show-obj-file :text))
            (send homals-parent :variable-labels (send show-var-file :text)))))
       )
  (send self :add-dialog-item (list (list ask-obj-name show-obj-file)
                                    (list ask-var-name show-var-file)
                                    (list get-names-files seperator)))))

(defmeth homals-dialog-proto :add-start-plot ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot-dialog (send homals-plot-dialog-proto :new homals-parent))
        (plot-button (send button-item-proto :new "Fetch Plots"
            :action #'(lambda ()
                (let ((showing (send plot-dialog :showing)))
                   (if showing (send plot-dialog :hide-window)
                               (send plot-dialog :show-window))
                   (send plot-dialog :showing (not showing))))))
       )
   (send self :plot-dialog plot-dialog)
   (send self :add-dialog-item (list plot-button) 't)))

(defmeth homals-plot-dialog-proto :add-plot-items ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot-item (send choice-item-proto :new
                     (list "Star-Plot"
                           "Score-Plot"
                           "Individual Category Plots"
                           "Combined Category Plot"
                           "Combined Category and Object Plot"
                           "Discrimination Measure Plot") :value 0))
        (plot (send button-item-proto :new "Plot"
               :action #'(lambda ()
                (let* ((p (send homals-parent :p))
                       (get-dim (remove nil (get-dimension-dialog p))))
                   (when get-dim
                      (let ((dims (if (> p 1)
                                      get-dim
                                      (t (error
                                           "Invalid Solution Dimension")))))
                      (send self :plot-redirect dims
                                           (send plot-item :value))))))))
       )
  (send self :add-dialog-item (list plot-item plot))))


(defmeth homals-dialog-proto :plot-redirect (dims selection)
 (let ((homals-parent (send self :homals-parent)))
   (case selection
                (0 (send star-plot-proto :new dims homals-parent))
                (1 (send score-plot-proto :new dims homals-parent))
                (2 (send category-plot-proto :new dims homals-parent))
                (3 (send combined-category-plot-proto :new dims homals-parent))
                (4 (send category-object-plot-proto :new dims homals-parent))
                (5 (send discrimination-plot-proto :new dims homals-parent)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Make specific plots, after convergence
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  :isnew Plot Constructor Method
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth plot-route-proto :isnew (dims homals-parent &rest args)
 (let (
       (plot (if (= (length dims) 3)
             (apply #'send homals-3d-plot-proto :new 3 args)
             (apply #'send homals-2d-plot-proto :new 2 args)))
       )
  (if (= (length dims) 1) (send plot :y-axis nil))
  (send self :homals-parent homals-parent)
  (send self :dims dims)
  (send self :plot plot)
  plot
))

(defmeth plot-route-proto :send-to-plot (&rest args)
  (let ((plot (send self :plot)))
  (apply #'send plot args)))

(defmeth plot-route-proto :cleanup ()
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Discrimination Plot Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth discrimination-plot-proto :isnew (dims homals-parent)
 (let ((plot (call-next-method dims homals-parent)))
  (send plot :plot-name "Discriminaton Measures")
  (send plot :setup dims self)))

(defmeth discrimination-plot-proto :make-point-labels ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (vl (select (send homals-parent :variable-labels)
                        (send homals-parent :active-variables)))
       )
   (send plot :point-label (iseq (send plot :num-points)) vl)))
 
(defmeth discrimination-plot-proto :selected-points ()
 (let ((plot (send self :plot)))
  (send plot :point-selected (iseq (send plot :num-points)) t)))

(defmeth discrimination-plot-proto :make-lines (&optional point-list)
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (av (send homals-parent :active-variables))
        (point-list (if point-list point-list (iseq (length av))))
        (d-m (send homals-parent :d-m))
       )
 (dolist (j point-list)
     (send plot :add-lines
         (mapcar #'(lambda (x) (list (aref (elt d-m j) x x) 0)) dims)))))

(defmeth discrimination-plot-proto :make-points ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (d-m (send homals-parent :d-m))
       )
 (dotimes (j (length d-m))
   (let ((d (elt d-m j)))
     (send plot :add-points
         (mapcar #'(lambda (x) (list (aref d x x))) dims))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Category Quantifaction and Object Score Plot Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth category-object-plot-proto :isnew (dims homals-parent)
 (let ((plot (call-next-method dims homals-parent)))
   (send plot :plot-name "Combined Category and Object Plot")
   (send plot :setup dims self)))

(defmeth category-object-plot-proto :make-point-labels ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (z (break-columns (send homals-parent :z) dims))
        (yy (send homals-parent :k-j-list))
        (catlabel (mapcar #'(lambda (x) (iseq 1 x)) yy))
        (av (send homals-parent :active-variables))
        (vl (select (send homals-parent :variable-labels) av))
        (pt-labels (combine (mapcar #'(lambda (x y) (mapcar #'(lambda (z)
                    (format nil "~a~a" x z)) y)) vl catlabel)))
        (ol (send homals-parent :object-labels))
      )
  (send plot :point-label (iseq (send plot :num-points)) 
                          (combine pt-labels ol))))


(defmeth category-object-plot-proto :selected-points ()
 (let ((plot (send self :plot))
       (n (send (send self :homals-parent) :n)))
    (send plot :point-selected
               (iseq (- (send plot :num-points) n)) t)))


(defmeth category-object-plot-proto :make-lines (&optional point-list)
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (p (length dims))
        (k (sum (send homals-parent :k-j-list)))
        (point-list (if point-list (select point-list (which (< point-list k)))
                                   (iseq k)))
        (yvars (select (transpose (map-elements #'coerce (select (column-list 
                         (send homals-parent :y)) dims) 'list)) point-list))
       )
   (mapcar #'(lambda (x) (send plot :add-lines 
               (transpose (list x (repeat 0 p))))) yvars)))

(defmeth category-object-plot-proto :make-points ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (n (send homals-parent :n))
        (yvars (map-elements #'coerce 
                   (select (column-list (send homals-parent :y)) dims) 'list))
        (z (break-columns (send homals-parent :z) dims))
        (ylen (length (first yvars)))
       )
 (send plot :add-points yvars)
 (send plot :add-points (column-list z))
 (send plot :point-symbol (iseq ylen (+ ylen n (- 1))) 'x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Object Score Plot Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth score-plot-proto :isnew (dims homals-parent)
 (send self :add-slot 'len)
 (send self :add-slot 'var-scroll-list)
 (send self :add-slot 'cat-scroll-list)
 (let* (
        (data (send homals-parent :data))
        (vl (send homals-parent :variable-labels))
        (pass-vars (reverse (set-difference (iseq (send homals-parent :m))
                                    (send homals-parent :active-variables))))
        (plot (if pass-vars (call-next-method dims homals-parent :go-away nil)
                            (call-next-method dims homals-parent)))
      )
  (send plot :plot-name "Object Score Plot")
  (send plot :setup dims self)
  (when pass-vars
     (send self :get-modless-variable (select vl pass-vars)
                             (mapcar #'(lambda (x) (sort-data
                                 (remove-duplicates x :test 'equal)))
                                 (select (column-list data) pass-vars))
                             (select (column-list data) pass-vars)))
plot))

(defmeth score-plot-proto :make-point-labels ()
 (let ((plot (send self :plot))
       (ol (send (send self :homals-parent) :object-labels)))
   (send plot :point-label (iseq (send plot :num-points)) ol)))

(defmeth score-plot-proto :selected-points ()
 (let ((plot (send self :plot)))
  (send plot :point-selected (iseq (send plot :num-points)) t)))

(defmeth score-plot-proto :make-lines (&optional point-list)
)

(defmeth score-plot-proto :cleanup ()
 (let* ((homals-parent (send self :homals-parent))
        (m (send homals-parent :m))
        (av (send homals-parent :active-variables)))
   (if (< (length av) m) (send self :reset-scroll))))


(defmeth score-plot-proto :make-points ()
 (let (
       (z (column-list (break-columns 
                 (send (send self :homals-parent) :z) (send self :dims))))
       (plot (send self :plot))
      )
  (send plot :add-points z)))

(defmeth score-plot-proto :reset-scroll ()
  (let ((len (send self :len))
        (cat-scroll (send self :cat-scroll-list)))
    (when len
      (send cat-scroll :selection nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dialog For Score Plot Proto
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth score-plot-proto :get-modless-variable 
                         (passvarlabels ac-pass-vars pass-data-vars)
  (let* (
         (num-pts (send self :send-to-plot :num-points))
         (symbs (repeat (select (plot-symbol-symbols)
                                (list 8 4 1 2 10 11 7 0)) 5))
         (len (max (mapcar #'length ac-pass-vars)))
         (ask-variable (send text-item-proto :new "Passive Variable:"))
         (var-scroll-list (send list-item-proto :new passvarlabels
                  :action #'(lambda (x)
                    (let ((pass-cat (elt ac-pass-vars 
                                       (send var-scroll-list :selection)))
                          (pass-data (elt pass-data-vars
                                 (send var-scroll-list :selection))))
                       (send self :send-to-plot :point-selected
                              (send self :send-to-plot :points-showing) nil)
                       (send self :reset-scroll)
                       (mapcar #'(lambda (y) 
                               (send cat-scroll-list :set-text y ""))
                           (iseq len))
                       (mapcar #'(lambda (y z)
                           (send cat-scroll-list :set-text z
                                       (format nil "~a" y)))
                            pass-cat
                           (iseq (length pass-cat)))
                       (send ask-symbols :do-action)))))
         (tell-cats (send text-item-proto :new "Categories:"))
         (cat-scroll-list (send list-item-proto :new (repeat "" len)
            :action #'(lambda (x)
              (send self :send-to-plot :point-selected
                           (send self :send-to-plot :points-showing) nil)
              (if (> (length (elt (send cat-scroll-list :slot-value 'list-data) 
                                    (send cat-scroll-list :selection))) 0)
                    (progn
                     (let* ((in-cat-pts (= (elt (elt ac-pass-vars
                                        (send var-scroll-list :selection))
                                  (send cat-scroll-list :selection))
                                (elt pass-data-vars
                                  (send var-scroll-list :selection))))
                            (showing-pts (send self :send-to-plot :point-showing
                                        (iseq num-pts)))
                            (match (which (mapcar #'(lambda (y z) (and y z))
                                      (coerce in-cat-pts 'list) showing-pts))))
                    (if match
                     (send self :send-to-plot :point-selected match 't))))))))
         (ask-symbols (send toggle-item-proto :new "Use Symbols"
                :action #'(lambda ()
               (if (send ask-symbols :value)
                    (let ((pass-cat (elt ac-pass-vars
                                      (send var-scroll-list :selection)))
                          (pass-data (elt pass-data-vars
                                      (send var-scroll-list :selection))))
                      (mapcar #'(lambda (y w)
                          (let ((ypass (which (= y pass-data))))
                            (send self :send-to-plot :point-symbol ypass
                                          (repeat w (length ypass)))))
                         pass-cat
                         (select symbs (iseq (length pass-cat)))))
                      (send self :send-to-plot :point-symbol
                         (iseq num-pts) (repeat 'disk num-pts)))
               (send self :send-to-plot :redraw))))

         (close (send button-item-proto :new "Close Plot"
                      :action #'(lambda () (send dialog :remove)
                                           (send self :send-to-plot :close))))
         (dialog (send dialog-proto :new 
                 (list (list ask-variable var-scroll-list)
                                  (list tell-cats cat-scroll-list)
                                  (remove nil 
                                    (list close 
                                         (if (kind-of-p (send self :plot) 
                                                         homals-2d-plot-proto)
                                          ask-symbols))))
                                    :title "Passive Variable Dialog"
                                    :go-away nil))
        )
  (send var-scroll-list :selection 0)
  (send self :len len)
  (send self :var-scroll-list var-scroll-list)
  (send self :cat-scroll-list cat-scroll-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Individual Category Quantification Plot Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth category-plot-proto :isnew (dims homals-parent)
 (let* (
        (av (send homals-parent :active-variables))
        (vl (select (send homals-parent :variable-labels) av))
        (ij (get-modal-variable vl))
        (plot (call-next-method dims homals-parent))
       )
 (send self :add-slot 'ij ij)
 (send plot :plot-name (format nil "Category Plot: Variable ~a" (elt vl ij))) 
 (send plot :setup dims self)))


(defmeth category-plot-proto :make-point-labels ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (ij (send self :ij))
        (av (send homals-parent :active-variables))
        (ac (send homals-parent :active-categories))
        (k-j (elt (send homals-parent :k-j-list) ij))
        (labels (mapcar #'(lambda (x) (format nil "~a~a"
                  (elt (select (send homals-parent :variable-labels) av) ij) x))
                    (iseq 1 k-j)))
       )
  (send plot :point-label (iseq (send plot :num-points)) labels)))

(defmeth category-plot-proto :selected-points ()
 (let ((plot (send self :plot)))
  (send plot :point-selected (iseq (send plot :num-points)) t)))


(defmeth category-plot-proto :make-lines (&optional point-list)
 (let* (
        (homals-parent (send self :homals-parent))
        (y (send homals-parent :y))
        (plot (send self :plot))
        (dims (send self :dims))
        (p (length dims))
        (ij (send self :ij))
        (k-j-cumsum (cumsum (cons 0 (send homals-parent :k-j-list))))
        (i-inds (iseq (elt k-j-cumsum ij) (1- (elt k-j-cumsum (1+ ij)))))
        (yy (column-list (select y i-inds dims)))
        (point-list (if point-list point-list (iseq (length (first yy)))))
       )
    (mapcar #'(lambda (x) (send plot :add-lines
                (transpose (list x (repeat 0 p)))))
                (transpose (mapcar #'(lambda (x)
                            (select (coerce x 'list) point-list)) yy)))))


(defmeth category-plot-proto :make-points ()
 (let* (
        (homals-parent (send self :homals-parent))
        (y (send homals-parent :y))
        (plot (send self :plot))
        (dims (send self :dims))
        (ij (send self :ij))
        (k-j-cumsum (cumsum (cons 0 (send homals-parent :k-j-list))))
        (i-inds (iseq (elt k-j-cumsum ij) (1- (elt k-j-cumsum (1+ ij)))))
        (yy (column-list (select y i-inds dims)))
       )
   (send plot :add-points yy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Combined Category Quantification Plot Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth combined-category-plot-proto :isnew (dims homals-parent)
 (let ((plot (call-next-method dims homals-parent)))
   (send plot :plot-name "Combined Category Quantification Plot")
   (send plot :setup dims self)))

(defmeth combined-category-plot-proto :make-point-labels ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (z (break-columns (send homals-parent :z) dims))
        (yy (send homals-parent :k-j-list))
        (catlabel (mapcar #'(lambda (x) (iseq 1 x)) yy))
        (av (send homals-parent :active-variables))
        (vl (select (send homals-parent :variable-labels) av))
        (pt-labels (combine (mapcar #'(lambda (x y) (mapcar #'(lambda (z)
                    (format nil "~a~a" x z)) y)) vl catlabel)))
       )
   (send plot :point-label (iseq (send plot :num-points)) pt-labels)))

(defmeth combined-category-plot-proto :selected-points ()
 (let ((plot (send self :plot)))
  (send plot :point-selected (iseq (send plot :num-points)) t)))


(defmeth combined-category-plot-proto :make-lines (&optional point-list)
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (p (length dims))
        (k (sum (send homals-parent :k-j-list)))
        (point-list (if point-list (select point-list (which (< point-list k)))
                                   (iseq k)))
        (yvars (select (transpose (map-elements #'coerce (select (column-list
                         (send homals-parent :y)) dims) 'list)) point-list))
       )
   (mapcar #'(lambda (x) (send plot :add-lines
               (transpose (list x (repeat 0 p))))) yvars)))



(defmeth category-object-plot-proto :make-points ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (n (send homals-parent :n))
        (yvars (map-elements #'coerce
                   (select (column-list (send homals-parent :y)) dims) 'list))
        (z (break-columns (send homals-parent :z) dims))
        (ylen (length (first yvars)))
       )
 (send plot :add-points yvars)
 (send plot :add-points (column-list z))
 (send plot :point-symbol (iseq ylen (+ ylen n (- 1))) 'x)))



(defmeth combined-category-plot-proto :make-points ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (dims (send self :dims))
        (n (send homals-parent :n))
        (yvars (map-elements #'coerce
                   (select (column-list (send homals-parent :y)) dims) 'list))
        (ylen (length (first yvars)))
       )
 (send plot :add-points yvars)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Star Plot Plot Prototype
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth star-plot-proto :isnew (dims homals-parent)
 (let* (
        (av (send homals-parent :active-variables))
        (vl (select (send homals-parent :variable-labels) av))
        (ij (get-modal-variable vl))
        (plot (call-next-method dims homals-parent))
       )
 (send self :add-slot 'ij ij)
 (send plot :plot-name (format nil "Star Plot: Variable ~a" (elt vl ij)))
 (send plot :setup dims self)))

(defmeth star-plot-proto :make-point-labels ()
 (let* (
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (ij (send self :ij))
        (n (send homals-parent :n))
        (av (send homals-parent :active-variables))
        (vl (select (send homals-parent :variable-labels) av))
        (ol (send homals-parent :object-labels))
        (k-j (elt (send homals-parent :k-j-list) ij))
       )
  (send plot :point-label (iseq (send plot :num-points))
    (combine (to-string-list ol)
     (mapcar #'(lambda (x) (format nil "~a~a" (elt vl ij) x)) (iseq 1 k-j))))))

(defmeth star-plot-proto :selected-points ()
 (let ((plot (send self :plot))
       (n (send (send self :homals-parent) :n)))
    (send plot :point-selected (iseq n (send plot :num-points)) t)))

(defmeth star-plot-proto :make-lines (&optional point-list)
 (let* ( 
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (y (send homals-parent :y))
        (n (send homals-parent :n))
        (dims (send self :dims))
        (p (length dims))
        (z (break-columns (send homals-parent :z) dims))
        (ij (send self :ij))
        (av (send homals-parent :active-variables))
        (ac (send homals-parent :active-categories))
        (k-j-cumsum (cumsum (cons 0 (send homals-parent :k-j-list))))
        (i-inds (iseq (elt k-j-cumsum ij) (1- (elt k-j-cumsum (1+ ij)))))
        (yy (select y i-inds dims))
        (plotvar (elt (column-list (send homals-parent :data)) (elt av ij)))
        (gg (make-indicator plotvar (elt ac ij)))
        (aa (matmult gg yy))
        (point-list (if point-list (select point-list (which (< point-list n)))
                                   (iseq n)))
       )
    (dolist (i point-list)
         (send plot :add-lines (mapcar #'(lambda (x)
                            (list (aref z i x) (aref aa i x))) (iseq p))))))


(defmeth star-plot-proto :make-points ()
 (let* ( 
        (homals-parent (send self :homals-parent))
        (plot (send self :plot))
        (y (send homals-parent :y))
        (ij (send self :ij))
        (av (send homals-parent :active-variables))
        (dims (send self :dims))
        (z (break-columns (send homals-parent :z) dims))
        (k-j-cumsum (cumsum (cons 0 (send homals-parent :k-j-list))))
        (i-inds (iseq (elt k-j-cumsum ij) (1- (elt k-j-cumsum (1+ ij)))))

        (yy (column-list (select y i-inds dims)))
       )
  (send plot :add-points (column-list z))
  (send plot :add-points yy :draw nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Create Standard Accessor Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro normal-accessor (key slot prototype)
`(defmeth ,prototype ,key (&optional (content nil set))
   (when set (setf (slot-value ',slot) content))
   (slot-value ',slot)))

(normal-accessor :data data homals-proto)
(normal-accessor :z z homals-proto)
(normal-accessor :p p homals-proto)
(normal-accessor :dialog dialog homals-proto)
(normal-accessor :y y homals-proto)

(normal-accessor :homals-parent homals-parent homals-dialog-proto)
(normal-accessor :dialog-items dialog-items homals-dialog-proto)
(normal-accessor :plot-dialog plot-dialog homals-dialog-proto)

(normal-accessor :showing showing homals-plot-dialog-proto)

(normal-accessor :plot-parent plot-parent homals-plot-proto)
(normal-accessor :zoom zoom homals-plot-proto)
(normal-accessor :lines lines homals-plot-proto)
(normal-accessor :plot-name plot-name homals-plot-proto)
(normal-accessor :overlay-list overlay-list homals-plot-proto)


(normal-accessor :plot plot plot-route-proto)
(normal-accessor :ij ij plot-route-proto)
(normal-accessor :dims dims plot-route-proto)
(normal-accessor :homals-parent homals-parent plot-route-proto)
(normal-accessor :len len score-plot-proto)
(normal-accessor :var-scroll-list var-scroll-list score-plot-proto)
(normal-accessor :cat-scroll-list cat-scroll-list score-plot-proto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Create Specialized Accessor Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-proto :m (&optional (val nil set))
 (if set 
    (setf (slot-value 'm) (array-dimension (send self :data) 1))
    (slot-value 'm)))
   
(defmeth homals-proto :n (&optional (val nil set))
 (if set
    (setf (slot-value 'n) (array-dimension (send self :data) 0)) 
    (slot-value 'n)))

(defmeth homals-proto :eps-0 (&optional (eps nil set))
 (if set
      (if (pos-numberp eps) 
          (setf (slot-value 'eps-0) (^ 10 (- eps)))
          (setf (slot-value 'eps-0) (^ 10 (- 1))))
      (slot-value 'eps-0)))
      
(defmeth homals-proto :eps-1 (&optional (eps nil set))
 (if set
      (if (pos-numberp eps)
          (setf (slot-value 'eps-1) (^ 10 (- eps)))
          (setf (slot-value 'eps-1) (^ 10 (- 1))))
      (slot-value 'eps-1)))

(defmeth homals-proto :active-variables (&optional (file nil set))
 (if set
  (let ((m (send self :m)))
   (if m
    (setf (slot-value 'active-variables)
           (if (> (length file) 0)
               (let ((file-cont (combine (read-data-columns file))))
                 (if (and (= m (length file-cont)) (one-or-zero-p file-cont))
                   (select (iseq m) (which (= 1 file-cont)))
                   (progn 
                    (format t
                      "~%~%Wrong Number of Indicators for Variable ~
                             Status in File: ~a ~%~
                       All Variables will be treated as Active~%" file)
                    (iseq m))))
               (iseq m)))))
 (slot-value 'active-variables)))

(defmeth homals-proto :active-categories (&optional (val nil set))
 (if set
  (let ((data (column-list (send self :data)))
        (active-variables (send self :active-variables)))
   (if data
   (setf (slot-value 'active-categories)
         (mapcar #'(lambda (x)
                    (remove 99 (sort-data (remove-duplicates x :test 'equal))))
                               (select data active-variables)))))
  (slot-value 'active-categories)))

(defmeth homals-proto :category-labels (&optional (val nil set))
 (if set
   (let ((data (column-list (send self :data))))
      (if data
      (setf (slot-value 'category-labels)
            (mapcar #'(lambda (x) (to-string-list (iseq 1 x)))
                        (mapcar #'number-of-values data)))))
   (slot-value 'category-labels)))

(defmeth homals-proto :object-labels (&optional (file nil set))
 (if set
  (let ((n (send self :n)))
   (if n
    (setf (slot-value 'object-labels)
        (to-string-list
          (if (> (length file) 0)
              (let ((file-cont (combine (read-data-columns file))))
                (if (= n (length file-cont))
                    file-cont
                    (progn
                      (format t
                       "~%~%Wrong Number of Object Labels in File: ~a~%~
                        Using default labels 1, 2, . . ., n for Objects~%" file)
                      (iseq 1 n))))
              (iseq 1 n))))))
  (slot-value 'object-labels)))

(defmeth homals-proto :variable-labels (&optional (file nil set))
 (if set
  (let ((m (send self :m)))
   (if m
    (setf (slot-value 'variable-labels)
        (to-string-list
              (if (> (length file) 0)
                  (let ((file-cont (combine (read-data-columns file))))
                   (if (= m (length file-cont))
                       file-cont
                       (progn  
                         (format t
                         "~%~%Wrong Number of Variable Labels in File: ~a ~%~
                          Using default labels A, B, C, ..., ~
                          for Variables~%" file)
                         (make-variable-labels (iseq m)))))
                  (make-variable-labels (iseq m)))))))
   (slot-value 'variable-labels)))


(defmeth homals-proto :output-file (&optional (file nil set))
  (if set 
   (setf (slot-value 'output-file) (if (> (length file) 0) file nil))
   (slot-value 'output-file)))

(defmeth homals-proto :k-j-list (&optional (val nil set))
 (if set
   (if val
      (setf (slot-value 'k-j-list) (append (slot-value 'k-j-list) (list val)))
      (setf (slot-value 'k-j-list) nil))
   (slot-value 'k-j-list)))

(defmeth homals-proto :d-m (&optional (val nil set))
 (if set
  (if val
     (setf (slot-value 'd-m) (append (slot-value 'd-m) (list val)))
     (setf (slot-value 'd-m) nil))
   (slot-value 'd-m)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Allows Removal/Replacement of Overlays for Saving a Plot Image
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-plot-proto :menu-template ()
  (flet ((remove-overlays ()
            (mapcar #'(lambda (x) (send self :delete-overlay x))
                  (send self :overlay-list))
            (if (kind-of-p self homals-3d-plot-proto)
                  (send self :margin 0 0 0 25)
                  (send self :margin 0 0 0 0)))
        (replace-overlays ()
            (let ((wid (send self :text-width "Zoom")))
              (if (kind-of-p self homals-3d-plot-proto)
                    (send self :margin (* 2 wid) 0 0 25)
                    (send self :margin (* 2 wid) 0 0 0))
              (mapcar #'(lambda (x) (send self :add-overlay x))
                 (send self :overlay-list))
              (send self :redraw-overlays))))
    (let ((item (send menu-item-proto
                  :new "Overlays"
                  :action #'(lambda () 
                    (let ((val (slot-value 'mark)))
                     (if val (remove-overlays) (replace-overlays))
                     (setf (slot-value 'mark) (not val)))))))
       (send item :mark 't)
       (append (call-next-method) (list item)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Miscelaneous Dialog and helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth homals-list-item-proto :last-click (&optional (val nil set))
  (if set (setf (slot-value 'last-click) val)
          (slot-value 'last-click)))

(defun list-item (p)
 (send homals-list-item-proto :new (to-string-list (iseq 1 p))
                :action #'(lambda (x) (let ((sel (send self :selection)))
                     (when x (if (= sel (send self :last-click))
                                 (send self :selection nil)))
                     (send self :last-click (if sel sel 0))))))

(defun get-dimension-dialog (p)
  (let* (
         (n (min 3 p))
         (ask (send text-item-proto :new "Enter one or more dimensions"))
         (tell (mapcar #'(lambda (x) (send text-item-proto :new
                            (format nil "Dim ~a:" x))) (iseq 1 n)))
         (vars (mapcar #'(lambda (x) (list-item p)) (iseq 1 n)))
         (ok (send modal-button-proto :new "Ok"
               :action #'(lambda ()
                 (let ((vals (remove nil
                          (mapcar #'(lambda (x) (send x :selection)) vars))))
                     vals))))
         (dialog (send modal-dialog-proto :new (cons (list ask)
            (append (list (combine (transpose (list tell vars))))
             (list ok)))))
        )
    (mapcar #'(lambda (x y) (send x :selection y)) vars 
                              (if (= 2 n) (list 0 1) (list 0 1 nil)))
    (mapcar #'(lambda (x y) (send x :last-click y)) vars
                              (if (= 2 n) (list 0 1) (list 0 1 0)))
    (send dialog :modal-dialog)))


(defun get-modal-variable (seq)
  (let* (
         (ask-variable (send text-item-proto :new "Variable:"))
         (scroll-list (send list-item-proto :new seq))
         (ok (send modal-button-proto :new "Ok"
                     :action #'(lambda () (send scroll-list :selection))))
         (dialog (send modal-dialog-proto :new
                       (list (list ask-variable scroll-list) (list ok))))
        )
    (send scroll-list :selection 0)
    (send dialog :modal-dialog)))


(defun to-string-list (l)
  "Args: l
Converts L to a list of strings."
  (mapcar #'(lambda (x) (format nil "~a" x)) l))

(defun make-dim-labels (dims)
  (mapcar #'(lambda (x) (format nil "Dim ~a" x)) (1+ dims)))

(defun make-variable-labels (l)
 (let* ((len (length l))
        (num-rep (ceiling (/ len 52)))
        (str-seq (mapcar #'string (combine (iseq 65 90) (iseq 97 122)))))
   (dotimes (i num-rep)
           (setf str-seq (append str-seq 
              (map-elements #'concatenate 'string
                (format nil "~a" (1+ i))
                (mapcar #'string (combine (iseq 65 90) (iseq 97 122)))))))
    (select (combine str-seq) l)))


(defun break-columns (z dims)
 (apply #'bind-columns (select (column-list z) dims)))

(defun pos-numberp (num)
  (if (numberp num)
      (if (> num 0) t nil) nil))

(defun natural-numberp (dim)
  (if (> (length dim) 0)
      (let ((dimval (read-from-string dim)))
        (if (numberp dimval)
            (if (and (> dimval 0) (= (mod dimval 1) 0))
                dimval)))))


(defun valid-natural-numberp (dim p)
  (if (> (length dim) 0)
      (let ((dimval (read-from-string dim)))
        (if (numberp dimval)
            (if (and (< 0 dimval (1+ p)) (= (mod dimval 1) 0))
                dimval)))))

(defun pad-string (str num)
 (let ((pad-length (- num (length str))))
    (concatenate 'string str (make-string pad-length :intial-element #\ ))))

(defun homals ()
  (let ((homals-object (send homals-proto :new)))
homals-object))

(defun one-or-zero-p (l)
  (not (member nil (mapcar #'(lambda (x) (or (equalp x 1) (equalp x 0))) l))))


(defun bullet ()
'#2a((0 1 1 1 1 1 1 1 1 0)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 1 1 1 1 0 0 1)
     (1 0 1 1 1 1 1 1 0 1)
     (1 0 1 1 1 1 1 1 0 1)
     (1 0 1 1 1 1 1 1 0 1)
     (1 0 1 1 1 1 1 1 0 1)
     (1 0 0 1 1 1 1 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (0 1 1 1 1 1 1 1 1 0)))

(defun empty ()
'#2a((0 1 1 1 1 1 1 1 1 0)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (1 0 0 0 0 0 0 0 0 1)
     (0 1 1 1 1 1 1 1 1 0)))

(defun check ()
'#2a((0 1 1 1 1 1 1 1 1 0)
     (1 1 0 0 0 0 0 0 1 1)
     (1 0 1 0 0 0 0 1 0 1)
     (1 0 0 1 0 0 1 0 0 1)
     (1 0 0 0 1 1 0 0 0 1)
     (1 0 0 0 1 1 0 0 0 1)
     (1 0 0 1 0 0 1 0 0 1)
     (1 0 1 0 0 0 0 1 0 1)
     (1 1 0 0 0 0 0 0 1 1)
     (0 1 1 1 1 1 1 1 1 0)))


(homals)




