;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This module has a complicated history. The first version was written
;; by Jan de Leeuw, mid 1994, to provide an Xlisp-Stat version of the
;; S-routines in Haerdle's book, and to provide a histogram-proto that
;; inherits from the scatterplot-proto.
;;
;; The second version was done begin 1995 by Jason Bond, who added a
;; slider for the binwidth.
;;
;; Version 3.0 was done by Jan de Leeuw in March 1995. It involved a
;; complete overhaul of the code. XOR drawing was eliminated, menu
;; support improved, slider support improved, code made more modular,
;; my-histogram-proto and my-polygon-proto separated.
;;
;; 3.0     ** March 22 1995 ** Still no support for warping and fitting
;;         a normal.
;; 3.1     ** March 23 1995 ** Support for fitting a normal.
;;
;; 3.2     ** March 26 1995 ** Make sure the menu-items toggle in the
;;         correct way. Improved updating of plots, much greater
;;         efficiency. Created menu items for options. Wrote my-histogram
;;         function. Foundations put in for warping and overlays.
;;         Optimized normal-density fitting.
;;
;; 3.3     ** May 22, 1995 ** Support for different bin-widths.  Function
;;         call for different bin-widths is:
;;         (my-histogram data :my-cutpoints cut-points) where cutpoints
;;         is a list of cutoff points.  Of course, changing the bin-widths
;;         using the slider makes everything go back to a single bin-width.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "make-proto")
(require "fit-normal")

(defproto my-histogram-proto
  '(data            ;; the data
    binwidth        ;; the binwidth
    origin          ;; the origin
    histo           ;; is there a histogram in the plot ?
    poly            ;; is there a polygon in the plot ?
    normal          ;; is there a normal in the plot ?
    prob            ;; counts or probabilities
    full-hist       ;; is the histogram full or hollow ?
    with-points     ;; does the polygon have midpoints on it ?
    full-overlay    ;; histogram plot overlay
    point-overlay   ;; polygon plot overlay
    counts          ;; bin-counts
    cut-points      ;; cut-points for bins
    mid-points      ;; mid-points of bins
    slider)         ;; slot for slider
  ()
    scatterplot-proto)

(make-assessors my-histogram-proto
               (data)
               (binwidth
                origin
                histo
                poly
                normal
                full-hist
                with-points
                full-overlay
                point-overlay
                prob
                counts
                cut-points
                mid-points
                slider))

(defmeth my-histogram-proto :isnew 
  (the-data &key the-binwidth the-origin my-cutpoints)
  (call-method graph-proto :isnew 2 :title "Histogram")
(send self :set-data 
      (sort-data the-data))
(send self :set-histo t)

(send self :set-binwidth
     (if my-cutpoints my-cutpoints
                      (if the-binwidth the-binwidth
                                       (send self :make-binwidth))))

(if the-origin (send self :set-origin the-origin)
               (send self :make-origin))

(send self :set-counts
      (send self :make-counts))

(if my-cutpoints (send self :set-cut-points my-cutpoints)
                 (send self :set-cut-points
                    (send self :make-cut-points)))
(send self :set-mid-points
      (send self :make-mid-points))
(send self :set-full-overlay
      (send self :make-full-overlay))
(send self :set-point-overlay
      (send self :make-point-overlay))
(send self :set-slider nil)
(send self :set-normal nil)
(send self :set-full-hist t)
(send self :set-with-points t)
(send self :set-prob nil)
(send self :menu 
      (send self :make-menu))
(send self :make-plot)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth my-histogram-proto :make-menu ()
  (let* (
         (slider (send menu-item-proto :new "Change-Bins"
                      :action #'(lambda () 
                                  (send self :make-slider))))
         (histo (send menu-item-proto :new "Show Histogram"
                      :action #'(lambda () 
                                  (send self :toggle-histo-poly)
                                  (send self :toggle-plot)
                                  (send self :refit-normal))))
         (full (send menu-item-proto :new "Full Histogram"
                       :action #'(lambda ()
                                   (send self :toggle-full-option)
                                   (send self :toggle-plot))))
         (poly (send menu-item-proto :new "Show Polygon"
                      :action #'(lambda () 
                                  (send self :toggle-histo-poly)
                                  (send self :toggle-plot)
                                  (send self :refit-normal))))
         (points (send menu-item-proto :new "Show Midpoints"
                       :action #'(lambda ()
                                   (send self :toggle-midpoint-option)
                                   (send self :toggle-plot))))
         (normal (send menu-item-proto :new "Show Normal" 
                      :action #'(lambda ()
                                  (send self :toggle-normal-item)
                                  (send self :toggle-normal-plot))))
         (dash (send dash-item-proto :new))
         (menu (send menu-proto :new "Histogram"))
         )
(send menu :append-items
      slider dash histo full dash poly points dash normal)
(send histo :mark t)
(send full :mark t)
(send points :mark t)
(send points :enabled nil)
menu))

(defmeth my-histogram-proto :toggle-histo-poly ()
  (let ((mm (send (send self :menu) :find-item "Show Histogram"))
        (mn (send (send self :menu) :find-item "Show Polygon"))
        (mo (send (send self :menu) :find-item "Full Histogram"))
        (mp (send (send self :menu) :find-item "Show Midpoints")))
    (send self :set-histo
          (not (send self :set-histo)))
    (send self :set-poly
          (not (send self :set-poly)))
    (send mn :mark
          (if (send self :set-poly) t nil))
    (send mm :mark
          (if (send self :set-histo) t nil))
    (cond ((send self :set-poly)
           (send mo :enabled nil)
           (send mp :enabled t))
          ((send self :set-histo)
           (send mo :enabled t)
           (send mp :enabled nil)))
    ))

(defmeth my-histogram-proto :toggle-full-option ()
(let ((mo (send (send self :menu) :find-item "Full Histogram")))
  (send self :set-full-hist
        (not (send self :set-full-hist)))
  (send mo :mark
        (if (send self :set-full-hist) t nil))
))

(defmeth my-histogram-proto :toggle-midpoint-option ()
  (let ((mp (send (send self :menu) :find-item "Show Midpoints")))
    (send self :set-with-points
          (not (send self :set-with-points)))
    (send mp :mark
          (if (send self :set-with-points) t nil))
    ))
 
(defmeth my-histogram-proto :toggle-normal-item ()
  (let ((mm (send (send self :menu) :find-item "Show Normal")))
    (send self :set-normal
          (not (send self :set-normal)))
    (send mm :mark
          (if (send self :set-normal) t nil))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Slider Support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth my-histogram-proto :make-slider ()
  (let* ((data (send self :set-data))
         (sdat (spacing data))
         (rdat (range data))
         (binwid (send self :set-binwidth))
         (slide (interval-slider-dialog (list sdat rdat) :title "Binwidth"
                                  :action #'(lambda (x)
                                              (send self :set-binwidth
                                                    (max x sdat))
                                              (send self :recompute)
                                              (send self :make-plot)))))
    (send slide :value binwid)
    (send self :set-slider slide)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Plot Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth my-histogram-proto :refit-normal ()
  (let* ((aa (fit-normal (send self :convert-linestarts)))
         (nn (send self :num-lines))
         (ll (+ (- nn 50) (iseq 50))))
    (send self :linestart-coordinate 0 ll (first aa))
    (send self :linestart-coordinate 1 ll (second aa))
    (if (send self :set-normal)
        (send self :toggle-normal-plot))
    ))
        
(defmeth my-histogram-proto :make-plot () 
  (send self :clear)
  (send self :horizontal-lines)
  (send self :hollow-vertical-lines)
  (send self :full-vertical-lines)
  (send self :poly-lines)
  (send self :toggle-plot)
  (send self :normal-lines)
  (send self :toggle-normal-plot)
  )

(defmeth my-histogram-proto :toggle-plot ()
  (let* ((nn (send self :num-bins))
         (ll (* 2 (iseq (1- (* 4 nn)))))
         (lh (select ll (iseq nn)))
         (lo (select ll (+ nn (iseq (1+ nn)))))
         (lf (select ll (+ (+ nn nn 1) (iseq (1- nn)))))
         (lp (select ll (+ (* 3 nn) (iseq (1- nn))))))
    (cond ((send self :set-histo)
           (send self :linestart-next lh (1+ lh))
           (send self :linestart-next lo (1+ lo))
           (if (send self :set-full-hist)
               (send self :linestart-next lf (1+ lf))
             (send self :linestart-next lf nil))
           (send self :linestart-next lp nil)
           (send self :clear-points))
          ((send self :set-poly)
           (send self :linestart-next lh nil)
           (send self :linestart-next lo nil)
           (send self :linestart-next lf nil)
           (send self :linestart-next lp (1+ lp))
           (if (send self :set-with-points)
               (send self :poly-points)
             (send self :clear-points))))
    (send self :adjust-to-data)
))

(defmeth my-histogram-proto :toggle-normal-plot ()
  (let* ((nn (send self :num-lines))
         (ll (+ (- nn 50) (iseq 49))))
    (if (send self :set-normal)
        (send self :linestart-next ll (1+ ll))
      (send self :linestart-next ll nil))
    (send self :adjust-to-data)
    ))
             
(defmeth my-histogram-proto :horizontal-lines ()

  (let* ((cn (send self :set-counts))
         (cp (send self :set-cut-points))
         (nn (length cn)))
    (dotimes (i nn)
      (let ((ci (elt cp i))
            (cj (elt cn i))
            (ck (elt cp (1+ i))))
        (send self :add-lines (list ci ck) (list cj cj) :draw nil)))
    ))

(defmeth my-histogram-proto :poly-lines ()
  (let* ((mp (send self :set-mid-points))
         (cn (send self :set-counts))
         (nn (1- (length cn))))
    (dotimes (i nn)
      (let ((ci (elt mp i))
            (cj (elt mp (1+ i)))
            (ck (elt cn i))
            (cl (elt cn (1+ i))))
        (send self :add-lines (list ci cj) (list ck cl) :draw nil)))
    ))

(defmeth my-histogram-proto :poly-points ()
  (let* ((mp (send self :set-mid-points))
         (cn (send self :set-counts)))
    (send self :add-points mp cn :draw nil))
  )

(defmeth my-histogram-proto :hollow-vertical-lines ()
  (let* ((cn (send self :set-counts))
         (cp (send self :set-cut-points))
         (nn (length cp)))
    (send self :add-lines (repeat (first cp) 2)
          (list 0 (first cn)) :draw nil)
    (send self :add-lines (repeat (first (last cp)) 2)
          (list 0 (first (last cn))) :draw nil)

    (dolist (i (1+ (iseq (- nn 2))))
      (let ((ci (elt cp i))
            (cj (elt cn (1- i)))
            (ck (elt cn i)))
        (send self :add-lines (list ci ci)
              (list (min cj ck) (max cj ck)) :draw nil)))
    ))

(defmeth my-histogram-proto :full-vertical-lines ()
  (let* ((cn (send self :set-counts))
         (cp (send self :set-cut-points))
         (nn (length cp)))
    (dolist (i (1+ (iseq (- nn 2))))
      (let ((ci (elt cp i))
            (cj (elt cn (1- i)))
            (ck (elt cn i)))
        (send self :add-lines (list ci ci)
              (list 0 (min cj ck)) :draw nil)))
    ))

(defmeth my-histogram-proto :normal-lines ()
  (let ((ll (fit-normal (send self :convert-linestarts))))
    (send self :add-lines (first ll) (second ll)
          :draw nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Compute Methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth my-histogram-proto :recompute ()
  (send self :set-counts
        (send self :make-counts))
  (send self :set-cut-points
        (send self :make-cut-points))
  (send self :set-mid-points
        (send self :make-mid-points))
  )

(defmeth my-histogram-proto :make-counts ()
  (let ((data (send self :set-data))
        (binwidth (send self :set-binwidth))
        (origin (send self :set-origin)))
    (count-in-bins data binwidth origin)
    ))


;;problem
(defmeth my-histogram-proto :make-cut-points ()
  (let* ((data (send self :set-data))
         (binwidth (send self :set-binwidth))
         (origin (send self :set-origin))
         (cn (length (send self :set-counts)))
         (minx (floor (/ (- (min data) origin) binwidth)))
         (maxx (ceiling (/ (- (max data) origin) binwidth)))
         (seq (if (= (1+ cn) (- (1+ maxx) minx))
                  (iseq minx maxx)
                  (if (= cn (- (1+ maxx) minx))
                      (iseq minx (1+ maxx))
                      (iseq minx (1- maxx))))))
    (+ origin (* binwidth seq))))

(defmeth my-histogram-proto :make-mid-points ()
  (let ((cp (send self :set-cut-points)))
    (/ (+ (butlast cp) (rest cp)) 2)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Initial Constructors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth my-histogram-proto :make-origin ()
  (send self :set-origin 
        (mean (send self :set-data))))

(defmeth my-histogram-proto :make-binwidth ()
  (send self :set-binwidth
        (if (send self :set-histo)
            (send self :opt-histo-binwidth)
          (send self :opt-poly-binwidth)))
  )

(defmeth my-histogram-proto :opt-histo-binwidth ()
"See Scott, Biometrika, 66, 1979, 605-610"
(let (
      (data (send self :set-data))
      )
(* (standard-deviation data) 
   (^ (* 24 (sqrt pi) (/ (length data))) (/ 3)))
))

(defmeth my-histogram-proto :opt-poly-binwidth ()
"See Scott, JASA, 80, 1985, 348-454"
(let (
      (data (send self :set-data))
      )
(* 2 (standard-deviation data) 
   (^ (* (/ 40 49) (sqrt pi) 
         (/ (length data))) (/ 5)))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Overlay support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmeth my-histogram-proto :make-full-overlay ()
  )

(defmeth my-histogram-proto :make-point-overlay ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Warp support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun warp-histogram (data binwidth origin m &key (kernel 0))
(let* (
      (n (length data))
      (d (/ binwidth m))
      (c (count-in-bins data d origin))
      (w (kernel-weights binwidth n m kernel))
      )
))

(defun kernel-weights (binwidth n m kernel)
(let* (
       (cm 0)
       (wm (repeat 0 m))
       (mm (iseq m))
       (nh (* n binwidth))
       (im (/ mm m))
       )
(case kernel 
  (0 (setf cm (/ m (* (1- (* 2 m)) nh)))
     (setf wm (repeat cm m)))
  (1 (setf cm (/ (* nh)))
     (setf wm (* cm (- 1 im))))
  (2 (setf cm (/ (* 3 m m) (* nh (1- (* 4 m m)))))
     (setf wm (* cm (- 1 (^ im 2)))))
  (3 (setf cm (/ (* 15 (^ m 4)) (* nh (1- (* 16 (^ m 4))))))
     (setf wm (* cm (^ (- 1 (^ im 2)) 2))))
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-in-bins (data binwidth origin)
(if (listp binwidth)
    (progn 
      (let (
            (beg-bin (butlast binwidth))
            (end-bin (rest binwidth))
            (bcnt (repeat 0 (1- (length binwidth))))
           )
      (mapcar #'(lambda (x) 
            (let ((posn (first (which (if-else (< beg-bin x end-bin) t nil)))))
                    (setf (elt bcnt posn) (1+ (elt bcnt posn))))) data)
      bcnt))

    (progn (let* (
                  (indx (floor (/ data binwidth)))
                  (jndx (- indx (min indx)))
                  (nbin (1+ (max jndx)))
                  (bcnt (repeat 0 nbin))
                 )
            (mapcar #'(lambda (x) (setf (elt bcnt x) (1+ (elt bcnt x)))) jndx)
bcnt
))))

(defun spacing (data)
  (min (abs (difference (sort-data data))))
)

(defun range (data)
  (- (max data) (min data)))

(defmeth menu-proto :find-item (str)
  "Method args: (str)
Finds and returns menu item with tile STR."
  (dolist (item (send self :items))
    (if (string-equal str (send item :title))
        (return item))))

(defmeth my-histogram-proto :close ()
  (if (send self :set-slider)
      (send (send self :set-slider) :close))
  (call-next-method)
  )

(defmeth my-histogram-proto :num-bins ()
  (length (send self :set-counts))
  )

(defmeth my-histogram-proto :num-cuts ()
  (length (send self :set-cut-points))
  )

(defun my-histogram (data &key (binwidth nil have-bin)
                          (origin nil have-origin)
                          (my-cutpoints nil points))
  (cond ((and have-bin have-origin)
         (send my-histogram-proto :new data :the-binwidth binwidth
               :the-origin origin))
        (have-bin
         (send my-histogram-proto :new data :the-binwidth binwidth))
        (have-origin
         (send my-histogram-proto :new data :the-origin origin))
        (points 
         (send my-histogram-proto :new data :my-cutpoints my-cutpoints))
        (t 
(send my-histogram-proto :new data)
))
)

;(def dat (normal-rand 100))

;(my-histogram dat :my-cutpoints (list -4  0 1 1.5 6))

