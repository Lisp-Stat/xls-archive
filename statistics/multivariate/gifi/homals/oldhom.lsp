;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is a module for doing Homogeneity Analyses using
;; Alternating Least Squares (HOMALS). Homogeneity Analysis is also
;; known as Multiple Correspondence Analysis. This includes a demo,
;; because it can actually shows all iterations, and the path they
;; take.
;;
;; Just say (homals g), where g is a categorical data matrix, i.e.
;; a matrix with n rows (observations) and m columns (variables),
;; with each column containing either numbers or strings.
;; Keyword arguments are the dimensionality p of the solution, 
;; two precision parameters eps-0 and eps-1, the
;; maximum number of iterations, whether you want a graphics
;; demo or not, if you want output written to a file, and if you
;; want to be able to look at and save other kinds of graphical output,
;; and which type of iteration you want to use.
;;
;; A first version was written about three years ago (in rather
;; embarassing code). The second version is a complete overhaul, done
;; at UPF in May 1995. It makes the package more modular, more functional,
;; more lisp-like, it makes it suitable for both demos and computing,
;; gets rid of global variables, makes it look good on both X11 and Mac
;; (on MSW I use the X11 options, which may not make sense, but I dont care),
;; gets rid of some coercions from matrix to vector, gives (nice) output to
;; a file, chooses between two iteration methods, has better stop
;; criteria, has more general input.
;;
;; In the back (actually, way in the back) of my mind there is a plan
;; to make this object-oriented, and to integrate it with Hildon-Minton's
;; gifi-gardens.
;;
;; Version 1.0 : Jan de Leeuw : May 15, 1995 (UPF, Spain)
;;
;; Version 1.1 : Jan de Leeuw : May 17, 1995 (Koeln, Germany)
;;      A quite spectacular crash on my PowerBook wiped out Version 1.1.
;;      I will keep its number alive, to remind me of the immense
;;      frustration I felt.
;;
;; Version 1.2 : Jan de Leeuw : June xx, 1995 (UPF, Spain)
;;
;;      This version is much extended, because it now allows for
;;      missing data; active and passive objects, active and passive
;;      categories, and active and passive variables;
;;      three-dimensional graphical output (although the
;;      built-in demo is always two-dimensional). The
;;      test for convergence has been changed, so that we do
;;      not spend much time in idle rotation. Labels have been
;;      added, and the graph-plot and joint-category-plot have been
;;      added to the graphical output menu/dialog. Parameters
;;      are checked for consistency (using continuable errors).
;;
;;      To prevent possible misunderstandings, a variable is
;;      active if and only if it participates in the analysis, the
;;      same thing is true for an object. A passive variable or passive
;;      object does appear in the numerical output, and additional
;;      graphical output can be requested as well. If a variable is
;;      passive, then all its categories are passive as well. But a passive
;;      object can very well be in active categories. If a particular
;;      category of an active variable is passive, then the observation
;;      is missing for that variable, and the indicator matrix is
;;      consequently incomplete (has a row of zeroes). Again, passive
;;      categories do appear in the output. Categories (active or
;;      passive) are always quantified as centroids of the active objects
;;      that are in them. Objects are quantified as the centroids of
;;      the active categories they are in, corrected for the eigenvalues.
;;
;;      In TeX notation we are minimizing the function
;;              $$\frac{1}{m}\sum_{j=1}^m
;;                      \mbox{tr }(X-G_jY_j)'M_\star(X-G_jY_j),$$
;;      where $G_j$ is the indicator matrix of variable $j$ (active
;;      objects and categories only), $Y_j$ are the category quanti-
;;      fications, and $X$ are the object scores, which are normalized by
;;      $\frac{1}{m}X'M_\star X=nI$ and by $u'M_\star X=0.$. The diagonal
;;      matrix $M_\star$, indicates the number of variables for which the
;;      active object is in an active category.
;;
;;      It is still true that all parameters are set by default (they
;;      are all keyword arguments), but this is crying out more and
;;      more for OOP.
;;
;;      The next major version will have restrictions on the parameters
;;      (i.e. PRINCALS etc.), reordering of the data, and induced
;;      correlation matrices.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun homals (data
               &key
               (ndim 2)                   ; # computing dimensions
               (pdim ndim)                ; # display dimensions
               (active-objects            ; list of active objects
                (iseq (first (array-dimensions data))))
               (active-variables          ; list of active variables
                (iseq (second (array-dimensions data))))
               (active-categories         ; list of active categories
                (mapcar #'(lambda (x)     ; for the active variables
                            (sort-data (remove-duplicates x :test 'equal)))
                        (select (column-list data) active-variables)))
               (object-labels             ; labels of all objects
                (to-string-list
                 (iseq (first (array-dimensions data)))))
               (variable-labels           ; labels of all variables
                (to-string-list
                 (iseq (second (array-dimensions data)))))
               (category-labels           ; labels of all categories
                (mapcar #'(lambda (x) (to-string-list (iseq x)))
                        (mapcar #'number-of-values
                                (column-list data))))
               (eps-0 1e-4)               ; function change
               (eps-1 1e-4)               ; solution change
               (itmax 100)                ; # iterations
               (demo nil)                 ; is there a demo ?
               (output-file "homals.out") ; is there output to a file ?
               (more-graphics nil)        ; is there graphics output ?
               (gram t)                   ; Gram or Procrustus ?
               (speed t))               ; Speed or Memory

"Args: data (&key (ndim 2) (pdim ndim)
 active-objects active-variables active-categories
 variable-labels category-labels object-labels
 (eps-0 1e-4) (eps-1 1e-4) (itmax 100)
 (demo nil) (output-file \"homals.out\") (more-graphics nil) 
 (gram t) (speed t))
--------------------------------------------------------------------------
Performs a Homogeneity Analysis (or Multiple Correspondence Analysis)
by the Alternating Least Squares (or Reciprocal Averaging) algorithm
of DATA in NDIM dimensions (PDIM are displayed). EPS-0, EPS-1, and ITMAX
regulate precision, DEMO indicates if you want a (two-dimensional) dynamic
graphics demonstration of the iterations (it is a graphical version of
VERBOSE). After convergence, output can be written to OUTPUT-FILE or some
additional graphs can be created if MORE-GRAPHICS is true. If GRAM is true,
Gram-Schmidt iterations are used, otherwise Procrustus iterations. If
SPEED is true, indicator matrices are made once and are stored"

(debug)

(if (< ndim pdim)
    (progn
      (cerror "Set display dimensionality to computing dimensionality"
              "Cannot display more dimensions then computed")
      (setf pdim ndim))
  
  )
(if (and (= 1 ndim) demo)
    (progn
      (cerror "Set number of computing dimensions to 2"
              "Cannot have a demo if only one dimension is computed")
      (setf ndim 2))
  )
(if (and (= 1 pdim) more-graphics)
    (progn
      (cerror "Set more-graphics to false"
              "Cannot display graphics in only one dimension")
      (setf more-graphics nil))
  )

(nodebug)

(let* ((n (first (array-dimensions data)))
       (e (select data (iseq n) active-variables))
       (m (length active-variables))
       (w (make-weights e active-categories active-objects))
       (x (make-random-orthonormal w n ndim))
       (q (if demo (homals-plot-demo-create-objects x)))
       (r (if demo (homals-plot-demo-create-graph x)))
       (fit-0 0)
       (fit-1 0)
       (itel 0)
       h)
  
  (if speed 
      (dotimes (j m)
               (let ((g (make-indicator (elt (column-list e) j)
                                                 (elt active-categories j))))
                 (setf h (append h (list g)))
                 )
               )
      )

  (loop
   (let ((z (make-array (list n ndim) :initial-element 0))
         (new-fit-0 0)
         (new-fit-1 0))
     (dotimes (j m)
       (let* ((g (if speed (elt h j)
                     (make-indicator (elt (column-list e) j)
                                     (elt active-categories j))))
              (y (make-category-quantifications
                  x g active-objects)))
         (setf z (+ z (matmult g y)))))
     (setf z (center z w))
     (setf new-fit-0 (/ (sum (diagonal (matmult (transpose z)
                                                (matmult (diagonal w) x))))
                        (* m ndim)))
     (if gram
         (setf z (gram-schmidt z w))
         (setf z (procrustus z w)))
     (setf new-fit-1 (sqrt (/ (sum (^ (matmult (transpose z)
                                               (matmult (diagonal w) x)) 2))
                              ndim)))
     (if demo (progn
                (homals-plot-demo-upgrade-objects q x z)
                (homals-plot-demo-upgrade-graph r e z)))
     (if (or (and (> eps-0 (abs (- new-fit-0 fit-0)))
                  (> eps-1 (abs (- new-fit-1 fit-1))))
             (> itel itmax))
         (progn
           (if output-file
               (let ((c (make-array (list ndim ndim) :initial-element 0)))
                 (dotimes (j m)
                   (let* ((g (if speed (elt h j)
                                 (make-indicator (elt (column-list e) j)
                                                 (elt active-categories j))))
                          (y (make-category-quantifications
                              z g active-objects))
                          (d (make-discrimination-measures
                              y g active-objects)))
                     (setf c (+ c d))))
                 (let* ((e (elt (eigen c) 1))
                        (k (apply #'bind-columns e)))
                   (setf z (matmult z k)))
                 (homals-numerical-output
                        output-file data z
                        active-objects active-variables active-categories
                        object-labels variable-labels category-labels)))
           (if more-graphics
               (if (find :MACINTOSH *features*)
                   (homals-mac-graphical-output data z)
                 (homals-x11-graphical-output data z)))
           (return))
       (progn 
         (format t "Iteration ~4d Fit measure ~,10f Change measure ~,10f~%"
                 itel new-fit-0 new-fit-1)
         (setf fit-0 new-fit-0)
         (setf fit-1 new-fit-1)
         (setf itel (1+ itel))
         (setf x z)))))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Graphics needed for the two-dimensional demo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun homals-plot-demo-create-objects (x)
  (let ((q (plot-points
            (elt (column-list x) 0) (elt (column-list x) 1)
            :title "Object Score History" :show nil)))
    (if (find :MACINTOSH *features*)
           (send q :location 20 20)
           (send q :location 50 50))
    (send q :adjust-to-data)
    (send q :show-window)
    q
    ))

(defun homals-plot-demo-create-graph (x)
  (let ((r (plot-points
            (elt (column-list x) 0) (elt (column-list x) 1)
            :title "Graph Plot History" :show nil)))
    (if (find :MACINTOSH *features*)
           (send r :location 300 20)
           (send r :location 470 50))
    (send r :adjust-to-data)
    (send r :show-window)
    r
    ))

(defun homals-plot-demo-upgrade-objects (q x z)
  (let ((n (first (array-dimensions z))))
    (send q :add-points
          (elt (column-list z) 0) (elt (column-list z) 1))
    (dotimes (i n)
      (send q :add-lines
            (list (aref z i 0) (aref x i 0))
            (list (aref z i 1) (aref x i 1))))
    (send q :adjust-to-data)
  ))

(defun homals-plot-demo-upgrade-graph (r data z)
  (send r :clear)
  (send r :add-points
        (elt (column-list z) 0) (elt (column-list z) 1))
  (let ((n (first (array-dimensions data)))
        (m (second (array-dimensions data))))
    (dotimes (j m)
      (let* ((g (make-indicator (elt (column-list data) j)))
             (y (make-category-quantifications z g))
             (a (matmult g y)))
        (dotimes (i n)
          (send r :add-lines
                (list (aref z i 0) (aref a i 0))
                (list (aref z i 1) (aref a i 1)))))))
  (send r :adjust-to-data)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Output routines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun homals-numerical-output (output-file data z ao av ac ol vl cl)
  (let ((p (second (array-dimensions z)))
        (n (first (array-dimensions data)))
        (m (second (array-dimensions data))))
    (setf z (* z (sqrt n)))
    (with-open-file
     (output output-file :direction :output)

     (format output "~a~%" (make-string (+ 27 (* 13 p)) :initial-element #\=))
     (format output "Object Scores~%")
     (format output "~a~%" (make-string (+ 27 (* 13 p)) :initial-element #\=))
     (dotimes (i n)
       (if (find i ao)
         (format output "~20a *** A " (elt ol i))
         (format output "~20a *** P " (elt ol i)))
       (dotimes (s p)
         (format output "~12,8f " (aref z i s)))
       (format output "~%"))
     (format output "~a~%~a~%" (make-string (+ 27 (* 13 p)) :initial-element #\=) #\C-L)

     (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\=))
     (format output "Category Quantifications~%")
     (format output "~a~%~%" (make-string (+ 36 (* 13 p)) :initial-element #\=))
     (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\-))
     (dotimes (j m)
       (if (find j av)
           (format output "Variable ~20a      (ACTIVE)~%" (elt vl j))
           (format output "Variable ~20a     (PASSIVE)~%" (elt vl j)))
       (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\-))
       (let* ((r (elt (column-list data) j))
              (u (sort-data (remove-duplicates r :test 'equal)))
              (g (make-indicator r u))
              (y (make-category-quantifications z g))
              (d (column-sums g))
              (k (length d)))
         (dotimes (l k)
           (if (find (elt u l) (elt ac j) :test 'equal)
               (format output "~20a *** ~4d *** A "
                       (elt (elt cl j) l) (elt d l))
               (format output "~20a *** ~4d *** P "
                       (elt (elt cl j) l) (elt d l)))
           (dotimes (s p)
             (format output "~12,8f " (aref y l s)))
           (format output "~%")))
       (format output "~a~%" (make-string (+ 36 (* 13 p)) :initial-element #\-)))
     (format output "~a~%" #\C-L)

     (format output "~a~%" (make-string (max 43 (* 13 p)) :initial-element #\=))
     (format output "Discrimination Measures~%")
     (format output "~a~%~%" (make-string (max 43 (* 13 p)) :initial-element #\=))
     (let ((e (make-array (list p p) :initial-element 0)))
       (dotimes (j m)
         (format output "~a~%" (make-string (max 43 (* 13 p)) :initial-element #\-))
       (if (find j av)
           (format output "Variable ~20a      (ACTIVE)~%" (elt vl j))
           (format output "Variable ~20a     (PASSIVE)~%" (elt vl j)))
       (format output "~a~%" (make-string (max 43 (* 13 p)) :initial-element #\-))
         (let* ((g (make-indicator (elt (column-list data) j)))
                (y (make-category-quantifications z g))
                (d (make-discrimination-measures y g)))
           (setf e (+ e d))
           (dotimes (s p)
                    (dotimes (u p)
                             (format output "~12,8f " (aref d s u)))
                    (format output "~%"))))
       (setf e (/ e m))
       (format output "~a~%" (make-string (max 43 (* 13 p)) :initial-element #\-))
       (format output "Average Discrimination Measure~%")   
       (format output "~a~%" (make-string (max 43 (* 13 p)) :initial-element #\-))
       (dotimes (s p)
                (dotimes (u p)
                       (format output "~12,8f " (aref e s u)))
              (format output "~%"))
       (format output "~a~%" (make-string (max 43 (* 13 p)) :initial-element #\-)))
)))

(defun homals-mac-graphical-output (data z)
  (let* ((output-menu (send menu-proto :new "Graphics"))
         (scor-item (send menu-item-proto :new "Score-plot"
                          :action #'(lambda ()
                                      (make-score-plot z))))
         (joint-item (send menu-item-proto :new "Joint-Category-plot"
                          :action #'(lambda ()
                                      (make-joint-plot data z))))
         (graph-item (send menu-item-proto :new "Graph-plot"
                           :action #'(lambda ()
                                       (make-graph-plot data z))))
         (star-item (send menu-item-proto :new "Star-plot"
                          :action #'(lambda ()
                                      (make-star-plot data z))))
         (catg-item (send menu-item-proto :new "Category-plot"
                          :action #'(lambda ()
                                      (make-category-plot data z))))
         (dash-item (send dash-item-proto :new))
         (clos-item (send menu-item-proto :new "Close Menu"
                          :action #'(lambda()
                                      (send output-menu :remove)))))
  (send output-menu :append-items scor-item joint-item graph-item dash-item
        star-item catg-item dash-item clos-item)
  (send output-menu :install)
  ))

(defun homals-x11-graphical-output (data z)
  (let* ((plot-item (send choice-item-proto :new 
                          (list "Score-Plot"
                                "Joint-Category-Plot"
                                "Graph-Plot"
                                "Star-Plot"
                                "Category-Plot") :value 0))
         (ok (send modal-button-proto :new "Enough"))
         (plot (send button-item-proto :new "Plot"
                     :action #'(lambda ()
                                 (case (send plot-item :value)
                                   (0 (make-score-plot z))
                                   (1 (make-joint-plot data z))
                                   (2 (make-graph-plot data z))
                                   (3 (make-star-plot data z))
                                   (4 (make-category-plot data z))))))
         (pdiag (send modal-dialog-proto :new (list 
                                               (list plot-item)
                                               (list plot ok))
                      :default-button ok)))
(send pdiag :modal-dialog)
))

(defun dimension-dialog (p)
  (let* ((prompt-item (send text-item-proto :new "Pick Dimensions"))
         (dim-1-item
          (send text-item-proto :new "First Dimension"))
         (dim-2-item
          (send text-item-proto :new "Second Dimension"))
         (dim-3-item
          (send text-item-proto :new "Third Dimension"))
         (pik-1-item
          (send text-item-proto :new "0" :editable t))
         (pik-2-item
          (send text-item-proto :new "1" :editable t))
         (pik-3-item
          (send text-item-proto :new "2" :editable t))
         (ok (send modal-button-proto :new "OK"
                   :action #'(lambda ()
                               (append
                                (list
                                 (- (char-code
                                     (elt (send pik-1-item :text) 0)) 48)
                                 (- (char-code
                                     (elt (send pik-2-item :text) 0)) 48))
                                 (if (> p 2)
                                     (list (- (char-code
                                         (elt (send pik-3-item :text) 0)) 48)))
                                ))))
         (cancel (send modal-button-proto :new "Cancel"))
         (vdiag (send modal-dialog-proto
                      :new (list
                            (list prompt-item)
                            (list
                             (append (list dim-1-item dim-2-item)
                                     (if (> p 2)
                                         (list dim-3-item)))
                             (append (list pik-1-item pik-2-item)
                                     (if (> p 2)
                                         (list pik-3-item))))
                            (list ok cancel))
                      :default-button ok)))
    (send vdiag :modal-dialog)
    ))
                                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Make specific plots, after convergence
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-score-plot (z)
  (let* ((p (second (array-dimensions z)))
         (q (dimension-dialog p)))
    (if (= p 2) 
        (plot-points
         (select (column-list z) q) :title "Object Score Plot")
      (spin-plot 
         (select (column-list z) q) :title "Object Score Plot"))
  ))

(defun make-category-plot (data z)
  (let* ((p (second (array-dimensions z)))
         (q (dimension-dialog p))
         (ij (elt (get-value-dialog "Variable:" :initial 0) 0))
         (gg (make-indicator (elt (column-list data) ij)))
         (yy (make-category-quantifications z gg)))
  (if (= p 2)
      (plot-points
       (select (column-list yy) q)
       :title (concatenate 'string "Category Plot Variable "
                           (num-to-string ij)))
      (spin-plot
       (select (column-list yy) q)
       :title (concatenate 'string "Category Plot Variable "
                           (num-to-string ij))))
  ))
    

(defun make-star-plot (data z)
  (let* ((p (second (array-dimensions z)))
         (q (dimension-dialog p))
         (ij (elt (get-value-dialog "Variable:" :initial 0) 0))
         (gg (make-indicator (elt (column-list data) ij)))
         (nn (first (array-dimensions gg)))
         (yy (make-category-quantifications z gg))
         (aa (matmult gg yy))
         (pq (if (= p 2)
                 (plot-points (select (column-list z) q))
                 (spin-plot (select (column-list z) q)))))
    (send pq :title (concatenate 'string "Star Plot Variable "
                                 (num-to-string ij)))
    (dotimes (i nn)
      (send pq :add-lines
            (mapcar #'(lambda (j) (list (aref z i j) (aref aa i j))) q)))
     ))

(defun make-joint-plot (data z)
  (let* ((n (first (array-dimensions data)))
         (m (second (array-dimensions data)))
         (p (second (array-dimensions z)))
         (q (dimension-dialog p))
         (pq (if (= p 2)
                 (plot-points (select (column-list z) q))
                 (spin-plot (select (column-list z) q)))))
    (send pq :title "Joint Category Plot")
     (dotimes (j m)
      (let* ((gg (make-indicator (elt (column-list data) j)))
             (yy (make-category-quantifications z gg)))
        (send pq :add-points (row-list yy))))
     ))


(defun make-graph-plot (data z)
  (let* ((n (first (array-dimensions data)))
         (m (second (array-dimensions data)))
         (p (second (array-dimensions z)))
         (q (dimension-dialog p))
         (pq (if (= p 2)
                 (plot-points (select (column-list z) q))
                 (spin-plot (select (column-list z) q)))))
    (send pq :title "Graph Plot")
    (dotimes (j m)
      (let* ((gg (make-indicator (elt (column-list data) j)))
             (yy (make-category-quantifications z gg))
             (aa (matmult gg yy)))
        (dotimes (i n)
          (send pq :add-lines
                (mapcar #'(lambda (j) (list (aref z i j) (aref aa i j))) q)))))
     ))

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

(defun make-category-quantifications
  (x g &optional (s (iseq (first (array-dimensions g)))))
  (let* ((h (select g s (iseq (second (array-dimensions g)))))
         (z (select x s (iseq (second (array-dimensions x)))))
         (d (geninv (column-sums h))))           
    (matmult (diagonal d) (matmult (transpose h) z))
))

(defun make-discrimination-measures
  (y g &optional (s (iseq (first (array-dimensions g)))))
  (let* ((h (select g s (iseq (second (array-dimensions g)))))
         (d (geninv (column-sums h))))           
    (matmult (transpose y) (matmult (diagonal d) y))
  ))

(defun make-weights (e s u)
  (let* ((n (first (array-dimensions e)))
         (m (second (array-dimensions e)))
         (f (column-list e))
         (w (make-list n :initial-element 0)))
    (dotimes (j m)
      (incf (select w u)
            (select (row-sums (make-indicator (elt f j) (elt s j))) u)))    
    (/ w m)
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

(defun center (x w)
  (let (
        (n (first (array-dimensions x)))
        (mu (/ (matmult w x) (sum w)))
        )
    (- x (outer-product (repeat 0 n) mu #'+))
    ))

(defun procrustus (x w)
  "Args: X
The closest orthonormal
matrix to X in the metric W to X is computed."
  (let* ((u (diagonal (sqrt w)))
         (v (diagonal (geninv (sqrt w))))
         (y (sv-decomp (matmult u x))))
    (matmult v (matmult (first y) (transpose (third y))))
  ))

(defun gram-schmidt (x w)
  "Args: X
X is decomposed as KS, with K w-orthonormal and S upper-triangular,
returns K."
  (let* ((u (diagonal w))
         (v (diagonal (geninv (sqrt w))))
         (y (chol-decomp (matmult (matmult (transpose x) u) x))))
    (matmult v (matmult x (inverse (transpose (first y)))))
    ))

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
  

(defun make-random-orthonormal (w n m)
  "Args: n m
Makes a matrix of order N x M with standard normals,
then centers and w-orthogonalizes it."
  (let ((z (make-array (list n m) :displaced-to
                       (coerce (normal-rand (* n m)) 'vector))))
    (gram-schmidt (center z w) w)
    ))

(defun to-string-list (l)
  "Args: l
Converts L to a list of strings."
  (mapcar #'(lambda (x) (format nil "~a" x)) l)
  )

(defun geninv (x &optional (ozo 1))
  (if (compound-data-p x)
      (map-elements #'geninv x)
    (if (= 0 x) ozo (/ x))))









