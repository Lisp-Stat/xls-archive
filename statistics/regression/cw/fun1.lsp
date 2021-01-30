;
;  various miscellaneous functions 
;

(defun sweep (a margin f &rest args)
"Args: a margin  f (&rest args))
Equivalent to the sweep function in S.  Takes 2 two-dimensional
array A, and produces an array of the same size and shape with f
applied to each column of A (margin = 'cols) or each row of A
(margin = 'rows)."
(cond
  ((eq margin 'cols)
   (apply #'bind-columns (mapcar f (column-list a))))
  ((eq margin 'rows)
   (apply #'bind-rows (mapcar f (row-list a))))))

(defun rmel (elmts x)
"Args: (elmts x)
Returns a sublist of x with elements indexed in elmts deleted. elmts
may be a single number if only one element of x is to be removed,
otherwise elmts must be a list."
  (let* ((z x) 
        rs)
    (cond ((numberp elmts) 
          (setf z (select x (remove elmts (iseq 0 (- (length x) 1))))))
      (t (setf rs (reverse (sort-data elmts)))
       (dotimes (i (length elmts))
         (setf z (select z (remove (nth i rs) (iseq 0 (- (length z) 1))))))))
  z))

(defun substr (string start &optional (length (length string)))
"Args: (string start &optional (length (length string)))
Returns part of a string, starting at the start-th place, and extending
to length places."
  (subseq string start (min (+ start length) (length string))))


;;;
;;; toggle-item-proto - creates toggle menu items that show title1 and 
;;;    give action1 if rule is t and title2 and action2 if rule is nil.
;;;

(defproto mytoggle-item-proto '(rule title1 action1 title2 action2) 
  () menu-item-proto)

(defmeth mytoggle-item-proto :isnew (rule title1 action1 title2 action2) 
  (setf (slot-value 'rule) rule)
  (setf (slot-value 'title1) title1)
  (setf (slot-value 'title2) title2)
  (setf (slot-value 'action1) action1)
  (setf (slot-value 'action2) action2)
  (call-next-method title1))

(defmeth mytoggle-item-proto :update () 
  (send self :title (if (funcall (slot-value 'rule))
                        (slot-value 'title1) (slot-value 'title2)))
  (call-next-method))

(defmeth mytoggle-item-proto :do-action () 
  (if (funcall (slot-value 'rule))
      (funcall (slot-value 'action1))
      (funcall (slot-value 'action2))))


(defun select-list-dialog (items
                           &key (title "Choose items")
                           (left-title "Variables")
                           (right-title "Subset"))
"Args: (items &key title left-title right-title)
Creates a dialog box with a left and right scrolling list.  All items start
in the left list.  Double clicking on an item moves it to the other list.
Returns a list of two lists.  Each list is an index of the items in the
scrolling list, in the order they appear on the screen."
  (let* ((left-title  (send text-item-proto :new left-title))
         (right-title (send text-item-proto :new right-title))
         (left-items (iseq (length items)))
         (right-items nil)
         (left-list  (send list-item-proto :new items))
         (right-list (send list-item-proto :new (repeat "" (length items))))
         (cancel (send modal-button-proto :new "Cancel"))
         (ok (send modal-button-proto :new "OK" :action 
                   #'(lambda () (list left-items right-items) )))
         (dialog (send modal-dialog-proto :new
                       (list 
                        (list title)
                        (list 
                         (list left-title left-list)
                         (list right-title right-list))
                        (list ok cancel)))))
    (defmeth left-list :do-action (&optional double)
      (when (and double (<= (send self :selection) (- (length left-items) 1)))
       (let* ((sel (select left-items (send self :selection))))
         (setf left-items (remove sel left-items))
         (setf right-items (append right-items (list sel))) 
         (when left-items
               (mapcar #'(lambda (item j) (send self :set-text j item))
                       (select items left-items) (iseq (length left-items))))
         (when (< (length left-items) (length items))
               (mapcar #'(lambda (j) (send self :set-text j ""))
                       (iseq (length left-items) (1- (length items)))))
         (when right-items
               (mapcar #'(lambda (item j) (send right-list :set-text j item))
                      (select items right-items) (iseq (length right-items))))
         )))
    (defmeth right-list :do-action (&optional double)
      (when (and double (<= (send self :selection) 
                            (- (length right-items) 1)))
       (let* ((sel (select right-items (send self :selection))))
         (setf right-items (remove sel right-items))
         (setf left-items (append left-items (list sel))) 
         (when right-items
               (mapcar #'(lambda (item j) (send self :set-text j item))
                       (select items right-items) 
                       (iseq (length right-items))))
         (when (< (length right-items) (length items))
               (mapcar #'(lambda (j) (send self :set-text j ""))
                       (iseq (length right-items) (1- (length items)))))
         (when left-items
               (mapcar #'(lambda (item j) (send left-list :set-text j item))
                      (select items left-items) (iseq (length left-items))))
         )))
    (send dialog :modal-dialog)))

(defun backsolve (x y)
"Args:  x  y
Solves the triangular system xb=y for b, where x is an upper triangular 
matrix."
  (let* ((n (length y))
         (d (diagonal x))
         (x (row-list (- (sweep x 'cols #'(lambda (a) (/ a d))) 
                         (identity-matrix n))))
         (b (/ y d)))
   (dotimes (j (1- n))
            (let* ((nj2 (- n j 2)))
              (setf (select b nj2) (- (select b nj2) 
                   (inner-product (select x nj2) b)))))
    b))


(defun range (x) (list (min x) (max x)))

(defun mindrange (x) (* .5 (apply '+ (range x))))

(defun cosangle (a B)
"Args:  (a B)
Returns the cosine of the angle between a vector a and a subspace
spanned by B."
  (let* ((p (project b :intercept nil)))
    (cosangle1 a (send p :project a))))

(defun cosangle1 (a b)
"Args:  (a b)
Returns the cosine of the angle between two lists or vectors a and b"
  (/ (inner-product a b) (sqrt (* (inner-product a a) (inner-product b b)))))

(defun angle (a b)
"Returns the angle between vector a and subspace b in degrees"
  (* 180 (acos (cosangle a b)) (/ pi)))

(defun hnorm-quant (p)
"Args: p
Returns the p-th quantile of the standard half normal distribution."
  (normal-quant (+ .5 (* .5 p))))

(defmeth scatterplot-proto :add-boxplot (y &key (x 1.0) (width 1.0) (draw t)
                                           (range 1.5))
"Modified version of the standard boxplot to put in outliers, points beyond 
range*iq-range above or below the median, explicitly."
  (unless (= 2 (send self :num-variables)) (error "only works for 2D plots"))
  (let* ((half-box (* 0.4 width))
         (half-foot (* 0.1 width))
         (fiv (fivnum+ y :range range))
         (low (select fiv 0))
         (q1 (select fiv 1))
         (med (select fiv 2))
         (q3 (select fiv 3))
         (high (select fiv 4)))
    (send self :plotline (- x half-foot) low  (+ x half-foot) low  nil)
    (send self :plotline (- x half-foot) high (+ x half-foot) high nil)
    (send self :plotline x low x q1   nil)
    (send self :plotline x q3  x high nil)
    (send self :plotline (- x half-box) q1  (+ x half-box) q1  nil)
    (send self :plotline (- x half-box) med (+ x half-box) med nil)
    (send self :plotline (- x half-box) q3  (+ x half-box) q3  nil)
    (send self :plotline (- x half-box) q1  (- x half-box) q3  nil)
    (send self :plotline (+ x half-box) q1  (+ x half-box) q3  nil)
    ;; *** Here are two versions that allow linking to work properly.
    ;; ***
    ;; *** this version uses an undocumented feature called masking.
    ;; *** It is like removing except it is local to a plot, not linked.
    ;; *** I never could decide if it was worth using, so I did not
    ;; *** document it. It MAY not work properly.
    (let ((np (send self :num-points)))
      (send self :add-points (repeat x (length y)) y :draw nil)
      (send self :point-masked (+ np (iseq (length y))) (< low y high)))
    ;; *** Here is an alternate version that just adds all the points
;    (send self :add-points (repeat x (length y)) y :draw draw)
    (if draw (send self :redraw-content))))

(defun fivnum+ (x &key (range 1.5))
"Returns a list of six items as follows: low-fence q1 med q3 high-fence
outliers, where outliers is a list of values outside the fences.  Range
determines the fences; the usual value is 1.5*iq-range."
  (flet ((quant (x p)
                (let* ((n (length x))
                       (np (* p (- n 1)))
                       (low (floor np))
                       (high (ceiling np)))
                      (/ (+ (select x low) (select x high)) 2))))
    (let* ((x (sort-data x))
           (n (- (length x) 1))
           (q1 (quant x .25))
           (q3 (quant x .75))
           (q2 (quant x .50))
           (r (* range (- q3 q1)))
           (low-ind (cond ((= range 0) 0)
                            ((<= (- q1 r) (select x 0)) 0)
                            (t (do ((j 0 (1+ j)))
                                   ((and (< (select x j) (- q1 r))
                                        (>= (select x (1+ j)) (- q1 r)))
                                    (+ j 1))))))
           (high-ind (cond ((= range 0) n)
                           ((>= (+ q3 r) (select x n)) n)
                           (t (do ((j n (- j 1)))
                                  ((and (> (select x j) (+ q3 r))
                                        (<= (select x (- j 1)) (+ q3 r)))
                                   (- j 1)))))))
          (list (select x low-ind) q1 q2 q3 (select x high-ind)
                (cond ((and (= low-ind 0) (= high-ind n)) nil)
                       ((= low-ind 0) (select x (iseq (1+ high-ind) n)))
                       ((= high-ind n) (select x (iseq  low-ind)))
                      (t (append (select x (iseq (1+ high-ind) n))
                                 (select x (iseq low-ind)))))))))


(defun boxplot (data &key (title "Box Plot") (range 1.5))
"Args: (data &key (title \"Box Plot\"))
DATA is a sequence, a list of sequences or a matrix. Makes a boxplot of the
sequence or a parallel box plot of the sequences in the list or the columns
of the matrix.  The range determines the location of the outer fences, and is
the same as in S." 
  (let ((p (send scatterplot-proto :new 2 :title title :show nil)))
    (setq data 
          (cond ((matrixp data) (column-list data))
                ((or (not (listp data)) (numberp (car data))) (list data))
                (t data)))
        (let ((range (get-nice-range (min data) (max data) 4)))
          (send p :range 1 (nth 0 range) (nth 1 range))
          (send p :y-axis t nil (nth 2 range)))
    (send p :range 0 0 (1+ (length data)))
    (dotimes (i (length data))
          (send p :add-boxplot (nth i data) :x (1+ i)))
    (send p :show-window)
    p))


;;; geometric mean

(defun geometric-mean (x) 
"Args:  (x)
Returns the geometric mean if the elements of x are strictly positive, and
returns nil otherwise."
  (if (> (min x) 0) (exp (mean (log x))) nil))

;;;
;;; contour plotting function with an added mouse mode
;;;

(defun contour-plot (f &rest args)
"Message args:  f args
This function calls the standard contour-function for the function f with
arguements args.  Mouse clicks on the plot
result in printing the value of the contour closest to the click.  A shift
click prints a symbol at the point clicked and the value of the function at
that point."
   (let ((p (apply #'contour-function f args)))
        (send p :add-mouse-mode 'show-contour
              :title "Show contour"
              :click :do-show-screen-coordinate
              :cursor 'finger)
        (send p :mouse-mode 'show-contour)
        (send p :add-slot 'contour-function f)
        p))

(defmeth graph-proto :do-show-screen-coordinate (x y m1 m2)
"Message args:  (x y m1 m2)
Show contour mouse mode.  m1 is t for shift click."
     (let* ((f (slot-value 'contour-function))
            (val (format nil "x ~s"
                              (apply f (send self :canvas-to-scaled x y)))))
          (cond (m1 (let ((mode (send self :draw-mode)))
                 (send self :draw-mode 'xor)
                 (send self :draw-string val x y)
                 (send self :while-button-down #'(lambda (x y) nil))
                 (send self :draw-string val x y)
                 (send self :draw-mode mode)))
                (t
                 (send self :draw-string val x y)
                 (send self :save-screen-coordinates 
                       (list val (send self :canvas-to-scaled x y)))))))

(defmeth graph-proto :save-screen-coordinates (&optional new)
"Message args:  (new)
New is a list of a string and x, y coordinates.  Puts them in a list to be
redrawn when the :redraw message is sent."
  (when (null (send self :has-slot 'locations)) 
        (send self :add-slot 'locations nil)
        (defmeth self :redraw ()
                 (call-next-method)
                 (mapcar #'(lambda (a)
                             (apply #'send self :draw-string 
                                    (first a) (apply #'send self 
                                         :scaled-to-canvas (second a))))
                         (slot-value 'locations))))
  (when new (setf (slot-value 'locations) (cons new (slot-value 'locations))))
  (slot-value 'locations))


;;;
;;; project prototype
;;;

(defproto project-proto 
  '(x q r basis included weights intercept tol)
  ()
  *object*
  "Orthogonal Projections")

(defun project (x  &key  
                   (intercept t) (weights nil wset) (pweights nil pset)
                   (included t iset)
                   (tol 10e-7))

"Args:  (x  &key  
            (intercept t) weights pweights
            (included t iset)
            (tol 10e-7))
Computes the projection on the column space of x (with a column of
1s appended if constant is t, usings weights for a weight vector, and
excluding rows according to the included vector.  The method of computing 
is the qr factorization by default."
  (let* ((x (cond
              ((matrixp x) x)
              ((vectorp x) (bind-columns (coerce x 'list)))
              ((and (consp x) (consp (first x))) (apply #'bind-columns x))
              (t (bind-columns x)))) 
         (n (array-dimension x 0))
         (weights (cond
                   ((and pset (= (length pweights) n)) pweights)
                   ((and wset (= (length  weights) n)) weights)
                   (t (repeat 1 n))))
         (included (if iset included (repeat t n)))
         (h (send project-proto :new x intercept weights included tol)))
    h))

(defmeth project-proto :isnew (x  intercept weights included tol)
  (setf (slot-value 'intercept) intercept)
  (setf (slot-value 'weights) weights)
  (setf (slot-value 'included) included)
  (setf (slot-value 'tol) tol)
  (send self :x x))

(defmeth project-proto :compute ()
  (let* ((included (send self :included))
         (inc (which included))
         (not-inc (which (mapcar 'not included)))
         (sqrt-wt (sqrt (send self :weights)))
         (x (sweep (send self :x) 'cols #'(lambda (a) (* sqrt-wt a))))
         (intercept (send self :intercept))
         (size (array-dimensions x))
         (basis (if intercept (combine 0 (1+ (iseq (select size 1))))
                    (iseq (select size 1))))
         qr) 
    (when intercept (setf x (bind-columns sqrt-wt x)))
    (setf qr (send self :get-projection x inc basis)) 
    (setf (select x inc (first qr)) (second qr))
    (when not-inc
            (setf (select x not-inc (first qr)) 
                  (matmult (select x not-inc (first qr)) 
                           (inverse (third qr)))))
    (setf (slot-value 'q) (select x (iseq 0 (1- (nth 0 size))) (first qr)))
    (setf (slot-value 'r) (third qr))
    (setf (slot-value 'basis) 
          (if intercept (rest (- (first qr) 1)) (first qr)))
    (slot-value 'basis)))

(defmeth project-proto :get-projection (x inc basis)
"This method checks for zero pivots, and, if any are found, deletes them and
recursively calls itself."
  (let* ((qr (qr-decomp (select x inc basis)))
         (tol (send self :tol))
         (bad-pivot (position tol (abs (diagonal (second qr))) :test #'>)))
    (cond ((null bad-pivot) (cons basis qr))
          (t (send self :get-projection x inc 
                        (select basis (remove bad-pivot 
                                              (iseq (length basis))))))))) 

(defmeth project-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'q) nil))
  (null (slot-value 'q)))

(defmeth project-proto :q (&optional firstp)
"Computes the matrix consisting of the firstp columns of Q."
  (when (send self :needs-computing) (send self :compute))
  (let* ((q (slot-value 'q))
         (firstp (if firstp 
                  (+ (length (which (< (send self :basis) firstp)))
                     (if (send self :intercept) 1 0)) firstp)))
    (if firstp
        (select q (iseq 0 (1- (array-dimension q 0))) (iseq 0 (1- firstp)))
        q)))

(defmeth project-proto :r (&optional firstp)
  "Returns upper-triangular matrix for firstp columns."
  (when (send self :needs-computing) (send self :compute))
  (let* ((r (slot-value 'r))
         (firstp (if firstp 
                     (+ (length (which (< (send self :basis) firstp)))
                        (if (send self :intercept) 1 0)) firstp)))
      (if firstp (select r (iseq 0 (1- firstp)) (iseq 0 (1- firstp)))
      r)))

(defmeth project-proto :project (y &optional firstp)
"Computes QQ'y."
  (let* ((w (sqrt (send self :weights)))
         (q (send self :q firstp))
         (y (* y (mapcar #'(lambda(i) (if i 1 0)) (send self :included)))))
    (/ (matmult q (matmult (transpose q) (* w y))) w)))

(defmeth project-proto :rank (&optional firstp)
"Message args: ($optional firstp)
Returns the dimension of the space spanned by the firstp columns of X and by the constant column if intercept is t, or the dimension of R(1,X) by 
default."
   (let ((basis (send self :basis))
         (int (if (send self :intercept) 1 0)))
        (if firstp (+ int (length (which (if-else (< basis firstp) t nil))))
                   (+ int (length basis)))))

(defmeth project-proto :p2.1 (y firstp &optional secondp)
"Computes (Q2Q2' - Q1Q1')y."
  (- (send self :project y secondp) (send self :project y firstp)))

(defmeth project-proto :residuals (y &optional (firstp nil))
"Computes (I - QQ')y"
  (- y (send self :project y firstp)))

(defmeth project-proto :num-inc ()
  (length (which (send self :included))))

(defmeth project-proto :residual-sum-of-squares (y &optional (firstp nil))
  (sum (^ (select (send self :residuals y firstp) 
                  (which (send self :included))) 2)))

(defmeth project-proto :sigma-hat (y &optional (firstp nil))
  (sqrt (/ (send self :residual-sum-of-squares y firstp)
           (- (send self :num-inc) (send self :rank firstp)))))

(defmeth project-proto :studentized-residuals (y &optional (firstp nil))
  (let* ((res (send self :residuals y firstp))
         (lev (send self :leverages firstp))
         (sig (send self :sigma-hat y firstp))
         (inc (send self :included)))
        (if-else inc
                 (/ res (* sig (sqrt (pmax .00001 (- 1 lev)))))
                 (/ res (* sig (sqrt (+ 1 lev)))))))

(defmeth project-proto :qty (y &optional (firstp nil))
"Computes Q'y, properly weighted."
  (let* ((w (sqrt (send self :weights)))
         (q (send self :q firstp)))
    (matmult (transpose q) (* w (* y (if-else (send self :included) 1 0))))))

(defmeth project-proto :coef-estimates (y &optional firstp)
"Computes coefficient estimates by backsolving."
   (backsolve (send self :r firstp) (send self :qty y firstp)))

(defmeth project-proto :projection-matrix (&key firstp case-list)
  (let* ((q (send self :q firstp)))
    (when case-list (setf q (select q case-list 
                                    (iseq 0 (1- (array-dimension q 1))))))
    (matmult q (transpose q))))

(defmeth project-proto :leverages (&optional firstp)
  (let* ((q (send self :q firstp)))
    (mapcar #'(lambda (a) (sum (^ a 2))) (coerce q 'list))))

(defmeth project-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix as supplied to m. With an argument
NEW-X sets the x matrix to NEW-X and recomputes the estimates."
  (when new-x
        (setf (slot-value 'x)
               (cond
                 ((matrixp new-x) new-x)
                 ((vectorp new-x) (apply #'bind-columns (coerce new-x 'list)))
                 ((consp new-x) (apply #'bind-columns new-x))))
        (send self :needs-computing t))
   (slot-value 'x))

(defmeth project-proto :intercept (&rest args)
"Message args: (&optional new-intercept)
With no argument returns T if the model includes an intercept term, nil if
not. With an argument NEW-INTERCEPT the model is changed to include or exclude
an intercept, according to the value of NEW-INTERCEPT, and estimates are
recomputed."
  (when args 
        (setf (slot-value 'intercept) (car args))
        (send self :needs-computing t))
  (slot-value 'intercept))

(defmeth project-proto :tol (&optional tol)
"Message args: (&optional tol)
With no argument returns the tolerance."
  (when tol (setf (slot-value 'tol) tol))
  (slot-value 'tol))

(defmeth project-proto :weights (&optional new-w)
"Message args: (&optional new-w)
With no argument returns the weight sequence as supplied to m; NIL means
ans unweighted model. NEW-W sets the weights sequence to NEW-W and
recomputes the estimates."
  (when new-w (setf (slot-value 'weights) new-w) (send self :needs-computing t))
  (slot-value 'weights))

(defmeth project-proto :pweights (&optional (new nil set))
  (if set (send self :weights new) (send self :weights)))

(defmeth project-proto :basis ()
"Message args: ()
Returns the indices of the linearly independeent columns of X."
  (slot-value 'basis))

(defmeth project-proto :included (&optional new-included)
"Message args: (&optional new-included)
With no argument,  NIL means a case is not used in calculating
estimates, and non-nil means it is used.  NEW-INCLUDED is a sequence
of length of y of nil and t to select cases.  Estimates are recomputed."
  (when (and new-included 
             (= (length new-included) (length (slot-value 'included)))) 
        (setf (slot-value 'included) new-included) 
        (send self :needs-computing t))
  (slot-value 'included))

(defmeth project-proto :toggle-cases (&optional (idnum nil))
"Message args: idnum, a list of case numbers
Deletes/restores specified cases.  If idnum is nil, all cases are restored."
(let* (included)
  (cond
    ((null idnum)
     (setf included (mapcar 'not (iseq 0 (1- (send self :num-cases))))))
    (t
     (setf included (send self :included))
     (dolist (i (coerce idnum 'list))
             (setf (nth i included) (not (nth i included))))))
  (send self :included included)))

(defmeth project-proto :num-cases ()
  (array-dimension (send self :q) 0))

