;;;; Course project:  Sta2102S, University of Toronto, Spring 1993
;;;; Instructor: N. Reid
;;;; Student: M. Ennis
;;;; Project Title:  Correspondence analysis

;;;; corresp.lsp
;;;; XLISP-STAT correspondence analysis proto
;;;; and methods, including plotting proto.
;;;; Marguerite Ennis, UoT, April 1993


;;;
;;;Some data for testing and illustrating the software
;;;

(setf frl (list "F1" "F2" "F3" "F4" "F5"))
(setf fcl (list "CAR" "BUS" "TRN" "MB" "BIC" "WLK"))
(setf factories '#2a((125 270 211 45 41 6) (120 143 94 62 40 8)
                (49 189 230 35.5 50.5 13) (104 201 190 48 61 8)
                (180 248 130 114 106 10)) )
(setf behaviour '#2a( (1 35 43 32 16) (7 108 130 76 33)
                      (4 170 200 94 31) (4 111 101 29 15) ))
(setf brl (list "0" "N" "P" "U"))
(setf bcl (list "1" "2" "3" "4" "5"))

(setf protein '#2a( 
(10.1  1.4 0.5  8.9  0.2 42.3 0.6 5.5 1.7)
( 8.9 14.0 4.3 19.9  2.1 28.0 3.6 1.3 4.3)
(13.5  9.3 4.1 17.5  4.5 26.6 5.7 2.1 4.0)
( 7.8  6.0 1.6  8.3  1.2 56.7 1.1 3.7 4.2)
( 9.7 11.4 2.8 12.5  2.0 34.3 5.0 1.1 4.0)
(10.6 10.8 3.7 25.0  9.9 21.9 4.8 0.7 2.4)
( 8.4 11.6 3.7 11.1  5.4 24.6 6.5 0.8 3.6)
( 9.5  4.9 2.7 33.7  5.8 26.3 5.1 1.0 1.4)
(18.0  9.9 3.3 19.5  5.7 28.1 4.8 2.4 6.5)
(10.2  3.0 2.8 17.6  5.9 41.7 2.2 7.8 6.5)
( 5.3 12.4 2.9  9.7  0.3 40.1 4.0 5.4 4.2)
(13.9 10.0 4.7 25.8  2.2 24.0 6.2 1.6 2.9)
( 9.0  5.1 2.9 13.7  3.4 36.8 2.1 4.3 6.7)
( 9.5 13.6 3.6 23.4  2.5 22.4 4.2 1.8 3.7)
( 9.4  4.7 2.7 23.3  9.7 23.0 4.6 1.6 2.7)
( 6.9 10.2 2.7 19.3  3.0 36.1 5.9 2.0 6.6)
( 6.2  3.7 1.1  4.9 14.2 27.0 5.9 4.7 7.9)
( 6.2  6.3 1.5 11.1  1.0 49.6 3.1 5.3 2.8)
( 7.1  3.4 3.1  8.6  7.0 29.2 5.7 5.9 7.2)
( 9.9  7.8 3.5 24.7  7.5 19.5 3.7 1.4 2.0)
(13.1 10.1 3.1 23.8  2.3 25.6 2.8 2.4 4.9)
(17.4  5.7 4.7 20.6  4.3 24.3 4.7 3.4 3.3)
( 9.3  4.6 2.1 16.6  3.0 43.6 6.4 3.4 2.9)
(11.4 12.5 4.1 18.8  3.4 18.6 5.2 1.5 3.8) 
( 4.4  5.0 1.2  9.5  0.6 55.9 3.0 5.7 3.2) ))

(setf prl (list "Al" "Au" "Be" "Bu" "Cz" "De" "EG" "Fi" "Fr"
"Gr" "Hu" "Ir" "It" "Ne" "No" "Po" "Pr" "Ru" "Sp"
"Se" "Sw" "UK" "Ru" "WG" "Yu" ))
(setf pcl (list "me" "pp" "eg" "mi" "fi" "ce" "st" "nu" "fv"))

;;;
;;; Contingency table Correspondence analysis proto
;;; -                 -

(defproto cc-proto
   '(x
     row-labels
     col-labels 
     row-n 
     col-n 
     needs-computing
     rank
     fg 
     basicv
     axes-labels ))

;;;
;;; function with which to run the model 
;;;

(defun cc (x &key
               row-labels
               col-labels
               (print T) )
"Args: x &key row-labels col-labels (print T) )
X          - matrix of counts
row-lables - list of strings
col-labels - list of strings
Returns a corresp analysis object, from which plots can be
obtained by the following 2 messages:
    :plot-2d (&optional (axes '(0 1)) )
    :plot-3d (&optional (axes '(0 1 2)) )  "

(let ( (x (cond 
            ((matrixp x) x)
            ((vectorp x) (list x))
            ((and (consp x) (numberp (car x)))
                                 (list x))
            (t x)))
       (m (send cc-proto :new)) )
   (send m :x x)
   (send m :row-n (array-dimension x 0))
   (send m :col-n (array-dimension x 1))
   (send m :row-labels row-labels)
   (send m :col-labels col-labels)
   (if print (send m :pr-inertia))
   m 
 )
)

(defmeth cc-proto :isnew () (send self 
                     :needs-computing T))

(defmeth cc-proto :needs-computing (&optional (tf nil set))
  (if set (setf (slot-value 'needs-computing) tf))
  (slot-value 'needs-computing))

;;;
;;; Computing and display methods
;;;

;; First, 2 functions needed in the COMPUTE method
(defun column-sums (x)
  (mapcar #'sum (column-list x)))
(defun row-sums (x)
  (mapcar #'sum (row-list x)))

;;The compute method finds the row coordinates in F, the
;;col coordinates in G and then binds them together in FG since
;;row and col coords are plotted on one graph.  

(defmeth cc-proto :compute ()
"message args: ()
Computes estimates. For internal use"
 (let* ( (x (send self :x))
         (P (/ x (sum x)))
         (r (row-sums P))
         (c (column-sums P))
         (rc (outer-product r c))
         (S (/ (- P rc) (sqrt rc)))
         (n (send self :row-n))
         (m (send self :col-n))
         (sv (if (>= n m) (sv-decomp S) 
                          (sv-decomp (transpose S))))
         (M1 (if (>= n m) (first sv) (third sv)))
         (N1 (if (>= n m) (third sv) (first sv)))
         (basicv (second sv))
         (eigen (^ basicv 2))
         (rank (length (which (> basicv 0.00001))))
         (F (matmult (diagonal (/ 1 (sqrt r)))
                     M1
                     (diagonal basicv)))
         (G (matmult (diagonal (/ 1 (sqrt c)))
                     N1
                     (diagonal basicv)))
         (FG (column-list (bind-rows F G)) )
         (FG (select FG (iseq rank) (iseq (+ n m))))
       )
   (setf (slot-value 'basicv) 
                      (select basicv (iseq rank)))
   (setf (slot-value 'fg) FG)
   (setf (slot-value 'rank) rank)
   (send self :axes-labels 
                      (select basicv (iseq rank)))
   (send self :needs-computing F)
 )
)

;;Method to print the inertia of the principal axes
;;

(defmeth cc-proto :pr-inertia ()
"Message args ()
Prints the inertia of the principal axes"
  (if (send self :needs-computing) (send self
                                      :compute))
  (let* ( (eigen (^ (send self :slot-value 'basicv) 2))
          (rank1 (- (send self :rank) 1))
          (eigen (select eigen (iseq 0 rank1)))  
          (total (sum eigen))
          (perc  (* (/ eigen total) 100))
          (cperc (cumsum perc))
          (nax   (iseq 0 rank1)) )
    (format t "~%          CORRESPONDENCE ANALYSIS~%")
    (format t "~% Decomposition of total inertia along principal axes~%~%")
    (format t "AXES   INERTIA (eigenvalues)   %of INERTIA       Cum %~%")
    (dotimes (i (+ rank1 1))
         (format t "~3d     ~10g            ~10g     ~10g~%"
                      (select nax i)
                      (select eigen i)
                      (select perc i)
                      (select cperc i)))
    (format t "~%Total  ~10g~%" total)
))

;;;
;;; Slot accessors and mutators
;;;

(defmeth cc-proto :x (&optional new-x)
"Message args: (&optional new-x)
With no argument returns the x matrix. With an argument
NEW-X sets the x matrix to NEW-X, recomputes" 
 (when (and new-x (matrixp new-x))
       (setf (slot-value 'x) new-x)
       (send self :needs-computing t))
 (slot-value 'x))

(defmeth cc-proto :row-n (&optional (n nil set))
"Message args: (&optional row-n)
With no argument returns the # of rows in X. With an 
argument sets it." 
  (if set (setf (slot-value 'row-n) n))
  (slot-value 'row-n) )

(defmeth cc-proto :col-n (&optional (n nil set))
"Message args: (&optional col-n)
With no argument returns the # of cols in X. With an 
argument sets it." 
  (if set (setf (slot-value 'col-n) n))
  (slot-value 'col-n) )

(defmeth cc-proto :rank (&optional (n nil set))
"Message args: (&optional rank)
With no argument returns the rank of X. With an 
argument sets it." 
  (if set (setf (slot-value 'rank) n))
  (slot-value 'rank) )

(defmeth cc-proto :col-labels (&optional
                      (names nil set))
"Message args: (&optional col-labels)
With no argument returns the col labels. With an 
argument sets it. In the absence of supplied labels, 
C0, C1, etc are used."  
 (if set (setf (slot-value 'col-labels)
                 (mapcar #'string names)))
 (let ( (m (send self :col-n))
        (mnames (slot-value 'col-labels)) )
    (if (not (and mnames (= (length mnames) m)))
       (setf (slot-value 'col-labels)
                  (mapcar #'(lambda (a)
                  (format nil "C~a" a))
                  (iseq 0 (- m 1))))))
 (slot-value 'col-labels)
)

(defmeth cc-proto :row-labels (&optional
                      (names nil set))
"Message args: (&optional col-labels)
With no argument returns the col labels. With an 
argument sets it. In the absence of supplied labels, 
R0, R1, etc are used."
 (if set (setf (slot-value 'row-labels)
                 (mapcar #'string names)))
 (let ( (m (send self :row-n))
        (mnames (slot-value 'row-labels)) )
    (if (not (and mnames (= (length mnames) m)))
       (setf (slot-value 'row-labels)
                  (mapcar #'(lambda (a)
                  (format nil "R~a" a))
                  (iseq 0 (- m 1))))))
 (slot-value 'row-labels)
)

(defmeth cc-proto :axes-labels 
                    (&optional (basicv nil set))
"Message args: (&optional basic-values)
With no argument returns all possible axes labels. With an 
argument sets it. The basic values are used to compute the 
% of inertia explained, which is used in each label."
  (when set 
     (let* ( (eigen (^ basicv 2))
             (perc (round (* (/ eigen (sum eigen)) 100)))
             (axes (iseq (length basicv))) 
             (labs
                (mapcar #'(lambda (ax p)
                     (format nil "Ax~s ~s%" ax p))
                 axes  (coerce perc 'list)))
           )        
        (setf (slot-value 'axes-labels) labs)
   )  )
  (slot-value 'axes-labels))

;;;
;;; The method for making 3-D plots is given below.
;;;

(defmeth cc-proto :plot-3d (&optional 
                              (axes '(0 1 2)))
"Message args: (&optional axes)
Opens a window with a spin-plot of the supplied axes or, by
default, the 1st 3 axes. The plot can (and should!) be linked 
to other plots including the plot-2d plot using the menu. 
Returns a plot object"
  (if (send self :needs-computing) (send self :compute))
  (let* ( (fg (send self :slot-value 'fg))
          (n (send self :row-n))
          (m (send self :col-n))
          (rlab (send self :row-labels))
          (clab (send self :col-labels))
          (rank (send self :rank))
          (xvar (select fg (first axes)))
          (yvar (select fg (second axes)))
          (zvar (select fg (third axes)))
          (fact (max (max xvar) (abs (min xvar))
                     (max yvar) (abs (min yvar))
                     (max zvar) (abs (min zvar))))
          (xvar (/ xvar fact))
          (yvar (/ yvar fact))
          (zvar (/ zvar fact))
          (titl (format nil "Plot ~s" axes))
          (v-labs (mapcar #'(lambda (i)
                                 (format nil "~s" i)) axes))
          (p (spin-plot (list xvar yvar zvar)
                        :title titl
                        :variable-labels v-labs
                        :scale nil)) )
   (send p :point-label (iseq (+ n m)) (append rlab clab))
   p
 )
)

;;;
;;; The method for making 2-D plots is given below. It uses 
;;; a plot proto cc-2dplot-proto that is defined below.
;;;

(defmeth cc-proto :plot-2d (&optional 
                              (axes '(0 1)))
"Message args: (&optional axes)
Opens a window with a 2-d plot of the supplied axes or, by
default, the 1st 2 axes. The plot can (and should!) be linked 
to other plots including the plot-3d plot using the menu. 
Returns a plot object"
  (if (send self :needs-computing) (send self :compute))
  (let* ( 
     (fg (send self :slot-value 'fg))
     (n (send self :row-n))
     (m (send self :col-n))
     (rlab (send self :row-labels))
     (clab (send self :col-labels))
     (rank (send self :rank))
     (xvar (select fg (first axes)))
     (yvar (select fg (second axes)))
     (dely (* 0.1 (- (max yvar) (min yvar))))
     (delx (* 0.1 (- (max xvar) (min xvar))))
     (ymind (- (min yvar) dely) )
     (ymaxd (+ (max yvar) dely) )
     (xmind (- (min xvar) delx) )
     (xmaxd (+ (max xvar) delx) )
     (titl (format nil "Plot ~s" axes))
     (p (send cc-2dplot-proto :new 2 :title titl))
  )
    (send p :cc-object self)
    (send p :add-lines (list (list xmind xmaxd 0 0)
                             (list 0 0 ymind ymaxd)) 
            :draw nil)
    (send p :linestart-next 1 nil)
    (send p :adjust-to-data)
    (send p :label-axes 
              (select (send self :axes-labels) axes))    
    (send p :add-points (list xvar yvar)
            :point-labels (append rlab clab))
    (send p :point-symbol (iseq n (+ n m -1)) 'x)
    (send p :scale-type 'fixed)
    p
 )
)

;;;
;;; The cc-2dplot-proto inherits from the graph-proto
;;; and is used in the :plot-2d method of cc-proto. It
;;; was necessary to make this proto since the 2-d plot
;;; has axes and labels in non-standard places. This meant,
;;; most importantly, a new redraw-content method.
;;;
 

(defproto cc-2dplot-proto '(axes-labels
                            cc-object)                      
   () graph-proto)


(defmeth cc-2dplot-proto :redraw-content ()
   (let ((labels (send self :axes-labels)))
     (send self :start-buffering)
     (call-next-method)  
     (if labels (send self :label-axes labels))
     (send self :buffer-to-screen)
  ))

;;;
;;; A method to label the axes.
;;;

(defmeth cc-2dplot-proto :label-axes (labels)
"Message args: labels 
          where labels is a list of 2 labels.
The method draws this text on the graph near the axes"
  (let* ( (labx (first labels))
          (laby (second labels))
          (nulnul (send self :real-to-canvas 0 0))
        )
    (send self :draw-text laby (first nulnul) 12 1 0)
    (send self :draw-text-up labx 
                              12 (second nulnul) 1 0)
    (send self :axes-labels labels)
 )
)

;;;
;;; Methods to put row, column labels on the plot. The cc-object 
;;; is needed to get access to the # of rows and columns.
;;; 

(defmeth cc-2dplot-proto :show-cols ()
"Message args ()
Sets the points selected to the ones corresponding to the 
columns"
  (let* ( (cc (send self :cc-object))
          (n (send cc :row-n))
          (m (send cc :col-n))
          (col-indexes (iseq n (+ n m -1))) )
     (send self :showing-labels T)
     (send self :point-selected col-indexes T)
))

(defmeth cc-2dplot-proto :show-rows ()
"Message args ()
Sets the points selected to the ones corresponding to the 
rows"
  (let* ( (cc (send self :cc-object))
          (n (send cc :row-n))
          (row-indexes (iseq n)) )
     (send self :showing-labels T)
     (send self :point-selected row-indexes T)
))

;;;
;;; The option to show row and/or column labels is added to
;;; the standard menu through the :menu-template method.
;;; (No provision is made to turn them off as that is 
;;; automatically done by clicking on the plot.)
;;;

(defmeth cc-2dplot-proto :menu-template ()
  (flet ( (action1 () (send self :show-cols))
          (action2 () (send self :show-rows)) )
    (let ( (item1 (send menu-item-proto 
                    :new "Col labels"
                    :action #'action1))
           (item2 (send menu-item-proto
                    :new "Row labels"
                    :action #'action2))
           (dash (send dash-item-proto :new)) )
      (append (call-next-method) (list dash item1 item2)))))

;;;
;;; Accessor and mutation methods for the cc-2dplot-proto
;;;

(defmeth cc-2dplot-proto :cc-object
                        (&optional (cc nil set))
"Message args: (&optional cc-object)
With no argument, returns the cc-proto object that initiated
the 2-d plot. When supplied with such an object, sets it."
  (when set (setf (slot-value 'cc-object) cc))
  (slot-value 'cc-object)) 


(defmeth cc-2dplot-proto :axes-labels 
                        (&optional (labs nil set))
"Message args: (&optional axes-labels)
    where axes-labels is a list of 2 strings.
With no argument, returns the axes-labels, else sets it."
  (when set (setf (slot-value 'axes-labels) labs))
  (slot-value 'axes-labels)) 
