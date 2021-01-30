; Xlispstat program to demonstrate Principal Component Analysis
; November 1992, Jan Graffelman (graff@upf.es)
; Just load this file and you are on your way.
;========================================================================
; Construct Title & CopyRight.
;
;
(message-dialog "Demonstration of Principal Component Analysis")
(message-dialog "Written by Jan Graffelman. UPF. November 1992")
;========================================================================
(defproto my-data '(data points bpoints pc eigenv eigenval center stand 
                    covmatrix ticks
                    cormatrix n p corr) () 
graph-proto)

(def pca (send my-data :new 2))


(defmeth my-data :set-data (x)
   (setf (slot-value 'data) x)
   (setf (slot-value 'p) (length (select x 0)))
   (setf (slot-value 'n) (length x))
   (slot-value 'data))

(defmeth my-data :set-points (x)
  (setf (slot-value 'points) x)
(slot-value 'points))

(defmeth my-data :set-bpoints (x)
  (setf (slot-value 'bpoints) x)
(slot-value 'bpoints))

(defmeth my-data :set-ticks (x)
  (setf (slot-value 'ticks) x)
(slot-value 'ticks))

(defmeth my-data :get-ticks ()
  (slot-value 'ticks))

(defmeth my-data :get-points ()
  (slot-value 'points))

(defmeth my-data :get-bp-points ()
  (slot-value 'bpoints))

(defmeth my-data :clearp ()
  (setf (slot-value 'points) nil)
  (setf (slot-value 'bpoints) nil))


(defmeth my-data :get-data ()
   (slot-value 'data))

(defmeth my-data :num-cases ()
   (slot-value 'n))

(defmeth my-data :num-variables ()
   (slot-value 'p))

(defmeth my-data :get-centered ()
(let* ((n (send self :slot-value 'n))
       (p (send self :slot-value 'p))
       (matdata (combine (send self :get-data))) ; convert to matrix
       (v (repeat 1 n))
       (v1 (matrix (list n 1) v))          ; colum vector
       (v2 (matrix (list 1 n) v))          ; row vector
       (m1 (* (matmult v1 v2) (/ 1 n)))    ; matrix with all 1/n
       (m2 (matrix (list n p) matdata))    ; convert data to matrix
       (m3 (matmult m1 m2))                ; means
       (Xc (- matdata m3)))                ; centered data
       (setf (slot-value 'center) (matrix (list n p) Xc))) ; stored as matrix!
(slot-value 'center))


(defmeth my-data :print-data ()
   (let* ((n (send self :slot-value 'n))
       (p (send self :slot-value 'p))
       (matdata (combine (send self :get-data)))) ; convert to matrix
       (print-matrix(matrix (list n p) matdata))))   ; convert data to matrix


(defmeth my-data :print-ev ()
(let* ((n (send self :slot-value 'n))
       (p (send self :slot-value 'p))
       (matdata (send self :eigenval))) ; convert to matrix
       (print-matrix (matrix (list 1 p) matdata))))   ; convert data to matrix



(defmeth my-data :cov-matrix ()
    (let* ((Xc (send self :get-centered))
           (n (send self :slot-value 'n))
           (S (/ (matmult (transpose Xc) Xc) n)))
           (setf (slot-value 'covmatrix) S))
(slot-value 'covmatrix))


(defmeth my-data :get-standard ()
(let* ((S (send self :cov-matrix))
       (stdv (sqrt (diagonal S)))
       (Xc (send self :get-centered))
       (D (diagonal (/ 1 stdv)))
       (Xs (matmult Xc D)))
       (setf (slot-value 'stand) Xs))
(slot-value 'stand))


(defmeth my-data :cor-matrix ()
(let* ((n (send self :slot-value 'n))
       (Xse (send self :get-standard))
       (R (/ (matmult (transpose Xse) Xse) n)))
       (setf (slot-value 'cormatrix) R))
(slot-value 'cormatrix))

(defmeth my-data :eigenv ()
(let* ((p (send self :slot-value 'p))
       (R (send self :cor-matrix))
       (V (transpose (matrix (list p p) (combine (eigenvectors R))))))
       (setf (slot-value 'eigenv) V))
(slot-value 'eigenv))


(defmeth my-data :eigenval ()
(let* ((p (send self :slot-value 'p))
       (R (send self :cor-matrix))
       (ev (eigenvalues R)))
       (setf (slot-value 'eigenval) ev))
(slot-value 'eigenval))


(defmeth my-data :prc ()
(let* ((Xs (send self :get-standard))
       (V (send self :eigenv))
       (PC (matmult Xs V)))
       (setf (slot-value 'pc) PC))
(slot-value 'pc))

(defmeth my-data :get-cor ()
(let* ((pc (send self :prc))
       (n (send self :slot-value 'n))
       (Sy (/ (matmult (transpose pc) pc) n))
       (V (send self :eigenv))
       (Rxy (matmult V (diagonal (sqrt (diagonal Sy))))))
       (setf (slot-value 'corr) Rxy))
(slot-value 'corr))


(defmeth my-data :plot12 ()
(let* ((pc (send self :prc))
       (PCl (coerce (transpose pc) 'list))
       (xmnm (min (select PCl 0)))
       (xmxm (max (select PCl 0)))
       (ymnm (min (select PCl 1)))
       (ymxm (max (select PCl 1)))
       (xra (get-nice-range xmnm xmxm 4))
       (yra (get-nice-range ymnm ymxm 4)))
       (send self :clear)
       (send self :clearp)
       (send self :range 0 (select xra 0) (select xra 1))
       (send self :range 1 (select yra 0) (select yra 1))
       (send self :add-points (list (select PCl 0) (select PCl 1)))
       (send self :x-axis t t (select xra 2))
       (send self :set-ticks (select xra 2))
       (send self :y-axis t t (select yra 2))
       (setq pol ())
       (dotimes (i (length (select PCl 0)))
	(setq po (send self :real-to-canvas 
		       (select (select PCl 0) i)
		       (select (select PCl 1) i)))
                       (send self :draw-text (string (int-char (+ i 97))) 
			     (select po 0) (select po 1) 2 0)
                       (setq pol (append pol (list po))))
       (send self :set-points pol)
pc)
)


(defmeth my-data :plot23 ()
(let* ((pc (send self :prc))
       (PCl (coerce (transpose pc) 'list))
       (xmnm (min (select PCl 1)))
       (xmxm (max (select PCl 1)))
       (ymnm (min (select PCl 2)))
       (ymxm (max (select PCl 2)))
       (xra (get-nice-range xmnm xmxm 4))
       (yra (get-nice-range ymnm ymxm 4)))
       (send self :clear)
       (send self :clearp)
       (send self :range 0 (select xra 0) (select xra 1))
       (send self :range 1 (select yra 0) (select yra 1))
       (send self :add-points (list (select PCl 1) (select PCl 2)))
       (send self :x-axis t t (select xra 2))
       (send self :y-axis t t (select yra 2))
       (setq pol ())
       (dotimes (i (length (select PCl 0)))
        (setq po (send self :real-to-canvas
                       (select (select PCl 1) i)
                       (select (select PCl 2) i)))
                       (send self :draw-text (string (int-char (+ i 97)))
                             (select po 0) (select po 1) 2 0)
                             (setq pol (append pol (list po))))
(send self :set-points pol)
pc)
)



(defmeth my-data :plot13 ()
(let* ((pc (send self :prc))
       (PCl (coerce (transpose pc) 'list))
       (xmnm (min (select PCl 0)))
       (xmxm (max (select PCl 0)))
       (ymnm (min (select PCl 2)))
       (ymxm (max (select PCl 2)))
       (xra (get-nice-range xmnm xmxm 4))
       (yra (get-nice-range ymnm ymxm 4)))
       (send self :clear)
       (send self :clearp)
       (send self :range 0 (select xra 0) (select xra 1))
       (send self :range 1 (select yra 0) (select yra 1))
       (send self :add-points (list (select PCl 0) (select PCl 2)))
       (send self :x-axis t t (select xra 2))
       (send self :y-axis t t (select yra 2))
       (setq pol ())
       (dotimes (i (length (select PCl 0)))
         (setq po (send self :real-to-canvas
                       (select (select PCl 0) i)
                       (select (select PCl 2) i)))
                       (send self :draw-text (string (int-char (+ i 97)))
                             (select po 0) (select po 1) 2 0)
                             (setq pol (append pol (list po))))
(send self :set-points pol)
pc)
)



(defmeth my-data :biplot ()
(let* ((Rxyl (coerce (transpose (send self :get-cor)) 'list))
       (xval (select Rxyl 0))
       (yval (select Rxyl 1))
       (i 0))
       (send self :plot12)
       (send self :add-points (list xval yval))
       (setf bpo ())
(dotimes (i 4)
        (send self :add-lines (list (list 0 (car xval)) (list 0 (car yval))))
	(setq po (send self :real-to-canvas (car xval) (car yval)))
	(send self :draw-text (string (int-char (+ i 65))) 
	      (select po 0) (select po 1) 2 0)
        (setq yval (cdr yval))
        (setq xval (cdr xval))
        (setf bpo (append bpo (list po))))
(send self :set-bpoints bpo))
)


(defmeth my-data :redraw()

      (send self :redraw-background)
      (send self :redraw-overlays)
      (send self :redraw-content)
      (setf pr (send self :get-points))
      (setf bp (send self :get-bp-points))
      (dotimes (i (length pr))
                       (send self :draw-text (string (int-char (+ i 97)))
                       (select (select pr i) 0) (select (select pr i) 1) 2 0))
      (dotimes (i (length bp))
                       (send self :draw-text (string (int-char (+ i 65)))
                       (select (select bp i) 0) (select (select bp i) 1) 2 0))
      () )


(send pca :title "Principal Component Analysis")


(setf biplot-item (send menu-item-proto :new "Biplot"
		     :action
		     #'(lambda ()
			 (send pca :biplot))))

(setf covar-item (send menu-item-proto :new "Covariance Matrix"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :cov-matrix)))))

(setf corr-item (send menu-item-proto :new "Correlation Matrix"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :cor-matrix)))))

(setf ori-data-item (send menu-item-proto :new "Original Data"
                     :action
                     #'(lambda ()
                         (send pca :print-data))))

(setf cen-data-item (send menu-item-proto :new "Centered Data"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :get-centered)))))


(setf std-data-item (send menu-item-proto :new "Standardized Data"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :get-standard)))))

(setf prc-item (send menu-item-proto :new "Principal Components"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :prc)))))


(setf plot12-item (send menu-item-proto :new "Plot PC1 - PC2"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :plot12)))))


(setf plot13-item (send menu-item-proto :new "Plot PC1 - PC3"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :plot13)))))

(setf plot23-item (send menu-item-proto :new "Plot PC2 - PC3"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :plot23)))))


(setf eigen-item (send menu-item-proto :new "Eigenvectors"
                     :action
                     #'(lambda ()
                         (print-matrix (send pca :eigenv)))))

(setf value-item (send menu-item-proto :new "Eigenvalues"
                     :action
                     #'(lambda ()
                         (send pca :print-ev))))

(setf save-item (send menu-item-proto :new "Save in Postscript Format"
                     :action
                     #'(lambda ()
			 (setf ans (get-string-dialog 
				    "Postscript file: "
				    :initial "biplot.ps"))
                         (send pca :save-image ans))))

(setf datafile-item (send menu-item-proto :new "Datafile"
                     :action
                     #'(lambda ()
                         (setf ans (get-string-dialog
                                    "Enter data file: "
                                    :initial "comm.dat"))
;  avoid use of fcolums
;                         (setf col (fcolumns ans))
			 (setq jan (read-data-columns ans))
			 (send pca :set-data (transpose jan)))))




(setf men (send menu-proto :new "Menu PCA"))
(send men :append-items 
datafile-item
ori-data-item 
cen-data-item 
std-data-item
covar-item 
corr-item 
eigen-item
value-item
prc-item
plot12-item
plot13-item
plot23-item
biplot-item
#+X11
save-item
)

(send men :items)
(send pca :menu men)
(send pca :set-ticks 0)














