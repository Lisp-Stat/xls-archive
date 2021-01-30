
;;;;
;;;; sir--i-ii-model.lsp XLISP-STAT sir-i-ii model proto and methods
;;;; sir 1.0 Copyright (c) 1990 by Ker-Chau Li
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;


;;;;
;;;;
;;;; Sir-I-II Model Prototype
;;;;
;;;;

(defproto sir-I-II-model-proto
  '(x y nslice eigen-vectors1 eigen-values1 total-evalue1 eigen-vectors2 eigen-values2 total-evalue2)
  ()
  *object*
  "Sliced Inverse Regression  ( I & II ) Model")

(defun sir-I-II-model (x y &key (print T))
"Args: (x y  &key(print T))
X           - list of independent variables (list of lists)
Y           - dependent variable.
PRINT    - if not NIL print summary information and plot sir1-y-sir2
Returns a sir model object. To examine the model further assign the
result to a variable and send it messages.
Messages include :
                  x  -  original independent variables
                  std-x - standard-deviation of x (a list)
                  matrix-x  -  matrix form of x  
                  y  -  original dependent variable
                  sir11  -  the variable for the first direction of SIR1
                  sir12  -  the variable for the second direction of SIR1
                  sir13  -  the variable for the third direction of SIR1
                  sir21  -  the variable for the first direction of SIR2
                  sir22  -  the variable for the second direction of SIR2
                  sir23  -  the variable for the third direction of SIR2
                  newx1  -  the list of variables for all SIR1 directions
                  newx2  -  the list of variables for all SIR2 directions
                  nobs   -  # of observations in the data set
                  dim    -  # of independent variables
                  eigen-values1   -  list of all eigenvalues for SIR1
                  eigen-vectors1  -  list of eigenvectors for SIR1 (in list form)
                  std-eigen-vectors1 - standardized eigen-vectors of SIR1 (in list form)
                  total-evalue1   -  sum of all eigenvalues for SIR1
                  eigen-values2   -  list of all eigenvalues for SIR2
                  eigen-vectors2  -  list of eigenvectors for SIR2 (in list form)
                  std-eigen-vectors2 - standardized eigen-vectors of SIR2 (in list form)
                  total-evalue2   -  sum of all eigenvalues for SIR2
                  p-values1  -  p-values for checking if directions found  by
                                SIR1 are real. if only the first k p-values are needed,
                                use the format :
                                :p-values :only k
                  nslice   -  # of slices used
                  plot11   -  spin-plot of sir11-y-sir12
                  plot12   -  spin-plot of sir11-sir12-sir13
                  plot21   -  spin-plot of sir21-y-sir22
                  plot22   -  spin-plot of sir21-sir22-sir23"

  (let (
        (m (send sir-I-II-model-proto :new))
        )
    (send m :x x)
    (send m :y y)
    (send m :compute)
    (if print (send m :display))
    m))

(defmeth sir-I-II-model-proto :isnew () (send self :needs-computing t))

(defmeth sir-I-II-model-proto :save ()
  `(sir-I-II-model',(send self :x)
                     ',(send self :y)
                     ))
(defmeth sir-I-II-model-proto :compute ()
  (let*(
        (x (send self :x))
        (y (send self :y))
        (sir-I-II-out (sir-I-II-no-output x y))
        )
    (setf (slot-value 'x) x)
    (setf (slot-value 'y) y)
    (setf (slot-value 'nslice) (nth 2 sir-I-II-out))
    (setf (slot-value 'eigen-vectors1) (nth 0 (nth 0 sir-I-II-out)))
    (setf (slot-value 'eigen-values1) (nth 1 (nth 0 sir-I-II-out)))
    (setf (slot-value 'total-evalue1) (nth 2 (nth 0 sir-I-II-out)))
    (setf (slot-value 'eigen-vectors2) (nth 0 (nth 1 sir-I-II-out)))
    (setf (slot-value 'eigen-values2) (nth 1 (nth 1 sir-I-II-out)))
    (setf (slot-value 'total-evalue2) (nth 2 (nth 1 sir-I-II-out)))
    ))

(defmeth sir-I-II-model-proto :needs-computing (&optional set)
  (if set (setf (slot-value 'nslice) nil))
  (null (slot-value 'nslice)))

(defmeth sir-I-II-model-proto :display ()
  (let (
        (evt1 (send self :eigen-vectors1))
        (evt2 (send self :eigen-vectors2))
        (sevt1 (send self :std-eigen-vectors1))
        (sevt2 (send self :std-eigen-vectors2))
        )
    (format t "~%=========================================================")
    (format t "~%     *** Sliced Inverse Regression Model    ***~2%")
    (format t "Number of observation:               ~s" (send self :nobs))
    (format t "~2%Number of independent variables:      ~s" (send self :dim))
    (format t "~2%Number of slices:                     ~s" (send self :nslice))
    (format t "~2%=========================================================")
    (format t "~%     ***               SIR-I                ***")
    (format t "~2%the first direction found by SIR-I:~%")
    (format t "~s" (nth 0 evt1))
    (format t "~2%the second direction found by SIR-I:~%")
    (format t "~S"(nth 1 evt1))
    (format t "~2%the third direction found by SIR-I:~%")
    (format t "~s"(nth 2 evt1))
    (format t "~2%the first <<standardized>> direction found by SIR-I:~%")
    (format t "~s" (nth 0 sevt1))
    (format t "~2%the second <<standardized>> direction found by SIR-I:~%")
    (format t "~S"(nth 1 sevt1))
    (format t "~2%the third <<standardized>> direction found by SIR-I:~%")
    (format t "~s"(nth 2 sevt1))
    (format t "~2%the companion output eigenvalues of SIR-I:~%")
    (format t "~s"(send self :eigen-values1))
    (format t "~2%the sum of all eigenvalues for SIR-I:~%")
    (format t "~s"(send self :total-evalue1))
    (format t "~2%=========================================================")
    (format t "~%     ***               SIR-II               ***")
    (format t "~2%the first direction found by SIR-II:~%")
    (format t "~s" (nth 0 evt2))
    (format t "~2%the second direction found by SIR-II:~%")
    (format t "~S"(nth 1 evt2))
    (format t "~2%the third direction found by SIR-II:~%")
    (format t "~s"(nth 2 evt2))
    (format t "~2%the first <<standardized>> direction found by SIR-II:~%")
    (format t "~s" (nth 0 sevt2))
    (format t "~2%the second <<standardized>> direction found by SIR-II:~%")
    (format t "~S"(nth 1 sevt2))
    (format t "~2%the third <<standardized>> direction found by SIR-II:~%")
    (format t "~s"(nth 2 sevt2))
    (format t "~2%the companion output eigenvalues of SIR-II:~%")
    (format t "~s"(send self :eigen-values2))
    (format t "~2%the sum of all eigenvalues for SIR-II:~%")
    (format t "~s"(send self :total-evalue2))
    (format t "~2%=========================================================~%")
    (send self :plot11)
;    (send self :plot12)
    (send self :plot21)
;    (send self :plot22)
    ))

(defmeth sir-I-II-model-proto :x (&optional new-x)
  (when new-x
        (setf (slot-value 'x) new-x)
        (send self :needs-computing t))
  (slot-value 'x))

(defmeth sir-I-II-model-proto :y (&optional new-y)
  (when new-y
        (setf (slot-value 'y) new-y)
        (send self :needs-computing t))
  (slot-value 'y))

(defmeth sir-I-II-model-proto :eigen-values1 ()
  (slot-value 'eigen-values1))

(defmeth sir-I-II-model-proto :eigen-vectors1 ()
  (slot-value 'eigen-vectors1))

(defmeth sir-I-II-model-proto :total-evalue1 ()
  (slot-value 'total-evalue1))

(defmeth sir-I-II-model-proto :eigen-values2 ()
  (slot-value 'eigen-values2))

(defmeth sir-I-II-model-proto :eigen-vectors2 ()
  (slot-value 'eigen-vectors2))

(defmeth sir-I-II-model-proto :total-evalue2 ()
  (slot-value 'total-evalue2))

(defmeth sir-I-II-model-proto :newx1 ()
  (let*(
        (mx (send self :matrix-x))
        (nx1 (copy-list (send self :x)))
        (evector1 (send self :eigen-vectors1))
        )
      (dotimes (i (send self :dim))
           (setf (nth i nx1) (coerce (%* mx (nth i evector1)) 'list)))
    nx1))


(defmeth sir-I-II-model-proto :newx2 ()
  (let*(
        (mx (send self :matrix-x))
        (nx2 (copy-list (send self :x)))
        (evector2 (send self :eigen-vectors2))
        )
      (dotimes (i (send self :dim))
           (setf (nth i nx2) (coerce (%* mx (nth i evector2)) 'list)))
    nx2))

(defmeth sir-I-II-model-proto :sir11 ()
  (let*(
        (mx (send self :matrix-x))
        (ev11 (nth 0 (send self :eigen-vectors1)))
        )
    (coerce (%* mx ev11) 'list)))

(defmeth sir-I-II-model-proto :sir12 ()
  (let*(
        (mx (send self :matrix-x))
        (ev12 (nth 1 (send self :eigen-vectors1)))
        )
    (coerce (%* mx ev12) 'list)))

(defmeth sir-I-II-model-proto :sir13 ()
  (let*(
        (mx (send self :matrix-x))
        (ev13 (nth 2 (send self :eigen-vectors1)))
        )
    (coerce (%* mx ev13) 'list)))

(defmeth sir-I-II-model-proto :sir21 ()
  (let*(
        (mx (send self :matrix-x))
        (ev21 (nth 0 (send self :eigen-vectors2)))
        )
    (coerce (%* mx ev21) 'list)))

(defmeth sir-I-II-model-proto :sir22 ()
  (let*(
        (mx (send self :matrix-x))
        (ev22 (nth 1 (send self :eigen-vectors2)))
        )
    (coerce (%* mx ev22) 'list)))

(defmeth sir-I-II-model-proto :sir23 ()
  (let*(
        (mx (send self :matrix-x))
        (ev23 (nth 2 (send self :eigen-vectors2)))
        )
    (coerce (%* mx ev23) 'list)))

(defmeth sir-I-II-model-proto :std-x ()
  (stand (send self :x)))

(defmeth sir-I-II-model-proto :std-eigen-vectors1 ()
  (let*(
        (std-x (send self :std-x))
        (dim (length std-x))
        (eigen-vectors1 (send self :eigen-vectors1))
        (std-eigen-vectors1 (repeat (list (repeat 0 dim)) dim))
        )
    (dotimes (i dim)
             (setf (nth i std-eigen-vectors1) (* (nth i eigen-vectors1) std-x)))
    std-eigen-vectors1))

(defmeth sir-I-II-model-proto :std-eigen-vectors2 ()
  (let*(
        (std-x (send self :std-x))
        (dim (length std-x))
        (eigen-vectors2 (send self :eigen-vectors2))
        (std-eigen-vectors2 (repeat (list (repeat 0 dim)) dim))
        )
    (dotimes (i dim)
             (setf (nth i std-eigen-vectors2) (* (nth i eigen-vectors2) std-x)))
    std-eigen-vectors2))


(defmeth sir-I-II-model-proto :matrix-x ()
  (let*(
        (x (send self :x))
        (dim (send self :dim))
        (nobs (send self :nobs))
        (mx (transpose (make-array (list dim nobs) :initial-contents x)))
        )
    mx))

(defmeth sir-I-II-model-proto :nobs ()
  (length (send self :y)))

(defmeth sir-I-II-model-proto :dim ()
  (length (send self :x)))

(defmeth sir-I-II-model-proto :p-values1 (&key (only nil))
  (let*(
        (dim (send self :dim))
        (nobs (send self :nobs))
        (evl (send self :eigen-values1))
        (tev (send self :total-evalue1))
        (ns (send self :nslice))
        (dim1 (if only only dim))
        count
        pv
        )
    (dotimes (i dim1)
             (setf count (+ i 1))
             (setf pv (- 1 (chisq-cdf (* nobs tev) (* (- dim i) (- ns count)))))
             (setf tev (- tev (nth i evl)))
             (format t "~%P-value for checking if the ~s-th component for SIRIis real:     ~s~%" count pv)
             )
    (format t "~%")
    ))

;;(defmeth sir-I-II-model-proto :p-values2 (&key (only nil))
;;  (let*(
;;        (dim (send self :dim))
;;        (nobs (send self :nobs))
;;        (evl (send self :eigen-values2))
;;        (tev (send self :total-evalue2))
;;        (ns (send self :nslice))
;;        (dim1 (if only only dim))
;;        count
;;        pv
;;        )
;;    (dotimes (i dim1)
;;             (setf count (+ i 1))
;;             (setf pv (- 1 (chisq-cdf (* nobs tev) (* (- dim i) (- ns count)))))
;;             (setf tev (- tev (nth i evl)))
;;             (format t "~%P-value for checking if the ~s-th component for SIRIIis real:     ~s~%" ;;count pv)
;;             )
;;    (format t "~%")
;;    ))


(defmeth sir-I-II-model-proto :nslice ()
  (slot-value 'nslice))

(defmeth sir-I-II-model-proto :plot11 ()
  (setf plot11 (spin-plot (list (send self :sir11) (send self :y) (send self :sir12))
                         :variable-labels (list "sirI-1" "y" "sirI-2")
                         :title "Sir-I-view: sirI-1-y-sirI-2"))
  (send plot11 :linked t)
  )
                       

(defmeth sir-I-II-model-proto :plot12 ()
  (setf plot12 (spin-plot (list (send self :sir11) (send self :sir12) (send self :sir13))
                         :variable-labels (list "sirI-1" "sirI-2" "sirI-3")
                         :title "Sir-I-view: sirI-1 - sirI-2 - sirI-3"))
  (send plot12 :linked t)
  )

(defmeth sir-I-II-model-proto :plot21 ()
  (setf plot21 (spin-plot (list (send self :sir21) (send self :y) (send self :sir22))
                         :variable-labels (list "sirII-1" "y" "sirII-2")
                         :title "Sir-II-view: sirII-1-y-sirII-2"))
  (send plot21 :linked t)
  )
                       
(defmeth sir-I-II-model-proto :plot22 ()
  (setf plot22 (spin-plot (list (send self :sir21) (send self :sir22) (send self :sir23))
                         :variable-labels (list "sirII-1" "sirII-2" "sirII-3")
                         :title "Sir-II-view: sirII-1 - sirII-2 - sirII-3"))
  (send plot22 :linked t)
  )


;;=======================================================================

(defun sir-I-II-no-output ( x y)
"sir-I-II"
  (let* ( 
          (y-order (order y))
          (nobs (length y))
          (dim (length x))
          (x-mat (transpose (make-array (list dim nobs) :initial-contents x)))
          (cov-x (covariance-matrix1 x-mat ))
          (cov-x-inv (inverse cov-x))
          (nslice (nth 0 (get-value-dialog "number of slices" )))
          (ncase1(floor (/ nobs nslice)))
          (nslice2 (- nobs (* ncase1 nslice)))
          (nslice1 (- nslice nslice2))
          (ncase2( + ncase1 1))
          (iseq-ncase (iseq 0 (- ncase1 1)))
          (case (select y-order iseq-ncase))
          (iseq-dim (iseq 0 (- dim 1)))
          (x-slice (select x-mat case iseq-dim))
          (covx-slice (list (covariance-matrix1 x-slice)))
          (ave-covx (car (copy-list covx-slice)))
          (evv1 (repeat 0 dim))
          (evector1 (repeat 0 dim))
          (evv2 (repeat 0 dim))
          (evector2 (repeat 0 dim))
          cov-3
          meat1
          evalues1
          total-evalue1
          order-evalue1
          ev1
          meat2
          evalues2
          total-evalue2
          order-evalue2
          ev2
          )
       
    (dotimes (i (- nslice1 1))
             (setf iseq-ncase (+ iseq-ncase ncase1))
             (setf case (select y-order   iseq-ncase))
             (setf x-slice (select x-mat case iseq-dim))
             (setf covx-slice(append  covx-slice   (list (covariance-matrix1 x-slice)))))
        
    (setf iseq-ncase (append (list (- (nth 0 iseq-ncase) 1)) iseq-ncase))
    (dotimes (i nslice2 )
             (setf iseq-ncase(+ iseq-ncase ncase2))
             (setf case (select y-order   iseq-ncase))
             (setf x-slice (select x-mat case iseq-dim))
             (setf covx-slice (append  covx-slice   (list (covariance-matrix1 x-slice)))))

    (dotimes (i (- nslice1 1))
             (setf ave-covx (+ ave-covx (nth (+ 1 i) covx-slice)))
             )
    (setf ave-covx (/ (*   ave-covx ncase1 ) nobs))
    (dotimes (i  nslice2 )
             (setf ave-covx (+ ave-covx (/ (* ncase2 (nth (+ nslice1 i) covx-slice)) nobs)
                               )))
    (setf meat1 (eigen-decomp (- cov-x ave-covx) cov-x))    
    (setf evalues1  (coerce (nth 1 meat1) 'list))
    (setf total-evalue1 (sum evalues1))
    (setf ev1 (coer (nth 0 meat1) ))
    (setf order-evalue1 (reverse (order evalues1)))
    (setf evalues1 (reverse (sort-data evalues1)))
    (dotimes (i dim)
             (setf (nth i evv1) (coerce (nth (nth i order-evalue1) ev1) 'list))
             )

;;====================================================================

    (dotimes (i nslice)
             (setf (nth i covx-slice) (- (nth i covx-slice) ave-covx))
             )
    (setf cov-3 (%* (%* (nth 0 covx-slice) cov-x-inv) (nth 0 covx-slice)))
    (dotimes (i (- nslice1 1))
             (setf cov-3 (+ cov-3 (%* (%* (nth (+ i 1) covx-slice ) cov-x-inv) (nth (+ i 1) covx-slice))))
             )
    (setf cov-3 (* cov-3 (/ ncase1 nobs)))
    (dotimes (i nslice2)
             (setf cov-3 (+ cov-3 (* (%* (%* (nth (+ i nslice1) covx-slice ) cov-x-inv) (nth (+ i 1) covx-slice)) (/ ncase2 nobs) )))
             )
    (setf meat2 (eigen-decomp cov-3 cov-x))        
    (setf evalues2  (coerce (nth 1 meat2) 'list))
    (setf total-evalue2 (sum evalues2))
    (setf ev2 (coer (nth 0 meat2) ))
    (setf order-evalue2 (reverse (order evalues2)))
    (setf evalues2 (reverse (sort-data evalues2)))
    (dotimes (i dim)
             (setf (nth i evv2) (coerce (nth (nth i order-evalue2) ev2) 'list))
             )
    (list (list evv1 evalues1 total-evalue1) (list evv2 evalues2 total-evalue2) nslice)
    ))

(defun coer (x)
  (split-list (apply #'append
               (mapcar #'(lambda (x) (coerce x 'list))
                     (row-list x))) (nth 1 (array-dimensions x))))


